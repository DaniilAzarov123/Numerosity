# =============================================================================
# Experiment 1 — Data Preparation
#
# Loads raw JSON files, marks and removes bad participants, applies
# log-transformation, computes geometric means and absolute errors, and
# saves four analysis-ready CSV files in the data_processed folder:
#
#   1. E1_geometric_means_log.csv – one row per (subject × seed) pair,
# with errors computed in log scale
#   2. E1_geometric_means_raw.csv – one row per (subject × seed) pair,
# with errors computed in raw scale
#   3. E1_trials.csv - one row per trial
#   4. E1_metacognition.csv - one row per subject, 
# with answers to meta-cognitive questions at the end of the Experiment
#
# =============================================================================

library(jsonlite)
library(dplyr)
library(tidyverse)

# ── 1. Load raw data ──────────────────────────────────────────────────────────

file_list <- list.files(path = "data", full.names = TRUE)

dataset <- lapply(seq_along(file_list), function(i) {
  cat(i, " ")
  fromJSON(file_list[i])
})  %>%  bind_rows()


# ── 2. Identify test trials ───────────────────────────────────────────────────

numerosity_trials <- dataset %>%
  filter(trial_type == "numerosity-estimation",
         callibration == "test")

# ── 3. Mark bad participants ──────────────────────────────────────────────────

# Mean absolute error per participant (absDeviation is on the raw scale,
# as it comes directly from the experiment software)
mean_abs_error <- tapply(
  X = numerosity_trials$absDeviation,
  INDEX = numerosity_trials$subject_id,
  FUN = mean
)

# Exclude the top 5% by raw absolute error
error_threshold <- quantile(mean_abs_error, probs = 0.95)

numerosity_trials <- numerosity_trials %>%
  mutate(bad_subject = case_when(
    mean_abs_error[subject_id] > error_threshold ~ "bad",
    TRUE ~ "good"
  ))

cat("Excluded participants:", sum(numerosity_trials$bad_subject == "bad" &
                                    !duplicated(numerosity_trials$subject_id)))
cat("Retained participants:", length(unique(
  numerosity_trials$subject_id[numerosity_trials$bad_subject == "good"])), "\n")

# ── 4. Pre-process retained trials ───────────────────────────────────────────

filtered_data <- numerosity_trials %>%
  filter(bad_subject == "good") %>%
  mutate(response = as.numeric(response)) %>%
  filter(!is.na(response)) %>%
  # These two categories were miscoded in the experiment
  filter(!sprites %in% c("butterflies", "marbles")) %>%
  mutate(
    log_response = log(response),    # log scale — used in most analyses
    log_numerosity = log(numerosity),  # log scale — used in most analyses
    # Signed deviation on the RAW scale — needed for direction-of-2nd-estimate
    # and category-bias analyses, where sign and raw magnitude both matter
    signed_deviation_raw = response - numerosity,
    sprites = as.factor(sprites)
  ) %>%
  # Drop unnecessary columns
  select(-url,-trial_type,-view_history, 
         -slider_start, -callibration,-question_order)

rownames(filtered_data) <- NULL

# ── 5. Compute geometric means and absolute errors ────────────────────────────
#
# We need two versions of the aggregated summary, because different analyses
# operate on different scales: log and raw scale

# -- 5a. Log-scale summary (used for WoIC, WoC, and quadratic analyses) -------

geometric_means_log <- filtered_data %>%
  select(subject_id, seed, log_response, log_numerosity, matchType) %>%
  group_by(subject_id, seed) %>%
  summarise(
    log_numerosity = last(log_numerosity),
    matchType = last(matchType),
    first_estimate_log = first(log_response),
    second_estimate_log = last(log_response),
    # Geometric mean of the log-scale estimates (log_response values are already
    # logged, so this is exp(mean(log(log_response))) — two logs and one exp)
    geometric_mean_log = exp(mean(log(log_response))),
    first_error_log = abs(first_estimate_log - log_numerosity),
    second_error_log = abs(second_estimate_log - log_numerosity),
    geom_mean_error_log = abs(geometric_mean_log - log_numerosity),
    .groups = "drop"
  ) %>%
  mutate(
    matchType = factor(matchType, levels = c("paired", "unpaired"))
  )

# -- 5b. Raw-scale summary (used for similarity plot and WoIC-magnitude plot) --

geometric_means_raw <- filtered_data %>%
  select(subject_id, seed, response, numerosity, matchType) %>%
  group_by(subject_id, seed) %>%
  summarise(
    numerosity = last(numerosity),
    matchType = last(matchType),
    first_estimate_raw = first(response),
    second_estimate_raw = last(response),
    # Geometric mean on raw scale: exp(mean of logs of raw responses)
    geometric_mean_raw = exp(mean(log(response))),
    first_error_raw = abs(first_estimate_raw - numerosity),
    second_error_raw = abs(second_estimate_raw - numerosity),
    geom_mean_error_raw = abs(geometric_mean_raw - numerosity),
    .groups = "drop"
  ) %>%
  mutate(
    matchType = factor(matchType, levels = c("paired", "unpaired")),
    # |Est1 - Est2| on the raw scale — used in the similarity mixed model
    estimate_difference_raw = abs(first_estimate_raw - second_estimate_raw),
    # WoIC magnitude: how much does averaging improve over the average individual error?
    mean_individual_error_raw = (first_error_raw + second_error_raw) / 2,
    woic_magnitude_raw = mean_individual_error_raw - geom_mean_error_raw,
    # Centred numerosity for the quadratic model
    c_numerosity = as.vector(scale(numerosity, center = TRUE, scale = FALSE))
  )

# ── 6. Add between- and within-person random partners (WoC analysis) ─────────
#
# For each (subject × seed) pair we draw one partner response from four conditions:
#   (a) Different participant, SAME display (same seed)              → WoC, same display
#   (b) Different participant, DIFFERENT display (same log_num)      → WoC, different display
#   (c) Same participant, DIFFERENT display (same log_num, diff seed)→ WoIC, different display
#   (d) Same participant, SAME display (other presentation)          → WoIC, same display
#
# We iterate only over FIRST estimates (which == "first") because each
# (subject × seed) pair should contribute exactly one row to the WoC analysis.
# Partner responses are in LOG scale, consistent with the main WoIC analysis.
# Results are joined directly onto geometric_means_log (one row per subject × seed).

first_estimates <- filtered_data %>%
  filter(which == "first")

set.seed(1234)

n <- nrow(first_estimates)
partner_diff_same_display <- numeric(n)
partner_diff_display_diff <- numeric(n)
partner_same_display_diff  <- numeric(n)

for (i in seq_len(n)) {
  if (i %% 500 == 0) cat(i, " ")
  
  id_i <- first_estimates$subject_id[i]
  seed_i <- first_estimates$seed[i]
  log_num_i <- first_estimates$log_numerosity[i]
  
  # (a) Different participant, same display (same seed) — LOG response
  pool <- first_estimates %>%
    filter(subject_id != id_i, seed == seed_i)
  partner_diff_same_display[i] <- pool$log_response[sample(nrow(pool), 1)]
  
  # (b) Different participant, different display (same log_numerosity, different seed) — LOG
  pool <- first_estimates %>%
    filter(subject_id != id_i, seed != seed_i, log_numerosity == log_num_i)
  partner_diff_display_diff[i] <- pool$log_response[sample(nrow(pool), 1)]
  
  # (c) Same participant, different display (same log_numerosity, different seed) — LOG
  pool <- first_estimates %>%
    filter(subject_id == id_i, seed != seed_i, log_numerosity == log_num_i)
  partner_same_display_diff[i] <- pool$log_response[sample(nrow(pool), 1)]
  
}

# Attach partner columns to geometric_means_log (one row per subject × seed)
geometric_means_log <- geometric_means_log %>%
  left_join(
    first_estimates %>%
      select(subject_id, seed) %>%
      mutate(
        partner_diff_same_display = partner_diff_same_display,
        partner_diff_display_diff = partner_diff_display_diff,
        partner_same_display_diff = partner_same_display_diff
      ),
    by = c("subject_id", "seed")
  )

# ── 7. Extract and save metacognition data ────────────────────────────────────

metacognition_df <- dataset %>%
  filter(trial_type == "survey-multi-choice",
         subject_id %in% unique(filtered_data$subject_id)) %>%
  mutate(
    which_average_better = sapply(response, \(x) as.character(x$averageEstimates)),
    which_individual_better = sapply(response, \(x) as.character(x$firstOrSecond))
  ) %>%
  select(subject_id, which_average_better, which_individual_better) %>%
  mutate(
    which_average_better = if_else(
      which_average_better ==
        "When the two displays occured back-to-back, in immediate succession.",
      "paired", "unpaired"
    ),
    which_individual_better = if_else(
      which_individual_better == "The second time the objects were shown",
      "second", "first"
    )
  )

# ── 8. Save ───────────────────────────────────────────────────────────────────

write.csv(geometric_means_log, 
          "data_processed/E1_geometric_means_log.csv", 
          row.names = FALSE)
write.csv(geometric_means_raw, 
          "data_processed/E1_geometric_means_raw.csv", 
          row.names = FALSE)
write.csv(filtered_data, 
          "data_processed/E1_trials.csv",              
          row.names = FALSE)
write.csv(metacognition_df, 
          "data_processed/E1_metacognition.csv",
          row.names = FALSE)
