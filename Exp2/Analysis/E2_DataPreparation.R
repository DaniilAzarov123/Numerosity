# =============================================================================
# Experiment 2 — Data Preparation
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
  d <- fromJSON(file_list[i])
  # DataPipe adds an internal_node_id column not present in direct SONA files
  if ("internal_node_id" %in% colnames(d)) d <- dplyr::select(d, -internal_node_id)
  d
}) %>% bind_rows()

# ── 2. Identify test trials ───────────────────────────────────────────────────

numerosity_trials <- dataset %>%
  filter(trial_type == "numerosity-estimation-ensemble",
         callibration == "test")

# ── 3. Mark bad participants ──────────────────────────────────────────────────

# Mean absolute error per participant (raw scale, as output by experiment)
mean_abs_error <- tapply(
  X = numerosity_trials$absDeviation,
  INDEX = numerosity_trials$subject_id,
  FUN = mean, na.rm = TRUE
)

error_threshold <- quantile(mean_abs_error, probs = 0.95)

numerosity_trials <- numerosity_trials %>%
  mutate(bad_subject = if_else(
    mean_abs_error[subject_id] > error_threshold, "bad", "good"
  ))

cat("Excluded participants:", sum(numerosity_trials$bad_subject == "bad" &
      !duplicated(numerosity_trials$subject_id)), "\n")
cat("Retained participants:", length(unique(
      numerosity_trials$subject_id[numerosity_trials$bad_subject == "good"])), "\n")

# ── 4. Pre-process retained trials ───────────────────────────────────────────

filtered_data <- numerosity_trials %>%
  filter(bad_subject == "good",
         appearance %in% c("samePos_sameObj", "samePos_diffObj",
                           "diffPos_sameObj", "diffPos_diffObj")) %>%
  mutate(response = as.numeric(response)) %>%
  filter(!is.na(response)) %>%
  # Expand the per-display numerosity, stimulus, and image names lists 
  # into separate columns
  mutate(numerosity = lapply(numerosity, as.list),
         stimulus = lapply(stimulus, as.list),
         image = lapply(image, as.list)) %>%
  unnest_wider(numerosity, names_sep = "_") %>%
  unnest_wider(stimulus, names_sep = "_") %>%
  unnest_wider(image, names_sep = "_") %>%
  mutate(
    log_response = log(response),
    log_mean_numerosity = log(mean_numerosity),
    # Signed deviation on the RAW scale — needed for direction-of-2nd-estimate
    signed_deviation_raw = response - mean_numerosity,
    appearance = factor(appearance,
                        levels = c("samePos_sameObj", "samePos_diffObj",
                                   "diffPos_sameObj", "diffPos_diffObj"))
  ) %>%
  # Drop unnecessary columns
  select(-success,-timeout,-question_order,
         -failed_images,-failed_audio,-failed_video,
         -trial_type,-url,-view_history,-callibration)

rownames(filtered_data) <- NULL

# ── 5. Compute geometric means and absolute errors ────────────────────────────
#
# We need two versions of the aggregated summary, because different analyses
# operate on different scales: log and raw scale

# -- 5a. Log-scale summary ----------------------------------------------------

geometric_means_log <- filtered_data %>%
  select(subject_id, seed, log_response, log_mean_numerosity, appearance) %>%
  group_by(subject_id, seed) %>%
  summarise(
    log_mean_numerosity = last(log_mean_numerosity),
    appearance = last(appearance),
    first_estimate_log = first(log_response),
    second_estimate_log = last(log_response),
    geometric_mean_log = exp(mean(log(log_response))),
    first_error_log = abs(first_estimate_log - log_mean_numerosity),
    second_error_log = abs(second_estimate_log - log_mean_numerosity),
    geom_mean_error_log = abs(geometric_mean_log - log_mean_numerosity),
    .groups = "drop"
  ) %>%
  mutate(appearance = factor(appearance,
                             levels = c("samePos_sameObj", "samePos_diffObj",
                                        "diffPos_sameObj", "diffPos_diffObj")))

# -- 5b. Raw-scale summary ----------------------------------------------------

geometric_means_raw <- filtered_data %>%
  select(subject_id, seed, response, mean_numerosity, appearance) %>%
  group_by(subject_id, seed) %>%
  summarise(
    mean_numerosity = last(mean_numerosity),
    appearance = last(appearance),
    first_estimate_raw = first(response),
    second_estimate_raw = last(response),
    geometric_mean_raw = exp(mean(log(response))),
    first_error_raw = abs(first_estimate_raw - mean_numerosity),
    second_error_raw = abs(second_estimate_raw - mean_numerosity),
    geom_mean_error_raw = abs(geometric_mean_raw - mean_numerosity),
    .groups = "drop"
  ) %>%
  mutate(
    appearance = factor(appearance,
                        levels = c("samePos_sameObj", "samePos_diffObj",
                                   "diffPos_sameObj", "diffPos_diffObj")),
    estimate_difference_raw = abs(first_estimate_raw - second_estimate_raw),
    mean_individual_error_raw = (first_error_raw + second_error_raw) / 2,
    woic_magnitude_raw = mean_individual_error_raw - geom_mean_error_raw,
    c_mean_numerosity = as.vector(scale(mean_numerosity,
                                        center = TRUE, scale = FALSE))
  )

# ── 6. Add between-person random partners (WoC analysis) ─────────────────────
#
# For each (subject × seed) pair we draw one partner response from a different
# participant viewing the SAME display (same seed). Only first estimates are
# used so each trial pair contributes exactly one row.
# Partner responses are in LOG scale, consistent with the WoIC analysis.

first_estimates <- filtered_data %>% filter(which == "first")

set.seed(1234)

n <- nrow(first_estimates)
partner_between <- numeric(n)

for (i in seq_len(n)) {
  if (i %% 500 == 0) cat(i, " ")

  id_i <- first_estimates$subject_id[i]
  seed_i <- first_estimates$seed[i]

  pool <- first_estimates %>%
    filter(subject_id != id_i, seed == seed_i)
  partner_between[i] <- pool$log_response[sample(nrow(pool), 1)]
}


geometric_means_log <- geometric_means_log %>%
  left_join(
    first_estimates %>%
      select(subject_id, seed) %>%
      mutate(partner_between = partner_between),
    by = c("subject_id", "seed")
  )

# ── 7. Extract and save metacognition data ────────────────────────────────────
#
# Three questions:
#   Q1 (First_Second_Average): which half of estimates was more accurate?
#   Q2 (Difficult_Objects):    which object category was hardest to estimate?
#   Q3 (Easy_Objects):         which object category was easiest to estimate?

metacognition_df <- dataset %>%
  filter(trial_type == "survey-multi-choice",
         subject_id %in% unique(filtered_data$subject_id)) %>%
  select(subject_id, response) %>%
  separate(response,
           into = c("Q1_resp", "Q2_resp", "Q3_resp"),
           sep = ", ", remove = TRUE) %>%
  mutate(across(starts_with("Q"), ~ gsub('.*=\\s*"([^"]*)".*', "\\1", .))) %>%
  mutate(
    Q1_resp = case_when(
      Q1_resp == "The mean of the first and second estimates" ~ "Average",
      Q1_resp == "In the first half" ~ "First_half",
      TRUE ~ "Second_half"
    ),
    Q1_resp = as.factor(Q1_resp),
    Q2_resp = as.factor(Q2_resp),
    Q3_resp = as.factor(Q3_resp)
  )

rownames(metacognition_df) <- NULL

# ── 8. Save ───────────────────────────────────────────────────────────────────

write.csv(geometric_means_log, 
          "data_processed/E2_geometric_means_log.csv", 
          row.names = FALSE)
write.csv(geometric_means_raw, 
          "data_processed/E2_geometric_means_raw.csv", 
          row.names = FALSE)
write.csv(filtered_data,
          "data_processed/E2_trials.csv",
          row.names = FALSE)
write.csv(metacognition_df,
          "data_processed/E2_metacognition.csv",
          row.names = FALSE)
