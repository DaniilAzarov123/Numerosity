# =============================================================================
# Experiment 1 — Data Analysis
#
# Reads the four CSVs produced by E1_DataPreparation.R.
# Analyses are:
#   1. Regression to the mean
#   2. Similarity between estimates
#   3. Wisdom of the Inner Crowd (WoIC)
#   4. WoIC magnitude × numerosity (quadratic)
#   5. Wisdom of the Crowd (WoC)
#   6. Direction of the second estimate
#   7. Metacognition
#   8. Supplementary: Permutation test (similarity of individual estimates)
#   9. Supplementary: Fatigue effect
#
# =============================================================================

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(performance)
library(effectsize)
library(emmeans)
library(glmmTMB)
library(rstatix)

# ── Match Type labels (reused across plots) ───────────────────────────────────

matchType_labels <- c(
  "paired" = "Paired",
  "unpaired" = "Unpaired"
)

matchType_colors <- c(
  "paired" = "red",
  "unpaired" = "blue"
)


# ── Load data ─────────────────────────────────────────────────────────────────

# Log-scale summary
gm_log <- read.csv("data_processed/E1_geometric_means_log.csv") %>%
  mutate(matchType = factor(matchType, levels = c("paired", "unpaired")))

# Raw-scale summary
gm_raw <- read.csv("data_processed/E1_geometric_means_raw.csv") %>%
  mutate(matchType = factor(matchType, levels = c("paired", "unpaired")))

# Trial-level data
trials <- read.csv("data_processed/E1_trials.csv") %>%
  mutate(matchType = factor(matchType, levels = c("paired", "unpaired")),
         sprites = as.factor(sprites))

# Long format of log-scale errors (one row per estimate type per trial pair)
df_long_log <- gm_log %>%
  pivot_longer(
    cols = c(first_error_log, second_error_log, geom_mean_error_log),
    names_to = "estimate_type",
    values_to = "abs_error_log"
  ) %>%
  mutate(estimate_type = factor(estimate_type,
                                levels = c("geom_mean_error_log",
                                           "first_error_log",
                                           "second_error_log")))

n_subj <- length(unique(trials$subject_id))

# ── 1. Regression to the mean ─────────────────────────────────────
#
# A slope significantly below 1 indicates regression to the mean: small
# numerosities are overestimated and large ones are underestimated.
# Both response and numerosity are log-transformed.

model_regression <- lmer(log_response ~ log_numerosity + (1 | subject_id),
                         data = trials)

summary(model_regression)
linearHypothesis(model_regression, "log_numerosity = 1")
car::Anova(model_regression, type = 2)
ranova(model_regression)
performance(model_regression)
effectsize::eta_squared(model_regression)

# Plot
p1 <- ggplot(trials, aes(x = log_numerosity, y = log_response)) +
  geom_smooth(se = FALSE, method = "glm", 
              linewidth = 2, linetype = "dashed",
              color = 'black', alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.02) +
  geom_abline(slope = 1, intercept = 0, linewidth = 2,
              linetype = "dashed", color = "gray") +
  annotate("text", x = 4.82, y = 4.82, label = "y = x",
           color = "black", angle = 36, hjust = 0, vjust = -0.5, size = 7) +
  scale_x_continuous(breaks = seq(3.9, 5.1, 0.2)) +
  scale_y_continuous(breaks = seq(3.9, 5.1, 0.2)) +
  labs(x = "Numerosity (log)", y = "Estimate (log)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        axis.line = element_line(linewidth = 1, color = "black"))
p1

# Save plot if needed
# ggsave("plots/regression_to_mean.png", plot = p1,
#        width = 10, height = 8, dpi = 600)

# ── 2. Similarity between estimates ───────────────────────────────
#
# |Est1 - Est2| is computed on the raw scale because we want the difference
# in actual object counts.

# Spearman correlation between Est1 and Est2 per condition
gm_raw %>%
  group_by(matchType) %>%
  cor_test(first_estimate_raw, second_estimate_raw, method = "spearman")

# Mixed model on |Est1 - Est2| (raw scale)
model_similarity <- lmer(estimate_difference_raw ~ matchType + 
                           (1 | numerosity) + (1 | subject_id),
                         data = gm_raw)

summary(model_similarity)
car::Anova(model_similarity, type = 2)
performance(model_similarity)
ranova(model_similarity)
effectsize::eta_squared(model_similarity)

# Plot
p2 <- ggplot(gm_raw,
       aes(x = numerosity, y = estimate_difference_raw,
           color = matchType, group = matchType)) +
  stat_summary(fun = mean, geom = "point", size = 8,
               position = position_dodge(0)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1,
               position = position_dodge(0)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, position = position_dodge(0)) +
  scale_x_continuous(breaks = sort(unique(gm_raw$numerosity))) +
  scale_y_continuous(breaks = seq(7.5, 17.5, 2.5)) +
  scale_color_manual(values = matchType_colors,
                     labels = matchType_labels) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(x = "Numerosity (raw)",
       y = expression("|Estimate"[1] ~ "-" ~ "Estimate"[2] ~ "|")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 35, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.9),
        axis.line = element_line(linewidth = 1, color = "black"))
p2

# Save plot if needed
# ggsave("plots/similarity.png", plot = p2,
#        width = 12, height = 8, dpi = 600)

# ── 3. Wisdom of the Inner Crowd (WoIC) ──────────────────────────
#
# Is the geometric mean of two estimates more accurate than individual
# estimates, and does this depend on temporal pairing?
# Errors are in log space because numerosity estimation noise is multiplicative
# (Weber's law); log-scale errors are therefore approximately homoscedastic.

model_woic <- lmer(abs_error_log ~ estimate_type * matchType + (1 | subject_id),
                   data = df_long_log)

summary(model_woic)
car::Anova(model_woic, type = 2)
ranova(model_woic)
performance(model_woic)
effectsize::eta_squared(model_woic)

# Post-hoc contrasts
contrast(
  emmeans(model_woic, ~ estimate_type * matchType),
  method = list(
    "GeomMean: paired vs unpaired" = c(1,  0,  0, -1,  0,  0),
    "GeomMean vs avg(Est1,Est2): paired" = c(1, -0.5, -0.5,  0,  0,  0),
    "GeomMean vs avg(Est1,Est2): unpaired" = c(0,  0,  0,  1, -0.5, -0.5),
    "Est1 vs Est2: paired" = c(0,  1, -1,  0,  0,  0),
    "Est1 vs Est2: unpaired" = c(0,  0,  0,  0,  1, -1)
  ),
  adjust = "bonferroni"
) %>%
  as.data.frame() %>%
  mutate(
    sig = case_when(p.value < .001 ~ "***",
                    p.value < .01 ~ "**",
                    p.value < .05 ~ "*",
                    TRUE ~ ""),
    cohen_d = sqrt(1 / n_subj) * estimate / SE
  )

# Plot
p3 <- ggplot(df_long_log,
       aes(x = estimate_type, y = abs_error_log,
           group = matchType, color = matchType)) +
  stat_summary(fun = mean, geom = "point", size = 8,
               position = position_dodge(0.4)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.35,
               position = position_dodge(0.4)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1,
               position = position_dodge(0.4)) +
  scale_x_discrete(
    breaks = c("first_error_log", "second_error_log", "geom_mean_error_log"),
    labels = c(expression(1^st ~ Estimate),
               expression(2^nd ~ Estimate),
               "Geometric Mean")
  ) +
  scale_color_manual(values = matchType_colors,
                     labels = matchType_labels) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(y = "Absolute Error (log)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 35, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.2),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1, color = "black"))
p3

# Save plot if needed
# ggsave("plots/woic.png", plot = p3,
#        width = 12, height = 8, dpi = 600)

# ── 4. WoIC magnitude × numerosity (quadratic) ──────────────
#
# Does aggregation benefit more when individual estimates straddle the 
# truth compared to when individual estimates are biased?
# If so, we expect an inverse U-shape function of WoIC effect (operationalized
# as the difference between the average of two individual errors and 
# the aggregated estimate error) of numerosity.

model_quadratic <- lmer(
  woic_magnitude_raw ~ matchType * (c_numerosity + I(c_numerosity^2)) + 
    (1 | subject_id),
  data = gm_raw
)

summary(model_quadratic)
# Test whether the quadratic term is significant for unpaired trials
linearHypothesis(model_quadratic,
                 "I(c_numerosity^2) + matchTypeunpaired:I(c_numerosity^2) = 0")

# Plot
p4 <- ggplot(gm_raw, aes(x = numerosity, y = woic_magnitude_raw, color = matchType)) +
  stat_summary(fun = mean, geom = "point", size = 8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.1) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  scale_color_manual(values = matchType_colors,
                     labels = matchType_labels) +
  labs(x = "Numerosity (raw)", y = "WoIC Magnitude (raw)",
       color = "Trial Type") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 35, color = "black"),
        legend.position = c(0.9,0.8),
        axis.line = element_line(linewidth = 1, color = "black"))

p4

# Save plot if needed
# ggsave("plots/woic_magnitude_quadratic.png", plot = p4,
#        width = 14, height = 10, dpi = 600)

# ── 5. Wisdom of the Crowd (WoC) ─────────────────────────────────
#
# Compare aggregation accuracy across individuals (WoC) vs within individuals
# (WoIC), separately for same-display and different-display pairings.

woc_data <- gm_log %>%
  mutate(
    gm_between_same = rowMeans(cbind(first_estimate_log, partner_diff_same_display)),
    gm_between_diff = rowMeans(cbind(first_estimate_log, partner_diff_display_diff)),
    gm_within_diff = rowMeans(cbind(first_estimate_log, partner_same_display_diff)),
    gm_within_same = rowMeans(cbind(first_estimate_log, second_estimate_log)),
    error_between_same = abs(gm_between_same - log_numerosity),
    error_between_diff = abs(gm_between_diff - log_numerosity),
    error_within_diff = abs(gm_within_diff - log_numerosity),
    error_within_same = abs(gm_within_same - log_numerosity)
  )

df_long_woc <- woc_data %>%
  select(subject_id, log_numerosity,
         error_between_same, error_between_diff,
         error_within_diff,  error_within_same) %>%
  pivot_longer(
    cols = starts_with("error_"),
    names_to = "condition",
    values_to = "abs_error_log"
  ) %>%
  mutate(
    crowd_type = if_else(str_detect(condition, "between"), "between", "within"),
    display = if_else(str_detect(condition, "same$"), "same", "diff")
  )

model_woc <- lmer(abs_error_log ~ display * crowd_type + (1 | subject_id),
                  data = df_long_woc)

summary(model_woc)
car::Anova(model_woc, type = 2)
ranova(model_woc)
performance(model_woc)
effectsize::eta_squared(model_woc)

# Post-hoc contrasts
contrast(
  emmeans(model_woc, ~ display * crowd_type),
  method = list(
    "Between vs Within (avg over display)" = c( 0.5, 0.5, -0.5, -0.5),
    "Same vs Diff display: between" = c(-1, 1, 0, 0),
    "Same vs Diff display: within" = c(0, 0, -1, 1),
    "Between vs Within: diff display" = c(1, 0, -1, 0),
    "Between vs Within: same display" = c(0, 1, 0, -1)
  ),
  adjust = "bonferroni"
) %>%
  as.data.frame() %>%
  mutate(
    sig = case_when(p.value < .001 ~ "***",
                    p.value < .01 ~ "**",
                    p.value < .05 ~ "*",
                    TRUE ~ ""),
    cohen_d = sqrt(1 / n_subj) * estimate / SE
  )

# Plot
p5 <- ggplot(df_long_woc,
       aes(x = crowd_type, y = abs_error_log, color = display)) +
  stat_summary(fun = mean, geom = "point", size = 8,
               position = position_dodge(0.4)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.35,
               position = position_dodge(0.4)) +
  scale_color_manual(values = c("#FFD700", "#76B7B2"),
                     breaks = c("diff", "same"),
                     labels = c("Different", "Same"),
                     name = "Display") +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_x_discrete(breaks = c("between", "within"),
                   labels = c("Across Individuals", "Within Individuals")) +
  labs(y = "Absolute Error (log)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 35, color = "black"),
        legend.title = element_text(size = 35, color = "black"),
        legend.position = c(0.2, 0.8),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1, color = "black"))
p5

# Save plot if needed
# ggsave("plots/woc_display.png", plot = p5,
#        width = 12, height = 8, dpi = 600)

# ── 6. Direction of the second estimate ───────────────────────────
#
# Are second estimates adjusted more correctly (towards the true numerosity)
# in paired or unpaired condition more?
# If the first estimate was an under-estimation, and second estimate too, 
# but was closer to the truth or straddled the truth or straddled the truth
# producing even a larger absolute error than the first estimate - in all these
# cases second estimate was coded as "in the correct direction" because a 
# subject correctly identified the DIRECTION (regardless of the magnitude)

df_direction <- trials %>%
  select(subject_id, sprites, seed, numerosity, matchType, which,
         signed_deviation_raw) %>%
  pivot_wider(names_from = which,
              values_from = signed_deviation_raw,
              names_prefix = "dev_") %>%
  mutate(
    direction = case_when(
      # Sign flips - crossed the true value
      (dev_first > 0 & dev_second < 0) | (dev_first < 0 & dev_second > 0) ~ "Correct",
      # First was off, second hit exactly
      dev_first != 0 & dev_second == 0 ~ "Correct",
      # Same sign, but second is closer to zero (i.e., closer to truth)
      dev_first < 0 & dev_second > dev_first ~ "Correct",
      dev_first > 0 & dev_second < dev_first ~ "Correct",
      # No change
      dev_first == dev_second ~ "Same",
      # All other cases: moved further away
      TRUE ~ "Wrong"
    ),
    direction = factor(direction, levels = c("Correct", "Wrong", "Same"))
  )

df_direction_summary <- df_direction %>%
  group_by(subject_id, matchType, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(subject_id, matchType, direction, fill = list(n = 0))

model_direction <- glmmTMB(
  n ~ direction * matchType + (1 | subject_id),
  ziformula = ~ 1, # zero-inflated
  family = poisson,
  data = df_direction_summary
)

summary(model_direction)

# Post-hoc contrasts
contrast(
  emmeans(model_direction, ~ direction * matchType),
  method = list(
    "Correct vs Incorrect (avg over matchType)" = c(0.5, -0.5, 0, 0.5, -0.5, 0),
    "Correct vs Same (avg over matchType)" = c(0.5, 0, -0.5, 0.5, 0, -0.5),
    "Incorrect vs Same (avg over matchType)" = c(0, 0.5, -0.5, 0, 0.5, -0.5),
    "Paired vs Unpaired: Correct" = c(1, 0, 0, -1, 0, 0),
    "Paired vs Unpaired: Incorrect" = c(0, 1, 0, 0, -1, 0),
    "Paired vs Unpaired: Same" = c(0, 0, 1, 0, 0, -1)
  ),
  adjust = "bonferroni"
) %>%
  as.data.frame() %>%
  mutate(
    sig = case_when(p.value < .001 ~ "***",
                    p.value < .01 ~ "**",
                    p.value < .05 ~ "*",
                    TRUE ~ ""),
    cohen_d = sqrt(1 / n_subj) * estimate / SE
  )

# Plot
p6 <- ggplot(df_direction_summary,
       aes(x = direction, y = n, color = matchType)) +
  stat_summary(fun = mean, geom = "point", size = 8,
               position = position_dodge(0.4)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.35,
               position = position_dodge(0.4)) +
  scale_color_manual(values = matchType_colors,
                     labels = matchType_labels) +
  scale_x_discrete(breaks = c("Correct", "Wrong", "Same"),
                   labels = c("Correct", "Incorrect", "Same")) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(x = expression("Direction of 2"^"nd" * " Estimate"), y = "Count") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 35, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.2),
        axis.line = element_line(linewidth = 1, color = "black"))
p6

# Save plot if needed
# ggsave("plots/dir_second_est.png", plot = p6,
#        width = 12, height = 8, dpi = 600)

# ── 7. Metacognition ──────────────────────────────────────────────────────────

metacognition_df <- read.csv("data_processed/E1_metacognition.csv")

# Q1: paired vs. unpaired — majority say "paired" (incorrect)
table(metacognition_df$which_average_better) / nrow(metacognition_df)
chisq.test(table(metacognition_df$which_average_better))

# Q2: first vs. second estimate
table(metacognition_df$which_individual_better) / nrow(metacognition_df)
chisq.test(table(metacognition_df$which_individual_better))


# ── 8. SUPPLEMENTARY: Permutation test (similarity of individual estimates) ───
#
# What sources of shared structure drive similarity between Est1 and
# Est2? To answer this, we compute Spearman correlations between Est1 and a
# permuted Est2 under different constraints.
#

N_rep <- 10^4 # reduce for faster computation

gm_raw_permut <- gm_raw %>%
  left_join(
    trials %>%
      distinct(subject_id, seed, sprites),
    by = c("subject_id", "seed")
  )

# == Within-participants permutations ==========
# Est2 is always drawn from the SAME participant.

permutation_rho_within <- function(data, extra_var = NULL) {
  if (is.null(extra_var)) {
    data$group_key <- as.character(data$subject_id)
  } else {
    data$group_key <- paste(data$subject_id, data[[extra_var]], sep = "_")
  }
  groups <- split(data, data$group_key)
  replicate(N_rep, {
    est1 <- c()
    est2 <- c()
    for (g in groups) {
      perm <- sample(nrow(g))
      est1 <- c(est1, g$first_estimate_raw)
      est2 <- c(est2, g$second_estimate_raw[perm])
    }
    cor(est1, est2, method = "spearman")
  })
}

# == Across-participants permutations ==========
# Est2 is always drawn from a DIFFERENT participant.

permutation_rho_across <- function(data, group_var = NULL) {
  if (is.null(group_var)) {
    # No grouping: sample Est2 from any other participant's rows
    subjects <- unique(data$subject_id)
    replicate(N_rep, {
      est2_new <- numeric(nrow(data))
      for (id in subjects) {
        idx  <- which(data$subject_id == id)
        pool <- data$first_estimate_raw[data$subject_id != id]
        est2_new[idx] <- sample(pool, length(idx), replace = TRUE)
      }
      cor(data$first_estimate_raw, est2_new, method = "spearman")
    })
  } else {
    # Grouping: sample Est2 from a different participant in the same group
    data$group_key <- as.character(data[[group_var]])
    groups <- split(data, data$group_key)
    replicate(N_rep, {
      est1_all <- c()
      est2_all <- c()
      for (g in groups) {
        subjects_in_group <- unique(g$subject_id)
        est2_new <- numeric(nrow(g))
        for (id in subjects_in_group) {
          idx  <- which(g$subject_id == id)
          pool <- g$first_estimate_raw[g$subject_id != id]
          if (length(pool) == 0) pool <- g$first_estimate_raw  # fallback for singletons
          est2_new[idx] <- sample(pool, length(idx), replace = TRUE)
        }
        est1_all <- c(est1_all, g$first_estimate_raw)
        est2_all <- c(est2_all, est2_new)
      }
      cor(est1_all, est2_all, method = "spearman")
    })
  }
}

# == Run permuatations ==========
set.seed(100)

# Within participants
rho_within_random <- permutation_rho_within(gm_raw_permut)
rho_within_numerosity <- permutation_rho_within(gm_raw_permut, "numerosity")
rho_within_category <- permutation_rho_within(gm_raw_permut, "sprites")

# Across participants
rho_across_random <- permutation_rho_across(gm_raw_permut)
rho_across_numerosity <- permutation_rho_across(gm_raw_permut, "numerosity")
rho_across_category <- permutation_rho_across(gm_raw_permut, "sprites")
rho_across_display <- permutation_rho_across(gm_raw_permut, "seed")

# == Combine ==========
rho_df_within <- bind_rows(
  data.frame(rho = rho_within_random, condition = "Random"),
  data.frame(rho = rho_within_numerosity, condition = "Same Numerosity"),
  data.frame(rho = rho_within_category, condition = "Same Object Category")
) %>% mutate(facet = "Within Participants")

rho_df_across <- bind_rows(
  data.frame(rho = rho_across_random, condition = "Random"),
  data.frame(rho = rho_across_numerosity, condition = "Same Numerosity"),
  data.frame(rho = rho_across_category, condition = "Same Object Category"),
  data.frame(rho = rho_across_display, condition = "Same Display")
) %>% mutate(facet = "Across Participants")

rho_df_faceted <- bind_rows(rho_df_within, rho_df_across) %>%
  mutate(
    condition = factor(condition,
                       levels = c("Random", "Same Object Category",
                                  "Same Numerosity", "Same Display")),
    facet = factor(facet, levels = c("Within Participants",
                                     "Across Participants"))
  )

rho_df_faceted %>%
  group_by(facet, condition) %>%
  summarise(mean_rho = mean(rho), .groups = "drop")

# Exact line in Within Participants facet only
rho_exact <- cor(gm_raw_permut$first_estimate_raw,
                 gm_raw_permut$second_estimate_raw,
                 method = "spearman")
exact_line_df <- data.frame(
  rho = rho_exact,
  facet = factor("Within Participants",
                 levels = c("Within Participants", "Across Participants"))
)

# == Plot ==========

p7 <- ggplot(rho_df_faceted, aes(x = rho, fill = condition)) +
  geom_histogram(color = "black", alpha = 0.8,
                 position = "identity", bins = 200) +
  geom_vline(data = exact_line_df,
             aes(xintercept = rho),
             color = "#76B7B2", linewidth = 2) +
  facet_wrap(~ facet, ncol = 1) +
  scale_fill_manual(
    values = c("grey", "#D37295", "#FFD700", "#76B7B2"),
    breaks = c("Random", "Same Object Category",
               "Same Numerosity", "Same Display"),
    labels = c("Random",
               "Same Object Category",
               "Same Numerosity",
               "Same Display")
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(-0.1, 1.0)) +
  labs(x = "Spearman's correlation, \u03c1",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 25, color = "black"),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 30, color = "black"),
        legend.text = element_text(size = 20, color = "black",
                                   margin = margin(l = 10)),
        legend.title = element_blank(),
        legend.position = 'top',
        strip.text = element_text(size = 30, color = "black"),
        axis.line = element_line(linewidth = 1, color = "black"))
p7

# Save plot if needed
# ggsave("plots/permutation_cor.png", plot = p7,
#        width = 14, height = 10, dpi = 600)


# ── 9. SUPPLEMENTARY: Fatigue effect ──────────────────────────────────
#
# In the unpaired condition, second estimates were always in the 
# second half of the experiment, but in the paired condition,
# both first and second estimates were equally distributed
# along the experiment length.
#
# This might have led to less accurate second estimates in the 
# unpaired trials. To test that this effect is due to fatigue,
# let's compare paired trials in the first and second halves of
# the experiment.

df_fatigue <- gm_log %>%
  filter(matchType == "paired") %>%
  mutate(which_half = factor(if_else(seed <= 47, "First", "Second"),
                             levels = c("First", "Second"))) %>%
  pivot_longer(
    cols = c(first_error_log, second_error_log, geom_mean_error_log),
    names_to = "estimate_type",
    values_to = "abs_error_log"
  ) %>%
  mutate(estimate_type = factor(estimate_type,
                                levels = c("geom_mean_error_log",
                                           "first_error_log",
                                           "second_error_log")))

model_fatigue <- lmer(abs_error_log ~ estimate_type * which_half + 
                        (1 | subject_id),
                      data = df_fatigue)

summary(model_fatigue)

# Plot
paired_color <- matchType_colors[names(matchType_colors)=="paired"]
p8 <- ggplot(df_fatigue,
             aes(x = estimate_type, y = abs_error_log,
                 group = which_half, alpha = which_half)) +
  stat_summary(fun = mean, geom = "point", size = 8,
               position = position_dodge(0.4), color = paired_color) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.35,
               position = position_dodge(0.4), color = paired_color) +
  stat_summary(fun = mean, geom = "line", linewidth = 1,
               position = position_dodge(0.4), color = paired_color) +
  scale_x_discrete(
    breaks = c("geom_mean_error_log", "first_error_log", "second_error_log"),
    labels = c("Geometric Mean",
               expression(1^st ~ Estimate),
               expression(2^nd ~ Estimate))
  ) +
  scale_alpha_manual(values = c("First" = 1, "Second" = 0.4),
                     labels = c("First", "Second")) +
  labs(y = "Absolute Error (log)", alpha = "Experiment Half") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 25, color = "black"),
        legend.title = element_text(size = 35, color = "black"),
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1, color = "black"))
p8

# Save plot if needed
# ggsave("plots/fatigue.png", plot = p8,
#        width = 12, height = 8, dpi = 600)
