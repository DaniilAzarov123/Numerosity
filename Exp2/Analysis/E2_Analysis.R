# =============================================================================
# Experiment 2 — Data Analysis
#
# Reads the CSVs produced by E2_DataPreparation.R.
#
# Analyses are:
#   1.  Regression to the mean
#   2.  Similarity between estimates
#   3.  Wisdom of the Inner Crowd (WoIC)
#   4.  WoIC magnitude × numerosity (quadratic)
#   5.  Wisdom of the Crowd (WoC)
#   6.  Direction of the second estimate
#   7.  Within- vs cross-seed similarity
#   8.  Metacognition
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

# ── Appearance labels (reused across plots) ───────────────────────────────────

appearance_labels <- c(
  "samePos_sameObj" = "Same Positions, Same Categories",
  "samePos_diffObj" = "Same Positions, Different Categories",
  "diffPos_sameObj" = "Different Positions, Same Categories",
  "diffPos_diffObj" = "Different Positions, Different Categories"
)

appearance_colors <- c(
  "samePos_sameObj" = "green",
  "samePos_diffObj" = "orange",
  "diffPos_sameObj" = "red",
  "diffPos_diffObj" = "blue"
)

appearance_levels <- c("samePos_sameObj", "samePos_diffObj",
                       "diffPos_sameObj", "diffPos_diffObj")

# ── Load data ─────────────────────────────────────────────────────────────────

# Log-scale summary
gm_log <- read.csv("data_processed/E2_geometric_means_log.csv") %>%
  mutate(appearance = factor(appearance, levels = appearance_levels))

# Raw-scale summary
gm_raw <- read.csv("data_processed/E2_geometric_means_raw.csv") %>%
  mutate(appearance = factor(appearance, levels = appearance_levels))

# Trial-level data
trials <- read.csv("data_processed/E2_trials.csv") %>%
  mutate(appearance = factor(appearance, levels = appearance_levels))

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

model_regression <- lmer(log_response ~ log_mean_numerosity + (1 | subject_id),
                         data = trials)

summary(model_regression)
linearHypothesis(model_regression, "log_mean_numerosity = 1")
car::Anova(model_regression, type = 2)
ranova(model_regression)
performance(model_regression)
effectsize::eta_squared(model_regression)

# Plot
p1 <- ggplot(trials, aes(x = log_mean_numerosity, y = log_response)) +
  geom_smooth(se = FALSE, method = "glm", linewidth = 2, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.02) +
  geom_abline(slope = 1, intercept = 0, linewidth = 2,
              linetype = "dashed", color = "red") +
  annotate("text", x = 4.8, y = 4.8, label = "y = x",
           color = "red", angle = 40, hjust = 0, vjust = -0.5, size = 7) +
  scale_x_continuous(breaks = seq(3.9, 5.1, 0.2)) +
  scale_y_continuous(breaks = seq(3.9, 5.1, 0.2)) +
  labs(x = "Mean Numerosity (log)", y = "Estimate (log)") +
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

# Spearman correlations per appearance condition
gm_raw %>%
  group_by(appearance) %>%
  cor_test(first_estimate_raw, second_estimate_raw, method = "spearman")

# Mixed model on |Est1 - Est2|
model_similarity <- lmer(
  estimate_difference_raw ~ appearance + 
    (1 | mean_numerosity) + (1 | subject_id),
  data = gm_raw
)

summary(model_similarity)
car::Anova(model_similarity, type = 2)
ranova(model_similarity)
performance(model_similarity)
effectsize::eta_squared(model_similarity)

# Post-hoc contrasts
pairs(emmeans(model_similarity, specs = "appearance"),
      adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(
    sig = case_when(p.value < .001 ~ "***",
                    p.value < .01 ~ "**",
                    p.value < .05 ~ "*",
                    TRUE ~ ""),
    cohen_d = sqrt(1 / n_subj) * estimate / SE
  )

# Plot
p2 <- ggplot(gm_raw,
       aes(x = mean_numerosity, y = estimate_difference_raw,
           color = appearance, group = appearance)) +
  stat_summary(fun = mean, geom = "point", size = 5, alpha = 0.8,
               position = position_dodge(10)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.8,
               position = position_dodge(10)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, alpha = 0.8, position = position_dodge(10)) +
  scale_color_manual(values = appearance_colors,
                     labels = appearance_labels) +
  scale_x_continuous(limits = c(40, 160), breaks = seq(50, 150, 25)) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(x = "Mean Numerosity",
       y = expression("|Estimate"[1] ~ "-" ~ "Estimate"[2] ~ "|")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 25, color = "black"),
        legend.title = element_blank(),
        legend.position = 'top',
        axis.line = element_line(linewidth = 1, color = "black"))+
  guides(color = guide_legend(ncol = 2))
p2

# Save plot if needed
# ggsave("plots/similarity.png", plot = p2,
#        width = 16, height = 8, dpi = 600)

# ── 3. Wisdom of the Inner Crowd (WoIC) ──────────────────────────
#
# Is the geometric mean more accurate than individual estimates, and does
# this depend on display appearance?
# Errors are in log space (Weber's law; multiplicative noise).

model_woic <- lmer(abs_error_log ~ estimate_type * appearance + (1 | subject_id),
                   data = df_long_log)

summary(model_woic)
car::Anova(model_woic, type = 2)
ranova(model_woic)
performance(model_woic)
effectsize::eta_squared(model_woic)

# Post-hoc contrasts
contrast(
  emmeans(model_woic, ~ estimate_type * appearance),
  method = list(
    
    # WoIC effect within each appearance condition
    "GeomMean vs avg(Est1,Est2): samePos_sameObj" = c(1, -0.5, -0.5, 0, 0, 0,
                                                      0, 0, 0, 0, 0, 0),
    "GeomMean vs avg(Est1,Est2): samePos_diffObj" = c(0, 0, 0, 1, -0.5, -0.5,
                                                      0, 0, 0, 0, 0, 0),
    "GeomMean vs avg(Est1,Est2): diffPos_sameObj" = c(0, 0, 0, 0, 0, 0,
                                                      1, -0.5, -0.5, 0, 0, 0),
    "GeomMean vs avg(Est1,Est2): diffPos_diffObj" = c(0, 0, 0, 0, 0, 0,
                                                      0, 0, 0, 1, -0.5, -0.5),
    
    # GeomMean pairwise across appearance conditions
    "GeomMean: samePos_sameObj vs samePos_diffObj" = c(1, 0, 0, -1, 0, 0,
                                                       0, 0, 0, 0, 0, 0),
    "GeomMean: samePos_sameObj vs diffPos_sameObj" = c(1, 0, 0, 0, 0, 0,
                                                       -1, 0, 0, 0, 0, 0),
    "GeomMean: samePos_sameObj vs diffPos_diffObj" = c(1, 0, 0, 0, 0, 0,
                                                       0, 0, 0, -1, 0, 0),
    "GeomMean: samePos_diffObj vs diffPos_sameObj" = c(0, 0, 0, 1, 0, 0,
                                                       -1, 0, 0, 0, 0, 0),
    "GeomMean: samePos_diffObj vs diffPos_diffObj" = c(0, 0, 0, 1, 0, 0,
                                                       0, 0, 0, -1, 0, 0),
    "GeomMean: diffPos_sameObj vs diffPos_diffObj" = c(0, 0, 0, 0, 0, 0,
                                                       1, 0, 0, -1, 0, 0),
    
    # Est1 vs Est2 within each appearance condition
    "Est1 vs Est2: samePos_sameObj" = c(0, 1, -1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0),
    "Est1 vs Est2: samePos_diffObj" = c(0, 0, 0, 0, 1, -1,
                                        0, 0, 0, 0, 0, 0),
    "Est1 vs Est2: diffPos_sameObj" = c(0, 0, 0, 0, 0, 0,
                                        0, 1, -1, 0, 0, 0),
    "Est1 vs Est2: diffPos_diffObj" = c(0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 1, -1)
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
           group = appearance, color = appearance)) +
  stat_summary(fun = mean, geom = "point", size = 8,
               position = position_dodge(0.4)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.35,
               position = position_dodge(0.4)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1,
               position = position_dodge(0.4)) +
  scale_color_manual(values = appearance_colors,
                     labels = appearance_labels) +
  scale_x_discrete(
    breaks = c("geom_mean_error_log", "first_error_log", "second_error_log"),
    labels = c("Geometric Mean",
               expression(1^st ~ Estimate),
               expression(2^nd ~ Estimate))
  ) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(y = "Absolute Error (log)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 30, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.35, 0.85),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1, color = "black"))
p3

# Save plot if needed
# ggsave("plots/woic.png", plot = p3,
#        width = 14, height = 8, dpi = 600)

# ── 4. WoIC magnitude × numerosity (quadratic) ─────────────────
#
# Does aggregation benefit more when individual estimates straddle the 
# truth compared to when individual estimates are biased?
# If so, we expect an inverse U-shape function of WoIC effect (operationalized
# as the difference between the average of two individual errors and 
# the aggregated estimate error) of numerosity.

model_woic_magnitude <- lmer(
  woic_magnitude_raw ~ appearance * (c_mean_numerosity + I(c_mean_numerosity^2)) +
    (1 | subject_id),
  data = gm_raw
)

summary(model_woic_magnitude)
car::Anova(model_woic_magnitude, type = 2)
ranova(model_woic_magnitude)
performance(model_woic_magnitude)
effectsize::eta_squared(model_woic_magnitude)

# Test whether the quadratic term is significant within each appearance condition
linearHypothesis(model_woic_magnitude,
  "I(c_mean_numerosity^2) + appearancesamePos_diffObj:I(c_mean_numerosity^2) = 0")
linearHypothesis(model_woic_magnitude,
  "I(c_mean_numerosity^2) + appearancediffPos_sameObj:I(c_mean_numerosity^2) = 0")
linearHypothesis(model_woic_magnitude,
  "I(c_mean_numerosity^2) + appearancediffPos_diffObj:I(c_mean_numerosity^2) = 0")

# Plot
p4 <- ggplot(gm_raw, aes(x = mean_numerosity, y = woic_magnitude_raw,
                   color = appearance)) +
  stat_summary(fun = mean, geom = "point", size = 8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, width = 0.1) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  facet_wrap(~ appearance, nrow = 2,
             labeller = labeller(appearance = appearance_labels)) +
  scale_color_manual(values = appearance_colors) +
  labs(x = "Mean Numerosity (raw)", y = "WoIC Magnitude (raw)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        strip.text = element_text(size = 25, color = "black"),
        axis.line = element_line(linewidth = 1, color = "black"),
        legend.position = "none")
p4

# Save plot if needed
# ggsave("plots/woic_magnitude_quadratic.png", plot = p4,
#        width = 18, height = 10, dpi = 600)

# ── 5. Wisdom of the Crowd (WoC) ─────────────────────────────────
#
# Compare aggregation accuracy across individuals (WoC) vs within individuals
# (WoIC), separately for each appearance condition.
# The between-person partner (same display, different participant) was sampled
# in the data preparation script and stored in gm_log$partner_between.
# The within-person partner is second_estimate_log (already in gm_log).

woc_data <- gm_log %>%
  mutate(
    gm_between = rowMeans(cbind(first_estimate_log, partner_between)),
    gm_within = rowMeans(cbind(first_estimate_log, second_estimate_log)),
    error_between = abs(gm_between - log_mean_numerosity),
    error_within = abs(gm_within - log_mean_numerosity)
  )

df_long_woc <- woc_data %>%
  select(subject_id, log_mean_numerosity, appearance,
         error_between, error_within) %>%
  pivot_longer(
    cols = c(error_between, error_within),
    names_to = "crowd_type",
    values_to = "abs_error_log"
  ) %>%
  mutate(crowd_type = factor(crowd_type,
                             levels = c("error_between", "error_within")))

model_woc <- lmer(abs_error_log ~ crowd_type * appearance + (1 | subject_id),
                  data = df_long_woc)

summary(model_woc)
car::Anova(model_woc, type = 2)
ranova(model_woc)
performance(model_woc)
effectsize::eta_squared(model_woc)

# Post-hoc contrasts
contrast(
  emmeans(model_woc, ~ crowd_type * appearance),
  method = list(
    
    # Between vs Within averaged over all appearance conditions
    "Between vs Within (avg over appearance)" = c(0.25, -0.25, 0.25, -0.25,
                                                  0.25, -0.25, 0.25, -0.25),
    
    # Between vs Within within each appearance condition
    "Between vs Within: samePos_sameObj" = c(1, -1, 0, 0, 0, 0, 0, 0),
    "Between vs Within: samePos_diffObj" = c(0, 0, 1, -1, 0, 0, 0, 0),
    "Between vs Within: diffPos_sameObj" = c(0, 0, 0, 0, 1, -1, 0, 0),
    "Between vs Within: diffPos_diffObj" = c(0, 0, 0, 0, 0, 0, 1, -1),
    
    # Same vs different positions (avg over objects), within between
    "Same vs Diff positions (between, avg obj)" = c(0.5, 0, 0.5, 0, 
                                                    -0.5, 0, -0.5, 0),
    
    # Same vs different objects (avg over positions), within between
    "Same vs Diff objects (between, avg pos)" = c(0.5, 0, -0.5, 0,
                                                  0.5, 0, -0.5, 0),
    
    # Same vs different positions (avg over objects), within within
    "Same vs Diff positions (within, avg obj)" = c(0, 0.5, 0, 0.5,
                                                   0, -0.5,  0, -0.5),
    
    # Same vs different objects (avg over positions), within within
    "Same vs Diff objects (within, avg pos)" = c(0, 0.5, 0, -0.5,
                                                 0, 0.5, 0, -0.5)
  ),
  adjust = "bonferroni"
) %>%
  as.data.frame() %>%
  mutate(
    sig     = case_when(p.value < .001 ~ "***",
                        p.value < .01 ~ "**",
                        p.value < .05 ~ "*",
                        TRUE ~ ""),
    cohen_d = sqrt(1 / n_subj) * estimate / SE
  )

# Plot
p5 <- ggplot(df_long_woc,
       aes(x = crowd_type, y = abs_error_log, color = appearance)) +
  stat_summary(fun = mean, geom = "point", size = 8, alpha = 0.8,
               position = position_dodge(0.6)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, alpha = 0.8, width = 0.35,
               position = position_dodge(0.6)) +
  scale_color_manual(values = appearance_colors,
                     labels = appearance_labels) +
  scale_x_discrete(breaks = c("error_between", "error_within"),
                   labels = c("Across Individuals", "Within Individuals")) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(y = "Absolute Error (log)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 30, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.32, 0.8),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1, color = "black"))
p5

# Save plot if needed
# ggsave("plots/woc_display.png", plot = p5,
#        width = 16, height = 8, dpi = 600)

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
  select(subject_id, seed, mean_numerosity, appearance, which,
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
      # Same sign, overestimate that got smaller
      dev_first > 0 & dev_second < dev_first ~ "Correct",
      # Same sign, underestimate that got smaller in magnitude
      dev_first < 0 & dev_second > dev_first ~ "Correct",
      # No change
      dev_first == dev_second ~ "Same",
      # All other cases: moved further away
      TRUE ~ "Wrong"
    ),
    direction = factor(direction, levels = c("Correct", "Wrong", "Same"))
  )

df_direction_summary <- df_direction %>%
  group_by(subject_id, appearance, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(subject_id, appearance, direction, fill = list(n = 0))

model_direction <- glmmTMB(
  n ~ direction * appearance + (1 | subject_id),
  family = nbinom2, # zero-inflated
  data = df_direction_summary
)

summary(model_direction)
drop1(model_direction, test = "Chisq",
      scope = ~ direction * appearance)

# Post-hoc contrasts
contrast(
  emmeans(model_direction, ~ direction * appearance),
  method = list(
    
    # Direction comparisons averaged over appearance
    "Correct vs Wrong (avg over appearance)" = c(0.25, -0.25, 0, 0.25, -0.25, 0,
                                                 0.25, -0.25, 0, 0.25, -0.25, 0),
    "Correct vs Same (avg over appearance)" = c(0.25, 0, -0.25, 0.25, 0, -0.25,
                                                0.25, 0, -0.25, 0.25, 0, -0.25),
    "Wrong vs Same (avg over appearance)" = c(0, 0.25, -0.25, 0, 0.25, -0.25,
                                              0, 0.25, -0.25, 0, 0.25, -0.25),
    
    # Correct pairwise across appearance conditions
    "Correct: samePos_sameObj vs samePos_diffObj" = c(1, 0, 0, -1, 0, 0,
                                                      0, 0, 0, 0, 0, 0),
    "Correct: samePos_sameObj vs diffPos_sameObj" = c(1, 0, 0, 0, 0, 0,
                                                      -1, 0, 0, 0, 0, 0),
    "Correct: samePos_sameObj vs diffPos_diffObj" = c(1, 0, 0, 0, 0, 0,
                                                      0, 0, 0, -1, 0, 0),
    "Correct: samePos_diffObj vs diffPos_sameObj" = c(0, 0, 0, 1, 0, 0,
                                                      -1, 0, 0, 0, 0, 0),
    "Correct: samePos_diffObj vs diffPos_diffObj" = c(0, 0, 0, 1, 0, 0,
                                                      0, 0, 0, -1, 0, 0),
    "Correct: diffPos_sameObj vs diffPos_diffObj" = c(0, 0, 0, 0, 0, 0,
                                                      1, 0, 0, -1, 0, 0)
  ),
  adjust = "bonferroni"
) %>%
  as.data.frame() %>%
  mutate(
    sig = case_when(p.value < .001 ~ "***",
                    p.value < .01  ~ "**",
                    p.value < .05  ~ "*",
                    TRUE ~ ""),
    cohen_d = sqrt(1 / n_subj) * estimate / SE
  )

# Plot
p6 <- ggplot(df_direction_summary,
       aes(x = direction, y = n, color = appearance)) +
  stat_summary(fun = mean, geom = "point", size = 5, alpha = 0.8,
               position = position_dodge(0.6)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               linewidth = 2, alpha = 0.8, width = 0.35,
               position = position_dodge(0.6)) +
  scale_color_manual(values = appearance_colors,
                     labels = appearance_labels) +
  scale_x_discrete(breaks = c("Correct", "Wrong", "Same"),
                   labels = c("Correct", "Incorrect", "Same")) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(x = expression("Direction of 2"^"nd" * " Estimate"),
       y = "Count") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 30, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.35, 0.2),
        axis.line = element_line(linewidth = 1, color = "black"))
p6

# Save plot if needed
# ggsave("plots/dir_second_est.png", plot = p6,
#        width = 14, height = 8, dpi = 600)

# ── 7. Within- vs cross-seed similarity ─────────────────────────
#
# Within each appearance condition, do participants give more similar estimates
# to the exact same trial (same seed) vs a different trial with the same mean
# numerosity (different seed)? If display variability within a trial drives
# within-person sampling, same-seed estimates should be more similar.
#
# For each row, a cross-seed partner is sampled from a different seed but the
# same subject and same mean numerosity (within the same appearance condition).

gm_raw_similarity <- gm_raw %>%
  select(subject_id, seed, mean_numerosity, appearance,
         first_estimate_raw, second_estimate_raw) %>%
  mutate(within_similarity = abs(first_estimate_raw - second_estimate_raw))


# Sample a cross-seed partner for each row within subject × appearance
set.seed(1234)

gm_raw_similarity <- gm_raw_similarity %>%
  group_by(subject_id, appearance) %>%
  group_modify(~ {
    df <- .x
    cross_partner <- numeric(nrow(df))
    for (i in seq_len(nrow(df))) {
      candidates <- which(df$mean_numerosity == df$mean_numerosity[i] &
                            df$seed != df$seed[i])
      if (length(candidates) == 0) {
        cross_partner[i] <- NA_real_
      } else {
        j <- sample(candidates, 1)
        cross_partner[i] <- df$second_estimate_raw[j]
      }
    }
    df$cross_similarity <- abs(df$first_estimate_raw - cross_partner)
    df
  }) %>%
  ungroup()

df_long_similarity <- gm_raw_similarity %>%
  select(subject_id, appearance, mean_numerosity,
         within_similarity, cross_similarity) %>%
  pivot_longer(
    cols = c(within_similarity, cross_similarity),
    names_to = "display_type",
    values_to = "similarity"
  ) %>%
  mutate(display_type = factor(display_type,
                               levels = c("within_similarity", "cross_similarity")))

model_similarity_seed <- lmer(
  similarity ~ appearance * display_type + 
    (1 | mean_numerosity) + (1 | subject_id),
  data = df_long_similarity
)

summary(model_similarity_seed)
car::Anova(model_similarity_seed, type = 2)
ranova(model_similarity_seed)
performance(model_similarity_seed)
effectsize::eta_squared(model_similarity_seed)

# Plot
p7 <- ggplot(df_long_similarity,
       aes(x = mean_numerosity, y = similarity,
           color = display_type, group = display_type)) +
  stat_summary(fun = mean, geom = "point", size = 5, alpha = 0.8,
               position = position_dodge(3)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.8,
               position = position_dodge(3)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               linewidth = 2, alpha = 0.8,
               position = position_dodge(3)) +
  facet_wrap(~ appearance, nrow = 2,
             labeller = labeller(appearance = appearance_labels)) +
  scale_color_manual(breaks = c("within_similarity", "cross_similarity"),
                     labels = c("Same Display", "Different Display"),
                     values = c("pink", "brown"),
                     name   = "Display") +
  labs(x = "Mean Numerosity",
       y = expression("|Estimate"[1] ~ "-" ~ "Estimate"[2] ~ "|")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        legend.text = element_text(size = 25, color = "black"),
        legend.title = element_blank(),
        legend.position = 'top',
        axis.line = element_line(linewidth = 1, color = "black"),
        strip.text = element_text(size = 25, color = "black"))+
  guides(color = guide_legend(ncol = 2))
p7

# Save plot if needed
# ggsave("plots/similarity_seed.png", plot = p7,
#        width = 18, height = 12, dpi = 600)

# ── 8. Metacognition ──────────────────────────────────────────────────────────
#
# Q1: Which estimates were more accurate — first half, second half, or average?
# Q2: Which object category was hardest to estimate?
# Q3: Which object category was easiest to estimate?

metacognition_df <- read.csv("data_processed/E2_metacognition.csv") %>%
  mutate(across(starts_with("Q"), as.factor))

# Q1: first half vs second half vs average
table(metacognition_df$Q1_resp) / nrow(metacognition_df)
chisq.test(table(metacognition_df$Q1_resp))

# Q2: most difficult category
table(metacognition_df$Q2_resp) / nrow(metacognition_df)
chisq.test(table(metacognition_df$Q2_resp))

# Q3: easiest category
table(metacognition_df$Q3_resp) / nrow(metacognition_df)
chisq.test(table(metacognition_df$Q3_resp))

# Cross-tabulation of Q2 and Q3
with(metacognition_df, table(Q2_resp, Q3_resp))

# Plot Q1
p8 <- ggplot(metacognition_df, aes(x = Q1_resp)) +
  geom_bar(fill = "blue", alpha = 0.8) +
  scale_x_discrete(breaks = c("Average", "First_half", "Second_half"),
                   labels = c("Average",
                              expression(1^st ~ Half),
                              expression(2^nd ~ Half))) +
  labs(y = "Number of Participants") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1, color = "black"))
p8

# Save plot if needed
# ggsave("plots/q1.png", plot = p8,
#        width = 12, height = 8, dpi = 600)

# Plot Q2 and Q3 together
q2_q3_df_long_summary <- metacognition_df %>%
  pivot_longer(cols = c(Q2_resp, Q3_resp),
               names_to  = "Question",
               values_to = "Response") %>%
  mutate(Response = factor(Response,
                           levels = c("jellyBeans", "mints", "bears",
                                      "crackers", "rocks", "fruits",
                                      "letters", "animalCrackers", "None")))

p9 <- ggplot(q2_q3_df_long_summary, aes(Response, fill = Question)) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_manual(breaks = c("Q2_resp", "Q3_resp"),
                    labels = c("Most difficult to estimate?",
                               "Easiest to estimate?"),
                    values = c("red", "blue")) +
  labs(x = "Category", y = "Number of Participants") +
  theme_minimal() +
  theme(axis.text = element_text(size = 35, color = "black"),
        axis.title = element_text(size = 35, color = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.25, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 35, color = "black"),
        axis.line = element_line(linewidth = 1, color = "black"))
p9

# Save plot if needed
# ggsave("plots/q2_q3.png", plot = p9,
#        width = 14, height = 8, dpi = 600)
