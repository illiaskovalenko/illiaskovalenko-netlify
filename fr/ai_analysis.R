# =========================================================
# AI Impact in Student Life - Data Loading & Exploration
# =========================================================

# Libraries
library(tidyverse)
library(MASS) # For ordered / multinomial models
library(broom) # Extracting tidy regression models
library(stats) # Clustering

# ---------------------------------------------------------
# 1. Data loading
# ---------------------------------------------------------

tbl_database <- read_csv("database.csv") # source file available on https://www.kaggle.com/datasets/sohaibdevv/ai-and-student-life-2026-the-new-normal

# ---------------------------------------------------------
# 2. Initial inspection
# ---------------------------------------------------------

glimpse(tbl_database)
summary(tbl_database)
any(is.na(tbl_database))

# ---------------------------------------------------------
# 3. Data preparation
# ---------------------------------------------------------

tbl_database <- tbl_database |>
  mutate(
    across(where(is.character) & !contains("ID"), as.factor),
    gpa_diff = GPA_Post_AI - GPA_Baseline
  )

# ---------------------------------------------------------
# 4. GPA difference - global summary
# ---------------------------------------------------------

gpa_summary <- tbl_database |>
  summarise(
    min = min(gpa_diff),
    median = median(gpa_diff),
    mean = mean(gpa_diff),
    sd = sd(gpa_diff),
    max = max(gpa_diff)
  )

gpa_summary

# ---------------------------------------------------------
# 5. GPA difference - distribution & boxplot
# ---------------------------------------------------------

plot_gpa_diff <- tbl_database |>
  ggplot(aes(x = "", y = gpa_diff)) +
  geom_boxplot(fill = "#4C78A8", color = "black", alpha = 0.7) +
  geom_point(color = "black", size = 0.08) +
  labs(
    title = "Statistical distribution of GPA scores' difference",
    x = "",
    y = "GPA difference"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_gpa_diff

# ---------------------------------------------------------
# 6. GPA difference - grouped summaries
# ---------------------------------------------------------

gpa_by_major <- tbl_database |>
  summarise(
    min = min(gpa_diff),
    median = median(gpa_diff),
    mean = mean(gpa_diff),
    sd = sd(gpa_diff),
    max = max(gpa_diff),
    .by = Major
  ) |>
  arrange(desc(mean))

gpa_by_tool <- tbl_database |>
  summarise(
    min = min(gpa_diff),
    median = median(gpa_diff),
    mean = mean(gpa_diff),
    max = max(gpa_diff),
    .by = Primary_AI_Tool
  ) |>
  arrange(desc(mean))

gpa_by_usage <- tbl_database |>
  summarise(
    min = min(gpa_diff),
    median = median(gpa_diff),
    mean = mean(gpa_diff),
    max = max(gpa_diff),
    .by = Main_Usage_Case
  ) |>
  arrange(desc(mean))

gpa_by_major
gpa_by_tool
gpa_by_usage

# =========================================================
# GPA Change Analysis
# =========================================================

# ---------------------------------------------------------
# 1. Distribution diagnostics
# ---------------------------------------------------------

# Q-Q plot (normality check)
qqnorm(tbl_database$gpa_diff)
qqline(tbl_database$gpa_diff, col = "red")

# Histogram
plot_gpa_hist <- tbl_database |>
  ggplot(aes(x = gpa_diff)) +
  geom_histogram(fill = "#4C78A8", alpha = 0.7, bins = 30) +
  labs(
    title = "Distribution of GPA Difference",
    x = "GPA_Post_AI - GPA_Baseline",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

plot_gpa_hist

# Interpretation:
# The distribution is discretized due to rounding effects (GPA score systematically rounded up).
# However, the sample size is large (n = 1500), the paired t-test should remain robust.

# ---------------------------------------------------------
# 2. Visual comparison (before vs after)
# ---------------------------------------------------------

plot_gpa_scatter <- tbl_database |>
  ggplot(aes(x = GPA_Baseline, y = GPA_Post_AI)) +
  geom_jitter(
    width = 0.02,
    height = 0.2,
    alpha = 0.9,
    size = 0.01,
    color = "#4C78A8"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "GPA Before vs After AI Usage",
    subtitle = "Points above the line indicate improvement",
    x = "Baseline GPA",
    y = "Post-AI GPA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

plot_gpa_scatter

# ---------------------------------------------------------
# 3. Statistical tests
# ---------------------------------------------------------

# Paired t-test
t_test <- t.test(
  tbl_database$GPA_Baseline,
  tbl_database$GPA_Post_AI,
  paired = TRUE
)

t_test

# Wilcoxon signed-rank test (non-parametric robustness check)
wilcox_test <- wilcox.test(
  tbl_database$GPA_Post_AI,
  tbl_database$GPA_Baseline,
  paired = TRUE
)

wilcox_test

# Interpretation:
# Both tests indicate a statistically significant increase in GPA.
# Positive differences dominate negative ones across observations.
# Students tend to improve more often than they decline.

# =========================================================
# GPA Modeling
# =========================================================

# ---------------------------------------------------------
# 1. Naive OLS model: GPA difference explained by AI usage variables
# ---------------------------------------------------------

model_gpa_diff <- lm(
  gpa_diff ~ Primary_AI_Tool +
    Task_Frequency_Daily +
    Main_Usage_Case +
    Time_Saved_Hours_Weekly,
  data = tbl_database
)

summary(model_gpa_diff)
tidy(model_gpa_diff)

plot(model_gpa_diff, which = 1)

# Interpretation:
# The model has very low explanatory power (R-squared).
# AI usage variables do not explain GPA change (no predictors with significative p-value).
# Residuals show horizontal bands, reflecting the discretized nature of gpa_diff.

# ---------------------------------------------------------
# 2. Adjusted OLS model: post-AI GPA controlled by baseline GPA
# ---------------------------------------------------------

model_gpa_post <- lm(
  GPA_Post_AI ~ GPA_Baseline +
    Primary_AI_Tool +
    Task_Frequency_Daily +
    Main_Usage_Case +
    Time_Saved_Hours_Weekly,
  data = tbl_database
)

summary(model_gpa_post)
tidy(model_gpa_post)

plot(model_gpa_post, which = 1)

# Interpretation:
# GPA_Post_AI is almost entirely explained by GPA_Baseline (>95%).
# AI-related variables remain non-significant.
# Residual diagnostics reveal discretization and a ceiling effect near maximum GPA.
# A linear model might not be the most suitable to describe the relationship between GPA before and after.

# ---------------------------------------------------------
# 3. Extended OLS model: adding socio-demographic variables (Age, Major)
# ---------------------------------------------------------

model_gpa_post_extended <- lm(
  GPA_Post_AI ~ GPA_Baseline +
    Age +
    Major +
    Primary_AI_Tool +
    Task_Frequency_Daily +
    Main_Usage_Case +
    Time_Saved_Hours_Weekly,
  data = tbl_database
)

summary(model_gpa_post_extended)
tidy(model_gpa_post_extended)

# Interpretation:
# Adding age and major does not substantially improve the model.
# Baseline GPA remains the dominant predictor.
# Other effects are weak and should not be overinterpreted without further evidence.

# ---------------------------------------------------------
# 4. Logistic model: probability of GPA improvement
# ---------------------------------------------------------

tbl_database <- tbl_database |>
  mutate(gpa_improved = as.integer(gpa_diff > 0))

model_gpa_improved <- glm(
  gpa_improved ~ GPA_Baseline +
    Age +
    Major +
    Primary_AI_Tool +
    Task_Frequency_Daily +
    Main_Usage_Case +
    Time_Saved_Hours_Weekly,
  data = tbl_database,
  family = binomial
)

summary(model_gpa_improved)

gpa_improved_results <- tidy(model_gpa_improved) |>
  mutate(
    odds_ratio = exp(estimate)
  )

gpa_improved_results

# Interpretation:
# The logistic model does not reveal robust predictors of GPA improvement.
# Fine Arts shows lower odds of improvement compared with Biology.
# However, this isolated effect should be interpreted cautiously and verified with further evidence.

# =========================================================
# Career Confidence, Ethics, Time Saved & Segmentation
# =========================================================

# ---------------------------------------------------------
# 1. Career confidence and GPA metrics
# ---------------------------------------------------------

# Pearson correlations
career_gpa_cor_pearson <- tbl_database |>
  summarise(
    cor_baseline = cor(GPA_Baseline, Career_Confidence_Score),
    cor_post = cor(GPA_Post_AI, Career_Confidence_Score),
    cor_diff = cor(gpa_diff, Career_Confidence_Score)
  )

career_gpa_cor_pearson

# Spearman correlations
career_gpa_cor_spearman <- tibble(
  variable = c("GPA_Baseline", "GPA_Post_AI", "gpa_diff"),
  correlation = c(
    cor(
      tbl_database$GPA_Baseline,
      tbl_database$Career_Confidence_Score,
      method = "spearman"
    ),
    cor(
      tbl_database$GPA_Post_AI,
      tbl_database$Career_Confidence_Score,
      method = "spearman"
    ),
    cor(
      tbl_database$gpa_diff,
      tbl_database$Career_Confidence_Score,
      method = "spearman"
    )
  )
)

career_gpa_cor_spearman

# Kendall correlation for GPA difference
career_gpa_cor_kendall <- cor(
  tbl_database$gpa_diff,
  tbl_database$Career_Confidence_Score,
  method = "kendall"
)

career_gpa_cor_kendall

# Mean confidence by GPA improvement status
career_by_gpa_improvement <- tbl_database |>
  group_by(gpa_improved) |>
  summarise(mean_confidence = mean(Career_Confidence_Score))

career_by_gpa_improvement


# ---------------------------------------------------------
# 2. Career confidence distribution
# ---------------------------------------------------------

qqnorm(tbl_database$Career_Confidence_Score)
qqline(tbl_database$Career_Confidence_Score, col = "red")

plot_career_confidence_hist <- tbl_database |>
  ggplot(aes(x = Career_Confidence_Score)) +
  geom_histogram(fill = "#4C78A8", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Career Confidence Score",
    x = "Scores",
    y = "Number of occurences"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_career_confidence_hist

# Interpretation:
# Career confidence is evenly distributed and weakly associated with GPA metrics.
# It is a discrete ordinal variable. However, given the sample size, an approximation can be made to allow OLS regression analysis.

# ---------------------------------------------------------
# 3. Career confidence model
# ---------------------------------------------------------

model_career_confidence <- lm(
  Career_Confidence_Score ~ GPA_Post_AI +
    Age +
    Major +
    Primary_AI_Tool +
    Task_Frequency_Daily +
    Main_Usage_Case +
    Time_Saved_Hours_Weekly,
  data = tbl_database
)

summary(model_career_confidence)
tidy(model_career_confidence)

# Interpretation:
# The model is not globally significant.
# Some major-level effects appear, but they should not be overinterpreted, as stated earlier.

# ---------------------------------------------------------
# 4. Inverted model: does baseline GPA predict AI usage?
# ---------------------------------------------------------

model_usage_from_gpa <- lm(
  Task_Frequency_Daily ~ GPA_Baseline + Age + Major,
  data = tbl_database
)

summary(model_usage_from_gpa)
tidy(model_usage_from_gpa)

# Interpretation:
# There is no evidence that baseline GPA predicts AI usage frequency.

# ---------------------------------------------------------
# 5. Ethics concern model
# ---------------------------------------------------------

model_ethics_concern <- polr(
  AI_Ethics_Concern ~ Primary_AI_Tool +
    Task_Frequency_Daily +
    Main_Usage_Case +
    Time_Saved_Hours_Weekly +
    Age +
    Major,
  data = tbl_database,
  Hess = TRUE
)

model_ethics_concern

ethics_concern_results <- tidy(model_ethics_concern) |>
  mutate(
    p_value = 2 * pnorm(-abs(statistic)),
    odds_ratio = exp(estimate)
  )

ethics_concern_results

# Interpretation:
# More frequent AI usage is weakly associated with lower ethical concern.
# The effect is small: exp(-0.034) ≈ 0.97, that is, a 3% change explained, p ≈ 0.038.
# Overall, no robust relationship is found between AI usage patterns and ethical concerns.

# ---------------------------------------------------------
# 6. Time saved analysis
# ---------------------------------------------------------

qqnorm(tbl_database$Time_Saved_Hours_Weekly)
qqline(tbl_database$Time_Saved_Hours_Weekly, col = "red")

plot_time_saved_hist <- tbl_database |>
  ggplot(aes(x = Time_Saved_Hours_Weekly)) +
  geom_histogram(fill = "#4C78A8", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Time Saved Weekly",
    x = "Hours",
    y = "Number of occurences"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_time_saved_hist

model_time_saved <- lm(
  Time_Saved_Hours_Weekly ~ Task_Frequency_Daily +
    Main_Usage_Case +
    Primary_AI_Tool +
    GPA_Baseline +
    Age +
    Major,
  data = tbl_database
)

summary(model_time_saved)
tidy(model_time_saved)

# Interpretation:
# The model does not identify meaningful predictors of weekly time saved.

# ---------------------------------------------------------
# 7. High AI usage model
# ---------------------------------------------------------

tbl_database <- tbl_database |>
  mutate(high_usage = Task_Frequency_Daily > median(Task_Frequency_Daily))

model_high_usage <- glm(
  high_usage ~ GPA_Baseline + Age + Major,
  data = tbl_database,
  family = binomial
)

summary(model_high_usage)
tidy(model_high_usage)

# Interpretation:
# The model does not meaningfully predict high AI usage.

# ---------------------------------------------------------
# 8. Clustering
# ---------------------------------------------------------

clustering_data <- tbl_database |>
  dplyr::select(
    Task_Frequency_Daily,
    Time_Saved_Hours_Weekly,
    Career_Confidence_Score,
    GPA_Baseline,
    gpa_diff
  ) |>
  scale()

set.seed(1999) # To enhance reproductability of the analysis

kmeans_model <- kmeans(
  clustering_data,
  centers = 3,
  nstart = 25
)

tbl_database$cluster <- factor(kmeans_model$cluster)

cluster_counts <- table(tbl_database$cluster)
cluster_proportions <- prop.table(table(tbl_database$cluster))

cluster_counts
cluster_proportions

cluster_profiles <- tbl_database |>
  group_by(cluster) |>
  summarise(
    n = n(),
    mean_usage = mean(Task_Frequency_Daily),
    mean_time_saved = mean(Time_Saved_Hours_Weekly),
    mean_confidence = mean(Career_Confidence_Score),
    mean_gpa_baseline = mean(GPA_Baseline),
    mean_gpa_diff = mean(gpa_diff)
  )

cluster_profiles

# Interpretation:
# Clusters are balanced but weakly differentiated.
# The clustering does not reveal strong behavioral profiles.
