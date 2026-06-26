################################################################################-
#
# CSC+ Experiment 1
# Data analysis. Secondary outcomes
# Maris Vainre and Alicia Smith 2025
#
################################################################################-




################################################################################-
# Set-up ----
################################################################################-


library(here)
library(dplyr)
library(rstatix)
library(purrr)
library(car)

Exp1_cleaned <- readRDS(here::here("./Data/Experiment1/1_cleaned/CSCplus_exp1_cleaned.rds"))




################################################################################-
# Likelihood ----
################################################################################-

#Outcome of interest for experiment 2

model <- aov(Likelihood ~ Condition, data = Exp1_cleaned)



################################################################################-
## Assumptions for ANOVA ----


### Normality of residuals - VIOLATED ----

# Q-Q plot
#qqnorm(model$residuals)
#qqline(model$residuals)

#hist(model$residuals)

exp1_likelihood_shapiro <- shapiro.test(model$residuals)

#Shapiro-Wilk normality test
#
#data:  model$residuals
#W = 0.92436, p-value = 0.0000000000000004046

### Homogeneity of variance - VIOLATED ----
rstatix::levene_test(Likelihood ~ Condition, data = Exp1_cleaned)

## A tibble: 1 × 4
#df1   df2 statistic       p
#<int> <int>     <dbl>   <dbl>
#  1     3   553      4.19 0.00599


### Peer-review revision: Distributional properties of the primary outcome ----
# Inspecting skewness and kurtosis to assess whether the normality violation
# is practically consequential, following the Associate Editor's recommendation.
# Thresholds used: |skewness| > 2 and |kurtosis| > 7 are commonly cited as
# indicating non-normality severe enough to concern ANOVA robustness.
#
# Note: Reliable is a single Likert item and therefore inherently discrete
# and bounded. Some departure from normality is expected by design; the
# question is whether it is severe enough to warrant non-parametric testing.

# Overall distributional properties
exp1_likelihood_dist <- psych::describe(Exp1_cleaned$Likelihood)
# Key columns: skew, kurtosis (excess kurtosis; normal distribution = 0)

#exp1_likelihood_dis
#vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1    1 557 28.97 27.12     21      26 29.65   0 100   100 0.73    -0.55 1.15


# Per-condition distributional properties
exp1_likelihood_dist_by_condition <- psych::describeBy(
  x     = Exp1_cleaned$Likelihood,
  group = Exp1_cleaned$Condition
)


# Decision rule (Curran et al., 1996):
# If |skew| <= 2 and |kurtosis| <= 7 across all conditions, ANOVA is likely
# robust despite the Shapiro-Wilk violation, particularly given n > 100 per
# cell (central limit theorem applies).
# For a single Likert item, ceiling or floor effects are the most plausible
# driver of non-normality; inspect histograms for this pattern specifically.
# If thresholds are exceeded, proceed with the non-parametric alternative.

#group: Control
#vars   n  mean    sd median trimmed  mad min max range skew kurtosis   se
#X1*    1 139 24.25 19.12     21   23.04 25.2   1  63    62 0.37    -1.14 1.62
#----------------------------------------------------------------------------------------------------------------- 
#  group: Inflation
#vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1*    1 140 19.99 16.17     19   18.76 23.72   1  54    53 0.35    -1.14 1.37
#----------------------------------------------------------------------------------------------------------------- 
#  group: Reaction
#vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1*    1 138 21.35 17.85     19   19.98 23.72   1  56    55 0.43    -1.17 1.52
#----------------------------------------------------------------------------------------------------------------- 
#  group: InflationReaction
#vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1*    1 140 20.21 17.91     17   18.72 23.72   1  56    55 0.41    -1.26 1.51

# Visual inspection per condition
# Histograms and Q-Q plots stratified by condition help detect whether
# non-normality is driven by a specific group or is uniform across conditions
par(mfrow = c(2, 4))
# Panel titles use display labels from CSCplus_0_Formatting.R; keys are the
# internal Condition codes, so subsetting still uses the raw `cond` value.
cond_lbl_map <- setNames(cond_lbls, c("Control", "Inflation", "Reaction", "InflationReaction"))
for (cond in unique(Exp1_cleaned$Condition)) {
  subset_data <- Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == cond]
  cond_lbl <- cond_lbl_map[as.character(cond)]
  hist(subset_data,
       main = paste("Histogram:", cond_lbl),
       xlab = "Likelihood",
       breaks = 15)
  qqnorm(subset_data, main = paste("Q-Q:", cond_lbl))
  qqline(subset_data, col = unimelb_blue)
}
par(mfrow = c(1, 1))



################################################################################-
## Non-parametric test ----

exp1_likelihood_KWtest <- kruskal.test(Likelihood ~ Condition, data = Exp1_cleaned)

#Kruskal-Wallis rank sum test
#
#data:  Likelihood by Condition
#Kruskal-Wallis chi-squared = 19.53, df = 3, p-value = 0.0002124


### Pairwise comparisons ----


exp1_likelihood_dunn_test <- rstatix::dunn_test(data = Exp1_cleaned,
                                                formula = Likelihood ~ Condition, 
                                                p.adjust.method = "none",
                                                detailed = TRUE)

## A tibble: 6 × 13
#.y.        group1    group2               n1    n2 estimate estimate1 estimate2 statistic         p method       p.adj p.adj.signif
#* <chr>      <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>     <dbl> <chr>        <dbl> <chr>       
#1 Likelihood Control   Inflation           139   140    -67.0      329.      262.    -3.49  0.000484  Dunn Test  4.84e-4 ***         
#2 Likelihood Control   Reaction            139   138    -52.6      329.      276.    -2.73  0.00634   Dunn Test  6.34e-3 **          
#3 Likelihood Control   InflationReaction   139   140    -78.5      329.      250.    -4.09  0.0000438 Dunn Test  4.38e-5 ****        
#4 Likelihood Inflation Reaction            140   138     14.4      262.      276.     0.749 0.454     Dunn Test  4.54e-1 ns          
#5 Likelihood Inflation InflationReaction   140   140    -11.5      262.      250.    -0.598 0.550     Dunn Test  5.50e-1 ns          
#6 Likelihood Reaction  InflationReaction   138   140    -25.9      276.      250.    -1.34  0.179     Dunn Test  1.79e-1 ns 


#Calculate Cohen's d
# Calculate standard deviations from the data

mean_control <- mean(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Control"])
mean_inflation_reaction <- mean(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "InflationReaction"])

sd_control <- sd(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Control"])
sd_inflation_reaction <- sd(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "InflationReaction"])

# Calculate Cohen's d
exp1_likelihood_cohenD <- (mean_control - mean_inflation_reaction) / sqrt((sd_control^2 + sd_inflation_reaction^2) / 2)





################################################################################-
# Technique and video ratings ----
################################################################################-

Exp1_vid_techniques <- Exp1_cleaned |>
  dplyr::select(c(PROLIFIC_PID, Condition, dplyr::starts_with("Tech"), dplyr::starts_with("Video_"))) |>
  
  # mutate Video_ variables to numeric
  dplyr::mutate(across(dplyr::starts_with("Video_"), as.numeric)) |>
  dplyr::mutate(across(dplyr::starts_with("Tech"), as.numeric))  




################################################################################-
## Technique ratings ----

# Descriptive statistics
Exp1_tech <- Exp1_vid_techniques |>
  dplyr::select(c(Condition, dplyr::starts_with("Tech")))


Exp1_Advert_techniques_summary_byCondition <- Exp1_tech |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(across(where(is.numeric), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)))) |>
  dplyr::ungroup()



### Emotional appeal ----
model <- aov(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_emoappeal_shapiro <- shapiro.test(model$residuals)

rstatix::levene_test(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_emoappeal_KWtest <- kruskal.test(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)
exp1_tech_emoappeal_KWtest 


### Inflation of effects ----
model <- aov(Tech_inflation ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_inflation_shapiro <- shapiro.test(model$residuals)

rstatix::levene_test(Tech_inflation ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_inflation_KWtest <- kruskal.test(Tech_inflation ~ Condition, data = Exp1_vid_techniques)
exp1_tech_inflation_KWtest


### Repetition ----
model <- aov(Tech_repetition ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_repetition_shapiro <- shapiro.test(model$residuals)

rstatix::levene_test(Tech_repetition ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_repetition_KWtest <- kruskal.test(Tech_repetition ~ Condition, data = Exp1_vid_techniques)
exp1_tech_repetition_KWtest 


### Influencers ----
model <- aov(Tech_influencers ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_influencers_shapiro <- shapiro.test(model$residuals)

rstatix::levene_test(Tech_influencers ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_influencers_KWtest <- kruskal.test(Tech_influencers ~ Condition, data = Exp1_vid_techniques)
exp1_tech_influencers_KWtest 




### Scarcity ----
model <- aov(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_scarcity_shapiro <- shapiro.test(model$residuals)

rstatix::levene_test(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_scarcity_KWtest <- kruskal.test(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)
exp1_tech_scarcity_KWtest 




################################################################################-
## Video usefulness ----

Exp1_vid <- Exp1_vid_techniques |>
  dplyr::select(c(Condition, dplyr::starts_with("Video_"))) 

# summarise data by Condition
Exp1_vid_summary_byCondition <- Exp1_vid |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(across(where(is.numeric), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)))) |>
  dplyr::ungroup()


### Analyses ----

#### Assumptions ----

# Reshape the data to long format
Exp1_vid_long <- Exp1_vid %>%
  pivot_longer(cols = starts_with("Video_"), 
               names_to = "variable", 
               values_to = "value")

# Check for normality using Shapiro-Wilk test
shapiro_test <- Exp1_vid_long %>%
  group_by(variable) %>%
  summarise(p_value = shapiro.test(value)$p.value)

print(shapiro_test)

# Check for homogeneity of variances using Levene's test
nested_data <- Exp1_vid_long %>%
  nest(data = -variable)

levene_test <- nested_data %>%
  mutate(test_result = purrr::map(data, ~{
    print(.x)  # Print each nested dataframe
    car::leveneTest(value ~ Condition, data = .x)
  }))

print(levene_test$test_result)

#all violated

#### Non-parametric tests ----
exp1_vid_informative_KWtest <- kruskal.test(Video_informative ~ Condition, data = Exp1_vid)
exp1_vid_informative_KWtest 

exp1_vid_interesting_KWtest <- kruskal.test(Video_interesting ~ Condition, data = Exp1_vid)
exp1_vid_interesting_KWtest 

exp1_vid_useful_KWtest <- kruskal.test(Video_useful~ Condition, data = Exp1_vid)
exp1_vid_useful_KWtest 
