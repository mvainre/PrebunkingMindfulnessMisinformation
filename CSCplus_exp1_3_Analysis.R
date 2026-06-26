################################################################################-
#
# CSC+ Experiment 1
# Data analysis
# Maris Vainre and Alicia Smith 2025
#
################################################################################-




################################################################################-
# Set-up ----
################################################################################-


library(here)
library(dplyr)
library(rstatix)
library(psych)

source(here::here("./Code/Write-up/CSCplus_0_Formatting.R"))
Exp1_cleaned <- readRDS(here::here("./Data/Experiment1/1_cleaned/CSCplus_exp1_cleaned.rds"))




################################################################################-
# Analysis ----
################################################################################-

#We pre-registered to do ANOVA

model <- aov(Reliable ~ Condition, data = Exp1_cleaned)



################################################################################-
## Assumptions for ANOVA ----


### Normality of residuals - VIOLATED ----

# Q-Q plot
#qqnorm(model$residuals)
#qqline(model$residuals)

#hist(model$residuals)

exp1_reliability_shapiro <- shapiro.test(model$residuals)

#Shapiro-Wilk normality test
#
#data:  onewayresid
#W = 0.97554, p-value = 0.00000005023

### Homogeneity of variance - OK ----
#rstatix::levene_test(Reliable ~ Condition, data = Exp1_cleaned)

# A tibble: 1 × 4
#df1   df2 statistic     p
#<int> <int>     <dbl> <dbl>
#  1     3   553     0.448 0.719

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
exp1_reliability_dist <- psych::describe(Exp1_cleaned$Reliable)
# Key columns: skew, kurtosis (excess kurtosis; normal distribution = 0)

#exp1_reliability_dist
#vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
#X1    1 557 3.49 1.37    3.8    3.49 1.78   1   7     6 -0.02    -0.83 0.06

# Per-condition distributional properties
exp1_reliability_dist_by_condition <- psych::describeBy(
  x     = Exp1_cleaned$Reliable,
  group = Exp1_cleaned$Condition
)
# Decision rule (Curran et al., 1996):
# If |skew| <= 2 and |kurtosis| <= 7 across all conditions, ANOVA is likely
# robust despite the Shapiro-Wilk violation, particularly given n > 100 per
# cell (central limit theorem applies).
# For a single Likert item, ceiling or floor effects are the most plausible
# driver of non-normality; inspect histograms for this pattern specifically.
# If thresholds are exceeded, proceed with the non-parametric alternative.


#Descriptive statistics by group 
#group: Control
#vars   n  mean   sd median trimmed   mad min max range skew kurtosis   se
#X1*    1 139 17.71 9.33     19    17.6 10.38   1  37    36 0.04    -0.85 0.79
#----------------------------------------------------------------------------------------------------------------- 
#  group: Inflation
#vars   n  mean   sd median trimmed  mad min max range skew kurtosis   se
#X1*    1 140 17.22 9.67   17.5   16.93 9.64   1  39    38 0.27    -0.63 0.82
#----------------------------------------------------------------------------------------------------------------- 
#  group: Reaction
#vars   n  mean   sd median trimmed   mad min max range  skew kurtosis   se
#X1*    1 138 17.52 10.1     20   17.57 14.83   1  36    35 -0.01    -1.18 0.86
#----------------------------------------------------------------------------------------------------------------- 
#  group: InflationReaction
#vars   n  mean    sd median trimmed   mad min max range  skew kurtosis  se
#X1*    1 140 19.24 10.64     20   19.47 13.34   1  39    38 -0.15    -1.15 0.9



# Visual inspection per condition
# Histograms and Q-Q plots stratified by condition help detect whether
# non-normality is driven by a specific group or is uniform across conditions
par(mfrow = c(2, 4))
# Panel titles use display labels from CSCplus_0_Formatting.R; keys are the
# internal Condition codes, so subsetting still uses the raw `cond` value.
cond_lbl_map <- setNames(cond_lbls, c("Control", "Inflation", "Reaction", "InflationReaction"))
for (cond in unique(Exp1_cleaned$Condition)) {
  subset_data <- Exp1_cleaned$Reliable[Exp1_cleaned$Condition == cond]
  cond_lbl <- cond_lbl_map[as.character(cond)]
  hist(subset_data,
       main = paste("Histogram:", cond_lbl),
       xlab = "Reliability",
       breaks = 15)
  qqnorm(subset_data, main = paste("Q-Q:", cond_lbl))
  qqline(subset_data, col = unimelb_blue)
}
par(mfrow = c(1, 1))





################################################################################-
## Pre-registered omnibus ----
model <- aov(Reliable ~ Condition, data = Exp1_cleaned)
summary(model)

#Df Sum Sq Mean Sq F value Pr(>F)
#Condition     3   10.7   3.563   1.915  0.126
#Residuals   553 1028.7   1.860 

rstatix::eta_squared(model) 
#Condition 
#0.01028284 

# Pre-registered all-pairwise, familywise-corrected
rstatix::tukey_hsd(Exp1_cleaned, Reliable ~ Condition)

# A tibble: 6 × 9
#term      group1    group2            null.value estimate conf.low conf.high p.adj p.adj.signif
#* <chr>     <chr>     <chr>                  <dbl>    <dbl>    <dbl>     <dbl> <dbl> <chr>       
#  1 Condition Control   Inflation                  0  -0.281    -0.702    0.139  0.312 ns          
#2 Condition Control   Reaction                   0  -0.225    -0.648    0.197  0.516 ns          
#3 Condition Control   InflationReaction          0  -0.376    -0.797    0.0443 0.098 ns          
#4 Condition Inflation Reaction                   0   0.0562   -0.365    0.478  0.986 ns          
#5 Condition Inflation InflationReaction          0  -0.0950   -0.515    0.325  0.937 ns          
#6 Condition Reaction  InflationReaction          0  -0.151    -0.573    0.270  0.792 ns     

# Robustness check
exp1_reliability_dunn_test <- rstatix::dunn_test(Exp1_cleaned, Reliable ~ Condition,
                   p.adjust.method = "holm", detailed = TRUE)

#.y.      group1    group2               n1    n2 estimate estimate1 estimate2 statistic      p method    p.adj p.adj.signif
#* <chr>    <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>  <dbl> <chr>     <dbl> <chr>       
#  1 Reliable Control   Inflation           139   140   -30.7       303.      272.    -1.60  0.109  Dunn Test 0.547 ns          
#2 Reliable Control   Reaction            139   138   -23.6       303.      279.    -1.22  0.221  Dunn Test 0.884 ns          
#3 Reliable Control   InflationReaction   139   140   -41.0       303.      262.    -2.14  0.0325 Dunn Test 0.195 ns          
#4 Reliable Inflation Reaction            140   138     7.16      272.      279.     0.373 0.709  Dunn Test 1     ns          
#5 Reliable Inflation InflationReaction   140   140   -10.3       272.      262.    -0.538 0.590  Dunn Test 1     ns          
#6 Reliable Reaction  InflationReaction   138   140   -17.5       279.      262.    -0.909 0.363  Dunn Test 1     ns 

################################################################################-
## Robustness check. Non-parametric test ----

exp1_reliability_KWtest <- kruskal.test(Reliable ~ Condition, data = Exp1_cleaned)

#Kruskal-Wallis rank sum test
#
#data:  Reliable by Condition
#Kruskal-Wallis chi-squared = 4.9478, df = 3, p-value = 0.1757


### Pairwise comparisons ----


exp1_reliability_dunn_test_nonpara <- rstatix::dunn_test(data = Exp1_cleaned,
                                                    formula = Reliable ~ Condition, 
                                                    p.adjust.method = "none",
                                                    detailed = TRUE)

# A tibble: 6 × 13
#  .y.      group1    group2               n1    n2 estimate estimate1 estimate2 statistic      p method     p.adj p.adj.signif
#* <chr>    <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>  <dbl> <chr>      <dbl> <chr>       
#1 Reliable Control   Inflation           139   140   -30.7       303.      272.    -1.60  0.109  Dunn Test 0.109  ns          
#2 Reliable Control   Reaction            139   138   -23.6       303.      279.    -1.22  0.221  Dunn Test 0.221  ns          
#3 Reliable Control   InflationReaction   139   140   -41.0       303.      262.    -2.14  0.0325 Dunn Test 0.0325 *           
#4 Reliable Inflation Reaction            140   138     7.16      272.      279.     0.373 0.709  Dunn Test 0.709  ns          
#5 Reliable Inflation InflationReaction   140   140   -10.3       272.      262.    -0.538 0.590  Dunn Test 0.590  ns          
#6 Reliable Reaction  InflationReaction   138   140   -17.5       279.      262.    -0.909 0.363  Dunn Test 0.363  ns


#Calculate Cohen's d
# Calculate standard deviations from the data

mean_control <- mean(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "Control"])
mean_inflation_reaction <- mean(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "InflationReaction"])

sd_control <- sd(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "Control"])
sd_inflation_reaction <- sd(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "InflationReaction"])

# Calculate Cohen's d
exp1_reliability_cohenD <- (mean_control - mean_inflation_reaction) / sqrt((sd_control^2 + sd_inflation_reaction^2) / 2)

