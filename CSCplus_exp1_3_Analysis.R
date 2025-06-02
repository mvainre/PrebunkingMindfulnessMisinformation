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
library(rcompanion)

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



################################################################################-
## Non-parametric test ----

exp1_reliability_KWtest <- kruskal.test(Reliable ~ Condition, data = Exp1_cleaned)

# eta-squared
rstatix::kruskal_effsize(Reliable ~ Condition, data = Exp1_cleaned)

# epsilon-squared - effect size
H <- exp1_reliability_KWtest$statistic
k <- length(unique(Exp1_cleaned$Condition))
n <- length(Exp1_cleaned$Reliable)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared


### Pairwise comparisons ----


exp1_reliability_dunn_test <- rstatix::dunn_test(data = Exp1_cleaned,
                                                    formula = Reliable ~ Condition, 
                                                    p.adjust.method = "none",
                                                    detailed = TRUE)

# # A tibble: 6 × 13
# .y.      group1    group2               n1    n2 estimate estimate1 estimate2 statistic      p method     p.adj p.adj.signif
# * <chr>    <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>  <dbl> <chr>      <dbl> <chr>       
#   1 Reliable Control   Inflation           139   140   -30.7       303.      272.    -1.60  0.109  Dunn Test 0.109  ns          
#   2 Reliable Control   Reaction            139   138   -23.6       303.      279.    -1.22  0.221  Dunn Test 0.221  ns          
#   3 Reliable Control   InflationReaction   139   140   -41.0       303.      262.    -2.14  0.0325 Dunn Test 0.0325 *           
#   4 Reliable Inflation Reaction            140   138     7.16      272.      279.     0.373 0.709  Dunn Test 0.709  ns          
#   5 Reliable Inflation InflationReaction   140   140   -10.3       272.      262.    -0.538 0.590  Dunn Test 0.590  ns          
#   6 Reliable Reaction  InflationReaction   138   140   -17.5       279.      262.    -0.909 0.363  Dunn Test 0.363  ns 

# Vargha and Delaney's A - effect size
multiVDA(Reliable ~ Condition, data = Exp1_cleaned)


#Calculate Cohen's d
# Calculate standard deviations from the data

mean_control <- mean(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "Control"])
mean_inflation_reaction <- mean(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "InflationReaction"])

sd_control <- sd(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "Control"])
sd_inflation_reaction <- sd(Exp1_cleaned$Reliable[Exp1_cleaned$Condition == "InflationReaction"])

# Calculate Cohen's d
exp1_reliability_cohenD <- (mean_control - mean_inflation_reaction) / sqrt((sd_control^2 + sd_inflation_reaction^2) / 2)

