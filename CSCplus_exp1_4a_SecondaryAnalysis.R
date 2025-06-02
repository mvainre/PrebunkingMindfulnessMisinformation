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
library(rcompanion)

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
#rstatix::levene_test(Likelihood ~ Condition, data = Exp1_cleaned)

## A tibble: 1 × 4
#df1   df2 statistic       p
#<int> <int>     <dbl>   <dbl>
#  1     3   553      4.19 0.00599



################################################################################-
## Non-parametric test ----

exp1_likelihood_KWtest <- kruskal.test(Likelihood ~ Condition, data = Exp1_cleaned)

#Kruskal-Wallis rank sum test
#
#data:  Likelihood by Condition
#Kruskal-Wallis chi-squared = 19.53, df = 3, p-value = 0.0002124

# eta-squared - effect size
rstatix::kruskal_effsize(Likelihood ~ Condition, data = Exp1_cleaned)


# epsilon-squared - effect size
H <- exp1_likelihood_KWtest$statistic
k <- length(unique(Exp1_cleaned$Condition))
n <- length(Exp1_cleaned$Likelihood)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared

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

# Vargha and Delaney's A - effect size
multiVDA(Reliable ~ Condition, data = Exp1_cleaned)

#Calculate Cohen's d - control x inflationReaction
# Calculate standard deviations from the data

mean_control <- mean(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Control"])
mean_inflation_reaction <- mean(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "InflationReaction"])

sd_control <- sd(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Control"])
sd_inflation_reaction <- sd(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "InflationReaction"])

# Calculate Cohen's d
exp1_likelihood_cohenD <- (mean_control - mean_inflation_reaction) / sqrt((sd_control^2 + sd_inflation_reaction^2) / 2)



#Calculate Cohen's d - control x inflation
# Calculate standard deviations from the data

mean_inflation <- mean(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Inflation"])
sd_inflation <- sd(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Inflation"])

# Calculate Cohen's d
exp1_likelihood_cohenD <- (mean_control - mean_inflation) / sqrt((sd_control^2 + sd_inflation^2) / 2)


#Calculate Cohen's d - control x reaction
# Calculate standard deviations from the data

mean_reaction <- mean(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Reaction"])
sd_reaction <- sd(Exp1_cleaned$Likelihood[Exp1_cleaned$Condition == "Reaction"])

# Calculate Cohen's d
exp1_likelihood_cohenD <- (mean_control - mean_reaction) / sqrt((sd_control^2 + sd_reaction^2) / 2)



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
#W = 0.88034, p-value < 0.00000000000000022
rstatix::levene_test(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_emoappeal_KWtest <- kruskal.test(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)
exp1_tech_emoappeal_KWtest 

# eta-squared - effect size
rstatix::kruskal_effsize(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)

# epsilon-squared - effect size
H <- exp1_tech_emoappeal_KWtest$statistic
k <- length(unique(Exp1_vid_techniques$Condition))
n <- length(Exp1_vid_techniques$Tech_emoappeal)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared


exp1_tech_emoappeal_dunn_test <- rstatix::dunn_test(data = Exp1_vid_techniques,
                                                 formula = Tech_emoappeal ~ Condition, 
                                                 p.adjust.method = "none",
                                                 detailed = TRUE)

# # A tibble: 6 × 13
# .y.            group1    group2               n1    n2 estimate estimate1 estimate2 statistic          p method   p.adj p.adj.signif
# * <chr>          <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>      <dbl> <chr>    <dbl> <chr>       
#   1 Tech_emoappeal Control   Inflation           139   140    47.8       234.      282.     2.78  0.00535    Dunn … 5.35e-3 **          
#   2 Tech_emoappeal Control   Reaction            139   138    78.1       234.      312.     4.54  0.00000572 Dunn … 5.72e-6 ****        
#   3 Tech_emoappeal Control   InflationReaction   139   140    52.9       234.      287.     3.09  0.00202    Dunn … 2.02e-3 **          
#   4 Tech_emoappeal Inflation Reaction            140   138    30.3       282.      312.     1.76  0.0776     Dunn … 7.76e-2 ns          
#   5 Tech_emoappeal Inflation InflationReaction   140   140     5.19      282.      287.     0.303 0.762      Dunn … 7.62e-1 ns          
#   6 Tech_emoappeal Reaction  InflationReaction   138   140   -25.1       312.      287.    -1.46  0.144      Dunn … 1.44e-1 ns     

# Vargha and Delaney's A - effect size
multiVDA(Tech_emoappeal ~ Condition, data = Exp1_vid_techniques)

### Inflation of effects ----
model <- aov(Tech_inflation ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_inflation_shapiro <- shapiro.test(model$residuals)
# W = 0.82853, p-value < 0.00000000000000022
rstatix::levene_test(Tech_inflation ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_inflation_KWtest <- kruskal.test(Tech_inflation ~ Condition, data = Exp1_vid_techniques)
exp1_tech_inflation_KWtest

# eta-squared - effect size
rstatix::kruskal_effsize(Tech_inflation ~ Condition, data = Exp1_vid_techniques)

# epsilon-squared - effect size
H <- exp1_tech_inflation_KWtest$statistic
k <- length(unique(Exp1_vid_techniques$Condition))
n <- length(Exp1_vid_techniques$Tech_inflation)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared

exp1_tech_inflation_dunn_test <- rstatix::dunn_test(data = Exp1_vid_techniques,
                                                    formula = Tech_inflation ~ Condition, 
                                                    p.adjust.method = "none",
                                                    detailed = TRUE)

# # A tibble: 6 × 13
# .y.            group1    group2               n1    n2 estimate estimate1 estimate2 statistic        p method     p.adj p.adj.signif
# * <chr>          <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>    <dbl> <chr>      <dbl> <chr>       
#   1 Tech_inflation Control   Inflation           139   140    43.2       243.      286.     2.54  0.0109   Dunn Te… 1.09e-2 *           
#   2 Tech_inflation Control   Reaction            139   138    62.2       243.      305.     3.65  0.000261 Dunn Te… 2.61e-4 ***         
#   3 Tech_inflation Control   InflationReaction   139   140    37.8       243.      281.     2.23  0.0259   Dunn Te… 2.59e-2 *           
#   4 Tech_inflation Inflation Reaction            140   138    19.0       286.      305.     1.12  0.264    Dunn Te… 2.64e-1 ns          
#   5 Tech_inflation Inflation InflationReaction   140   140    -5.38      286.      281.    -0.317 0.751    Dunn Te… 7.51e-1 ns          
#   6 Tech_inflation Reaction  InflationReaction   138   140   -24.4       305.      281.    -1.43  0.152    Dunn Te… 1.52e-1 ns    

# Vargha and Delaney's A - effect size
multiVDA(Tech_inflation ~ Condition, data = Exp1_vid_techniques)

### Repetition ----
model <- aov(Tech_repetition ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_repetition_shapiro <- shapiro.test(model$residuals)
#W = 0.76581, p-value < 0.00000000000000022
rstatix::levene_test(Tech_repetition ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_repetition_KWtest <- kruskal.test(Tech_repetition ~ Condition, data = Exp1_vid_techniques)
exp1_tech_repetition_KWtest 

# eta-squared - effect size
rstatix::kruskal_effsize(Tech_repetition ~ Condition, data = Exp1_vid_techniques)

# epsilon-squared - effect size
H <- exp1_tech_repetition_KWtest$statistic
k <- length(unique(Exp1_vid_techniques$Condition))
n <- length(Exp1_vid_techniques$Tech_repetition)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared

exp1_tech_repetition_dunn_test <- rstatix::dunn_test(data = Exp1_vid_techniques,
                                                    formula = Tech_repetition ~ Condition, 
                                                    p.adjust.method = "none",
                                                    detailed = TRUE)

# # A tibble: 6 × 13
# .y.             group1    group2               n1    n2 estimate estimate1 estimate2 statistic      p method     p.adj p.adj.signif
# * <chr>           <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>  <dbl> <chr>      <dbl> <chr>       
#   1 Tech_repetition Control   Inflation           139   140    11.8       262.      274.     0.710 0.478  Dunn Test 0.478  ns          
#   2 Tech_repetition Control   Reaction            139   138    21.5       262.      284.     1.29  0.198  Dunn Test 0.198  ns          
#   3 Tech_repetition Control   InflationReaction   139   140    34.3       262.      296.     2.06  0.0399 Dunn Test 0.0399 *           
#   4 Tech_repetition Inflation Reaction            140   138     9.71      274.      284.     0.581 0.561  Dunn Test 0.561  ns          
#   5 Tech_repetition Inflation InflationReaction   140   140    22.4       274.      296.     1.35  0.178  Dunn Test 0.178  ns          
#   6 Tech_repetition Reaction  InflationReaction   138   140    12.7       284.      296.     0.762 0.446  Dunn Test 0.446  ns       


### Influencers ----
model <- aov(Tech_influencers ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_influencers_shapiro <- shapiro.test(model$residuals)
#W = 0.18885, p-value < 0.00000000000000022
rstatix::levene_test(Tech_influencers ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_influencers_KWtest <- kruskal.test(Tech_influencers ~ Condition, data = Exp1_vid_techniques)
exp1_tech_influencers_KWtest 

rstatix::kruskal_effsize(Tech_influencers ~ Condition, data = Exp1_vid_techniques)

# epsilon-squared - effect size
H <- exp1_tech_influencers_KWtest$statistic
k <- length(unique(Exp1_vid_techniques$Condition))
n <- length(Exp1_vid_techniques$Tech_influencers)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared

exp1_tech_influencers_dunn_test <- rstatix::dunn_test(data = Exp1_vid_techniques,
                                                     formula = Tech_influencers ~ Condition, 
                                                     p.adjust.method = "none",
                                                     detailed = TRUE)

# # A tibble: 6 × 13
# .y.              group1    group2               n1    n2 estimate estimate1 estimate2 statistic     p method    p.adj p.adj.signif
# * <chr>            <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl> <dbl> <chr>     <dbl> <chr>       
#   1 Tech_influencers Control   Inflation           139   140  2.06         276.      279.   0.359   0.720 Dunn Test 0.720 ns          
#   2 Tech_influencers Control   Reaction            139   138  2.05         276.      279.   0.357   0.721 Dunn Test 0.721 ns          
#   3 Tech_influencers Control   InflationReaction   139   140  5.90         276.      282.   1.03    0.304 Dunn Test 0.304 ns          
#   4 Tech_influencers Inflation Reaction            140   138 -0.00652      279.      279.  -0.00113 0.999 Dunn Test 0.999 ns          
#   5 Tech_influencers Inflation InflationReaction   140   140  3.84         279.      282.   0.671   0.503 Dunn Test 0.503 ns          
#   6 Tech_influencers Reaction  InflationReaction   138   140  3.85         279.      282.   0.669   0.503 Dunn Test 0.503 ns   


### Scarcity ----
model <- aov(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)

exp1_Tech_scarcity_shapiro <- shapiro.test(model$residuals)
#W = 0.54126, p-value < 0.00000000000000022
rstatix::levene_test(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)

#### Non-parametric test ----
exp1_tech_scarcity_KWtest <- kruskal.test(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)
exp1_tech_scarcity_KWtest 

rstatix::kruskal_effsize(Tech_scarcity ~ Condition, data = Exp1_vid_techniques)

# epsilon-squared - effect size
H <- exp1_tech_scarcity_KWtest$statistic
k <- length(unique(Exp1_vid_techniques$Condition))
n <- length(Exp1_vid_techniques$Tech_scarcity)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared

exp1_tech_scarcity_dunn_test <- rstatix::dunn_test(data = Exp1_vid_techniques,
                                                      formula = Tech_scarcity ~ Condition, 
                                                      p.adjust.method = "none",
                                                      detailed = TRUE)

# # A tibble: 6 × 13
# .y.           group1    group2               n1    n2 estimate estimate1 estimate2 statistic      p method     p.adj p.adj.signif
# * <chr>         <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>  <dbl> <chr>      <dbl> <chr>       
#   1 Tech_scarcity Control   Inflation           139   140    -4.29      275.      270.    -0.342 0.733  Dunn Test 0.733  ns          
#   2 Tech_scarcity Control   Reaction            139   138     2.89      275.      278.     0.229 0.819  Dunn Test 0.819  ns          
#   3 Tech_scarcity Control   InflationReaction   139   140    18.2       275.      293.     1.45  0.148  Dunn Test 0.148  ns          
#   4 Tech_scarcity Inflation Reaction            140   138     7.19      270.      278.     0.571 0.568  Dunn Test 0.568  ns          
#   5 Tech_scarcity Inflation InflationReaction   140   140    22.5       270.      293.     1.79  0.0735 Dunn Test 0.0735 ns          
#   6 Tech_scarcity Reaction  InflationReaction   138   140    15.3       278.      293.     1.21  0.225  Dunn Test 0.225  ns 


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

# eta-squared - effect size
rstatix::kruskal_effsize(Video_informative ~ Condition, data = Exp1_vid)

# epsilon-squared - effect size
H <- exp1_vid_informative_KWtest$statistic
k <- length(unique(Exp1_vid$Condition))
n <- length(Exp1_vid$Video_informative)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared


rstatix::dunn_test(data = Exp1_vid,
                   formula = Video_informative ~ Condition, 
                   p.adjust.method = "none",
                   detailed = TRUE)

# .y.               group1    group2               n1    n2 estimate estimate1 estimate2 statistic          p method         p.adj p.adj.signif
# * <chr>             <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>      <dbl> <chr>          <dbl> <chr>       
#   1 Video_informative Control   Inflation           139   140     66.4      226.      292.     3.45  0.000564   Dunn Test 0.000564   ***         
#   2 Video_informative Control   Reaction            139   138     53.9      226.      279.     2.79  0.00530    Dunn Test 0.00530    **          
#   3 Video_informative Control   InflationReaction   139   140     93.2      226.      319.     4.84  0.00000132 Dunn Test 0.00000132 ****        
#   4 Video_informative Inflation Reaction            140   138    -12.5      292.      279.    -0.649 0.516      Dunn Test 0.516      ns          
#   5 Video_informative Inflation InflationReaction   140   140     26.8      292.      319.     1.39  0.164      Dunn Test 0.164      ns          
#   6 Video_informative Reaction  InflationReaction   138   140     39.3      279.      319.     2.04  0.0418     Dunn Test 0.0418     *        

# Vargha and Delaney's A - effect size
multiVDA(Video_informative ~ Condition, data = Exp1_vid)


exp1_vid_interesting_KWtest <- kruskal.test(Video_interesting ~ Condition, data = Exp1_vid)
exp1_vid_interesting_KWtest 

# eta-squared - effect size
rstatix::kruskal_effsize(Video_interesting~ Condition, data = Exp1_vid)

# epsilon-squared - effect size
H <- exp1_vid_interesting_KWtest$statistic
k <- length(unique(Exp1_vid$Condition))
n <- length(Exp1_vid$Video_interesting)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared


rstatix::dunn_test(data = Exp1_vid,
                   formula = Video_interesting ~ Condition, 
                   p.adjust.method = "none",
                   detailed = TRUE)

# # A tibble: 6 × 13
# .y.               group1    group2               n1    n2 estimate estimate1 estimate2 statistic      p method     p.adj p.adj.signif
# * <chr>             <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>  <dbl> <chr>      <dbl> <chr>       
#   1 Video_interesting Control   Inflation           139   140     8.18      264.      272.     0.425 0.671  Dunn Test 0.671  ns          
# 2 Video_interesting Control   Reaction            139   138    18.4       264.      282.     0.951 0.341  Dunn Test 0.341  ns          
# 3 Video_interesting Control   InflationReaction   139   140    35.2       264.      299.     1.83  0.0679 Dunn Test 0.0679 ns          
# 4 Video_interesting Inflation Reaction            140   138    10.2       272.      282.     0.529 0.597  Dunn Test 0.597  ns          
# 5 Video_interesting Inflation InflationReaction   140   140    27.0       272.      299.     1.40  0.160  Dunn Test 0.160  ns          
# 6 Video_interesting Reaction  InflationReaction   138   140    16.8       282.      299.     0.869 0.385  Dunn Test 0.385  ns   

# Vargha and Delaney's A - effect size
multiVDA(Video_interesting ~ Condition, data = Exp1_vid)


exp1_vid_useful_KWtest <- kruskal.test(Video_useful~ Condition, data = Exp1_vid)
exp1_vid_useful_KWtest 

# eta-squared - effect size
rstatix::kruskal_effsize(Video_useful~ Condition, data = Exp1_vid)

# epsilon-squared - effect size
H <- exp1_vid_useful_KWtest$statistic
k <- length(unique(Exp1_vid$Condition))
n <- length(Exp1_vid$Video_useful)
epsilon_squared <- (H - k + 1) / (n - k)
epsilon_squared


rstatix::dunn_test(data = Exp1_vid,
                   formula = Video_useful ~ Condition, 
                   p.adjust.method = "none",
                   detailed = TRUE)

# # A tibble: 6 × 13
# .y.          group1    group2               n1    n2 estimate estimate1 estimate2 statistic        p method       p.adj p.adj.signif
# * <chr>        <chr>     <chr>             <int> <int>    <dbl> <dbl[1d]> <dbl[1d]>     <dbl>    <dbl> <chr>        <dbl> <chr>       
#   1 Video_useful Control   Inflation           139   140   160.        154.      314.     8.32  8.61e-17 Dunn Test 8.61e-17 ****        
#   2 Video_useful Control   Reaction            139   138   181.        154.      336.     9.39  5.95e-21 Dunn Test 5.95e-21 ****        
#   3 Video_useful Control   InflationReaction   139   140   158.        154.      312.     8.21  2.28e-16 Dunn Test 2.28e-16 ****        
#   4 Video_useful Inflation Reaction            140   138    21.2       314.      336.     1.10  2.71e- 1 Dunn Test 2.71e- 1 ns          
#   5 Video_useful Inflation InflationReaction   140   140    -2.24      314.      312.    -0.117 9.07e- 1 Dunn Test 9.07e- 1 ns          
#   6 Video_useful Reaction  InflationReaction   138   140   -23.5       336.      312.    -1.22  2.24e- 1 Dunn Test 2.24e- 1 ns   

# Vargha and Delaney's A - effect size
multiVDA(Video_useful ~ Condition, data = Exp1_vid)

