################################################################################-
#
# CSC+ inoculation 2025
# Script by Maris Vainre, Lisa Doan and Alicia Smith
# Secondary analyses
#
################################################################################-





################################################################################-
# Setup ----
################################################################################-

library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(WRS2)
library(car)

set.seed(17092024) #for bootstrapping

# Load data ----
Exp2_vid_techniques <- readRDS(here::here("./Data/Experiment2/1_cleaned/CSCplus_exp2.rds")) |>
  dplyr::select(c(ProlificID, Condition, dplyr::starts_with("Advert"), dplyr::starts_with("Tech"), dplyr::starts_with("Video_"))) |>
  
  # mutate Video_ variables to numeric
  dplyr::mutate(across(dplyr::starts_with("Video_"), as.numeric)) |>
  dplyr::mutate(across(dplyr::starts_with("Tech"), as.numeric)) |>
  #capitalise values
  dplyr::mutate(Advert1_type = stringr::str_to_title(Advert1_type),
                Advert2_type = stringr::str_to_title(Advert2_type)) 



################################################################################-
# Techniques and video usefulness ----
################################################################################-



################################################################################-
## Techniques ----

long_df <- Exp2_vid_techniques |>
  dplyr::select(c(ProlificID, Condition, Advert1_type, Advert2_type, dplyr::starts_with("Tech"))) |>
  tidyr::pivot_longer(cols = starts_with("Tech"),
                      names_to = c("Tech", ".value"),
                      names_pattern = "Tech(\\d)_(.*)")

long_df1 <- long_df |>
  dplyr::select(-Advert2_type) |>
  dplyr::filter(Tech == "1") |>
  dplyr::rename(Advert_type = Advert1_type)

long_df2 <- long_df |>
  dplyr::select(-Advert1_type) |>
  dplyr::filter(Tech == "2") |>
  dplyr::rename(Advert_type = Advert2_type)

Exp2_Advert_techniques <- dplyr::bind_rows(long_df1, long_df2) |>
  dplyr::select(-Tech) 

# summarise data by Condition
Exp2_Advert_techniques_summary_byCondition <- Exp2_Advert_techniques |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(across(where(is.numeric), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)))) |>
  dplyr::ungroup()

Exp2_Advert_techniques_summary_byAdvert <- Exp2_Advert_techniques |>
  dplyr::group_by(Advert_type) |>
  dplyr::summarise(across(where(is.numeric), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)))) |>
  dplyr::ungroup()

################################################################################-
# Analysis ----
################################################################################-

## Emotional appeal
# Normality and homogeneity violated 
model <- aov(emoappeal ~ Advert_type*Condition, data = Exp2_Advert_techniques)
shapiro.test(model$residuals)
#W = 0.92009, p-value < 2.2e-16
rstatix::levene_test(emoappeal ~ Advert_type*Condition, data = Exp2_Advert_techniques)
# # A tibble: 1 × 4
# df1   df2 statistic           p
# <int> <int>     <dbl>       <dbl>
#   1     3  1194      11.4 0.000000235

model_nonpara_Exp2_advert_emoappeal <- WRS2::bwtrim(emoappeal ~ Advert_type*Condition, 
                                                    id = ProlificID, 
                                                    data = Exp2_Advert_techniques,
                                                    tr = 0.05,
                                                    nboot = 5000)
model_nonpara_Exp2_advert_emoappeal


## Inflation
# Normality and homogeneity violated 
model <- aov(inflation ~ Advert_type*Condition, data = Exp2_Advert_techniques)
shapiro.test(model$residuals)
#W = 0.9506, p-value < 2.2e-16
rstatix::levene_test(inflation ~ Advert_type*Condition, data = Exp2_Advert_techniques)
# # A tibble: 1 × 4
# df1   df2 statistic       p
# <int> <int>     <dbl>   <dbl>
#   1     3  1194      4.03 0.00728

model_nonpara_Exp2_advert_inflation <- WRS2::bwtrim(inflation ~ Advert_type*Condition, 
                                                    id = ProlificID, 
                                                    data = Exp2_Advert_techniques,
                                                    tr = 0.05,
                                                    nboot = 5000)
model_nonpara_Exp2_advert_inflation


## Influencers
# Normality and homogeneity violated 
model <- aov(influencers ~ Advert_type*Condition, data = Exp2_Advert_techniques)
shapiro.test(model$residuals)
#W = 0.26026, p-value < 2.2e-16
rstatix::levene_test(influencers ~ Advert_type*Condition, data = Exp2_Advert_techniques)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     3  1194     0.716 0.543

model_nonpara_Exp2_advert_influencers <- WRS2::bwtrim(influencers ~ Advert_type*Condition, 
                                                      id = ProlificID, 
                                                      data = Exp2_Advert_techniques,
                                                      tr = 0.05,
                                                      nboot = 5000)
model_nonpara_Exp2_advert_influencers

## Repetition
# Normality and homogeneity violated 
model <- aov(repetition ~ Advert_type*Condition, data = Exp2_Advert_techniques)
shapiro.test(model$residuals)
#W = 0.78678, p-value < 2.2e-16
rstatix::levene_test(repetition ~ Advert_type*Condition, data = Exp2_Advert_techniques)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     3  1194      6.55 0.000216

model_nonpara_Exp2_advert_repetition <- WRS2::bwtrim(repetition ~ Advert_type*Condition, 
                                                     id = ProlificID, 
                                                     data = Exp2_Advert_techniques,
                                                     tr = 0.05,
                                                     nboot = 5000)
model_nonpara_Exp2_advert_repetition

## Scarcity
# Normality and homogeneity violated 
model <- aov(scarcity ~ Advert_type*Condition, data = Exp2_Advert_techniques)
shapiro.test(model$residuals)
#W = 0.63049, p-value < 2.2e-16
rstatix::levene_test(scarcity ~ Advert_type*Condition, data = Exp2_Advert_techniques)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     3  1194      11.6 0.000000166

model_nonpara_Exp2_advert_scarcity <- WRS2::bwtrim(scarcity ~ Advert_type*Condition, 
                                                   id = ProlificID, 
                                                   data = Exp2_Advert_techniques,
                                                   tr = 0.05,
                                                   nboot = 5000)
model_nonpara_Exp2_advert_scarcity



################################################################################-
## Video usefulness ----

Exp2_vid <- Exp2_vid_techniques |>
  dplyr::select(c(ProlificID, Condition, dplyr::starts_with("Video_"))) |>
  dplyr::mutate(Condition = factor(Condition, levels = c("Control", "Inoculation")))

# summarise data by Condition
Exp2_vid_summary_byCondition <- Exp2_vid |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(across(where(is.numeric), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)))) |>
  dplyr::ungroup()


### Analyses ----

# test for t-test assumptions

# Check assumptions and run t-tests for each numeric variable
numeric_vars <- c("Video_interesting", "Video_informative", "Video_useful")

results <- lapply(numeric_vars, function(var) {
  # Subset data for the variable
  data <- Exp2_vid %>% select(Condition, !!sym(var))
  
  # Check normality
  shapiro_test <- shapiro.test(data[[var]])
  
  # Check homogeneity of variances
  levene_test <- leveneTest(data[[var]] ~ data$Condition)
  
  # Run t-test
  t_test <- t.test(data[[var]] ~ data$Condition)
  
  list(
    variable = var,
    shapiro_test = shapiro_test,
    levene_test = levene_test,
    t_test = t_test
  )
})

# Print results
results


# Run non-parametric tests for each numeric variable.
# using Mann_Whitney_U test as the data is not normally distributed and the variances are not equal


# Run Mann-Whitney U test for each numeric variable
Exp2_vid_results <- lapply(numeric_vars, function(var) {
  # Subset data for the variable
  data <- Exp2_vid %>% select(Condition, !!sym(var))
  
  # Run Mann-Whitney U test
  test_result <- wilcox.test(data[[var]] ~ data$Condition)
  
  list(
    variable = var,
    test_result = test_result
  )
})

# Print results
Exp2_vid_results
