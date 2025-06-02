################################################################################-
#
# CSC+ inoculation 2025
# Script by Maris Vainre, Lisa Doan and Alicia Smith
# Main analysis
#
################################################################################-





################################################################################-
# Setup ----
################################################################################-

library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(rstatix)

set.seed(17092024) #for bootstrapping

Exp2_cleaned <- readRDS(here::here("./Data/Experiment2/1_cleaned/CSCplus_exp2.rds"))



################################################################################-
# Get only relevant data and convert to long format ----
################################################################################-

data_compact <- Exp2_cleaned |>
  dplyr::select(c(ProlificID, Condition, Advert1_type, Advert2_type, Likelihood1, Likelihood2)) |>
  tidyr::pivot_longer(
    cols = c(Advert1_type, Advert2_type, Likelihood1, Likelihood2),
    names_to = c(".value", "advert_number"),
    names_pattern = "(Advert|Likelihood)(\\d)"
  ) |>
  dplyr::mutate(Advert = factor(Advert, levels = c("balanced", "inflated")),
                Condition = factor(Condition, levels = c("Control", "Inoculation")),
                Likelihood = as.numeric(Likelihood))

################################################################################-
# Analysis ----
################################################################################-

# Boxplot 
fig_exp2_mainOutcome <-data_compact %>% 
  ggplot(mapping = aes(x = Condition, y = Likelihood, colour = Advert, fill = Advert)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.8), alpha = 1) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75)) +
  labs(x = "Condition", 
       y = "Likelihood of joining the programme") +
  scale_fill_manual(values = c("#000F46", "#FF2D3C")) +  # Specify the colors
  scale_color_manual(values = c("#000F46", "#FF2D3C")) +  # Specify the colors
  theme_ipsum() +  # Apply hrbrthemes theme
  theme(
    legend.position = "top",  # Move legend to top
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

#We pre-registered to do ANOVA

model <- aov(Likelihood ~ Condition*Advert, data = data_compact)

## Check assumptions of Mixed ANOVA ----

# Outliers 
ggplot(data_compact, aes(x = Condition, y = Likelihood)) +
  geom_boxplot() +
  facet_wrap(~ Advert) +
  labs(title = "Boxplot of Likelihood by Condition and Advert Number")

#no apparent outliers

### Normality - VIOLATED ----
ggplot(data_compact, aes(sample = Likelihood)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ interaction(Condition, advert_number)) +
  labs(title = "QQ Plots of Likelihood")

data_compact %>%
  dplyr::group_by(Condition, Advert) %>%
  rstatix::shapiro_test(Likelihood)

#violated for all groups:
# A tibble: 4 × 5
#Condition   Advert   variable   statistic          p
#<chr>       <chr>    <chr>          <dbl>      <dbl>
#1 Control     balanced Likelihood     0.977 0.0000988 
#2 Control     inflated Likelihood     0.974 0.0000333 
#3 Inoculation balanced Likelihood     0.977 0.000111  
#4 Inoculation inflated Likelihood     0.966 0.00000209


exp2_likelihood_shapiro <- shapiro.test(model$residuals)

#Shapiro-Wilk normality test
#
#data:  model$residuals
#W = 0.98442, p-value = 4.966e-10


### Homogeneity of variance - VIOLATED ----
exp2_likelihood_levene <- rstatix::levene_test(Likelihood ~ Condition*Advert, data = data_compact)

# A tibble: 1 × 4
#    df1   df2 statistic       p
#  <int> <int>     <dbl>   <dbl>
#1     3  1194      4.20 0.00577
#


# Homogeneity of covariance matrices 
rstatix::box_m(data_compact[,"Likelihood", drop=FALSE], data_compact$Condition)




## Fit the model ----

### Parametric test (assumptions violated!) ----

mixed_anova <- rstatix::anova_test(data = data_compact, 
                                   dv = Likelihood, 
                                   wid = ProlificID, 
                                   between = Condition, 
                                   within = Advert)
mixed_anova

#ANOVA Table (type III tests)
#
#            Effect DFn DFd      F           p p<.05   ges
#1        Condition   1 586 25.533 0.000000582     * 0.034
#2           Advert   1 586 27.641 0.000000205     * 0.009
#3 Condition:Advert   1 586  8.063 0.005000000     * 0.003


### Non-parametric test ----

model_nonpara <- WRS2::bwtrim(Likelihood ~ Advert*Condition, 
                              id = ProlificID, 
                              data = data_compact,
                              tr = 0.05,
                              nboot = 5000)
model_nonpara


#value df1      df2 p.value
#Condition        25.4232   1 534.8904  0.0000
#Advert           26.1632   1 526.6968  0.0000
#Condition:Advert  6.4904   1 526.6968  0.0111


# Post hoc test
WRS2::sppba(Likelihood ~ Condition*Advert, 
             id = ProlificID, 
             data = data_compact)

#Test statistics:
#  Estimate
#balanced-inflated     4.37
#
#Test whether the corrresponding population parameters are the same:
#  p-value: 0 

#interaction only
WRS2::sppbi(formula = Likelihood ~ Condition * Advert, id = ProlificID, 
            data = data_compact)
#Test statistics:
#  Estimate
#balanced-inflated Control-Inoculation   -4.545
#
#Test whether the corrresponding population parameters are the same:
#  p-value: 0.026 

