################################################################################-
#
# CSC+ inoculation 2025
# Script by Maris Vainre, Lisa Doan and Alicia Smith
# Descriptive data
#
################################################################################-





################################################################################-
# Setup ----
################################################################################-

library(here)
library(dplyr)
library(tidyr)
library(Hmisc)
library(ggplot2)
library(hrbrthemes)

source(here::here("./Code/Write-up/CSCplus_0_Formatting.R"))


Exp2_cleaned <- readRDS(here::here("./Data/Experiment2/1_cleaned/CSCplus_exp2.rds"))


                        
################################################################################-
# Participants ----
################################################################################-

## Demographics ----

Demographics <- Exp2_cleaned |>
  dplyr::select(c(ProlificID, Condition, gender, Age, ethnicity))

Age <- Demographics |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(n      = n(),
                   missing = sum(is.na(Age)), 
                   age_m  = mean(as.numeric(Age), na.rm = TRUE),
                   age_sd = sd(as.numeric(Age), na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = c(n, age_m, age_sd), names_to = "metric", values_to = "value") |>
  dplyr::mutate(demographic = "Age",
                Condition = as.character(Condition)) |>
  dplyr::select(demographic, Condition, metric, value)

AgeT <- Demographics |>
  dplyr::summarise(n      = n(),
                   missing = sum(is.na(Age)), 
                   age_m  = mean(as.numeric(Age), na.rm = TRUE),
                   age_sd = sd(as.numeric(Age), na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = c(n, age_m, age_sd), names_to = "metric", values_to = "value") |>
  dplyr::mutate(demographic = "Age",
                Condition = "Total") |>
  dplyr::select(demographic, Condition, metric, value)

Gender <- Demographics |>
  dplyr::group_by(Condition, gender) |>
  dplyr::summarise(gender_n = n()) |>
  dplyr::mutate(metric = paste0("gender_", gender)) |>
  dplyr::select(Condition, metric, gender_n) |>
  dplyr::rename(value = gender_n) |>
  dplyr::mutate(demographic = "Gender",
                Condition = as.character(Condition)) |>
  dplyr::select(demographic, Condition, metric, value)

GenderT <- Demographics |>
  dplyr::group_by(gender) |>
  dplyr::summarise(gender_n = n()) |>
  dplyr::mutate(metric = paste0("gender_", gender),
                Condition = "Total") |>
  dplyr::select(Condition, metric, gender_n) |>
  dplyr::rename(value = gender_n) |>
  dplyr::mutate(demographic = "Gender") |>
  dplyr::select(demographic, Condition, metric, value)


Ethnicity <- Demographics |>
  dplyr::group_by(Condition, ethnicity) |>
  dplyr::summarise(ethnicity_n = n()) |>
  dplyr::mutate(metric = paste0("ethnicity_", ethnicity)) |>
  dplyr::select(Condition, metric, ethnicity_n) |>
  dplyr::rename(value = ethnicity_n) |>
  dplyr::mutate(demographic = "Ethnicity",
                Condition = as.character(Condition)) |>
  dplyr::select(demographic, Condition, metric, value)


EthnicityT <- Demographics |>
  dplyr::group_by(ethnicity) |>
  dplyr::summarise(ethnicity_n = n()) |>
  dplyr::mutate(metric = paste0("ethnicity_", ethnicity),
                Condition = "Total") |>
  dplyr::select(Condition, metric, ethnicity_n) |>
  dplyr::rename(value = ethnicity_n) |>
  dplyr::mutate(demographic = "Ethnicity") |>
  dplyr::select(demographic, Condition, metric, value)


Demographics_summary <- dplyr::bind_rows(Age, AgeT, Gender, GenderT, Ethnicity, EthnicityT) |>
  tidyr::pivot_wider(names_from = Condition, values_from = value) |>
  dplyr::mutate(metric = sub(".*_", "", metric),
                demographic = ifelse(metric == "n", "Participants", demographic))


write.csv(Demographics_summary, here::here("./Data/Experiment2/4_results/CSCplus_exp2_demographics.csv"), row.names = FALSE)


rm(Age, AgeT, Ethnicity, EthnicityT, Gender, GenderT, Demographics)







## Background info ----

Background <- Exp2_cleaned |>
  dplyr::select(c(ProlificID, Condition, Part_selfImp_type, Part_selfImp_YN:Other, 
                  Lifetime_Meditation, MindfulnessCommon,
                  check2_IntWellness))


# Summarize numeric variables per condition
Numeric_summary_per_condition <- Background %>%
  dplyr::group_by(Condition) |>
  dplyr::summarise(sum_stress = sum(`Stress or time-management`, na.rm = TRUE),
                   sum_physEx = sum(`Physical exercise, like yoga or a running club`, na.rm = TRUE),
                   sum_medit = sum(`Mindfulness or other contemplative practices`, na.rm = TRUE),
                   sum_other = sum(`Other`, na.rm = TRUE),
                   Mindfulness_common_m  = mean(as.numeric(MindfulnessCommon), na.rm = TRUE),
                   Mindfulness_common_sd = sd(as.numeric(MindfulnessCommon), na.rm = TRUE))

Numeric_summary_total <- Background %>%
  dplyr::summarise(sum_stress = sum(`Stress or time-management`, na.rm = TRUE),
                   sum_physEx = sum(`Physical exercise, like yoga or a running club`, na.rm = TRUE),
                   sum_medit = sum(`Mindfulness or other contemplative practices`, na.rm = TRUE),
                   sum_other = sum(`Other`, na.rm = TRUE),
                   Mindfulness_common_m  = mean(as.numeric(MindfulnessCommon), na.rm = TRUE),
                   Mindfulness_common_sd = sd(as.numeric(MindfulnessCommon), na.rm = TRUE)) |>
  dplyr::mutate(Condition = "Total")


# Summarize categorical variables per condition
Categorical_summary_per_condition <- Background %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(sum_Partselfh = list(table(Part_selfImp_YN)),
                   Lifetime_Meditation_counts = list(table(Lifetime_Meditation)),
                   
                   #count cases where check2_IntWellness is "No" or "Not really"
                   sum_check2_IntWellness_no = sum(check2_IntWellness %in% c(0, 1)),
                   sum_check2_IntWellness_yes = sum(check2_IntWellness %in% c(2, 3)))

# Summarize categorical variables overall
Categorical_summary_total <- Background %>%
  dplyr::summarise(sum_Partselfh = list(table(Part_selfImp_YN)),
                   Lifetime_Meditation_counts = list(table(Lifetime_Meditation)),
                   #count cases where check2_IntWellness is "No" or "Not really"
                   sum_check2_IntWellness_no = sum(check2_IntWellness %in% c(0, 1)),
                   sum_check2_IntWellness_yes = sum(check2_IntWellness %in% c(2, 3))) |>
  dplyr::mutate(Condition = "Total")

# Unnest the lists in categorical summaries
Categorical_summary_per_condition <- Categorical_summary_per_condition %>%
  tidyr::unnest_wider(sum_Partselfh) %>%
  tidyr::unnest_wider(Lifetime_Meditation_counts)

Categorical_summary_total <- Categorical_summary_total %>%
  tidyr::unnest_wider(sum_Partselfh) %>%
  tidyr::unnest_wider(Lifetime_Meditation_counts)

# Combine numeric and categorical summaries per condition
Summary_per_condition <- dplyr::left_join(Numeric_summary_per_condition, Categorical_summary_per_condition, by = "Condition") |>
  dplyr::mutate(Condition = as.character(Condition))

# Combine numeric and categorical summaries overall
Summary_total <- dplyr::bind_cols(Numeric_summary_total, Categorical_summary_total %>% 
                                    select(-Condition))

# Combine per condition and overall summaries
Backgr_summary <- dplyr::bind_rows(Summary_per_condition, Summary_total) |>
  dplyr::mutate(across(-Condition, as.numeric)) |>
  tidyr::pivot_longer(cols = -Condition, names_to = "Variable", values_to = "Value") |>
  tidyr::pivot_wider(names_from = Condition, values_from = Value) |>
  dplyr::mutate(metric = dplyr::case_when(Variable == "Mindfulness_common_m" ~ "m",
                                          Variable == "Mindfulness_common_sd" ~ "sd",
                                          TRUE ~ "n"),
                subcategory = dplyr::case_when(Variable == "sum_stress" ~ "Stress or time-management",
                                               Variable == "sum_physEx" ~ "Physical exercise, like yoga or a running club",
                                               Variable == "sum_medit"  ~ "Mindfulness or other contemplative practices",
                                               Variable == "sum_other"  ~ "Other",
                                               Variable == "Mindfulness_common_m"  ~ "",
                                               Variable == "Mindfulness_common_sd" ~ "",
                                               Variable == "sum_check2_IntWellness_no" ~ "No",
                                               Variable == "sum_check2_IntWellness_yes" ~ "Yes",
                                               TRUE ~ Variable),
                variable = dplyr::case_when(Variable == "sum_stress" ~ "Self-improvement programme type", 
                                            Variable == "sum_physEx" ~ "Self-improvement programme type", 
                                            Variable == "sum_medit"  ~ "Self-improvement programme type", 
                                            Variable == "sum_other"  ~ "Self-improvement programme type", 
                                            Variable == "Yes" ~ "Participated in self-improvement programme",
                                            Variable == "No" ~ "Participated in self-improvement programme",
                                            Variable == "Mindfulness_common_m"  ~ "Mindfulness common in community",
                                            Variable == "Mindfulness_common_sd" ~ "Mindfulness common in community",
                                            Variable == "sum_check2_IntWellness_no" ~ "Interested in wellness (post-experiment)",
                                            Variable == "sum_check2_IntWellness_yes" ~ "Interested in wellness (post-experiment)",
                                            TRUE ~ "Lifetime experience with meditation")) |>
  dplyr::relocate(metric, .after = Variable) |>
  dplyr::relocate(c(variable, subcategory), .after = Variable) |>
  dplyr::select(-Variable) |>
  dplyr::mutate(variable = factor(variable, levels = c("Participated in self-improvement programme", 
                                                       "Self-improvement programme type",
                                                       "Lifetime experience with meditation", 
                                                       "Mindfulness common in community",
                                                       "Interested in wellness (post-experiment)"))) |>
  dplyr::arrange(variable)


write.csv(Backgr_summary, here::here("./Data/Experiment2/4_results/CSCplus_exp2_Backgr.csv"), row.names = FALSE)


rm(Categorical_summary_total, Categorical_summary_per_condition, 
   Numeric_summary_total, Numeric_summary_per_condition, 
   Summary_total, Summary_per_condition)



################################################################################-
# Outcomes ----

Exp2_outcomes <- Exp2_cleaned |>
  dplyr::select(c(ProlificID, Condition, Advert1_type, Advert2_type, Likelihood1, Likelihood2)) |>
  tidyr::pivot_longer(
    cols = c(Advert1_type, Advert2_type, Likelihood1, Likelihood2),
    names_to = c(".value", "advert_number"),
    names_pattern = "(Advert|Likelihood)(\\d)"
  ) |>
  dplyr::mutate(Advert = factor(Advert, levels = c("balanced", "inflated")),
                Condition = factor(Condition, levels = c("Control", "Inoculation")),
                Likelihood = as.numeric(Likelihood))


Exp2_outcomes_summary <- Exp2_outcomes |>
  dplyr::group_by(Advert, Condition) |>
  dplyr::summarise(likelihood_m  = mean(Likelihood, na.rm = TRUE),
                   likelihood_sd = sd(Likelihood, na.rm = TRUE))


fig_exp2_mainOutcome <- Exp2_outcomes %>% 
  ggplot(mapping = aes(x = Condition, y = Likelihood, colour = Advert, fill = Advert)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8), alpha = 1) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Ratings of likelihood of joining programme", 
       x = "Condition", 
       y = "Likelihood of joining the programme") +
  scale_fill_manual(values = c(unimelb_blue, unimelb_red)) +  # Specify the colors
  scale_color_manual(values = c(unimelb_blue, unimelb_red)) +  # Specify the colors
  theme_ipsum() +  # Apply hrbrthemes theme
  theme(
    legend.position = "top",  # Move legend to top
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )



################################################################################-
# Techniques and advert reflection ----

Exp2_vid_techniques <- Exp2_cleaned |>
  dplyr::select(c(ProlificID, Condition, dplyr::starts_with("Advert"), dplyr::starts_with("Tech"), dplyr::starts_with("Video_"))) |>
  
  # mutate Video_ variables to numeric
  dplyr::mutate(across(dplyr::starts_with("Video_"), as.numeric)) |>
  dplyr::mutate(across(dplyr::starts_with("Tech"), as.numeric)) |>
  #capitalise values
  dplyr::mutate(Advert1_type = stringr::str_to_title(Advert1_type),
                Advert2_type = stringr::str_to_title(Advert2_type)) 
  

## Techniques ----

long_df <- Exp2_vid_techniques |>
  dplyr::select(c(Condition, Advert1_type, Advert2_type, dplyr::starts_with("Tech"))) |>
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
  dplyr::select(-Tech) |>
  dplyr::rename(`Emotional appeal` = emoappeal,
                Influencers = influencers,
                Repetition = repetition,
                `Inflation of effects` = inflation,
                `Scarcity of product` = scarcity)

Exp2_Advert_techniques_long <- Exp2_Advert_techniques |>
  tidyr::pivot_longer(cols = -c(Condition, Advert_type), names_to = "variable", values_to = "value") |>
  dplyr::mutate(Condition = factor(Condition, levels = c("Control", "Inoculation")),
                Advert_type = factor(Advert_type, levels = c("Balanced", "Inflated")))


# Calculate the mean value for each variable
Exp2_Advert_techniques_summary <- Exp2_Advert_techniques_long %>%
  dplyr::group_by(Condition, Advert_type, variable) %>%
  dplyr::summarise(mean_value = mean(value), sd_value = sd(value), .groups = 'drop')

# Create the barplot with geom_jitter
Fig_Exp2_tech <- ggplot(data = Exp2_Advert_techniques_summary, aes(x = variable, y = mean_value)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = unimelb_blue, alpha = 0.75) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2, position = position_dodge(0.9)) +
  #geom_jitter(data = Exp2_Advert_techniques_long, aes(x = variable, y = value), width = 0.2, height = 0.3, color = "red") +
  facet_grid(Advert_type ~ Condition, labeller = labeller(Advert_type = Hmisc::capitalize)) +
  theme_ipsum() +  # Apply hrbrthemes theme
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        #legend.position = "top",  # Move legend to top
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor = element_blank()  # Remove minor grid lines
        ) +
  labs(#title = "Ratings of advertising techniques used", 
       x = "Technique", 
       y = "Perceived amount used")

Fig_Exp2_tech <- ggplot(data = Exp2_Advert_techniques_summary, aes(x = variable, y = mean_value, 
                                                                   color = Condition, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Advert_type, labeller = labeller(Advert_type = Hmisc::capitalize)) +
  scale_fill_manual(values = c("Control" = unimelb_blue, "Inoculation" = unimelb_red)) +
  scale_color_manual(values = c("Control" = unimelb_blue, "Inoculation" = unimelb_red)) +
  #scale_fill_manual(values = c("Balanced" = unimelb_blue, "Inflated" = unimelb_red)) +
  #scale_color_manual(values = c("Balanced" = unimelb_blue, "Inflated" = unimelb_red)) +
  theme_ipsum() +  # Apply hrbrthemes theme
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        legend.position = "top"
  ) +
  labs(x = "Technique", 
       y = "Perceived amount used",
       fill = "Condition",
       color = "Condition")


# create a summary table for presentation
Exp2_Advert_techniques_df <- Exp2_Advert_techniques_summary %>%
  mutate(mean_value = round(mean_value, roundto), sd_value = round(sd_value, roundto)) %>%
  tidyr::pivot_wider(names_from = c(Condition, variable), values_from = c(mean_value, sd_value), names_glue = "{Condition}_{variable}_{.value}")

colnames(Exp2_Advert_techniques_df) <- gsub("_value", "", colnames(Exp2_Advert_techniques_df))

# List of columns in the desired order
desired_order <- c(
  "Advert_type",
  "Control_Emotional appeal_mean", "Control_Emotional appeal_sd",
  "Control_Inflation of effects_mean", "Control_Inflation of effects_sd",
  "Control_Influencers_mean", "Control_Influencers_sd",
  "Control_Repetition_mean", "Control_Repetition_sd",
  "Control_Scarcity of product_mean", "Control_Scarcity of product_sd",
  "Inoculation_Emotional appeal_mean", "Inoculation_Emotional appeal_sd",
  "Inoculation_Inflation of effects_mean", "Inoculation_Inflation of effects_sd",
  "Inoculation_Influencers_mean", "Inoculation_Influencers_sd",
  "Inoculation_Repetition_mean", "Inoculation_Repetition_sd",
  "Inoculation_Scarcity of product_mean", "Inoculation_Scarcity of product_sd"
)

# Reorder columns
Exp2_Advert_techniques_df <- Exp2_Advert_techniques_df %>%
  dplyr::select(all_of(desired_order))

## Video usefulness ----

# create a df summarising the average ratings for Video_ variables per condition
Exp2_vid <- Exp2_vid_techniques |>
  dplyr::select(c(Condition, dplyr::starts_with("Video_"))) |>
  dplyr::mutate(Condition = factor(Condition, levels = c("Control", "Inoculation")))

# Calculate the mean and standard deviation for each Video_rating
Exp2_Video_ratings_summary <- Exp2_vid %>%
  tidyr::pivot_longer(cols = starts_with("Video"), names_to = "Video_rating", values_to = "value") %>%
  dplyr::group_by(Condition, Video_rating) %>%
  dplyr::summarise(mean_value = mean(value), 
                   sd_value = sd(value), .groups = 'drop') |>
  dplyr::mutate(Video_rating = dplyr::case_when(Video_rating == "Video_informative" ~ "Informative",
                                                Video_rating == "Video_interesting" ~ "Interesting",
                                                Video_rating == "Video_useful" ~ "Useful"))

# create a summary table for presentation
Exp2_Video_ratings_df <- Exp2_Video_ratings_summary %>%
  dplyr::mutate(mean_value = round(mean_value, roundto), sd_value = round(sd_value, roundto)) %>%
  tidyr::pivot_wider(names_from = Condition, values_from = c(mean_value, sd_value), names_glue = "{Condition}_{.value}")


#Exp2_Video_ratings_Fig <- ggplot(data = Exp2_Video_ratings_summary, aes(x = Video_rating, y = mean_value)) +
#  geom_bar(stat = "identity", position = position_dodge(), fill = unimelb_blue, alpha = 0.75) +
#  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2, position = position_dodge(0.9)) +
#  facet_wrap(~ Condition) +
#  theme_minimal() +
#  theme_ipsum() +  # Apply hrbrthemes theme
#  theme(#axis.text.x = element_text(angle = 20, hjust = 1),
#        #legend.position = "top",  # Move legend to top
#        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
#        panel.grid.minor = element_blank()  # Remove minor grid lines
#  ) +
#  scale_y_continuous(limits = c(0, 100)) +
#  labs(#title = "Mean Video Ratings by Condition",
#       x = "Video was...",
#       y = "Agreement (100 = completely agree)")

# Reshape data from wide to long format
Exp2_vid_long <- Exp2_vid %>%
  pivot_longer(cols = starts_with("Video_"), names_to = "Video_rating", values_to = "value")

custom_labels <- c(
  "Video_interesting" = "Interesting",
  "Video_informative" = "Informative",
  "Video_useful" = "Useful"
)

# Create the plot
Exp2_Video_ratings_Fig <- ggplot(data = Exp2_vid_long, aes(x = Video_rating, y = value, color = Condition, fill = Condition)) +
  geom_boxplot(position = position_dodge(0.9), alpha = 0.75,outlier.shape = NA) +
  geom_jitter(aes(color = Condition), position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.9), alpha = 0.5) +
  scale_fill_manual(values = c("Control" = unimelb_blue, "Inoculation" = unimelb_red)) + 
  scale_color_manual(values = c("Control" = unimelb_blue, "Inoculation" = unimelb_red)) + 
  scale_x_discrete(labels = custom_labels) + 
  theme_minimal() +
  theme_ipsum() +  # Apply hrbrthemes theme
  theme(
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "top"
    ) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Video was...",
    y = "Agreement (100 = completely agree)"
  )

Exp2_Video_ratings_Fig

################################################################################-
# Participants ----
################################################################################-

# pivot longer
data_compact <- Exp2_cleaned |>
  dplyr::select(c(ProlificID, Condition, Advert1_type, Advert2_type, Likelihood1, Likelihood2)) |>
  tidyr::pivot_longer(
    cols = c(Advert1_type, Advert2_type, Likelihood1, Likelihood2),
    names_to = c(".value", "advert_number"),
    names_pattern = "(Advert|Likelihood)(\\d)"
  ) |>
  dplyr::mutate(Advert = factor(Advert, levels = c("balanced", "inflated")),
                Condition = factor(Condition, levels = c("Control", "Inoculation")))


Outcomes_desc <- data_compact |>
  dplyr::group_by(Condition, Advert) |>
  dplyr::summarise(n = n(),
                   m = mean(Likelihood),
                   sd = sd(Likelihood))


