################################################################################-
#
# CSC+ Experiment 1
# Graphs
# Maris Vainre and Alicia Smith 2025
#
################################################################################-



################################################################################-
# Setup ----
################################################################################-

library(here)
library(dplyr)
library(tidyr)
library(haven)
library(Hmisc)
library(ggplot2)
library(hrbrthemes)
library(ggrain)

source(here::here("./Code/Write-up/CSCplus_0_Formatting.R"))

Exp1_cleaned <- readRDS(here::here("./Data/Experiment1/1_cleaned/CSCplus_exp1_cleaned.rds"))




################################################################################-
# Participants ----
################################################################################-



## Demographics ----

Demographics <- Exp1_cleaned |>
  dplyr::select(c(PROLIFIC_PID, Condition, gender, Age, ethnicity))

Age <- Demographics |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(n      = n(),
                   age_m  = mean(as.numeric(Age)),
                   age_sd = sd(as.numeric(Age))) |>
  tidyr::pivot_longer(cols = c(n, age_m, age_sd), names_to = "metric", values_to = "value") |>
  dplyr::mutate(demographic = "Age",
                Condition = as.character(Condition)) |>
  dplyr::select(demographic, Condition, metric, value)

AgeT <- Demographics |>
  dplyr::summarise(n      = n(),
                   age_m  = mean(as.numeric(Age)),
                   age_sd = sd(as.numeric(Age))) |>
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


write.csv(Demographics_summary, here::here("./Data/Experiment1/4_results/CSCplus_exp1_demographics.csv"), row.names = FALSE)


rm(Age, AgeT, Ethnicity, EthnicityT, Gender, GenderT, Demographics)




## Background info ----

Background <- Exp1_cleaned |>
  dplyr::select(c(PROLIFIC_PID, Condition, Part_selfImp, 
                  `Stress or time-management`:Other, Lifetime_Meditation, Mindfulness_common))


# Summarize numeric variables per condition
Numeric_summary_per_condition <- Background %>%
  dplyr::group_by(Condition) |>
  dplyr::summarise(sum_stress = sum(`Stress or time-management`, na.rm = TRUE),
                   sum_physEx = sum(`Physical exercise, like yoga or a running club`, na.rm = TRUE),
                   sum_medit = sum(`Mindfulness or other contemplative practices`, na.rm = TRUE),
                   sum_other = sum(`Other`, na.rm = TRUE),
                   Mindfulness_common_m  = mean(as.numeric(Mindfulness_common), na.rm = TRUE),
                   Mindfulness_common_sd = sd(as.numeric(Mindfulness_common), na.rm = TRUE))

Numeric_summary_total <- Background %>%
  dplyr::summarise(sum_stress = sum(`Stress or time-management`, na.rm = TRUE),
                   sum_physEx = sum(`Physical exercise, like yoga or a running club`, na.rm = TRUE),
                   sum_medit = sum(`Mindfulness or other contemplative practices`, na.rm = TRUE),
                   sum_other = sum(`Other`, na.rm = TRUE),
                   Mindfulness_common_m  = mean(as.numeric(Mindfulness_common), na.rm = TRUE),
                   Mindfulness_common_sd = sd(as.numeric(Mindfulness_common), na.rm = TRUE)) |>
  dplyr::mutate(Condition = "Total")


# Summarize categorical variables per condition
Categorical_summary_per_condition <- Background %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(sum_Partselfh = list(table(Part_selfImp)),
                   Lifetime_Meditation_counts = list(table(Lifetime_Meditation)))

# Summarize categorical variables overall
Categorical_summary_total <- Background %>%
  dplyr::summarise(sum_Partselfh = list(table(Part_selfImp)),
                   Lifetime_Meditation_counts = list(table(Lifetime_Meditation))) |>
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
                                               TRUE ~ Variable),
                variable = dplyr::case_when(Variable == "sum_stress" ~ "Self-improvement programme type", 
                                            Variable == "sum_physEx" ~ "Self-improvement programme type", 
                                            Variable == "sum_medit"  ~ "Self-improvement programme type", 
                                            Variable == "sum_other"  ~ "Self-improvement programme type", 
                                            Variable == "Yes" ~ "Participated in self-improvement programme",
                                            Variable == "No" ~ "Participated in self-improvement programme",
                                            Variable == "Mindfulness_common_m"  ~ "Mindfulness common in community",
                                            Variable == "Mindfulness_common_sd" ~ "Mindfulness common in community",
                                            TRUE ~ "Lifetime experience with meditation")) |>
  dplyr::relocate(metric, .after = Variable) |>
  dplyr::relocate(c(variable, subcategory), .after = Variable) |>
  dplyr::select(-Variable) |>
  dplyr::mutate(variable = factor(variable, levels = c("Participated in self-improvement programme", 
                                                       "Self-improvement programme type",
                                                       "Lifetime experience with meditation", 
                                                       "Mindfulness common in community"))) |>
  dplyr::arrange(variable)


write.csv(Backgr_summary, here::here("./Data/Experiment1/4_results/CSCplus_exp1_Backgr.csv"), row.names = FALSE)


rm(Categorical_summary_total, Categorical_summary_per_condition, 
   Numeric_summary_total, Numeric_summary_per_condition, 
   Summary_total, Summary_per_condition)


## Analyse demographics + background info ----

# Age

age_anova <- aov(Age ~ Condition, data = Demographics)
summary(age_anova)
# Df Sum Sq Mean Sq F value Pr(>F)
# Condition     3     27    8.85   0.051  0.985
# Residuals   553  96517  174.53               

# Gender

table_gender <- table(Demographics$Condition, Demographics$gender)
chisq.test(table_gender)
# X-squared = 7.8728, df = 6, p-value = 0.2476

# Ethnicity

table_eth <- table(Demographics$Condition, Demographics$ethnicity)
chisq.test(table_eth)
# X-squared = 8.1671, df = 9, p-value = 0.5174

# Self-improvement programme

Self_imp <- Background %>%
  subset(select = c("Condition", "Part_selfImp"))

table_selfImp <- table(Self_imp$Condition, Self_imp$Part_selfImp)
chisq.test(table_selfImp)
# X-squared = 1.3305, df = 3, p-value = 0.7219

# Stress/time management

Stress <- Background %>%
  subset(select = c("Condition", "Stress or time-management")) %>%
  rename(stress = `Stress or time-management`) %>%
  mutate(stress = ifelse(is.na(stress), 0, 1))

table_stress <- table(Stress$Condition, Stress$stress)
chisq.test(table_stress)
# X-squared = 0.53191, df = 3, p-value = 0.9118

# Physical exercise

physical_ex <- Background %>%
  subset(select = c("Condition", "Physical exercise, like yoga or a running club")) %>%
  rename(Exercise = `Physical exercise, like yoga or a running club`) %>%
  mutate(Exercise = ifelse(is.na(Exercise), 0, 1))

table_exercise <- table(physical_ex$Condition, physical_ex$Exercise)
chisq.test(table_exercise)
# X-squared = 2.6677, df = 3, p-value = 0.4457

# Mindfulness/contemplative

Mindfulness <- Background %>%
  subset(select = c("Condition", "Mindfulness or other contemplative practices")) %>%
  rename(mindfulness = `Mindfulness or other contemplative practices`) %>%
  mutate(mindfulness = ifelse(is.na(mindfulness), 0, 1))

table_mindfulness <- table(Mindfulness$Condition, Mindfulness$mindfulness)
chisq.test(table_mindfulness)
# X-squared = 1.6731, df = 3, p-value = 0.6429

# Mindfulness in community

Community <- Background %>%
  subset(select = c("Condition", "Mindfulness_common")) 

community_anova <- aov(Mindfulness_common ~ Condition, data = Community)
summary(community_anova)
# Df Sum Sq Mean Sq F value Pr(>F)
# Condition     3   1349   449.7   0.725  0.537
# Residuals   553 342784   619.9   

################################################################################-
# Outcomes ----

Exp1_outcomes <- Exp1_cleaned |>
  dplyr::select(PROLIFIC_PID, Condition, Reliable, Likelihood, dplyr::starts_with("Tech_"), dplyr::starts_with("Video_"))


Exp1_outcomes_summary <- Exp1_cleaned |>
  dplyr::select(PROLIFIC_PID, Condition, Reliable, Likelihood) |>
  dplyr::group_by(Condition) |>
  dplyr::summarise(rel_m  = mean(Reliable, na.rm = TRUE),
                   rel_sd = sd(Reliable, na.rm = TRUE),
                   likelihood_m  = mean(Likelihood, na.rm = TRUE),
                   likelihood_sd = sd(Likelihood, na.rm = TRUE))



## Main outcome - reliability ----



fig_exp1_mainOutcome <- ggplot(Exp1_outcomes, aes(x = Condition, y = Reliable)) +
  geom_boxplot(outlier.shape = NA, fill = unimelb_blue, color = unimelb_blue,  alpha = 0.6) +  # Boxplot without outliers
  geom_jitter(width = 0.2,  color = unimelb_blue) +  # Scatter plot (jitter)
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +  # Set y-axis limits and breaks
  scale_x_discrete(labels = cond_lbls) +
  labs(
    y = "Perceived reliability of advert (lower is better)",
    x = "Condition"
  ) +
  theme(
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

## Main outcome - reliability ---- Alternative raincloud plot
# Side by side

ggplot(Exp1_outcomes, aes(1, x = Condition, y = Reliable, fill = Condition, color = Condition)) +
  geom_rain(alpha = 0.6, rain.side = 'l',
    boxplot.args = list(
      color = "black",
      outlier.shape = NA,
      show.legend = FALSE
    ),
    boxplot.args.pos = list(position = ggpp::position_dodge2nudge(x = .15, direction = "split"),
      width = 0.1
    ),
    violin.args.pos = list(side = "r", width = 0.7, position = position_nudge(x = 0.22)
    )
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x.bottom = element_line(size = 0.8),
    axis.line.y.left = element_line(size = 0.8),
    axis.ticks = element_line(size = 0.8),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  xlab("Condition") + ylab("Perceived Reliability of Advert (lower is better)") +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +  # Set y-axis limits and breaks
  scale_x_discrete(labels = cond_lbls) +
  scale_fill_brewer(palette = 'Paired') +
  scale_color_brewer(palette = 'Paired') +
  guides(fill = 'none', color = 'none')

# Overlaid 

ggplot(Exp1_outcomes, aes(1, Reliable, fill = Condition, color = Condition)) +
  geom_rain(alpha = .4, rain.side = 'l',
            boxplot.args = list(color = "black", outlier.shape = NA),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = 0.1, width = 0.1), width = 0.1
            ), 
            violin.args.pos = list(side = "r", width = 0.8, position = position_nudge(x = 0.17)
            )) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x.bottom = element_line(size = 0.8),
    axis.line.y.left = element_line(size = 0.8),
    axis.ticks.y =  element_line(size = 0.8),
    axis.ticks.x =  element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  xlab("Condition") + ylab("Perceived Reliability of Advert (lower is better)") +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +  # Set y-axis limits and breaks
  scale_fill_manual(
    values = c("Control" = "#88a0dc", 
               "Inflation" = "#7c4b73", 
               "Reaction" = "#f9d14a", 
               "InflationReaction" = "#ed968c"),
    labels = c("Control", "Benefit inflation", "Emotional manipulation", "Combined")) +
  scale_color_manual(
    values = c("Control" = "#88a0dc", 
               "Inflation" = "#7c4b73", 
               "Reaction" = "#f9d14a", 
               "InflationReaction" = "#ed968c"),
    labels = c("Control", "Benefit inflation", "Emotional manipulation", "Combined"))

## Secondary outcomes - likelihood of joining ----

fig_exp1_likelihood <- ggplot(Exp1_outcomes, aes(x = Condition, y = Likelihood)) +
  geom_boxplot(outlier.shape = NA, fill = unimelb_blue, color = unimelb_blue,  alpha = 0.6) +  # Boxplot without outliers
  geom_jitter(width = 0.2,  color = unimelb_blue) +  # Scatter plot (jitter)
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set y-axis limits and breaks
  scale_x_discrete(labels = cond_lbls) +
  labs(
    y = "Likelihood to join programme (lower is better)",
    x = "Condition"
  ) +
  theme(
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


### Techniques perceived ----


#Calculate means and standard deviations
Tech_vars <- Exp1_outcomes %>%
  dplyr::select(Condition, starts_with("Tech_")) %>%
  tidyr::pivot_longer(cols = starts_with("Tech_"), names_to = "Tech_Variable", values_to = "Value") %>%
  dplyr::group_by(Condition, Tech_Variable) %>%
  dplyr::summarise(mean = mean(Value, na.rm = TRUE),
                   sd   = sd(Value, na.rm = TRUE)) |>
  dplyr::mutate(Tech_Variable = dplyr::recode(Tech_Variable,
                                              "Tech_emoappeal" = "Emotional appeal",
                                              "Tech_inflation" = "Inflation of effects",
                                              "Tech_influencers" = "Influencers",
                                              "Tech_repetition" = "Repetition",
                                              "Tech_scarcity" = "Scarcity of product"))


Exp1_Advert_techniques_summary <- Tech_vars |>
  tidyr::pivot_wider(names_from = Tech_Variable,
    values_from = c(mean, sd),
    names_glue = "{Tech_Variable}_{.value}") |>
  dplyr::select(Condition, starts_with("Emotional"), starts_with("Inflation"), 
                starts_with("Influencers"), starts_with("Repetition"), starts_with("Scarcity"))




# Define colors for each condition
condition_colors <- c("Control" = unimelb_blue, 
                      "Inflation" = unimelb_yellow, 
                      "Reaction" = unimelb_green, 
                      "InflationReaction" = unimelb_red)

# Create the plot
Fig_exp1_Techs <- ggplot(Tech_vars, aes(x = Tech_Variable, y = mean, fill = Condition, color = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.6) +  # Barplot
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(width = 0.9)) +  # Error bars
  theme_ipsum() +
  #scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 1)) +  # Set y-axis limits and breaks
  coord_cartesian(ylim = c(0, 2)) +
  labs(
    y = "Average rating (higher = used more)",
    x = "Technique"
  ) +
  theme(
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(angle = 20, hjust = 1), 
    legend.position = "top"  # Move legend to the top
  ) +
  scale_fill_manual(values = condition_colors, labels = cond_lbls) +  # Use different colors for each condition
  scale_color_manual(values = condition_colors, labels = cond_lbls)  # Use different colors for each condition

Fig_exp1_Techs


### Video usefulness ----

# create a df summarising the average ratings for Video_ variables per condition
Exp1_vid <- Exp1_outcomes |>
  dplyr::select(c(Condition, dplyr::starts_with("Video_"))) |>
  dplyr::mutate(across(starts_with("Video"), as.numeric))

# Calculate the mean and standard deviation for each Video_rating
Exp1_Video_ratings_summary <- Exp1_vid %>%
  tidyr::pivot_longer(cols = starts_with("Video"), names_to = "Video_rating", values_to = "value") %>%
  dplyr::group_by(Condition, Video_rating) %>%
  dplyr::summarise(mean_value = mean(value), 
                   sd_value = sd(value), .groups = 'drop') |>
  dplyr::mutate(Video_rating = dplyr::case_when(Video_rating == "Video_informative" ~ "Informative",
                                                Video_rating == "Video_interesting" ~ "Interesting",
                                                Video_rating == "Video_useful" ~ "Useful"))

# create a summary table for presentation
Exp1_Video_ratings_df <- Exp1_Video_ratings_summary %>%
  dplyr::mutate(mean_value = round(mean_value, roundto), sd_value = round(sd_value, roundto)) %>%
  tidyr::pivot_wider(names_from = Condition, values_from = c(mean_value, sd_value), names_glue = "{Condition}_{.value}")

# Reshape data from wide to long format
Exp1_vid_long <- Exp1_vid %>%
  pivot_longer(cols = starts_with("Video_"), names_to = "Video_rating", values_to = "value")

custom_labels <- c(
  "Video_interesting" = "Interesting",
  "Video_informative" = "Informative",
  "Video_useful" = "Useful"
)

condition_colors <- c("Control" = unimelb_blue, 
                      "Inflation" = unimelb_yellow, 
                      "Reaction" = unimelb_green, 
                      "InflationReaction" = unimelb_red)

# Create the plot
Exp1_Video_ratings_Fig <- ggplot(data = Exp1_vid_long, aes(x = Video_rating, y = value, color = Condition, fill = Condition)) +
  geom_boxplot(position = position_dodge(0.9), alpha = 0.75,outlier.shape = NA) +
  geom_jitter(aes(color = Condition), position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.9), alpha = 0.5) +
  scale_fill_manual(values = condition_colors, labels = cond_lbls) +  # Use different colors for each condition
  scale_color_manual(values = condition_colors, labels = cond_lbls) +  # Use different colors for each condition
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

Exp1_Video_ratings_Fig

