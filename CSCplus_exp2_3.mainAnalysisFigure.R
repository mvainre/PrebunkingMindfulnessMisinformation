################################################################################-
#
# CSC+ inoculation 2025
# Script by Alicia Smith
# Main analysis Figure
#
################################################################################-


# Load required libraries
library(ggplot2)
library(dplyr)

# Define sample sizes
n_control <- 298
n_inoculation <- 301

# Create the summary data frame
summary_data <- data.frame(
  Advert = c("Balanced", "Balanced", "Inflated", "Inflated"),
  Condition = c("Control", "Inoculation", "Control", "Inoculation"),
  Mean = c(60.25, 53.15, 57.67, 45.85),
  SD = c(23.63, 25.18, 25.14, 27.98),
  n = c(n_control, n_inoculation, n_control, n_inoculation)  # Assign correct sample sizes
)

# Calculate 95% CI
summary_data <- summary_data %>%
  mutate(
    SE = SD / sqrt(n),
    CI = qt(0.975, df = n - 1) * SE
  )

# Create the column graph with SE error bars
ggplot(summary_data, aes(x = Advert, y = Mean, fill = Condition)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = Mean - CI, ymax = Mean + CI),
                position = position_dodge(0.6), width = 0.2) +
  scale_fill_manual(values = c("#88a0dc", "#ed968c")) +
  scale_color_manual(values = c("#88a0dc", "#ed968c")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "",
       x = "Advert Type",
       y = "Mean Response",
       fill = "Condition") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x.bottom = element_line(size = 0.8),
    axis.line.y.left = element_line(size = 0.8),
    axis.ticks.y =  element_line(size = 0.8),
    axis.ticks.x =  element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )