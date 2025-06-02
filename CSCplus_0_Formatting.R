################################################################################-
#
# CSC+ Write-up
# Formatting details
# Maris Vainre 2024
#
################################################################################-

library(flextable)
library(officer)



################################################################################-
# Specs ----
################################################################################-

options(scipen = 999)
roundto = 2 #decimal points to round to

################################################################################-
# Terms ----
################################################################################-

ctrl      <- "Control"
infl      <- "Inflation"
react     <- "Reaction"
inflreact <- "Inflation + Reaction"
inocul    <- "Inoculation"

cond_lbls <- c(ctrl, infl, react, inflreact)

tech_labs <- c(
  "Tech_emoappeal" = "Emotional appeal",
  "Tech_inflation" = "Inflation of benefits",
  "Tech_influencers" = "Influencers",
  "Tech_repetition" = "Repetition",
  "Tech_scarcity" = "Scarcity of product"
)



################################################################################-
# Colours ----
################################################################################-

unimelb_blue   <- "#000F46"
unimelb_red    <- "#FF2D3C"
unimelb_yellow <- "#FFD629"
unimelb_green  <- "#9FB825"




################################################################################-
# Tables ----
################################################################################-

padding_tbl <- 10

border_vline_heading <- fp_border(color = "black", width = 1)
border_vline_inner <- fp_border(color = "white", width = 1)

style_data <- fp_par(text.align = "left", padding = 1)
style_text <- fp_text(font.family = "Arial Narrow")
style_header <- update(style_text, bold = TRUE)


Journal_theme <- function(x, ...) {
  x <- colformat_double(x, big.mark = ",", decimal.mark = ".", digits = roundto)
  x <- hline_bottom(x, border = border_vline_heading)
  x <- hline_top(x, border = border_vline_heading, part = "all")
  x <- style(x, pr_p = style_data, pr_t = style_text, part = "all")
  x <- style(x, pr_t = style_header, part = "header")
  #x <- autofit(x, add_w = 4, add_h = 0)
  x <- set_table_properties(x, layout = "autofit", width = 1)
  x <- fit_to_width(x, 10)
  #x <- border_remove(x)
  #std_border <- fp_border(width = 1, color = "white")
  #x <- border_outer(x, part="all", border = std_border )
  #x <- border_inner_h(x, border = std_border, part="all")
  autofit(x)
}
