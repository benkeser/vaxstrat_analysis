# ---------------------------------------------------
# Make latex table for simulation 1 results
# ---------------------------------------------------

here::i_am("make_sim_1_table.R")

library(dplyr)
library(knitr)
library(kableExtra)

# read results 
default_summary <- readRDS(here::here("results/sim_1/default_summary.Rds"))
violate_er_summary <- readRDS(here::here("results/sim_1/violate_er_summary.Rds"))
violate_cw_summary <- readRDS(here::here("results/sim_1/violate_cw_summary.Rds"))
violate_cw_er_summary <- readRDS(here::here("results/sim_1/violate_cw_er_summary.Rds"))
  
summary_list <- list(
  "Default"      = default_summary,
  "Violate ER"   = violate_er_summary,
  "Violate CW"   = violate_cw_summary,
  "Violate Both" = violate_cw_er_summary
)

format_section <- function(df, n_value) {
  df %>%
    filter(n == n_value) %>%
    mutate(
      method = factor(
        method,
        levels = c("aipw_CW", "aipw_ER", "aipw_ER_CW"),
        labels = c("CW AIPW", "ER AIPW", "CW & ER AIPW")
      )
    ) %>%
    arrange(method) %>%
    select(
      Method = method,
      `Bias` = bias_additive,
      `Variance`  = var_additive,
      `Coverage`  = coverage_additive,
      `Bias ` = bias_mult,
      `Variance `  = var_mult,
      `Coverage `  = coverage_mult
    )
}



make_latex_table <- function(summary_list, n_value, caption, label) {
  
  section_tables <- lapply(summary_list, format_section, n_value = n_value)
  
  combined <- bind_rows(section_tables, .id = "Scenario")
  
  kable(
    combined %>% select(-Scenario),
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    caption = caption,
    label = label,
    align = "lcccccc"
  ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c(
      " " = 1,
      "Additive Scale" = 3,
      "Multiplicative Scale" = 3
    )) %>%
    group_rows(
      index = sapply(section_tables, nrow),
      group_label = names(section_tables)
    )
}

# n = 500
make_latex_table(
  summary_list,
  n_value = 500,
  caption = "Bias, Variance, and Coverage (n = 500)",
  label = "tab:bias_var_cov_n500"
)

# n = 4000
make_latex_table(
  summary_list,
  n_value = 4000,
  caption = "Bias, Variance, and Coverage (n = 4000)",
  label = "tab:bias_var_cov_n4000"
)



