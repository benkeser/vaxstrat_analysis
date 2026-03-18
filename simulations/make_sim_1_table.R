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

format_section <- function(df) {
  df %>%
    # filter(n == n_value) %>%
    mutate(
      method = factor(
        method,
        levels = c("aipw_CW", "aipw_ER", "aipw_ER_CW"),
        labels = c("CW AIPW", "ER AIPW", "CW & ER AIPW")
      ),
      `$sqrt(n) times$ Bias` = sqrt(n) * bias_additive,
      `$n times$ Variance` = n * var_additive, 
      `$n times$ MSE` = n * mse_additive, 
      `$sqrt(n) times$ Bias ` = sqrt(n) * bias_mult,
      `$n times$ Variance `  = n * var_mult,
      `$n times$ MSE ` = n * mse_mult
    ) %>%
    arrange(method) %>%
    select(
      Method = method,
      n,
      `$sqrt(n) times$ Bias`,
      `$n times$ Variance`,
      `$n times$ MSE`,
      `Coverage` = coverage_additive,
      `$sqrt(n) times$ Bias `,
      `$n times$ Variance `,
      `$n times$ MSE `,
      `Coverage ` = coverage_mult
    )
}


make_latex_table <- function(summary_list, caption, label) {
  
  section_tables <- lapply(summary_list, format_section)
  
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

make_latex_table(
  summary_list,
  caption = "Bias, Variance, and Coverage",
  label = "tab:bias_var_cov"
)

# ------------------------
# New format
# ---------------------------------------------------


# ---------------------------------------------------
# Make latex table for simulation 1 results
# (Scaled + Overleaf layout version)
# ---------------------------------------------------

here::i_am("make_sim_1_table.R")

library(dplyr)

# read results 
default_summary       <- readRDS(here::here("results/sim_1/default_summary.Rds"))
violate_er_summary    <- readRDS(here::here("results/sim_1/violate_er_summary.Rds"))
violate_cw_summary    <- readRDS(here::here("results/sim_1/violate_cw_summary.Rds"))
violate_cw_er_summary <- readRDS(here::here("results/sim_1/violate_cw_er_summary.Rds"))

summary_list <- list(
  "PI and ER satisfied"       = default_summary,
  "PI satisfied, ER violated" = violate_er_summary,
  "PI violated, ER satisfied" = violate_cw_summary,
  "ER and PI violated"        = violate_cw_er_summary
)

# ---------------------------------------------------
# Method label mapping (LaTeX math)
# ---------------------------------------------------

method_labels <- c(
  "aipw_CW"    = "$\\psi_{1,\\text{PI},n}^+ - \\psi_{0,n}^+$",
  "aipw_ER"    = "$\\psi_{1,\\text{ER},n}^+ - \\psi_{0,n}^+$",
  "aipw_ER_CW" = "$\\psi_{1,\\cdot,n}^+ - \\psi_{0,n}^+$"
)

fmt <- function(x) sprintf("%.3f", x)

# ---------------------------------------------------
# Build LaTeX table manually (scaled version)
# ---------------------------------------------------

build_table <- function(summary_list) {
  
  cat("\\begin{table}[!h]\n")
  cat("\\centering\n")
  cat("\\caption{\\label{tab:tab:bias_var_cov}Scaled Bias, Variance, and MSE with Coverage}\n")
  cat("\\centering\n")
  cat("\\begin{tabular}[t]{lcccccclcc}\n")
  cat("\\toprule\n")
  cat("\\multicolumn{2}{c}{ } & \\multicolumn{4}{c}{Additive Scale} & \\multicolumn{4}{c}{Multiplicative Scale} \\\\\n")
  cat("\\cmidrule(l{3pt}r{3pt}){3-6} \\cmidrule(l{3pt}r{3pt}){7-10}\n")
  cat("Method & $n$ & $\\sqrt{n}$ Bias & $n$ Var. & $n$ MSE & Cov. & $\\sqrt{n}$ Bias & $n$ Var. & $n$ MSE  & Cov. \\\\\n")
  cat("\\midrule\n")
  
  for (scenario_name in names(summary_list)) {
    
    df <- summary_list[[scenario_name]]
    
    cat("\\addlinespace[0.3em]\n")
    cat(paste0("\\multicolumn{10}{l}{\\textbf{", scenario_name, "}}\\\\\n"))
    
    for (method in names(method_labels)) {
      
      sub <- df %>%
        filter(method == !!method,
               n %in% c(500, 4000)) %>%
        arrange(n) %>%
        mutate(
          sqrt_bias_add  = sqrt(n) * bias_additive,
          n_var_add      = n * var_additive,
          n_mse_add      = n * mse_additive,
          sqrt_bias_mult = sqrt(n) * bias_mult,
          n_var_mult     = n * var_mult,
          n_mse_mult     = n * mse_mult
        )
      
      row1 <- sub[sub$n == 500, ]
      row2 <- sub[sub$n == 4000, ]
      
      # n = 500
      cat(paste0(
        "\\multirow{2}{*}{", method_labels[[method]], "} & ",
        "500 & ",
        fmt(row1$sqrt_bias_add), " & ",
        fmt(row1$n_var_add), " & ",
        fmt(row1$n_mse_add), " & ",
        fmt(row1$coverage_additive), " & ",
        fmt(row1$sqrt_bias_mult), " & ",
        fmt(row1$n_var_mult), " & ",
        fmt(row1$n_mse_mult), " & ",
        fmt(row1$coverage_mult), "\\\\\n"
      ))
      
      # n = 4000
      cat(paste0(
        " & 4000 & ",
        fmt(row2$sqrt_bias_add), " & ",
        fmt(row2$n_var_add), " & ",
        fmt(row2$n_mse_add), " & ",
        fmt(row2$coverage_additive), " & ",
        fmt(row2$sqrt_bias_mult), " & ",
        fmt(row2$n_var_mult), " & ",
        fmt(row2$n_mse_mult), " & ",
        fmt(row2$coverage_mult), "\\\\\n"
      ))
      
      cat(" \\midrule\n")
    }
  }
  
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

# ---------------------------------------------------
# Run
# ---------------------------------------------------

build_table(summary_list)
