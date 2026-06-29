# ------------------------------------------------------------
# Make cross fitting table for the supplement
# ------------------------------------------------------------

here::i_am("R/make_table_cross_fit.R")

library(dplyr)

# ------------------------------------------------------------
# Read results
# ------------------------------------------------------------

summary_df <- readRDS(here::here("results/cross_fit/default_summary.Rds"))

# ------------------------------------------------------------
# Method label mapping
# ------------------------------------------------------------

method_labels <- c(
  "aipw_CW"    = "$\\psi_{1,\\text{PI},n}^+ - \\psi_{0,n}^+$",
  "aipw_ER"    = "$\\psi_{1,\\text{ER},n}^+ - \\psi_{0,n}^+$",
  "aipw_ER_CW" = "$\\psi_{1,\\cdot,n}^+ - \\psi_{0,n}^+$"
)

fmt <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("--")
  }
  sprintf("%.3f", x)
}

# ------------------------------------------------------------
# Build table
# ------------------------------------------------------------

build_table <- function(
    summary_df,
    sample_sizes = c(500, 4000),
    methods = names(method_labels)
) {
  
  df <- summary_df %>%
    filter(
      estimand == "nat_inf",
      method %in% methods,
      n %in% sample_sizes
    ) %>%
    mutate(
      cross_fit_lab = ifelse(cross_fit, "Yes", "No"),
      cross_fit_lab = factor(cross_fit_lab, levels = c("Yes", "No")),
      
      sqrt_bias_add  = sqrt(n) * bias_additive,
      n_var_add      = n * var_additive,
      n_mse_add      = n * mse_additive,
      
      sqrt_bias_mult = sqrt(n) * bias_mult,
      n_var_mult     = n * var_mult,
      n_mse_mult     = n * mse_mult
    )
  
  cat("\\begin{table}[!h]\n")
  cat("\\centering\n")
  cat("\\caption{\\label{tab:cross_fit_pi_er_satisfied}Scaled bias, variance, and MSE with coverage for estimators with and without cross-fitting when partial principal ignorability and exclusion restrictions are satisfied.}\n")
  cat("\\centering\n")
  cat("\\begin{tabular}[t]{llcccccclcc}\n")
  cat("\\toprule\n")
  cat("\\multicolumn{3}{c}{ } & \\multicolumn{4}{c}{Additive Scale} & \\multicolumn{4}{c}{Multiplicative Scale} \\\\\n")
  cat("\\cmidrule(l{3pt}r{3pt}){4-7} \\cmidrule(l{3pt}r{3pt}){8-11}\n")
  cat("Method & Cross-fit & $n$ & $\\sqrt{n}$ Bias & $n$ Var. & $n$ MSE & Cov. & $\\sqrt{n}$ Bias & $n$ Var. & $n$ MSE  & Cov. \\\\\n")
  cat("\\midrule\n")
  
  for (sample_size in sample_sizes) {
    
    cat("\\addlinespace[0.3em]\n")
    cat(paste0("\\multicolumn{11}{l}{\\textbf{$n = ", sample_size, "$}}\\\\\n"))
    
    for (method_name in methods) {
      
      sub <- df %>%
        filter(
          method == method_name,
          n == sample_size
        ) %>%
        arrange(cross_fit_lab)
      
      if (nrow(sub) == 0) {
        next
      }
      
      for (j in seq_len(nrow(sub))) {
        
        row <- sub[j, ]
        
        method_entry <- if (j == 1) {
          paste0("\\multirow{", nrow(sub), "}{*}{", method_labels[[method_name]], "}")
        } else {
          ""
        }
        
        cat(paste0(
          method_entry, " & ",
          row$cross_fit_lab, " & ",
          row$n, " & ",
          fmt(row$sqrt_bias_add), " & ",
          fmt(row$n_var_add), " & ",
          fmt(row$n_mse_add), " & ",
          fmt(row$coverage_additive), " & ",
          fmt(row$sqrt_bias_mult), " & ",
          fmt(row$n_var_mult), " & ",
          fmt(row$n_mse_mult), " & ",
          fmt(row$coverage_mult), "\\\\\n"
        ))
      }
      
      cat("\\midrule\n")
    }
  }
  
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

# ------------------------------------------------------------
# Run
# ------------------------------------------------------------

build_table(summary_df)
