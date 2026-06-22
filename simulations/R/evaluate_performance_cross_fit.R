# ------------------------------------------------------------------------------
# Script to evaluate performance of generic simulations 
# ------------------------------------------------------------------------------

options(echo = TRUE)

# Path to installed packages on cluster
.libPaths(c(
  "/apps/R/4.4.0/lib64/R/site/library",
  "/apps/R/4.4.0/lib64/R/library",
  "~/Rlibs_ve_trial"
))

here::i_am("R/evaluate_performance_simulation_1.R")

source(here::here("R/simulate_data.R"))
source(here::here("R/get_truth.R"))

devtools::load_all("~/vaxstrat")

library(dplyr)
library(purrr)

# ------------------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------------------

project_dir <- "/projects/dbenkes/allison/vegrowth_analysis/results/cross_fit/"

cargs <- commandArgs(trailingOnly = TRUE)
setting <- cargs[[1]]

cfg <- yaml::read_yaml("config_cross_fit.yml")
config <- cfg[[setting]]

result_dir <- file.path(project_dir, setting)

# ------------------------------------------------------------------------------
# Truth
# ------------------------------------------------------------------------------

truth_file <- here::here(paste0("truth/cross_fit/", setting, "_truth.Rds"))

if (!file.exists(truth_file)) {
  truth <- get_truth_cross_fit(config)
  saveRDS(truth, truth_file)
} else {
  truth <- readRDS(truth_file)
}

# ------------------------------------------------------------------------------
# Result files
# ------------------------------------------------------------------------------

files_cf <- list.files(
  path = result_dir,
  pattern = paste0("^", setting, "_cross_fit_seed_.*\\.Rds$"),
  full.names = TRUE
)

files_og <- list.files(
  path = result_dir,
  pattern = paste0("^", setting, "_single_fit_seed_.*\\.Rds$"),
  full.names = TRUE
)

message("Found ", length(files_cf), " cross-fit files")
message("Found ", length(files_og), " single-fit/original files")

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

get_seed <- function(fname, setting, cross_fit) {
  if (cross_fit) {
    as.integer(gsub(paste0(setting, "_cross_fit_seed_|\\.Rds"), "", basename(fname)))
  } else {
    as.integer(gsub(paste0(setting, "_single_fit_seed_|\\.Rds"), "", basename(fname)))
  }
}

make_grid <- function(seed, config) {
  expand.grid(
    seed = seed,
    n_sample_size = as.numeric(config$n_sample_size),
    effect_protect = config$effect_protect,
    doomed_inflation = as.numeric(config$doomed_inflation),
    protected_epsilon = as.numeric(config$protected_epsilon),
    doomed_epsilon = as.numeric(config$doomed_epsilon),
    immune_epsilon = as.numeric(config$immune_epsilon)
  )
}

match_truth_row <- function(truth, grid_row) {
  truth %>%
    filter(
      doomed_inflation == grid_row$doomed_inflation,
      protected_epsilon == grid_row$protected_epsilon,
      doomed_epsilon == grid_row$doomed_epsilon,
      immune_epsilon == grid_row$immune_epsilon
    )
}

extract_method_result <- function(res, method) {
  res$nat_inf[[method]]$pt_est
}

make_result_df_one_grid <- function(res, grid_row, truth, cross_fit) {
  
  methods <- c("aipw_CW", "aipw_ER", "aipw_ER_CW")
  tr <- match_truth_row(truth, grid_row)
  
  if (nrow(tr) != 1) {
    stop(
      "Truth match did not return exactly one row for seed ",
      grid_row$seed,
      ", n = ",
      grid_row$n_sample_size
    )
  }
  
  out <- lapply(methods, function(method) {
    
    pt <- extract_method_result(res, method)
    
    additive_estimate <- as.numeric(pt["additive_effect"])
    additive_se <- as.numeric(pt["additive_se"])
    
    log_mult_estimate <- as.numeric(pt["log_multiplicative_effect"])
    log_mult_se <- as.numeric(pt["log_multiplicative_se"])
    
    psi_1 <- as.numeric(pt["psi_1"])
    se_psi_1 <- as.numeric(pt["se_psi_1"])
    
    psi_0 <- as.numeric(pt["psi_0"])
    se_psi_0 <- as.numeric(pt["se_psi_0"])
    
    data.frame(
      seed = grid_row$seed,
      cross_fit = cross_fit,
      n = grid_row$n_sample_size,
      effect_protect = grid_row$effect_protect,
      doomed_inflation = grid_row$doomed_inflation,
      protected_epsilon = grid_row$protected_epsilon,
      doomed_epsilon = grid_row$doomed_epsilon,
      immune_epsilon = grid_row$immune_epsilon,
      
      estimand = "nat_inf",
      method = method,
      
      # Additive estimate and CI
      additive_estimate = additive_estimate,
      additive_lower_ci = additive_estimate - 1.96 * additive_se,
      additive_upper_ci = additive_estimate + 1.96 * additive_se,
      
      # Multiplicative estimate and CI
      mult_estimate = exp(log_mult_estimate),
      mult_lower_ci = exp(log_mult_estimate - 1.96 * log_mult_se),
      mult_upper_ci = exp(log_mult_estimate + 1.96 * log_mult_se),
      
      # psi_1
      psi_1 = psi_1,
      psi_1_lower_ci = psi_1 - 1.96 * se_psi_1,
      psi_1_upper_ci = psi_1 + 1.96 * se_psi_1,
      
      # psi_0
      psi_0 = psi_0,
      psi_0_lower_ci = psi_0 - 1.96 * se_psi_0,
      psi_0_upper_ci = psi_0 + 1.96 * se_psi_0,
      
      # Truth
      additive_truth = tr$effect_nat_inf,
      mult_truth = tr$effect_nat_inf_mult,
      psi_1_truth = tr$E_Y1__protected_or_doomed,
      psi_0_truth = tr$E_Y0__protected_or_doomed
    )
  })
  
  bind_rows(out) %>%
    mutate(
      additive_diff = additive_estimate - additive_truth,
      mult_diff = log(mult_estimate) - log(mult_truth),
      psi_1_diff = psi_1 - psi_1_truth,
      psi_0_diff = psi_0 - psi_0_truth,
      
      additive_coverage = as.integer(
        additive_truth >= additive_lower_ci &
          additive_truth <= additive_upper_ci
      ),
      mult_coverage = as.integer(
        mult_truth >= mult_lower_ci &
          mult_truth <= mult_upper_ci
      ),
      psi_1_coverage = as.integer(
        psi_1_truth >= psi_1_lower_ci &
          psi_1_truth <= psi_1_upper_ci
      ),
      psi_0_coverage = as.integer(
        psi_0_truth >= psi_0_lower_ci &
          psi_0_truth <= psi_0_upper_ci
      )
    )
}

evaluate_file <- function(fname, setting, config, truth, cross_fit) {
  
  res_obj <- readRDS(fname)
  seed <- get_seed(fname, setting, cross_fit)
  grid <- make_grid(seed, config)
  
  if (length(res_obj) != nrow(grid)) {
    stop(
      "Length mismatch in file: ", basename(fname),
      ". length(res_obj) = ", length(res_obj),
      ", nrow(grid) = ", nrow(grid)
    )
  }
  
  bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    make_result_df_one_grid(
      res = res_obj[[i]],
      grid_row = grid[i, ],
      truth = truth,
      cross_fit = cross_fit
    )
  }))
}

# ------------------------------------------------------------------------------
# Evaluate all results
# ------------------------------------------------------------------------------

all_result_df <- bind_rows(
  lapply(files_cf, evaluate_file,
         setting = setting,
         config = config,
         truth = truth,
         cross_fit = TRUE),
  
  lapply(files_og, evaluate_file,
         setting = setting,
         config = config,
         truth = truth,
         cross_fit = FALSE)
)

saveRDS(
  all_result_df,
  here::here(paste0("results/cross_fit/", setting, "_all.Rds"))
)

# ------------------------------------------------------------------------------
# Summarize bias, variance, MSE, coverage
# ------------------------------------------------------------------------------

summary_df <- all_result_df %>%
  group_by(cross_fit, estimand, method, n) %>%
  summarise(
    confirm_complete = n(),
    
    bias_additive = mean(additive_diff, na.rm = TRUE),
    var_additive = var(additive_estimate, na.rm = TRUE),
    mse_additive = mean(additive_diff^2, na.rm = TRUE),
    coverage_additive = mean(additive_coverage, na.rm = TRUE),
    
    bias_mult = mean(mult_diff, na.rm = TRUE),
    var_mult = var(log(mult_estimate), na.rm = TRUE),
    mse_mult = mean(mult_diff^2, na.rm = TRUE),
    coverage_mult = mean(mult_coverage, na.rm = TRUE),
    
    bias_psi_1 = mean(psi_1_diff, na.rm = TRUE),
    var_psi_1 = var(psi_1, na.rm = TRUE),
    mse_psi_1 = mean(psi_1_diff^2, na.rm = TRUE),
    coverage_psi_1 = mean(psi_1_coverage, na.rm = TRUE),
    
    bias_psi_0 = mean(psi_0_diff, na.rm = TRUE),
    var_psi_0 = var(psi_0, na.rm = TRUE),
    mse_psi_0 = mean(psi_0_diff^2, na.rm = TRUE),
    coverage_psi_0 = mean(psi_0_coverage, na.rm = TRUE),
    
    .groups = "drop"
  )

saveRDS(
  summary_df,
  here::here(paste0("results/cross_fit/", setting, "_summary.Rds"))
)