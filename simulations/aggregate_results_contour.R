
options(echo = TRUE)

# Path to installed packages on cluster
.libPaths(c("/apps/R/4.4.0/lib64/R/site/library","/apps/R/4.4.0/lib64/R/library", "~/Rlibs_ve_trial"))

here::i_am("aggregate_results_contour.R")

source(here::here("simulate_data.R"))

library(vegrowth)
library(dplyr)

# Path to projects folder where results will be saved
project_dir <- "/projects/dbenkes/allison/vegrowth_analysis/results/contour"

# command args config setting
cargs <- commandArgs(trailingOnly = TRUE)
setting <- cargs[[1]]

cfg <- yaml::read_yaml("config_contour.yml")
config <- cfg[[setting]]

result_dir <- file.path(project_dir, setting)

# List all matching .Rds files
files <- list.files(
  path = result_dir,
  pattern = paste0("^", setting, "_overall_seed_.*\\.Rds$"),
  full.names = TRUE
)

all_result_df <- data.frame()

for(fname in files){
  res_obj <- readRDS(fname)
  seed <- as.integer(gsub(paste0(setting, "_overall_seed_|\\.Rds"), "", basename(fname)))
  
  grid <- expand.grid(seed = seed, 
                      n_sample_size = as.numeric(config$n_sample_size),
                      effect_protect = config$effect_protect,
                      doomed_inflation = as.numeric(config$doomed_inflation),
                      nat_inf_inflation = as.numeric(config$nat_inf_inflation),
                      nat_inf_epsilon = as.numeric(config$nat_inf_epsilon),
                      doomed_epsilon = as.numeric(config$doomed_epsilon))
  
  for(i in 1:nrow(grid)){
    
    res <- res_obj[[i]]
    
    result_df <- data.frame(
      seed = seed,
      n = grid$n_sample_size[i],
      effect_protect = grid$effect_protect[i],
      doomed_inflation = grid$doomed_inflation[i],
      nat_inf_inflation = grid$nat_inf_inflation[i],
      nat_inf_epsilon = grid$nat_inf_epsilon[i],
      dooomed_epsilon = grid$doomed_epsilon[i],
      estimand = c(rep("nat_inf",5),
                   rep("doomed",4),
                   rep("pop",3)),
      method = c(c("gcomp", "ipw", "aipw", "tmle", "lower bound"),
                 c("gcomp", "ipw", "aipw", "lower bound"),
                 c("gcomp", "ipw", "aipw")),
      # Additive estimate and confidence interval
      additive_estimate = c(
        c(res$nat_inf$gcomp$pt_est['additive_effect'],
          res$nat_inf$ipw$pt_est['additive_effect'],
          res$nat_inf$aipw$pt_est['additive_effect'],
          res$nat_inf$tmle$pt_est['additive_effect'],
          res$nat_inf$bound$pt_est['additive_effect_lower']),
        c(res$doomed$gcomp$pt_est['additive_effect'],
          res$doomed$ipw$pt_est['additive_effect'],
          res$doomed$aipw$pt_est['additive_effect'],
          res$doomed$bound$pt_est['additive_effect_lower']),
        c(res$pop$gcomp$pt_est['additive_effect'],
          res$pop$ipw$pt_est['additive_effect'],
          res$pop$aipw$pt_est['additive_effect'])
      ),
      additive_reject = c(
        c(as.numeric(res$nat_inf$gcomp$reject$additive),
          as.numeric(res$nat_inf$ipw$reject$additive),
          as.numeric(res$nat_inf$aipw$reject$additive),
          as.numeric(res$nat_inf$tmle$reject$additive),
          as.numeric(res$nat_inf$bound$reject$additive)),
        c(as.numeric(res$doomed$gcomp$reject$additive),
          as.numeric(res$doomed$ipw$reject$additive),
          as.numeric(res$doomed$aipw$reject$additive),
          as.numeric(res$doomed$bound$reject$additive)),
        c(as.numeric(res$pop$gcomp$reject$additive),
          as.numeric(res$pop$ipw$reject$additive),
          as.numeric(res$pop$aipw$reject$additive))
      ),
      # Multiplicative estimate and confidence interval
      mult_estimate = c(
        c(exp(as.numeric(res$nat_inf$gcomp$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$nat_inf$ipw$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$nat_inf$aipw$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$nat_inf$tmle$pt_est['log_multiplicative_effect'])),
          res$nat_inf$bound$pt_est['mult_effect_lower']),
        c(exp(as.numeric(res$doomed$gcomp$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$doomed$ipw$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$doomed$aipw$pt_est['log_multiplicative_effect'])),
          res$doomed$bound$pt_est['mult_effect_lower']),
        c(exp(as.numeric(res$pop$gcomp$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$pop$ipw$pt_est['log_multiplicative_effect'])),
          exp(as.numeric(res$pop$aipw$pt_est['log_multiplicative_effect'])))
      ),
      mult_reject = c(
        c(as.numeric(res$nat_inf$gcomp$reject$mult),
          as.numeric(res$nat_inf$ipw$reject$mult),
          as.numeric(res$nat_inf$aipw$reject$mult),
          as.numeric(res$nat_inf$tmle$reject$mult),
          as.numeric(res$nat_inf$bound$reject$mult)),
        c(as.numeric(res$doomed$gcomp$reject$mult),
          as.numeric(res$doomed$ipw$reject$mult),
          as.numeric(res$doomed$aipw$reject$mult),
          as.numeric(res$doomed$bound$reject$mult)),
        c(as.numeric(res$pop$gcomp$reject$mult),
          as.numeric(res$pop$ipw$reject$mult),
          as.numeric(res$pop$aipw$reject$mult))
      )
    )
    
    all_result_df <- rbind(all_result_df, result_df)
  }
  
}

saveRDS(all_result_df, paste0("results/contour/", setting, "_overall.Rds"))