# ------------------------------------------------------------------
# Script for monotoniciy sensitivity analysis
# ------------------------------------------------------------------

here::i_am("real_data_analysis/sensitivity_monotonicity.R")

library(SuperLearner)
library(plotly)
library(dplyr)

devtools::load_all("../shigella_projects/packages/vaxstrat/")

data <- readRDS("real_data_analysis/provide_data/per_protocol_data.Rds")

# get rid of one obs with missing wk10 haz
data <- data[!is.na(data$wk10_haz),]

covariate_list <- c("gender",
                    "wk10_haz",
                    "num_hh_sleep") 

# One-hot encode factors
one_hot_data <- fastDummies::dummy_cols(data,
                                        select_columns = colnames(data)[
                                          !(colnames(data) %in% c("sid", "rotaarm", "enr_date")) &
                                            (sapply(data, is.character) | sapply(data, is.factor))
                                        ],
                                        remove_first_dummy = FALSE,
                                        remove_selected_columns = TRUE,
                                        ignore_na = TRUE)

# Remove spaces from column names in the one-hot encoded data
colnames(one_hot_data) <- gsub(" ", "_", colnames(one_hot_data))

### Get new column names for covariates (covariate_list will have changed if any are factors)
covariate_data <- data[,covariate_list, drop = FALSE]
if(any(sapply(covariate_data, is.factor) == TRUE)){
  one_hot_covariate_data <- fastDummies::dummy_cols(covariate_data,
                                                    remove_first_dummy = FALSE,
                                                    remove_selected_columns = TRUE,
                                                    ignore_na = TRUE)
  colnames(one_hot_covariate_data) <- gsub(" ", "_", colnames(one_hot_covariate_data)) # remove spaces here as well
  one_hot_covariate_colnames <- colnames(one_hot_covariate_data)
} else {
  one_hot_covariate_colnames <- colnames(covariate_data)
}

result <- vaxstrat(
  data = one_hot_data, 
  Y_name = "any_abx_wk52",
  Z_name = "rotaarm",
  S_name = "rotaepi",
  X_name = one_hot_covariate_colnames, 
  estimand = "nat_inf", 
  method = "sens_mono",
  exclusion_restriction = TRUE,
  cross_world = TRUE,
  ml = TRUE,
  Y_Z_X_library = c("SL.glm", "SL.gam", "SL.earth", "SL.step.forward"),
  Y_X_library = c("SL.glm", "SL.gam", "SL.earth", "SL.step.forward"),
  S_X_library = c("SL.glm", "SL.gam", "SL.earth", "SL.step.forward"),
  S_Z_X_library = c("SL.glm", "SL.gam", "SL.earth", "SL.step.forward"),
  Y_X_S1_model = "any_abx_wk52 ~ 1",
  family = "binomial",
  seed = 54321,
  effect_dir = "negative",
  epsilon = exp(seq(log(0.55), log(2.2), by = -log(0.55) / 10))
)

# ------------------------------------------------------------
# Contour plot for monotonicity sensitivity analysis
# ------------------------------------------------------------

sens_mono <- result$nat_inf$sens_mono

sens_mono_df <- data.frame(
  p = sens_mono$p,
  epsilon = sens_mono$epsilon,
  additive_effect = sens_mono$additive_effect
)

# sort so matrix aligns correctly
sens_mono_df <- sens_mono_df %>%
  arrange(epsilon, p)

p_vals <- sort(unique(sens_mono_df$p))
epsilon_vals <- sort(unique(sens_mono_df$epsilon))

z_matrix <- matrix(
  sens_mono_df$additive_effect,
  nrow = length(epsilon_vals),
  ncol = length(p_vals),
  byrow = TRUE
)

# ------------------------------------------------------------
# Global color scale
# ------------------------------------------------------------

global_range <- range(sens_mono_df$additive_effect, na.rm = TRUE)
levels_global <- pretty(global_range, n = 15)
contour_size <- diff(levels_global)[1]

zmin <- min(levels_global)
zmax <- max(levels_global)

my_colorscale <- "YlGnBu"

# ------------------------------------------------------------
# Make contour
# ------------------------------------------------------------

fig <- plot_ly(
  x = p_vals,
  y = epsilon_vals,
  z = z_matrix,
  type = "contour",
  coloraxis = "coloraxis",
  zmin = zmin,
  zmax = zmax,
  contours = list(
    start = levels_global[1],
    end = levels_global[length(levels_global)],
    size = contour_size,
    showlabels = TRUE,
    labelfont = list(size = 11)
  ),
  line = list(color = "black", width = 1.5),
  showscale = FALSE
) %>%
  add_trace(
    x = c(0, 0),
    y = range(epsilon_vals),
    type = "scatter",
    mode = "lines",
    line = list(color = "black", dash = "dash"),
    inherit = FALSE,
    showlegend = FALSE
  ) %>%
  add_trace(
    x = range(p_vals),
    y = c(1, 1),
    type = "scatter",
    mode = "lines",
    line = list(color = "black", dash = "dash"),
    inherit = FALSE,
    showlegend = FALSE
  ) %>%
  layout(
    xaxis = list(title = "p"),
    yaxis = list(title = "\u03B5"),
    coloraxis = list(
      colorscale = my_colorscale,
      cmin = zmin,
      cmax = zmax,
      colorbar = list(
        title = list(text = "Additive effect", side = "top")
      )
    ),
    margin = list(t = 20, b = 75)
  )

fig

# Save without margin
htmlwidgets::saveWidget(fig, here::here("real_data_analysis/results/contour_monotonicity.html"), selfcontained = TRUE)

# Then take the screenshot with webshot padding instead
webshot2::webshot(
  here::here("real_data_analysis/results/contour_monotonicity.html"),
  here::here("real_data_analysis/results/contour_monotonicity.png"),
  vwidth = 900, vheight = 750,
  zoom = 3,
  delay = 2
)
