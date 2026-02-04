# ----------------------------------------------------------------------------
# Make contour plots for effects 
# ----------------------------------------------------------------------------

# todo
# keep ratio protected to doomed same
# another row with more immune
# third row with better VE (this VE is ~50%) -- stick w larger immune and higher VE

here::i_am("plot_contour.R")

library(plotly)
library(RColorBrewer)
cfg <- yaml::read_yaml("config_contour.yml")

# New 2:
# setting_names <- c("provide_immune_60_ve_50__2",
#                    "provide_immune_60_ve_66__2",
#                    "provide_immune_60_ve_85__2")
# 
# setting_annotations <- c("Protected: 20%\nDoomed: 20%\nImmune: 60%\nVE: 50%",
#                          "Protected: 27%\nDoomed: 13%\nImmune: 60%\nVE: 66%",
#                          "Protected: 34%\nDoomed: 6%\nImmune: 60%\nVE: 85%")

# NEW 1:
setting_names <- c("provide_immune_40_ve_66",
                   "provide_immune_60_ve_66",
                   "provide_immune_80_ve_66")

setting_annotations <- c("Protected: 40%\nDoomed: 20%\nImmune: 40%\nVE: 66%",
                         "Protected: 27%\nDoomed: 13%\nImmune: 60%\nVE: 66%",
                         "Protected: 13%\nDoomed: 7%\nImmune: 80%\nVE: 66%")

# setting_names <- c("provide_immune_30_ve_66__2",
#                    "provide_immune_40_ve_66__2",
#                    "provide_immune_50_ve_66__2",
#                    "provide_immune_60_ve_66__2",
#                    "provide_immune_70_ve_66__2",
#                    "provide_immune_80_ve_66__2")
# 
# setting_annotations <- c("Protected: 47%\nDoomed: 23%\nImmune: 30%\nVE: 66%",
#                          "Protected: 40%\nDoomed: 20%\nImmune: 40%\nVE: 66%",
#                          "Protected: 33%\nDoomed: 17%\nImmune: 50%\nVE: 66%",
#                          "Protected: 27%\nDoomed: 13%\nImmune: 60%\nVE: 66%",
#                          "Protected: 20%\nDoomed: 10%\nImmune: 70%\nVE: 66%",
#                          "Protected: 13%\nDoomed: 7%\nImmune: 80%\nVE: 66%")

# Original % Immune plot
# setting_names <- c("provide_immune_30_ve_66",
#                    "provide_immune_40_ve_66",
#                    "provide_immune_50_ve_66",
#                    "provide_immune_60_ve_66",
#                    "provide_immune_70_ve_66",
#                    "provide_immune_80_ve_66")
# 
# setting_annotations <- c("Protected: 47%\nDoomed: 23%\nImmune: 30%\nVE: 66%",
#                          "Protected: 40%\nDoomed: 20%\nImmune: 40%\nVE: 66%",
#                          "Protected: 33%\nDoomed: 17%\nImmune: 50%\nVE: 66%",
#                          "Protected: 27%\nDoomed: 13%\nImmune: 60%\nVE: 66%",
#                          "Protected: 20%\nDoomed: 10%\nImmune: 70%\nVE: 66%",
#                          "Protected: 13%\nDoomed: 7%\nImmune: 80%\nVE: 66%")


# Original VE plot
# setting_names <- c("provide_immune_70_ve_85",
#                    "provide_immune_70_ve_66",
#                    "provide_immune_70_ve_50")
# 
# setting_annotations <- c("Protected: 25%\nDoomed: 5%\nImmune: 70%\nVE: 85%",
#                         "Protected: 20%\nDoomed: 10%\nImmune: 70%\nVE: 66%",
#                         "Protected: 15%\nDoomed: 15%\nImmune: 70%\nVE: 50%")

# setting_names <- c("provide_immune_60_ve_50__2",
#                    "provide_immune_60_ve_66__2",
#                    "provide_immune_60_ve_85__2")
# 
# setting_annotations <- c("Protected: 20%\nDoomed: 20%\nImmune: 60%\nVE: 50%",
#                          "Protected: 27%\nDoomed: 13%\nImmune: 60%\nVE: 66%",
#                          "Protected: 34%\nDoomed: 6%\nImmune: 60%\nVE: 85%")

all_rows <- list()

# To compute global min/max for shared color scale
all_truth <- lapply(setting_names, function(setting) {
  readRDS(here::here(paste0("results/contour/", setting, "_truth.Rds"))) #%>%
    #filter(doomed_inflation < -0.1)
})
global_range <- range(unlist(lapply(all_truth, function(truth) {
  c(truth$effect_nat_inf, truth$effect_doomed, truth$effect_pop)
})))
levels_global <- pretty(global_range, n = 15)
contour_size <- diff(levels_global)[1]
zmin <- min(levels_global)
zmax <- max(levels_global)

my_colorscale <- "YlGnBu"

make_contour <- function(x, y, z_matrix, trace_name) {
  plot_ly(
    x = rev(x), y = rev(y), z = z_matrix,
    type = "contour",
    zmin = zmin, zmax = zmax,
    coloraxis = "coloraxis",  # <- use shared coloraxis
    contours = list(
      start = levels_global[1],
      end = levels_global[length(levels_global)],
      size = contour_size,
      showlabels = TRUE,
      labelfont = list(size = 14)
    ),
    line = list(width = 2, color = 'black'),
    name = trace_name,
    showscale = FALSE  # turn off per-trace colorbars
  ) %>%
    layout(
      xaxis = list(title = "Protected effect", autorange = "reversed"),
      yaxis = list(title = "Doomed effect", autorange = "reversed")
    )
}

show_legend <- TRUE

# Loop over settings
for (row_idx in seq_along(setting_names)) {
  setting <- setting_names[row_idx]
  
  truth <- all_truth[[row_idx]]
  x <- sort(unique(truth$effect_protected))
  y <- sort(unique(truth$effect_doomed))
  
  #z_nat_inf <- matrix(truth$effect_nat_inf, nrow = length(x), ncol = length(y))
  #z_doomed  <- matrix(truth$effect_doomed, nrow = length(x), ncol = length(y))
  #z_pop     <- matrix(truth$effect_pop, nrow = length(x), ncol = length(y))
  
  z_nat_inf <- matrix(truth$effect_nat_inf, nrow = length(y), ncol = length(x))
  z_doomed  <- matrix(truth$effect_doomed, nrow = length(y), ncol = length(x))
  z_pop     <- matrix(truth$effect_pop, nrow = length(y), ncol = length(x))
  
  # Load simulation results
  sim_res <- readRDS(here::here(paste0("results/contour/", setting, "_combined_contour_data.Rds"))) #%>%
    #filter(doomed_inflation < -0.1)
  combos <- expand.grid(doomed_inflation = unique(sim_res$doomed_inflation),
                        protected_inflation = unique(sim_res$protected_inflation))
  
  # make sure order did not get swapped/still using right ones
  combos$effect_protected <- truth$effect_protected[match(
    paste(truth$doomed_inflation, truth$protected_inflation),
    paste(combos$doomed_inflation, combos$protected_inflation)
  )]
  combos$effect_doomed <- truth$effect_doomed[match(
    paste(truth$doomed_inflation, truth$protected_inflation),
    paste(combos$doomed_inflation, combos$protected_inflation)
  )]

  # Compute power
  # combos$doomed_power <- NA
  combos$pop_power <- NA
  combos$nat_inf_er_power <- NA
  combos$nat_inf_cw_power <- NA
  combos$nat_inf_er_cw_power <- NA
  
  for (i in seq_len(nrow(combos))) {
    sub <- sim_res[sim_res$doomed_inflation == combos$doomed_inflation[i] &
                     sim_res$protected_inflation == combos$protected_inflation[i], ]
    # combos$doomed_power[i] <- mean(as.numeric(sub$doomed_reject), na.rm = TRUE) # NOTE some NAs for doomed reject, a handful of seeds where there are some NAs within the object for different combos of deltas, not sure why
    combos$pop_power[i]    <- mean(as.numeric(sub$pop_reject))
    combos$nat_inf_er_power[i]<- mean(as.numeric(sub$nat_inf_er_reject))
    combos$nat_inf_cw_power[i]<- mean(as.numeric(sub$nat_inf_cw_reject))
    combos$nat_inf_er_cw_power[i]<- mean(as.numeric(sub$nat_inf_er_cw_reject))
  }
  
  # tbh this was chatgpt, idk what it's doing, making polygon to overlay for power
  get_hull <- function(df, xcol, ycol) {
    if (nrow(df) < 3) return(df[0, ])
    hull_idx <- chull(df[[xcol]], df[[ycol]])
    hull_idx <- c(hull_idx, hull_idx[1])
    df[hull_idx, ]
  }
  
  # gray out where doomed effect > protected effect
  mask_points <- expand.grid(x = x, y = y)
  mask_points <- subset(mask_points, y < x)
  
  # Convex hull to get the polygon boundary
  if (nrow(mask_points) > 2) {
    hull_idx <- chull(mask_points$x, mask_points$y)
    shade_poly <- mask_points[hull_idx, ]
  } else {
    shade_poly <- data.frame(x = numeric(0), y = numeric(0))
  }
  
  p_thresh <- 0.8
  # hull_doomed   <- get_hull(subset(combos, doomed_power >= p_thresh), "effect_protected", "effect_doomed")
  hull_pop      <- get_hull(subset(combos, pop_power >= p_thresh), "effect_protected", "effect_doomed")
  hull_nat_inf_er  <- get_hull(subset(combos, nat_inf_er_power >= p_thresh), "effect_protected", "effect_doomed")
  hull_nat_inf_cw  <- get_hull(subset(combos, nat_inf_cw_power >= p_thresh), "effect_protected", "effect_doomed")
  hull_nat_inf_er_cw  <- get_hull(subset(combos, nat_inf_er_cw_power >= p_thresh), "effect_protected", "effect_doomed")
  
  # fig1 <- make_contour(x, y, z_doomed, "Doomed") %>%
  #   add_polygons(
  #     data = shade_poly, x = ~x, y = ~y,
  #     fillcolor = "rgba(128,128,128,0.4)",  # semi-transparent gray
  #     line = list(width = 0),
  #     inherit = FALSE,
  #     showlegend = FALSE
  #   ) %>%
  #   add_trace(data = hull_doomed, x = ~effect_protected, y = ~effect_doomed,
  #             type = "scatter", mode = "lines", 
  #             line = list(color = "#ED0000FF", width = 5),
  #             name = "Doomed power ≥80% (n=700)", 
  #             legendgroup = "power", 
  #             showlegend = show_legend)
  
  fig2 <- make_contour(x, y, z_pop, "Population") %>%
    add_polygons(
      data = shade_poly, x = ~x, y = ~y,
      fillcolor = "rgba(128,128,128,0.4)",
      line = list(width = 0),
      inherit = FALSE,
      showlegend = FALSE
    ) %>%
    add_trace(data = hull_pop, x = ~effect_protected, y = ~effect_doomed,
              type = "scatter", mode = "lines", 
              line = list(color = "#ED0000FF", width = 5),
              name = "Population power ≥80% (n=700)", 
              legendgroup = "power", 
              showlegend = show_legend)
  
  fig3 <- make_contour(x, y, z_nat_inf, "Naturally infected") %>%
    add_polygons(
      data = shade_poly, x = ~x, y = ~y,
      fillcolor = "rgba(128,128,128,0.4)",
      line = list(width = 0),
      inherit = FALSE,
      showlegend = FALSE
    ) %>%
    add_trace(data = hull_nat_inf_er, x = ~effect_protected, y = ~effect_doomed,
              type = "scatter", mode = "lines", 
              line = list(color = "#00468BFF", width = 5),
              name = "Naturally infected ER power ≥80% (n=700)", 
              legendgroup = "power", 
              showlegend = show_legend)
  
  fig4 <- make_contour(x, y, z_nat_inf, "Naturally infected") %>%
    add_polygons(
      data = shade_poly, x = ~x, y = ~y,
      fillcolor = "rgba(128,128,128,0.4)",
      line = list(width = 0),
      inherit = FALSE,
      showlegend = FALSE
    ) %>%
    add_trace(data = hull_nat_inf_cw, x = ~effect_protected, y = ~effect_doomed,
              type = "scatter", mode = "lines", 
              line = list(color = "#925E9FFF", width = 5),
              name = "Naturally infected CW power ≥80% (n=700)", 
              legendgroup = "power", 
              showlegend = show_legend)
  
  fig5 <- make_contour(x, y, z_nat_inf, "Naturally infected") %>%
    add_polygons(
      data = shade_poly, x = ~x, y = ~y,
      fillcolor = "rgba(128,128,128,0.4)",
      line = list(width = 0),
      inherit = FALSE,
      showlegend = FALSE
    ) %>%
    add_trace(data = hull_nat_inf_er_cw, x = ~effect_protected, y = ~effect_doomed,
              type = "scatter", mode = "lines", 
              line = list(color = "#42B540FF", width = 5),
              name = "Naturally infected ER + CW power ≥80% (n=700)", 
              legendgroup = "power", 
              showlegend = show_legend)
  
  # row_fig <- subplot(fig1, fig2, fig3, fig4, fig5, nrows = 1,
  #                    shareX = TRUE, shareY = TRUE,
  #                    titleX = TRUE, titleY = TRUE) %>%
  #   layout(
  #     margin = list(l = 250)  # increase left margin to give space for annotations
  #   )
  # 
  # # store with row label
  # all_rows[[row_idx]] <- row_fig %>%
  #   layout(annotations = list(
  #     list(
  #       text = setting_annotations[row_idx],  # use the full annotation text
  #       x = -0.15, y = 0.5,                   # position to the left of the row
  #       xref = "paper", yref = "paper",
  #       textangle = 0,
  #       font = list(size = 14),
  #       showarrow = FALSE,
  #       xanchor = "center", yanchor = "middle"
  #     )
  #   ))
  
  # all_rows[[length(all_rows) + 1]] <- fig1
  all_rows[[length(all_rows) + 1]] <- fig2
  all_rows[[length(all_rows) + 1]] <- fig3
  all_rows[[length(all_rows) + 1]] <- fig4
  all_rows[[length(all_rows) + 1]] <- fig5
  
  
  show_legend <- FALSE
}


# ------------------------------------------------------------------------------
# Assemble final 3 x 4 plot (NO nested subplots)
# ------------------------------------------------------------------------------

n_rows <- length(setting_names)
n_cols <- 4


final_fig <- do.call(
  subplot,
  c(
    all_rows,
    list(
      nrows = n_rows,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    )
  )
)

# ------------------------------------------------------------------------------
# Row annotations (left side, centered per row)
# ------------------------------------------------------------------------------

row_ys <- seq(
  from = 1 - 1 / (2 * n_rows),
  to   = 1 / (2 * n_rows),
  length.out = n_rows
)

row_annotations <- lapply(seq_len(n_rows), function(i) {
  list(
    text = setting_annotations[i],
    x = -0.08,
    y = row_ys[i],
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    xanchor = "right",
    yanchor = "middle",
    align = "right",
    font = list(size = 14)
  )
})

# ------------------------------------------------------------------------------
# Column titles (top, evenly spaced)
# ------------------------------------------------------------------------------

col_titles <- c(
  # "Doomed",
  "Population",
  "Naturally infected (ER)",
  "Naturally infected (CW)",
  "Naturally infected (ER + CW)"
)

col_xs <- seq(
  from = 1 / (2 * n_cols),
  to   = 1 - 1 / (2 * n_cols),
  length.out = n_cols
)

col_annotations <- lapply(seq_len(n_cols), function(i) {
  list(
    text = col_titles[i],
    x = col_xs[i],
    y = 1.04,
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    xanchor = "center",
    yanchor = "bottom",
    font = list(size = 16)
  )
})

# ------------------------------------------------------------------------------
# Final layout (colorbar, legend, annotations)
# ------------------------------------------------------------------------------

final_fig <- final_fig %>%
  layout(
    coloraxis = list(
      colorscale = my_colorscale,
      cmin = zmin,
      cmax = zmax,
      colorbar = list(
        title = list(text = "Effect size", side = "top"),
        tickfont = list(size = 14)
      )
    ),
    legend = list(
      orientation = "h",
      x = 1.02,
      y = 0,
      xanchor = "left",
      yanchor = "top",
      font = list(size = 14),
      bgcolor = "rgba(255,255,255,0)"
    ),
    annotations = c(row_annotations, col_annotations),
    margin = list(l = 260, t = 120)
  )

final_fig


# -------

final_fig <- do.call(
  subplot,
  c(
    all_rows,
    list(
      nrows = length(setting_names),
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    )
  )
) %>%
  layout(
    coloraxis = list(
      colorscale = my_colorscale,
      cmin = zmin,
      cmax = zmax,
      colorbar = list(
        title = list(text = "Effect size", side = "top"),
        tickfont = list(size = 14)
      )
    ),
    legend = list(
      orientation = "h",
      x = 1.025,
      y = 0,
      
      xanchor = "left",
      yanchor = "top",
      font = list(size = 14),
      bgcolor = "rgba(255,255,255,0)"
    ),
    annotations = list(
      # list(
      #   text = "Doomed",
      #   x = 0.1, y = 1.025, xref = "paper", yref = "paper",
      #   showarrow = FALSE,
      #   font = list(size = 16, family = "Arial", color = "black")
      # ),
      list(
        text = "Population",
        x = 0.1, y = 1.025, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, family = "Arial", color = "black")
      ),
      list(
        text = "Naturally infected (ER)",
        x = .35, y = 1.025, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, family = "Arial", color = "black")
      ),
      list(
        text = "Naturally infected (CW)",
        x = .7, y = 1.025, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, family = "Arial", color = "black")
      ),
      list(
        text = "Naturally infected (ER + CW)",
        x = 1, y = 1.025, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, family = "Arial", color = "black")
      )
    )
  )


final_fig

#plotly::export(final_fig, file = here::here("results/contour/figures/contour_VE.png"))

save_image(final_fig, here::here("results/contour/figures/contour_VE_new.png"), width = 1200, height = 1000)

# orca(final_fig, file = here::here("results/contour/figures/contour_VE.png"), 
#      format = tools::file_ext(file),
#      scale = NULL, width = 1200, height = 1000, mathjax = FALSE,
#      parallel_limit = NULL, verbose = FALSE, debug = FALSE,
#      safe = FALSE, more_args = NULL)

#plotly::plotly_IMAGE(final_fig, width = 1200, height = 1000, format = "png", here::here("results/contour/figures/contour_VE.png"))
