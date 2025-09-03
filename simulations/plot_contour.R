# ----------------------------------------------------------------------------
# Make contour plots for effects 
# ----------------------------------------------------------------------------

here::i_am("plot_contour.R")

source(here::here("get_truth.R"))

cfg <- yaml::read_yaml("config_contour.yml")
config <- cfg[["contour_plot"]]

results <- readRDS(here::here("results/contour/contour_plot_truth.Rds"))

str(results)

# contour plot where x axis is nat_inf_inflation, y axis is doomed_inflation, z is effect_nat_inf

ggplot(results, aes(x = nat_inf_inflation,
                    y = doomed_inflation,
                    z = effect_nat_inf)) +
  #geom_contour_filled(bins = 5) +  # filled contour plot
  geom_contour(color = "black", alpha = 0.5) +  # contour lines
  labs(x = "Protected inflation",
       y = "Doomed inflation",
       fill = "Effect in naturally infected",
       title = "Contour plot of effect in naturally infected") +
  theme_minimal(base_size = 14)
