# --- Load Required Libraries ---
required_packages <- c(
  'raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr',
  'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer', 'ggpubr', 'gridExtra', 'grid'
)
to_install <- required_packages[!(required_packages %in% installed.packages()[, 'Package'])]
if (length(to_install) != 0) install.packages(to_install)
invisible(lapply(required_packages, library, character.only = TRUE))
rm(to_install)

# --- Read Data ---
dat_before <- readRDS("./data/dat_before.rds")
dat_during <- readRDS("./data/dat_during.rds")
dat_after  <- readRDS("./data/dat_after.rds")

# --- Combine Data and Add Period Column ---
dat_before$Period <- "BEFORE"
dat_during$Period <- "DURING"
dat_after$Period  <- "AFTER"
dat_all <- bind_rows(dat_before, dat_during, dat_after)

# --- Ensure factor ordering ---
dat_all <- dat_all %>%
  mutate(
    Prob = factor(Prob, levels = c("HWO", "HWH", "HWL", "HWG")),
    Period = factor(Period, levels = c("BEFORE", "DURING", "AFTER")),
    CT = factor(CT)    # circulation type
  )

# --- Ensure all Prob x Period x CT combinations exist (fill missing with 0) ---
dat_all_complete <- dat_all %>%
  complete(Prob, Period, CT, fill = list(Value = 0)) %>%
  arrange(Prob, Period, CT)

# --- Plot: 4 rows (Prob) x 3 columns (Period a/b/c) ---
# Make text a bit bigger everywhere:
# - use theme_void(base_size = ...) to raise general base text size
# - increase strip (facet) text, legend text, and the geom_text label size
# quick-fix: keep pie size, bigger text, labels nudged outward, subtle separators (thin white lines)
final2 <- ggplot(dat_all_complete, aes(x = "", y = Value, fill = CT)) +
  geom_col() +   # thin white separators (subtle, not long black lines)
  geom_text(aes(label = ifelse(Value == 0, "", sprintf("%.1f", Value))),
            position = position_stack(vjust = 0.5),
            color = 'black', size = 6) + # small outward shift for readability
  coord_polar(theta = "y") +
  facet_grid(Prob ~ Period, switch = "y",
             labeller = labeller(Period = c(BEFORE = "a", DURING = "b", AFTER = "c"))) +
  scale_fill_manual(
    values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00"),
    labels = c("anticyclonic", "cyclonic", "directional", "indeterminate"),
    name = NULL
  ) +
  theme_void(base_size = 18) +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 22, face = "bold"),
    strip.text.y.left = element_text(size = 22, face = "bold", angle = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.key.size = unit(0.9, "cm"),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(5, 20, 5, 5)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
print(final2)

# Save (adjust size as needed). Use plot = final2 to ensure correct plot is saved.
# Uncomment and edit path if you want to save:
 ggsave(filename = paste0("./figures/", "multiple_pie2.png"),
        plot = final2,
        width = 30, height = 37, units = "cm", bg = 'white')
 