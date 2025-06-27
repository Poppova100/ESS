library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world_map <- ne_countries(scale = 50, returnclass = 'sf')

european_countries <- c(
  "Albania", "Andorra", "Austria", "Belgium", "Belarus", "Bosnia and Herzegovina",
  "Bulgaria", "Croatia", "Cyprus", "Czech Rep.", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Kosovo", "Latvia",
  "Lithuania", "Liechtenstein", "Luxembourg", "Malta", "Moldova", "Monaco",
  "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal",
  "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City"
)

europe_bbox <- st_bbox(c(xmin = -13, ymin = 35, xmax = 40, ymax = 70), crs = st_crs(world_map))
europe_map <- st_crop(world_map, europe_bbox)

df <- tibble(
  country = european_countries,
  some_value = runif(length(european_countries))
)

europe_map <- europe_map %>%
  left_join(df, by = c("name" = "country"))

ggplot(europe_map) +
  geom_sf() +
  theme(aspect.ratio = 9/13)

study_area <- ggplot(europe_map) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-13, 31), ylim = c(38, 65), expand = FALSE) +
  theme_bw(base_size = 15) +
  labs(x = NULL, y = NULL) +
  theme(aspect.ratio = 1) +
  annotate("rect", xmin = 2, xmax = 16, ymin = 48, ymax = 55,
           color = "red2", fill = NA, size = 1.2)

# ggsave("./figures/study_area.png", study_area, height = 10, width = 15, units = "cm", dpi = 600)
