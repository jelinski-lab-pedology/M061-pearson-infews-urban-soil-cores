# Note: This script creates the site type and land use figures. They require original site data (coordinates) that were intentionally excluded from the public repo. The rendered maps are in 02-output/a-figures/ as final outputs.


# Site type colors
SITE_TYPE_COLORS <- c(
  "Community Garden/Urban Farm" = "#F9E871",
  "Park/Green Space" = "#7da15b",
  "Residential" = "#FFB7CE"
)

# Map configuration
MAP_CONFIG <- list(
  center_lat = 44.953642,
  center_lon = -93.156002,
  padding_ew = 0.2,
  padding_ns = 0.1,
  zoom = 12,
  maptype = "alidade_smooth" # requires Stadia Maps API key; enter in Console with register_stadiamaps("your-key")
)

# Prepare unique site data for mapping ------------------------------------

infews_sites_unique <- infews_site_raw %>%
  group_by(address) %>%
  slice(1) %>%
  ungroup()

# Site type map -----------------------------------------------------------

# Filter out NA values
infews_sites_clean <- infews_sites_unique %>% filter(!is.na(site_type))

# Get basemap
basemap <- get_stadiamap(
  bbox = c(left = MAP_CONFIG$center_lon - MAP_CONFIG$padding_ew,
           bottom = MAP_CONFIG$center_lat - MAP_CONFIG$padding_ns,
           right = MAP_CONFIG$center_lon + MAP_CONFIG$padding_ew,
           top = MAP_CONFIG$center_lat + MAP_CONFIG$padding_ns),
  zoom = MAP_CONFIG$zoom,
  maptype = MAP_CONFIG$maptype
)

# Create map
site_type_map <- ggmap(basemap) +
  geom_point(
    data = infews_sites_clean,
    aes(x = longitude, y = latitude, fill = site_type),
    size = 3,
    alpha = 0.8,
    shape = 21,
    stroke = 0.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = SITE_TYPE_COLORS,
    name = "Site Type"
  ) +
  labs() +
  theme_void() +
  theme(
    legend.position = c(0.02, 0.02),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 4)))

# Display and save the map
print(site_type_map)
ggsave("02-output/a-figures/site-type-map07AUG2025.png", site_type_map,
       width = 12, height = 8, dpi = 300)


# Land use map ------------------------------------------------------------

# Filter out NA values
infews_site_clean <- infews_site_raw %>% filter(!is.na(land_use))

# Get basemap
basemap <- get_stadiamap(
  bbox = c(left = MAP_CONFIG$center_lon - MAP_CONFIG$padding_ew,
           bottom = MAP_CONFIG$center_lat - MAP_CONFIG$padding_ns,
           right = MAP_CONFIG$center_lon + MAP_CONFIG$padding_ew,
           top = MAP_CONFIG$center_lat + MAP_CONFIG$padding_ns),
  zoom = MAP_CONFIG$zoom,
  maptype = MAP_CONFIG$maptype
)

# Create map
# NOTE: depends on LAND_USE_COLORS, defined in 01-code/a-Rscripts/05-output-figures.R
land_use_map <- ggmap(basemap) +
  geom_point(
    data = infews_site_clean,
    aes(x = longitude, y = latitude, fill = land_use),
    size = 3,
    alpha = 0.8,
    shape = 21,
    stroke = 0.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = LAND_USE_COLORS,
    name = "Land Use",
    labels = GetLandUseLabels()
  ) +
  labs() +
  theme_void() +
  theme(
    legend.position = c(0.02, 0.02),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 4)))

# Display and save the map
print(land_use_map)
ggsave("02-output/a-figures/land-use-map07AUG2025.png", land_use_map,
       width = 12, height = 8, dpi = 300)
