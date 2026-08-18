# Note: Prior to publication, this script included code for site type and land use figures, which required original site data (coordinates) that were intentionally excluded from the public repo. The rendered maps are in 02-output/a-figures/ as final outputs. The code for the figures is in 03-support-files/archive/x000-site-maps.R.

# Define color schemes and labels ------------------------------------

# Land use colors
LAND_USE_COLORS <- c(
  "Garden_non_food_producing" = "#439786",
  "Garden_food_producing_in_ground" = "#8B0000",
  "Maintained_lawn" = "#32CD32",
  "Natural_vegetation" =  "#8B4513",
  "Garden_food_producing_raised_beds" = "#FF6347"
)

# Land use abbreviated codes
LAND_USE_CODES <- c(
  "Garden_food_producing_in_ground" = "GFPG",
  "Garden_food_producing_raised_beds" = "GFPR", 
  "Garden_non_food_producing" = "GNFP",
  "Maintained_lawn" = "LAWN",
  "Natural_vegetation" = "NATV"
)

# Multi-panel stock figure ------------------------------------------------

# Prepare data for plotting
soc_clean <- soc_stock_site %>% filter(!is.na(land_use) & land_use != "NA")
phos_clean <- phosphorus_stock_site %>% filter(!is.na(land_use) & land_use != "NA")

# Create individual ggplots for each stock type
# Create summary data for significance labels
carbon_labels <- data.frame(
  land_use = factor(c("Garden_food_producing_in_ground", "Garden_food_producing_raised_beds", 
                      "Garden_non_food_producing", "Maintained_lawn", "Natural_vegetation")),
  label = c("A", "AB", "C", "C", "B"),
  y_pos = 87
)

carbon_plot <- ggplot(soc_clean, aes(x = land_use, y = stock)) +
  geom_boxplot() +
  geom_text(data = carbon_labels,
            aes(x = land_use, y = y_pos, label = label),
            size = 4,
            fontface = "bold") +
  labs(
    title = "Soil Organic Carbon",
    x = "Land Use",
    y = expression("1m SOC stock (kgC m"^-2*")")
  ) +
  scale_x_discrete(labels = LAND_USE_CODES[levels(factor(soc_clean$land_use))]) +
  ylim(0, 100) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

phosphorus_labels <- data.frame(
  land_use = factor(c("Garden_food_producing_in_ground", "Garden_food_producing_raised_beds", 
                      "Garden_non_food_producing", "Maintained_lawn", "Natural_vegetation")),
  label = c("A", "AB", "B", "B", "B"),
  y_pos = 1.45
)

phosphorus_plot <- ggplot(phos_clean, aes(x = land_use, y = stock)) +
  geom_boxplot() +
  geom_text(data = phosphorus_labels,
            aes(x = land_use, y = y_pos, label = label),
            size = 4,
            fontface = "bold") +
  labs(
    title = "Total Phosphorus",
    x = "Land Use",
    y = expression("1m P stock (kgP m"^-2*")")
  ) +
  scale_x_discrete(labels = LAND_USE_CODES[levels(factor(phos_clean$land_use))]) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

# Combine plots using patchwork
carbon_phosphorus_stocks <- carbon_plot | phosphorus_plot

# Display and save the figure
print(carbon_phosphorus_stocks)
ggsave("02-output/a-figures/carbon-phosphorus-stocks-27AUG2025.png", carbon_phosphorus_stocks,
       width = 12, height = 8, dpi = 300)

# Depth profile figure ----------------------------------------------------

# Calculate mean values by land use and depth interval
soil_summary <- infews_comp %>%
  filter(!is.na(land_use),
         top %in% c(0, 10, 20, 40, 60, 80),
         bottom %in% c(10, 20, 40, 60, 80, 100)) %>%
  group_by(land_use, top, bottom) %>%
  summarise(
    mean_total_carbon = mean(total_carbon, na.rm = TRUE),
    mean_bray_p = mean(bray_p, na.rm = TRUE),
    mean_pH = mean(pH, na.rm = TRUE),
    mean_lead = mean(lead, na.rm = TRUE),
    n_samples = n(),
    .groups = 'drop'
  )

# Function to create step plot data
create_step_data <- function(data, value_col) {
  step_data <- data %>%
    select(land_use, top, bottom, value = all_of(value_col)) %>%
    mutate(depth = top) %>%
    select(land_use, depth, value) %>%
    arrange(land_use, depth)
  
  # Add final points at 100cm
  final_points <- data %>%
    filter(top == 80) %>%
    select(land_use, value = all_of(value_col)) %>%
    mutate(depth = 100)
  
  step_data <- bind_rows(step_data, final_points) %>%
    arrange(land_use, depth)
  
  return(step_data)
}

# Create step data for each property
carbon_data <- create_step_data(soil_summary, "mean_total_carbon")
p_data <- create_step_data(soil_summary, "mean_bray_p") 
ph_data <- create_step_data(soil_summary, "mean_pH")
lead_data <- create_step_data(soil_summary, "mean_lead")

# Apply cleaned labels
land_use_labels <- GetLandUseLabels()
carbon_data$land_use_clean <- land_use_labels[carbon_data$land_use]
p_data$land_use_clean <- land_use_labels[p_data$land_use]
ph_data$land_use_clean <- land_use_labels[ph_data$land_use]
lead_data$land_use_clean <- land_use_labels[lead_data$land_use]

# Create individual plots
totalc <- ggplot(carbon_data, aes(x = value, y = depth, color = land_use_clean)) +
  geom_path(size = 1.2) +
  geom_point(size = 2) +
  scale_y_reverse(limits = c(100, 0), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(limits = c(0, 8)) +
  labs(
    title = "Total Organic Carbon (%)",
    x = "",
    y = "Depth (cm)",
    color = "Land Use"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

brayp <- ggplot(p_data, aes(x = value, y = depth, color = land_use_clean)) +
  geom_path(size = 1.2) +
  geom_point(size = 2) +
  scale_y_reverse(limits = c(100, 0), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(limits = c(0, 200)) +
  labs(
    title = "Bray P (ppm)",
    x = "",
    y = "",
    color = "Land Use"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

pH_plot <- ggplot(ph_data, aes(x = value, y = depth, color = land_use_clean)) +
  geom_path(size = 1.2) +
  geom_point(size = 2) +
  scale_y_reverse(limits = c(100, 0), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(limits = c(6.8, 7.5)) +
  labs(
    title = "pH",
    x = "",
    y = "",
    color = "Land Use"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

lead_plot <- ggplot(lead_data, aes(x = value, y = depth, color = land_use_clean)) +
  geom_path(size = 1.2) +
  geom_point(size = 2) +
  scale_y_reverse(limits = c(100, 0), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(limits = c(0, 200)) +
  labs(
    title = "Lead (ppm)",
    x = "",
    y = "",
    color = "Land Use"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Combine plots
depth_profile_figure <- totalc + brayp + pH_plot + lead_plot + 
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9))

# Display and save the figure
print(depth_profile_figure)
ggsave("02-output/a-figures/depth-profile-figure-28AUG2025.png", depth_profile_figure,
       width = 12, height = 10, dpi = 300)

# Mean values at 80-100cm depth interval by land use ---------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== MEAN SOIL PROPERTIES AT 80-100cm BY LAND USE ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

soil_summary %>%
  filter(top == 80) %>%
  select(land_use, n_samples,
         mean_total_carbon, mean_bray_p, mean_pH, mean_lead) %>%
  print()

# Depth-stratified carbon stock figure ------------------------------------

# Prepare data for plotting - reshape to long format
c_stock_depth_long <- c_stock_depth_zones %>%
  filter(!is.na(land_use)) %>%
  select(code, land_use, stock_top20, stock_bottom80) %>%
  pivot_longer(
    cols = c(stock_top20, stock_bottom80),
    names_to = "depth_zone",
    values_to = "stock"
  ) %>%
  mutate(
    depth_zone = factor(depth_zone,
                        levels = c("stock_top20", "stock_bottom80"),
                        labels = c("0-20 cm", "20-100 cm")),
    land_use = factor(land_use,
                      levels = c("Garden_food_producing_in_ground",
                                 "Garden_food_producing_raised_beds",
                                 "Garden_non_food_producing",
                                 "Maintained_lawn",
                                 "Natural_vegetation"))
  )

# Create significance labels data frame
depth_labels <- data.frame(
  land_use = factor(rep(c("Garden_food_producing_in_ground",
                          "Garden_food_producing_raised_beds",
                          "Garden_non_food_producing",
                          "Maintained_lawn",
                          "Natural_vegetation"), each = 2),
                    levels = c("Garden_food_producing_in_ground",
                               "Garden_food_producing_raised_beds",
                               "Garden_non_food_producing",
                               "Maintained_lawn",
                               "Natural_vegetation")),
  depth_zone = factor(rep(c("0-20 cm", "20-100 cm"), 5),
                      levels = c("0-20 cm", "20-100 cm")),
  
  # Add significance labels
  label = c("A", "B",  # GFPG
            "A", "A",  # GFPR
            "A", "A",  # GNFP
            "A", "B",  # LAWN
            "A", "B"), # NATV
 
  # Stagger position of significance labels for readability 
   y_pos = c(70, 70,  # GFPG
             40, 40,  # GFPR  
             50, 50,  # GNFP
             60, 60,  # LAWN
             40, 40)  # NATV
)

# Create grouped boxplot comparing depth zones within each land use
depth_comparison_plot <- ggplot(c_stock_depth_long,
                                aes(x = land_use, y = stock, fill = depth_zone)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.size = 1) +
  geom_text(data = depth_labels,
            aes(x = land_use, y = y_pos, label = label),
            position = position_dodge(width = 0.8),
            size = 4,
            fontface = "bold") +
  labs(
    # title = "Carbon Stock Distribution by Depth Zone",
    x = "Land Use",
    y = expression("Carbon Stock (kgC m"^-2*")"),
    fill = "Depth Zone"
  ) +
  ylim(0, 75) + # 4 values removed; 3 from GFPG, 1 from LAWN
  scale_x_discrete(labels = LAND_USE_CODES[levels(c_stock_depth_long$land_use)]) +
  scale_fill_manual(values = c("0-20 cm" = "#D55E00", "20-100 cm" = "#0072B2")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 0),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(depth_comparison_plot)

# Save the figure with date stamp
today_date <- format(Sys.Date(), "%d%b%Y")
ggsave(paste0("02-output/a-figures/carbon-depth-comparison-", today_date, ".png"),
       depth_comparison_plot,
       width = 10, height = 6, dpi = 300)

# Paired line plot showing individual cores ---------------------

depth_paired_plot <- ggplot(c_stock_depth_long,
                            aes(x = depth_zone, y = stock, group = code)) +
  geom_line(aes(color = land_use), alpha = 0.3, size = 0.5) +
  geom_point(aes(color = land_use), alpha = 0.5, size = 1) +
  stat_summary(aes(group = land_use, color = land_use),
               fun = median, geom = "line", size = 1.5) +
  stat_summary(aes(group = land_use, color = land_use),
               fun = median, geom = "point", size = 3, shape = 18) +
  facet_wrap(~land_use, nrow = 1, labeller = labeller(land_use = LAND_USE_CODES)) +
  scale_color_manual(values = LAND_USE_COLORS) +
  labs(
    # title = "Paired Carbon Stock Comparison: Top 20cm vs. Bottom 80cm",
    x = "Depth Zone",
    y = expression("Carbon Stock (kgC m"^-2*")")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, face = "italic"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(0.5, "lines")
  )

# Display the paired plot
print(depth_paired_plot)

# Save the paired figure
ggsave(paste0("02-output/a-figures/carbon-depth-paired-", today_date, ".png"),
       depth_paired_plot,
       width = 14, height = 5, dpi = 300)

