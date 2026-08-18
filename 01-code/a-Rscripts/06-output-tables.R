# Surface Summary Statistics Table ----------------------------------------

# Function to calculate arithmetic means, CVs, and medians
CalculateSoilStats <- function(data, variable_name, decimals) {
  stats_df <- data %>%
    filter(!is.na(land_use) & !is.na(get(variable_name))) %>%
    group_by(land_use) %>%
    summarise(
      mean_val = mean(get(variable_name), na.rm = TRUE),
      sd_val = sd(get(variable_name), na.rm = TRUE),
      cv_val = (sd(get(variable_name), na.rm = TRUE) / mean(get(variable_name), na.rm = TRUE)) * 100,
      median_val = median(get(variable_name), na.rm = TRUE),
      min_val = min(get(variable_name), na.rm = TRUE),
      max_val = max(get(variable_name), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      mean_cv_median = paste0(
        round(mean_val, decimals), " ± ", round(cv_val, 1), "% ", 
        "(", round(median_val, decimals), ")"
      ),
      range = paste0(round(min_val, decimals), "-", round(max_val, decimals))
    ) %>%
    select(land_use, mean_cv_median, range, median_val)
  
  return(stats_df)
}

# Calculate sample sizes (unique plots per land use)
sample_sizes <- infews_top %>%
  filter(!is.na(land_use)) %>%
  group_by(land_use) %>%
  summarise(n = n_distinct(code), .groups = 'drop')

# Calculate overall summary statistics across all land uses
overall_carbon <- infews_top %>%
  filter(!is.na(total_carbon)) %>%
  summarise(
    mean_val = mean(total_carbon, na.rm = TRUE),
    cv_val = (sd(total_carbon, na.rm = TRUE) / mean(total_carbon, na.rm = TRUE)) * 100,
    median_val = median(total_carbon, na.rm = TRUE)
  )

overall_lead <- infews_top %>%
  filter(!is.na(lead)) %>%
  summarise(
    mean_val = mean(lead, na.rm = TRUE),
    cv_val = (sd(lead, na.rm = TRUE) / mean(lead, na.rm = TRUE)) * 100,
    median_val = median(lead, na.rm = TRUE)
  )

overall_ph <- infews_top %>%
  filter(!is.na(pH)) %>%
  summarise(
    mean_val = mean(pH, na.rm = TRUE),
    cv_val = (sd(pH, na.rm = TRUE) / mean(pH, na.rm = TRUE)) * 100,
    median_val = median(pH, na.rm = TRUE)
  )

overall_brayp <- infews_top %>%
  filter(!is.na(bray_p)) %>%
  summarise(
    mean_val = mean(bray_p, na.rm = TRUE),
    cv_val = (sd(bray_p, na.rm = TRUE) / mean(bray_p, na.rm = TRUE)) * 100,
    median_val = median(bray_p, na.rm = TRUE)
  )

overall_phosphorus <- infews_top %>%
  filter(!is.na(total_p)) %>%
  summarise(
    mean_val = mean(total_p, na.rm = TRUE),
    cv_val = (sd(total_p, na.rm = TRUE) / mean(total_p, na.rm = TRUE)) * 100,
    median_val = median(total_p, na.rm = TRUE)
  )

overall_ksat <- infews_top %>%
  filter(!is.na(ksat_avg)) %>%
  summarise(
    mean_val = mean(ksat_avg, na.rm = TRUE),
    cv_val = (sd(ksat_avg, na.rm = TRUE) / mean(ksat_avg, na.rm = TRUE)) * 100,
    median_val = median(ksat_avg, na.rm = TRUE)
  )

overall_bd <- infews_top %>%
  filter(!is.na(bd_comp)) %>%
  summarise(
    mean_val = mean(bd_comp, na.rm = TRUE),
    cv_val = (sd(bd_comp, na.rm = TRUE) / mean(bd_comp, na.rm = TRUE)) * 100,
    median_val = median(bd_comp, na.rm = TRUE)
  )

# Calculate overall sample size
overall_n <- infews_top %>%
  filter(!is.na(land_use)) %>%
  summarise(n = n_distinct(code)) %>%
  pull(n)

# Calculate statistics for each soil property
carbon_data <- CalculateSoilStats(infews_top, "total_carbon", 1)
lead_data <- CalculateSoilStats(infews_top, "lead", 1)
ph_data <- CalculateSoilStats(infews_top, "pH", 1)
brayp_data <- CalculateSoilStats(infews_top, "bray_p", 1)
phosphorus_data <- CalculateSoilStats(infews_top, "total_p", 0)
ksat_data <- CalculateSoilStats(infews_top, "ksat_avg", 1)
bd_data <- CalculateSoilStats(infews_top, "bd_comp", 2)

# Create a comprehensive data frame with ALL columns including Overall
soil_properties <- data.frame(
  Property = c("Total Carbon (%)", "Lead (ppm)", "pH", "Bray Phosphorus (ppm)", 
               "Total Phosphorus (ppm)", "Ksat (cm/hr)", "Bulk Density (g/cm³)"),
  
  # Mean ± CV% (Median) for each land use
  Garden_Food_In_Ground = c(
    carbon_data$mean_cv_median[carbon_data$land_use == "Garden_food_producing_in_ground"],
    lead_data$mean_cv_median[lead_data$land_use == "Garden_food_producing_in_ground"],
    ph_data$mean_cv_median[ph_data$land_use == "Garden_food_producing_in_ground"],
    brayp_data$mean_cv_median[brayp_data$land_use == "Garden_food_producing_in_ground"],
    phosphorus_data$mean_cv_median[phosphorus_data$land_use == "Garden_food_producing_in_ground"],
    ksat_data$mean_cv_median[ksat_data$land_use == "Garden_food_producing_in_ground"],
    bd_data$mean_cv_median[bd_data$land_use == "Garden_food_producing_in_ground"]
  ),
  
  Garden_Food_Raised_Beds = c(
    carbon_data$mean_cv_median[carbon_data$land_use == "Garden_food_producing_raised_beds"],
    lead_data$mean_cv_median[lead_data$land_use == "Garden_food_producing_raised_beds"],
    ph_data$mean_cv_median[ph_data$land_use == "Garden_food_producing_raised_beds"],
    brayp_data$mean_cv_median[brayp_data$land_use == "Garden_food_producing_raised_beds"],
    phosphorus_data$mean_cv_median[phosphorus_data$land_use == "Garden_food_producing_raised_beds"],
    ksat_data$mean_cv_median[ksat_data$land_use == "Garden_food_producing_raised_beds"],
    bd_data$mean_cv_median[bd_data$land_use == "Garden_food_producing_raised_beds"]
  ),
  
  Garden_Non_Food = c(
    carbon_data$mean_cv_median[carbon_data$land_use == "Garden_non_food_producing"],
    lead_data$mean_cv_median[lead_data$land_use == "Garden_non_food_producing"],
    ph_data$mean_cv_median[ph_data$land_use == "Garden_non_food_producing"],
    brayp_data$mean_cv_median[brayp_data$land_use == "Garden_non_food_producing"],
    phosphorus_data$mean_cv_median[phosphorus_data$land_use == "Garden_non_food_producing"],
    ksat_data$mean_cv_median[ksat_data$land_use == "Garden_non_food_producing"],
    bd_data$mean_cv_median[bd_data$land_use == "Garden_non_food_producing"]
  ),
  
  Maintained_Lawn = c(
    carbon_data$mean_cv_median[carbon_data$land_use == "Maintained_lawn"],
    lead_data$mean_cv_median[lead_data$land_use == "Maintained_lawn"],
    ph_data$mean_cv_median[ph_data$land_use == "Maintained_lawn"],
    brayp_data$mean_cv_median[brayp_data$land_use == "Maintained_lawn"],
    phosphorus_data$mean_cv_median[phosphorus_data$land_use == "Maintained_lawn"],
    ksat_data$mean_cv_median[ksat_data$land_use == "Maintained_lawn"],
    bd_data$mean_cv_median[bd_data$land_use == "Maintained_lawn"]
  ),
  
  Natural_Vegetation = c(
    carbon_data$mean_cv_median[carbon_data$land_use == "Natural_vegetation"],
    lead_data$mean_cv_median[lead_data$land_use == "Natural_vegetation"],
    ph_data$mean_cv_median[ph_data$land_use == "Natural_vegetation"],
    brayp_data$mean_cv_median[brayp_data$land_use == "Natural_vegetation"],
    phosphorus_data$mean_cv_median[phosphorus_data$land_use == "Natural_vegetation"],
    ksat_data$mean_cv_median[ksat_data$land_use == "Natural_vegetation"],
    bd_data$mean_cv_median[bd_data$land_use == "Natural_vegetation"]
  ),
  
  # Overall summary column
  Overall = c(
    paste0(round(overall_carbon$mean_val, 1), " ± ", round(overall_carbon$cv_val, 1), "% (", round(overall_carbon$median_val, 1), ")"),
    paste0(round(overall_lead$mean_val, 1), " ± ", round(overall_lead$cv_val, 1), "% (", round(overall_lead$median_val, 1), ")"),
    paste0(format(round(overall_ph$mean_val, 1), nsmall = 1), " ± ", round(overall_ph$cv_val, 1), "% (", format(round(overall_ph$median_val, 1), nsmall = 1), ")"),
    paste0(round(overall_brayp$mean_val, 1), " ± ", round(overall_brayp$cv_val, 1), "% (", round(overall_brayp$median_val, 1), ")"),
    paste0(round(overall_phosphorus$mean_val, 0), " ± ", round(overall_phosphorus$cv_val, 1), "% (", round(overall_phosphorus$median_val, 0), ")"),
    paste0(round(overall_ksat$mean_val, 1), " ± ", round(overall_ksat$cv_val, 1), "% (", round(overall_ksat$median_val, 1), ")"),
    paste0(round(overall_bd$mean_val, 2), " ± ", round(overall_bd$cv_val, 1), "% (", round(overall_bd$median_val, 2), ")")
  )
)

# Create the publication-ready table using gt
surface_soil_table <- soil_properties %>%
  gt() %>%
  tab_header(
    title = "Surface Soil Properties (0-10 cm)",
    subtitle = "Mean ± Coefficient of Variation (%) (Median)"
  ) %>%
  tab_spanner(
    label = "Land Use",
    columns = c(Garden_Food_In_Ground, Garden_Food_Raised_Beds, Garden_Non_Food, 
                Maintained_Lawn, Natural_Vegetation, Overall)
  ) %>%
  cols_label(
    Property = "",
    Garden_Food_In_Ground = "Garden Food Producing In Ground",
    Garden_Food_Raised_Beds = "Garden Food Producing Raised Beds",
    Garden_Non_Food = "Garden Non-Food Producing",
    Maintained_Lawn = "Maintained Lawn",
    Natural_Vegetation = "Natural Vegetation",
    Overall = "Overall"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title()
  ) %>%
  cols_width(
    Property ~ px(200),
    everything() ~ px(120)
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold"
  )

# Add sample sizes as a footnote
n_footnote <- paste0("Sample sizes: ",
                     "Garden Food In Ground (n=", sample_sizes$n[sample_sizes$land_use == "Garden_food_producing_in_ground"], "), ",
                     "Garden Food Raised Beds (n=", sample_sizes$n[sample_sizes$land_use == "Garden_food_producing_raised_beds"], "), ",
                     "Garden Non-Food (n=", sample_sizes$n[sample_sizes$land_use == "Garden_non_food_producing"], "), ",
                     "Maintained Lawn (n=", sample_sizes$n[sample_sizes$land_use == "Maintained_lawn"], "), ",
                     "Natural Vegetation (n=", sample_sizes$n[sample_sizes$land_use == "Natural_vegetation"], "), ",
                     "Overall (n=", overall_n, ").")

surface_soil_table <- surface_soil_table %>%
  tab_source_note(source_note = n_footnote)

# Display and save the table
surface_soil_table

gtsave(surface_soil_table, 
       "02-output/b-tables/surface-summary-stats-28AUG2025.png", 
       vwidth = 1400, 
       vheight = 600)

# Optional: Print ranges for your review
cat("\n=== RANGES FOR YOUR REVIEW ===\n")
print("Carbon ranges:")
print(carbon_data %>% select(land_use, range))

print("Lead ranges:")
print(lead_data %>% select(land_use, range))

print("pH ranges:")
print(ph_data %>% select(land_use, range))

print("Bray P ranges:")
print(brayp_data %>% select(land_use, range))

print("Total P ranges:")
print(phosphorus_data %>% select(land_use, range))

print("Ksat ranges:")
print(ksat_data %>% select(land_use, range))

print("Bulk density ranges:")
print(bd_data %>% select(land_use, range))