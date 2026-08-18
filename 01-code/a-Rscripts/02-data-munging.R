# Develop nonlinear model to predict bulk density from total carbon -------
# Fill in missing and invalid bulk density values

# Remove 2021 data and filter bulk density outliers
infews_subset1 <- filter(infews_raw, year != 2021)
infews_subset2 <- filter(infews_subset1, bulk_density <= 2.6 & bulk_density != 0)

# Create and plot linear model
bd_lm <- lm(bulk_density ~ total_carbon, data = infews_subset2)
plot(infews_subset2$total_carbon, infews_subset2$bulk_density)
abline(bd_lm, col = "red", lty = 2, lwd = 2)

# Fit the non-linear model. Starting values come from plot above
bd_nls <- nls(bulk_density ~ a * exp(-b * total_carbon) + c,
              start = list(a = 1.5, b = 0.15, c = 0.5),
              algorithm = "port",
              data = infews_subset2,
              lower = list(a = 0.5, b = 0.01, c = 0.1),
              upper = list(a = 3.0, b = 0.5, c = 1.0))

# Extract coefficients from the model
coef_a <- coef(bd_nls)["a"]
coef_b <- coef(bd_nls)["b"]
coef_c <- coef(bd_nls)["c"]

# Create predictions
infews_raw$bd_pred <- coef_a * exp(-coef_b * infews_raw$total_carbon) + coef_c

# Create the complete bulk density variable. Uses existing bd values when 
# present. Uses predicted variable when bd value not present in full data set 
# OR when year = 2021
infews_raw$bd_meas <- infews_raw$bulk_density
infews_raw$bd_meas[infews_raw$bd_meas == 0 | infews_raw$bd_meas >= 2.6] <- NA
infews_raw$bd_comp <- ifelse(is.na(infews_raw$bd_meas), infews_raw$bd_pred, infews_raw$bd_meas)
infews_raw$bd_comp <- ifelse(infews_raw$year == 2021, infews_raw$bd_pred, infews_raw$bd_comp)

# Create a multi-panel plot to compare observed and predicted bulk densities
par(mfrow = c(2, 2))

# Plot 1: Original data with model fit
plot(infews_subset2$total_carbon, infews_subset2$bulk_density,
     xlab = "Total Carbon", ylab = "Bulk Density",
     main = "Data with Model Fit")
curve(coef_a * exp(-coef_b * x) + coef_c, 
      add = TRUE, col = "red", lwd = 2)

# Plot 2: Predicted vs Observed
plot(infews_subset2$bulk_density, infews_subset2$bd_pred,
     xlab = "Observed Bulk Density",
     ylab = "Predicted Bulk Density",
     main = "Predicted vs Observed")
abline(0, 1, col = "red")

# Plot 3: Residuals
plot(predict(bd_nls), residuals(bd_nls),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")

# Plot 4: Original vs Complete data
plot(infews_raw$total_carbon, infews_raw$bd_comp,
     xlab = "Total Carbon", ylab = "Complete Bulk Density",
     main = "Final Dataset")

# Calculate summary statistics for bulk density model-----------------------------------
summary(bd_nls)

# Filter to only rows used in model fitting (complete cases)
infews_complete <- infews_subset2 %>%
  filter(!is.na(bulk_density) & !is.na(total_carbon))

# Calculate R²
cor(fitted(bd_nls), infews_complete$bulk_density)^2

# Breakdown by reason for using predicted BD
samples_2021 <- sum(infews_raw$year.x == 2021, na.rm = TRUE)
samples_invalid <- sum(is.na(infews_raw$bd_meas) & infews_raw$year.x != 2021, na.rm = TRUE)

cat("  2021 samples:", samples_2021, "\n")
cat("  Invalid BD (other years):", samples_invalid, "\n")

# Standardize land use categories in site level csv -----------------------
unique(infews_site_raw$land_use)

infews_site_raw$land_use_detail <- infews_site_raw$land_use
infews_site_raw$land_use[infews_site_raw$land_use == "Unmanaged_Turfgrass"] <- "Natural_vegetation"
infews_site_raw$land_use[infews_site_raw$land_use == "Garden"] <- "Garden_non_food_producing"

unique(infews_site_raw$land_use)

# Verify that site type categories are consistent
unique(infews_site_raw$site_type)

# Add a unique numeric code to every plot ---------------------------------

# Create a sample code to combine the site and sample data sheets 
# (8 digits: year, site, plot)
infews_site_raw$code <- paste(infews_site_raw$year, infews_site_raw$site, infews_site_raw$plot, sep = "")
infews_raw$code <- paste(infews_raw$year, infews_raw$site, infews_raw$plot, sep = "")

# Combine sample and site sheets via left join to create "complete" (comp) 
# data set
infews_comp <- left_join(infews_raw, infews_site_raw, by = c("code" = "code"))

# Export to create a csv with both site and sample level data; read back in
write.csv(infews_comp, here::here("00-data", "b-prepared", "infews-combined-data.csv"))

infews_comp <- read.csv(
  here::here("00-data", "b-prepared", "infews-combined-data.csv")
)

# Create data subsets for analysis + figures -------------------------------

# Create a subset with only the surface samples (0-10cm data)
# Individual analyses will filter for their specific properties and remove NAs as needed
infews_top <- filter(infews_comp, top == 0)

# Create a subset with samples from 0-20cm depth (0-10cm and 10-20cm increments)
infews_top20 <- filter(infews_comp, top %in% c(0, 10))

# Create averaged 0-20cm dataset (mean of 0-10 and 10-20cm samples)
infews_top20_avg <- infews_top20 %>%
  group_by(code) %>%
  summarise(
    # Depth columns - set to represent 0-20cm zone
    top = 0,
    bottom = 20,
    
    # Lab measurements - calculate mean
    # Returns NA if all values are NA; if one value is NA, uses value from non-NA sample
    pH = ifelse(all(is.na(pH)), NA_real_, mean(pH, na.rm = TRUE)),
    bray_p = ifelse(all(is.na(bray_p)), NA_real_, mean(bray_p, na.rm = TRUE)),
    olsen_p = ifelse(all(is.na(olsen_p)), NA_real_, mean(olsen_p, na.rm = TRUE)),
    total_carbon = ifelse(all(is.na(total_carbon)), NA_real_, mean(total_carbon, na.rm = TRUE)),
    inorganic_carbon = ifelse(all(is.na(inorganic_carbon)), NA_real_, mean(inorganic_carbon, na.rm = TRUE)),
    total_p = ifelse(all(is.na(total_p)), NA_real_, mean(total_p, na.rm = TRUE)),
    lead = ifelse(all(is.na(lead)), NA_real_, mean(lead, na.rm = TRUE)),
    bulk_density = ifelse(all(is.na(bulk_density)), NA_real_, mean(bulk_density, na.rm = TRUE)),
    ksat = ifelse(all(is.na(ksat)), NA_real_, mean(ksat, na.rm = TRUE)),
    loi_om = ifelse(all(is.na(loi_om)), NA_real_, mean(loi_om, na.rm = TRUE)),
    bd_pred = ifelse(all(is.na(bd_pred)), NA_real_, mean(bd_pred, na.rm = TRUE)),
    bd_meas = ifelse(all(is.na(bd_meas)), NA_real_, mean(bd_meas, na.rm = TRUE)),
    bd_comp = ifelse(all(is.na(bd_comp)), NA_real_, mean(bd_comp, na.rm = TRUE)),

    # Site-level metadata - keep first value (same for all depths)
    year = first(year.x),
    site = first(site.x),
    plot = first(plot.x),
    land_use = first(land_use),
    land_use_detail = first(land_use_detail),
    site_type = first(site_type),
    ksat_avg = first(ksat_avg)
  ) %>%
  ungroup()
