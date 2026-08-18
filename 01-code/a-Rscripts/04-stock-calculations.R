# Utility function for standardized land use labels in figures ------------

GetLandUseLabels <- function() {
  c("Garden_food_producing_in_ground" = "Garden Food Producing\nIn Ground",
    "Garden_food_producing_raised_beds" = "Garden Food Producing\nRaised Beds", 
    "Garden_non_food_producing" = "Garden Non-Food\nProducing",
    "Maintained_lawn" = "Maintained Lawn",
    "Natural_vegetation" = "Natural Vegetation")
}

# Stock calculation function ----------------------------------------------

CalculateElementStock <- function(data, element_col, conversion_factor = 1, 
                                  element_name = element_col, units = "kg/m²") {
  
  # Print header
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(paste("===", element_name, "Stock Calculations ==="), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Filter data to remove NAs for the specific element
  data_clean <- data %>% 
    filter(!is.na(!!sym(element_col)) & !is.na(bd_comp) & 
             !is.na(bottom) & !is.na(top) & !is.na(code))
  
  cat(paste("\nSample size after removing NAs:", nrow(data_clean), "horizon samples\n"))
  
  # Calculate stock for individual horizon/sample
  stock_col_name <- paste0(gsub("total_|_avg", "", element_col), "_stock_horizon")
  
  data_clean[[stock_col_name]] <- data_clean$bd_comp * 
    (data_clean[[element_col]] / conversion_factor) * 
    (data_clean$bottom - data_clean$top) * 10
  
  cat(paste("Calculated", stock_col_name, "for each horizon\n"))
  
  # Aggregate by core (sum all horizons for each core)
  stock_summary <- data_clean %>%
    group_by(code) %>%
    summarise(
      stock = sum(!!sym(stock_col_name), na.rm = TRUE),
      n_horizons = n(),
      .groups = 'drop'
    )
  
  stock_summary$code <- as.character(stock_summary$code)
  
  cat(paste("Aggregated to", nrow(stock_summary), "cores\n"))
  
  # Join with site data to get land use and site type information
  stock_with_site <- left_join(stock_summary, infews_site_raw, 
                               by = c("code" = "code"))

  # Print summary statistics
  cat("\nSummary Statistics:\n")
  cat(paste("Mean stock:", round(mean(stock_with_site$stock, na.rm = TRUE), 3), units, "\n"))
  cat(paste("Median stock:", round(median(stock_with_site$stock, na.rm = TRUE), 3), units, "\n"))
  cat(paste("Range:", round(min(stock_with_site$stock, na.rm = TRUE), 3), "to", 
            round(max(stock_with_site$stock, na.rm = TRUE), 3), units, "\n"))
  
  # Return the complete dataset with site information
  return(stock_with_site)
}

# Standardized Kruskal-Wallis analysis function for stock data ------------

PerformKruskalWallisStocks <- function(data, variable = "stock", group_var = "land_use", 
                                       variable_label = "Stock", 
                                       units = "units",
                                       significance_labels = NULL,
                                       label_offset = NULL) {
  
  # Print section header
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(paste("===", variable_label, "Kruskal-Wallis Analysis ==="), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Filter data to remove NAs for this specific variable
  data_clean <- data %>% 
    filter(!is.na(!!sym(variable)) & !is.na(!!sym(group_var))) %>%
    mutate(group_abbrev = case_when(
      !!sym(group_var) == "Garden_food_producing_in_ground" ~ "GFPG",
      !!sym(group_var) == "Garden_food_producing_raised_beds" ~ "GFPR", 
      !!sym(group_var) == "Garden_non_food_producing" ~ "GNFP",
      !!sym(group_var) == "Maintained_lawn" ~ "LAWN",
      !!sym(group_var) == "Natural_vegetation" ~ "NATV"
    ))
  
  # Create exploratory boxplot by site type
  p_site <- ggplot(data_clean, aes(x = .data[[variable]], y = site_type)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    labs(y = "Site Type", x = paste(variable_label, paste0("(", units, ")")))

  print(p_site)
  
  # Perform Kruskal-Wallis test
  formula_str <- paste(variable, "~", group_var)
  kw_result <- kruskal.test(as.formula(formula_str), data = data_clean)
  
  cat("\nKruskal-Wallis Test Results:\n")
  print(kw_result)
  
  # Post-hoc analysis: Dunn's test with Benjamini-Hochberg adjustment
  dunn_result <- dunn.test(data_clean[[variable]], 
                           data_clean$group_abbrev, 
                           method = "bh",  # Benjamini-Hochberg correction
                           alpha = 0.05)
  
  # Calculate median and IQR for each group
  summary_stats <- data_clean %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      median = median(!!sym(variable), na.rm = TRUE),
      Q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      mean = mean(!!sym(variable), na.rm = TRUE),
      SE = sd(!!sym(variable), na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = 'drop'
    )
  
  cat("\nGroup Medians and IQR:\n")
  print(summary_stats)
  
  # Create ggplot with significance labels
  p <- ggplot(data_clean, aes(x = .data[[variable]], y = .data[[group_var]])) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    labs(y = "Land Use", x = paste(variable_label, paste0("(", units, ")"))) +
    scale_y_discrete(labels = GetLandUseLabels())
  
  # Add significance labels if provided
  if (!is.null(significance_labels) && !is.null(label_offset)) {
    p <- p + geom_text(data = summary_stats, 
                       aes(x = median + label_offset, y = !!sym(group_var)), 
                       label = significance_labels,
                       size = 4,
                       fontface = "bold")
  }
  
  print(p)
  
  # Return results for further use
  return(list(
    kruskal_wallis = kw_result,
    dunn = dunn_result,
    summary = summary_stats,
    plot = p,
    data_used = data_clean
  ))
}

# Function to extract Kruskal-Wallis results for summary table ------------

ExtractKwResults <- function(kw_test, test_name) {
  data.frame(
    Variable = test_name,
    Chi_squared = round(kw_test$statistic, 3),
    df = kw_test$parameter,
    p_value = round(kw_test$p.value, 4),
    Significant = ifelse(kw_test$p.value < 0.05, "Yes", "No")
  )
}

# Carbon stock calculations -----------------------------------------------

# Calculate soil organic carbon stocks
soc_stock_site <- CalculateElementStock(
  data = infews_comp,
  element_col = "total_carbon",
  conversion_factor = 100,  # Convert percent to fraction
  element_name = "Soil Organic Carbon",
  units = "kgC/m²"
)

# Statistical analysis for carbon stocks
carbon_stock_results <- PerformKruskalWallisStocks(
  data = soc_stock_site,
  variable = "stock",
  variable_label = "Soil Organic Carbon Stock",
  units = "kgC/m²",
  significance_labels = c("A", "AB", "C", "C", "B"),
  label_offset = 55
)

# Add x-axis limits to the land use plot
carbon_stock_results$plot <- carbon_stock_results$plot + xlim(0, 100)
print(carbon_stock_results$plot)

# Phosphorus stock calculations -------------------------------------------

# Calculate phosphorus stocks (P reported as ppm, convert to kg/kg)
phosphorus_stock_site <- CalculateElementStock(
  data = infews_comp,
  element_col = "total_p",
  conversion_factor = 1e6,  # Convert ppm to kg/kg
  element_name = "Total Phosphorus",
  units = "kgP/m²"
)

# Statistical analysis for phosphorus stocks
phosphorus_stock_results <- PerformKruskalWallisStocks(
  data = phosphorus_stock_site,
  variable = "stock",
  variable_label = "Total Phosphorus Stock",
  units = "kgP/m²",
  significance_labels = c("A", "AB", "B", "B", "B"),
  label_offset = 1
)

# Summary of all stock calculations --------------------------------------

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== SUMMARY OF ALL STOCK CALCULATIONS ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

stock_summary_table <- data.frame(
  Element = c("Soil Organic Carbon", "Total Phosphorus"),
  n_cores = c(nrow(soc_stock_site), nrow(phosphorus_stock_site)),
  Mean_stock = c(round(mean(soc_stock_site$stock, na.rm = TRUE), 3),
                 round(mean(phosphorus_stock_site$stock, na.rm = TRUE), 4)),
  Median_stock = c(round(median(soc_stock_site$stock, na.rm = TRUE), 3),
                   round(median(phosphorus_stock_site$stock, na.rm = TRUE), 4)),
  Units = c("kgC/m²", "kgP/m²")
)

print(stock_summary_table)

# Summary of statistical analyses ----------------------------------------

# Function to extract Kruskal-Wallis results for summary table
ExtractKwStockResults <- function(kw_test, test_name) {
  data.frame(
    Stock_Type = test_name,
    Chi_squared = round(kw_test$statistic, 3),
    df = kw_test$parameter,
    p_value = round(kw_test$p.value, 4),
    Significant = ifelse(kw_test$p.value < 0.05, "Yes", "No")
  )
}

# Combine all stock test results into summary table
all_stock_kw_results <- rbind(
  ExtractKwStockResults(carbon_stock_results$kruskal_wallis, "Soil Organic Carbon"),
  ExtractKwStockResults(phosphorus_stock_results$kruskal_wallis, "Total Phosphorus")
)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== SUMMARY OF ALL STOCK KRUSKAL-WALLIS TEST RESULTS ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(all_stock_kw_results)

# Depth-stratified carbon stock analysis ----------------------------------

# Calculate horizon-level carbon stocks
infews_comp_c_stock <- infews_comp %>%
  filter(!is.na(total_carbon) & !is.na(bd_comp) &
           !is.na(bottom) & !is.na(top) & !is.na(code)) %>%
  mutate(carbon_stock_horizon = bd_comp * (total_carbon / 100) * (bottom - top) * 10)

# Calculate stocks for 0-20cm depth zone (sum horizons where top < 20)
c_stock_top20 <- infews_comp_c_stock %>%
  filter(top < 20) %>%
  group_by(code) %>%
  summarise(
    stock_top20 = sum(carbon_stock_horizon, na.rm = TRUE),
    n_horizons_top = n(),
    .groups = 'drop'
  )

# Calculate stocks for 20-100cm depth zone (sum horizons where top >= 20)
c_stock_bottom80 <- infews_comp_c_stock %>%
  filter(top >= 20) %>%
  group_by(code) %>%
  summarise(
    stock_bottom80 = sum(carbon_stock_horizon, na.rm = TRUE),
    n_horizons_bottom = n(),
    .groups = 'drop'
  )

# Convert code to character for joining
c_stock_top20$code <- as.character(c_stock_top20$code)
c_stock_bottom80$code <- as.character(c_stock_bottom80$code)

# Join top and bottom stocks with site data
c_stock_depth_zones <- left_join(c_stock_top20, c_stock_bottom80, by = "code") %>%
  left_join(infews_site_raw, by = "code") %>%
  filter(!is.na(stock_top20) & !is.na(stock_bottom80)) # Keep only cores with both zones

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== DEPTH-STRATIFIED CARBON STOCK ANALYSIS ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat(paste("\nTotal cores with both depth zones:", nrow(c_stock_depth_zones), "\n"))

# Perform Wilcoxon signed-rank test for each land use class
land_use_classes <- unique(c_stock_depth_zones$land_use[!is.na(c_stock_depth_zones$land_use)])

wilcox_results_list <- list()

for (lu in land_use_classes) {
  
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat(paste("Land Use:", lu, "\n"))
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # Filter data for this land use
  lu_data <- c_stock_depth_zones %>% filter(land_use == lu)
  
  cat(paste("n =", nrow(lu_data), "cores\n"))
  
  # Calculate summary statistics
  cat("\nTop 20cm stocks:\n")
  cat(paste("  Median:", round(median(lu_data$stock_top20, na.rm = TRUE), 2), "kgC/m²\n"))
  cat(paste("  Mean:", round(mean(lu_data$stock_top20, na.rm = TRUE), 2), "kgC/m²\n"))
  cat(paste("  Range:", round(min(lu_data$stock_top20, na.rm = TRUE), 2), "-",
            round(max(lu_data$stock_top20, na.rm = TRUE), 2), "kgC/m²\n"))
  
  cat("\nBottom 80cm stocks:\n")
  cat(paste("  Median:", round(median(lu_data$stock_bottom80, na.rm = TRUE), 2), "kgC/m²\n"))
  cat(paste("  Mean:", round(mean(lu_data$stock_bottom80, na.rm = TRUE), 2), "kgC/m²\n"))
  cat(paste("  Range:", round(min(lu_data$stock_bottom80, na.rm = TRUE), 2), "-",
            round(max(lu_data$stock_bottom80, na.rm = TRUE), 2), "kgC/m²\n"))
  
  # Perform Wilcoxon signed-rank test (paired)
  wilcox_test <- wilcox.test(lu_data$stock_top20, lu_data$stock_bottom80,
                             paired = TRUE, alternative = "two.sided")
  
  cat("\nWilcoxon Signed-Rank Test:\n")
  print(wilcox_test)
  
  # Store results
  wilcox_results_list[[lu]] <- data.frame(
    land_use = lu,
    n = nrow(lu_data),
    median_top20 = round(median(lu_data$stock_top20, na.rm = TRUE), 2),
    median_bottom80 = round(median(lu_data$stock_bottom80, na.rm = TRUE), 2),
    V_statistic = wilcox_test$statistic,
    p_value = round(wilcox_test$p.value, 4)
  )
}

# Combine all Wilcoxon test results into a summary table
wilcox_summary_table <- do.call(rbind, wilcox_results_list)
rownames(wilcox_summary_table) <- NULL

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== SUMMARY OF WILCOXON SIGNED-RANK TESTS ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(wilcox_summary_table)

# Shapiro-Wilk test to check normality; confirmed non-normal and Wilcoxon is correct choice
# Create difference variable
c_stock_depth_zones <- c_stock_depth_zones %>%
  mutate(stock_diff = stock_top20 - stock_bottom80)

# For each land use, check normality of differences
for (lu in unique(c_stock_depth_zones$land_use)) {
  lu_data <- filter(c_stock_depth_zones, land_use == lu)
  
  cat("\n", lu, "(n =", nrow(lu_data), ")")
  
  # Shapiro-Wilk test
  if(nrow(lu_data) >= 3) {
    shapiro_result <- shapiro.test(lu_data$stock_diff)
    print(shapiro_result)
  }
  
  # Q-Q plot
  qqnorm(lu_data$stock_diff, main = paste("Q-Q Plot:", lu))
  qqline(lu_data$stock_diff)
  
  # Histogram
  hist(lu_data$stock_diff, main = paste("Differences:", lu),
       xlab = "Stock difference (top20 - bottom80)")
}
