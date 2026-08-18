# Utility function for standardized land use labels in figures ------------

GetLandUseLabels <- function() {
  c("Garden_food_producing_in_ground" = "Garden Food Producing\nIn Ground",
    "Garden_food_producing_raised_beds" = "Garden Food Producing\nRaised Beds", 
    "Garden_non_food_producing" = "Garden Non-Food\nProducing",
    "Maintained_lawn" = "Maintained Lawn",
    "Natural_vegetation" = "Natural Vegetation")
}

# Standardized Kruskal-Wallis analysis function ---------------------------

PerformKruskalWallis <- function(data, variable, group_var = "land_use", 
                                 variable_label = variable, 
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
  
  # Create exploratory boxplots
  boxplot(data_clean[[variable]] ~ data_clean$site_type, 
          horizontal = TRUE, las = 2, cex.axis = 0.7,
          main = paste(variable_label, "by Site Type"))
  boxplot(data_clean[[variable]] ~ data_clean[[group_var]], 
          horizontal = TRUE, las = 2, cex.axis = 0.7,
          main = paste(variable_label, "by Land Use"))
  
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
    labs(y = "Land Use", x = variable_label) +
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

# Carbon ------------------------------------------------------------------

# Perform Kruskal-Wallis analysis for carbon
carbon_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "total_carbon",
  variable_label = "Total Organic Carbon (%)",
  significance_labels = c("A", "A", "B", "B", "B"),
  label_offset = 16
)

# Lead --------------------------------------------------------------------

# Perform Kruskal-Wallis analysis for lead
lead_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "lead",
  variable_label = "Lead (mg kg-1)",
  significance_labels = c("B", "A", "A", "AB", "AB"),
  label_offset = 500
)

# Add xlim constraint for lead plot
lead_results$plot <- lead_results$plot + xlim(0, 700)
print(lead_results$plot)

# Bray phosphorus ---------------------------------------------------------

# Perform Kruskal-Wallis analysis for Bray phosphorus
bray_p_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "bray_p",
  variable_label = "Bray Phosphorus (mg kg-1)",
  significance_labels = c("A", "A", "B", "C", "C"),
  label_offset = 250
)

# Add xlim constraint for bray phosphorus plot
bray_p_results$plot <- bray_p_results$plot + xlim(0, 400)
print(bray_p_results$plot)

# Total Phosphorus --------------------------------------------------------

# Perform Kruskal-Wallis analysis for total phosphorus
phosphorus_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "total_p",
  variable_label = "Total Phosphorus (mg kg-1)",
  significance_labels = c("A", "A", "B", "B", "B"),
  # label_offset = 450
  label_offset = c(1100, 500, 1050, 1050, 1000)  # per-group offsets
)

# Add xlim constraint for total phosphorus plot
phosphorus_results$plot <- phosphorus_results$plot + xlim(0, 2000)
print(phosphorus_results$plot)

# pH ----------------------------------------------------------------------

# Perform Kruskal-Wallis analysis for pH
ph_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "pH",
  variable_label = "pH (1:1 H2O)",
  significance_labels = c("A", "AB", "B", "B", "AB"),
  label_offset = 1.5
)

# Saturated hydraulic conductivity (Ksat) ---------------------------------

# Perform Kruskal-Wallis analysis for Ksat
ksat_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "ksat_avg",
  variable_label = "Ksat (cm hr-1)",
  significance_labels = c("A", "A", "B", "B", "AB"),
  label_offset = 35
)

# Add xlim constraint for Ksat plot
ksat_results$plot <- ksat_results$plot + xlim(0, 100)
print(ksat_results$plot)

# Bulk density ------------------------------------------------------------

# Perform Kruskal-Wallis analysis for bulk density
bd_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "bd_comp",
  variable_label = "Bulk Density (g cm-3)",
  significance_labels = c("C", "AB", "A", "B", "C"),
  label_offset = 1.5
)


# Bulk density (measured values only) -------------------------------------

# Uses bd_meas which has impossible values (0 or >= 2.6 g/cm3) set to NA
# and excludes model-predicted values. NAs are dropped by PerformKruskalWallis.
bd_meas_results <- PerformKruskalWallis(
  data = infews_top,
  variable = "bd_meas",
  variable_label = "Bulk Density - Measured Only (g cm-3)",
  significance_labels = c("B", "AB", "A", "B", "B"),
  label_offset = 1.7
)

# Summary table of all Kruskal-Wallis test results ----------------------

# Combine all test results into summary table
all_kw_results <- rbind(
  ExtractKwResults(carbon_results$kruskal_wallis, "Total Carbon"),
  ExtractKwResults(lead_results$kruskal_wallis, "Lead"),
  ExtractKwResults(ph_results$kruskal_wallis, "pH"),
  ExtractKwResults(bray_p_results$kruskal_wallis, "Bray Phosphorus"),
  ExtractKwResults(phosphorus_results$kruskal_wallis, "Total Phosphorus"),
  ExtractKwResults(ksat_results$kruskal_wallis, "Ksat"),
  ExtractKwResults(bd_results$kruskal_wallis, "Bulk Density")
)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== SUMMARY OF ALL KRUSKAL-WALLIS TEST RESULTS ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(all_kw_results)

# Exploratory: Bulk density (measured) vs Ksat scatterplot -----------------

LAND_USE_COLORS <- c(
  "Garden_non_food_producing" = "#439786",
  "Garden_food_producing_in_ground" = "#8B0000",
  "Maintained_lawn" = "#32CD32",
  "Natural_vegetation" = "#8B4513",
  "Garden_food_producing_raised_beds" = "#FF6347"
)

bd_ksat_data <- infews_top %>%
  filter(!is.na(bd_meas) & !is.na(ksat_avg))

bd_ksat_cor <- cor.test(bd_ksat_data$bd_meas, bd_ksat_data$ksat_avg, method = "spearman")
cat("\nSpearman correlation (bd_meas vs ksat_avg):\n")
cat("  rho =", round(bd_ksat_cor$estimate, 3), "\n")
cat("  p =", signif(bd_ksat_cor$p.value, 4), "\n")

bd_ksat_plot <- ggplot(bd_ksat_data, aes(x = bd_meas, y = ksat_avg, color = land_use)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = LAND_USE_COLORS, labels = GetLandUseLabels()) +
  theme_bw() +
  labs(
    x = expression("Bulk Density (g cm"^-3*")"),
    y = expression("Ksat (cm hr"^-1*")"),
    color = "Land Use"
  ) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Spearman rho = ", round(bd_ksat_cor$estimate, 3),
                          "\np = ", signif(bd_ksat_cor$p.value, 4)),
           hjust = 1.1, vjust = 1.5, size = 3.5)

print(bd_ksat_plot)

# Bray P vs Total P correlation -------------------------------------------

bray_totalp_data <- infews_top %>%
  filter(!is.na(bray_p) & !is.na(total_p))

bray_totalp_cor <- cor.test(bray_totalp_data$bray_p, bray_totalp_data$total_p, method = "spearman")
cat("\nSpearman correlation (bray_p vs total_p):\n")
cat("  rho =", round(bray_totalp_cor$estimate, 3), "\n")
cat("  p =", signif(bray_totalp_cor$p.value, 4), "\n")

bray_totalp_plot <- ggplot(bray_totalp_data, aes(x = bray_p, y = total_p, color = land_use)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = LAND_USE_COLORS, labels = GetLandUseLabels()) +
  theme_bw() +
  labs(
    x = "Bray P (ppm)",
    y = "Total P (ppm)",
    color = "Land Use"
  ) +
  coord_cartesian(ylim = c(0, 2000)) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Spearman rho = ", round(bray_totalp_cor$estimate, 3),
                          "\np = ", signif(bray_totalp_cor$p.value, 4)),
           hjust = 1.1, vjust = 1.5, size = 3.5)

print(bray_totalp_plot)
