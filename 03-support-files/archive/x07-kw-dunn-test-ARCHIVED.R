# Load required packages
library(dplyr)
library(ggplot2)
library(dunn.test)  # For Dunn's test

# Carbon ------------------------------------------------------------------

## Plot carbon data
infews.carbon.top <- filter(infews.carbon, top == 0)
boxplot(infews.carbon.top$total_carbon ~ infews.carbon.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.carbon.top$total_carbon ~ infews.carbon.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
carbon.kw <- kruskal.test(total_carbon ~ land_use, data = infews.carbon.top)
print("=== Carbon Kruskal-Wallis Test ===")
print(carbon.kw)

## Post-hoc analysis: Dunn's test with Benjamini-Hochberg adjustment
dunn_carbon <- dunn.test(infews.carbon.top$total_carbon, 
                         infews.carbon.top$land_use, 
                         method = "bh",  # Benjamini-Hochberg correction
                         alpha = 0.05)

## Calculate median and IQR for each group (non-parametric equivalents)
carbon_summary <- infews.carbon.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(total_carbon, na.rm = TRUE),
    Q1 = quantile(total_carbon, 0.25, na.rm = TRUE),
    Q3 = quantile(total_carbon, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== Carbon Group Medians ===")
print(carbon_summary)

## Plot results (using medians instead of means for non-parametric)
ggplot(infews.carbon.top[!is.na(infews.carbon.top$land_use), ], 
       aes(x = total_carbon, y = land_use)) +
  geom_boxplot() +
  geom_text(data = carbon_summary, 
            aes(x = median + 16, y = land_use), 
            label = c("A", "A", "B", "B", "B"),
            size = 4,
            fontface = "bold") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "Total Carbon (%)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# Lead --------------------------------------------------------------------

## Plot lead data
infews.lead.top <- filter(infews.lead, top == 0)
boxplot(infews.lead.top$lead ~ infews.lead.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.lead.top$lead ~ infews.lead.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
lead.kw <- kruskal.test(lead ~ land_use, data = infews.lead.top)
print("=== Lead Kruskal-Wallis Test ===")
print(lead.kw)

## Post-hoc analysis: Dunn's test
dunn_lead <- dunn.test(infews.lead.top$lead, 
                       infews.lead.top$land_use, 
                       method = "bh",
                       alpha = 0.05)

## Calculate median and IQR for each group
lead_summary <- infews.lead.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(lead, na.rm = TRUE),
    Q1 = quantile(lead, 0.25, na.rm = TRUE),
    Q3 = quantile(lead, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== Lead Group Medians ===")
print(lead_summary)

## Plot results
ggplot(infews.lead.top[!is.na(infews.lead.top$land_use), ], 
       aes(x = lead, y = land_use)) +
  geom_boxplot() +
  geom_text(data = lead_summary, 
            aes(x = median + 850, y = land_use), 
            label = c("B", "A", "A", "B", "B"),  # Update based on Dunn results
            size = 4,
            fontface = "bold") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "Lead (ppm)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# pH ----------------------------------------------------------------------

## Plot pH data
infews.pH.top <- filter(infews.pH, top == 0)
boxplot(infews.pH.top$pH ~ infews.pH.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.pH.top$pH ~ infews.pH.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
pH.kw <- kruskal.test(pH ~ land_use, data = infews.pH.top)
print("=== pH Kruskal-Wallis Test ===")
print(pH.kw)

## Post-hoc analysis: Dunn's test
dunn_pH <- dunn.test(infews.pH.top$pH, 
                     infews.pH.top$land_use, 
                     method = "bh",
                     alpha = 0.05)

## Calculate median and IQR for each group
pH_summary <- infews.pH.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(pH, na.rm = TRUE),
    Q1 = quantile(pH, 0.25, na.rm = TRUE),
    Q3 = quantile(pH, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== pH Group Medians ===")
print(pH_summary)

## Plot results
ggplot(infews.pH.top[!is.na(infews.pH.top$land_use), ], 
       aes(x = pH, y = land_use)) +
  geom_boxplot() +
  geom_text(data = pH_summary, 
            aes(x = median + 1.5, y = land_use), 
            label = c("A", "B", "B", "B", "B"),  # Update based on Dunn results
            size = 4,
            fontface = "bold") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "pH (1:1 H2O)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# Bray phosphorus ---------------------------------------------------------

## Plot bray phosphorous data
infews.bray.p.top <- filter(infews.bray.p, top == 0)
boxplot(infews.bray.p.top$bray_p ~ infews.bray.p.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.bray.p.top$bray_p ~ infews.bray.p.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
bray.p.kw <- kruskal.test(bray_p ~ land_use, data = infews.bray.p.top)
print("=== Bray Phosphorus Kruskal-Wallis Test ===")
print(bray.p.kw)

## Post-hoc analysis: Dunn's test
dunn_brayp <- dunn.test(infews.bray.p.top$bray_p, 
                        infews.bray.p.top$land_use, 
                        method = "bh",
                        alpha = 0.05)

## Calculate median and IQR for each group
brayp_summary <- infews.bray.p.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(bray_p, na.rm = TRUE),
    Q1 = quantile(bray_p, 0.25, na.rm = TRUE),
    Q3 = quantile(bray_p, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== Bray Phosphorus Group Medians ===")
print(brayp_summary)

## Plot results
ggplot(infews.bray.p.top[!is.na(infews.bray.p.top$land_use), ], 
       aes(x = bray_p, y = land_use)) +
  geom_boxplot() +
  geom_text(data = brayp_summary, 
            aes(x = median + 450, y = land_use), 
            label = c("A", "A", "B", "B", "B"),  # Update based on Dunn results
            size = 4,
            fontface = "bold") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "Bray Phosphorus (ppm)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# Total Phosphorus --------------------------------------------------------

## Plot total phosphorous data
infews.phosphorus.top <- filter(infews.phosphorus, top == 0)
boxplot(infews.phosphorus.top$total_p ~ infews.phosphorus.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.phosphorus.top$total_p ~ infews.phosphorus.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
phosphorus.kw <- kruskal.test(total_p ~ land_use, data = infews.phosphorus.top)
print("=== Total Phosphorus Kruskal-Wallis Test ===")
print(phosphorus.kw)

## Post-hoc analysis: Dunn's test
dunn_phosphorus <- dunn.test(infews.phosphorus.top$total_p, 
                             infews.phosphorus.top$land_use, 
                             method = "bh",
                             alpha = 0.05)

## Calculate median and IQR for each group
phosphorus_summary <- infews.phosphorus.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(total_p, na.rm = TRUE),
    Q1 = quantile(total_p, 0.25, na.rm = TRUE),
    Q3 = quantile(total_p, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== Total Phosphorus Group Medians ===")
print(phosphorus_summary)

## Plot results
ggplot(infews.phosphorus.top[!is.na(infews.phosphorus.top$land_use), ], 
       aes(x = total_p, y = land_use)) +
  geom_boxplot() +
  geom_text(data = phosphorus_summary, 
            aes(x = median + 1200, y = land_use), 
            label = c("A", "A", "B", "B", "B"),  # Update based on Dunn results
            size = 4,
            fontface = "bold") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "Total Phosphorus (ppm)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# Saturated hydraulic conductivity (Ksat) ---------------------------------

## Plot Ksat data
infews.ksat.top <- filter(infews.ksat, top == 0)
boxplot(infews.ksat.top$ksat_avg ~ infews.ksat.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.ksat.top$ksat_avg ~ infews.ksat.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
ksat.kw <- kruskal.test(ksat_avg ~ land_use, data = infews.ksat.top)
print("=== Ksat Kruskal-Wallis Test ===")
print(ksat.kw)

## Post-hoc analysis: Dunn's test
dunn_ksat <- dunn.test(infews.ksat.top$ksat_avg, 
                       infews.ksat.top$land_use, 
                       method = "bh",
                       alpha = 0.05)

## Calculate median and IQR for each group
ksat_summary <- infews.ksat.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(ksat_avg, na.rm = TRUE),
    Q1 = quantile(ksat_avg, 0.25, na.rm = TRUE),
    Q3 = quantile(ksat_avg, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== Ksat Group Medians ===")
print(ksat_summary)

## Plot results
ggplot(infews.ksat.top[!is.na(infews.ksat.top$land_use), ], 
       aes(x = ksat_avg, y = land_use)) +
  geom_boxplot() +
  geom_text(data = ksat_summary, 
            aes(x = median + 72, y = land_use), 
            label = c("B", "A", "B", "B", "B"),  # Update based on Dunn results
            size = 4,
            fontface = "bold") +
  xlim(0, 100) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "Ksat (cm hr-1)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# Bulk density ------------------------------------------------------------

## Plot bulk density data
infews.bd.top <- filter(infews_comp, top == 0)
boxplot(infews.bd.top$bd_comp ~ infews.bd.top$site_type, horizontal = T, las = 2, cex.axis = 0.7)
boxplot(infews.bd.top$bd_comp ~ infews.bd.top$land_use, horizontal = T, las = 2, cex.axis = 0.7)

## Perform Kruskal-Wallis test
bd.kw <- kruskal.test(bd_comp ~ land_use, data = infews.bd.top)
print("=== Bulk Density Kruskal-Wallis Test ===")
print(bd.kw)

## Post-hoc analysis: Dunn's test
dunn_bd <- dunn.test(infews.bd.top$bd_comp, 
                     infews.bd.top$land_use, 
                     method = "bh",
                     alpha = 0.05)

## Calculate median and IQR for each group
bd_summary <- infews.bd.top %>%
  group_by(land_use) %>%
  summarise(
    median = median(bd_comp, na.rm = TRUE),
    Q1 = quantile(bd_comp, 0.25, na.rm = TRUE),
    Q3 = quantile(bd_comp, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    n = n()
  ) %>%
  filter(!is.na(land_use))

print("=== Bulk Density Group Medians ===")
print(bd_summary)

## Plot results
ggplot(infews.bd.top[!is.na(infews.bd.top$land_use), ], 
       aes(x = bd_comp, y = land_use)) +
  geom_boxplot() +
  geom_text(data = bd_summary, 
            aes(x = median + 1.5, y = land_use), 
            label = c("B", "B", "A", "B", "B"),  # Update based on Dunn results
            size = 4,
            fontface = "bold") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(y = "Land Use", x = "Bulk Density (g cm-3)") +
  scale_y_discrete(labels = c("Garden_food_producing_in_ground" = "Garden Food Producing In Ground",
                              "Garden_food_producing_raised_beds" = "Garden Food Producing Raised Beds", 
                              "Garden_non_food_producing" = "Garden Non-Food Producing",
                              "Maintained_lawn" = "Maintained Lawn",
                              "Natural_vegetation" = "Natural Vegetation"))

# Optional: Create a summary table of all test results -------------------

## Function to extract p-values and test statistics
extract_kw_results <- function(kw_test, test_name) {
  data.frame(
    Variable = test_name,
    Chi_squared = kw_test$statistic,
    df = kw_test$parameter,
    p_value = kw_test$p.value,
    Significant = ifelse(kw_test$p.value < 0.05, "Yes", "No")
  )
}

## Combine all results
all_kw_results <- rbind(
  extract_kw_results(carbon.kw, "Total Carbon"),
  extract_kw_results(lead.kw, "Lead"),
  extract_kw_results(pH.kw, "pH"),
  extract_kw_results(bray.p.kw, "Bray Phosphorus"),
  extract_kw_results(phosphorus.kw, "Total Phosphorus"),
  extract_kw_results(ksat.kw, "Ksat"),
  extract_kw_results(bd.kw, "Bulk Density")
)

print("=== Summary of All Kruskal-Wallis Tests ===")
print(all_kw_results)