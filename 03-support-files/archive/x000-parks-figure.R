# Creating a plot of parks/green spaces sampled in 2022 and their 0-10cm total carbon content for a collaborator to report to parks

# Load required packages

library(tibble)
library(dplyr)
library(here)
library(terra)

## Load in INFEWS raw sample and site data 
infews_raw = read.csv(here::here("00-data", "a-raw", "infews-raw.csv"))
infews_site_raw = read.csv(here::here("00-data", "a-raw", "infews-site-raw.csv"))

# Add a unique numeric code to every plot (site)

## Make a sample code so we can combine the site and sample data sheets (8 digits: year, site, plot)
infews_site$code <- paste(infews_site$Year, infews_site$Site, infews_site$Plot, sep = "")
infews$code <- paste(infews$Year, infews$Site, infews$Plot, sep = "")

## Combine sample and site sheets via left join
infews.comp <- left_join(infews, infews_site, by = c("code" = "code"))

# Filter out 0-10cm data
infews.top <- filter(infews.comp, Top == 0)

# Remove all but 2022 samples
subset1 <- filter(infews.top, Year.x != 2018)
subset2 <- filter(subset1, Year.x != 2019)
subset3 <- filter(subset2, Year.x != 2020)
subset4 <- filter(subset3, Year.x != 2021)

unique(subset4$Year.x)

# Remove but all Parks/Green Spaces for site type
subset5 <- filter(subset4, Site_Type == "Park/Green Space")

# Note: script originally trimmed out certain parks by cite name; that was removed prior to publishing.

# Plot 2022 parks/green space NAMES against total carbon
boxplot(subset5$Total_Carbon ~ subset5$Location)

# Format and export figure

jpeg("2022 total carbon in parks 19JAN2024.jpg",
     width = 8600,
     height = 5600,
     res = 800,
     pointsize = 8)

#layout.matrix <- matrix(c(1,2,3), nrow = 1, ncol = 3)
#layout(mat = layout.matrix)
## layout.show(1,2,3)
#par(mar = c(5, 4.5, 20, 1)
#lwd = 2 # increase the line thickness
#cex.axis = 1.2 # increase default axis label size
#)


boxplot(subset13$Total_Carbon ~ subset13$Location)

dev.off()


