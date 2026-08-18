# Load in raw sample and site data csvs
infews_raw <- read.csv(here::here("00-data", "a-raw", "infews-raw.csv"))
infews_site_raw <- read.csv(here::here("00-data", "a-raw", "infews-site-raw.csv"))

# Inspect structure
str(infews_raw)
str(infews_site_raw)

# Inspect unique values
unique(infews_raw$top)
unique(infews_raw$inorganic_carbon)
unique(infews_raw$pH)
unique(infews_raw$bray_p)