# M061 Pearson INFEWS Urban Soil Cores Project

## Project Overview
This is an urban soil cores analysis project studying carbon, lead, and phosphorus in Minneapolis-St. Paul metropolitan area soils as part of the INFEWS (Innovations at the Nexus of Food, Energy, and Water Systems) initiative.

## Directory Structure Summary

### Data Storage (`00-data/`)
- **`a-raw/`**: Raw datasets including soil core data, site information, and spatial boundaries
  - Core sample data from December 2022
  - Spatial shapefiles for Twin Cities metro area boundaries and land cover
  - Ashley's core data and surface transects
- **`b-prepared/`**: Processed/cleaned datasets ready for analysis
  - Combined INFEWS dataset (CSV and shapefile formats)
  - Clipped land cover rasters

### Code (`01-code/`)
- **`a-Rscripts/`**: Primary analysis code location - **THIS IS WHERE YOUR CODE LIVES**
  - `000-master.R`: Master script - executes workspace setup and data loading
  - `00-prepare-workspace.R`: Workspace setup and package loading
  - `01-load-in-and-inspect-data.R`: Data loading and initial inspection
  - `02-data-munging.R`: Data cleaning, bulk density modeling, and dataset preparation
  - `03-data-analysis.R`: Statistical analysis (generic Kruskal-Wallis/Dunn functions)
  - `04-stock-calculations.R`: Carbon/lead/phosphorus stock calculations + depth-stratified analysis
  - `05-output-figures.R`: Figure generation (maps, boxplots, depth profiles)
  - `06-output-tables.R`: Publication-ready summary table generation
  - `000-fig-for-lindsey.R`: Ad-hoc stakeholder figure (2022 parks total carbon)
  - `000 connect to google sheets.R`: Original exploratory Google Sheets script (reference only)
- **`b-markdowns/`**: Placeholder for markdown documentation

### Outputs (`02-output/`)
- **`a-figures/`**: Generated figures for publication (carbon stocks, land use maps, depth profiles)
- **`b-tables/`**: Generated summary tables and statistics

### Support Files (`03-support-files/`)
- **`archive/`**: Archived reference materials
- **`cache/`**: Cached computational results
- **`logs/`**: Project development logs and notes
- **`temp/`**: Temporary files

### Manuscript (`04-manuscript/`)
- Organized subfolders for drafts, submissions, and final versions (currently empty placeholders; manuscript stored in Obsidian while in development)

### Other Directories
- **`05-metadata/`**: Project metadata (placeholder)
- **`06-presentations/`**: Conference presentations and workshop materials
- **`docs/`**: Documentation placeholder

## Key Files
- **`M061-pearson-infews-urban-soil-cores.Rproj`**: R project file
- **`README.md`**: Detailed project template documentation
- **`LICENSE`**: MIT license

## Main Analysis Workflow
The R scripts in `01-code/a-Rscripts/` follow a streamlined sequential pipeline:

### **Core Scripts (Sequential Execution)**
1. **`000-master.R`**: Simple master script - executes workspace setup and data loading
2. **`00-prepare-workspace.R`**: Workspace setup and package loading
3. **`01-load-in-and-inspect-data.R`**: Data loading and initial inspection
4. **`02-data-munging.R`**: Data cleaning, bulk density modeling, and dataset preparation
5. **`03-data-analysis.R`**: **REFACTORED** - Generic statistical analysis functions
6. **`04-stock-calculations.R`**: Carbon/lead/phosphorus stock calculations  
7. **`05-output-figures.R`**: **REFACTORED** - Automated figure generation with reusable functions
8. **`06-output-tables.R`**: **REFACTORED** - Automated publication-ready table generation

### **Archived Scripts** (`x` prefix)
- **`x07-kw-dunn-test-ARCHIVED.R`**: Replaced by generic functions in `03-data-analysis.R`
- **`x000-bd-model-testing.R`**: Original bulk density model development code
- **`x000 connect to google sheets.R`**: Early Google Sheets connection script
- **`x00 read in data and exploratory analysis.R`**: Initial exploratory analysis

## Project Type
This is an R-based soil science research project analyzing urban soil properties with spatial analysis components, following the Jelinski Lab standardized project template structure.

## Package Dependencies
`googlesheets4`, `tibble`, `dplyr`, `here`, `terra`, `ggplot2`, `patchwork`, `gt`, `webshot2`, `ggmap`, `dunn.test`, `tidyr`

**Note:** Map generation requires a Stadia Maps API key (`register_stadiamaps()`).

## High-Level Summary of Codebase Functionality

### **Core Analysis Pipeline**
The codebase follows a well-structured sequential workflow analyzing urban soil properties across different land uses in the Twin Cities metropolitan area:

**Data Sources:**
- Google Sheets integration for live data access
- Soil core samples from 2021-2022 across 5 land use categories:
  - Garden food producing (in-ground & raised beds)
  - Garden non-food producing  
  - Maintained lawn
  - Natural vegetation

### **Key Analysis Components**

**1. Data Pipeline (`02-data-munging.R`)**
- **Bulk density modeling**: Non-linear exponential model to predict missing bulk density values from total carbon
- **Data integration**: Combines sample-level and site-level data via unique plot codes
- **Quality control**: Filters outliers and standardizes land use categories
- **Subset creation**: Creates analysis-ready datasets for each soil property
  - `infews_top`: Surface samples (0-10cm) for concentration analyses
  - `infews_top20`: Samples from 0-20cm depth (0-10cm and 10-20cm increments)
  - `infews_top20_avg`: Averaged lab values across 0-20cm zone (mean of 0-10cm and 10-20cm samples), one row per plot for all soil properties. *Created but not yet used in downstream analyses.*

**2. Statistical Analysis (`03-data-analysis.R`) - REFACTORED**
- **Generic functions**: `PerformKruskalWallis()` handles all non-parametric analyses uniformly
- **Automated workflow**: Single function call generates analysis, plots, and statistical summaries
- **Non-parametric approach**: Kruskal-Wallis tests with Dunn's post-hoc tests (Benjamini-Hochberg correction)
- **Variables analyzed**: Total carbon, lead, pH, Bray P, total P, saturated hydraulic conductivity, bulk density
- **Focus**: Surface soil (0-10cm) differences across land use types
- **Standardized outputs**: Consistent formatting, significance labels, and summary tables

**3. Stock Calculations (`04-stock-calculations.R`)**
- **Full-profile stocks**: Calculates soil organic carbon, lead, and phosphorus stocks for entire 1m profiles
  - Carbon stocks (kgC/m²)
  - Lead stocks (kgPb/m²)
  - Phosphorus stocks (kgP/m²)
- **Depth-stratified carbon analysis**: **NEW** - Compares carbon stocks in top 20cm vs. bottom 80cm within each land use
  - Calculates horizon-level carbon stocks from `infews_comp`
  - Aggregates to depth zones: 0-20cm and 20-100cm per core
  - **Wilcoxon signed-rank tests** (paired, non-parametric) for each land use class
  - Tests whether carbon storage differs significantly between depth zones
  - **Key finding**: Most land uses (in-ground gardens, lawns, natural vegetation) have significantly MORE carbon in bottom 80cm than top 20cm
  - Dataset: `c_stock_depth_zones` contains paired top/bottom stocks with site metadata
  - Summary table: `wilcox_summary_table` with test statistics and p-values

**4. Visualization (`05-output-figures.R`) - REFACTORED**
- **Modular functions**: `CreateSiteMap()`, `CreateStockBoxplot()`, `CreateMultiStockFigure()`, `CreateDepthProfileFigure()`
- **Standardized styling**: Centralized color schemes (`SITE_TYPE_COLORS`, `LAND_USE_COLORS`, `LAND_USE_CODES`), map config (`MAP_CONFIG`), and labels
- **Automated generation**: Single function calls create publication-ready figures
- **Multiple formats**: Spatial maps, individual/multi-panel boxplots, depth profiles
- **Version control**: Date-stamped filenames (newer figures use dynamic `format(Sys.Date())`, older ones have hardcoded dates)
- **Depth-stratified carbon figures**: Complete with significance labels
  - **Grouped boxplot**: Side-by-side comparison of depth zones by land use with Wilcoxon significance letters
  - **Paired line plot**: Individual cores as connected lines, faceted by land use, with median overlay
  - Uses `tidyr::pivot_longer()` for data reshaping

**5. Summary Statistics (`06-output-tables.R`) - REFACTORED**
- **Generic function**: `CalculateSoilStats()` handles all soil properties uniformly
- **Comprehensive automation**: Single script generates complete summary table
- **Publication format**: GT package tables with sample sizes, formatting, and footnotes
- **Standardized metrics**: Mean ± SE (median) format across all variables

### **Technical Strengths**
- **Reproducible workflow**: Sequential numbered scripts with clear dependencies
- **Modular design**: Generic functions eliminate code duplication and ensure consistency
- **Robust statistics**: Non-parametric approaches with proper multiple comparison corrections
- **Quality control**: Outlier detection, missing value handling, model validation
- **Professional outputs**: Publication-quality figures and tables with automated generation
- **Spatial integration**: GIS-ready shapefiles and coordinate data

### **Current State (Post-Refactoring)**
The codebase has been significantly **streamlined and modularized** with generic functions that eliminate code duplication. The refactored workflow now provides:
- **Single-function execution** for complex analyses and figure generation
- **Standardized formatting** across all outputs
- **Automated publication-ready** tables and figures
- **Improved maintainability** through centralized styling and configuration

The workflow successfully processes raw soil data through to publication-ready outputs analyzing urban soil quality patterns across different land management practices, now with greatly improved code efficiency and consistency.

### **Recent Updates (as of December 2025)**

**Completed: Depth-Stratified Carbon Stocks Analysis**
- Added depth-zone comparison analysis to investigate carbon distribution in soil profiles
- **Research question**: Is there significantly more carbon in the top 20cm compared to the rest of the profile?
- **Methodology**:
  - Paired Wilcoxon signed-rank tests (non-parametric, within-land-use comparisons)
  - Wilcoxon chosen over paired t-test based on Shapiro-Wilk normality tests showing significant departures from normality in 2 of 5 land use groups (in-ground gardens: p<0.001; lawns: p<0.001)
  - Diagnostic tests included at end of `04-stock-calculations.R` to justify test choice
  - Compares carbon stocks (kgC/m²) in 0-20cm vs. 20-100cm depth zones
  - Paired design: same cores contribute measurements from both depth zones
- **Key datasets created**:
  - `infews_top20` in `02-data-munging.R`: Samples from 0-10cm and 10-20cm depths
  - `infews_top20_avg` in `02-data-munging.R`: Averaged soil properties for 0-20cm zone
  - `c_stock_depth_zones` in `04-stock-calculations.R`: Paired carbon stocks by depth zone
  - `wilcox_summary_table` in `04-stock-calculations.R`: Statistical test results
- **Key findings**: Three land uses (in-ground gardens, lawns, natural vegetation) have significantly MORE carbon in bottom 80cm than top 20cm (counterintuitive result)
- **Visualization**: Two figure types completed in `05-output-figures.R`
  - Grouped boxplots with significance labels showing depth zone comparisons
  - Paired line plots showing individual core trajectories
- **Status**: Complete and publication-ready

**Bulk Density Model Documentation (December 2024)**
- Added diagnostic code to `02-data-munging.R` to document bulk density model performance
- **Code additions** (lines 65-80):
  - Model summary statistics including parameter estimates and residual standard error
  - R² calculation (0.025) showing model explains limited variance across all samples
  - Quantification of predicted BD usage: 723 samples (31.8%) use predicted values
    - 525 from 2021 (all 2021 samples forced to use predictions due to systematic measurement errors)
    - 198 from other years with invalid BD (0 or ≥2.6 g/cm³)
- **Key justification**: Despite low overall R², model successfully maintains physically realistic depth trends in 2021 cores where measured values showed impossible patterns (e.g., BD decreasing with depth while carbon also decreased)
- Model preserves fundamental inverse carbon-BD relationship, ensuring realistic depth profiles for carbon stock calculations