# Food Chain Length Global Drivers

## Project Overview

This repository contains the analysis code and theoretical models for investigating **"Climate constrains whether food chain length increases with ecosystem size via body-size effects"**. 

The project integrates empirical data from freshwater ecosystems with theoretical modeling to understand the drivers of food chain length (FCL) across global environmental gradients. The work examines how ecosystem size, productivity (nutrient availability), hydrological disturbance, apex predator body size, and climate zones interact to determine food chain length in lentic (lake/pond) and lotic (river/stream) ecosystems.

## Project Structure

```
FCL_global_drivers/
├── CESAB_FCL_project.Rproj          # R project file
├── README.md                         # This file
├── Global empirical patterns/        # Empirical analysis of FCL drivers
│   ├── data/                         # Processed and raw data
│   │   ├── FCL_dataset.RData         # Food chain length dataset
│   │   ├── Env.RData                 # Environmental predictors
│   │   ├── Predictors_Lotic.RData    # Lotic (river) predictors
│   │   ├── Predictors_Lentic.RData   # Lentic (lake) predictors
│   │   ├── df_fish.RData             # Fish isotope database
│   │   ├── df_fish_length_weights.RData  # Apex predator body sizes
│   │   ├── df_indiv.RData            # Individual-level fish data
│   │   ├── df_rich.RData             # Species richness per site
│   │   ├── complete_df.RData         # Complete filtered dataset
│   │   └── ISOFRESH.xlsx             # Raw isotope data
│   ├── figures/                      # Output figures
│   └── scripts/
│       ├── 0_functions.R             # Utility functions
│       ├── 1_datasets_preparation.R  # Data preparation and cleaning
│       ├── 2_Description of the dataset.R  # Descriptive statistics & Figure 1
│       ├── 3_Analysis_Global_Patterns.R    # Main statistical analyses & figures
│       └── 4_further_checks.R        # Validation checks (FishBase vs measured)
└── Theoretical modelling/            # Theoretical model representation
    ├── data/                         # Model output files
    │   ├── result_BSmax_Temp.txt     # Body size model results
    │   ├── result_Disturb.txt        # Disturbance model results
    │   ├── result_Nut_Temp.txt       # Productivity-temperature model results
    │   └── result_S_Temp.txt         # Richness model results
    ├── figures/                      # Output figures
    └── scripts/
      ├── 1_Theoretical_model_simulations.R      # Model simulations (long runtime; manual execution)
      └── 2_Theoretical_model_representations.R  # Figure generation from model outputs
```

## Dependencies

### System Requirements
- R (>= 4.0)

### R Packages
The project requires the following R packages:

- **Data manipulation**: `dplyr`, `readxl`
- **Visualization**: `ggplot2`, `ggpubr`, `patchwork`, `cowplot`
- **Statistical modeling**: `mgcv` (GAM models), `sjPlot` (model summaries)
- **Data**: `rfishbase` (FishBase API access)
- **Theoretical modeling**: `ATNr`, `deSolve`, `future.apply`, `future`, `doFuture`, `tidyr`
- **Utilities**: `here` (path management), `forcats` (factor handling)

Install all dependencies with:
```r
install.packages(c("ggplot2", "ggpubr", "dplyr", "readxl", "forcats", 
                   "rfishbase", "here", "mgcv", "sjPlot", "patchwork", "cowplot",
                   "ATNr", "deSolve", "future.apply", "future", "doFuture", "tidyr"))
```

## Workflow

### 1. Data Preparation Phase
**Script**: `Global empirical patterns/scripts/1_datasets_preparation.R`

This script:
- Loads FCL computed from species maximum trophic position
- Processes environmental predictors (size, productivity, hydrological disturbance, temperature, precipitation)
- Extracts apex predator body sizes from FishBase
- Calculates species richness per site
- Outputs: `complete_df.RData`, `Env.RData`, `df_fish_length_weights.RData`, `df_rich.RData`

### 2. Descriptive Analysis
**Script**: `Global empirical patterns/scripts/2_Description of the dataset.R`

Creates Figure 1 showing:
- Geographic distribution of study sites
- Ecosystem type comparison (Lentic vs. Lotic)
- Environmental variable distributions
- Climate zone representation
- FCL distribution

### 3. Main Statistical Analyses
**Script**: `Global empirical patterns/scripts/3_Analysis_Global_Patterns.R`

Conducts hypothesis tests:
1. **Ecosystem size × Climate zone interaction**: LM with slopes across climate zones
2. **Energy limitation**: Univariate relationship between productivity (TP) and FCL
3. **Disturbance hypothesis**: Hydrological disturbance effects on FCL
4. **Body size constraints**: Apex predator length effects on FCL and ecosystem size interaction
5. **Richness effects** (supplementary): Species richness as mediator

Outputs: Figure 2 and Figure 3 with hypothesis tests and model summaries

### 4. Validation Checks
**Script**: `Global empirical patterns/scripts/4_further_checks.R`

Validates FishBase data quality:
- Compares measured apex predator sizes in the dataset vs. FishBase reports
- Outputs: SI4 validation figure

### 5. Theoretical Model Simulations (Manual)
**Script**: `Theoretical modelling/scripts/1_Theoretical_model_simulations.R`

Runs the theoretical food web simulations and exports text outputs:
- `result_Nut_Temp.txt`
- `result_BSmax_Temp.txt`
- `result_Disturb.txt`

Note: this step is computationally intensive (hours) and is intentionally not auto-run in the script.

### 6. Theoretical Model Visualizations
**Script**: `Theoretical modelling/scripts/2_Theoretical_model_representations.R`

Generates visualizations of theoretical predictions:
- Effect of productivity and temperature on FCL
- Effect of apex predator body size and temperature on FCL
- Effect of disturbance severity on FCL
- Outputs: Figure 1 (theoretical) with 3 hypothesis panels

## Key Functions

### `decorate_plot()` - Plot Decoration Utility
Located in `0_functions.R`

Adds decorated borders, icons, and titles to ggplot2 plots for publication-quality figures.

**Parameters**:
- `p`: ggplot2 plot object
- `icon_path`: Path to icon image
- `border_col`: Border color
- `top_text`: Title text above border
- Additional styling parameters

## Main Analyses

### Statistical Models

1. **Ecosystem Size Model**
   ```R
   mod_size_2 <- lm(FCL ~ Climate_zone_e2 + Climate_zone_e2:size_z_scored, df2)
   ```
   Tests size effects within each climate zone

2. **Body Size Model**
   ```R
   mod_Lmax <- lm(FCL ~ Lmax, data = df_3)
   ```
   Relationship between apex predator length and FCL

3. **GAM Model for Energy Limitation**
   ```R
   mod_TP_gam <- gam(FCL ~ Climate_zonee3 + s(TP, by=Climate_zonee3, k=3), 
                     data=df2, method="REML")
   ```
   Non-linear productivity effects with climate zone interactions

## Output Figures

### Empirical Analysis
- **Figure 1**: Dataset description and ecosystem characteristics
- **Figure 2**: Main drivers of food chain length
- **Figure 3**: Body-size constraints hypothesis
- **SI2**: Species richness analysis
- **SI4**: FishBase validation

### Theoretical Models
- **Figure 1 (Theory)**: Predicted effects of drivers on FCL

## Data Sources

1. **FCL Data**: Computed from δ15N isotope data (ISOFRESH database, Bouletreau et al. 2025)
2. **Environmental Data**: Climate and hydrological data from global datasets
3. **Fish Body Size Data**: FishBase database (rfishbase package)
4. **Individual Fish Data**: ISOFRESH database (Excel file)

**References**:
- Bouletreau et al. (2025). ISOFRESH: An isotopic database for freshwater ecosystems. https://doi.org/10.1051/kmae/2025010
- Perga et al. (2025). Food web baseline computation. https://doi.org/10.5281/zenodo.17718458

## Running the Analysis

### To reproduce all analyses:

1. Set working directory to project root (open `.Rproj` file in RStudio)
2. Run scripts in order:
   ```R
   # From Global empirical patterns/scripts/
   source("1_datasets_preparation.R")     # Prepare data
   source("2_Description of the dataset.R") # Descriptive stats
   source("3_Analysis_Global_Patterns.R")    # Main analyses
   source("4_further_checks.R")           # Validation
   
   # From Theoretical modelling/scripts/
   # source("1_Theoretical_model_simulations.R") # Optional: long runtime simulation step
   source("2_Theoretical_model_representations.R") # Theory figures
   ```

### Using the `here` package

All file paths use the `here()` function for cross-platform compatibility. The project will work regardless of working directory.

## Climate Classifications

The project uses an 18-class climate classification system, grouped into 6 broader categories:

- **Cold and wet/mesic**: Cold, wet conditions
- **Cool and moist**: Moderate climate with good moisture
- **Cool temperate and dry/xeric**: Cool with low precipitation
- **Warm temperate**: Moderate warmth with temperate conditions
- **Hot and moist**: High temperature with moisture
- **Hot and dry**: Hot and arid conditions

## Author

**Marie-Elodie Perga**  
*marie-elodie.perga@unil.ch*

University of Lausanne (UNIL)

## Citation

If you use this code or data in your research, please cite:

```
Perga, M.-E., et al. (2026). Climate constrains whether food chain length 
increases with ecosystem size via body-size effects. [Journal]. 
DOI: [To be added]
```

## Version History

- **v3.0** (April 2026): Latest version with all analyses

## License

[Specify appropriate license - e.g., MIT, CC-BY-4.0, etc.]

## Notes

- All paths are relative using the `here` package for reproducibility
- Models are run with REML estimation (GAMs) or ordinary least squares (LMs)
- Climate zone coding: Refer to 3_Analysis_Global_Patterns.R for recoding schema
- FishBase access requires internet connection (rfishbase package)

## Troubleshooting

### Missing packages
If you encounter missing package errors, reinstall all dependencies:
```r
install.packages(c("ggplot2", "ggpubr", "dplyr", "readxl", "forcats", 
                   "rfishbase", "here", "mgcv", "sjPlot", "patchwork", "cowplot",
                   "ATNr", "deSolve", "future.apply", "future", "doFuture", "tidyr"))
```

### FishBase queries fail
- Ensure internet connection is active
- The rfishbase package may require updating
- Update with: `devtools::install_github("ropensci/rfishbase")`

### Path issues
- Ensure the project is opened via the `.Rproj` file
- All scripts use `here()` for automatic path resolution

---

*Last updated: April 28, 2026*
