# Contributing to FCL Global Drivers

Thank you for your interest in contributing to this project! This document provides guidelines and instructions for contributing.

## Code of Conduct

Be respectful and constructive in all interactions. We aim to maintain a welcoming and inclusive community.

## How to Contribute

### Reporting Issues

If you find a bug or have a suggestion:

1. Check existing [Issues](../../issues) to avoid duplicates
2. Provide a clear description of the problem or suggestion
3. Include:
   - R version and package versions
   - Steps to reproduce (if applicable)
   - Expected vs. actual behavior
   - Any error messages or screenshots

### Submitting Changes

1. **Fork the repository** and create a feature branch
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the style guide (see below)

3. **Test your changes**
   - Run all affected scripts
   - Verify outputs match expected results
   - Check for any new warnings or errors

4. **Commit with clear messages**
   ```bash
   git commit -m "Brief description of changes"
   ```

5. **Push to your fork** and submit a Pull Request
   ```bash
   git push origin feature/your-feature-name
   ```

6. **Provide a clear PR description** including:
   - What changes were made
   - Why the changes were necessary
   - Any related issues

## Style Guide

### R Code Style

- **Indentation**: 2 spaces (not tabs)
- **Line length**: Maximum 80 characters
- **Variable names**: Use snake_case for functions and variables
- **Comments**: Use `##` for section headers, `#` for inline comments
- **Functions**: Include documentation comments
- **Packages**: Load with `library()` at script top, use `here()` for paths

### Example:
```R
## Load data -------
load(here("data", "my_dataset.RData"))

## Process data -------
# Filter for valid entries
df_clean <- df %>%
  filter(is.finite(value)) %>%
  mutate(log_value = log(value))
```

### File Naming

- **Scripts**: `#_brief_description.R` (e.g., `1_datasets_preparation.R`)
- **Data files**: `descriptive_name.RData` (e.g., `FCL_dataset.RData`)
- **Figures**: `figure_descriptive_name.png` (e.g., `Figure_1_drivers.png`)

## Development Workflow

### Setting Up Your Environment

1. Fork and clone the repository
2. Open `CESAB_FCL_project.Rproj` in RStudio
3. Install dependencies:
   ```R
   install.packages(c("ggplot2", "ggpubr", "dplyr", "readxl", "forcats", 
                      "rfishbase", "here", "mgcv", "sjPlot", "patchwork", "cowplot"))
   ```

### Using Docker (Optional)

Build the Docker image:
```bash
docker build -t fcl_drivers .
```

Run analysis in container:
```bash
docker run -v $(pwd):/workspace fcl_drivers Rscript "Global empirical patterns/scripts/1_datasets_preparation.R"
```

## Documentation

- **README.md**: Overview and setup instructions
- **Script headers**: Include title, date, version, dependencies, author
- **Inline comments**: Explain non-obvious logic
- **Function documentation**: Use roxygen-style comments for complex functions

Example script header:
```R
################################################################################
# Script Title: Brief description
# Description:  Longer explanation of what the script does
# Date:         YYYY-MM-DD
# Version:      X.X
# Dependencies: pkg1, pkg2, pkg3
# Author:       Your Name
################################################################################
```

## Version Control Guidelines

- **Commits**: Use clear, concise messages (max 50 characters)
- **Branches**: Use descriptive names (`feature/new-analysis`, `fix/path-issue`)
- **Pull Requests**: Reference related issues using `#issue-number`

## Data Management

- **Large files**: Use `.gitignore` to exclude data files (already configured)
- **Reproducibility**: All paths use `here()` package
- **Documentation**: Explain data sources and processing steps

## Testing

Before submitting, verify:
- [ ] Scripts run without errors
- [ ] All `here()` paths resolve correctly
- [ ] No hardcoded paths remain
- [ ] Output files are generated as expected
- [ ] Code follows style guide
- [ ] Comments are clear and helpful

## Questions?

- Open an issue with the question tag
- Check existing documentation in README.md
- Contact: Marie-Elodie Perga (marie-elodie.perga@unil.ch)

## Recognition

All contributors will be acknowledged in the project. Significant contributions may be included in publication acknowledgments.

Thank you for contributing! 🎉
