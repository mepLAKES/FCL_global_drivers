# Docker Setup Guide

This guide helps you run the FCL Global Drivers analysis in a Docker container, ensuring reproducibility across different systems.

## Prerequisites

- Docker installed ([Get Docker](https://docs.docker.com/get-docker/))
- Docker Compose (optional, but recommended)
- Git

## Quick Start

### Using Docker Compose (Recommended)

1. **Clone the repository**
   ```bash
   git clone https://github.com/mepLAKES/FCL_global_drivers.git
   cd FCL_global_drivers
   ```

2. **Build and run the container**
   ```bash
   docker-compose up -d fcl-analysis
   docker-compose exec fcl-analysis /bin/bash
   ```

3. **Run analysis scripts inside the container**
   ```R
   # Inside the container's R session
   source("Global empirical patterns/scripts/1_datasets_preparation.R")
   source("Global empirical patterns/scripts/2_Description of the dataset.R")
   source("Global empirical patterns/scripts/3_Analysis_Global_Patterns.R")
   ```

### Using Docker CLI

1. **Build the image**
   ```bash
   docker build -t fcl-drivers:latest .
   ```

2. **Run the container interactively**
   ```bash
   docker run -it -v $(pwd):/workspace fcl-drivers:latest
   ```

3. **Run a specific script**
   ```bash
   docker run -v $(pwd):/workspace fcl-drivers:latest \
     Rscript "Global empirical patterns/scripts/1_datasets_preparation.R"
   ```

## Running Individual Analysis Steps

### Step 1: Data Preparation
```bash
docker run -v $(pwd):/workspace fcl-drivers:latest \
  Rscript "Global empirical patterns/scripts/1_datasets_preparation.R"
```

### Step 2: Descriptive Statistics
```bash
docker run -v $(pwd):/workspace fcl-drivers:latest \
  Rscript "Global empirical patterns/scripts/2_Description of the dataset.R"
```

### Step 3: Statistical Analysis
```bash
docker run -v $(pwd):/workspace fcl-drivers:latest \
  Rscript "Global empirical patterns/scripts/3_Analysis_Global_Patterns.R"
```

### Step 4: Validation Checks
```bash
docker run -v $(pwd):/workspace fcl-drivers:latest \
  Rscript "Global empirical patterns/scripts/4_further_checks.R"
```

### Step 5: Theoretical Model Figures
```bash
docker run -v $(pwd):/workspace fcl-drivers:latest \
  Rscript "Theoretical modelling/scripts/2_Theoretical_model_representations.R"
```

## Interactive R Session

For interactive exploration and development:

```bash
docker run -it -v $(pwd):/workspace fcl-drivers:latest R --vanilla
```

Then in R:
```R
setwd("/workspace")
# Your R commands here
```

## Environment Variables

Override default behavior with environment variables:

```bash
docker run \
  -e R_LIBS_USER=/workspace/.libPaths \
  -v $(pwd):/workspace \
  fcl-drivers:latest
```

## Mounting Additional Volumes

To access data from other locations:

```bash
docker run -it \
  -v $(pwd):/workspace \
  -v /path/to/external/data:/data \
  fcl-drivers:latest
```

## Troubleshooting

### Container won't start
- Ensure Docker is running: `docker ps`
- Check Docker daemon: `docker --version`
- Rebuild image: `docker build --no-cache -t fcl-drivers:latest .`

### Permission errors
- Use `sudo` if needed: `sudo docker run ...`
- Or add user to docker group: `sudo usermod -aG docker $USER`

### Running out of disk space
- Clean up old images: `docker image prune`
- Remove stopped containers: `docker container prune`

### Plots not generating
- Ensure `/workspace` volume is mounted correctly
- Check file permissions: `ls -la Global\ empirical\ patterns/figures/`

### Package installation fails
- Rebuild image without cache: `docker build --no-cache -t fcl-drivers:latest .`
- Check internet connection inside container: `docker run fcl-drivers:latest ping 8.8.8.8`

## Optimization

### Reduce image size
```bash
docker build --target production -t fcl-drivers:slim .
```

### Speed up builds (use cache)
```bash
docker build -t fcl-drivers:latest .
# Subsequent builds reuse layers
```

## Advanced Usage

### Run all analyses in sequence
Create a script `run_all_analyses.sh`:
```bash
#!/bin/bash
docker run -v $(pwd):/workspace fcl-drivers:latest Rscript \
  "Global empirical patterns/scripts/1_datasets_preparation.R"
docker run -v $(pwd):/workspace fcl-drivers:latest Rscript \
  "Global empirical patterns/scripts/2_Description of the dataset.R"
docker run -v $(pwd):/workspace fcl-drivers:latest Rscript \
  "Global empirical patterns/scripts/3_Analysis_Global_Patterns.R"
docker run -v $(pwd):/workspace fcl-drivers:latest Rscript \
  "Global empirical patterns/scripts/4_further_checks.R"
docker run -v $(pwd):/workspace fcl-drivers:latest Rscript \
  "Theoretical modelling/scripts/2_Theoretical_model_representations.R"
```

Then run:
```bash
chmod +x run_all_analyses.sh
./run_all_analyses.sh
```

### Custom Dockerfile for specific needs
Extend the provided Dockerfile:
```dockerfile
FROM fcl-drivers:latest

# Add additional packages or configurations
RUN apt-get update && apt-get install -y additional-package

RUN R -e "install.packages('additional-r-package')"
```

## Further Documentation

- [Docker Documentation](https://docs.docker.com/get-started/)
- [Docker Compose Guide](https://docs.docker.com/compose/gettingstarted/)
- [Rocker Project (R in Docker)](https://rocker-project.org/)

## Support

For issues or questions:
- Open a [GitHub Issue](../../issues)
- Contact: Marie-Elodie Perga (marie-elodie.perga@unil.ch)

---

*Last updated: April 28, 2026*
