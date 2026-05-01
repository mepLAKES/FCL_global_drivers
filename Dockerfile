# Use rocker/tidyverse as base image (includes R and common packages)
FROM rocker/tidyverse:4.3.0

# Set maintainer
LABEL maintainer="Marie-Elodie Perga <marie-elodie.perga@unil.ch>"

# Set working directory
WORKDIR /workspace

# Install system dependencies required for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required for the analysis
RUN R -e "install.packages(c( \
    'here', \
    'mgcv', \
    'sjPlot', \
    'patchwork', \
    'cowplot', \
    'rfishbase', \
    'readxl', \
    'forcats', \
    'ATNr', \
    'deSolve', \
    'future.apply', \
    'future', \
    'doFuture', \
    'tidyr' \
    ), repos='https://cloud.r-project.org/')"

# Install FishBase data (optional - can be downloaded on runtime)
# RUN R -e "rfishbase::rfishbase_setup()"

# Copy project files into container
COPY . /workspace/

# Set R to use 'here' package for paths
ENV R_LIBS_USER=/workspace/.libPaths

# Create output directories if they don't exist
RUN mkdir -p "Global empirical patterns/figures" \
    && mkdir -p "Theoretical modelling/figures"

# Default command - start R interactive session
CMD ["R", "--vanilla"]

# Optional: To run specific script, override CMD:
# docker run -v $(pwd):/workspace fcl_drivers Rscript "Global empirical patterns/scripts/1_datasets_preparation.R"
