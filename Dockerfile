FROM rocker/geospatial:4.4.3

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install additional system dependencies (only if missing)
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error --skipinstalled \
    sp \
    terra \
    sf \
    tidyterra \
    tidyverse \
    lubridate

# Set working directory
WORKDIR /workspace

# Default command (optional)
CMD ["R"]
