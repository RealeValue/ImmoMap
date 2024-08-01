# =========================================================================
# Copyright © 2024 REV
# =========================================================================

# Use the rocker/shiny base image
FROM rocker/shiny

# Install system dependencies
RUN apt-get update -y && apt-get install -y build-essential curl libssl3 libssl-dev gnupg2 software-properties-common \
    dirmngr apt-transport-https apt-utils lsb-release ca-certificates libcurl4-openssl-dev libssl-dev libxml2-dev \
    pandoc pandoc-citeproc libharfbuzz-dev libfribidi-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev

# Install additional system dependencies
RUN apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsodium-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev
RUN apt-get install -y unixodbc-dev 

# Install Microsoft ODBC driver
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN curl https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN apt-get update && apt install -y apt-utils
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
RUN apt-get remove -y libodbc2 libodbcinst2 odbcinst unixodbc-common
RUN ACCEPT_EULA=Y apt-get install -y msodbcsql17

# Install R packages (odbc R package must be installed AFTER installing ODBC in the OS)
RUN apt-get install -y libproj22 libudunits2-0 libgdal30
RUN Rscript -e "install.packages(c('lubridate', 'tidyverse', 'jsonlite', 'dbplyr', 'odbc', 'config', 'DBI', 'devtools', 'Rcpp', 'shiny'))"
RUN Rscript -e "install.packages(c('odbc', 'shinydashboard', 'DT', 'leaflet', 'leaflet.extras', 'tmap', 'stars', 'sf', 'formattable'))"

# Copy configuration files, data, and app
COPY config.yml config.yml
COPY data data
COPY app app

# Set permissions for the app directory
RUN chmod -R 777 /app

# Set user and expose port
USER shiny
EXPOSE 8180

# Set working directory
WORKDIR "/app"

# Run your app
CMD Rscript app.R
