# =========================================================================
# Copyright © 2024 REV
# =========================================================================

FROM rocker/shiny


# Some of these packages such as apt-utils, apt-transport-https or gnupg2 are required so that the R repo can be added and R installed

RUN apt-get update -y && apt-get install -y build-essential curl libssl3 libssl-dev gnupg2 software-properties-common \
  dirmngr apt-transport-https apt-utils lsb-release ca-certificates  libcurl4-openssl-dev libssl-dev libxml2-dev \
  pandoc pandoc-citeproc libharfbuzz-dev libfribidi-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev apt-utils

RUN apt-get update -y && apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsodium-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev
RUN apt-get install -y unixodbc-dev libproj22 libudunits2-0 libgdal30

# See about installing ODBC drivers here: https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-2017
# Note that the driver version installed needs to match the version used in the code
# In this case for Ubuntu 18.04: ODBC SQL driver 17

## see https://learn.microsoft.com/en-us/answers/questions/1180640/issue-install-msodbcsql17-on-docker
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN curl https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN exit
RUN apt-get update && apt install -y apt-utils
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
RUN apt-get remove -y libodbc2 libodbcinst2 odbcinst unixodbc-common
RUN ACCEPT_EULA=Y apt-get install -y msodbcsql17



# Install R packages (odbc R package must be installed AFTER installing ODBC in the OS)
RUN Rscript -e "install.packages(c('lubridate', 'tidyverse', 'jsonlite', 'dbplyr', 'odbc', 'config', 'DBI', 'devtools', 'Rcpp', 'shiny'))"

RUN Rscript -e "install.packages(c('odbc', 'shinydashboard', 'DT', 'leaflet', 'leaflet.extras', 'tmap', 'stars', 'sf', 'formattable'))"

COPY config.yml config.yml
COPY data data
COPY app app

RUN chmod -R 777 /app
USER shiny
EXPOSE 8180

WORKDIR "/app"

CMD Rscript app.R