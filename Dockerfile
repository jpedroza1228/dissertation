FROM rocker/tidyverse:latest
MAINTAINER Jonathan Pedroza (cpppedroza@gmail.com)

## Install packages from CRAN
RUN install2.r --error \
    sf \
    tidycensus \
    leaflet \
    spdep \
    spatialreg \
    rgdal \
    rgeos \
    tictoc \
    