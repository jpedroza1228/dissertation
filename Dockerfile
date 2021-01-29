FROM rocker/tidyverse
MAINTAINER Jonathan Pedroza (cpppedroza@gmail.com)

## Install packages from CRAN
RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    tidyverse \
    sf \
    tidycensus \
    leaflet \
    spdep \
    spatialreg \
    rgdal \
    rgeos \
    tictoc \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
