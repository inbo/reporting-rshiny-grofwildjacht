FROM openanalytics/r-ver:4.0.5 as builder

MAINTAINER Stijn Van Hoey stijn.vanhoey@inbo.be

RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin \
    libproj15 \
    libgeos-3.8.0 libgeos-c1v5  \
    curl \
    ca-certificates \
    pandoc \
    libudunits2-0 \
    && rm -rf /var/lib/apt/lists/*

# Use the remotes package instead of devtools as it is mutch lighter
RUN R -q -e "install.packages('remotes')"

RUN R -q -e "remotes::install_cran(c('shiny', 'sp', 'dplyr', 'plyr', 'reshape2', 'mgcv', 'rgdal', 'rgeos', 'raster', 'stringr', 'maptools', 'leaflet', 'mapview', 'flexdashboard', 'shinyjs', 'data.table', 'rmarkdown'))"
RUN R -q -e "remotes::install_version('plotly', version = '4.9.2.1')"
RUN R -q -e "remotes::install_version('DT', version = '0.12')"
RUN R -q -e "remotes::install_github('inbo/INBOtheme')"
RUN R -q -e "install.packages('oaStyle', repos = c(rdepot = 'https://repos.openanalytics.eu/repo/public', getOption('repos')))"

# to prevent bobbing with shinycssloaders
RUN R -q -e "remotes::install_github('daattali/shinycssloaders')"

# For downloading the maps
# Attention: do not install phantomjs directly, will not work then!
RUN R -e "webshot::install_phantomjs()"

FROM builder as tmp

# Install the package without the source files ending up in the Docker image
COPY reporting-grofwild /tmp/package
RUN R -q -e "remotes::install_local('/tmp/package', dependencies=FALSE)"

COPY data /tmp/data
RUN R -q -e "reportingGrofwild::createShapeData('/tmp/data')"

FROM builder as final

COPY --from=tmp /usr/local/lib/R/site-library/reportingGrofwild/ /usr/local/lib/R/site-library/reportingGrofwild/

# set host
COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e reportingGrofwild::runWildApp()"]
