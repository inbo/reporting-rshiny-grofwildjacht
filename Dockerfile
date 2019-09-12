FROM openanalytics/r-base

MAINTAINER Stijn Van Hoey stijn.vanhoey@inbo.be 

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev


# Dependencies for rgdal and rgeos
RUN  apt-get update && apt-get install -y software-properties-common && \
     add-apt-repository ppa:ubuntugis/ppa

# install imports of reporting-grofwild app that are not on cloud
RUN R -e "install.packages(c('shiny', 'sp', 'plotly', 'plyr', 'devtools', 'methods', 'reshape2', 'mgcv', 'rgdal', 'rgeos', 'shinycssloaders', 'raster'), repos = 'https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('inbo/INBOtheme')"

# install depends of reporting-grofwild app
RUN R -e "install.packages(c('maptools'), repos='https://cloud.r-project.org/')"

# install suggests of reporting-grofwild app
RUN R -e "install.packages(c('leaflet', 'mapview'), repos='https://cloud.r-project.org/')"

# For downloading the maps
# Attention: do not install phantomjs directly, will not work then!
RUN R -e "webshot::install_phantomjs()"

 
# copy the app to the image by installing package (need latest version!!)
ENV latestApp reportingGrofwild_0.1.0.tar.gz
COPY $latestApp /root/
RUN R CMD INSTALL /root/$latestApp
RUN rm /root/$latestApp


# set host
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e reportingGrofwild::runWildApp()"]
