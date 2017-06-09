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
    libssl1.0.0 

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of reporting-grofwild app
RUN R -e "devtools::install_github('inbo/INBOtheme')"

# copy the app to the image by installing package
COPY reporting-grofwild.tar.gz /root/
RUN R CMD INSTALL /root/reporting-grofwild.tar.gz
RUN rm /root/reporting-grofwild.tar.gz

# set host
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e reporting-grofwild::runWildApp.R()"]