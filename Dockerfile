FROM openanalytics/r-ver:4.0.5

LABEL maintainer="Machteld Varewyck machteld.varewyck@openanalytics.eu"

RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin \
    libproj15 \
    libgeos-3.8.0 libgeos-c1v5  \
    libcurl4-openssl-dev \
    curl \
    ca-certificates \
    pandoc \
    libudunits2-0 \
    libmagick++-dev \
    libssl-dev \
    lmodern \
    fonts-cantarell \
    texlive-plain-generic \
    texlive-fonts-extra \
    && rm -rf /var/lib/apt/lists/*

# Use the remotes package instead of devtools as it is much lighter
RUN R -q -e "install.packages('remotes')"

RUN R -q -e "remotes::install_cran(c('shiny', 'sf', 'dplyr', 'plyr', 'reshape2', 'mgcv', 'stringr', 'leaflet', 'flexdashboard', 'testthat', 'shinyjs', 'data.table', 'tinytex', 'tidyr', 'arrow', 'kableExtra'))"
RUN R -q -e "remotes::install_version('DT', version = '0.23', repos = 'https://cloud.r-project.org', upgrade = 'never')"
RUN R -q -e "remotes::install_version('plotly', version = '4.10.1', repos = 'https://cloud.r-project.org', upgrade = 'never')" 
RUN R -q -e "remotes::install_version('rmarkdown', version = '2.18', repos = 'https://cloud.r-project.org', upgrade = 'never')"
RUN R -q -e "remotes::install_version('magick', version = '2.7.3', repos = 'https://cloud.r-project.org', upgrade = 'never')"
# NOTE: Need at least these versions of plotly, rmarkdown and magick for dashboard rmarkdown to work

RUN R -q -e "remotes::install_github('inbo/INBOtheme@v0.5.10')"
RUN R -q -e "install.packages('oaStyle', repos = c(rdepot = 'https://repos.openanalytics.eu/repo/public', getOption('repos')))"

# to prevent bobbing with shinycssloaders
RUN R -q -e "remotes::install_github('daattali/shinycssloaders')"

# For the rmarkdown pdf report
RUN R -e "tinytex::install_tinytex()" 
ENV PATH="/root/bin:${PATH}" 
# many packages needed for kableExtra background feature
RUN R -e "tinytex::tlmgr_install(pkgs = c('fancyhdr', 'sectsty', 'titling', 'grffile', 'texlive-scripts', 'mathspec', 'multirow', 'wrapfig', 'colortbl', 'pdflscape', 'tabu', 'varwidth', 'threeparttable', 'threeparttablex', 'environ', 'trimspaces', 'ulem', 'makecell'))" 

# For downloading the maps
# Attention: do not install phantomjs directly, will not work then!
RUN R -q -e "remotes::install_cran('webshot')"
RUN R -e "webshot::install_phantomjs()"

# Git sha
ARG GIT_SHA
ENV GIT_SHA=$GIT_SHA

# For access to S3 on UAT
# For calculating areas - fix #435
RUN R -q -e "remotes::install_cran(c('config', 'aws.s3', 'aws.ec2metadata', 'lwgeom'))"


# Install the package without the source files ending up in the Docker image
COPY reporting-grofwild /tmp/package
RUN R -q -e "remotes::install_local('/tmp/package', dependencies=FALSE)"

# set host
COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e reportingGrofwild::runWildApp()"]
