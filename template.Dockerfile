#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

# Missing installation - rmd pdf
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl
RUN R -e "webshot::install_phantomjs()"

# For the rmarkdown pdf report
RUN R -e "tinytex::install_tinytex()" 
ENV PATH="/root/bin:${PATH}" 
RUN R -e "tinytex::tlmgr_install(pkgs = c('fancyhdr', 'sectsty', 'titling', 'grffile'))" 

  
#include packamon.local-r-dependencies

#include packamon.runtime-settings
