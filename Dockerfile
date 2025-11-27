FROM rocker/shiny:4.3.2


RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    pandoc \
    libabsl-dev \
    && rm -rf /var/lib/apt/lists/*


COPY requirements.R /tmp/requirements.R
RUN Rscript /tmp/requirements.R


COPY sap-app/ /srv/shiny-server/


EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
