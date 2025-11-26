FROM rocker/shiny

# Instala TODAS as dependencias de sistema comuns para R Espacial e Grafico
# Inclui suporte para: sf, terra, leaflet, kableExtra, systemfonts, textshaping, devtools
RUN apt-get update && apt-get install -y \
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
    && rm -rf /var/lib/apt/lists/*

# Instala pacotes R (Isso ja esta em cache, nao vai demorar)
COPY requirements.R /tmp/requirements.R
RUN Rscript /tmp/requirements.R

# --- ATUALIZE ESTA LINHA ---
# Instala writexl, colourpicker e outros comuns que podem ter sido esquecidos
RUN R -e "install.packages(c('writexl', 'colourpicker', 'shinyBS', 'shinycssloaders'), repos='https://cloud.r-project.org')"
# ---------------------------

COPY sap-app/ /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
