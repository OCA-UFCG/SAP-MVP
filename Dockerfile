# Base R Shiny image (includes Shiny Server)
FROM rocker/shiny

# Install system dependencies (for package compilation)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy your R package requirements
COPY requirements.R /tmp/requirements.R

COPY . /tmp/.

# Install R packages
RUN Rscript /tmp/requirements.R

# Copy your Shiny app to the default Shiny Server directory
COPY app.R /srv/shiny-server/

# Expose the Shiny Server port
EXPOSE 3838

# (Optional) You can change this if you want a custom port inside container
# EXPOSE 8180

# Use Shiny Server as the entrypoint (default in rocker/shiny)
CMD ["/usr/bin/shiny-server"]
