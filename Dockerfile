FROM ghcr.io/rocker-org/devcontainer/tidyverse:4.3

# Dependencies installation
RUN apt-get update -qq \
  && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
#    libsodium-dev \
    libxml2-dev \
    libprotobuf-dev protobuf-compiler libudunits2-dev libgdal-dev libgeos-dev libproj-dev libjq-dev \
# For protolite in geojson, which seems to be used in shinycss
#    libicu-dev \
# For phyloseq
    libigraph-dev  \
# For krona
    radiant \
# Devtools, but probalbly use in other as well
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libglpk-dev libmagick++-dev  \
    && apt-get clean && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true

# Install R library beforehand so I don't need to repeat this everytime I install packages.
COPY DESCRIPTION /tmp/tmpdesc/
RUN R -e "devtools::install_deps('/tmp/tmpdesc')"

# Set the user (optional, for better security)
USER rstudio
