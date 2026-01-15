# Set base image
FROM --platform=linux/amd64 rocker/shiny

# My authorship
LABEL maintainer="dinotrex08@iolani.org"
LABEL version="1.0.0"
LABEL description="Blackjack Game for Iolani School"

# Convenience packages
RUN apt update
RUN apt install -y curl git wget nano libmagick++-dev

# Install R packages
RUN R -e "install.packages(c('shiny','bslib','magick','stringr'))"

# Copy app to image
RUN rm -r /srv/shiny-server/*
COPY app.R /srv/shiny-server/app.R
COPY assets/ /srv/shiny-server/assets/