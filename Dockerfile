FROM rocker/shiny:latest

WORKDIR /

RUN apt-get update -qq
RUN apt-get -y -f install \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev

# Installs all required R packages (and their dependencies) starting from those hat are available on the remote repo
# and then from the locally available libs (for the time being)
RUN install2.r --error --skipinstalled \
    cowplot \
    data.table \
    devtools \
    dplyr \
    DT \
    ggnewscale \
    ggthemes \
    htmltools \
    mapdata \
    mapplots \
    maps \
    openxlsx \
    png \
    scatterpie \
    sf \
    shiny \
    shinycssloaders \
    shinyjs \
    shinyWidgets \
    stringr \
    units

# Sets the working directory to the shiny-server root folder
WORKDIR /srv/shiny-server

# Empties the shiny-server folder
RUN rm -rf *

# Copies the provided default shiny-server configuration under /etc/shiny-server
COPY ./build/shiny/shiny-server.conf /etc/shiny-server

# Copies the local R scripts (necessary to initialize the app) in a folder under /srv/shiny-server
COPY ./update_libs.R .

# External argument(s)
ARG GITHUB_AUTH_TOKEN

# Environment variables

ENV _R_SHLIB_STRIP_=true
ENV GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN

# Copies the entire structure of the Shiny app under a dedicated folder
COPY ./shiny interactive_catdis_browser

# Updates the R libs
ADD "https://www.random.org/cgi-bin/randbyte?nbytes=10&format=h" skipcache

RUN Rscript update_libs.R

# Sets the Shiny log level to 'TRACE', stores the environment variable in .Renviron and copies that file under the 'shiny' user folder
RUN echo SHINY_LOG_LEVEL = TRACE >> /home/shiny/.Renviron && chown shiny:shiny /home/shiny/.Renviron

# Removes an unnecessary directory and files under the Shiny app folder
RUN rm -rf *.R

# Continues configuring Shiny
RUN echo "shiny:pass" | chpasswd
RUN adduser shiny sudo

# See: https://stackoverflow.com/questions/61125475/application-logs-to-stdout-with-shiny-server-and-docker
ENV SHINY_LOG_STDERR=1
ENV SHINY_LOG_LEVEL=DEBUG

# User running the Shiny server
USER shiny

# TCP/IP Port
EXPOSE 3838

# Starts Shiny
CMD ["/usr/bin/shiny-server"]
