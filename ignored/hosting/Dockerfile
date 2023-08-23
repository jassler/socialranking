# Get the base R image from Rocker
FROM rocker/shiny:latest

# System libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libjpeg-dev

# Install R packages required
# (change it according to the packages your app needs)
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'socialranking'), repos='https://cloud.r-project.org/')"

# Copy the app to the image
COPY shiny.R /srv/shiny-server/

# Set the working directory
WORKDIR /srv/shiny-server/

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/shiny.R', host = '0.0.0.0', port = 3838)"]
