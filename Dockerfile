FROM rocker/shiny:4.2.1
RUN install2.r remotes rsconnect maps leaflet dplyr shiny httr jsonlite sf geojsonsf magrittr DT htmltools leafpop osmextract ggplot2 shinydashboard shinydashboardPlus shinyWidgets png shinyBS shinyjs
RUN installGithub.r rstudio/leaflet.mapboxgl
WORKDIR /home/rshinyApp
COPY data data
COPY www www
COPY gomap.js gomap.js
COPY app.R app.R
COPY deploy.R deploy.R
CMD Rscript deploy.R
