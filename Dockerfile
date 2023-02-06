FROM rocker/shiny:4.2.1
RUN install2.r rsconnect maps leaflet dplyr shiny httr jsonlite sf geojsonsf magrittr DT htmltools leafpop osmextract ggplot2 shinydashboard shinydashboardPlus shinyWidgets png shinyBS shinyjs
RUN installGithub.r rstudio/leaflet.mapboxgl
WORKDIR /home/rshinyApp
COPY app.R app.R
COPY deploy.R deploy.R
CMD Rscript deploy.R
