FROM rocker/shiny:4.2.1
RUN install2.r rsconnect 
WORKDIR /home/rshinyApp
COPY deploy.R deploy.R
CMD Rscript deploy.R