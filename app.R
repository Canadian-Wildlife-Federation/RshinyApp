
#add necessary libraries
library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(httr)
library(jsonlite)
library(sf)
library(geojsonsf)
library(magrittr)
library(DT)
library(leaflet.mapboxgl)
library(htmltools)
library(leafpop)


#API call from bcfishpass
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crossings_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27HORS%27%20AND%20all_spawningrearing_km%3e0")
df <- crossings_res %>%
      dplyr::mutate(lat = sf::st_coordinates(crossings_res)[,2],
                    lon = sf::st_coordinates(crossings_res)[,1])

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#priority and intermediate barrier list data frame to be used for filtering of ids server side
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
priority <- read.csv("data/priority_barriers.csv")
intermediate <- read.csv("data/inter_barriers.csv")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#marker color functions
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
col <- colorFactor(c("#d52a2a", "#32cd32", "#ffb400", "#965ab3"), domain = c("PASSABLE", "BARRIER", "POTENTIAL", "UNKNOWN"))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#mapbox options
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
options(mapbox.accessToken = "pk.eyJ1IjoidG9tYXMtbWsiLCJhIjoiY2w5b2JjNnl0MGR2YjN1bXpjenUwa2hnZyJ9.s21BqE7q2yEgDKFE5zNp_g")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#vector tile rendering
# wy_url = "https://tiles.hillcrestgeo.ca/bcfishpass/bcfishpass.streams/{z}/{x}/{y}.pbf"
# download.file(wy_url, "stream.pbf")
# sf::st_layers("stream.pbf")

#main app page
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  #Add custom css
  includeCSS("www/simple_app_styling.css"),
  #create top navbar
  navbarPage("WCRP Dashboard",
    #create tabs in nav bar
    tabPanel("All Barriers", 
             tabsetPanel(id = "alltab",
                         tabPanel(value = "Tab_1", 
                                   #add content to tab panel
                                   selectInput("priority", "Barrier List:", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "All"),
                                   selectInput("variable", "Barrier Status:", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
                                   leafletOutput("mymap"), 
                                   dataTableOutput("mytable"),
                          )
             )
    ),
    tabPanel("Priority Barriers",
             tabsetPanel(id = "prioritytab",
                         tabPanel(value = "Tab_2", 
                                  #add content to tab panel
                                  selectInput("priority", "Barrier List:", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "Priority"),
                                  selectInput("variable", "Barrier Status:", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
                         )
             )
    ),
    tabPanel("Intermediate Barriers",
             tabsetPanel(id = "intermediatetab",
                         tabPanel(value = "Tab_3", 
                                  #add content to tab panel
                                  selectInput("priority", "Barrier List:", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "Intermediate"),
                                  selectInput("variable", "Barrier Status:", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
                         )
             )
      ),
    #create element to display an image on right side of navbar
    tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div><a href=\"\"><img src=\"https://cwf-fcf.org/assets/wrapper-reusables/images/logo/white-cwf-logo-en.svg\" style=\"float:right;width:200px;height:40px;padding-top:10px;padding-right:5px;\"></a></div>');
    console.log(header)")),
  )
)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#popup formatting
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
df$label <- paste0("<table style=\"border: 1px solid black\">
                        <caption>this is a popup</caption>
                        <tr>
                          <th>id</th>
                          <th>", df$id, "</th>
                        <tr>
                        <tr>
                          <th>stream name</th>
                          <br>
                          <th>", df$pscis_stream_name, "</th>
                        <tr>
                      </table>")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#server rendering
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #conditions based on barrier list dropdown
  priority_div <- reactive({
    input$priority
  })
  
  y <- reactive({
    if (priority_div() == "Priority") {
      df <- df %>%
        dplyr::filter(
          id %in% priority$aggregated_crossings_id
        )
    } else if (priority_div() == "Intermediate") {
      df <- df %>%
        dplyr::filter(
          id %in% intermediate$intermediate
        )
    } else {
      df <- df
    }
  })
  
  
  #leaflet map rendering
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addMapboxGL(style = "mapbox://styles/mapbox/streets-v9", group = "Mapbox") %>%
      addMapboxGL(style = "mapbox://styles/mapbox/satellite-v9", group = "Mapbox Satellite") %>%
      addCircleMarkers(data = y() %>%
                         dplyr::filter(
                           barrier_status == "PASSABLE"
                         ),
                       lat = ~lat, 
                       lng = ~lon,
                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Passable"
      ) %>%
      addCircleMarkers(data = y() %>%
                         dplyr::filter(
                           barrier_status == "BARRIER"
                         ),
                       lat = ~lat, 
                       lng = ~lon,
                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Barrier"
      ) %>%
      addCircleMarkers(data = y() %>%
                         dplyr::filter(
                           barrier_status == "POTENTIAL"
                         ),
                       lat = ~lat, 
                       lng = ~lon,
                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Potential"
      ) %>%
      addCircleMarkers(data = y() %>%
                         dplyr::filter(
                           barrier_status == "UNKNOWN"
                         ),
                       lat = ~lat, 
                       lng = ~lon,
                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Unknown"
      ) %>%
      addLegend("topright", pal = col, values = df$barrier_status) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM", "Mapbox", "Mapbox Satellite"),
        overlayGroups = c("Passable", "Barrier", "Potential", "Unknown"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })
  
  #data table rendering
  output$mytable <- renderDataTable({y()[, c("id", "pscis_stream_name", "barrier_status")] %>%
      dplyr::filter(
        barrier_status %in% input$variable
      )
  })
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# finally, we need to call the shinyapp function with the ui and server as arguments
app <- shinyApp(ui, server)


#run app locally
runApp(app)