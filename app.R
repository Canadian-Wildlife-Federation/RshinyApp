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
library(osmextract)
library(mapboxapi)
library(ggplot2)





#API call from bcfishpass
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crossings_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27HORS%27%20AND%20all_spawningrearing_km%3e0")
df <- crossings_res %>%
      dplyr::mutate(long = sf::st_coordinates(crossings_res)[, 1],
                    lat = sf::st_coordinates(crossings_res)[, 2])

boundary <- read_sf("https://features.hillcrestgeo.ca/fwa/collections/whse_basemapping.fwa_watershed_groups_poly/items.json?watershed_group_code=HORS")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#stream vector tile reading (just geojson for now)
#-----------------------------------------------------------------------------

acc_stream_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.streams/items.json?properties=gnis_name&filter=watershed_group_code%20=%20%27HORS%27%20AND%20access_model_ch_co_sk%20IS%20NOT%20NULL")
df_str <- st_zm(acc_stream_res)

non_stream_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.streams/items.json?properties=gnis_name&filter=watershed_group_code%20=%20%27HORS%27%20AND%20access_model_ch_co_sk%20IS%20NULL")
df_nonstr <- st_zm(non_stream_res)

# %>%
#       dplyr::mutate(lat = sf::st_coordinates(acc_stream_res)[,2],
#                     lon = sf::st_coordinates(acc_stream_res)[,1])

#-----------------------------------------------------------------------------


#priority and intermediate barrier list data frame to be used for filtering of ids server side
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
priority <- read.csv("data/priority_barriers.csv")
priority$aggregated_crossings_id <- as.character(priority$aggregated_crossings_id)
intermediate <- read.csv("data/inter_barriers.csv")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#marker color functions and stream labels
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
col <- colorFactor(c("#d52a2a", "#32cd32", "#ffb400", "#965ab3"), domain = c("PASSABLE", "BARRIER", "POTENTIAL", "UNKNOWN"))

labs <- as.list(df_str$gnis_name)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#mapbox options
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
options(mapbox.accessToken = "pk.eyJ1IjoidG9tYXMtbWsiLCJhIjoiY2w5b2JjNnl0MGR2YjN1bXpjenUwa2hnZyJ9.s21BqE7q2yEgDKFE5zNp_g", mapbox.antialias = TRUE)
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
  #include JS function
  includeScript("gomap.js"),
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
    tabPanel("Useful Statistics",
             tabsetPanel(id = "prioritytab",
                         tabPanel(value = "Tab_2",
                                  #add content to tab panel
                                  selectInput("options", "Attribute", c("Barrier Status" = "barr", "Feature Type" = "type")),
                                  plotOutput("attr_bar")
                         )
             )
    ),
    tabPanel("Prioritization process",
            #  tabsetPanel(id = "intermediatetab",
            #              tabPanel(value = "Tab_3", 
            #                       #add content to tab panel
            #                       # selectInput("priority", "Barrier List:", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "Intermediate"),
            #                       # selectInput("variable", "Barrier Status:", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
            #              )
            #  )
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
                        <h4>ID: ", df$id, "</h4>
                        <br>
                        <tr class=\"popup\">
                          <th>Crossing Source:  </th>
                          <th>", df$crossing_source, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Feature Type:  </th>
                          <th>", df$crossing_feature_type, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>PSCIS Status:  </th>
                          <th>", df$pscis_status, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Crossing Type Code:  </th>
                          <th>", df$crossing_type_code, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Crossing Subtype Code:  </th>
                          <th>", df$crossing_subtype_code, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Barrier Status:  </th>
                          <th>", df$barrier_status, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>PSCIS Road Name:  </th>
                          <th>", df$pscis_road_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>PSCIS Stream Name:  </th>
                          <th>", df$pscis_stream_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>PSCIS Assessment Comment:  </th>
                          <th>", df$pscis_assessment_comment, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>PSCIS Assessment Date:  </th>
                          <th>", df$pscis_assessment_date, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Dam Name:  </th>
                          <th>", df$dam_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Dam Owner:  </th>
                          <th>", df$dam_owner, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>GNIS Stream Name:  </th>
                          <th>", df$gnis_stream_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Stream Order:  </th>
                          <th>", df$stream_order, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Anthropogenic Barriers:  </th>
                          <th>", df$barriers_anthropogenic_dnstr, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Count of Anthropogenic Barriers:  </th>
                          <th>", df$barriers_anthropogenic_dnstr_count, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>All Habitat Blocked:  </th>
                          <th>", df$all_spawningrearing_km, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Upstream Habitat Blocked:  </th>
                          <th>", df$all_spawningrearing_belowupstrbarriers_km, "</th>
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

    # on click function
    # onclick <- sprintf(
    #   "Shiny.setInputValue('click', '%s')",
    #   rownames(df)
    # )

    # button with onClick function
    # button <- sprintf(
    #   "<a class='go-map'><i class='fa fa-crosshairs'></i></a>" #href='' data-lat='", lat, "'' data-long='", lon,"'
    # )

    #mutate(Action = paste('<a class="go-map" href="''" data-lat='", Lat, "' data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))

    if (priority_div() == "Priority") {
      df <- df %>%
        dplyr::filter(
          id %in% priority$aggregated_crossings_id
        ) %>%
        dplyr::filter(
          barrier_status %in% input$variable
        )
        df <- left_join(df, priority, by = c("id" = "aggregated_crossings_id"))
        df<- df %>%  mutate(label = paste0("<table style=\"border: 1px solid black\">
                        <h4>ID: ", df$aggregated_crossings_id, "</h4>
                        <br>
                        <tr class=\"popup\">
                          <th>Stream Name:  </th>
                          <th>", df$stream_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Feature Type:  </th>
                          <th>", df$barrier_type, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Priority:  </th>
                          <th>", df$priority, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Owner:  </th>
                          <th>", df$owner, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Habitat Quality (Upstream):  </th>
                          <th>", df$upstr_hab_quality, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Road Name:  </th>
                          <th>", df$next_steps, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Reason:  </th>
                          <th>", df$reason, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th>Notes:  </th>
                          <th>", df$notes, "</th>
                        <tr>
                      </table>"))
    } else if (priority_div() == "Intermediate") {
      df <- df %>%
        dplyr::filter(
          id %in% intermediate$intermediate
        ) %>%
        dplyr::filter(
          barrier_status %in% input$variable
        )
    } else {
      df <- df %>%
        dplyr::filter(
          barrier_status %in% input$variable
        )
    }
  })
  
  
  #leaflet map rendering
  output$mymap <- renderLeaflet({

    
    leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>%
    addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addMapboxGL(style = "mapbox://styles/mapbox/streets-v9", group = "Mapbox", options = leafletOptions(pane = "polygons")) %>%
      # addWMSTiles("http://maps.geogratis.gc.ca/wms/hydro_network_en?version=1.3.0",
      #             layers = "nhn:nhn:drainageareas:nhnda",
      #             options = WMSTileOptions(format = "image/png", transparent = TRUE),
      #             attribution = "Government of Canada; Natural Resources Canada; Strategic Policy and Innovation Sector",
      #             group = "Watershed") %>%
      addMapboxGL(style = "mapbox://styles/mapbox/satellite-v9", group = "Mapbox<br>Satellite", options = leafletOptions(pane = "polygons")) %>%
      addCircleMarkers(data = y() %>%

                        dplyr::filter(
                          barrier_status == "PASSABLE"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Passable",
                       options = leafletOptions(pane = "maplabels")
      ) %>%
      addCircleMarkers(data = y() %>%

                        dplyr::filter(
                          barrier_status == "BARRIER"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Barrier",
                       options = leafletOptions(pane = "maplabels")
      ) %>%
      addCircleMarkers(data = y() %>%
                        dplyr::filter(
                          barrier_status == "POTENTIAL"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Potential",
                       options = leafletOptions(pane = "maplabels")
      ) %>%
      addCircleMarkers(data = y() %>%
                        dplyr::filter(
                          barrier_status == "UNKNOWN"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(),
                       color = ~col(barrier_status),
                       popup = ~label,
                       group = "Unknown",
                       options = leafletOptions(pane = "maplabels")
                       ) %>%
      addPolylines(data = df_str, color = "blue", opacity = 1,  label = ~paste0(gnis_name), group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      #addPolylines(data = df_nonstr, color = "grey", group = "Non-Streams") %>%
      addPolygons(data = boundary, stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5,
    color = "black", opacity = 1, group = "Watershed<br>Boundary", fillColor = NA, options = leafletOptions(pane = "polygons")) %>%
      addEasyButton(easyButton(
        icon = "fa-home", title = "Deafult View",
        onClick = JS("function(btn, map){ map.setZoom(9); }"))) %>%
      addLegend("topright", pal = col, values = df$barrier_status) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Mapbox", "Mapbox<br>Satellite"),
        overlayGroups = c("Streams", "Watershed<br>Boundary"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # reference for gomap.js function
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("mymap")
      dist <- 0.0005
      lat <- input$goto$lat
      long <- input$goto$lng
      map %>% fitBounds(long - dist, lat - dist, long + dist, lat + dist)
    })
  })

  #data table rendering
  output$mytable <- renderDataTable({

    dt <- y()[, c("id", "pscis_stream_name", "barrier_status", "crossing_feature_type", "lat", "long")] %>%
              #  dplyr::filter(
              #   barrier_status %in% input$variable
              # ) %>%
              st_drop_geometry() %>%
              mutate(Location = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))


    action <- DT::dataTableAjax(session, dt, outputId = "mytable")

    datatable(dt, options = list(ajax = list(url = action)),
      colnames = c("ID", "Stream Name", "Barrier Status", "Potenital Crossing Type", "Lat", "Lon", "Location"),
      escape = FALSE,
      selection = "none",
      style = "bootstrap"
      )

    })

  #Tab panel #2---------------------------------------------------------------------
  output$attr_bar <- renderPlot(width = "auto",
  height = "auto",
  res = 150,
  {
    if (input$options == "barr"){
      df1 <- df %>%
            count(barrier_status) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))

      ggplot(df1, aes(x=barrier_status, y=Perc, fill=barrier_status)) +
      geom_bar(stat="identity") + 
      geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
      labs(title = "Attribute Summary for HORS", x = "Barrier Status", y = "Proportion %", fill="barrier_status") +
      theme(plot.title = element_text(hjust = 0.5))
      #scale_fill_discrete(name= "Type")
    }
    else if (input$options == "type"){
      df1 <- df %>%
            count(crossing_feature_type) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))

      ggplot(df1, aes(x=crossing_feature_type, y=Perc, fill=crossing_feature_type)) +
      geom_bar(stat="identity") + 
      geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
      labs(title = "Attribute Summary for HORS", x = "Feature Type", y = "Proportion %", fill="crossing_feature_type") + 
      theme(plot.title = element_text(hjust = 0.5))
      #scale_fill_discrete(name= "Type")
    }
  }
  )

}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# finally, we need to call the shinyapp function with the ui and server as arguments
app <- shinyApp(ui, server)


#run app locally
runApp(app)