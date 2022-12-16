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
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(png)
library(shinyBS)





#API call from bcfishpass
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crossings_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27HORS%27%20AND%20all_spawningrearing_km%3e0")
df <- crossings_res %>%
      dplyr::mutate(long = sf::st_coordinates(crossings_res)[, 1],
                    lat = sf::st_coordinates(crossings_res)[, 2])

boundary <- read_sf("https://features.hillcrestgeo.ca/fwa/collections/whse_basemapping.fwa_watershed_groups_poly/items.json?watershed_group_code=HORS")



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#watershed connectivity function
watershed_connectivity <- function(habitat_type){
  request <- paste('https://features.hillcrestgeo.ca/bcfishpass/functions/postgisftw.wcrp_watershed_connectivity_status/items.json?watershed_group_code=HORS&barrier_type=',habitat_type, sep = "")
  res <- GET(request)
  data <- fromJSON(rawToChar(res$content))
  return(data$connectivity_status)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#static statistics
hab_conf <- sum(df$pscis_status == "HABITAT CONFIRMATION", na.rm = TRUE)
assessed <- sum(is.na(df$pscis_assessment_date))
total <- 526.95 #total km in HORS
access <- round(total * (watershed_connectivity("ALL") / 100), 2)
gain <- round(total - access, 2)

#-------------------------------------------------------------------------------------------------------------------------

#stream vector tile reading (just geojson for now)
#-----------------------------------------------------------------------------

acc_stream_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.streams/items.json?properties=gnis_name&filter=watershed_group_code%20=%20%27HORS%27%20AND%20access_model_ch_co_sk%20IS%20NOT%20NULL")
df_str <- st_zm(acc_stream_res)
#df_str[is.na(df_str)] <- "Not Named"
df_null <- df_str[rowSums(is.na(df_str)) != 0, ]
df_str <- na.omit(df_str)

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

#main app page
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  includeCSS("www/simple_app_styling.css"),
  #include JS function
  includeScript("gomap.js"),
  #create top navbar
  navbarPage("WCRP Dashboard",
    #create tabs in nav bar
    tabPanel("Useful Statistics",
             tabsetPanel(id = "prioritytab",
                         tabPanel(value = "Tab_2",
                                  useShinydashboard(),
                                  #add content to tab panel
                                  mainPanel(
                                    selectInput("options", "Attribute", c("Barrier Status" = "barr", "Feature Type" = "type")),
                                    plotOutput("attr_bar")
                                  ),
                                  sidebarPanel(
                                    fluidRow(
                                      shinyWidgets::progressBar(id = "connect",
                                                  value = watershed_connectivity("ALL"),
                                                  display_pct = TRUE,
                                                  title = "Overall Connectivity Status",
                                                  status = "custom",
                                                  total = 100,
                                                  striped = TRUE,
                                      ),
                                      actionButton("refresh", "Update Status"),
                                      plotOutput("progress")
                                    )
                                  ),
                                  sidebarPanel(
                                    #species boxes
                                    fluidRow(
                                      h1("Species of Interest"),
                                      br(),
                                      box(
                                        title = "Chinook Salmon | Kekèsu | Oncorhynchus tshawytscha ",
                                        "Chinook Salmon are the first to return each year, usually in early August, and have the most limited distribution within the watershed. Known spawning occurs in parts of the Horsefly River mainstem above the confluence with the Little Horsefly River and throughout McKinley Creek as far as Elbow Lake. Important rearing systems include Patenaude Creek, Kroener Creek, Black Creek, Woodjam Creek, Deerhorn Creek, and Wilmot Creek.",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                      
                                      ),
                                      box(
                                        title = "Coho Salmon | Sxeyqs | Oncorhynchus kisutch",
                                        "Coho Salmon are the most widely distributed of the three focal species in the watershed, with the ability to migrate into smaller, upper tributary systems (DFO 1991). Spawning occurs in the Little Horsefly River between Gruhs Lake and Horsefly Lake, McKinley Creek below McKinley Lake, Woodjam Creek, Patenaude Creek, Tisdall Creek, and Black Creek. Rearing fry and juveniles have been observed in the Little Horsefly River, Patenaude Creek, and McKinley Creek up to Bosk Lake. ",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                    
                                      ),
                                      box(
                                        title = "Sockeye Salmon | Sqlelten7ùwi | Oncorhynchus nerka ",
                                        "Sockeye Salmon have historically been the most abundant of the three focal species in the watershed, though the population has seen significant declines in recent years (DFO 1991, S. Hocquard pers. comm.). Sockeye Salmon spawning is known to occur throughout the Horsefly River (up to the impassable falls), in the Little Horsefly River between Gruhs Lake and Horsefly Lake, Moffat Creek (up to the impassible falls), and McKinley Creek up to Elbow Lake (PSF 2018, DFO 1991, S. Hocquard pers. comm.). Additionally, a spawning channel aimed at enhancing the Sockeye Salmon population was constructed by Fisheries and Oceans Canada in 1989 (DFO 1991). Currently, there are no Sockeye Salmon rearing in the Horsefly River watershed – all emergent fry migrate down to Quesnel Lake.",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                      ),
                                    ),
                                      
                                  ),
                                    #barriers boxes
                                    sidebarPanel(fluidRow(
                                      h1("Barrier Types"),
                                      br(),
                                      box(
                                        title = "Small Dams (<3 m height)",
                                        "There are nine mapped small dams on “potentially accessible” stream segments in the watershed, blocking a total of 8.09 km (~23% of the total blocked habitat) of modelled spawning and rearing habitat for anadromous salmon, resulting in a Medium extent. The extent rating of these structures was confirmed by the planning team. There are two known fish-passage structures in the watershed, including on the dam at the outlet of McKinley Lake. The remaining dams likely block passage for anadromous salmon and would require significant resources to remediate. However, due to the limited extent of dams in the watershed, a final pressure rating of Medium was assigned. Four small dams were identified on the priority barrier list. Three of the dams require further assessment and confirmation of upstream habitat quality, and the dam observed at the outlet of Kwun Lake does not exist.",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                      
                                      ),
                                      box(
                                        title = "Road-stream Crossings",
                                        "Road-stream crossings are the most abundant barrier type in the watershed, with 103 assessed and modelled crossings located on stream segments with modelled habitat. Demographic road crossings (highways, municipal, and paved roads) block 7.31 km of habitat (~21% of the total blocked habitat), with 73% of assessed crossings having been identified as barriers to fish passage. Resource roads block 19.57 km of habitat (~56%), with 60% of assessed crossings having been identified as barriers. The planning team felt that the data was underestimating the severity of road-stream crossing barriers in the watershed, and therefore decided to update the rating from High to Very High. The planning team also felt that an irreversibility rating of Medium was appropriate due to the technical complexity and resources required to remediate road-stream crossings.",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                    
                                      ),
                                      box(
                                        title = "Trail-stream crossings",
                                        "There is very little spatial data available on trail-stream crossings in the watershed, so the planning team was unable to quantify the true Extent and Severity of this barrier type. However, the planning team felt that trail-stream crossings are not prevalent within the watershed and that, where they do exist, they do not significantly impact passage for anadromous salmon. As most crossings will be fords or similar structures, remediation may not be required, or remediation costs associated with these barriers would be quite low. Overall, the planning team felt that the pressure rating for trail-stream crossings was likely Low; however, the lack of ground-truthed evidence to support this rating was identified as a knowledge gap within this plan.",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                      ),
                                      box(
                                        title = "Lateral Barriers",
                                        "There are numerous types of lateral barriers that potentially occur in the watershed, including dykes, berms, and linear development (i.e., road and rail lines), all of which can restrict the ability of anadromous salmon to move into floodplains, riparian wetlands, and other off-channel habitats. No comprehensive lateral barrier data exists within the watershed, so pressure ratings were based on qualitative local knowledge. Lateral barriers are not thought to be as prevalent as road- or rail-stream crossings but are likely very severe where they do exist. Significant lateral barriers are known to occur along the mainstem of the Horsefly River, which disconnect the mainstem river from historic floodplain and off-channel habitat. Overall, the planning team decided that a High pressure rating adequately captured the effect that lateral barriers are having on connectivity in the watershed. Work to begin quantifying and mapping lateral habitat will begin in 2022-23, as described in the Operational Plan under Strategy 2: Lateral barrier remediation. ",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                      ),
                                      box(
                                        title = "Natural Barriers",
                                        "Natural barriers to fish passage can include debris flows, log jams, sediment deposits, etc., but natural features that have always restricted fish passage (e.g., waterfalls) are not considered under this barrier type. Natural barriers are difficult to include in a spatial prioritization framework due to their transient nature. The planning team identified known natural barriers that occur throughout the watershed, such as beaver dams and log jams. Generally, these natural barriers are only severe impediments to fish passage during low-flow years, but reduced baseflows have become more common in recent years. Based on this, the planning team felt that natural barriers will be severe most years where they exist, but are mostly reversible, resulting in an overall pressure rating of Low.",
                                        id = "mybox",
                                        collapsible = TRUE,
                                        closable = FALSE,
                                        collapsed = TRUE
                                      )
                                     
                                    )),
                                    h1(paste0("Assessments done : ", toString(assessed))),
                                    h1(paste0("# of barriers with Habitat Confirmations : ", toString(hab_conf))),
                                    h1(paste0("Total km blocked : ", toString(gain), "km")),
                                    h1(paste0("# of crossings on accessible streams: ", length(df$aggregated_crossings_id))),
                                    plotOutput("attr_pie"),
                                    div(tags$img(src = "www/salmon.png", alt = "something went wrong", deleteFile = FALSE), style = "text_align: center;")

                                  
                         )
             )
    ),

    tabPanel("Interactive Map", 
             tabsetPanel(id = "alltab",
                         tabPanel("", value = "Tab_1", 
                                   #add content to tab panel
                                  fluidRow(id = "row1",
                                    selectInput("priority", "Barrier List", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "All"),
                                    bsTooltip("priority", "Here is some text with your instructions", placement = "bottom", trigger = "hover", options = NULL),
                                    selectInput("variable", "Barrier Status", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
                                    bsTooltip("variable", "Here is some text with your instructions", placement = "bottom", trigger = "hover", options = NULL)
                                  ),
                                  tags$hr(style="border-color: white;"),
                                  fluidRow(id = "row2",
                                           column(7,
                                                  leafletOutput("mymap")
                                           ),
                                           column(5,
                                                  dataTableOutput("mytable")
                                           ),
                                  )
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

# fluidRow(
#                                     column(
#                                       width = 6,
#                                       uiOutput("active_side"), 
#                                       actionButton("toggle", "Toggle flip box"),
#                                       flipBox(
#                                         id = "myflipbox", 
#                                         trigger = "hover",
#                                         width = 12,
#                                         front = div(
#                                           class = "text-center",
#                                           h1("Flip on hover"),
#                                           img(
#                                             src = "https://image.flaticon.com/icons/svg/149/149076.svg",
#                                             height = "300px",
#                                             width = "100%"
#                                           )
#                                         ),
#                                         back = div(
#                                           class = "text-center",
#                                           height = "300px",
#                                           width = "100%",
#                                           h1("Flip on hover"),
#                                           p("More information....")
#                                         )
#                                       )
#                                     ),
#                                     column(
#                                       width = 6,
#                                       uiOutput("active_side_2"),
#                                       flipBox(
#                                         id = "myflipbox2",
#                                         width = 12,
#                                         front = div(
#                                           class = "text-center",
#                                           h1("Flip on click"),
#                                           img(
#                                             src = "https://image.flaticon.com/icons/svg/149/149076.svg",
#                                             height = "300px",
#                                             width = "100%"
#                                           )
#                                         ),
#                                         back = div(
#                                           class = "text-center",
#                                           height = "300px",
#                                           width = "100%",
#                                           h1("Flip on click"),
#                                           p("More information....")
#                                         )
#                                       )
#                                     )
#                                   )
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#popup formatting
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
df$label <- paste0("<table style=\"border: 1px solid rgb(241, 241, 241)\">
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
      addMapboxGL(style = "mapbox://styles/mapbox/streets-v9", group = "Mapbox", options = leafletOptions(pane = "polygons")) %>%
      addMapboxGL(style = "mapbox://styles/mapbox/satellite-v9", group = "Mapbox<br>Satellite", options = leafletOptions(pane = "polygons")) %>%
      addCircleMarkers(data = y() %>%

                        dplyr::filter(
                          barrier_status == "PASSABLE"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                          var childCount = cluster.getChildCount(); 
                          var c = ' marker-custom-';  
                          if (childCount < 5) {  
                            c += 'large';  
                          } else if (childCount < 50) {  
                            c += 'medium';  
                          } else { 
                            c += 'small';  
                          }    
                          return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

                        }")),
                       color = "white",
                       fillColor = ~col(barrier_status),
                       popup = ~label,
                       group = "Passable",
                       opacity = 1,
                       fillOpacity = 0.90,
                       options = leafletOptions(pane = "maplabels")
      ) %>%
      addCircleMarkers(data = y() %>%

                        dplyr::filter(
                          barrier_status == "BARRIER"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-custom-';  
    if (childCount < 5) {  
      c += 'large';  
    } else if (childCount < 50) {  
      c += 'medium';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }")),
                       color = "white",
                       fillColor = ~col(barrier_status),
                       popup = ~label,
                       group = "Barrier",
                       opacity = 1,
                       fillOpacity = 0.90,
                       options = leafletOptions(pane = "maplabels")
      ) %>%
      addCircleMarkers(data = y() %>%
                        dplyr::filter(
                          barrier_status == "POTENTIAL"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-custom-';  
    if (childCount < 5) {  
      c += 'large';  
    } else if (childCount < 50) {  
      c += 'medium';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }")),
                       color = "white",
                       fillColor = ~col(barrier_status),
                       popup = ~label,
                       group = "Potential",
                       opacity = 1,
                       fillOpacity = 0.90,
                       options = leafletOptions(pane = "maplabels")
      ) %>%
      addCircleMarkers(data = y() %>%
                        dplyr::filter(
                          barrier_status == "UNKNOWN"
                        ),
                       lat = ~lat,
                       lng = ~long,

                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-custom-';  
    if (childCount < 5) {  
      c += 'large';  
    } else if (childCount < 50) {  
      c += 'medium';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }")),
                       color = "white",
                       fillColor = ~col(barrier_status),
                       popup = ~label,
                       group = "Unknown",
                       opacity = 1,
                       fillOpacity = 0.90,
                       options = leafletOptions(pane = "maplabels")
                       ) %>%
      addPolylines(data = df_str, color = "blue", opacity = 1, label = ~paste0(gnis_name),
      labelOptions = labelOptions(
        style = list(
          "color" = "black",
          "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
          "font-size" = "15px",
          "border-color" = "rgba(0,0,0,0.5)"
      )),  group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      addPolylines(data = df_null, color = "blue", opacity = 1,  group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      #addPolylines(data = df_nonstr, color = "grey", group = "Non-Streams") %>%
      addPolygons(data = boundary, stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5,
    color = "black", opacity = 1, group = "Watershed<br>Boundary", fillColor = NA, options = leafletOptions(pane = "polygons")) %>%
      addEasyButton(easyButton(
        icon = "fa-home", title = "Deafult View",
        onClick = JS("function(btn, map){ map.setView([52.280408375,	-121.005149476], 10); }"))) %>%
      addLegend("topright", pal = col, values = df$barrier_status) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Mapbox", "Mapbox<br>Satellite"),
        overlayGroups = c("Streams", "Watershed<br>Boundary"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  #zoom_level = JS("function(map){ return map.getZoom(); }")

  #zoom level for tooltip
#   observe({
#     if (is.null(input$mymap_zoom))
#       return()
#     isolate({
#       leafletProxy(
#         mapId = "mymap") %>%
#         addPolylines(data = df_str, color = "blue", opacity = 1,  label = if(input$mymap_zoom < 8) gnis_name,  group = "Streams", options = leafletOptions(pane = "maplabels"))
#     })
# })

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

    datatable(dt, options = list(scrollY = 'calc(100vh - 350px)', ajax = list(url = action)),
      colnames = c("ID", "Stream Name", "Barrier Status", "Potenital Crossing Type", "Latitude", "Longitude", "Location"),
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
      labs(title = "Attribute Summary for HORS", x = "Barrier Status", y = "Proportion %", fill="Barrier Status") +
      scale_fill_manual(values=c("#d52a2a", "#32cd32", "#ffb400", "#965ab3")) +
      theme(plot.title = element_text(hjust = 0.5))
    }
    else if (input$options == "type"){
      df1 <- df %>%
            count(crossing_feature_type) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))

      ggplot(df1, aes(x=crossing_feature_type, y=Perc, fill=crossing_feature_type)) +
      geom_bar(stat="identity") + 
      geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
      labs(title = "Attribute Summary for HORS", x = "Feature Type", y = "Proportion %", fill="Crossing Feature Type") + 
      theme(plot.title = element_text(hjust = 0.5))
    }
  }
  )

  #pie chart attempt
  output$attr_pie <- renderPlot(width = "auto",
  height = "auto",
  res = 150,
  {
    if (input$options == "barr"){
      df1 <- df %>%
            count(barrier_status) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))

      ggplot(df1, aes(x="", y=Perc, fill = barrier_status)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
      labs(title = "Attribute Summary for HORS", x = "Barrier Status", y = "Proportion %", fill="Barrier Status") +
      scale_fill_manual(values=c("#d52a2a", "#32cd32", "#ffb400", "#965ab3")) +
      theme_void() # remove background, grid, numeric labels
    }
    else if (input$options == "type"){
      df1 <- df %>%
            count(crossing_feature_type) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))

      ggplot(df1, aes(x="", y=Perc, fill=crossing_feature_type)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
      labs(title = "Attribute Summary for HORS", x = "Feature Type", y = "Proportion %", fill="Crossing Feature Type") + 
      theme_void() # remove background, grid, numeric labels
    }
  }
  )

  #progressbar plot
  #theme_void() + theme(legend.position="none")
  output$progress <- renderPlot(width = "auto",
    height = "auto",
    res = 150,
    {
    df <- data.frame(perc <- c(7, 93), val <- c("connected", "disconnected"))
    ggplot(data = df, aes(x="", y=perc, fill = val)) + geom_bar(stat="identity", width = 0.1) + coord_flip() +
    geom_text(aes(label=scales::percent(perc/100)), position = position_stack(vjust = .5), color = "white") + 
    theme_void() + theme(legend.position = "none") +
    scale_fill_manual(values = c("#d52a2a", "#32cd32"))
  })

  ###New function
  output$png <- renderPlot({
    pic = readPNG('www/salmon.png')
    plot.new()
    grid::grid.raster(pic)

  })

  #Update connectivity status
  observeEvent(input$refresh, {
      updateProgressBar(session = session, id = "connect", value = watershed_connectivity("ALL"))
    })


}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# finally, we need to call the shinyapp function with the ui and server as arguments
app <- shinyApp(ui, server)


#run app locally
runApp(app)