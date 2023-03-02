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
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(png)
library(shinyBS)
library(shinyjs)





#API call from bcfishpass
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crossings_res <- read_sf("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27HORS%27%20AND%20all_spawningrearing_km%3e0")
df <- crossings_res %>%
      dplyr::mutate(long = sf::st_coordinates(crossings_res)[, 1],
                    lat = sf::st_coordinates(crossings_res)[, 2]) %>%
      dplyr::mutate(link = paste("https://features.hillcrestgeo.ca/bcfishpass/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27HORS%27%20AND%20all_spawningrearing_km%3E0%20AND%20aggregated_crossings_id%20=%20",id,sep = ""))

boundary <- read_sf("https://features.hillcrestgeo.ca/fwa/collections/whse_basemapping.fwa_watershed_groups_poly/items.json?watershed_group_code=HORS")



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#live reporting functions
watershed_connectivity <- function(habitat_type){
  request <- paste('https://features.hillcrestgeo.ca/bcfishpass/functions/postgisftw.wcrp_watershed_connectivity_status/items.json?watershed_group_code=HORS&barrier_type=',habitat_type, sep = "")
  res <- GET(request)
  data <- fromJSON(rawToChar(res$content))
  return(data$connectivity_status)
}

barrier_severity <- function(barrier_type){
  request <- paste('https://features.hillcrestgeo.ca/bcfishpass/functions/postgisftw.wcrp_barrier_severity/items.json?watershed_group_code=HORS&barrier_type=',barrier_type, sep = "")
  res <- GET(request)
  data <- fromJSON(rawToChar(res$content))
  return(c(data$n_assessed_barrier, data$n_assess_total, data$pct_assessed_barrier))
}

barrier_extent <- function(barrier_type){
  request <- paste('https://features.hillcrestgeo.ca/bcfishpass/functions/postgisftw.wcrp_barrier_extent/items.json?watershed_group_code=HORS&barrier_type=',barrier_type, sep = "")
  res <- GET(request)
  data <- fromJSON(rawToChar(res$content))
  return(c(data$all_habitat_blocked_km, data$all_habitat_blocked_pct))
}

barrier_count <- function(barrier_type){
  request <- paste('https://features.hillcrestgeo.ca/bcfishpass/functions/postgisftw.wcrp_barrier_count/items.json?watershed_group_code=HORS&barrier_type=',barrier_type, sep = "")
  res <- GET(request)
  data <- fromJSON(rawToChar(res$content))
  return(c(data$n_passable, data$n_barrier, data$n_potential, data$n_unknown))
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#live statistics
hab_conf <- sum(df$pscis_status == "HABITAT CONFIRMATION", na.rm = TRUE)
assessed <- sum(is.na(df$pscis_assessment_date))
total <- 526.95 #total km in HORS that in the next iteration of the model will be a live function
access <- round(total * (watershed_connectivity("ALL") / 100), 2)
gain <- round(total - access, 2)
gain_goal <- round((total*0.96) - access, 2)
hab_connected <- gain_goal - 14.59
dam_assessed_total <- barrier_severity("DAM")[2]

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
#stats related to priority tables
design <- sum(priority$next_steps == "Design")
remediation <- sum(priority$next_steps == "Remediation")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#misc. tables
acknow <- read.csv("data/acknowledgements.csv")
datadict <- read.csv("data/datadict.csv")
priordict <- read.csv("data/priority_dict.csv")

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
#addResourcePath("www", "C:/CABD/GitHub/Canadian-Wildlife-Federation/BC_WRCP/RshinyApp/www")
#main app page
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  includeCSS("www/simple_app_styling.css"),
  #include JS function
  includeScript("gomap.js"),
  useShinyjs(),
  #create top navbar
  navbarPage("Horsefly River WCRP", #position = "fixed-top",
    #create tabs in nav bar
    tabPanel("Watershed Summary",
             tabsetPanel(id = "prioritytab",
                         tabPanel(value = "Tab_2",
                                  useShinydashboard(),
                                  #add content to tab panel
                                           fluidRow(
                                             box(width = 12, title = "Status of Connectivity", tags$div("This measure of connectivity is based on the percent of total linear habitat available for anadromous salmon in the Horsefly River watershed.", style="font-weight:bold;margin-top: 15px;text-align:center;font-size:15px;font-style:italic"), 
                                                 id = 'constatus',
                                                 #h3("Watershed Overview"),
                                                 shinyWidgets::progressBar(id = "connect",
                                                                       value = watershed_connectivity("ALL"),
                                                                       display_pct = TRUE,
                                                                       status = "custom",
                                                                       total = 100,
                                                                       striped = TRUE),
                                                  tags$div(id="colorstrip-constatus",),
                                                  tags$div(id="constatus-strip-text",
                                                           tags$div(id="poor-text","Poor: 0-80%"),
                                                           tags$div(id="good-text","Good: 81-90%"),
                                                           tags$div(id="verygood-text","Very Good: >90%")
                                                           ),
                                                 br(),
                                                 tags$div(h3("Connectivity Overview"),align="center"),
                                                 hr(),
                                                 fluidRow(id="statattr",
                                                   column(width=6,
                                                          infoBox("Number of barriers on Accessible Streams:", paste0(length(df$aggregated_crossings_id)), icon = icon("solid fa-ban"), fill = TRUE)),
                                                   column(width=6,
                                                          infoBox("Amount of habitat that's been reconnected:", paste0(toString(hab_connected)), icon = icon("solid fa-water"), fill = TRUE)),
                                                   column(width=6,
                                                          infoBox("Amount of stream still blocked:", paste0(toString(gain), " km"), icon = icon("solid fa-road-barrier"), fill = TRUE)),
                                                   column(width=6,
                                                          infoBox("Amount of habitat left to reconnect to connectivity goals:", paste0(toString(gain_goal), " km"), icon = icon("solid fa-house-medical-circle-check"), fill = TRUE)),
                                                 ),
                                                 tags$div(h3("Work Completed to Date"),align="center"),
                                                 hr(),
                                                 fluidRow(
                                                   column(width=3,
                                                          valueBox(assessed, "Assessments Done", icon = icon("solid fa-clipboard-check"))),
                                                   column(width=3,
                                                          valueBox(hab_conf, "Habitat Confirmations", icon = icon("solid fa-seedling"))),
                                                   column(width=3,
                                                          valueBox(paste0(toString(design)), "Designs Created", icon = icon("solid fa-pen-ruler"))),
                                                   column(width=3,
                                                   valueBox(paste0(toString(remediation)), "Remediations Completed", icon = icon("solid fa-person-digging")))
                                             ))),

                                          fluidRow(
                                             column(width=5,
                                                fluidRow(
                                                  box(width = 12, title = "Barrier Types",
                                                    fluidRow(
                                                      selectInput("options", "Select a barrier type:", c("Small Dams (<3 m height)" = "dam", "Road-stream Crossings" = "road", "Trail-stream crossings" = "trail", "Lateral Barriers" = "lateral", "Natural Barriers" = "natural"))
                                                      #bsTooltip("options", "Select your barrier type of interest from the dropdown.", placement = "top", trigger = "hover", options = NULL)
                                                    ),
                                                    fluidRow(
                                                      column(width = 12,
                                                             uiOutput("threat")
                                                             )
                                                    ),
                                                    fluidRow(
                                                             uiOutput("box")
                                                    ),
                                                    fluidRow(
                                                             id = "pass_title",
                                                             h2("Summary of Passability")
                                                    ),
                                                    fluidRow(class = "rowhide",
                                                             plotOutput("attr_pie")
                                                    )
                                                    )
                                                    )
                                                    ),
                                             column(width=7,
                                                    fluidRow(
                                                      box(width = 12, title = "Connectivity Goals", id = 'congoals',
                                                          
                                                          infoBox(paste0("By 2040, the percent (%) of total linear habitat accessible to anadromous salmon will increase from ", toString(watershed_connectivity("ALL")), "% to 96% within the Horsefly River watershed (i.e., reconnect at least ", toString(gain_goal), " km of habitat)."), "", icon = icon("solid fa-1"), fill = TRUE),
                                                          infoBox("By 2024, the total area of overwintering habitat accessible to Anadromous Salmon will increase by 1,500 m2 within the Horsefly River watershed.", "", icon = icon("solid fa-2"), fill = TRUE)
                                                      )),
                                                    fluidRow(
                                                      box(width = 12, title = "Target Species", id = "species",
                                                      #species boxes
                                                      fluidRow(id="speciesrow",
                                                        box(
                                                          tags$img(src = "https://media.fisheries.noaa.gov/dam-migration/750x500-chinook-salmon.jpg?itok=DF6fsFXy", style="display: block;margin-left: auto;margin-right: auto;width: 75%"), 
                                                          tags$figcaption("Credit: Roger Tabor, USFWS", style="text-align:right; font-size:smaller; margin-right:12.5%"),
                                                          br(),
                                                          title = "Chinook Salmon | Kekèsu | Oncorhynchus tshawytscha ",
                                                          "Chinook Salmon are the first to return each year, usually in early August, and have the most limited distribution within the watershed. Known spawning occurs in parts of the Horsefly River mainstem above the confluence with the Little Horsefly River and throughout McKinley Creek as far as Elbow Lake. Important rearing systems include Patenaude Creek, Kroener Creek, Black Creek, Woodjam Creek, Deerhorn Creek, and Wilmot Creek.",
                                                          id = "expanders",
                                                          collapsible = TRUE,
                                                          closable = FALSE,
                                                          collapsed = TRUE
                                                        )),
                                                      fluidRow(id="speciesrow",
                                                        box(
                                                          tags$img(src = "https://media.fisheries.noaa.gov/dam-migration/750x500-coho-salmon.jpg?itok=lan8JjNI", style="display: block;margin-left: auto;margin-right: auto;width: 75%"), 
                                                          tags$figcaption("Credit: NOAA Fisheries", style="text-align:right; font-size:smaller; margin-right:12.5%"),
                                                          br(),
                                                          title = "Coho Salmon | Sxeyqs | Oncorhynchus kisutch",
                                                          "Coho Salmon are the most widely distributed of the three focal species in the watershed, with the ability to migrate into smaller, upper tributary systems (DFO 1991). Spawning occurs in the Little Horsefly River between Gruhs Lake and Horsefly Lake, McKinley Creek below McKinley Lake, Woodjam Creek, Patenaude Creek, Tisdall Creek, and Black Creek. Rearing fry and juveniles have been observed in the Little Horsefly River, Patenaude Creek, and McKinley Creek up to Bosk Lake. ",
                                                          id = "expanders",
                                                          collapsible = TRUE,
                                                          closable = FALSE,
                                                          collapsed = TRUE
                                                        )),
                                                      fluidRow(id="speciesrow",
                                                        box(
                                                          tags$img(src = "https://media.fisheries.noaa.gov/styles/full_width/s3/dam-migration/900x600-sockeye-salmon-noaa.jpg?itok=6e4dVBEy", style="display: block;margin-left: auto;margin-right: auto;width: 75%"), 
                                                          tags$figcaption("Credit: NOAA Fisheries, USFWS", style="text-align:right; font-size:smaller; margin-right:12.5%"),
                                                          br(),
                                                          title = "Sockeye Salmon | Sqlelten7ùwi | Oncorhynchus nerka ",
                                                          "Sockeye Salmon have historically been the most abundant of the three focal species in the watershed, though the population has seen significant declines in recent years (DFO 1991, S. Hocquard pers. comm.). Sockeye Salmon spawning is known to occur throughout the Horsefly River (up to the impassable falls), in the Little Horsefly River between Gruhs Lake and Horsefly Lake, Moffat Creek (up to the impassible falls), and McKinley Creek up to Elbow Lake (PSF 2018, DFO 1991, S. Hocquard pers. comm.). Additionally, a spawning channel aimed at enhancing the Sockeye Salmon population was constructed by Fisheries and Oceans Canada in 1989 (DFO 1991). Currently, there are no Sockeye Salmon rearing in the Horsefly River watershed – all emergent fry migrate down to Quesnel Lake.",
                                                          id = "expanders",
                                                          collapsible = TRUE,
                                                          closable = FALSE,
                                                          collapsed = TRUE
                                                        )))),
                                                    
                                                    fluidRow(
                                                      box(width = 12, title = "First Nations Partners", id = "species",
                                                          fluidRow(
                                                            column(width = 5,
                                                                   div(class="territoriesmap", img(src="territories.png", width = "100%")),
                                                            ),
                                                            column(width = 7,
                                                              p("The Horsefly River watershed comprises parts of Secwepemcúl’ecw, the traditional territory of the Northern Secwepemc te Qelmucw (NStQ), represented by the Northern Shuswap Tribal Council and four member communities or autonomous nations:",
                                                                tags$div(
                                                                  tags$ul(
                                                                    tags$li("Xatśūll Cmetem’ (Soda Creek First Nations)"),
                                                                    tags$li("Stswēceḿc Xgāt’tem (Canoe Creek/Dog Creek First Nations)"),
                                                                    tags$li("T’ēxelc (Williams Lake First Nation)"),
                                                                    tags$li("Tsq’ēsceń (Canim Lake First Nation))"), style="font-size:17px;"
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                      )
                                                    )
                                                  
                                           )),
                                     
                                    )),
    ),
    
    tabPanel("Interactive Map (sidebyside)", 
             tabsetPanel(id = "alltab",
                         tabPanel("", value = "Tab_1", 
                                   #add content to tab panel
                                  fluidRow(id = "row1",
                                    selectInput("priority", "Barrier List", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "All"),
                                    bsTooltip("priority", "Here is some text with your instructions", placement = "top", trigger = "hover", options = NULL),
                                    selectInput("variable", "Barrier Status", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
                                    bsTooltip("variable", "Here is some text with your instructions", placement = "top", trigger = "hover", options = NULL)
                                  ),
                                  tags$hr(style="border-color: transparent;"),
                                  fluidRow(id = "row2",
                                           column(width=7,
                                                  leafletOutput("mymap")
                                           ),
                                           column(width=5,
                                                  dataTableOutput("mytable")
                                           ),
                                  )
                          )
             )
    ),
    
    tabPanel("Interactive Map (stacked)", 
             tabsetPanel(id = "alltabside",
                         tabPanel("", value = "Tab_1_side", 
                                  #add content to tab panel
                                  fluidRow(id = "row1",
                                           selectInput("priority", "Barrier List", c("All" = "All", "Priority" = "Priority", "Intermediate" = "Intermediate"), selected = "All"),
                                           bsTooltip("priority", "Here is some text with your instructions", placement = "top", trigger = "hover", options = NULL),
                                           selectInput("variable", "Barrier Status", c("Passable" = "PASSABLE", "Barrier" = "BARRIER","Potential"="POTENTIAL","Unknown"="UNKNOWN"), selected = c("PASSABLE", "BARRIER","POTENTIAL","UNKNOWN"), multiple = TRUE),
                                           bsTooltip("variable", "Here is some text with your instructions", placement = "top", trigger = "hover", options = NULL)
                                  ),
                                  tags$hr(style="border-color: transparent;"),
                                  fluidRow(id = "row2",
                                           leafletOutput("mymap2"),
                                           br(),
                                           dataTableOutput("mytable2")
                                  )
                         )
             )
    ),
    
    tabPanel("Background Information",
              tabsetPanel(id = "alltab",
                          tabPanel("", value = "Tab_3", 
                                   #add content to tab panel
                                   #fluidRow(
                                     #column(width=12,
                                            fluidRow(style="margin-left:10vw;margin-right:10vw;border-radius:4px;background-color:white;",
                                                     div(class="paras",
                                                           h2("Background"),
                                                           hr(),
                                                           p("Healthy, well-connected streams and rivers within the Horsefly River watershed support thriving populations of migratory fish, improving the overall ecosystem health of the watershed. In turn, these fish provide the continued sustenance, cultural, and ceremonial needs of the Northern Secwépemc people, as they have since time immemorial. Both residents and visitors to the watershed work together to mitigate the negative effects of anthropogenic aquatic barriers, improving the resiliency of streams and rivers for the benefit and appreciation of all.", style="text-align:center;font-style:italic"),
                                                           br(),
                                                           p("The Canadian Wildlife Federation and partners have developed a Watershed Connectivity Remediation Plan (WCRP) for the Horsefly River watershed (Secwepemcúl’ecw). The overall aim of the WCRP is to improve connectivity for anadromous salmon and the livelihoods that they support, including the continued sustenance, cultural, and ceremonial needs of the Northern Secwépemc people. This 20-year plan was developed to identify priority actions that the Horsefly River WCRP planning team (see Table 1 for a list of team members) will undertake between 2021-2040 to conserve and restore fish passage in the watershed, through crossing remediation, lateral barrier remediation, dam remediation, and barrier prevention strategies. The full WCRP can be viewed", a(href="https://horsefly-wcrp.netlify.app/intro.html", " here.")),
                                                           p("This dashboard provides an easy tool for project partners and other interested parties to easily access and explore the watershed connectivity status, project progress, and explore the barriers in the watershed (including the intermediate and priority barrier lists)."),
                                                           h2("Barrier Prioritization"),
                                                           hr(),
                                                           div(class="barrprior", img(src="barrier_prioritization.png", style="width:80%;"),align="center"),
                                                           br(),
                                                           p("The barrier prioritization process comprises three stages:",
                                                             tags$div(
                                                               tags$ul(
                                                                 tags$li("Stage 1: preliminary barrier list"),
                                                                 tags$li("Stage 2: intermediate barrier list"),
                                                                 tags$li("Stage 3: priority barrier list")
                                                             ))),
                                                           p("Initially, the barrier prioritization analysis ranked all barriers in the watershed by the amount of habitat blocked to produce a \"preliminary barrier list\", which also accounted for assessing \"sets\" of barriers for which remediation could be coordinated to maximize connectivity gains. From this list, the top-ranking subset of barriers - comprising more barriers than are needed to achieve the goals - is selected to produce an \"intermediate barrier list\". Barriers that did not rank highly in the model results but were identified as priority barriers by the local partners were also added to the intermediate barrier list. A longer list of barriers is needed due to the inherent assumptions and uncertainty in the connectivity and habitat models and gaps in available data. Barriers that have been modelled (i.e., points where streams and road/rail networks intersect) are assumed to be barriers until field verification is undertaken and structures that have been assessed as \"potential\" barriers (e.g., may be passable at certain flow levels or for certain life history stages) require further investigation before a definitive remediation decision is made. Additionally, the habitat model identifies stream segments that have the potential to support spawning or rearing habitat for target species but does not attempt to quantify habitat quality or suitability, which will require additional field verification once barrier assessments have completed. As such, the intermediate barrier list should be considered as a starting point in the prioritization process and represents structures that are a priority to evaluate further through barrier assessment and habitat confirmations because some structures will likely be passable, others will not be associated with usable habitat, and others may not be feasible to remediate because of logistic considerations."),
                                                           p("The intermediate barrier list was updated following the barrier assessments and habitat confirmations that were undertaken during the 2021 field season - some barriers were moved forward to the \"priority barrier list\" and others were eliminated from consideration due to one or more of the considerations discussed in Table . The priority barrier list represents structures that were confirmed to be partial or full barriers to fish passage and that block access to confirmed habitat. Barriers on the priority list were reviewed by planning team members and selected for inclusion for proactive pursual of remediation.  For more details on the habitat, connectivity, and barrier prioritization models, please see ", a("Appendix A", href="https://horsefly-wcrp.netlify.app/appendixa"), " and ", a("Appendix B", href="https://horsefly-wcrp.netlify.app/appendixb"), " of the WCRP."),
                                                           h2("Data Dictionary"),
                                                           hr(),
                                                           tableOutput("dict"),
                                                           h2("Priority Data Dictionary TEST TO SEE IF IT WORKED"),
                                                           hr(),
                                                           tableOutput("pdict")
                                   ))#))
                          )
              )

      ),
    
    tabPanel("Acknowledgements",
               tabsetPanel(id = "alltab",
                           tabPanel("", value = "Tab_4", 
                                    #add content to tab panel
                                    #fluidRow(
                                      #column(width=12, style="padding-right:15px;padding-left:15px;",
                                             fluidRow(style="margin-left:10vw;margin-right:10vw;border-radius:4px;background-color:white;height:100vh",
                                                      div(class="paras",
                                                        p("This dashboard summarizes the culmination of a collaborative planning process undertaken in the Horsefly River watershed over many months of work with a multi-partner planning team of individuals and groups passionate about the conservation and restoration of freshwater ecosystems and the species they support. Plan development was funded by the BC Salmon Restoration and Innovation Fund, Canada Nature Fund for Aquatic Species at Risk, and the RBC Bluewater Project. We were fortunate to benefit from the feedback, guidance, and wisdom of many groups and individuals who volunteered their time throughout this process — this publication would not have been possible without the engagement of our partners and the planning team."),
                                                        p("We recognize the incredible fish passage and connectivity work that has occurred in the Horsefly River watershed to date, and we are excited to continue partnering with local groups and organizations to build upon existing initiatives and provide a road map to push connectivity remediation forward over the next 20 years and beyond."),
                                                        p("The Canadian Wildlife Federation recognizes that the lands and waters that form the basis of this project are the traditional unceded territory of the Northern Secwepemc people. We are grateful for the opportunity to learn from the stewards of this land and work together to benefit Pacific Salmon. A special thank you to Nishitha Singi for sharing the traditional Secwepemctsín names used in this plan."),
                                                        br(),
                                                        fluidRow(align="center",
                                                          tableOutput("tableawk"))
                                    ))#))

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

#popup formatting for all and intermediate tables
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
df$label <- paste0("<table style=\"border: 1px solid rgb(241, 241, 241)\">
                        <h4>ID: ", df$id, "</h4>
                        <br>
                        <tr class=\"popup\">
                          <th class=\"popup\">Crossing Source:  </th>
                          <th class=\"popup\">", df$crossing_source, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Feature Type:  </th>
                          <th class=\"popup\">", df$crossing_feature_type, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">PSCIS Status:  </th>
                          <th class=\"popup\">", df$pscis_status, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Crossing Type Code:  </th>
                          <th class=\"popup\">", df$crossing_type_code, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Crossing Subtype Code:  </th>
                          <th class=\"popup\">", df$crossing_subtype_code, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Barrier Status:  </th>
                          <th class=\"popup\">", df$barrier_status, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">PSCIS Road Name:  </th>
                          <th class=\"popup\">", df$pscis_road_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">PSCIS Stream Name:  </th>
                          <th class=\"popup\">", df$pscis_stream_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">PSCIS Assessment Comment:  </th>
                          <th class=\"popup\">", df$pscis_assessment_comment, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">PSCIS Assessment Date:  </th>
                          <th class=\"popup\">", df$pscis_assessment_date, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Dam Name:  </th>
                          <th class=\"popup\">", df$dam_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Dam Owner:  </th>
                          <th class=\"popup\">", df$dam_owner, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">GNIS Stream Name:  </th>
                          <th class=\"popup\">", df$gnis_stream_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Stream Order:  </th>
                          <th class=\"popup\">", df$stream_order, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Anthropogenic Barriers:  </th>
                          <th class=\"popup\">", df$barriers_anthropogenic_dnstr, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Count of Anthropogenic Barriers:  </th>
                          <th class=\"popup\">", df$barriers_anthropogenic_dnstr_count, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">All Habitat Blocked:  </th>
                          <th class=\"popup\">", df$all_spawningrearing_km, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Upstream Habitat Blocked:  </th>
                          <th class=\"popup\">", df$all_spawningrearing_belowupstrbarriers_km, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">geoJSON  </th>
                          <th class=\"popup\"><a href =",  df$link, " target='_blank'>Data Sheet</a></th>
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
                          <th class=\"popup\">Stream Name:  </th>
                          <th class=\"popup\">", df$stream_name, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Feature Type:  </th>
                          <th class=\"popup\">", df$barrier_type, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Priority:  </th>
                          <th class=\"popup\">", df$priority, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Owner:  </th>
                          <th class=\"popup\">", df$owner, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Habitat Quality (Upstream):  </th>
                          <th class=\"popup\">", df$upstr_hab_quality, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Road Name:  </th>
                          <th class=\"popup\">", df$next_steps, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Reason:  </th>
                          <th class=\"popup\">", df$reason, "</th>
                        <tr>
                        <tr class=\"popup\">
                          <th class=\"popup\">Notes:  </th>
                          <th class=\"popup\">", df$notes, "</th>
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
      addPolylines(data = df_str, color = "cadetblue", weight = 1.5, opacity = 1, label = ~paste0(gnis_name),
      labelOptions = labelOptions(
        style = list(
          "color" = "black",
          "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
          "font-size" = "15px",
          "border-color" = "rgba(0,0,0,0.5)"
      )),  group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      addPolylines(data = df_null, color = "cadetblue", weight = 1.5, opacity = 1,  group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      #addPolylines(data = df_nonstr, color = "grey", group = "Non-Streams") %>%
      addPolygons(data = boundary, stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5,
    color = "red", weight = 2, opacity = 1, group = "Watershed<br>Boundary", fillColor = NA, options = leafletOptions(pane = "polygons")) %>%
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

    datatable(dt, options = list(scrollY = 'calc(100vh - 350px)', ajax = list(url = action), columnDefs = list(list(visible=FALSE, targets=c(5,6)))),
      colnames = c("ID", "Stream Name", "Barrier Status", "Potenital Crossing Type", "Latitude", "Longitude", "Location"),
      escape = FALSE,
      selection = "none",
      style = "bootstrap"
      )

    })
  
  ###################################################################################################
  ##########################DELETE DUPE CODE BELOW ONCE LAYOUT IS DECIDED############################
  ###################################################################################################
  
  #leaflet map rendering
  output$mymap2 <- renderLeaflet({
    
    
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
      addPolylines(data = df_str, color = "cadetblue", weight = 1.5, opacity = 1, label = ~paste0(gnis_name),
                   labelOptions = labelOptions(
                     style = list(
                       "color" = "black",
                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                       "font-size" = "15px",
                       "border-color" = "rgba(0,0,0,0.5)"
                     )),  group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      addPolylines(data = df_null, color = "cadetblue", weight = 1.5, opacity = 1,  group = "Streams", options = leafletOptions(pane = "maplabels")) %>%
      #addPolylines(data = df_nonstr, color = "grey", group = "Non-Streams") %>%
      addPolygons(data = boundary, stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5,
                  color = "red", weight = 2, opacity = 1, group = "Watershed<br>Boundary", fillColor = NA, options = leafletOptions(pane = "polygons")) %>%
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
  output$mytable2 <- renderDataTable({
    
    dt <- y()[, c("id", "pscis_stream_name", "barrier_status", "crossing_feature_type", "lat", "long")] %>%
      #  dplyr::filter(
      #   barrier_status %in% input$variable
      # ) %>%
      st_drop_geometry() %>%
      mutate(Location = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    
    
    action <- DT::dataTableAjax(session, dt, outputId = "mytable")
    
    datatable(dt, options = list(scrollY = 'calc(100vh - 350px)', ajax = list(url = action), columnDefs = list(list(visible=FALSE, targets=c(5,6)))),
              colnames = c("ID", "Stream Name", "Barrier Status", "Potenital Crossing Type", "Latitude", "Longitude", "Location"),
              escape = FALSE,
              selection = "none",
              style = "bootstrap"
    )
    
  })
  ##############################################################################################
  ##############################################################################################
  ##############################################################################################
  
  # Stats Tab panel ---------------------------------------------------------------------
  # output$attr_bar <- renderPlot(width = "auto",
  # height = "auto",
  # res = 150,
  # {
  #   if (input$options == "barr"){
  #     df1 <- df %>%
  #           count(barrier_status) %>%
  #           mutate(Perc = (n/sum(n)) * 100) %>%
  #           mutate(Freq = n/sum(n))

  #     ggplot(df1, aes(x=barrier_status, y=Perc, fill=barrier_status)) +
  #     geom_bar(stat="identity") + 
  #     geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
  #     labs(title = "Attribute Summary for HORS", x = "Barrier Status", y = "Proportion %", fill="Barrier Status") +
  #     scale_fill_manual(values=c("#d52a2a", "#32cd32", "#ffb400", "#965ab3")) +
  #     theme(plot.title = element_text(hjust = 0.5))
  #   }
  #   else if (input$options == "type"){
  #     df1 <- df %>%
  #           count(crossing_feature_type) %>%
  #           mutate(Perc = (n/sum(n)) * 100) %>%
  #           mutate(Freq = n/sum(n))

  #     ggplot(df1, aes(x=crossing_feature_type, y=Perc, fill=crossing_feature_type)) +
  #     geom_bar(stat="identity") + 
  #     geom_text(aes(label=scales::percent(Freq)), position = position_stack(vjust = .5)) +
  #     labs(title = "Attribute Summary for HORS", x = "Feature Type", y = "Proportion %", fill="Crossing Feature Type") + 
  #     theme(plot.title = element_text(hjust = 0.5))
  #   }
  # }
  # )

  #pie chart
  output$attr_pie <- renderPlot(width = "auto",
  height = "auto",
  bg="transparent",
  res = 150,
  {
    if (input$options == "dam"){
      df1 <- df %>%
            filter(crossing_feature_type == "DAM") %>%
            count(barrier_status) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))
            # Compute the cumulative percentages (top of each rectangle)
            df1$ymax <- cumsum(df1$Perc)
            # Compute the bottom of each rectangle
            df1$ymin <- c(0, head(df1$ymax, n=-1))
            # Compute label position
            df1$labelPosition <- (df1$ymax + df1$ymin) / 2
            # Compute a good label
            df1$label <- paste0(df1$barrier_status, ": ", scales::percent(df1$Freq))

      ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, x="", y=Perc, fill = barrier_status)) +
      geom_rect() +
      #geom_bar(stat="identity", width=1, color="#f5f5f5") +
      coord_polar("y", start=0) +
      geom_text( x=4.75, aes(y=labelPosition, label=label), size=3) + # x here controls label position (inner / outer)
      labs(x = "Barrier Status", y = "Proportion %", fill="Barrier Status") +
      scale_fill_manual(values=c("#d52a2a", "#32cd32", "#ffb400", "#965ab3")) +
      theme_void() + # remove background, grid, numeric labels 
      theme(legend.position="none") 
    }
    else if (input$options == "road"){
      df1 <- df %>%
            filter(crossing_feature_type == "ROAD, RESOURCE/OTHER" | crossing_feature_type == "ROAD, DEMOGRAPHIC") %>%
            count(barrier_status) %>%
            mutate(Perc = (n/sum(n)) * 100) %>%
            mutate(Freq = n/sum(n))

            # Compute the cumulative percentages (top of each rectangle)
            df1$ymax <- cumsum(df1$Perc)
            # Compute the bottom of each rectangle
            df1$ymin <- c(0, head(df1$ymax, n=-1))
            # Compute label position
            df1$labelPosition <- (df1$ymax + df1$ymin) / 2
            # Compute a good label
            df1$label <- paste0(df1$barrier_status, ": ", scales::percent(df1$Freq))

      ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, x="", y=Perc, fill = barrier_status)) +
      #geom_bar(stat="identity", width=1, color="#f5f5f5") +
      geom_rect() +
      coord_polar("y", start=0) +
      geom_text( x=4.25, aes(y=labelPosition, label=label), size=2.5) + # x here controls label position (inner / outer)
      #geom_text(aes(x = 1.6, label=paste(barrier_status, scales::percent(Freq))), color = "black", size = 2, position = labelPosition, check_overlap = TRUE) +
      labs(x = "Barrier Status", y = "Proportion %", fill="Barrier Status") +
      scale_fill_manual(values=c("#d52a2a", "#32cd32", "#ffb400", "#965ab3")) +
      theme_void() + # remove background, grid, numeric labels 
      theme(legend.position="none") 
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

  #Update connectivity status
  observeEvent(input$refresh, {
      updateProgressBar(session = session, id = "connect", value = watershed_connectivity("ALL"))
    })
  
  #Rendering Acknowledgements Table
  output$tableawk <- renderTable(acknow)

  #rendering data dictionary
  output$dict <- renderTable(datadict)
  output$pdict <- renderTable(priordict)

  #updating boxes given dropdown value

  output$threat <- renderUI({
    if (input$options == "dam") {
      column(width=5, infoBox("Overall Threat Rating", "MEDIUM", icon = icon("solid fa-circle-exclamation"), color = "yellow", fill = TRUE))
    } else if (input$options == "road") {
      column(width=5, infoBox("Overall Threat Rating", "VERY HIGH", icon = icon("solid fa-circle-exclamation"), color = "red", fill = TRUE))
    } else if (input$options == "trail") {
      column(width=5, infoBox("Overall Threat Rating", "LOW", icon = icon("solid fa-circle-exclamation"), color = "olive", fill = TRUE))
    } else if (input$options == "natural") {
      column(width=5, infoBox("Overall Threat Rating", "LOW", icon = icon("solid fa-circle-exclamation"), color = "olive", fill = TRUE))
    } else if (input$options == "lateral") {
      column(width=5, infoBox("Overall Threat Rating", "HIGH", icon = icon("solid fa-circle-exclamation"), color = "orange", fill = TRUE))
    }
  })

  observe({
    if (input$options == "dam" | input$options == "road") {
      shinyjs::show(selector = ".rowhide")
    } else {
      shinyjs::hide(selector = ".rowhide")
    }
  })

  observe({
    if (input$options == "dam" | input$options == "road") {
      shinyjs::showElement(id = "pass_title")
    } else {
      shinyjs::hideElement(id = "pass_title")
    }
  })

  observe({
    if (input$priority == "All") {
      shinyjs::showElement(id = "variable")
    } else {
      shinyjs::hideElement(id = "variable")
    }
  })

  output$box <- renderUI({
    if (input$options == "dam") {
      p(paste0("There are ", toString(dam_assessed_total), " mapped small dams on “potentially accessible” stream segments in the watershed, blocking a total of ", toString(barrier_extent("DAM")[1]), " km (~", toString(barrier_extent("DAM")[2]), "% of the total blocked habitat) of modelled spawning and rearing habitat for anadromous salmon, resulting in a Medium extent. The extent rating of these structures was confirmed by the planning team.There are two known fish-passage structures in the watershed, including on the dam at the outlet of McKinley Lake. The remaining dams likely block passage for anadromous salmon and would require significant resources to remediate. However, due to the limited extent of dams in the watershed, a final pressure rating of Medium was assigned. Four small dams were identified on the priority barrier list. Three of the dams require further assessment and confirmation of upstream habitat quality, and the dam observed at the outlet of Kwun Lake does not exist."), style="font-size:17px",
        )
    } else if (input$options == "road") {
      p(paste0("Road-stream crossings are the most abundant barrier type in the watershed, with ", toString(length(df$aggregated_crossings_id)) ," assessed and modelled crossings located on stream segments with modelled habitat. Demographic road crossings (highways, municipal, and paved roads) block ",  toString(barrier_extent("ROAD,%20DEMOGRAPHIC")[1]), " km of habitat (~", toString(barrier_extent("ROAD,%20DEMOGRAPHIC")[2]), "% of the total blocked habitat), with ", toString(barrier_severity("ROAD,%20DEMOGRAPHIC")[3]), "% of assessed crossings having been identified as barriers to fish passage. Resource roads block ", toString(barrier_extent("ROAD,%20RESOURCE/OTHER")[1]), " km of habitat (~", toString(barrier_extent("ROAD,%20RESOURCE/OTHER")[2]), "%), with ", toString(barrier_severity("ROAD,%20RESOURCE/OTHER")[3]), "% of assessed crossings having been identified as barriers. The planning team felt that the data was underestimating the severity of road-stream crossing barriers in the watershed, and therefore decided to update the rating from High to Very High. The planning team also felt that an irreversibility rating of Medium was appropriate due to the technical complexity and resources required to remediate road-stream crossings."), style="font-size:17px",               
        )
    } else if (input$options == "trail") {
      p("There is very little spatial data available on trail-stream crossings in the watershed, so the planning team was unable to quantify the true Extent and Severity of this barrier type. However, the planning team felt that trail-stream crossings are not prevalent within the watershed and that, where they do exist, they do not significantly impact passage for anadromous salmon. As most crossings will be fords or similar structures, remediation may not be required, or remediation costs associated with these barriers would be quite low. Overall, the planning team felt that the pressure rating for trail-stream crossings was likely Low; however, the lack of ground-truthed evidence to support this rating was identified as a knowledge gap within this plan.",  style="font-size:17px",
        )
    } else if (input$options == "natural") {
      p("Natural barriers to fish passage can include debris flows, log jams, sediment deposits, etc., but natural features that have always restricted fish passage (e.g., waterfalls) are not considered under this barrier type. Natural barriers are difficult to include in a spatial prioritization framework due to their transient nature. The planning team identified known natural barriers that occur throughout the watershed, such as beaver dams and log jams. Generally, these natural barriers are only severe impediments to fish passage during low-flow years, but reduced baseflows have become more common in recent years. Based on this, the planning team felt that natural barriers will be severe most years where they exist, but are mostly reversible, resulting in an overall pressure rating of Low.", style="font-size:17px",
        )
    } else if (input$options == "lateral") {
      p("There are numerous types of lateral barriers that potentially occur in the watershed, including dykes, berms, and linear development (i.e., road and rail lines), all of which can restrict the ability of anadromous salmon to move into floodplains, riparian wetlands, and other off-channel habitats. No comprehensive lateral barrier data exists within the watershed, so pressure ratings were based on qualitative local knowledge. Lateral barriers are not thought to be as prevalent as road- or rail-stream crossings but are likely very severe where they do exist. Significant lateral barriers are known to occur along the mainstem of the Horsefly River, which disconnect the mainstem river from historic floodplain and off-channel habitat. Overall, the planning team decided that a High pressure rating adequately captured the effect that lateral barriers are having on connectivity in the watershed. Work to begin quantifying and mapping lateral habitat will begin in 2022-23, as described in the Operational Plan under Strategy 2: Lateral barrier remediation. ", style="font-size:17px",
        )
    }
  })
  # observeEvent(output$options, {
  #   if (input$options == "dam") {
  #     updateBox(session = session,
  #               id = "expanders1",
  #               "There are nine mapped small dams on “potentially accessible” stream segments in the watershed, blocking a total of 8.09 km (~23% of the total blocked habitat) of modelled spawning and rearing habitat for anadromous salmon, resulting in a Medium extent. The extent rating of these structures was confirmed by the planning team.There are two known fish-passage structures in the watershed, including on the dam at the outlet of McKinley Lake. The remaining dams likely block passage for anadromous salmon and would require significant resources to remediate. However, due to the limited extent of dams in the watershed, a final pressure rating of Medium was assigned. Four small dams were identified on the priority barrier list. Three of the dams require further assessment and confirmation of upstream habitat quality, and the dam observed at the outlet of Kwun Lake does not exist.",
  #               title = "Small Dams (<3 m height)"
  #               )
  #   } else if (input$options == "road") {
  #     updateBox(session = session,
  #               id = "expanders1",
  #               "Road-stream crossings are the most abundant barrier type in the watershed, with 103 assessed and modelled crossings located on stream segments with modelled habitat. Demographic road crossings (highways, municipal, and paved roads) block 7.31 km of habitat (~21% of the total blocked habitat), with 73% of assessed crossings having been identified as barriers to fish passage. Resource roads block 19.57 km of habitat (~56%), with 60% of assessed crossings having been identified as barriers. The planning team felt that the data was underestimating the severity of road-stream crossing barriers in the watershed, and therefore decided to update the rating from High to Very High. The planning team also felt that an irreversibility rating of Medium was appropriate due to the technical complexity and resources required to remediate road-stream crossings.",               
  #               title = "Road-stream Crossings"
  #               )
  #   }
  # })


                                                          # box(
                                                          #   title = "Road-stream Crossings",
                                                          #   "Road-stream crossings are the most abundant barrier type in the watershed, with 103 assessed and modelled crossings located on stream segments with modelled habitat. Demographic road crossings (highways, municipal, and paved roads) block 7.31 km of habitat (~21% of the total blocked habitat), with 73% of assessed crossings having been identified as barriers to fish passage. Resource roads block 19.57 km of habitat (~56%), with 60% of assessed crossings having been identified as barriers. The planning team felt that the data was underestimating the severity of road-stream crossing barriers in the watershed, and therefore decided to update the rating from High to Very High. The planning team also felt that an irreversibility rating of Medium was appropriate due to the technical complexity and resources required to remediate road-stream crossings.",
                                                          #   id = "expanders",
                                                          #   collapsible = TRUE,
                                                          #   closable = FALSE,
                                                          #   collapsed = TRUE
                                                          # ),
                                                          # box(
                                                          #   title = "Trail-stream crossings",
                                                          #   "There is very little spatial data available on trail-stream crossings in the watershed, so the planning team was unable to quantify the true Extent and Severity of this barrier type. However, the planning team felt that trail-stream crossings are not prevalent within the watershed and that, where they do exist, they do not significantly impact passage for anadromous salmon. As most crossings will be fords or similar structures, remediation may not be required, or remediation costs associated with these barriers would be quite low. Overall, the planning team felt that the pressure rating for trail-stream crossings was likely Low; however, the lack of ground-truthed evidence to support this rating was identified as a knowledge gap within this plan.",
                                                          #   id = "expanders",
                                                          #   collapsible = TRUE,
                                                          #   closable = FALSE,
                                                          #   collapsed = TRUE
                                                          # ),
                                                          # box(
                                                          #   title = "Lateral Barriers",
                                                          #   "There are numerous types of lateral barriers that potentially occur in the watershed, including dykes, berms, and linear development (i.e., road and rail lines), all of which can restrict the ability of anadromous salmon to move into floodplains, riparian wetlands, and other off-channel habitats. No comprehensive lateral barrier data exists within the watershed, so pressure ratings were based on qualitative local knowledge. Lateral barriers are not thought to be as prevalent as road- or rail-stream crossings but are likely very severe where they do exist. Significant lateral barriers are known to occur along the mainstem of the Horsefly River, which disconnect the mainstem river from historic floodplain and off-channel habitat. Overall, the planning team decided that a High pressure rating adequately captured the effect that lateral barriers are having on connectivity in the watershed. Work to begin quantifying and mapping lateral habitat will begin in 2022-23, as described in the Operational Plan under Strategy 2: Lateral barrier remediation. ",
                                                          #   id = "expanders",
                                                          #   collapsible = TRUE,
                                                          #   closable = FALSE,
                                                          #   collapsed = TRUE
                                                          # ),
                                                          # box(
                                                          #   title = "Natural Barriers",
                                                          #   "Natural barriers to fish passage can include debris flows, log jams, sediment deposits, etc., but natural features that have always restricted fish passage (e.g., waterfalls) are not considered under this barrier type. Natural barriers are difficult to include in a spatial prioritization framework due to their transient nature. The planning team identified known natural barriers that occur throughout the watershed, such as beaver dams and log jams. Generally, these natural barriers are only severe impediments to fish passage during low-flow years, but reduced baseflows have become more common in recent years. Based on this, the planning team felt that natural barriers will be severe most years where they exist, but are mostly reversible, resulting in an overall pressure rating of Low.",
                                                          #   id = "expanders",
                                                          #   collapsible = TRUE,
                                                          #   closable = FALSE,
                                                          #   collapsed = TRUE
                                                          # )
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# finally, we need to call the shinyapp function with the ui and server as arguments
#app <- 
shinyApp(ui, server)


#run app locally if using a code editor other than RStudio
###########################################################
### MAKE SURE LINE BELOW IS COMMENTED OUT WHEN DEPLOYED ###
###########################################################
#runApp(app)
