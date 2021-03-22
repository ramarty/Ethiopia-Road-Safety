# Ethiopia Road Safety Dashboard: App

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readxl)
library(rgdal)
library(data.table)
library(rgeos)
library(doBy)
library(bit64)
library(stargazer)
library(grid)
library(gridExtra)
library(xtable)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)
library(raster)
library(stargazer)
library(ggalluvial)
library(rsconnect)
library(colorRamps)

load("data_for_shiny.Rda")
#gsub("crush|Crush", accidents_all$`Cause of Accident`

# * * * * * ui =================================================================
ui <- dashboardPage(
  
  # Tab Headers ----------------------------------------------------------------
  dashboardHeader(title = "Crashes Along Addis Adama Expressway", titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crashes Along Expressway", tabName = "maps_trends", icon = icon("globe", lib = "glyphicon")),
      menuItem("Crashes Over Time", tabName = "Crashes_over_time", icon = icon("calendar", lib = "glyphicon")),
      menuItem("Crash Cause and Type", tabName = "accident_cause_type", icon = icon("stats", lib = "glyphicon")),
      menuItem("Crash Characteristics", tabName = "accident_characteristics", icon = icon("stats", lib = "glyphicon")),
      menuItem("Predicting Crash Occuring", tabName = "regression_accident_hour", icon = icon("stats", lib = "glyphicon")),
      menuItem("Predicting Crash Location", tabName = "regression_predict_where", icon = icon("stats", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # Tab: Map Trends --------------------------------------------------------
      tabItem(tabName = "maps_trends",
              fluidRow(
                column(8,
                       box(leafletOutput("plot1", height = 500),width=12),
                       box(plotlyOutput("plot2", height = 275),width=12),
                       h2("Crash Hotspots"),
                       box(tableOutput('plot_hotspot_Total'),width=4,title="Total"),
                       box(tableOutput('plot_hotspot_Adama'),width=4,title="Towards Adama"),
                       box(tableOutput('plot_hotspot_Addis'),width=4,title="Towards Addis Ababa")
                ),
                column(4,
                       tabBox(width=12,
                              tabPanel("Crash Cause",
                                       checkboxGroupInput("accident_cause", 
                                                          "", 
                                                          choices = accident_cause_simple_list,
                                                          selected = accident_cause_simple_list),
                                       checkboxInput("accident_cause_allnone","All/None",value=TRUE)
                              ),
                              tabPanel("Crash Type",
                                       checkboxGroupInput("accident_type", 
                                                          "", 
                                                          choices = accident_type_simple_list,
                                                          selected = accident_type_simple_list),
                                       checkboxInput("accident_type_allnone","All/None",value=TRUE))
                       ),
                       box(width=12,
                           checkboxGroupInput("accident_year", 
                                              "Year", 
                                              choices = list("2015" = 2015,
                                                             "2016" = 2016,
                                                             "2017" = 2017),
                                              selected = 2015:2017, inline=T),
                           
                           checkboxGroupInput("vehicle_type",
                                              "Vehicle Type",
                                              choices=vehicle_type_str_list,
                                              selected=vehicle_type_str_list,
                                              inline=T),
                           
                           radioButtons("accident_vs_accidenttraffic", 
                                        "Type", 
                                        choices = list("Number of Crashes" = "accident",
                                                       "Number of Crashes per 1000 vehicles" = "accident_traffic"),
                                        selected = "accident")
                           
                       ),
                       box(width=12,
                           numericInput("number_rows_show_hotspot",
                                        "Rows to Show in Tables",
                                        value=10)
            )
          )
        )
      ),
      # -- End Tab
      
      # Tab: Regressions -------------------------------------------------------
      tabItem(tabName = "regression_accident_hour",
              h2("Predicting Whether a Crash Occured Within an Hour"),
              h5("This page examines what factors are associated with a traffic 
                 crash occuring within an hour. The dependent variable is
                 a binary variable indicating whether a crash occured in the
                 hour. The unit of analysis is each hour across the time frame. 
                 Subsetting the crash categories will change the dependent
                 variable. Subsetting the year will change the timeframe 
                 of the analysis."),
              fluidRow(
                column(5,
                       box(uiOutput("plot3", height = 700),width=12)
                ),
                column(4,
                       tabBox(width=12,
                              tabPanel("Crash Cause",
                                       checkboxGroupInput("accident_cause2", 
                                                          "", 
                                                          choices = accident_cause_simple_list,
                                                          selected = accident_cause_simple_list),
                                       checkboxInput("accident_cause_allnone2","All/None",value=TRUE)),
                              tabPanel("Crash Type",
                                       checkboxGroupInput("accident_type2", 
                                                          "", 
                                                          choices = accident_type_simple_list,
                                                          selected = accident_type_simple_list),
                                       checkboxInput("accident_type_allnone2","All/None",value=TRUE))
                       ),
                       box(width=12,
                           checkboxGroupInput("accident_year2", 
                                              "Year", 
                                              choices = list("2015" = 2015,
                                                             "2016" = 2016,
                                                             "2017" = 2017),
                                              selected = 2015:2017)
                       )),
                column(3,
                       box(width=12,
                           title="Select Control Variables",
                           checkboxGroupInput("controlvars2", 
                                              "", 
                                              choices = list("Traffic (1000 Vehicles)" = "traffic",
                                                             "Median Speed (km/hr)" = "speed_perc_0.5",
                                                             "Driving Towards Addis" = "direction",
                                                             "Friday or Saturday" = "friday_saturday",
                                                             "Holiday (+/- 1 day)" = "holidays_1day_buff",
                                                             "Precpitation (mm)" = "precipitation",
                                                             "Early Morning or Night" = "night_earlymorning"),
                                              selected = c("traffic",
                                                           "speed_perc_0.5",
                                                           "direction",
                                                           "friday_saturday",
                                                           "holidays_1day_buff",
                                                           "precipitation",
                                                           "night_earlymorning"))
                       )
                )
              )
              
              
      ),
      # -- End Tab
      
      # Tab: Regressions Predict Where -----------------------------------------
      tabItem(tabName = "regression_predict_where",
              h2("Predicting Where a Crash Occured"),
              fluidRow(
                column(8,
                       box(uiOutput("plot4", height = 700),width=12)
                ),
                column(4,
                       tabBox(width=12,
                              tabPanel("Crash Cause",
                                       checkboxGroupInput("accident_cause3", 
                                                          "", 
                                                          choices = accident_cause_simple_list,
                                                          selected = accident_cause_simple_list),
                                       checkboxInput("accident_cause_allnone3","All/None",value=TRUE)),
                              tabPanel("Crash Type",
                                       checkboxGroupInput("accident_type3", 
                                                          "", 
                                                          choices = accident_type_simple_list,
                                                          selected = accident_type_simple_list),
                                       checkboxInput("accident_type_allnone3","All/None",value=TRUE))
                       ),
                       box(width=12,
                           checkboxGroupInput("accident_year3", 
                                              "Year", 
                                              choices = list("2015" = 2015,
                                                             "2016" = 2016,
                                                             "2017" = 2017),
                                              selected = 2015:2017),
                           radioButtons("binary_dep3", 
                                        "Continuous or Binary Dependent Variable ", 
                                        choices = list("Continuous" = "Continuous",
                                                       "Binary" = "Binary"),
                                        selected = "Continuous")
                       ),
                       box(width=12,
                           checkboxGroupInput("controlvars3", 
                                              "Select Control Variables", 
                                              choices = list("Distance Nearest Toll Plaze (1km)" = "dist_plaza_nearest_km",
                                                             "Average Turn Angle Within 1km" = "turnangle_500m"),
                                              selected = c("dist_plaza_nearest_km",
                                                           "turnangle_500m"))
                       )
                       
                )
              )
              
      ),
      # -- End Tab
      
      # Tab: Accident Cause Type -----------------------------------------------
      tabItem(tabName = "accident_cause_type",
              h2("Accident Causes and Types"),
              fluidRow(
                column(8,
                       box(plotOutput("plot5", height = 700),width=12)
                ),
                column(4,
                       tabBox(width=12,
                              tabPanel("Crash Cause",
                                       checkboxGroupInput("accident_cause4", 
                                                          "", 
                                                          choices = accident_cause_simple_list,
                                                          selected = accident_cause_simple_list),
                                       checkboxInput("accident_cause_allnone4","All/None",value=TRUE)),
                              tabPanel("Crash Type",
                                       checkboxGroupInput("accident_type4", 
                                                          "", 
                                                          choices = accident_type_simple_list,
                                                          selected = accident_type_simple_list),
                                       checkboxInput("accident_type_allnone4","All/None",value=TRUE))
                       ),
                       box(width=12,
                           checkboxGroupInput("accident_year4", 
                                              "Year", 
                                              choices = list("2015" = 2015,
                                                             "2016" = 2016,
                                                             "2017" = 2017),
                                              selected = 2015:2017)
                       )
                       
                )
              )
              
      ),
      # -- End Tab
      
      # Tab: Crashes Over Time -------------------------------------------------
      tabItem(tabName = "Crashes_over_time",
              h2("Crashes Over Time"),
              fluidRow(
                column(9,
                       box(plotlyOutput("plot6", height = 300),width=12),
                       box(plotOutput("plot7", height = 300),width=12)
                ),
                column(3,
                       tabBox(width=12,
                              tabPanel("Accident Cause",
                                       checkboxGroupInput("accident_cause5", 
                                                          "", 
                                                          choices = accident_cause_simple_list,
                                                          selected = accident_cause_simple_list),
                                       checkboxInput("accident_cause_allnone5","All/None",value=TRUE)),
                              tabPanel("Accident Type",
                                       checkboxGroupInput("accident_type5", 
                                                          "", 
                                                          choices = accident_type_simple_list,
                                                          selected = accident_type_simple_list),
                                       checkboxInput("accident_type_allnone5","All/None",value=TRUE))
                       )
                )
                
              )
      ),
      # -- End Tab
      
      # Tab: Accident Characteristics ------------------------------------------
      tabItem(tabName = "accident_characteristics",
              h2("Crash Characteristics"),
              fluidRow(
                column(9,
                       box(plotOutput("plot8", height = 300),width=6,title="Hour"),
                       box(plotOutput("plot9", height = 300),width=6,title="Day of Week"),
                       box(plotOutput("plot10", height = 300),width=6,title="Weather"),
                       box(plotOutput("plot11", height = 300),width=6,title="Vehicle Type")
                ),
                column(3,
                       tabBox(width=12,
                              tabPanel("Crash Cause",
                                       checkboxGroupInput("accident_cause6", 
                                                          "", 
                                                          choices = accident_cause_simple_list,
                                                          selected = accident_cause_simple_list),
                                       checkboxInput("accident_cause_allnone6","All/None",value=TRUE)),
                              tabPanel("Crash Type",
                                       checkboxGroupInput("accident_type6", 
                                                          "", 
                                                          choices = accident_type_simple_list,
                                                          selected = accident_type_simple_list),
                                       checkboxInput("accident_type_allnone6","All/None",value=TRUE))
                       ),
                       box(width=12,
                           checkboxGroupInput("accident_year6", 
                                              "Year", 
                                              choices = list("2015" = 2015,
                                                             "2016" = 2016,
                                                             "2017" = 2017),
                                              selected = 2015:2017)
            )
          )
        )
      )
      # -- End Tab
      
      # End Parentheses --------------------------------------------------------
    )))

# * * * * * Server =============================================================
server <- function(input, output, session){
  
  # Map ------------------------------------------------------------------------
  output$plot1 <- renderLeaflet({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause) & (accidents_all$accident_type_simple %in% input$accident_type) & (accidents_all$year %in% input$accident_year) & (accidents_all$vehicle_type_str %in% input$vehicle_type),]
    
    # Accidents by km
    accidents_km <- summaryBy(lat+long+N~km,data=accidents_all@data,FUN=c(sum,mean))
    accidents_km$popup_text <- paste0("Accidents: ", accidents_km$N.sum,"<br>Km from Addis Ababa: ", accidents_km$km)
    
    coordinates(accidents_km) <- ~long.mean+lat.mean
    crs(accidents_km) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # Accidents by lat/lon
    accidents_all_sum <- summaryBy(N~lat+long+accident_loc_dist,data=accidents_all@data,FUN=sum,keep.names=T)
    accidents_all_sum$popup_text <- paste0("Accidents: ", accidents_all_sum$N,"<br>Km from Addis Ababa: ", accidents_all_sum$accident_loc_dist/1000)
    
    coordinates(accidents_all_sum) <- ~long+lat
    crs(accidents_all_sum) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # Convert to Traffic/Accidents
    years <- input$accident_year

    #addis_adama_points_data_subset_round$traffic_to_adama_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_adama_",years))) %>% rowSums
    #addis_adama_points_data_subset_round$traffic_to_addis_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_addis_",years))) %>% rowSums
    addis_adama_points_data_subset_round$traffic_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_",years))) %>% rowSums
    accidents_km <- merge(accidents_km, addis_adama_points_data_subset_round, all.x=T,all.y=F, by.x="km", by.y="km_round")
    accidents_km$N.sum_traffic <- accidents_km$N.sum / accidents_km$traffic_sub_years * 1000
    
    #addis_adama_points_data_subset$traffic_to_adama_sub_years <- subset(addis_adama_points_data_subset, select=c(paste0("traffic_to_adama_",years))) %>% rowSums
    #addis_adama_points_data_subset$traffic_to_addis_sub_years <- subset(addis_adama_points_data_subset, select=c(paste0("traffic_to_addis_",years))) %>% rowSums
    addis_adama_points_data_subset$traffic_sub_years <- subset(addis_adama_points_data_subset, select=c(paste0("traffic_",years))) %>% rowSums
    accidents_all_sum <- merge(accidents_all_sum, addis_adama_points_data_subset, by.x="accident_loc_dist", by.y="distance_adama_direction",all.x=T,all.y=F)
    accidents_all_sum$N_traffic <- accidents_all_sum$N / accidents_all_sum$traffic_sub_years * 1000
    
    accidents_km$popup_text <- paste0("Accidents: ", accidents_km$N.sum,
                                      "<br>Accidents per 1000 vehicles: ", round(accidents_km$N.sum_traffic,5),
                                      "<br>Km from Addis Ababa: ", accidents_km$km)
    accidents_all_sum$popup_text <- paste0("Accidents: ", accidents_all_sum$N,
                                           "<br>Accidents per 1000 vehicles: ", round(accidents_all_sum$N_traffic,5),
                                           "<br>Km from Addis Ababa: ", accidents_all_sum$accident_loc_dist/1000)

    
    if(input$accident_vs_accidenttraffic == "accident_traffic"){
    
      # Make Map
      accidents_km$N.sum_color <- accidents_km$N.sum - min(accidents_km$N.sum)
      accidents_km$N.sum_color <- accidents_km$N.sum_color/max(accidents_km$N.sum_color) * 100 + 1
      pal <- colorNumeric(c("blue", "orange", "red"), 1:max(accidents_km$N.sum_color))
      
      leaflet() %>% 
        # Base groups
        addTiles(group = "OSM (Default)") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
        # Layers
        addCircles(data=accidents_km, popup=~popup_text, fillOpacity = 1, stroke=T, fillColor = ~pal(N.sum_color),weight=1, color="black", radius=~N.sum_traffic*350000+10, group="Crashes Every 10km") %>%
        addCircles(data=accidents_all_sum, popup=~popup_text, fillOpacity = .75, stroke=F, color="black", radius=~N*15, group="Crash Locations") %>%
        # Layers Controls
        addLayersControl(
          baseGroups = c("OSM (Default)", "CartoDB"),
          overlayGroups = c("Crashes Every 10km", "Crash Locations"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Crash Locations")
    
    } else{
      
      # Make Map
      pal <- colorNumeric(c("blue", "orange", "red"), 1:max(accidents_km$N.sum))
      
      leaflet() %>% 
        # Base groups
        addTiles(group = "OSM (Default)") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
        # Layers
        addCircles(data=accidents_km, popup=~popup_text, fillOpacity = 1, stroke=T, fillColor = ~pal(N.sum),weight=1, color="black", radius=~N.sum*35+10, group="Crashes Every 10km") %>%
        addCircles(data=accidents_all_sum, popup=~popup_text, fillOpacity = .75, stroke=F, color="black", radius=~N*15, group="Crash Locations") %>%
        # Layers Controls
        addLayersControl(
          baseGroups = c("OSM (Default)", "CartoDB"),
          overlayGroups = c("Crashes Every 10km", "Crash Locations"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Crash Locations")
      
    }
    
    
  })
    
  # Crashes Along Space --------------------------------------------------------
  output$plot2 <- renderPlotly({
    accidents_subset <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause) & (accidents_all$accident_type_simple %in% input$accident_type) & (accidents_all$year %in% input$accident_year) & (accidents_all$vehicle_type_str %in% input$vehicle_type),]
    
    accidents_subset$N <- 1
    accidents_subset <- summaryBy(N ~ km + Direction, data=accidents_subset@data, keep.names=T, FUN=sum)
    
    expressway_df <- 0:78 %>% as.data.frame %>% dplyr::rename(km=".")
    
    expressway_addis_df <- merge(expressway_df, accidents_subset[accidents_subset$Direction == "Addis",], by="km",all.x=T) %>%
      mutate(Direction = "Addis")
    expressway_adama_df <- merge(expressway_df, accidents_subset[accidents_subset$Direction == "Adama",], by="km",all.x=T) %>%
      mutate(Direction = "Adama")
    
    expressway_df <- bind_rows(expressway_addis_df, expressway_adama_df)
    expressway_df$N[is.na(expressway_df$N)] <- 0
    
    #p <- plot_ly(expressway_df, x = ~km, y = ~N, type = 'scatter', mode = 'lines', color=~Direction)
    
    y_label <- "Number of Crashes"
    # Convert to Traffic/Accidents
    if(input$accident_vs_accidenttraffic == "accident_traffic"){
      
      years <- input$accident_year
      
      addis_adama_points_data_subset_round$traffic_to_adama_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_adama_",years))) %>% rowSums
      addis_adama_points_data_subset_round$traffic_to_addis_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_addis_",years))) %>% rowSums
      expressway_df <- merge(expressway_df, addis_adama_points_data_subset_round, all.x=T,all.y=F, by.x="km", by.y="km_round")
      expressway_df$N[expressway_df$Direction %in% "Adama"] <- expressway_df$N[expressway_df$Direction %in% "Adama"] / expressway_df$traffic_to_adama_sub_years[expressway_df$Direction %in% "Adama"] * 1000
      expressway_df$N[expressway_df$Direction %in% "Addis"] <- expressway_df$N[expressway_df$Direction %in% "Addis"] / expressway_df$traffic_to_adama_sub_years[expressway_df$Direction %in% "Addis"] * 1000
      
      y_label <- "Number of Crashes per 1000 Vehicles"
      
    }
    
    p <- ggplot(data=expressway_df, aes(x=km, y=N, color=Direction)) +
      geom_line() +
      geom_point() +
      labs(x="Kilometers from Addis Ababa", y=y_label,
           title=y_label) +
      theme_minimal() +
      scale_color_manual(values=c("darkorange1","blue2")) +
      theme(plot.title = element_text(hjust = 0.5, face="bold")) +
      ggplot2::ylim(0, max(expressway_df$N, na.rm=T))
    
    p <- ggplotly(p)
    p

  })
    
  # Table of Hotspots ----------------------------------------------------------
  output$plot_hotspot_Adama <- renderTable({
    accidents_subset <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause) & (accidents_all$accident_type_simple %in% input$accident_type) & (accidents_all$year %in% input$accident_year) & (accidents_all$vehicle_type_str %in% input$vehicle_type),]
    
    if(input$accident_vs_accidenttraffic == "accident_traffic"){
      
      years <- input$accident_year
      
      accidents_subset <- accidents_subset[accidents_subset$Direction == "Adama",]
      
      addis_adama_points_data_subset_round$traffic_to_adama_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_adama_",years))) %>% rowSums
      accidents_subset <- merge(accidents_subset, addis_adama_points_data_subset_round, all.x=T,all.y=F, by.x="km", by.y="km_round")

      accident_traffic_sum <- summaryBy(N+traffic_to_adama_sub_years~km,data=accidents_subset@data,FUN=c(mean,sum))
      
      accident_traffic_sum$`Crashes per 1000 Vehicles` <- accident_traffic_sum$N.sum / accident_traffic_sum$traffic_to_adama_sub_years.mean * 1000
      accident_traffic_sum <- subset(accident_traffic_sum, select=c(km, `Crashes per 1000 Vehicles`))
      
      df_out <- accident_traffic_sum %>% 
        arrange(desc(`Crashes per 1000 Vehicles`)) %>% 
        dplyr::rename(`Km from Addis Ababa`=km) %>%
        head(input$number_rows_show_hotspot)
      
      df_out$`Crashes per 1000 Vehicles` <- round(df_out$`Crashes per 1000 Vehicles`,5) %>% as.character()
      df_out$`Km from Addis Ababa` <- round(df_out$`Km from Addis Ababa`,0) %>% as.character()
      df_out
      
    } else{
      accidents_subset$km[accidents_subset$Direction == "Adama"] %>% 
        table %>% 
        as.data.frame %>% 
        dplyr::rename(`Km from Addis Ababa`=".") %>% 
        dplyr::rename(Crashes=Freq) %>% 
        arrange(desc(Crashes)) %>% 
        head(input$number_rows_show_hotspot)
    }

    
    

  })
    
  # Plot Hotspots in Direction of Addis
  output$plot_hotspot_Addis <- renderTable({
    accidents_subset <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause) & (accidents_all$accident_type_simple %in% input$accident_type) & (accidents_all$year %in% input$accident_year) & (accidents_all$vehicle_type_str %in% input$vehicle_type),]
    
    if(input$accident_vs_accidenttraffic == "accident_traffic"){
      
      years <- input$accident_year
      
      accidents_subset <- accidents_subset[accidents_subset$Direction == "Addis",]
      
      addis_adama_points_data_subset_round$traffic_to_addis_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_addis_",years))) %>% rowSums
      accidents_subset <- merge(accidents_subset, addis_adama_points_data_subset_round, all.x=T,all.y=F, by.x="km", by.y="km_round")
      
      accident_traffic_sum <- summaryBy(N+traffic_to_addis_sub_years~km,data=accidents_subset@data,FUN=c(mean,sum))
      
      accident_traffic_sum$`Crashes per 1000 Vehicles` <- accident_traffic_sum$N.sum / accident_traffic_sum$traffic_to_addis_sub_years.mean * 1000
      accident_traffic_sum <- subset(accident_traffic_sum, select=c(km, `Crashes per 1000 Vehicles`))
      
      df_out <- accident_traffic_sum %>% 
        arrange(desc(`Crashes per 1000 Vehicles`)) %>% 
        dplyr::rename(`Km from Addis Ababa`=km) %>%
        head(input$number_rows_show_hotspot)
      
      df_out$`Crashes per 1000 Vehicles` <- round(df_out$`Crashes per 1000 Vehicles`,5) %>% as.character()
      df_out$`Km from Addis Ababa` <- round(df_out$`Km from Addis Ababa`,0) %>% as.character()
      df_out
      
    } else{
    
      accidents_subset$km[accidents_subset$Direction == "Addis"] %>% 
        table %>% 
        as.data.frame %>% 
        dplyr::rename(`Km from Addis Ababa`=".") %>% 
        dplyr::rename(Crashes=Freq) %>% 
        arrange(desc(Crashes)) %>% 
        head(input$number_rows_show_hotspot)
      
    }
  })
    
  # Plot Hotspots: Total -------------------------------------------------------
  output$plot_hotspot_Total <- renderTable({
    accidents_subset <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause) & (accidents_all$accident_type_simple %in% input$accident_type) & (accidents_all$year %in% input$accident_year) & (accidents_all$vehicle_type_str %in% input$vehicle_type),]
    
    if(input$accident_vs_accidenttraffic == "accident_traffic"){
      
      years <- input$accident_year
      
      addis_adama_points_data_subset_round$traffic_to_addis_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_addis_",years))) %>% rowSums
      addis_adama_points_data_subset_round$traffic_to_adama_sub_years <- subset(addis_adama_points_data_subset_round, select=c(paste0("traffic_to_adama_",years))) %>% rowSums
      addis_adama_points_data_subset_round$traffic_sub_years <- addis_adama_points_data_subset_round$traffic_to_addis_sub_years + addis_adama_points_data_subset_round$traffic_to_adama_sub_years
      
      accidents_subset <- merge(accidents_subset, addis_adama_points_data_subset_round, all.x=T,all.y=F, by.x="km", by.y="km_round")
      
      accident_traffic_sum <- summaryBy(N+traffic_sub_years~km,data=accidents_subset@data,FUN=c(mean,sum))
      
      accident_traffic_sum$`Crashes per 1000 Vehicles` <- accident_traffic_sum$N.sum / accident_traffic_sum$traffic_sub_years.mean * 1000
      accident_traffic_sum <- subset(accident_traffic_sum, select=c(km, `Crashes per 1000 Vehicles`))
      
      df_out <- accident_traffic_sum %>% 
        arrange(desc(`Crashes per 1000 Vehicles`)) %>% 
        dplyr::rename(`Km from Addis Ababa`=km) %>%
        head(input$number_rows_show_hotspot)
      
      df_out$`Crashes per 1000 Vehicles` <- round(df_out$`Crashes per 1000 Vehicles`,5) %>% as.character()
      df_out$`Km from Addis Ababa` <- round(df_out$`Km from Addis Ababa`,0) %>% as.character()
      df_out
      
    } else {
    
      accidents_subset$km %>% 
        table %>% 
        as.data.frame %>% 
        dplyr::rename(`Km from Addis Ababa`=".") %>% 
        dplyr::rename(Crashes=Freq) %>% 
        arrange(desc(Crashes)) %>% 
        head(input$number_rows_show_hotspot)
    }
  })
    
  # Regression 1 ---------------------------------------------------------------
  output$plot3 <- renderText({
    
    accidents_subset <- accidents_all[!is.na(accidents_all$dayhour),]
    accidents_subset <- accidents_subset[(accidents_subset$accident_cause_simple %in% input$accident_cause2) & (accidents_subset$accident_type_simple %in% input$accident_type2) & (accidents_subset$year %in% input$accident_year2),]
    
    dataset_hours_long$accident_sub_yn <- dataset_hours_long$dayhour %in% unique(accidents_subset$dayhour)
    dataset_hours_long <- dataset_hours_long[dataset_hours_long$year %in% input$accident_year2,]
    
    dataset_hours_long$traffic <- dataset_hours_long$traffic / 1000
    dataset_hours_long$precipitation[is.na(dataset_hours_long$precipitation)] <- 0
    
    lm1 <- lm(accident_sub_yn ~ traffic + speed_perc_0.5 + direction + friday_saturday + holidays_1day_buff + precipitation + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])
    
    if(length(input$controlvars2) == 7){
      stargazer(lm1,
                column.labels=c(""),
                dep.var.labels.include=F,
                covariate.labels = c("Traffic (1000 Vehicles)",
                                     "Median Speed (km/hr)",
                                     "Driving Towards Addis",
                                     "Friday or Saturday",
                                     "Holiday (+/- 1 day)",
                                     "Precpitation (mm)",
                                     "Early Morning or Night"),
                omit.stat = c("f","ser"),
                dep.var.caption="Crash Occuring in an Hour",
                type = "html")
    } else{
      lm2 <- lm(as.formula(paste0("accident_sub_yn ~", paste(input$controlvars2, collapse=" + "))), data=dataset_hours_long[dataset_hours_long$direction != "Total",])
      
      stargazer(lm1,lm2,
                column.labels=c("",""),
                dep.var.labels.include=F,
                covariate.labels = c("Traffic (1000 Vehicles)",
                                     "Median Speed (km/hr)",
                                     "Driving Towards Addis",
                                     "Friday or Saturday",
                                     "Holiday (+/- 1 day)",
                                     "Precpitation (mm)",
                                     "Early Morning or Night"),
                omit.stat = c("f","ser"),
                dep.var.caption="Crash Occuring in an Hour",
                type = "html")
    }

  })
    
  # Regression 2 ---------------------------------------------------------------
  output$plot4 <- renderText({
    
    accidents_subset <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause3) & (accidents_all$accident_type_simple %in% input$accident_type3) & (accidents_all$year %in% input$accident_year3),]
    
    accidents_subset_Adama <- summaryBy(N~accident_loc_dist, data=accidents_subset@data[accidents_subset@data$Direction %in% "Adama",], keep.names=T, FUN=sum) %>% dplyr::rename(N_Adama=N)
    accidents_subset_Addis <- summaryBy(N~accident_loc_dist, data=accidents_subset@data[accidents_subset@data$Direction %in% "Addis",], keep.names=T, FUN=sum) %>% dplyr::rename(N_Addis=N)
    accidents_subset_Total <- summaryBy(N~accident_loc_dist, data=accidents_subset@data, keep.names=T, FUN=sum) %>% dplyr::rename(N_Total=N)
    
    addis_adama_points_data <- merge(addis_adama_points_data, accidents_subset_Adama, by.x="distance_adama_direction", by.y="accident_loc_dist", all.x=T, all.y=F)
    addis_adama_points_data <- merge(addis_adama_points_data, accidents_subset_Addis, by.x="distance_adama_direction", by.y="accident_loc_dist", all.x=T, all.y=F)
    addis_adama_points_data <- merge(addis_adama_points_data, accidents_subset_Total, by.x="distance_adama_direction", by.y="accident_loc_dist", all.x=T, all.y=F)
    
    addis_adama_points_data$N_Adama[is.na(addis_adama_points_data$N_Adama)] <- 0
    addis_adama_points_data$N_Addis[is.na(addis_adama_points_data$N_Addis)] <- 0
    addis_adama_points_data$N_Total[is.na(addis_adama_points_data$N_Total)] <- 0
    
    dep_var_name <- "Number of Crashes Within 1km"
    if(input$binary_dep3 %in% "Binary"){
      addis_adama_points_data$N_Adama[addis_adama_points_data$N_Adama > 0] <- 1
      addis_adama_points_data$N_Addis[addis_adama_points_data$N_Addis > 0] <- 1
      addis_adama_points_data$N_Total[addis_adama_points_data$N_Total > 0] <- 1
      dep_var_name <- "Accident Occured Within 1km"
    }
    
    lm_Adama <- lm(N_Adama ~ dist_plaza_nearest_km + turnangle_500m, data=addis_adama_points_data)
    lm_Addis <- lm(N_Addis ~ dist_plaza_nearest_km + turnangle_500m, data=addis_adama_points_data)
    lm_Total <- lm(N_Total ~ dist_plaza_nearest_km + turnangle_500m, data=addis_adama_points_data)
    
    if(length(input$controlvars3) == 2){
    stargazer(lm_Total,lm_Adama,lm_Addis,
              column.labels=c("Total","To Adama","To Addis"),
              dep.var.labels.include=F,
              covariate.labels = c("Distance Nearest Toll Plaze (km)",
                                   "Average Turn Angle Within 1km"),
              omit.stat = c("f","ser"),
              dep.var.caption=dep_var_name,
              type = "html")
    } else{
      
      lm_Adama_sub <- lm(as.formula(paste0("N_Adama ~", paste(input$controlvars3, collapse=" + "))), data=addis_adama_points_data)
      lm_Addis_sub <- lm(as.formula(paste0("N_Addis ~", paste(input$controlvars3, collapse=" + "))), data=addis_adama_points_data)
      lm_Total_sub <- lm(as.formula(paste0("N_Total ~", paste(input$controlvars3, collapse=" + "))), data=addis_adama_points_data)
      
      stargazer(lm_Total,lm_Total_sub,
                lm_Adama,lm_Adama_sub,
                lm_Addis,lm_Addis_sub,
                column.labels=c("Total","Total", "To Adama", "To Adama", "To Addis", "To Addis"),
                dep.var.labels.include=F,
                covariate.labels = c("Distance Nearest Toll Plaze (km)",
                                     "Average Turn Angle Within 1km"),
                omit.stat = c("f","ser"),
                dep.var.caption=dep_var_name,
                type = "html")
    }

  })
   
  # Alluvial ------------------------------------------------------------------- 
  output$plot5 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause4) & (accidents_all$accident_type_simple %in% input$accident_type4) & (accidents_all$year %in% input$accident_year4),]
    accidents_all_causetypesum <- summaryBy(N ~ accident_type_simple+accident_cause_simple,data=accidents_all@data,keep.names=T,FUN=sum)

    #alluvial(subset(accidents_all_causetypesum, select=c(accident_cause_simple, accident_type_simple)), 
    #         freq=accidents_all_causetypesum$N,
    #         alpha=accidents_all_causetypesum$N/max(accidents_all_causetypesum$N),
    #         #col = "Spectral",
    #         axis_labels = c("Cause", "Type"),
    #         cex = 0.7)
    
    ggplot(data = accidents_all_causetypesum,
           aes(axis1 = accident_cause_simple, axis2 = accident_type_simple,
               y = N)) +
      scale_x_discrete(limits = c("Cause", "Type"), expand = c(.1, .05)) +
      geom_alluvium(aes(fill = log(N, base=1.1), alpha=log(N))) +
      scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
      geom_stratum() + 
      geom_text(stat = "stratum", label.strata = TRUE) +
      theme_void() +
      theme(legend.position="none") 
      
    
  })

  #
  output$plot6 <- renderPlotly({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause5) & (accidents_all$accident_type_simple %in% input$accident_type5),]
    
    accidents_per_day <- accidents_all$`Accident Date` %>% table %>% as.data.frame %>% rename(day = ".") %>% rename(N_accidents = Freq)
    accidents_per_day <- accidents_per_day[!grepl("1900", accidents_per_day$day),]
    
    days_df <- seq(as.Date("2015-01-01"), as.Date("2017-12-31"), by="days") %>% as.data.frame %>% dplyr::rename(day = ".")
    days_df$day <- days_df$day %>% as.character
    days_df <- merge(days_df, accidents_per_day, by="day",all.x=T)
    days_df$N_accidents[is.na(days_df$N_accidents)] <- 0
    days_df$year <- days_df$day %>% substring(1,4)
    
    days_df$num_days <- 1
    days_accidents_df <- summaryBy(num_days ~ N_accidents+year, data=days_df, FUN=sum, keep.names=T)
    
    days_df$day <- days_df$day %>% as.POSIXct()
    
    #days_df$week <- week(days_df$day)  
    #days_df$week[days_df$week <= 9] <-  paste0("0",days_df$week[days_df$week <= 9])
    #days_df$week_id <- paste0(year(days_df$day), days_df$week) %>% as.factor %>% as.numeric
    
    #p <- ggplot(days_df) +
    #  geom_col(aes(x=day,y=N_accidents),fill="dodgerblue4") +
    #  labs(x="", y="Number of Crashes") +
    #  theme_minimal()
    
    p <- plot_ly(
      x = days_df$day,
      y = days_df$N_accidents,
      name = "Daily Crashes",
      type = "bar") %>%
      layout(title="Number of Crashes (Daily)")
  
    p
  
  })
  
  output$plot7 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause5) & (accidents_all$accident_type_simple %in% input$accident_type5),]
    
    accidents_per_day <- accidents_all$`Accident Date` %>% table %>% as.data.frame %>% rename(day = ".") %>% rename(N_accidents = Freq)
    accidents_per_day <- accidents_per_day[!grepl("1900", accidents_per_day$day),]
    
    days_df <- seq(as.Date("2015-01-01"), as.Date("2017-12-31"), by="days") %>% as.data.frame %>% dplyr::rename(day = ".")
    days_df$day <- days_df$day %>% as.character
    days_df <- merge(days_df, accidents_per_day, by="day",all.x=T)
    days_df$N_accidents[is.na(days_df$N_accidents)] <- 0
    days_df$year <- days_df$day %>% substring(1,4)
    
    days_df$num_days <- 1
    days_accidents_df <- summaryBy(num_days ~ N_accidents+year, data=days_df, FUN=sum, keep.names=T)
    
    days_df$day <- days_df$day %>% as.POSIXct()
    
    ggplot(data=days_accidents_df, aes(x=N_accidents, y=num_days, fill=year)) +
      geom_bar(stat="identity", position="dodge") + 
      labs(x="Number of Crashes That Occur in a Day", y="Days", fill="",
           title="Number of Crashes That Occur in a Day") +
      scale_x_continuous(breaks=0:8) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face="bold")) +
      scale_fill_manual(values=c("darkgreen","firebrick2","dodgerblue4"))
    
  })
  
  output$plot8 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause6) & (accidents_all$accident_type_simple %in% input$accident_type6) & (accidents_all$year %in% input$accident_year6),]

    accidents_by_hour <- table(accidents_all$hour) %>% as.data.frame
    names(accidents_by_hour) <- c("hour","number_accidents")
    
    ggplot() +
      geom_col(data=accidents_by_hour, 
               aes(x=hour, y=number_accidents), fill="orange3") + 
      theme_minimal() + 
      labs(x="Hour") + 
      labs(y="Number of Crashes")
  })
  
  output$plot9 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause6) & (accidents_all$accident_type_simple %in% input$accident_type6) & (accidents_all$year %in% input$accident_year6),]
    
    accidents_by_dayofweek <- table(accidents_all$Day) %>% as.data.frame
    names(accidents_by_dayofweek) <- c("Day of Week","number_accidents")
    
    ggplot() +
      geom_col(data=accidents_by_dayofweek, 
               aes(x=`Day of Week`, y=number_accidents), fill="orange3") + 
      theme_minimal() + 
      labs(x="Day of Week") + 
      labs(y="Number of Crashes")
  })
  
  
  output$plot10 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause6) & (accidents_all$accident_type_simple %in% input$accident_type6) & (accidents_all$year %in% input$accident_year6),]

    accidents_by_weather <- table(accidents_all$Weather) %>% as.data.frame
    names(accidents_by_weather) <- c("Weather","number_accidents")
    
    ggplot() +
      geom_col(data=accidents_by_weather, 
               aes(x=Weather, y=number_accidents), fill="orange3") + 
      theme_minimal() + 
      labs(x="Weather") + 
      labs(y="Number of Crashes")
  })    
  
  output$plot10 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause6) & (accidents_all$accident_type_simple %in% input$accident_type6) & (accidents_all$year %in% input$accident_year6),]
    
    accidents_by_weather <- table(accidents_all$Weather) %>% as.data.frame
    names(accidents_by_weather) <- c("Weather","number_accidents")
    
    ggplot() +
      geom_col(data=accidents_by_weather, 
               aes(x=Weather, y=number_accidents), fill="orange3") + 
      theme_minimal() + 
      labs(x="Weather") + 
      labs(y="Number of Crashes")
  })    
  
  output$plot11 <- renderPlot({
    
    accidents_all <- accidents_all[(accidents_all$accident_cause_simple %in% input$accident_cause6) & (accidents_all$accident_type_simple %in% input$accident_type6) & (accidents_all$year %in% input$accident_year6),]
    
    accidents_by_vehicletype <- table(accidents_all$vehicle_type_str) %>% as.data.frame
    names(accidents_by_vehicletype) <- c("vehicle_type_str","number_accidents")
    
    ggplot() +
      geom_col(data=accidents_by_vehicletype, 
               aes(x=vehicle_type_str, y=number_accidents), fill="orange3") + 
      theme_minimal() + 
      labs(x="Vehicle Type") + 
      labs(y="Number of Crashes")
  }) 
    
    
    
    

    observe({
      updateCheckboxGroupInput(
        session, "accident_cause", choices = accident_cause_simple_list,
        selected = if(input$accident_cause_allnone) accident_cause_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_cause2", choices = accident_cause_simple_list,
        selected = if(input$accident_cause_allnone2) accident_cause_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_cause3", choices = accident_cause_simple_list,
        selected = if(input$accident_cause_allnone3) accident_cause_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_cause4", choices = accident_cause_simple_list,
        selected = if(input$accident_cause_allnone4) accident_cause_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_cause5", choices = accident_cause_simple_list,
        selected = if(input$accident_cause_allnone5) accident_cause_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_cause6", choices = accident_cause_simple_list,
        selected = if(input$accident_cause_allnone6) accident_cause_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_type", choices = accident_type_simple_list,
        selected = if(input$accident_type_allnone) accident_type_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_type2", choices = accident_type_simple_list,
        selected = if(input$accident_type_allnone2) accident_type_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_type3", choices = accident_type_simple_list,
        selected = if(input$accident_type_allnone3) accident_type_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_type4", choices = accident_type_simple_list,
        selected = if(input$accident_type_allnone4) accident_type_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_type5", choices = accident_type_simple_list,
        selected = if(input$accident_type_allnone5) accident_type_simple_list
      )
    })
    
    observe({
      updateCheckboxGroupInput(
        session, "accident_type6", choices = accident_type_simple_list,
        selected = if(input$accident_type_allnone6) accident_type_simple_list
      )
    })
    
  
}

# Run the app ==================================================================
shinyApp(ui, server)


