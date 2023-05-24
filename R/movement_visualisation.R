## movement_visualisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Visualise seasonal movement of tracked individuals during breeding season in an interactive shiny app
#'
#' \code{movement_visualisation} visualises tracking data of individuals on an interactive map
#' to help diagnose whether a breeding attempt has been made and whether it was successful or not.
#' The decisions whether a breeding attempt has taken place and whether it was successful are
#' storedin an external data frame and visualised as a table in a panel of the shiny app. To
#' support the decision, in a separate panel \code{plot_move_metrics} is integrated which produces
#' a series of plots of movement metrics summarised over a moving time window for the analysed individual.
#' 
#' Input data for this function must be derived from \code{\link{data_prep}} (\code{nest_data_input$movementtrack} and \code{nest_data_input$pot_nests}),
#' \code{\link{predict_success}} (\code{pred_succ}) and \code{\link{move_metric_extraction}} (\code{milvus_5d_move_metrics})
#'
#' @param trackingdata filename for data.frame with tracking data derived from \code{\link{data_prep}} as \code{nest_data_input$movementtrack}. This data.frame is used to visualise the GPS locations and the movement trajectory on the map.
#' @param nest_locs filename for data.frame with coordinates of potential nest locations for every individual derived from \code{\link{data_prep} as \code{nest_data_input$pot_nests}. This data.frame is used to visualise the potential nest location on the map.
#' @param inddata filename for data.frame with predictions for breeding success derived from \code{\link{predict_success}} as \code{pred_succ}. This data.frame is used to visualise brood relevant metrics as table.
#' @param move_metrics filename for data.frame with seasonal movement metrics during breeding season derived from \code{\link{move_metric_extraction}} as \code{milvus_5d_move_metrics}. This data.frame is used to plot the move metrics.
#' @param uncertainty numeric value between 0 and 0.5. Individuals for which the nest success classification resulted in a probability <\code{uncertainty}. This value should match the value of \code{uncertainty} used in \code{\link{move_metric_extraction}} to ensure that data is available for all visualised individuals.
#' @param output_path path for the data.frame with the stored decisions about the examined individuals, whether they have had a nest and a successful brood.
#'
#' @return Returns an interactive shiny app that shows a map, a table and a plot for every analysed individual.
#'
#' @export
#' @importFrom here here
#' @importFrom dplyr select filter mutate arrange case_when
#' @importFrom data.table fread fwrite
#' @importFrom lubridate yday
#' @importFrom shiny actionButton column fluidPage fluidRow htmlOutput mainPanel observe observeEvent reactive reactiveValues renderText selectInput shinyApp sidebarLayout sidebarPanel sliderInput titlePanel updateSelectInput
#' @importFrom shinyWidgets chooseSliderSkin materialSwitch prettyRadioButtons
#' @importFrom shinythemes shinytheme
#' @importFrom leaflet addCircleMarkers addLayersControl addLegend addMeasure addPolylines addProviderTiles addScaleBar clearMarkers clearShapes clearTiles colorFactor colorNumeric fitBounds hideGroup labelFormat layersControlOptions leaflet leafletOutput leafletProxy providerTileOptions removeControl renderLeaflet scaleBarOptions
#' @importFrom sf st_as_sf st_coordinates st_transform
#' @importFrom htmltools HTML div h3
#' @importFrom DT dataTableOutput renderDataTable datatable dataTableProxy formatStyle selectPage styleEqual
#' @importFrom plotly plotlyOutput renderPlotly style

movement_visualisation <- function(trackingdata,
                                   nest_locs,
                                   inddata,
                                   move_metrics,
                                   uncertainty_success = 0.25,
                                   uncertainty_nest = 0.25,
                                   output_path = "output/05_full_run_CH/nest_success_output.csv"
) {
  
  
  
  # DATA PREPARATION -----------------------------------------------------------
  # Define a path where to save user decisions on nests
  output_path <- "output/05_full_run_CH/nest_success_output.csv"
  
  # Brood metrics for success prediction
  milvus_metrics <- inddata %>%
    dplyr::filter(succ_prob > uncertainty_success & succ_prob < (1-uncertainty_success) &
                    nest_prob > uncertainty_nest) %>%
    mutate(ID = year_id,
           "LDay" = lastvisitDay,
           "TCh2" = round(timeChick2),
           "VCh2" = revisitsChick2,
           "TDay" = round(residence_time_day),
           "TotT" = round(tottime100m),
           "VDay" = revisits_day,
           "PNest" = paste(round(nest_prob*100), "%"),
           Nest = NA, Success = NA) %>%
    dplyr::select(ID, LDay, TCh2, VCh2, TDay, TotT, VDay, PNest,
                  Nest, Success) %>%
    arrange(ID)
  # saves table as template to fill in data on nests based on user input
  fwrite(milvus_metrics, here(output_path), row.names = F)
  
  # Movement data
  milvus_track <- trackingdata %>%
    dplyr::filter(id %in% milvus_metrics$ID) %>%
    mutate(t_ = as.POSIXct(t_, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
           t_ = format(t_, format = "%Y-%m-%d %H:%M", tz = "UTC"),
           date = as.Date(t_, tz = "UTC"),
           month = as.integer(format(date, format = "%m")),
           year_day = yday(t_), # allows to use a numeric color palette
           tod_ = case_when(tod_ == "day" ~ "day",
                            tod_ == "dusk" ~ "twilight",
                            tod_ == "night" ~ "night",
                            tod_ == "dawn" ~ "twilight")) %>% # changes dusk and dawn to night
    st_as_sf(coords = c("x_", "y_"), crs = 3035) %>%
    st_transform(crs = 4326) %>%
    mutate(long = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  # Predicted nest locations
  milvus_nest <- nest_locs %>%
    dplyr::filter(id %in% milvus_metrics$ID) %>%
    st_as_sf(coords = c("x", "y"), crs = 3035) %>%
    st_transform(crs = 4326) %>%
    mutate(long = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  # Function for move metrics plot
  source(here("R//plot_move_metrics.r"))
  
  
  
  # SHINY APP ------------------------------------------------------------------
  
  
  
  # USER INTERFACE -------------------------------------------------------------
  ui <- fluidPage(theme = shinytheme("flatly"),
                  # layout of the action buttons
                  tags$style(
                    HTML('#save_decision, #zoom{
                       height:65px; width:110px; margin-top:50px;}')
                  ),
                  # layout of the shiny app
                  titlePanel(title = "Red Kite Movement Trajectory"),
                  sidebarLayout(
                    sidebarPanel(width = 3, style = "height: 675px; position:relative;",
                                 h3("Data selection"),
                                 fluidRow(
                                   # Build selection tool for changing the individual to be displayed
                                   column(width = 12,
                                          selectInput(inputId = "ID", label = "Red Kite",
                                                      choices = sort(as.character(unique(milvus_metrics$ID))), multiple = F)
                                   )
                                 ),
                                 # Change amount of data displayed
                                 fluidRow(
                                   column(width = 6,
                                          prettyRadioButtons(inputId = "data_period",
                                                             label = "Data Period",
                                                             choices = list("Whole Period" = 1,
                                                                            "March" = 3,
                                                                            "April" = 4,
                                                                            "May" = 5,
                                                                            "June" = 6),
                                                             selected = 1,
                                                             shape = "curve",
                                                             outline = F,
                                                             fill = T,
                                                             plain = T
                                          )
                                   ),
                                   # Action button to save the nest decision input
                                   column(width = 6, align = "center",
                                          actionButton(inputId = "zoom",
                                                       label = HTML("Zoom to<br>data extent"))
                                   )
                                 ),
                                 # Show warning if no data is available in the selected period
                                 fluidRow(
                                   column(width = 12,
                                          htmlOutput("warning")
                                   )
                                 ),
                                 # Switch to allow visualisation of day and night points
                                 fluidRow(
                                   column(width = 12,
                                          materialSwitch(
                                            inputId = "day_night",
                                            label = "Day & Night Locations", 
                                            value = F)
                                   )
                                 ),
                                 # Slider to select opacity of background map
                                 fluidRow(
                                   column(width = 12,
                                          chooseSliderSkin("Flat", "#5489C5"),
                                          sliderInput(inputId = "map_slider",
                                                      label = "Background Map Transparency",
                                                      min = 0, max = 1, value = 0.7, step = 0.1)
                                   )
                                 ),
                                 
                                 # Radio button to decide whether bird has a nest or not and if brood was successful or not
                                 fluidRow(
                                   column(width = 6,
                                          prettyRadioButtons(inputId = "nest_decision",
                                                             label = "Did this bird nest?",
                                                             choices = list("Yes" = 1,
                                                                            "No" = 0),
                                                             selected = 1,
                                                             status = "danger",
                                                             shape = "curve",
                                                             outline = F,
                                                             fill = T,
                                                             plain = T)
                                   ),
                                   # Action button to save the nest decision input
                                   column(width = 6, align = "center",
                                          actionButton(inputId = "save_decision",
                                                       label = HTML("Save<br>decisions"), class = "btn-danger")
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 6, style = "margin-top:-20px",
                                          prettyRadioButtons(inputId = "brood_decision",
                                                             label = HTML("Was the brood<br>successful?"),
                                                             choices = list("Yes" = 1,
                                                                            "No" = 0),
                                                             selected = 1,
                                                             status = "danger",
                                                             shape = "curve",
                                                             outline = F,
                                                             fill = T,
                                                             plain = T)
                                   )
                                 ),
                    ),
                    mainPanel(width = 9,
                              # For selected individual: Plotting movement track on a map
                              fluidRow(style = "padding-right:15px;",
                                       column(width = 12, class = "well", style = "height: 675px; position:relative;",
                                              h3("Map of selected movement trajectory"),
                                              leafletOutput("map", height = "575px")
                                       )
                              )
                    )
                  ),
                  sidebarLayout(
                    mainPanel(width = 7, 
                              # Table with brood specific metrics of the inspected bird
                              fluidRow(style = "padding-left:15px;",
                                       column(width = 12, class = "well", style = "height: 750px;",
                                              h3("Movement metrics & nest decisions"),
                                              div(DT::dataTableOutput("table"),
                                                  # style = "font-size:clamp(10px, 0.6vw, 0.7vw);") # this would allow dynamic adaptation of font size based on window size
                                                  style = "font-size:12px;")
                                       )
                              )
                    ),
                    sidebarPanel(width = 5,
                                 # Interactive plot with brood relevant metrics
                                 fluidRow(h3("Visualisation of seasonal movement metrics", style = "padding-left:15px;"),
                                          column(width = 12, class = "well", style = "height: 630px;",
                                                 plotlyOutput(outputId = "metrics_plot",
                                                              height = "600px")
                                          )
                                 )
                    )
                  )
  )
  
  
  
  # SERVER ---------------------------------------------------------------------
  server <- function(input, output, session){
    # Number of the input for the period to inspect
    input_period <- reactive({input$data_period})
    # ID of the bird/year
    input_id <- reactive({input$ID})
    # Should data be displayed in day-night mode
    input_day_night <- reactive({if(input$day_night == T){1} else{0}})
    # Defines map opacity
    input_map_opacity <- reactive({input$map_slider})
    # Defines data size
    input_size <- reactive({input$size_slider})
    
    # Creates reactiveValues to store the metrics data frame (empty)
    values <- reactiveValues(milvus_metrics = NULL)
    # loads the latest metrics data frame
    values$milvus_metrics <- fread(here(output_path))
    # Observes the action button (nest & brood decisions)
    observeEvent(input$save_decision, {
      # loads the latest metrics data frame
      values$milvus_metrics <- fread(here(output_path))
      # if "yes" is chosen, "yes" is assigned to the selected bird for Nest
      if (input$nest_decision == 1) {
        values$milvus_metrics <- values$milvus_metrics %>%
          mutate(Nest = case_when(ID == input_id() ~ "Yes",
                                  Nest == "Yes" ~ "Yes",
                                  Nest == "No" ~ "No"))
        # if "no" is chosen, "no" is assigned to the selected bird for Nest
      } else if (input$nest_decision == 0) {
        values$milvus_metrics <- values$milvus_metrics %>%
          mutate(Nest = case_when(ID == input_id() ~ "No",
                                  Nest == "Yes" ~ "Yes",
                                  Nest == "No" ~ "No"))
      }
      # if "yes" is chosen, "yes" is assigned to the selected bird for Success
      if (input$brood_decision == 1) {
        values$milvus_metrics <- values$milvus_metrics %>%
          mutate(Success = case_when(ID == input_id() ~ "Yes",
                                     Success == "Yes" ~ "Yes",
                                     Success == "No" ~ "No"))
        # if "no" is chosen, "no" is assigned to the selected bird for Success
      } else if (input$brood_decision == 0) {
        values$milvus_metrics <- values$milvus_metrics %>%
          mutate(Success = case_when(ID == input_id() ~ "No",
                                     Success == "Yes" ~ "Yes",
                                     Success == "No" ~ "No"))
      }
      # saves the data frame with the information if bird has a nest
      fwrite(values$milvus_metrics, here(output_path), row.names = F)
    })
    
    # Updates input selection (ID): Completed birds are grouped at the bottom
    observe({
      # The if else is only necessary to avoid the following case:
      # Two groups are shown in the selection. When a group consists of only one
      # item, the group name is shown instead of the item.
      if (length(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                              !is.na(values$milvus_metrics$Success),]$ID)) == 1) {
        # covers the case when there is only one item in the "Complete" group
        updateSelectInput(inputId = "ID", label = "Red Kite",
                          choices = list(
                            Incomplete = sort(as.character(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                                                          is.na(values$milvus_metrics$Success),]$ID))),
                            Complete = list(sort(as.character(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                                                                             !is.na(values$milvus_metrics$Success),]$ID))))
                          )
        )
      } else if (length(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                     is.na(values$milvus_metrics$Success),]$ID)) == 1) {
        # covers the case when there is only one item in the "Incomplete" group
        updateSelectInput(inputId = "ID", label = "Red Kite",
                          choices = list(
                            Incomplete = list(sort(as.character(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                                                               is.na(values$milvus_metrics$Success),]$ID)))),
                            Complete = sort(as.character(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                                                                        !is.na(values$milvus_metrics$Success),]$ID)))
                          )
        )
      } else {
        updateSelectInput(inputId = "ID", label = "Red Kite",
                          choices = list(
                            Incomplete = sort(as.character(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                                                          is.na(values$milvus_metrics$Success),]$ID))),
                            Complete = sort(as.character(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                                                                        !is.na(values$milvus_metrics$Success),]$ID)))
                          )
        )
      }
    })
    
    # Creates subset of locations by chosen ID and period
    milvus_track_subset <- reactive({
      if(input_period() == 1){ # whole period
        milvus_track %>%
          dplyr::filter(id == input_id())}
      else { # number of month
        milvus_track %>%
          dplyr::filter(id == input_id() & month == input_period())}
    })
    
    # Creates subset of nest by chosen ID and period
    milvus_nest_subset <- reactive({
      milvus_nest %>%
        dplyr::filter(id == input_id())})
    
    # Creates map of locations and trajectories
    output$map <- renderLeaflet({
      leaflet() %>%
        addLayersControl(
          baseGroups = c("Topo", "Satellite"),
          overlayGroups = c("Locations", "Nest", "Trajectory"),
          options = layersControlOptions(collapsed = T)) %>%
        hideGroup("Trajectory") %>%
        # Adds a scale bar
        addScaleBar(position = "bottomright",
                    options = scaleBarOptions(imperial = F)) %>%
        # Adds a tool to measure distances and areas
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "kilometers",
          secondaryLengthUnit = F,
          primaryAreaUnit = "hectares",
          activeColor = "#00CCCC",
          completedColor = "#006666"
        )
    })
    
    # Changes zoom extent to chosen bird
    # (only when new bird is chosen, but not when period selection etc. changes)
    observe({
      leafletProxy("map") %>%
        fitBounds(min(milvus_track[milvus_track$id == input_id(),]$long), min(milvus_track[milvus_track$id == input_id(),]$lat),
                  max(milvus_track[milvus_track$id == input_id(),]$long), max(milvus_track[milvus_track$id == input_id(),]$lat))
    })
    
    # Changes zoom extent when zoom button is clicked
    observeEvent(input$zoom, {
      leafletProxy("map") %>%
        fitBounds(min(milvus_track_subset()$long), min(milvus_track_subset()$lat),
                  max(milvus_track_subset()$long), max(milvus_track_subset()$lat))
    })
    
    # Changes base map according to selection
    observe({
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$OpenTopoMap, group = "Topo",
                         options = providerTileOptions(opacity = input_map_opacity())
        ) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite",
                         options = providerTileOptions(opacity = input_map_opacity())
        )
    })
    
    # Changes Palette, Labels, Points, Lines & Legend according to input
    observe({
      # Creates a palette that adapts to the range of dates
      pal <- colorNumeric(palette = "BrBG", domain = milvus_track_subset()$year_day)
      # Creates a palette for time of day (tod)
      pal_tod <- colorFactor(palette = c("#FFFFFF", "#000000", "#777777"), levels = c("day", "night", "twilight"))
      
      # Costum legend (Function)
      # Inputs for costum legend function
      if (input$day_night == T) {
        colors <- c("#740000", "#FFFFFF","#000000", "#777777") # fill colour inside borders of circles
        labels <- c("Nest", "Day", "Night", "Twilight")
        sizes <- c(rep(15, 3))
        shapes <- "circle"
        borders <- c("#740000", "#444444","#777777", "#444444") # border colour of circles
        margin_right <- -2.25 # 7.5 without twilight
        margin_left <- -2.25 # 0 without twilight
      }
      else {
        colors <- "#740000" # fill colour inside borders of circles
        labels <- "Nest"
        sizes <- 15
        shapes <- "circle"
        borders <- "#740000" # border colour of circles
        margin_right <- 11.5
        margin_left <- 0
      }
      # Function that creates a custom legend
      addLegendCustom <- function(map, layerId = NULL, group = NULL, labels, sizes, shapes, borders, opacity = 1, 
                                  position = c("topright", "bottomright", "bottomleft", "topleft")){
        position <- match.arg(position)
        make_shapes <- function(sizes, borders, shapes) {
          shapes <- gsub("circle", "100%", shapes)
          paste0(colors, "; width:", sizes, "px; margin-top:5px; margin-left:",
                 margin_left, "px; height:", sizes, "px; border:1.5px solid ",
                 borders, "; border-radius:", shapes)
        }
        make_labels <- function(sizes, labels) {
          paste0("<div style='display: inline-block; font-size:14px ;height: ", 
                 sizes, "px; margin-top:5px; margin-right:", margin_right,
                 "px; margin-left:", margin_left, "px; line-height: ",
                 sizes, "px;'>", labels, "</div>")
        }
        legend_colors <- make_shapes(sizes, borders, shapes)
        legend_labels <- make_labels(sizes, labels)
        
        return(addLegend(map, layerId = layerId, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
      }
      
      # Function that creates labels for date legend (from year_day back to desired format)
      myLabelFormat = function(...,date=FALSE){ 
        if(date){ 
          function(type = "numeric", cuts){
            as <- as.Date(cuts, origin="2020-01-01")
            format(as,"%d.%m.")
          } 
        }else{
          labelFormat(...)
        }
      }
      # Adds Data
      leafletProxy("map") %>%
        # Trajectories
        clearShapes() %>%
        addPolylines(
          data = milvus_track_subset(),
          lat = milvus_track_subset()$lat,
          lng = milvus_track_subset()$long,
          weight = 1,
          color = "#444444",
          opacity = 0.7,
          group = "Trajectory"
        ) %>%
        # Locations
        clearMarkers() %>%
        addCircleMarkers(
          data = milvus_track_subset(),
          radius = 4,
          color = ~pal(year_day),
          stroke = input$day_night,
          weight = 0.5,
          opacity = 0.7,
          fillColor = if(input$day_night == T){~pal_tod(tod_)}else{~pal(year_day)},
          fillOpacity = 0.7,
          group = "Locations",
          popup = ~paste0(t_)
        ) %>%
        # Adds legend
        removeControl(layerId = 1) %>% # removes legend
        removeControl(layerId = 2) # removes legend
      # Adds nest location and legends if data is available
      if (nrow(milvus_track_subset()) != 0) {
        leafletProxy("map", data = milvus_track_subset()) %>%
          # Nest location
          addCircleMarkers(
            data = milvus_nest_subset(),
            radius = 7,
            stroke = F,
            opacity = 0.7,
            fillColor = "#740000",
            fillOpacity = 0.7,
            group = "Nest",
            popup = ~paste0("Predicted nest location:<br>", round(lat, 2), "°N", round(long, 2), "°E ")
          ) %>%
          # Legend with date coloring
          addLegend(layerId = 1,
                    position = "bottomright",
                    pal = pal,
                    values = ~year_day,
                    opacity = 1,
                    #bins = 5,
                    labFormat = myLabelFormat(date=T),
                    title = NULL
          ) %>%
          # Legend with Nest, Day & Night
          addLegendCustom(layerId = 2,
                          labels,
                          sizes,
                          shapes,
                          borders,
                          position = "bottomright",
                          group = "Nest")
      }
    })
    
    # Creates a warning when no data is available in the selected period
    output$warning <- renderText({
      if (nrow(milvus_track_subset()) == 0) {
        return(paste("<span style=\"color:red\">No data in this period &#128559<br>&nbsp</span>"))
      }})
    
    # Creates a table with brood specific metrics of the inspected bird
    observe({
      # Shows the data frame with input data
      output$table <- DT::renderDataTable(
        datatable(values$milvus_metrics, options = list(dom = "ftip", pageLength = 20), rownames = F,
                  selection = "none", class = "compact hover row-border") %>%
          # Color codes the decisions made on nest and brood
          formatStyle(c("Nest", "Success"), target = "cell",
                      backgroundColor = styleEqual(c("No", "Yes", NA), c("#996622", "#66AAAA", "transparent"))) %>%
          # Highlights the row of the selected bird
          formatStyle("ID", target = "row",
                      fontWeight = styleEqual(c(input_id()), c("bold")),
                      backgroundColor = styleEqual(c(input_id()), c("#999999")))
      )
      # Ensures that the page of the table is shown that contains the observed bird
      proxy <- DT::dataTableProxy("table")
      if (input_id() %in% sort(unique(milvus_metrics$ID)[1:20])) {
        table_page <- 1
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[21:40])) {
        table_page <- 2
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[41:60])) {
        table_page <- 3
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[61:80])) {
        table_page <- 4
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[81:100])) {
        table_page <- 5
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[101:120])) {
        table_page <- 6
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[121:140])) {
        table_page <- 7
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[141:160])) {
        table_page <- 8
      } else if (input_id() %in% sort(unique(milvus_metrics$ID)[161:180])) {
        table_page <- 9
      } else {table_page <- 10}
      DT::selectPage(proxy = proxy, page = table_page)
    })
    
    # Creates an interactive plot with all brood relevant metrics
    output$metrics_plot <-renderPlotly({
      suppressWarnings({
        plot_move_metrics(movemetrics = move_metrics, individual = input_id())
      })
    })
  }
  
  
  
  # START SHINY APP ------------------------------------------------------------
  shinyApp(ui = ui, server = server)
}