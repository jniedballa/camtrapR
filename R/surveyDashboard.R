#' Shiny dashboard summarizing and analyzing camera trap survey data
#'
#' @description 
#' Open a Shiny dashboard to summarize a camera trapping survey. It provides an 
#' overview of the data, maps, can create and run occupancy models in unmarked, 
#' and plot species activity. 
#' 
#' Maps are interactive and can be panned, zoomed and queried. Users can choose 
#' a suitable basemap in the layers button.
#' 
#' For occupancy models, users can flexibly customize detection histories. Then 
#' the user can specify the model structure (by selecting site covariates on 
#' detection and occupancy probability from the camera trap table) and 
#' optionally including effort. Models are fitted automatically with unmarked. 
#' Response curves (marginal effect plots), model and parameter summaries are 
#' computed and updated automatically with every change to the model. 
#' Multiple models can be compared using model selection. 
#'
#' @param CTtable A data.frame containing the camera trap deployment information.
#' @param recordTable A data.frame containing the camera trap records.
#' @param stationCol The column name containing the camera trap station ID
#' @param cameraCol The column name containing the camera trap IDs (optional, only if 2 or more cameras per station)
#' @param xcol The column name containing the X coordinate of the camera trap station.
#' @param ycol The column name containing the Y coordinate of the camera trap station.
#' @param crs The coordinate reference system (CRS) of the camera trap data. Must be a valid argument to \code{\link[sf]{st_crs}}
#' @param setupCol The column name containing the camera trap deployment date (and time).
#' @param retrievalCol The column name containing the camera trap retrieval date (optionally date-time).
#' @param hasProblems A logical indicating whether there are periods of cameras malfunctioning
#' @param CTdateFormat The date format of the camera trap deployment and retrieval date and time (default: "ymd").
#' @param camerasIndependent logical. If multiple camera per station, are they independent?
#' @param speciesCol The column name containing the species names
#' @param recordDateTimeCol The column name containing the record date and time
#' @param recordDateTimeFormat The date/time format of \code{recordDateTimeCol}
#' @param timeZone Time zone of records in recordTable
#' @param exclude Species to be excluded from the data set
#'
#' @return A Shiny dashboard for camera trap survey data.
#'
#' 
#' @examples
#' 
#' \dontrun{
#' data("camtraps")
#' data("recordTableSample")
#'  
#'   surveyDashboard(
#'     CTtable = camtraps,
#'     recordTable = recordTableSample,
#'     xcol = "utm_x",
#'     ycol = "utm_y",
#'     crs = "epsg:32650",      # = UTM50N (Sabah, Malaysian Borneo)
#'     stationCol = "Station",
#'     setupCol = "Setup_date",
#'     retrievalCol = "Retrieval_date",
#'     CTdateFormat = "dmy"
#'   )
#'   }
#'   
#'   
#'
#' @note Current limitations include:
#' 
#' - supports only single-season data
#' 
#' - only single-species occupancy models
#' 
#' - no random or interaction effects in occupancy models
#' 
#' - no (spatial) model predictions yet (planned for Q4/2023)
#' 
#' - no support for spatial capture-recapture models (or anything related to individual IDs)
#' 
#' - only models in unmarked, no Bayesian models in ubms yet (likely to be included later)
#' 
#' 
#' @author Juergen Niedballa
#'  
#' @importFrom grDevices hcl.colors
#' @importFrom stats formula reorder
#' @importFrom graphics layout
#' @importFrom lubridate is.Date parse_date_time
#'  
#' 
#' @export
#' 
# ideas for future development:
# - export models / fitList / response curves (to workspace and/or save to disk)
# - language argument which offers translated uI?
# - ubms + unmarked?
# - interactions / random effects?
# - crs optional if CTtable is sf object
# 
surveyDashboard <- function(CTtable,
                            recordTable,
                            stationCol,
                            cameraCol = NULL,
                            xcol,
                            ycol,
                            crs,
                            setupCol,
                            retrievalCol,
                            hasProblems = FALSE,
                            CTdateFormat = "ymd",
                            camerasIndependent,
                            speciesCol = "Species",
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "ymd HMS",
                            timeZone = "UTC",
                            exclude = NULL) {
  
  
  pkg_required <- c(
    "shiny",
    "shinyWidgets",
    "shinydashboard",
    "dplyr",
    "DT",
    "ggplot2",
    "dplyr",
    "plotly",
    "patchwork",
    "mapview",
    "leaflet",
    "sf")
  
  for(pkg in pkg_required){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Please install the package", pkg, " to use the dashboard."))
    }
  }
  
  # first remove all empty columns in CTtable 
  CTtable <- CTtable[, sapply(CTtable, FUN = function(x) !all(is.na(x)))]
  
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Survey dashboard"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Summary", tabName = "Summary", icon = shiny::icon("house")),
        shinydashboard::menuItem("Overview map", tabName = "overview_map", icon = shiny::icon("map")),
        shinydashboard::menuItem("Cameras trap stations", tabName = "camera_table", icon = shiny::icon("camera")),
        shinydashboard::menuItem("Records", tabName = "record_table", icon = shiny::icon("paw")),
        shinydashboard::menuItem("Species detection maps", tabName = "Maps", icon = shiny::icon("map-marked-alt")),
        shinydashboard::menuItem("Camera operation matrix", tabName = "CameraOperation", icon = shiny::icon("clapperboard")),
        shinydashboard::menuItem("Detection History", tabName = "DetectionHistory", icon = shiny::icon("bars-staggered")),
        shinydashboard::menuItem("Occupancy models", tabName = "Occupancy", icon = shiny::icon("calculator")),
        shinydashboard::menuItem("Single species activity density", tabName = "ActivityDensity", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Two-Species activity overlap", tabName = "TwoSpeciesOverlap", icon = shiny::icon("chart-area"))
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "Summary",
          shiny::fluidRow(
            # first row (stations, species)
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("num_stations",
                width = NULL
              )
            ),
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("num_images",
                width = NULL
              )
            ),
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("num_species",
                width = NULL
              )
            ),
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("avg_records_per_station",
                width = NULL
              )
            )
          ),
          
          # second row (dates / trap nights)
          shiny::fluidRow(
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("date_range_min",
                width = NULL
              )
            ),
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("date_range_max",
                width = NULL
              )
            ),
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("trap_nights",
                width = NULL
              )
            ),
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("trap_nights_avg",
                width = NULL
              )
            )
          ),

          # third row: MCP, and space for some more stuff
          shiny::fluidRow(
            shinydashboard::box(
              width = 3,
              shinydashboard::valueBoxOutput("area_mcp",
                width = NULL
              )
            )
          ),


          # fourth row: plots
          shiny::fluidRow(
            shinydashboard::box(
              width = 4,
              plotly::plotlyOutput("plot_n_records")
            ),
            shinydashboard::box(
              width = 4,
              plotly::plotlyOutput("plot_n_species")
            ),
            shinydashboard::box(
              width = 4,
              plotly::plotlyOutput("plot_n_stations")
            )
          )
        ),
        
        
        shinydashboard::tabItem(tabName = "overview_map", 
                                leaflet::leafletOutput("overview_map",
                                                       width = NULL,
                                                       height = "800px"
                                                       )),
        
        
        shinydashboard::tabItem(tabName = "camera_table",
                shiny::tabsetPanel(
                  shiny::tabPanel(title = "Camera trap table",     DT::dataTableOutput("camera_traps_table")),
                  shiny::tabPanel(title = "Camera trap table (aggregated)", DT::dataTableOutput("camera_traps_table_agg"))
                )
        ),
        
        
        shinydashboard::tabItem(tabName = "record_table", 
                                DT::dataTableOutput("record_table")),
        
        
        shinydashboard::tabItem(
          
          
          tabName = "Maps",
          shiny::fluidRow(
            shiny::column(
              width = 3,
              offset = 1,
              shiny::selectInput(
                inputId = "species_for_map",
                label = "Species",
                choices = c("n_species", 
                            sort(unique(recordTable[, speciesCol]) [!unique(recordTable[, speciesCol]) %in% exclude])),
                selected = NULL
              )
            ),
              shiny::column(
                width = 4,
                shiny::checkboxInput(
                  inputId = "scale_size",
                  label = "Point size by value",
                  value = TRUE
                ),
                shiny::checkboxInput(
                  inputId = "no_record_more_transparent",
                  label = "Points without records transparent",
                  value = TRUE
                )
              )
            ),
          leaflet::leafletOutput("maps",
            width = NULL,
            height = "800px"
          )
        ),
        
        
        shinydashboard::tabItem(
          tabName = "CameraOperation",
          plotly::plotlyOutput("camop")
        ),
        
        
        shinydashboard::tabItem(
          tabName = "DetectionHistory",
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::selectInput(
                inputId = "species_dethist",
                label = "Species",
                choices = sort(unique(recordTable[, speciesCol]) [!unique(recordTable[, speciesCol]) %in% exclude]),
                selected = NULL
              )
            ),
            shiny::column(
              3,
              shiny::sliderInput("occasionLength", "Occasion Length (in days)",
                min = 1, max = 20, value = 5,
                step = 1, ticks = FALSE
              )
            ),
            shiny::column(
              3,
              shiny::selectInput("outputType", "Output Type", choices = c("binary", "count"), selected = "binary")
            ),
            shiny::column(
              3,
              shiny::selectInput("day1", "Day 1", choices = c("survey", "station"), selected = "survey")
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(width = 3,
                shinydashboard::valueBoxOutput("dethist_n_records",
                               width = NULL)
            ),
            shinydashboard::box(width = 3,
                shinydashboard::valueBoxOutput("dethist_n_detections",
                               width = NULL)
            ),
            shinydashboard::box(width = 3,
                shinydashboard::valueBoxOutput("dethist_n_stations",
                               width = NULL)
            ),
            shinydashboard::box(width = 3,
                shinydashboard::valueBoxOutput("dethist_percent_1s",
                               width = NULL)
            )
          ),
          plotly::plotlyOutput("detectionHistory"),
          shiny::actionButton("return_dethist", "Return detection history") # ,
        ),
        
        
        shinydashboard::tabItem(
          tabName = "Occupancy",
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::varSelectizeInput("covariates_on_detection", "Detection covariates", data = CTtable, multiple = T, options = list(selectize = T)),
            ),
            shiny::column(
              3,
              shiny::varSelectizeInput("covariates_on_occupancy", "Occupancy covariates", data = CTtable, multiple = T, options = list(selectize = T)),
            ),
            shiny::column(
              1,
              shiny::checkboxInput("effort_on_detection", "Effort on detection", value = FALSE)
            ),
            shiny::column(
              1,
              shiny::checkboxInput("scale_covariates", "Scale covariates", value = FALSE)
            ),
            shiny::column(
              2,
              shiny::actionButton(
                "model_to_modsel",
                label = "Add to model selection"
              )
            ) 
          ),
          shiny::fluidRow(
            shinydashboard::box(
              shiny::verbatimTextOutput("summary_occu"),
              title = "Model summary", 
              status = "primary",
              width = 6,
              collapsible = TRUE
            ),
            shinydashboard::box(
              title = "Confidence intervals of model coefficients", 
              status = "primary",
              width = 6,
              collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(width = 6, shiny::numericInput("pval", "p-value:", min = 0, max = 1, value = 0.95, step = 0.01)),
                shiny::column(width = 6, shiny::numericInput("digits", "Digits:", value = 2), min = 0, max = 5, step = 1)
              ),
              shiny::textOutput("model_coef_det_header", inline = T),
              shiny::verbatimTextOutput("model_coef_det"),
              shiny::textOutput("model_coef_state_header", inline = T),
              shiny::verbatimTextOutput("model_coef_state")
            )
          ),
          shinydashboard::box(
            shiny::fluidRow(
              shinydashboard::box(
                title = "Detection covariates",
                width = 6,
                shiny::plotOutput("plot_effect_det")
              ),
              shinydashboard::box(
                title = "Occupancy covariates",
                width = 6,
                shiny::plotOutput("plot_effect_occu")
              )
            ),
            width = 12,
            status = "info",
            title = "Covariate responses",
            collapsible = TRUE
          ),
          shinydashboard::box(
            title = "Model selection",
            collapsible = TRUE,
            status = "info",
            shiny::tableOutput("model_selection_table"),
            shiny::fluidRow(shiny::column(width = 12, 
                            shiny::actionButton(
                              "clear_modsel",
                              label = "Clear model selection"
                            )
            )
            )
          )
        ),
        
        
        shinydashboard::tabItem(
          tabName = "ActivityDensity",
          shiny::fluidRow(
            shiny::column(width = 12, shiny::selectInput("ad_species", 
                                           "Species", 
                                           choices = sort(unique(recordTable[, speciesCol]) [!unique(recordTable[, speciesCol]) %in% exclude])
                                           )
                   )
          ),
          shiny::fluidRow(
            shiny::column(width = 12, shiny::plotOutput("activity_density_plot"))
          )
        ),
        
        
        shinydashboard::tabItem(
          tabName = "TwoSpeciesOverlap",
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput("speciesA", "Select species A:",
                choices = sort(unique(recordTable[, speciesCol]) [!unique(recordTable[, speciesCol]) %in% exclude]),
                selected = sort(unique(recordTable[, speciesCol]))[1]
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput("speciesB", "Select species B:",
                choices = sort(unique(recordTable[, speciesCol]) [!unique(recordTable[, speciesCol]) %in% exclude]),
                selected = sort(unique(recordTable[, speciesCol]))[2]
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(width = 12, shiny::plotOutput("actOverlapPlot"))
          )
        )
      )
    )
  )



  server <- function(input, output) {
    
    # camera operation matrix
    camop_args <- list(
      CTtable = CTtable,
      setupCol = setupCol,
      retrievalCol = retrievalCol,
      dateFormat = CTdateFormat,
      hasProblems = hasProblems,
      stationCol = stationCol
    )


    if (!is.null(cameraCol)) {
      camop_args <- c(camop_args,
        cameraCol = cameraCol,
        byCamera = FALSE,
        allCamsOn = FALSE,
        camerasIndependent = camerasIndependent
      )
    }

    camop <- do.call(cameraOperation, camop_args)



    # aggregate camera trap table, if needed
    if(!is.null(cameraCol)) {
      
      # define function
      aggregateCTtableByStation <- function(df, stationCol) {
        
        # Identify data types
        num_cols     <- sapply(df, is.numeric)
        char_cols    <- sapply(df, is.character)
        factor_cols  <- sapply(df, is.factor)
        logical_cols <- sapply(df, is.logical)
        date_cols    <- sapply(df, is.Date)
        
        # Aggregate
        agg_fun <- function(x) {
          if (is.numeric(x)) {
            mean(x, na.rm = TRUE)
          } else if (is.logical(x)) {
            mean(as.integer(x), na.rm = TRUE)
          } else if (is.character(x) | is.factor(x)) {
            paste(unique(x), collapse = ", ")
          } else if (is.Date(x)) {
            paste(unique(x), collapse = ", ")
          } else {
            paste(unique(x), collapse = ", ") 
          }
        }
        
        df_agg <- aggregate(df, 
                            by = list(df[, stationCol]), 
                            FUN = agg_fun)
        df_agg[, stationCol] <- NULL
        
        colnames(df_agg) <- c(stationCol, 
                              names(df_agg)[-1])
        
        return(df_agg)
        # factors are converted to characters. Problematic?
      }
      
      CTtable_agg <- aggregateCTtableByStation(CTtable, stationCol = stationCol)
      stopifnot(rownames(camop) == CTtable_agg[, stationCol])
      
      df_covariates <- CTtable_agg
    } else {
      df_covariates <- CTtable
    }
    
    
    
    # remove excluded records (silently at the moment)
    if (!is.null(exclude)) {
      num_images_excluded <- sum(recordTable[, speciesCol] %in% exclude)
      recordTable <- recordTable[!recordTable[, speciesCol] %in% exclude, ]
    } else {
      num_images_excluded <- 0
    }

    # Calculate the number of unique stations and species
    num_stations <- length(unique(CTtable[, stationCol]))
    num_species <- length(unique(recordTable[, speciesCol]))

    # Calculate the date range
    date_range_min <- as.Date(min(parse_date_time(CTtable[, setupCol], orders = CTdateFormat)))
    date_range_max <- as.Date(max(parse_date_time(CTtable[, retrievalCol], orders = CTdateFormat)))

    # Calculate trap nights
    trap_nights <- round(sum(camop, na.rm = T), 1)
    trap_nights_avg <- round(mean(apply(camop, 1, sum, na.rm = T)), 1)

    # Calculate number of images and average number of records per station
    num_images <- nrow(recordTable)
    avg_records_per_station <- round(nrow(recordTable) / num_stations, 1)

    # Render the value boxes
    output$num_stations <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_stations,
        subtitle = "Stations",
        icon = shiny::icon("map-marker")
      )
    })

    output$num_images <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_images,
        subtitle = "Images",
        icon = shiny::icon("camera")
      )
    })
    
    output$num_images_removed <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_images_excluded,
        subtitle = "Images",
        icon = shiny::icon("user-minus")
      )
    })

    output$num_species <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_species,
        subtitle = "Species detected",
        icon = shiny::icon("paw")
      )
    })

    output$avg_records_per_station <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = avg_records_per_station,
        subtitle = "Avg records per station",
        icon = shiny::icon("table")
      )
    })

    output$date_range_min <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = date_range_min,
        subtitle = "First survey day",
        icon = shiny::icon("calendar-plus")
      )
    })

    output$date_range_max <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = date_range_max,
        subtitle = "Last survey day",
        icon = shiny::icon("calendar-minus"),
        width = NULL
      )
    })

    output$trap_nights <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = trap_nights,
        subtitle = "Trap nights (total)",
        icon = shiny::icon("calendar-days"),
        width = NULL
      )
    })

    output$trap_nights_avg <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = trap_nights_avg,
        subtitle = "Trap nights (average per station)",
        icon = shiny::icon("calendar-day"),
        width = NULL
      )
    })

    # plot number of records (by species)
    species_order <- rev(levels(reorder(
      factor(recordTable[[speciesCol]]),
      -table(recordTable[[speciesCol]])[factor(recordTable[[speciesCol]])]
    )))
    df_n_records <- as.data.frame(table(recordTable[, speciesCol]))
    colnames(df_n_records) <- c(speciesCol, "Count")
    df_n_records[, speciesCol] <- factor(df_n_records[, speciesCol],
      levels = species_order
    )

    
    # null column names used in ggplot call to avoid CRAN notes
    # find a better solution later, e.g. using .data
    Count <- NULL
    n_species <- NULL
    n_stations <- NULL

    output$plot_n_records <- plotly::renderPlotly({
      plotly::ggplotly(
        ggplot(df_n_records, aes(y = !!ggplot2::sym(speciesCol), x = Count)) +
          geom_col() +
          theme_bw() +
          labs(y = speciesCol) +
          ggtitle(label = "Number of records by species")
      )
    })

    # plot number of observed species (by station)
    df_n_species <- data.frame(tapply(recordTable[, speciesCol], INDEX = recordTable[, stationCol], 
                                      FUN = function(x) {length(unique(x))}))
    colnames(df_n_species) <- "n_species"
    df_n_species[, stationCol] <- rownames(df_n_species)
    station_order <- order(df_n_species[, "n_species"], decreasing = F)

    output$plot_n_species <- plotly::renderPlotly({
      plotly::ggplotly(
        ggplot(df_n_species, aes(y = !!ggplot2::sym(stationCol), x = n_species)) +
          geom_col() +
          theme_bw() +
          labs(y = stationCol, x = "Observed species") +
          scale_y_discrete(limits = df_n_species[station_order, stationCol]) +
          ggtitle(label = "Number of observed species by station")
      )
    })

    # plot number of stations with detections (by species)
    df_n_stations <- data.frame(tapply(recordTable[, stationCol], INDEX = recordTable[, speciesCol], 
                                       FUN = function(x) {length(unique(x))}))
    colnames(df_n_stations) <- "n_stations"
    df_n_stations[, speciesCol] <- rownames(df_n_stations)
    species_order <- df_n_stations[order(df_n_stations[, "n_stations"], decreasing = F), speciesCol]

    output$plot_n_stations <- plotly::renderPlotly({
      plotly::ggplotly(
        ggplot(df_n_stations, aes(y = !!ggplot2::sym(speciesCol), x = n_stations)) +
          geom_col() +
          theme_bw() +
          labs(y = speciesCol, x = "Number of stations") +
          scale_y_discrete(limits = species_order) +
          ggtitle(label = "Number of stations with detections (by species)")
      )
    })

    # TAB: camera traps table
    output$camera_traps_table <- DT::renderDataTable({
      DT::datatable(
        CTtable,
        options = list(pageLength = 10,
                       scrollX = TRUE)
      )
    })
    
    # TAB: camera traps table (aggregated)
    if(!is.null(cameraCol)) {
      output$camera_traps_table_agg <- DT::renderDataTable({
        DT::datatable(
          CTtable_agg,
          options = list(pageLength = 10,
                         scrollX = TRUE)
        )
      })
    } else {
      output$camera_traps_table_agg <- DT::renderDataTable({
        DT::datatable(
          NULL,
          options = list(pageLength = 10)
        )
      })
    }

    # TAB: record table
    output$record_table <- DT::renderDataTable({
      DT::datatable(
        recordTable,
        options = list(pageLength = 10,
                       scrollX = TRUE)
      )
    })

    # Tab: Maps
    detmaps <- suppressWarnings(detectionMaps(
      CTtable = CTtable,
      recordTable = recordTable,
      stationCol = stationCol,
      speciesCol = speciesCol,
      Xcol = xcol,
      Ycol = ycol,
      plotR = FALSE
    ))


    detmaps_sf <- sf::st_as_sf(
      detmaps,
      coords = c(xcol, ycol),
      crs = sf::st_crs(crs)
    )


    output$maps <- leaflet::renderLeaflet({

      if (input$species_for_map != "n_species") {
        species_tmp <- gsub("[[:space:][:punct:]]+", ".", input$species_for_map)
        layer.name <- input$species_for_map
      } else {
        species_tmp <- input$species_for_map
        layer.name <- "Species richness (observed)"
      }


      detmaps_sf_logi <- detmaps_sf
      detmaps_sf_logi[[species_tmp]] <- detmaps_sf_logi[[species_tmp]] >= 1
      detmaps_sf_logi <- detmaps_sf_logi[detmaps_sf_logi[[species_tmp]], ]

      # make stations without records more transparent
      if(input$no_record_more_transparent) {
        # numeric values doesn't seem to matter, but it is more transpent, so ok
        alpha <- ifelse(detmaps_sf[[species_tmp]] >= 1, 0.9, 0.85)
      } else {
        alpha <- 0.9
      }
      detmaps_sf$alpha <- alpha

      m <- mapview::mapview(
        detmaps_sf,
        xcol = xcol,
        ycol = ycol,
        zcol = species_tmp,
        label = detmaps_sf[[stationCol]],
        color = hcl.colors(100, "viridis"), # somehow this is not consistent across species
        cex = ifelse(input$scale_size, species_tmp, 10),
        alpha.regions = "alpha",
        layer.name = layer.name #input$species_for_map
      )

      # add little black dots at stations with detections
      if (input$species_for_map != "n_species" & !input$scale_size) {
        m <- m + mapview::mapview(
          detmaps_sf_logi,
          xcol = xcol,
          ycol = ycol,
          zcol = species_tmp,
          label = FALSE, 
          popup = FALSE,
          legend = FALSE,
          color = "black",
          col.regions = "black",
          alpha.regions = 1,
          cex = 2,
          layer.name = "Stations with detections"
        )
      }
      m@map
    })


    CTtable_sf <- sf::st_as_sf(CTtable,
      coords = c(xcol, ycol),
      crs = crs
    )


    # calculate survey areas as minimum convex polygon
    mcp <- sf::st_convex_hull(sf::st_union(CTtable_sf))
    area_mcp_m2 <- sf::st_area(mcp)
    area_mcp_km2 <- units::set_units(area_mcp_m2, "km^2")

    output$area_mcp <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = paste(round(as.numeric(area_mcp_km2), 2), "km\U00B2"),
        subtitle = "Survey area (MCP)",
        icon = shiny::icon("vector-square")
      )
    })


    output$overview_map <- leaflet::renderLeaflet({
      m <- mapview::mapview(
        CTtable_sf,
        xcol = xcol,
        ycol = ycol,
        label = CTtable_sf[[stationCol]],
        legend = FALSE,
        cex = 10,
        layer.name = "Camera trap stations"
      ) + mapview::mapview(mcp,
        col.regions = "firebrick",
        color = "firebrick",
        alpha.regions = 0.1,
        layer.name = paste0(
          "Minimum convex polygon (",
          paste(round(as.numeric(area_mcp_km2), 2), "km^2"),
          ")"
        ),
        hide = TRUE
      )
      m@map
    })



    # Tab: Camera Operation

    camop_df <- as.data.frame(camop)
    camop_df[, stationCol] <- rownames(camop_df)

    camop_df <- reshape2::melt(camop_df, id.vars = stationCol)
    colnames(camop_df)[colnames(camop_df) == "variable"] <- "Date"
    colnames(camop_df)[colnames(camop_df) == "value"] <- "Effort"


    output$camop <- plotly::renderPlotly({

        plotly::layout(p = plotly::plot_ly(camop_df,
                                           x = ~Date, y = ~ get(stationCol), z = ~Effort,
                                           type = "heatmap",
                                           colors = hcl.colors(100, palette = "viridis", rev = T)
                                           ),
                       yaxis = list(
                         title = stationCol,
                         categoryorder = "category descending"
                         )
        )
      }
      )


    # Tab: detectionHistory

    # container for saving reactive objects
    my_object <- shiny::reactiveValues()


    detection_hist <- shiny::reactive({
      
      # get user inputs
      species <- input$species_dethist
      occasionLength <- input$occasionLength
      outputType <- input$outputType
      day1 <- input$day1
      datesAsOccasionNames <- input$datesAsOccasionNames
      
      
      
      detection_hist <- detectionHistory(
        recordTable = recordTable,
        camOp = camop,
        species = species,
        occasionLength = occasionLength,
        day1 = day1,
        output = outputType,
        scaleEffort = FALSE,
        timeZone = timeZone,
        datesAsOccasionNames = datesAsOccasionNames,
        stationCol = stationCol,
        speciesCol = speciesCol,
        recordDateTimeCol = recordDateTimeCol,
        recordDateTimeFormat = recordDateTimeFormat
        
      )
      
      my_object$detection_hist <- detection_hist 
      my_object$occasionLength <- occasionLength
      my_object$species        <- species
      my_object$outputType     <- outputType
      my_object$day1           <- day1
      
      stopifnot(df_covariates[, stationCol] == rownames(detection_hist[[1]]))
      
      return(detection_hist)
    })

    
    
    occu_result <- shiny::reactive({
           
      covariates_on_detection <- input$covariates_on_detection
      covariates_on_occupancy <- input$covariates_on_occupancy
      effort_on_detection     <- input$effort_on_detection
      scale_covariates        <- input$scale_covariates
      
      if(input$scale_covariates) {
        numeric_cols <- sapply(df_covariates, is.numeric) 
        numeric_cols_to_scale <- names(numeric_cols)[numeric_cols] %in% colnames(df_covariates)
        
        if(any(numeric_cols_to_scale)) {
          df_covariates_scale <- scale(df_covariates [, names(numeric_cols)[numeric_cols][numeric_cols_to_scale], drop = F])
          df_covariates <- cbind( df_covariates [,!colnames(df_covariates) %in% names(numeric_cols)[numeric_cols][numeric_cols_to_scale]], 
                                  df_covariates_scale)
        } 
      }
      
      # suppressWarnings(    # to avoid: Warning: siteCovs contains characters. Converting them to factors.
        umf <- unmarked::unmarkedFrameOccu(
          y = detection_hist()[[1]],
          siteCovs = df_covariates,
          obsCovs = list(effort = detection_hist()[[2]])
        )
      # )
      

      # Use the detectionhist object and user inputs as arguments to the occu() function
      formula_tmp <- 
        formula(paste0(
          "~", ifelse(!effort_on_detection & length(covariates_on_detection) == 0, 1,
            paste(
              paste(covariates_on_detection, collapse = " + "),
              ifelse(effort_on_detection, paste(ifelse(length(covariates_on_detection) >= 1, "+", ""), "effort"), " ")
            )
          ),
          "~", ifelse(length(covariates_on_occupancy) == 0, "1",
            paste(covariates_on_occupancy, collapse = " + ")
          )
        ))

      occu_result <- unmarked::occu(formula = formula_tmp, data = umf)
      return(occu_result)
    })

    output$summary_occu <- shiny::renderPrint({
      summary(occu_result())
    })
    
    
    output$model_coef_det_header <- shiny::renderText({
      "Detection submodel:"
    })
    output$model_coef_det <- shiny::renderPrint({
      round(unmarked::confint(occu_result(), type = "det", level = input$pval), input$digits)
    })
    output$model_coef_state_header <- shiny::renderText({
      "Occupancy submodel:"
    })
    output$model_coef_state <- shiny::renderPrint({
      round(unmarked::confint(occu_result(), type = "state", level = input$pval), input$digits)
    })
    

    # Save model to fitList if requested
    shiny::observeEvent(input$model_to_modsel, {
      my_object$modList <- c(my_object$modList, list(occu_result()))
    })
    
    # Reset fitList when detection history changes
    shiny::observeEvent(my_object$detection_hist, {
      my_object$modList <- list()
    })
    
    # Reset fitList if requested by user
    shiny::observeEvent(input$clear_modsel, {
      my_object$modList <- list()
    })
    
    # Render model selection results table
    output$model_selection_table <- shiny::renderTable({
    
      if(length(my_object$modList) > 0) {
        
        fl <- unmarked::fitList(fits = my_object$modList)
        ms <- unmarked::modSel(fl)
        
        df_ms <- round(ms@Full[, c("nPars", "AIC", "delta", "AICwt", "cumltvWt", "Rsq")], 2)
        df_ms <- cbind(formula = ms@Full$formula, 
                       df_ms)
        rownames(df_ms) <- rownames(ms@Full)
        return(df_ms)
      } else {
        NULL
      }
    },
    rownames = F)


    # detection history function
    output$detectionHistory <- plotly::renderPlotly({


      my_object$covariates_on_detection <- input$covariates_on_detection
      my_object$covariates_on_occupancy <- input$covariates_on_occupancy
      my_object$effort_on_detection     <- input$effort_on_detection


      # convert detection history outputs to data frames
      dh1_df <- as.data.frame(detection_hist()[[1]])
      dh1_df[, stationCol] <- rownames(dh1_df)
      dh1_df <- reshape2::melt(dh1_df, id.vars = stationCol)
      colnames(dh1_df)[colnames(dh1_df) == "variable"] <- "Occasion"
      colnames(dh1_df)[colnames(dh1_df) == "value"] <- "Detection"


      dh2_df <- as.data.frame(detection_hist()[[2]])
      dh2_df[, stationCol] <- rownames(dh2_df)
      dh2_df <- reshape2::melt(dh2_df, id.vars = stationCol)
      colnames(dh2_df)[colnames(dh2_df) == "variable"] <- "Occasion"
      colnames(dh2_df)[colnames(dh2_df) == "value"] <- "Effort"


      # make value boxes for number of records / number of 1s in matrix

      output$dethist_n_records <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = sum(recordTable[, speciesCol] == my_object$species),
          subtitle = "Records",
          icon = shiny::icon("hashtag")
        )
      })

      output$dethist_n_detections <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = sum(dh1_df$Detection, na.rm = T),
          subtitle = "Detections in matrix",
          icon = shiny::icon("filter")
        )
      })

      output$dethist_n_stations <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = sum(tapply(dh1_df$Detection, INDEX = dh1_df[, stationCol], sum, na.rm = T) >= 1),
          subtitle = "Stations with detections",
          icon = shiny::icon("location-dot")
        )
      })

      output$dethist_avg_effort <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = round(mean(tapply(dh2_df$Effort, INDEX = dh2_df[, stationCol], sum, na.rm = T)), 1),
          subtitle = "Avg effort per station (days)",
          icon = shiny::icon("eye")
        )
      })

      output$dethist_percent_1s <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = paste(round(mean(dh1_df$Detection >= 1, na.rm = T) * 100, 1), "%"),
          subtitle = "Occasions with detection",
          icon = shiny::icon("percent")
        )
      })



      # plot detection history outputs with plotly
      fig1 <- plotly::layout(plotly::plot_ly(dh1_df,
                                             x = ~Occasion, y = ~ get(stationCol), z = ~Detection,
                                             type = "heatmap",
                                             colors = hcl.colors(length(unique(na.omit(dh1_df$Detection))),
                                                                 palette = "Blu-Yl", rev = T)
                                             ),
                                             yaxis = list(
                                               title = stationCol,
                                               categoryorder = "category descending"
                                             )
      )
      
      fig2 <- plotly::layout(plotly::plot_ly(dh2_df,
                                             x = ~Occasion, y = ~ get(stationCol), z = ~Effort,
                                             type = "heatmap",
                                             colors = hcl.colors(10, palette = "viridis", rev = T)
      ),
      yaxis = list(
        title = stationCol,
        categoryorder = "category descending"
      )
      )
      
      plotly::layout(p = plotly::subplot(fig1, fig2,
                                           shareY = T, shareX = T,
                                           titleX = T, titleY = T
                                           ),
                       annotations = list(
                         list(x = 0.2, y = 1.05, text = "Detections", showarrow = F, xref = "paper", yref = "paper"),
                         list(x = 0.8, y = 1.05, text = "Effort", showarrow = F, xref = "paper", yref = "paper")
                         )
                       )
        }
      )



    # plot covariate effect on detection probability
    output$plot_effect_det <- shiny::renderPlot({
      values_list <- shiny::reactiveValuesToList(input)

      if (values_list$effort_on_detection) {
        detcovs <- c(values_list$covariates_on_detection, "effort")
      } else {
        detcovs <- values_list$covariates_on_detection
      }

      detcovs <- as.character(detcovs)
     
      if (length(detcovs) == 0) {
        return(NULL)
      }

      effects_det_data <- lapply(detcovs,
        FUN = unmarked::plotEffectsData,
        object = occu_result(),
        type = "det"
      )

      names(effects_det_data) <- detcovs
      
      # avoid CRAN notes
      covariateValue <- Predicted <- lower <- upper <- NULL


      effects_det <- lapply(1:length(effects_det_data), FUN = function(i) {
        x <- effects_det_data[[i]]
        p <- ggplot(x, aes(x = covariateValue, y = Predicted))
        if (is.numeric(x$covariateValue)) {
          p <- p +
            geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
            geom_line(linewidth = 1)
        }
        if (is.factor(x$covariateValue)) {
          p <- p +
            geom_pointrange(aes(ymin = lower, ymax = upper),
              fatten = 5,
              linewidth = 1
            )
        }
        p <- p +
          theme_bw() +
          theme(panel.grid.minor = element_blank()) +
          xlab(label = detcovs[i]) +
          ylab(label = "Predicted detection") +
          labs(title = detcovs[i]) +
          ylim(0, 1)
      })

      Reduce("+", effects_det) # combine with patchwork
    })


    # plot covariate effect on occupancy probability
    output$plot_effect_occu <- shiny::renderPlot({
      occucovs <- input$covariates_on_occupancy
      occucovs <- as.character(occucovs)

      if (length(occucovs) == 0) {
        return(NULL)
      }

      effects_occu_data <- lapply(occucovs,
        FUN = unmarked::plotEffectsData,
        object = occu_result(),
        type = "state"
      )
      names(effects_occu_data) <- occucovs

      effects_occu <- lapply(1:length(effects_occu_data), FUN = function(i) {
        x <- effects_occu_data[[i]]
        p <- ggplot(x, aes(x = covariateValue, y = Predicted))
        if (is.numeric(x$covariateValue)) {
          p <- p +
            geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
            geom_line(linewidth = 1)
        }
        if (is.factor(x$covariateValue)) {
          p <- p +
            geom_pointrange(aes(ymin = lower, ymax = upper),
              fatten = 5,
              linewidth = 1
            )
        }
        p <- p +
          theme_bw() +
          theme(panel.grid.minor = element_blank()) +
          xlab(label = occucovs[i]) +
          ylab(label = "Predicted occupancy") +
          labs(title = occucovs[i]) +
          ylim(0, 1)
      })

      Reduce("+", effects_occu) # combine with patchwork
    })

    
    
    # ALternative, trying to avoid CRAN note
    # function loading results in global environment
    assign_to_global <- function(pos=1){   # pos defaults to 1 which equals an assingment to global environment
      assign(object_name, detection_hist(), envir=as.environment(pos) )
    }
    
    shiny::observeEvent(input$return_dethist, {
      object_name <- paste0("dethist", "_", 
                            gsub(" ", "_", my_object$species), "_",
                            my_object$occasionLength, "d", "_", 
                            my_object$outputType, "_",
                            "day1_", my_object$day1)
      
      #ä Save the detection_hist to the R workspace
      ## causes CRAN note :/
      # assign(object_name,
      #   detection_hist(),
      #   envir = .GlobalEnv
      # )
      

      assign_to_global()
      

      # Show a message confirming that the object has been saved
      shiny::showModal(shiny::modalDialog(
        title = "Object Saved",
        paste("The detection history object has been saved to the workspace as:", object_name)
      ))
    })


    # Tab: ActivityDensity
    output$activity_density_plot <- shiny::renderPlot({
      activityDensity(
        recordTable = recordTable,
        species = input$ad_species,
        speciesCol = speciesCol,
        recordDateTimeCol = recordDateTimeCol,
        recordDateTimeFormat = recordDateTimeFormat
      )
    })

    # Tab: Two-Species Overlap
    output$actOverlapPlot <- shiny::renderPlot({
      activityOverlap(
        recordTable = recordTable,
        speciesA = input$speciesA,
        speciesB = input$speciesB,
        speciesCol = speciesCol,
        recordDateTimeCol = recordDateTimeCol,
        recordDateTimeFormat = recordDateTimeFormat,
        main = paste0("Activity overlap: ", input$speciesA, " - ", input$speciesB)
      )
    })
  }

  shiny::shinyApp(ui, server)
}

