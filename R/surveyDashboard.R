#' Survey Dashboard for Camera Trap Data Analysis
#'
#' @description 
#' A comprehensive Shiny dashboard for analyzing camera trap survey data. The dashboard provides 
#' interactive visualization, data exploration, and analysis tools including:
#' 
#' \itemize{
#'   \item Data import from CSV files, Wildlife Insights exports, or camtrapDP format
#'   \item Interactive maps for camera locations and species detections
#'   \item Species activity pattern analysis
#'   \item Covariate extraction and analysis tools
#'   \item Single-species and community occupancy modeling
#'   \item Spatial prediction capabilities
#' }
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
#' @return A Shiny dashboard application for camera trap survey data analysis
#'
#'
#' @details
#' The dashboard includes several major components:
#' 
#' \strong{Data Import & Management:}
#' \itemize{
#'   \item CSV file import with column mapping
#'   \item Wildlife Insights data import (zip, CSV, or directory)
#'   \item Support for camtrapDP format (coming soon)
#'   \item Study area import from shapefiles
#'   \item Save/restore functionality for app state
#' }
#'
#' \strong{Data Processing:}
#' \itemize{
#'   \item Flexible station filtering with multiple criteria
#'   \item Temporal record filtering with independence criteria
#'   \item Automated covariate extraction from local rasters or online 
#'   elevation models
#'   \item Covariate correlation analysis with visualization
#' }
#'
#' \strong{Basic Analysis:}
#' \itemize{
#'   \item Basic summary statistics
#'   \item Interactive species detection maps
#'   \item Activity pattern analysis (single species and two-species overlap)
#'   \item Camera operation visualization
#' }
#'
#' \strong{Occupancy Modeling:}
#' \itemize{
#'   \item Basic workflow for simple model specification (linear effects)
#'   \item Advanced workflow for complex models (linear, quadratic, interaction, 
#'   random effects)
#'   \item Support for both unmarked and ubms packages
#'   \item Automated detection history creation
#'   \item Model comparison and selection
#'   \item Response curves and spatial predictions
#' }
#'
#' \strong{Community Occupancy Modeling:}
#' \itemize{
#'   \item Flexible species selection with filtering
#'   \item Support for fixed, random, and independent effects
#'   \item Species-site random effects
#'   \item Effort handling on detection
#'   \item MCMC diagnostics and convergence assessment
#'   \item Species richness and PAO predictions
#' }
#'
#' \strong{Visualization Improvements:}
#' \itemize{
#'   \item Response curve visualization
#'   \item MCMC diagnostics plots
#'   \item Spatial prediction maps
#' }
#'
#' @note 
#' \itemize{
#'   \item Interactive maps with multiple basemap options and customizable 
#'   color schemes and symbology
#'   \item Covariate scaling is performed automatically where appropriate
#'   \item The app state can be saved and restored
#' }
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Start the dashboard without parameters
#' # This opens the application with a welcome screen where data can be imported
#' surveyDashboard()
#' 
#' # Basic usage with minimal parameters
#' 
#' data("camtraps")
#' data("recordTableSample")
#'
#'   surveyDashboard(
#'     CTtable = camtraps,
#'     recordTable = recordTableSample,
#'     xcol = "utm_x",
#'     ycol = "utm_y",
#'     crs = "epsg:32650",      # = UTM50N
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
#' - no support for spatial capture-recapture models (or anything related to individual IDs)
#' 
#' 
#' @author Juergen Niedballa
#'  
#' @importFrom grDevices hcl.colors colorRampPalette
#' @importFrom graphics layout pairs plot.new
#' @importFrom lubridate is.Date parse_date_time
#' @importFrom utils read.csv str unzip
#' @importFrom shiny renderUI renderText outputOptions req observe observeEvent reactiveVal reactiveValues renderTable renderPrint renderPlot updateSelectInput updateSelectizeInput updateTextInput updateNumericInput updateSliderInput updateCheckboxGroupInput updateActionButton removeNotification showNotification showModal removeModal modalDialog modalButton HTML tags tabsetPanel tabPanel actionButton checkboxInput checkboxGroupInput fileInput numericInput radioButtons selectInput sliderInput textInput uiOutput verbatimTextOutput plotOutput textOutput wellPanel withProgress fluidRow column div hr h4 conditionalPanel helpText tagList tableOutput reactive reactiveTimer varSelectizeInput icon 
#' @importFrom shinydashboard dropdownMenu dropdownMenuOutput renderMenu
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom dplyr %>% group_by summarize n n_distinct pull sym
#' @importFrom sf st_buffer st_convex_hull st_drop_geometry st_intersection st_make_valid st_transform st_union
#' @importFrom terra rast vect project resample nlyr values<- mask
#' @importFrom leaflet leaflet leafletOutput renderLeaflet addTiles addCircleMarkers addLayersControl layersControlOptions addPolygons leafletProxy clearGroup
#' @importFrom ggplot2 theme_update element_text geom_violin geom_boxplot
#' 
#' @export




surveyDashboard <- function(CTtable = NULL,
                            recordTable = NULL,
                            stationCol = NULL,
                            cameraCol = NULL,
                            xcol = NULL,
                            ycol = NULL,
                            crs = NULL,
                            setupCol = NULL,
                            retrievalCol = NULL,
                            hasProblems = FALSE,
                            CTdateFormat = "ymd",
                            camerasIndependent = NULL,
                            speciesCol = "Species",
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "ymd HMS",
                            timeZone = "UTC",
                            exclude = NULL) {
  
  
  # pkg_required <- c(
  #   # UI packages
  #   "shiny",
  #   "shinyWidgets", 
  #   "shinydashboard",
  #   "DT",
  #   
  #   # Data manipulation
  #   "dplyr",
  #   "lubridate",
  #   "sf",
  #   "terra",
  #   
  #   # Visualization
  #   "ggplot2",
  #   "plotly",
  #   "patchwork",
  #   "mapview",
  #   "leaflet",
  #   "viridisLite",
  #   "scales",
  #   
  #   # Modeling
  #   "unmarked",
  #   "ubms",
  #   "bayesplot",
  #   "coda"
  # )
  # 
  # pkg_optional <- c(
  #   "corrplot",
  #   "psych",
  #   "rstudioapi",
  #   "callr"
  # )
  # 
  # # Check required packages
  # missing_required <- pkg_required[!sapply(pkg_required, requireNamespace, quietly = TRUE)]
  # if (length(missing_required) > 0) {
  #   stop("Please install the following required packages: ", 
  #        paste(missing_required, collapse = ", "))
  # }
  # 
  # # Check optional packages and warn if missing
  # missing_optional <- pkg_optional[!sapply(pkg_optional, requireNamespace, quietly = TRUE)]
  # if (length(missing_optional) > 0) {
  #   warning("The following optional packages are not installed. Some features may be limited: ",
  #           paste(missing_optional, collapse = ", "))
  # }
  # 
  
   # For now do aggressive package check until I load functions cleanly with pkg::function()
  
  # Load all required packages
  required_packages <- c(
    "shiny",
    "shinydashboard",
    "DT",
    "dplyr",
    "sf",
    "terra",
    "leaflet",
    "ggplot2",
    "plotly",
    "lubridate",
    "grDevices",
    "graphics",
    "utils",
    "stats",
    "scales",
    "corrplot",
    "unmarked",
    "ubms"
  )
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed"))
    }
    library(pkg, character.only = TRUE)
  }
  
  
  #Version check
  
  # Function to get version info from the local DESCRIPTION file
  get_local_version <- function(package = "camtrapR") {
    tryCatch({
      utils::packageVersion(package)
    }, error = function(e) {
      warning("Could not determine local version: ", e$message)
      return(NULL)
    })
  }
  
  # Function to get version info from GitHub
  get_github_version <- function(repo = "jniedballa/camtrapR") {
    tryCatch({
      # Get DESCRIPTION content from GitHub
      desc_url <- paste0("https://raw.githubusercontent.com/", repo, "/dev/DESCRIPTION")
      
      # Try to download DESCRIPTION file
      desc_content <- readLines(desc_url, warn = FALSE)
      
      # Find Version line
      version_line <- grep("^Version:", desc_content, value = TRUE)
      if (length(version_line) == 0) {
        warning("No version information found in GitHub DESCRIPTION")
        return(NULL)
      }
      
      # Extract version number
      version <- gsub("^Version:\\s*", "", version_line)
      package_version(version)
      
    }, error = function(e) {
      warning("Could not fetch GitHub version: ", e$message)
      return(NULL)
    })
  }
  
  # Function to compare versions and return notification text if needed
  check_version <- function() {
    local_ver <- get_local_version()
    github_ver <- get_github_version()
    
    if (is.null(local_ver) || is.null(github_ver)) {
      return(NULL)  # Return NULL if we couldn't get either version
    }
    
    if (github_ver > local_ver) {
      return(list(
        text = HTML(paste0("A newer version (", github_ver, ") is available on GitHub. ",
                      "You are currently using version ", local_ver, ". ",
                      "Please update to access the latest features and bug fixes:<br>",
                      "<code>remotes::install_github('jniedballa/camtrapR', ref = 'dev')</code>")),
        type = "warning"
      ))
    }
    return(NULL)  # Return NULL if current version is up to date
  }
  
  # Function to create version notification
  create_version_notification <- function() {
    version_info <- check_version()
    if (!is.null(version_info)) {
      shiny::showNotification(
        ui = version_info$text,
        type = version_info$type,
        duration = NULL,
        closeButton = TRUE,
        id = "version-check"
      )
    }
  }
  
  
  
  
  if(inherits(CTtable, "tbl")) CTtable <- as.data.frame(CTtable)
  if(inherits(recordTable, "tbl")) recordTable <- as.data.frame(recordTable)
  
  
  # first remove all empty columns in CTtable 
  CTtable <- CTtable[, sapply(CTtable, FUN = function(x) !all(is.na(x)))]
  
  
  # styling of export button
  tags$head(
    tags$style(HTML("
    .export-button-container {
      padding: 10px 10px;
      margin-top: 20px;
      text-align: center;
    }
    #export_all_data {
      width: 100%;
      white-space: normal;
      height: auto;
      min-height: 44px;
    }
    .modal-body h4 {
      margin-top: 20px;
      margin-bottom: 10px;
      color: #2c3e50;
    }
    .modal-body hr {
      margin-top: 20px;
      margin-bottom: 20px;
      border-top: 1px solid #eee;
    }
  "))
  )
  
  
  
  # Create flag indicating if data was provided via parameters
  has_params <- !is.null(CTtable) && !is.null(recordTable)
  
  
  # UI definition ####
  ui <- shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(
      title = "Survey dashboard",
      shinydashboard::dropdownMenuOutput("stateMenu")
    ),
    
    
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        # shinydashboard::menuItem("Import Data", tabName = "import_data", icon = shiny::icon("upload")),
        shinydashboard::menuItem("Import Data", tabName = "import_data", icon = shiny::icon("upload"),
                                 shinydashboard::menuSubItem("From CSV", tabName = "import_csv"),
                                 shinydashboard::menuSubItem("Wildlife Insights", tabName = "import_wi"),
                                 shinydashboard::menuSubItem("camtrapDP", tabName = "import_camtrapdp"),
                                 shinydashboard::menuSubItem("Import area of interest", tabName = "import_aoi"),
                                 shinydashboard::menuSubItem("File Size Control", tabName = "file_size_control")
        ),
        shinydashboard::menuItem("Data Summary", tabName = "Summary", icon = shiny::icon("house"), selected = T),
        shinydashboard::menuItem("Tables", icon = shiny::icon("table"),
                                 shinydashboard::menuSubItem("Cameras trap stations", tabName = "camera_table"),
                                 shinydashboard::menuSubItem("Records", tabName = "record_table")
        ),
        shinydashboard::menuItem("Maps", icon = shiny::icon("map"),
                                 shinydashboard::menuSubItem("Overview map", tabName = "overview_map"),
                                 shinydashboard::menuSubItem("Species detection maps", tabName = "Maps")
        ),
        
        shinydashboard::menuItem("Species Activity", icon = shiny::icon("chart-area"),
                                 shinydashboard::menuSubItem("Single species activity density", tabName = "ActivityDensity"),
                                 shinydashboard::menuSubItem("Two-Species activity overlap", tabName = "TwoSpeciesOverlap")
        ),
        shinydashboard::menuItem("Data Processing", icon = shiny::icon("cogs"),
                                 shinydashboard::menuSubItem("Filter Camera Traps", tabName = "filterCTData"),
                                 shinydashboard::menuSubItem("Records Temporal Filtering", tabName = "filterRecords"),
                                 shinydashboard::menuSubItem("Extract Covariates", tabName = "extract"),
                                 shinydashboard::menuSubItem("Covariate Correlation", tabName = "covariateCorrelation"),
                                 shinydashboard::menuSubItem("Camera operation matrix", tabName = "CameraOperation")
                                 
        ),
        shinydashboard::menuItem("Single-species Occupancy", icon = shiny::icon("calculator"),
                                 shinydashboard::menuSubItem("Detection History", tabName = "DetectionHistory"),
                                 shinydashboard::menuSubItem("Occupancy models", tabName = "Occupancy")
        ),
        shinydashboard::menuItem("Community Occupancy Models", tabName = "CommunityOccupancy", icon = shiny::icon("users")),
        
        
        # Add export button
        shiny::tags$li(
          class = "export-button-container",
          shiny::tags$hr(style = "margin: 10px 15px; border-top: 1px solid #4b646f;"),
          shiny::actionButton("export_all_data", "Export Data", 
                              icon = icon("download"))
        )
      )
    ),
    shinydashboard::dashboardBody(
      
      
      shiny::uiOutput("welcome_screen"),
      
      shinydashboard::tabItems(
        
        
        ## Tab: Import data ----
        
        shinydashboard::tabItem(
          tabName = "import_csv",
          shiny::tabsetPanel(
            shiny::tabPanel("Camera Trap Data",
                            shinydashboard::box(
                              title = "Import Camera Trap Data",
                              status = "primary",
                              solidHeader = TRUE,
                              width = 12,
                              shiny::fileInput("ct_file", "Upload Camera Trap CSV", accept = c(".csv")),
                              shiny::selectInput("stationCol", "Station Column", choices = NULL),
                              shiny::selectInput("cameraCol", "Camera Column (optional)", choices = NULL),
                              shiny::selectInput("xcol", "X Coordinate Column", choices = NULL),
                              shiny::selectInput("ycol", "Y Coordinate Column", choices = NULL),
                              shiny::textInput("crs", "Coordinate Reference System (e.g., 'EPSG:4326')"),
                              shiny::selectInput("setupCol", "Setup Date Column", choices = NULL),
                              shiny::selectInput("retrievalCol", "Retrieval Date Column", choices = NULL),
                              shiny::textInput("CTdateFormat", "Date Format", value = "ymd"),
                              shiny::checkboxInput("hasProblems", "Problem columns ('Problem1_from' / 'Problem1_to')?", value = FALSE),
                              shiny::actionButton("ct_done", "Done"),
                              shiny::h4("Data Preview"),
                              DT::DTOutput("ct_preview")
                            )
            ),
            shiny::tabPanel("Record Data",
                            shinydashboard::box(
                              title = "Import Record Data",
                              status = "primary",
                              solidHeader = TRUE,
                              width = 12,
                              shiny::fileInput("record_file", "Upload Record CSV", accept = c(".csv")),
                              shiny::selectInput("speciesCol", "Species Column", choices = NULL),
                              shiny::selectInput("recordDateTimeCol", "Date/Time Column", choices = NULL),
                              shiny::textInput("recordDateTimeFormat", "Date/Time Format", value = "ymd HMS"),
                              shiny::textInput("timeZone", "Time Zone", value = "UTC"),
                              shiny::textInput("exclude", "Species to Exclude (comma-separated)"),
                              shiny::actionButton("record_done", "Done"),
                              shiny::h4("Data Preview"),
                              DT::DTOutput("record_preview")
                            )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "import_wi",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Import Wildlife Insights Data",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              shiny::radioButtons("wi_import_type", "Choose import type:",
                                  choices = c("Zip file" = "zip", "Separate CSV files" = "csv", "Directory" = "directory"),
                                  selected = "zip"),
              shiny::conditionalPanel(
                condition = "input.wi_import_type == 'zip'",
                shiny::fileInput("wi_zip_file", "Upload Wildlife Insights ZIP file",
                                 accept = c(".zip"),
                                 multiple = T)
              ),
              shiny::conditionalPanel(
                condition = "input.wi_import_type == 'csv'",
                shiny::fileInput("wi_deployment_file", "Upload Deployment CSV file",
                                 accept = c(".csv")),
                shiny::fileInput("wi_detection_file", "Upload Detection CSV file",
                                 accept = c(".csv"))
              ),
              shiny::conditionalPanel(
                condition = "input.wi_import_type == 'directory'",
                shiny::textInput("wi_directory", "Enter path to Wildlife Insights data directory")
              ),
              shiny::checkboxInput("wi_cameras_independent", "Cameras are independent", value = FALSE),
              shiny::actionButton("wi_import_button", "Import Data", class = "btn-primary"),
              shiny::hr(),
              shiny::h4("Import Status"),
              shiny::verbatimTextOutput("wi_import_status"),
              shiny::h4("Data Preview"),
              shiny::tabsetPanel(
                shiny::tabPanel("Deployment Data", DT::dataTableOutput("wi_deployment_preview")),
                shiny::tabPanel("Detection Data", DT::dataTableOutput("wi_detection_preview"))
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "import_camtrapdp",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              status = "info",
              solidHeader = TRUE,
              title = "Import from camtrapDP format",
              div(
                style = "text-align: center; padding: 20px;",
                tags$h3(icon("file-import"), " Coming Soon!"),
                tags$p(
                  "Support for importing data in Camera Trap Data Package (camtrapDP) format will be available in a future update.",
                  style = "font-size: 16px; color: #666;"
                ),
                tags$p(
                  "This will enable direct import of standardized camera trap data following the camtrapDP specification.",
                  style = "font-size: 14px; color: #888;"
                )
              )
            )
          )
        ),
        
        
        shinydashboard::tabItem(
          tabName = "import_aoi",
          fluidRow(
            shinydashboard::box(
              title = "Study Area Import", width = 12, status = "primary", solidHeader = TRUE,
              shiny::textInput("aoi_path", "Path to Study Area Shapefile (.shp)", 
                               placeholder = "C:/path/to/shapefile.shp"),
              shiny::actionButton("aoi_done", "Import Study Area"),
              hr(),
              h4("Study Area Information"),
              shiny::verbatimTextOutput("aoi_info"),
              h4("Preview Map"),
              leaflet::leafletOutput("aoi_preview", height = "600px")
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "file_size_control",
          shinydashboard::box(
            title = "File Size Control",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            shiny::numericInput("max_file_size", "Maximum file size (MB)", value = 5, min = 5, max = 100),
            shiny::actionButton("update_max_size", "Update Maximum File Size")
          )
        ),
        
        ## Tab: Summary  ----
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
          
          
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              title = "Summary Plots",
              status = "primary",
              solidHeader = TRUE,
              shiny::tabsetPanel(
                shiny::tabPanel("Records by Species",
                                plotly::plotlyOutput("plot_n_records", height = "500px")
                ),
                shiny::tabPanel("Species by Station",
                                plotly::plotlyOutput("plot_n_species", height = "500px")
                ),
                shiny::tabPanel("Stations by Species",
                                plotly::plotlyOutput("plot_n_stations", height = "500px")
                )
              )
            )
          )
        ),
        
        ## Tab: Overview map  ----
        shinydashboard::tabItem(tabName = "overview_map", 
                                leaflet::leafletOutput("overview_map",
                                                       width = NULL,
                                                       height = "800px"
                                )),
        
        
        
        ## Tab: Camera table  ----
        shinydashboard::tabItem(tabName = "camera_table",
                                shiny::tabsetPanel(
                                  shiny::tabPanel(title = "Current Camera trap table",     
                                                  DT::DTOutput("current_camera_traps_table")
                                  ),
                                  shiny::tabPanel(title = "Aggregated Camera trap table", 
                                                  DT::DTOutput("aggregated_camera_traps_table")
                                  )
                                )
        ),
        
        ## Tab: Record table  ----
        shinydashboard::tabItem(tabName = "record_table", 
                                DT::dataTableOutput("record_table")),
        
        
        ## Tab: Maps  ----
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
        
        
        ## Tab:  Activity (single)  ----
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
            shiny::column(width = 12, shiny::plotOutput("activity_density_plot",
                                                        height = "600px"))
          )
        ),
        
        ## Tab: Activity ( species overlap)  ----
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
            shiny::column(width = 12, shiny::plotOutput("actOverlapPlot",
                                                        height = "600px"))
          )
        ),
        
        
        
        ## Tab: Filter Stations  ----
        
        shinydashboard::tabItem(
          tabName = "filterCTData",
          fluidRow(
            shinydashboard::box(
              title = "Filter Camera Trap Data", 
              width = 12, 
              status = "primary",
              solidHeader = TRUE,
              
              # Column selector
              fluidRow(
                column(4,
                       selectInput("filterColumn", "Select Column to Filter:", choices = NULL),
                       uiOutput("filterControls"),
                       shiny::actionButton("applyFilter", "Apply Filter", class = "btn-primary")
                ),
                
                # Active filters display
                column(8,
                       shinydashboard::box(
                         title = "Active Filters",
                         width = NULL,
                         status = "info",
                         uiOutput("activeFilters"),
                         shiny::actionButton("clearAllFilters", "Clear All Filters", class = "btn-warning")
                       )
                )
              ),
              
              hr(),
              
              
              fluidRow(
                column(12,
                       uiOutput("filterSummary")
                )
              ),
              
              fluidRow(
                column(12,
                       shinydashboard::box(
                         title = "Station Map",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,
                         leaflet::leafletOutput("filterMap", height = 400),
                         collapsible = TRUE
                       )
                )
              ),
              
              
              # # Filtered data table
              fluidRow(
                column(12,
                       # h4("Filtered Data"),
                       # DT::DTOutput("filtered_ct_table")
                       shinydashboard::box(
                         title = "Filtered Data",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,
                         DT::DTOutput("filtered_ct_table"),
                         collapsible = TRUE
                       )
                       
                )
              )
              
            )
          )
        ),
        
        ## Tab: Filter records ----
        
        shinydashboard::tabItem(
          tabName = "filterRecords",
          fluidRow(
            shinydashboard::box(
              title = "Temporal Filtering Settings", width = 12, status = "primary",
              numericInput("minDeltaTime", "Minimum time difference (minutes)", value = 0, min = 0),
              selectInput("deltaTimeComparedTo", "Compare delta time to:", 
                          choices = c("Last independent record" = "lastIndependentRecord", 
                                      "Last record" = "lastRecord")),
              uiOutput("camerasIndependentUI"),  # This will be conditionally rendered
              checkboxInput("removeDuplicateRecords", "Remove duplicate records", value = TRUE),
              shiny::actionButton("runTemporalFilter", "Apply Temporal Filter"),
              shiny::actionButton("restoreOriginalRecordTable", "Restore Original Record Table", class = "btn-warning")
              
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = "Filtered Record Table", width = 12, status = "info",
              DT::dataTableOutput("filteredRecordTable")
            )
          )
        ),
        
        ## Tab: Extract covariates  ----
        shinydashboard::tabItem(tabName = "extract",
                                shiny::tabsetPanel(
                                  selected = "Extract Covariates",
                                  shiny::tabPanel("Instructions",
                                                  fluidRow(
                                                    shinydashboard::box(
                                                      title = "How to Extract Covariates", 
                                                      width = 12, 
                                                      status = "info",
                                                      shiny::HTML(
                                                        "<h4>Instructions for Covariate Extraction</h4>
        <ol>
          <li><strong>Configure Global Settings</strong>
            <ul>
              <li><em>Camera Trap Covariate Settings:</em>
                <ul>
                  <li>Set buffer around camera traps for value extraction</li>
                  <li>Choose whether to use bilinear interpolation</li>
                  <li>Preview map shows camera locations and buffer</li>
                </ul>
              </li>
              <li><em>Prediction Raster Settings:</em>
                <ul>
                  <li>Choose prediction area: no clipping, camera trap grid, study area, or their intersection</li>
                  <li>Set buffer for prediction area (up to 500m for 'no clipping' option)</li>
                  <li>Upload raster template (optional) or specify resolution</li>
                  <li>Preview map shows selected extent with interactive basemap</li>
                </ul>
              </li>
            </ul>
          </li>
          
          <li><strong>Choose Data Source</strong>
            <ul>
              <li><em>Local Rasters:</em>
                <ul>
                  <li>Choose directory containing your covariate rasters, or</li>
                  <li>Specify individual raster files</li>
                  <li>Set file format (default: .tif)</li>
                  <li>Enable recursive search if rasters are in subdirectories</li>
                </ul>
              </li>
              <li><em>Elevation & Terrain:</em>
                <ul>
                  <li>Downloads elevation data from AWS Terrain Tiles</li>
                  <li>Choose zoom level for resolution (z10: ~80m, z11: ~40m, z12: ~20m)</li>
                  <li>Select which terrain indices to calculate (slope, aspect, TRI, TPI, roughness)</li>
                </ul>
              </li>
            </ul>
          </li>
          
          <li><strong>Processing Behavior</strong>
            <ul>
              <li>Values at camera locations are always extracted from original resolution data</li>
              <li>Prediction rasters are created based on:</li>
              <ol>
                <li>Provided template if available</li>
                <li>Otherwise, existing prediction rasters if available</li>
                <li>Otherwise, specified resolution if provided</li>
                <li>If no specifications given, local rasters will not create prediction rasters and elevation data will use source resolution</li>
              </ol>
              <li>Prediction rasters are masked to selected extent if clipping is requested</li>
            </ul>
          </li>
          
          <li><strong>Output</strong>
            <ul>
              <li>Covariate values are added to the camera trap table</li>
              <li>Original rasters are stored at their native resolution</li>
              <li>Prediction rasters are created according to specifications</li>
              <li>All covariate data can be cleared at once using the 'Clear All Covariates' button</li>
            </ul>
          </li>
        </ol>

        <h4>Notes:</h4>
        <ul>
          <li>Large rasters or numerous covariates may take some time to process</li>
          <li>When using elevation data, a minimum 5km buffer is applied for download to ensure proper terrain calculations</li>
          <li>The preview maps update in real-time to show the impact of your buffer and prediction area settings</li>
          <li>You can process local rasters and elevation data independently</li>
          <li>Clearing covariates removes all added columns from the camera trap table and clears both original and prediction rasters</li>
        </ul>"
                                                      )
                                                    )
                                                  )
                                  ),
                                  
                                  
                                  
                                  # Main extraction tab
                                  shiny::tabPanel("Extract Covariates",
                                                  # Settings boxes
                                                  fluidRow(
                                                    # Camera trap extraction settings
                                                    shinydashboard::box(
                                                      title = "Camera Trap Covariate Settings", 
                                                      status = "primary", 
                                                      solidHeader = TRUE,
                                                      width = 6,
                                                      
                                                      fluidRow(
                                                        column(6,
                                                               numericInput("bufferCT", "Buffer around camera traps (meters)", 
                                                                            value = 0, min = 0),
                                                               checkboxInput("bilinear", "Use bilinear interpolation", 
                                                                             value = FALSE)
                                                        ),
                                                        column(6,
                                                               leaflet::leafletOutput("ctBufferPreview", height = "300px")
                                                        )
                                                      )
                                                    ),
                                                    
                                                    # Prediction raster settings
                                                    shinydashboard::box(
                                                      title = "Prediction Raster Settings", 
                                                      status = "primary", 
                                                      solidHeader = TRUE,
                                                      width = 6,
                                                      
                                                      fluidRow(
                                                        column(6,
                                                               radioButtons("predictionExtent", "Prediction Area:",
                                                                            choices = c(
                                                                              "No clipping" = "none",
                                                                              "Camera trap grid" = "grid",
                                                                              "Study area" = "study_area",
                                                                              "Intersection of both" = "intersection"
                                                                            ),
                                                                            selected = "grid"),
                                                               
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.predictionExtent !== 'none'",
                                                                 numericInput("bufferPrediction", "Buffer prediction area (meters)", 
                                                                              value = 1000, min = 0)
                                                               ),
                                                               
                                                               fileInput("rasterTemplate", "Raster Template (optional)", 
                                                                         accept = c(".tif")),
                                                               
                                                               conditionalPanel(
                                                                 condition = "!input.rasterTemplate",
                                                                 numericInput("resolution", "Resolution (meters)", 
                                                                              value = NULL, min = 0, step = 1)
                                                               )
                                                        ),
                                                        column(6,
                                                               leaflet::leafletOutput("predictionExtentPreview", height = "300px")
                                                        )
                                                      )
                                                    )
                                                  ),
                                                  
                                                  # Data sources box
                                                  fluidRow(
                                                    shinydashboard::box(
                                                      title = "Data Sources",
                                                      status = "info",
                                                      solidHeader = TRUE,
                                                      width = 12,
                                                      
                                                      tabsetPanel(
                                                        # Local Rasters tab
                                                        tabPanel("Local Rasters",
                                                                 fluidRow(
                                                                   column(6,
                                                                          radioButtons("inputType", "Choose input type:",
                                                                                       choices = c("Directory" = "directory", 
                                                                                                   "Filenames" = "filenames")),
                                                                          
                                                                          conditionalPanel(
                                                                            condition = "input.inputType == 'directory'",
                                                                            textInput("directory", "Directory", 
                                                                                      placeholder = "Path to covariate rasters"),
                                                                            checkboxInput("recursive", "Search recursively", 
                                                                                          value = FALSE)
                                                                          ),
                                                                          
                                                                          conditionalPanel(
                                                                            condition = "input.inputType == 'filenames'",
                                                                            textInput("filenames", "Filenames", 
                                                                                      placeholder = "Comma-separated file paths")
                                                                          ),
                                                                          
                                                                          textInput("formats", "Formats", value = ".tif")
                                                                   )
                                                                 )
                                                        ),
                                                        
                                                        # Elevation & Terrain tab
                                                        tabPanel("Elevation & Terrain",
                                                                 div(
                                                                   class = "alert alert-info",
                                                                   "Elevation data from AWS Terrain Tiles"
                                                                 ),
                                                                 
                                                                 fluidRow(
                                                                   column(6,
                                                                          checkboxGroupInput("terrainMeasures", 
                                                                                             "Select Terrain Indices:",
                                                                                             choices = c(
                                                                                               "Slope" = "slope", 
                                                                                               "Aspect" = "aspect", 
                                                                                               "TRI (Terrain Ruggedness Index)" = "TRI", 
                                                                                               "TPI (Topographic Position Index)" = "TPI", 
                                                                                               "Roughness" = "roughness"
                                                                                             ),
                                                                                             selected = c("slope", "TRI"))
                                                                   ),
                                                                   column(6,
                                                                          radioButtons("elevationZoom", "Resolution:", 
                                                                                       choices = c(
                                                                                         "Zoom 12 (~20m)" = 12,
                                                                                         "Zoom 11 (~40m)" = 11,
                                                                                         "Zoom 10 (~80m)" = 10
                                                                                       ),
                                                                                       selected = 11) #,
                                                                          
                                                                   )
                                                                 )
                                                        )
                                                      )
                                                    )
                                                  ),
                                                  
                                                  # Action buttons
                                                  fluidRow(
                                                    column(12,
                                                           div(
                                                             style = "margin: 20px 0;",
                                                             shiny::actionButton("processLocalRasters", 
                                                                                 "Process Local Rasters", 
                                                                                 class = "btn-primary btn-lg"),
                                                             shiny::actionButton("processElevation", 
                                                                                 "Process Elevation Data", 
                                                                                 class = "btn-primary btn-lg"),
                                                             shiny::actionButton("clearAllCovariates", 
                                                                                 "Clear All Covariates", 
                                                                                 class = "btn-danger btn-lg pull-right")
                                                           )
                                                    )
                                                  )
                                  ),
                                  
                                  
                                  shiny::tabPanel("Updated CT Table",
                                                  DT::DTOutput("updatedCTTable")
                                  ),
                                  # Original Covariate Rasters tab
                                  shiny::tabPanel("Original Covariate Rasters",
                                                  fluidRow(
                                                    column(3,
                                                           selectInput("rasterBand", "Select Raster Band", choices = NULL)
                                                    ),
                                                    column(3,
                                                           selectInput("colorPalette", "Color Palette",
                                                                       choices = c("Terrain", "Viridis", "Plasma", "Inferno", "Rocket"),
                                                                       selected = "Inferno")
                                                    ),
                                                    column(3,
                                                           checkboxInput("invertColors", "Invert Color Ramp", value = FALSE)
                                                    ),
                                                    column(3,
                                                           selectInput("ctColorBy", "Color Camera Traps By",
                                                                       choices = c("White" = "white", "Raster Value" = "raster"))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    column(4,
                                                           sliderInput("ctPointSize", "Camera Trap Point Size", 
                                                                       min = 1, max = 20, value = 6, step = 1)
                                                    ),
                                                    column(4,
                                                           sliderInput("maxPixels", "Maximum number of pixels to plot (millions)", 
                                                                       value = 1, min = 1, max = 100, step = 1, ticks = FALSE)
                                                    )
                                                  ),
                                                  leaflet::leafletOutput("originalCovariatePlot", height = "600px")
                                  ),
                                  
                                  # Prediction Rasters tab
                                  shiny::tabPanel("Prediction Rasters",
                                                  fluidRow(
                                                    column(3,
                                                           selectInput("predictionRasterBand", "Select Prediction Raster Band", 
                                                                       choices = NULL)
                                                    ),
                                                    column(3,
                                                           selectInput("colorPalettePrediction", "Color Palette",
                                                                       choices = c("Terrain", "Viridis", "Plasma", "Inferno", "Rocket"),
                                                                       selected = "Inferno")
                                                    ),
                                                    column(3,
                                                           checkboxInput("invertColorsPrediction", "Invert Color Ramp", 
                                                                         value = FALSE)
                                                    ),
                                                    column(3,
                                                           selectInput("ctColorByPrediction", "Color Camera Traps By",
                                                                       choices = c("White" = "white", "Raster Value" = "raster"))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    column(4,
                                                           sliderInput("ctPointSizePrediction", "Camera Trap Point Size", 
                                                                       min = 1, max = 20, value = 6, step = 1)
                                                    ),
                                                    column(4,
                                                           sliderInput("maxPixelsPrediction", 
                                                                       "Maximum number of pixels to plot (millions)", 
                                                                       value = 1, min = 1, max = 100, step = 1, ticks = FALSE)
                                                           
                                                    )
                                                  ),
                                                  leaflet::leafletOutput("predictionRasterPlot", height = "600px")
                                  )
                                )
        ),
        
        
        ## Tab: Covariate correlation ----
        
        
        
        
        shinydashboard::tabItem(
          tabName = "covariateCorrelation",
          shiny::tabsetPanel(
            selected = "Correlation Analysis",
            shiny::tabPanel("Instructions",
                            fluidRow(
                              shinydashboard::box(
                                title = "How to Use Covariate Correlation Analysis", 
                                width = 12, 
                                status = "info",
                                shiny::HTML(
                                  "<h4>Instructions for Covariate Correlation Analysis</h4>
            <ol>
              <li><strong>Basic Settings</strong>
                <ul>
                  <li><em>Correlation Method:</em>
                    <ul>
                      <li>Pearson: For linear relationships between continuous variables</li>
                      <li>Spearman: For monotonic relationships, robust to outliers</li>
                      <li>Kendall: For ordinal relationships, good for small sample sizes</li>
                    </ul>
                  </li>
                  <li><em>Correlation Threshold:</em>
                    <ul>
                      <li>Set minimum correlation value to highlight (0-1)</li>
                      <li>Default 0.7 is commonly used</li>
                      <li>Lower values (e.g., 0.5) for stricter multicollinearity control</li>
                      <li>Higher values (e.g., 0.8) for more relaxed variable inclusion</li>
                    </ul>
                  </li>
                  <li><em>Non-numeric Variables:</em>
                    <ul>
                      <li>Option to exclude non-numeric covariates</li>
                      <li>Categorical variables are automatically excluded if enabled</li>
                    </ul>
                  </li>
                </ul>
              </li>
              
              <li><strong>Visualization Options</strong>
                <ul>
                  <li><em>Plot Type:</em>
                    <ul>
                      <li>Correlation matrix: Compact view of all correlations</li>
                      <li>Scatter plot matrix: Detailed view of relationships</li>
                    </ul>
                  </li>
                  <li><em>Display Method:</em>
                    <ul>
                      <li>Color: Simple color-coded squares</li>
                      <li>Circle/Square: Size indicates correlation strength</li>
                      <li>Ellipse: Shape shows correlation direction and strength</li>
                      <li>Shade: Uses color intensity</li>
                      <li>Pie: Fills circles with pie charts</li>
                    </ul>
                  </li>
                  <li><em>Ordering Method:</em>
                    <ul>
                      <li>Original: Keep input order</li>
                      <li>AOE/FPC: Order by eigenvectors or first principal component</li>
                      <li>Hclust: Group similar variables</li>
                      <li>Alphabet: Sort alphabetically</li>
                    </ul>
                  </li>
                </ul>
              </li>

              <li><strong>Output Components</strong>
                <ul>
                  <li><em>Correlation Plot:</em>
                    <ul>
                      <li>Blue indicates positive correlations</li>
                      <li>Red indicates negative correlations</li>
                      <li>Color intensity shows correlation strength</li>
                      <li>Values display exact correlation coefficients</li>
                    </ul>
                  </li>
                  <li><em>Highly Correlated Pairs:</em>
                    <ul>
                      <li>Table shows pairs exceeding threshold</li>
                      <li>Lists exact correlation values</li>
                      <li>Helps identify potential multicollinearity issues</li>
                    </ul>
                  </li>
                </ul>
              </li>
            </ol>

            <h4>Notes:</h4>
            <ul>
              <li>Large correlations suggest redundant information between variables</li>
              <li>Consider removing one variable from highly correlated pairs before modeling</li>
              <li>Choice of which variable to remove should be based on ecological relevance and data quality</li>
              <li>Remember that correlation does not imply causation</li>
              <li>Missing values are handled pairwise in correlation calculations</li>
            </ul>"
                                )
                              )
                            )
            ),
            
            shiny::tabPanel("Correlation Analysis",
                            fluidRow(
                              shinydashboard::box(
                                title = "Settings", width = 12, status = "primary",
                                fluidRow(
                                  # Most important settings first
                                  column(4,
                                         selectInput("plotType", 
                                                     "Plot Type:",
                                                     choices = c("Correlation matrix" = "matrix",
                                                                 "Scatter plot matrix" = "pairs"),
                                                     selected = "matrix")
                                  ),
                                  column(4,
                                         selectInput("correlationMethod", 
                                                     "Correlation Method:",
                                                     choices = c("pearson", "spearman", "kendall"),
                                                     selected = "pearson")
                                  ),
                                  column(4,
                                         checkboxInput("excludeNonNumeric", 
                                                       "Exclude non-numeric covariates", 
                                                       value = TRUE)
                                  )
                                ),
                                # Visualization settings in a second row when matrix view is selected
                                conditionalPanel(
                                  condition = "input.plotType == 'matrix'",
                                  fluidRow(
                                    column(4,
                                           selectInput("corrplotMethod", 
                                                       "Display Style:",
                                                       choices = c("Color Squares" = "color", 
                                                                   "Circles" = "circle", 
                                                                   "Filled Squares" = "square", 
                                                                   "Ellipses" = "ellipse", 
                                                                   "Shaded Squares" = "shade", 
                                                                   "Pie Charts" = "pie"),
                                                       selected = "color")
                                    ),
                                    column(4,
                                           selectInput("corrplotOrder",
                                                       "Variable Ordering:",
                                                       choices = c("Original Order" = "original",
                                                                   "Cluster by Similarity" = "hclust",
                                                                   "Order by First Principal Component" = "FPC",
                                                                   "Angular Order of Eigenvectors" = "AOE",
                                                                   "Alphabetical" = "alphabet"),
                                                       selected = "hclust")
                                    )
                                  )
                                )
                              )
                            ),
                            fluidRow(
                              # Main correlation plot
                              shinydashboard::box(
                                title = "Correlation Plot", 
                                width = 8, 
                                status = "info",
                                plotOutput("correlationPlot", height = "700px")
                              ),
                              # Highly correlated pairs with threshold control
                              shinydashboard::box(
                                title = "Highly Correlated Pairs", 
                                width = 4, 
                                status = "warning",
                                fluidRow(
                                  column(12,
                                         sliderInput("correlationThreshold", 
                                                     "Show pairs with |correlation| >",
                                                     min = 0, max = 1, value = 0.7, step = 0.05)
                                  )
                                ),
                                DT::dataTableOutput("correlationTable"),
                                tags$div(
                                  style = "margin-top: 20px;",
                                  textOutput("correlationWarning")
                                )
                              )
                            )
            )
            
          )
        ),
        
        ## Tab: Camera operation  ----
        shinydashboard::tabItem(
          tabName = "CameraOperation",
          plotly::plotlyOutput("camop",
                               height = "600px")
        ),
        
        ## Tab: Detection history  ----
        shinydashboard::tabItem(
          tabName = "DetectionHistory",
          shiny::tabsetPanel(
            selected = "Detection History",
            shiny::tabPanel("Instructions",
                            fluidRow(
                              shinydashboard::box(
                                title = "How to Create Detection Histories", 
                                width = 12, 
                                status = "info",
                                shiny::HTML(
                                  "<h4>Instructions for Creating and Using Detection Histories</h4>
            <ol>
              <li><strong>Basic Settings</strong>
                <ul>
                  <li><em>Species Selection:</em>
                    <ul>
                      <li>Choose target species from dropdown menu</li>
                      <li>Only species present in record table are shown</li>
                    </ul>
                  </li>
                  <li><em>Occasion Length:</em>
                    <ul>
                      <li>Define the length of sampling occasions in days</li>
                      <li>Maximum length depends on total survey duration</li>
                      <li>Consider species behavior and survey design when choosing length</li>
                    </ul>
                  </li>
                  <li><em>Output Type:</em>
                    <ul>
                      <li>Binary (0/1): Records detection/non-detection in each occasion</li>
                      <li>Count: Records number of detections in each occasion</li>
                      <li>Binary typically used for occupancy models</li>
                    </ul>
                  </li>
                  <li><em>Day 1 Definition:</em>
                    <ul>
                      <li>Survey: Start all stations from first day of overall survey</li>
                      <li>Station: Start each station from its own setup date</li>
                      <li>Affects how occasions are aligned across stations</li>
                    </ul>
                  </li>
                </ul>
              </li>

              <li><strong>Output Components</strong>
                <ul>
                  <li><em>Summary Statistics:</em>
                    <ul>
                      <li>Total number of records for selected species</li>
                      <li>Number of detections in detection history</li>
                      <li>Number of stations with detections</li>
                      <li>Percentage of occasions with detections</li>
                    </ul>
                  </li>
                  <li><em>Detection History Plot:</em>
                    <ul>
                      <li>Left panel: Detection matrix (0/1 or counts)</li>
                      <li>Right panel: Camera operation/effort matrix</li>
                      <li>Rows represent stations</li>
                      <li>Columns represent sampling occasions</li>
                      <li>Color intensity indicates detection/effort values</li>
                    </ul>
                  </li>
                  <li><em>unmarkedFrame Summary:</em>
                    <ul>
                      <li>Shows structure of data formatted for unmarked package</li>
                      <li>Includes number of sites and occasions</li>
                      <li>Lists available covariates</li>
                      <li>Displays observation model type</li>
                    </ul>
                  </li>
                </ul>
              </li>

              <li><strong>Export Options</strong>
                <ul>
                  <li><em>Detection History:</em>
                    <ul>
                      <li>Export raw detection history matrix</li>
                      <li>Includes both detection and effort matrices</li>
                      <li>Useful for custom analyses or verification</li>
                    </ul>
                  </li>
                  <li><em>unmarkedFrame:</em>
                    <ul>
                      <li>Export formatted data ready for occupancy modeling</li>
                      <li>Includes all necessary components for analysis: detection history, site covariates, observation covariate 'effort'</li>
                      <li>Can be used directly with unmarked or ubms package functions</li>
                    </ul>
                  </li>
                </ul>
              </li>
            </ol>

            <h4>Notes:</h4>
            <ul>
              <li>Detection histories form the basis for occupancy modeling</li>
              <li>Missing values (NA) occur when cameras were not operational</li>
              <li>Ensure sufficient detections for reliable model estimation</li>
              <li>Check camera operation matrix for adequate survey effort</li>
              <li>Binary format appropriate for most single-season occupancy models</li>
            </ul>"
                                )
                              )
                            )
            ),
            
            shiny::tabPanel("Detection History",
                            shiny::fluidRow(
                              shiny::column(
                                3,
                                shiny::selectInput(
                                  inputId = "species_dethist",
                                  label = "Species",
                                  choices = NULL,  # is updated dynamically
                                  selected = NULL
                                )
                              ),
                              shiny::column(
                                3,
                                shiny::sliderInput("occasionLength_single_species", "Occasion Length (in days)",
                                                   min = 1, max = 20, value = 10,
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
                            shinydashboard::box(
                              width = 12,
                              title = "Detection History Plot",
                              collapsible = TRUE,
                              collapsed = FALSE,
                              status = "primary",
                              solidHeader = TRUE,
                              plotly::plotlyOutput("detectionHistory")
                            ),
                            shinydashboard::box(
                              width = 12,
                              title = "unmarkedFrame Summary",
                              status = "info",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              collapsed = TRUE,
                              shiny::verbatimTextOutput("umf_summary")
                            ),
                            shiny::actionButton("return_dethist", "Return detection history"),
                            shiny::actionButton("return_umf", "Return unmarkedFrame")
            )
          )
        ),
        
        
        
        ## Tab: Ocupancy (single species) ----
        
        # UI modifications first - replace existing "Occupancy" tabItem with:
        
        shinydashboard::tabItem(
          tabName = "Occupancy",
          fluidRow(
            column(12,
                   shinydashboard::box(
                     width = NULL,
                     status = "primary",
                     title = "Workflow Selection",
                     radioButtons("occupancy_workflow", "Choose workflow:",
                                  choices = c(
                                    "Basic - Simple model specification with standard options" = "basic",
                                    "Advanced - Full control over model structure and effects" = "advanced"
                                  ),
                                  selected = "basic"
                     ),
                     helpText("Basic workflow is recommended for most analyses. Advanced workflow provides more control but requires deeper understanding of model structure.")
                   )
            )
          ),
          
          # Basic Workflow UI
          conditionalPanel(
            condition = "input.occupancy_workflow == 'basic'",
            tabsetPanel(
              id = "basic_workflow_tabs",
              
              # Model Setup tab
              tabPanel("Model Setup",
                       fluidRow(
                         # Left Column - All Inputs
                         column(width = 3,
                                wellPanel(
                                  h4("Model Configuration", class = "text-primary"),
                                  selectInput("basic_model_package", "Package:",
                                              choices = c("unmarked", "ubms"),
                                              selected = "unmarked"),
                                  selectInput("basic_model_type", "Model type:",
                                              choices = c("Occupancy", "Royle-Nichols"),
                                              selected = "Occupancy"),
                                  checkboxInput("basic_scale_covariates", "Scale covariates", value = FALSE),
                                  hr(),
                                  
                                  h4("Covariates", class = "text-primary"),
                                  varSelectizeInput("basic_det_covs", "Detection covariates",
                                                    data = NULL, multiple = TRUE,
                                                    options = list(selectize = TRUE)),
                                  checkboxInput("basic_effort_on_detection", "Include effort on detection", 
                                                value = FALSE),
                                  varSelectizeInput("basic_occ_covs", "Occupancy covariates",
                                                    data = NULL, multiple = TRUE,
                                                    options = list(selectize = TRUE)),
                                  
                                  # UBMS Settings
                                  conditionalPanel(
                                    condition = "input.basic_model_package == 'ubms'",
                                    hr(),
                                    h4("MCMC Settings", class = "text-primary"),
                                    numericInput("basic_ubms_chains", "Number of chains:", 
                                                 value = 3, min = 1),
                                    numericInput("basic_ubms_iter", "Number of iterations:", 
                                                 value = 2000, min = 100),
                                    numericInput("basic_ubms_thin", "Thinning:", 
                                                 value = 1, min = 1),
                                    uiOutput("basic_ubms_cores_input")
                                  ),
                                  
                                  hr(),
                                  # Run Model and Add to Model Selection Buttons
                                  div(style = "text-align: center; margin-top: 20px;",
                                      actionButton("basic_run_model", "Run Model", 
                                                   class = "btn-primary btn-lg btn-block"),
                                      div(style = "margin-top: 10px;",
                                          actionButton("basic_add_to_modsel", "Add to Model Selection", 
                                                       class = "btn-success btn-block")
                                      )
                                  )
                                )
                         ),
                         
                         # Right Column - Results
                         column(width = 9,
                                fluidRow(
                                  # Main model summary on left
                                  column(width = 7,
                                         shinydashboard::box(
                                           title = "Model Summary",
                                           status = "primary",
                                           width = NULL,
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           height = "500px",
                                           verbatimTextOutput("basic_model_summary")
                                         )
                                  ),
                                  
                                  # Parameter estimates on right
                                  column(width = 5,
                                         shinydashboard::box(
                                           title = "Parameter Estimates", 
                                           status = "primary",
                                           width = NULL,
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           
                                           # Confidence interval settings side by side
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("basic_pval", "Interval width:", 
                                                                 min = 0, max = 1, value = 0.95, step = 0.01)
                                             ),
                                             column(width = 6,
                                                    numericInput("basic_digits", "Decimal places:", 
                                                                 value = 2, min = 0, max = 5, step = 1)
                                             )
                                           ),
                                           hr(),
                                           textOutput("basic_coef_det_header", inline = TRUE),
                                           verbatimTextOutput("basic_coef_det"),
                                           textOutput("basic_coef_state_header", inline = TRUE),
                                           verbatimTextOutput("basic_coef_state")
                                         )
                                  )
                                ),
                                
                                # Model Selection below
                                fluidRow(
                                  column(width = 12,
                                         shinydashboard::box(
                                           title = "Model Selection",
                                           status = "info",
                                           width = NULL,
                                           solidHeader = TRUE,
                                           tableOutput("basic_model_selection")
                                         )
                                  )
                                )
                         )
                       )
              ),
              
              # Basic Results & Diagnostics tab
              tabPanel("Model selection",

                       fluidRow(
                         shinydashboard::box(
                           title = "Model Selection",
                           status = "info",
                           width = 12,
                           tableOutput("basic_model_selection"),
                           shiny::fluidRow(
                             shiny::column(
                               width = 12, 
                               shiny::actionButton("basic_clear_modsel", "Clear model selection", class = "btn-warning")
                             )
                           )
                         )
                       )
              ),
              
              # Basic Response Plots tab
              tabPanel("Response Plots",
                       fluidRow(
                         column(
                           width = 3,
                           selectInput("basic_plot_type", "Plot type:",
                                       choices = c("Detection covariates", "Occupancy covariates")
                           ),
                           numericInput("basic_ci_level", "Confidence level:", 
                                        value = 0.95, min = 0, max = 1, step = 0.01
                           )
                         ),
                         column(
                           width = 9,
                           plotOutput("basic_response_plot", height = "600px")
                         )
                       )
              ),
              
              # Basic Spatial Predictions tab
              tabPanel("Spatial Predictions",
                       fluidRow(
                         column(
                           width = 3,
                           selectInput("basic_pred_type", "Prediction type:",
                                       choices = c(
                                         "Occupancy probability" = "state",
                                         "Detection probability" = "det"
                                       )
                           ),
                           selectInput("basic_pred_source", "Covariate source:",
                                       choices = c(
                                         "Use extracted covariates" = "extracted",
                                         "Upload custom raster" = "custom"
                                       )
                           ),
                           conditionalPanel(
                             condition = "input.basic_pred_source == 'custom'",
                             fileInput("basic_custom_raster", "Upload raster:", accept = c(".tif"))
                           ),
                           actionButton("basic_run_prediction", "Generate Predictions", class = "btn-primary"),
                           
                           # shiny::selectInput("basic_prediction_layer", "Select prediction layer:",
                           #                    choices = c("Occupancy probability" = "Predicted",
                           #                                "Standard error" = "SE",
                           #                                "Lower CI" = "lower",
                           #                                "Upper CI" = "upper"),
                           #                    selected = "Predicted"),
                           uiOutput("basic_prediction_layer_choices"),
                           
                           shiny::selectInput("basic_predictionColorPalette", "Color Palette:",
                                              choices = c("Viridis", "Plasma", "Inferno",  "Rocket"),
                                              selected = "Inferno"),
                           
                           shiny::checkboxInput("basic_invertPredictionColors", "Invert Color Ramp", value = FALSE),
                           
                           # shiny::checkboxInput("basic_raster01", "Clamp raster values to min = 0 / max = 1", value = FALSE)
                         ),
                         column(
                           width = 9,
                           leaflet::leafletOutput("basic_prediction_map", height = "600px")
                         )
                       )
              )
            )
          ),
          
          # Advanced Workflow UI
          conditionalPanel(
            condition = "input.occupancy_workflow == 'advanced'",
            tabsetPanel(
              id = "advanced_workflow_tabs",
              
              # Advanced Model Setup tab 
              tabPanel("Model Setup",
                       fluidRow(
                         shinydashboard::box(
                           title = "Model Settings",
                           status = "primary",
                           width = 12,
                           fluidRow(
                             column(3,
                                    selectInput("adv_model_package", "Package:",
                                                choices = c("unmarked", "ubms"),
                                                selected = "unmarked"
                                    )
                             ),
                             column(3,
                                    selectInput("adv_model_type", "Model Type:",
                                                choices = c("Occupancy", "Royle-Nichols"),
                                                selected = "Occupancy"
                                    )
                             ),
                             column(3,
                                    checkboxInput("adv_effort_on_detection", "Effort on detection", value = FALSE)
                             ),
                             column(3,
                                    checkboxInput("adv_scale_covariates", "Scale covariates", value = FALSE)
                             )
                           )
                         )
                       ),
                       
                       fluidRow(
                         # Detection effects
                         column(6,
                                shinydashboard::box(
                                  title = "Detection Covariates",
                                  status = "primary",
                                  width = NULL,
                                  uiOutput("detectionCovariatesUI"),
                                  actionButton("addDetectionEffect", "Add Effect", class = "btn-primary"),
                                  conditionalPanel(
                                    condition = "input.addDetectionEffect % 2 == 1",
                                    selectInput("detectionEffectType", "Effect Type:",
                                                choices = c(
                                                  "Linear" = "linear",
                                                  "Quadratic" = "quadratic",
                                                  "Interaction" = "interaction",
                                                  "Random" = "random"
                                                )
                                    ),
                                    conditionalPanel(
                                      condition = "input.detectionEffectType == 'interaction' || input.detectionEffectType == 'random'",
                                      selectInput("detectionSecondCovariate", "Select second covariate:", choices = NULL)
                                    ),
                                    actionButton("confirmDetectionEffect", "Add", class = "btn-success")
                                  ),
                                  hr(),
                                  uiOutput("selectedDetectionEffectsUI")
                                )
                         ),
                         
                         # Occupancy effects
                         column(6,
                                shinydashboard::box(
                                  title = "Occupancy Covariates",
                                  status = "primary",
                                  width = NULL,
                                  uiOutput("occupancyCovariatesUI"),
                                  actionButton("addOccupancyEffect", "Add Effect", class = "btn-primary"),
                                  conditionalPanel(
                                    condition = "input.addOccupancyEffect % 2 == 1",
                                    selectInput("occupancyEffectType", "Effect Type:",
                                                choices = c(
                                                  "Linear" = "linear",
                                                  "Quadratic" = "quadratic",
                                                  "Interaction" = "interaction",
                                                  "Random" = "random"
                                                )
                                    ),
                                    conditionalPanel(
                                      condition = "input.occupancyEffectType == 'interaction' || input.occupancyEffectType == 'random'",
                                      selectInput("occupancySecondCovariate", "Select second covariate:", choices = NULL)
                                    ),
                                    actionButton("confirmOccupancyEffect", "Add", class = "btn-success")
                                  ),
                                  hr(),
                                  uiOutput("selectedOccupancyEffectsUI")
                                )
                         )
                       ),
                       
                       # Formula preview
                       fluidRow(
                         shinydashboard::box(
                           title = "Formula Preview",
                           status = "info",
                           width = 12,
                           verbatimTextOutput("formulaPreview")
                         )
                       ),
                       
                       # ubms settings
                       conditionalPanel(
                         condition = "input.adv_model_package == 'ubms'",
                         fluidRow(
                           shinydashboard::box(
                             title = "UBMS Settings",
                             status = "primary",
                             width = 12,
                             fluidRow(
                               column(3,
                                      numericInput("adv_ubms_chains", "Number of chains:", value = 3, min = 1)
                               ),
                               column(3,
                                      numericInput("adv_ubms_iter", "Number of iterations:", value = 2000, min = 100)
                               ),
                               column(3,
                                      numericInput("adv_ubms_thin", "Thinning:", value = 1, min = 1)
                               ),
                               column(3,
                                      uiOutput("adv_ubms_cores_input")
                               )
                             )
                           )
                         )
                       ),
                       
                       # Run model button
                       fluidRow(
                         column(12,
                                shinydashboard::box(
                                  width = NULL,
                                  actionButton("runAdvancedModel", "Run Model", class = "btn-primary btn-lg btn-block")
                                )
                         )
                       )
              ),
              
              # Advanced Results & Diagnostics tab
              tabPanel("Results & Diagnostics",
                       fluidRow(
                         shinydashboard::box(
                           title = "Model Summary",
                           status = "primary",
                           width = 6,
                           verbatimTextOutput("adv_model_summary")
                         ),
                         shinydashboard::box(
                           title = "Confidence intervals of model coefficients", 
                           status = "primary",
                           width = 6,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           shiny::fluidRow(
                             shiny::column(width = 6, shiny::numericInput("adv_pval", "Interval width:", 
                                                                          min = 0, max = 1, value = 0.95, step = 0.01)),
                             shiny::column(width = 6, shiny::numericInput("adv_digits", "Digits:", 
                                                                          value = 2, min = 0, max = 5, step = 1))
                           ),
                           shiny::textOutput("adv_coef_det_header", inline = TRUE),
                           shiny::verbatimTextOutput("adv_coef_det"),
                           shiny::textOutput("adv_coef_state_header", inline = TRUE),
                           shiny::verbatimTextOutput("adv_coef_state")
                         )
                       ),
                       fluidRow(
                         shinydashboard::box(
                           title = "Model Selection",
                           status = "info",
                           width = 12,
                           actionButton("adv_add_to_modsel", "Add to Model Selection", class = "btn-success"),
                           tableOutput("adv_model_selection")
                         )
                       ),
                       fluidRow(
                         shinydashboard::box(
                           title = "MCMC Diagnostics",
                           status = "warning",
                           width = 12,
                           conditionalPanel(
                             condition = "input.adv_model_package == 'ubms'",
                             selectInput("adv_parameter_trace", "Select parameter:",
                                         choices = NULL
                             ),
                             plotOutput("adv_trace_plot")
                           )
                         )
                       )
              ),
              
              # Advanced Response Plots tab
              tabPanel("Response Plots",
                       fluidRow(
                         column(
                           width = 3,
                           selectInput("adv_plot_submodel", "Submodel:",
                                       choices = c("Detection", "Occupancy")
                           ),
                           selectInput("adv_plot_effect", "Effect:",
                                       choices = NULL
                           ),
                           numericInput("adv_ci_level", "Confidence level:",
                                        value = 0.95, min = 0, max = 1, step = 0.01
                           ),
                           checkboxInput("adv_show_data", "Show data points", value = TRUE)
                         ),
                         column(
                           width = 9,
                           plotOutput("adv_response_plot", height = "600px")
                         )
                       )
              ),
              
              # Advanced Spatial Predictions tab
              tabPanel("Spatial Predictions",
                       fluidRow(
                         column(
                           width = 3,
                           selectInput("adv_pred_type", "Prediction type:",
                                       choices = c(
                                         "Occupancy probability" = "state",
                                         "Detection probability" = "det"
                                       )
                           ),
                           selectInput("adv_pred_source", "Covariate source:",
                                       choices = c(
                                         "Use extracted covariates" = "extracted",
                                         "Upload custom raster" = "custom"
                                       )
                           ),
                           conditionalPanel(
                             condition = "input.adv_pred_source == 'custom'",
                             fileInput("adv_custom_raster", "Upload raster:", accept = c(".tif"))
                           ),
                           # MCMC settings for UBMS predictions
                           conditionalPanel(
                             condition = "input.adv_model_package == 'ubms'",
                             numericInput("adv_pred_draws", "Number of draws:", value = 1000, min = 100),
                             numericInput("adv_pred_chains", "Number of chains:", value = 3, min = 1)
                           ),
                           actionButton("adv_run_prediction", "Generate Predictions", class = "btn-primary")
                         ),
                         column(
                           width = 9,
                           leaflet::leafletOutput("adv_prediction_map", height = "600px")
                         )
                       )
              )
            )
          )
        ),
        
        ## Tab: Community Occupancy ----
        shinydashboard::tabItem(
          tabName = "CommunityOccupancy",
          tabsetPanel(
            
            # Add this as the first tab in Community Occupancy Models tabsetPanel
            tabPanel("Instructions",
                     fluidRow(
                       shinydashboard::box(
                         title = "Community Occupancy Model Workflow", 
                         width = 12, 
                         status = "info",
                         shiny::HTML(
                           "<h4>Step-by-Step Guide to Community Occupancy Modeling</h4>
        <ol>
          <li><strong>Species Selection</strong>
            <ul>
              <li>First, select which species to include in your community model</li>
              <li>Review the species table showing detections and number of sites for each species</li>
              <li>Use the filtering options to select species based on minimum number of detections or sites</li>
              <li>You can manually select/deselect individual species by clicking the rows</li>
              <li>Consider removing rare species with too few detections for reliable parameter estimation</li>
            </ul>
          </li>

          <li><strong>Model Configuration</strong>
            <ul>
              <li>Choose model type (Occupancy or Royle-Nichols model)</li>
              <li>Set occasion length (in days) to create detection histories</li>
              <li>Configure detection and occupancy covariates:
                <ul>
                  <li><em>Fixed Effects:</em> Effect is same for all species</li>
                  <li><em>Species Random Effects:</em> Species-specific effects with shared variance</li>
                  <li><em>Independent Effects:</em> Completely independent effects for each species</li>
                </ul>
              </li>
              <li>Configure how species intercepts are modeled (fixed, random, or independent)</li>
              <li>Optional & recommended: Include survey effort as detection covariate</li>
              <li>Optional: Add species-site random effects</li>
              
            </ul>
          </li>

          <li><strong>Model Fitting</strong>
            <ul>
              <li>Set MCMC parameters (chains, iterations, burn-in, thinning)</li>
              <li>Run model</li>
              <li>Monitor progress (currently R console output, not in the dashboard)</li>
            </ul>
          </li>

          <li><strong>Results Inspection</strong>
            <ul>
              <li>Review parameter estimates and their uncertainties</li>
              <li>Check convergence diagnostics (Gelman-Rubin statistics)</li>
              <li>Examine trace plots for key parameters</li>
              <li>Optional: Run Goodness-of-Fit tests (not yet implemented)</li>
            </ul>
          </li>

          <li><strong>Effect Visualization</strong>
            <ul>
              <li>View response curves for covariates</li>
              <li>Compare effect sizes across species</li>
              <li>You can:
                <ul>
                  <li>Select subsets of species to display</li>
                  <li>Order effects by size</li>
                  <li>Adjust confidence intervals</li>
                  <li>Scale plot sizes for better visibility</li>
                </ul>
              </li>
            </ul>
          </li>

          <li><strong>Spatial Predictions</strong>
            <ul>
              <li>Generate species-specific occupancy/abundance maps</li>
              <li>Create species richness predictions</li>
              <li>Calculate percentage of area occupied</li>
              <li>Maps include uncertainty estimates</li>
            </ul>
          </li>
        </ol>

        <h4>Important Notes:</h4>
        <ul>
          <li>More species and covariates increase computation time</li>
          <li>Check convergence diagnostics before interpreting results</li>
          <li>Covariates are scaled to mean = 0 and standard deviation = 1 automatically.</li>
          <li>Prediction rasters shuold be provided in original scale. They are scaled automatically to match model covariatesy</li>
          <li>Species with very few detections may have unreliable estimates</li>
          <li>Save your model objects to avoid rerunning long computations</li>
        </ul>

        <h4>Tips for Model Convergence:</h4>
        <ul>
          <li>Start with simpler models and gradually add complexity</li>
          <li>Covariates are standardized automatically</li>
          <li>Increase iterations if chains haven't converged</li>
          <li>Check for highly correlated covariates</li>
          <li>Consider removing rare species or combining similar species</li>
        </ul>"
                         )
                       )
                     )
            ),
            
            tabPanel("Species Selection",
                     fluidRow(
                       shinydashboard::box(
                         title = "Species Selection", width = 12, status = "info",
                         fluidRow(
                           column(8,
                                  DT::dataTableOutput("speciesTable")
                           ),
                           column(4,
                                  h4("Selection Controls"),
                                  fluidRow(
                                    column(6, shiny::actionButton("selectAllSpecies", "Select All", class = "btn-block")),
                                    column(6, shiny::actionButton("deselectAllSpecies", "Deselect All", class = "btn-block"))
                                  ),
                                  hr(),
                                  h4("Filter Controls"),
                                  numericInput("minStations", "Min Stations:", value = 1, min = 1),
                                  numericInput("minRecords", "Min Records:", value = 1, min = 1),
                                  fluidRow(
                                    column(6, shiny::actionButton("selectByStations", "By Stations", class = "btn-block")),
                                    column(6, shiny::actionButton("selectByRecords", "By Records", class = "btn-block"))
                                  ),
                                  shiny::actionButton("selectByBoth", "Select by Both Criteria", class = "btn-block"),
                                  hr(),
                                  h4("Selection Summary"),
                                  textOutput("selectedSpeciesCount"),
                                  verbatimTextOutput("selectedSpeciesVector")
                           )
                         )
                       )
                     )
            ),
            
            
            tabPanel("Model Configuration",
                     fluidRow(
                       shinydashboard::box(
                         title = "Basic Settings", width = 12, status = "primary", collapsible = TRUE, collapsed = FALSE,
                         fluidRow(
                           column(4,
                                  selectInput("communityModelType", "Model Type:", 
                                              choices = c("Occupancy" = "Occupancy",   "Royle-Nichols" = "RN"),
                                              selected = "Occupancy")
                           ),
                           column(4, 
                                  # numericInput("occasionLength", "Occasion Length (days)", value = 7, min = 1)
                                  sliderInput("occasionLength_community", "Occasion Length (days)", 
                                              min = 1, max = 20, value = 10, 
                                              step = 1, ticks = FALSE)
                           ),
                           column(4, 
                                  checkboxInput("useNimble", "Use Nimble", value = FALSE)
                           )
                         )
                       )
                     ),
                     fluidRow(
                       shinydashboard::box(
                         title = "Advanced Settings", width = 12, status = "warning", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(4,
                                  selectInput("augmentationType", "Data Augmentation Type:",
                                              choices = c("None" = "none", "Max Known" = "maxknown", "Full" = "full"),
                                              selected = "none")
                           ),
                           column(4,
                                  conditionalPanel(
                                    condition = "input.augmentationType != 'none'",
                                    numericInput("augmentationValue", "Number of Potential Species:", value = NULL, min = 1)
                                  )
                           ),
                           column(4,
                                  textInput("richnessCategories", "Richness Categories (optional)",  placeholder = "Enter column name")
                           )
                         ),
                         fluidRow(
                           column(6,
                                  textInput("keyword_quadratic", "Keyword for Quadratic Effects", value = "_squared")
                           ),
                           column(6,
                                  textInput("modelFile", "Model File Name (optional)", placeholder = "Leave blank for temporary file")
                           )
                         )
                       )
                     ),
                     fluidRow(
                       shinydashboard::box(
                         title = "Model Configuration", width = 12, status = "info",
                         fluidRow(
                           column(6,
                                  h4("Detection Covariates"),
                                  selectInput("detIntercept", "Detection Intercept:", 
                                              choices = c("fixed", "ranef", "independent"),
                                              selected = "fixed"),
                                  varSelectizeInput("detCovFixed", "Fixed Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("detCovRanef", "Species Random Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("detCovIndep", "Independent Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  checkboxInput("speciesSiteRandomEffectDet", "Species-Site Random Effect on Detection", value = FALSE),
                                  h4("Effort as Detection Covariate"),
                                  checkboxInput("useEffortAsDetCov", "Use Effort as Detection Covariate", value = FALSE),
                                  conditionalPanel(
                                    condition = "input.useEffortAsDetCov == true",
                                    radioButtons("effortDetCovType", "Effort Effect Type:",
                                                 choices = c("Fixed" = "fixed", "Random" = "ranef"),
                                                 selected = "fixed")
                                  )
                           ),
                           column(6,
                                  h4("Occupancy Covariates"),
                                  selectInput("occuIntercept", "Occupancy Intercept:", 
                                              choices = c("fixed", "ranef", "independent"),
                                              selected = "fixed"),
                                  varSelectizeInput("occuCovFixed", "Fixed Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("occuCovRanef", "Species Random Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("occuCovIndep", "Independent Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE))
                           )
                         )
                       )
                     ),
                     fluidRow(
                       column(12, align = "center",
                              shiny::actionButton("createCommunityModel", "Create Model", class = "btn-primary btn-lg")
                       )
                     ),
                     fluidRow(
                       shinydashboard::box(
                         title = "Model Summary", width = 12, status = "success",
                         verbatimTextOutput("communityModelSummary")
                       )
                     )
            ),
            tabPanel("Model Fitting",
                     fluidRow(
                       shinydashboard::box(
                         title = "MCMC Settings", width = 12, status = "warning",
                         fluidRow(
                           column(3, numericInput("niter", "Number of Iterations", value = 1000, min = 100)),
                           column(3, numericInput("nburn", "Burn-in", value = 500, min = 0)),
                           column(3, numericInput("nthin", "Thinning", value = 1, min = 1)),
                           column(3, numericInput("nchains", "Number of Chains", value = 3, min = 1))
                         ),
                         shiny::actionButton("fitCommunityModel", "Fit Model", class = "btn-primary"),
                         shiny::actionButton("fitCommunityModel_background", "Fit Model (Background)", class = "btn-primary")
                       )
                     ),
                     fluidRow(
                       shinydashboard::box(
                         title = "Console output", width = 12, status = "info",
                         verbatimTextOutput("consoleOutput")
                       )
                     )
            ),
            tabPanel("Results",
                     tabsetPanel(
                       tabPanel("Parameter Estimates", DT::dataTableOutput("parameterEstimates")),
                       tabPanel("Parameter Quantiles", DT::dataTableOutput("parameterQuantiles")),
                       type = "pills"
                     )
            ),
            
            tabPanel("Diagnostics",
                     tabsetPanel(
                       # Convergence Diagnostics Tab
                       tabPanel("Convergence",
                                fluidRow(
                                  shinydashboard::box(
                                    title = "Convergence Summary", 
                                    width = 12, 
                                    status = "primary",
                                    verbatimTextOutput("convergenceDiagnostics")
                                  )
                                ),
                                fluidRow(
                                  shinydashboard::box(
                                    title = "Parameter-Specific Diagnostics", 
                                    width = 12, 
                                    status = "info",
                                    DT::dataTableOutput("gelman_diagnostics_table")
                                  )
                                ),
                                fluidRow(
                                  shinydashboard::box(
                                    title = "Trace Plots", 
                                    width = 12, 
                                    status = "warning",
                                    selectInput("trace_parameter", "Select Parameter:", choices = NULL),
                                    plotOutput("trace_plot", height = "400px")
                                  )
                                )
                       ),
                       
                       # # Goodness of Fit Tab
                       # tabPanel("Goodness of Fit",
                       #          fluidRow(
                       #            box(
                       #              title = "GoF Settings", 
                       #              width = 4, 
                       #              status = "primary",
                       #              numericInput("gof_nsim", "Number of simulations:", 
                       #                           value = 1000, min = 100, max = 10000),
                       #              actionButton("run_gof", "Run Goodness of Fit Test", 
                       #                           class = "btn-primary"),
                       #              actionButton("run_gof_background", "Run in Background", 
                       #                           class = "btn-info")
                       #            ),
                       #            box(
                       #              title = "Results", 
                       #              width = 8, 
                       #              status = "info",
                       #              verbatimTextOutput("gof_results")
                       #            )
                       #          ),
                       #          fluidRow(
                       #            box(
                       #              title = "Diagnostic Plots", 
                       #              width = 12, 
                       #              status = "primary",
                       #              plotOutput("gof_plots", height = "600px")
                       #            )
                       #          )
                       # )
                       tabPanel("Goodness of Fit",
                                fluidRow(
                                  shinydashboard::box(
                                    width = 12,
                                    status = "info",
                                    solidHeader = TRUE,
                                    title = "Community Model Goodness of Fit",
                                    div(
                                      style = "text-align: center; padding: 20px;",
                                      tags$h3(icon("chart-line"), " Coming Soon!"),
                                      tags$p(
                                        "Goodness of fit testing for community occupancy models will be implemented in a future update.",
                                        style = "font-size: 16px; color: #666;"
                                      ),
                                      tags$p(
                                        "This will include posterior predictive checks and other diagnostics specific to hierarchical community models.",
                                        style = "font-size: 14px; color: #888;"
                                      )
                                    )
                                  )
                                )
                       )
                     )
            ),
            
            
            
            # In the UI part of the Effect Plots tab
            tabPanel("Effect Plots",
                     fluidRow(
                       # Left panel with controls
                       column(3,
                              wellPanel(
                                # Group 1: Basic Settings
                                tags$div(
                                  class = "settings-group",
                                  h4("Basic Settings"),
                                  selectInput("plotSubmodel", "Submodel:", 
                                              choices = c("state", "det")),
                                  
                                  # Response selector only shows for RN models with state submodel
                                  conditionalPanel(
                                    condition = "input.communityModelType == 'RN' && input.plotSubmodel == 'state'",
                                    selectInput("plotResponse", "Response:", 
                                                choices = c("occupancy", "abundance"))
                                  )
                                ),
                                
                                # Group 2: Species Selection
                                tags$div(
                                  class = "settings-group",
                                  style = "margin-top: 20px;",
                                  h4("Species Selection"),
                                  selectInput("plotSpeciesSubset", "Select Species:", 
                                              choices = NULL,
                                              multiple = TRUE,
                                              selectize = TRUE)
                                ),
                                
                                # Group 3: Plot Selection & Settings
                                tags$div(
                                  class = "settings-group",
                                  style = "margin-top: 20px;",
                                  
                                  tags$div(
                                    class = "settings-group",
                                    style = "margin-top: 20px;",
                                    h4("Plot Settings"),
                                    selectInput("selectedPlot", "Select Effect:", 
                                                choices = NULL),  # Will be updated dynamically
                                    numericInput("plotDraws", "Number of Draws", 
                                                 value = 1000, min = 100),
                                    numericInput("plotLevelOuter", "Confidence Level", 
                                                 value = 0.95, min = 0, max = 1, step = 0.01),
                                    numericInput("plotLevelInner", "Inner Confidence Level", 
                                                 value = 0.75, min = 0, max = 1, step = 0.01),
                                    numericInput("plotScale", "Plot Size Scale", 
                                                 value = 1.5, min = 0.5, max = 3, step = 0.1),
                                    checkboxInput("orderByEffect", "Order by Effect Size", value = TRUE)
                                  ),
                                  
                                  numericInput("plotDraws", "Number of Draws", 
                                               value = 1000, min = 100),
                                  checkboxInput("orderByEffect", "Order by Effect Size", value = TRUE)
                                ),
                                
                                # Update button
                                shiny::actionButton("updatePlotSettings", "Update Plot", 
                                                    class = "btn-primary btn-block",
                                                    style = "margin-top: 20px;")
                              )
                       ),
                       
                       # Right panel with plots
                       column(9,
                              tabsetPanel(
                                tabPanel("Response Curves",
                                         plotOutput("responseCurvePlot", height = "800px")),
                                tabPanel("Effect Sizes",
                                         plotOutput("coefficientPlot", height = "800px"))
                              )
                       )
                     )
            ),
            
            
            tabPanel("Spatial Predictions",
                     fluidRow(
                       # Left sidebar with global settings
                       column(3,
                              wellPanel(
                                h4("Input Settings", class = "text-primary"),
                                
                                # Input source selection with help text
                                selectInput("prediction_raster_source", 
                                            "Prediction Surface:", 
                                            choices = c(
                                              "Use extracted covariates" = "extracted",
                                              "Upload custom raster" = "custom"
                                            )
                                ),
                                helpText("Use covariates extracted earlier or upload custom raster"),
                                
                                # Conditional panel for custom raster upload
                                conditionalPanel(
                                  condition = "input.prediction_raster_source == 'custom'",
                                  fileInput("covariate_raster", "Upload Raster:", accept = c(".tif"))
                                ),
                                
                                hr(),
                                
                                # Common MCMC settings
                                h4("MCMC Settings", class = "text-primary"),
                                numericInput("predictionDraws", "Posterior Draws:", 
                                             value = 1000, min = 100),
                                numericInput("predictionLevel", "Confidence Level:", 
                                             value = 0.95, min = 0, max = 1, step = 0.01),
                                
                                hr(),
                                
                                # Performance settings
                                h4("Performance Settings", class = "text-primary"),
                                checkboxInput("predictionBatch", "Use Batch Processing", value = FALSE),
                                conditionalPanel(
                                  condition = "input.predictionBatch == true",
                                  numericInput("batchSize", "Batch Size:", value = 1000, min = 100)
                                ),
                                numericInput("predictionSeed", "Random Seed (optional):", 
                                             value = NULL)
                              )
                       ),
                       
                       # Main content area with tabs
                       column(9,
                              tabsetPanel(
                                id = "predictionTabs",
                                type = "pills",
                                
                                # Species Occupancy tab
                                tabPanel("Species Occupancy",
                                         fluidRow(
                                           column(12, 
                                                  div(class = "panel panel-default",
                                                      div(class = "panel-heading d-flex justify-content-between align-items-center",
                                                          div(
                                                            style = "display: flex; gap: 10px; align-items: center;",
                                                            shiny::actionButton("runOccupancyPrediction", "Generate Predictions",
                                                                                class = "btn-primary",
                                                                                icon = icon("calculator")),
                                                            selectInput("occupancySpecies", "Select Species:", 
                                                                        choices = NULL, width = "300px"),
                                                            selectInput("occupancyMapType", "Display:", 
                                                                        choices = c(
                                                                          "Mean Occupancy" = "mean",
                                                                          "Standard Deviation" = "sd",
                                                                          "Lower CI" = "lower",
                                                                          "Upper CI" = "upper"
                                                                        ),
                                                                        selected = "mean",
                                                                        width = "200px"
                                                            )
                                                          )
                                                      ),
                                                      div(class = "panel-body",
                                                          leaflet::leafletOutput("occupancyMap", height = "600px"),
                                                          uiOutput("occupancyStats")
                                                      )
                                                  )
                                           )
                                         )
                                ),
                                
                                # Species Richness tab
                                tabPanel("Species Richness",
                                         fluidRow(
                                           column(12,
                                                  div(class = "panel panel-default",
                                                      div(class = "panel-heading d-flex justify-content-between align-items-center",
                                                          div(
                                                            style = "display: flex; gap: 10px; align-items: center;",
                                                            shiny::actionButton("runRichnessPrediction", "Generate Predictions",
                                                                                class = "btn-primary",
                                                                                icon = icon("calculator")),
                                                            selectInput("richnessType", "Display:", 
                                                                        choices = c(
                                                                          "Mean Richness" = "mean",
                                                                          "Standard Deviation" = "sd",
                                                                          "Lower CI" = "lower",
                                                                          "Upper CI" = "upper"
                                                                        ),
                                                                        selected = "mean",
                                                                        width = "200px"
                                                            )
                                                          )
                                                      ),
                                                      div(class = "panel-body",
                                                          leaflet::leafletOutput("richnessMap", height = "600px"),
                                                          uiOutput("richnessStats")
                                                      )
                                                  )
                                           )
                                         )
                                ),
                                
                                # PAO Analysis tab
                                tabPanel("Area Occupied (PAO)",
                                         fluidRow(
                                           column(12,
                                                  div(class = "panel panel-default",
                                                      div(class = "panel-heading d-flex justify-content-between align-items-center",
                                                          div(
                                                            style = "display: flex; gap: 10px; align-items: center;",
                                                            shiny::actionButton("runPAOPrediction", "Generate Predictions",
                                                                                class = "btn-primary",
                                                                                icon = icon("calculator"))
                                                          )
                                                      ),
                                                      div(class = "panel-body",
                                                          plotOutput("paoPlot", height = "500px"),
                                                          DT::dataTableOutput("paoTable")
                                                      )
                                                  )
                                           )
                                         )
                                )
                              )
                       )
                     ),
                     
                     # Footer with memory usage warning
                     fluidRow(
                       column(12,
                              div(class = "alert alert-info mt-3",
                                  icon("info-circle"),
                                  "Predictions can be memory-intensive. Use batch processing for large datasets or if you encounter memory issues."
                              )
                       )
                     )
            )
            
          )
        )
      )
    )
  )
  
  
  
  server <- function(input, output, session) { 
    
    #  version check
    observe({
      version_info <- check_version()
      if (!is.null(version_info)) {
        showNotification(
          version_info$text,
          type = version_info$type,
          duration = NULL,
          closeButton = TRUE,
          id = "version-check"
        )
      }
    })
    
    # Welcome screen (if dashboards started without data)
    output$welcome_screen <- renderUI({
      if (!is.null(CTtable) && !is.null(recordTable)) {
        return(NULL)
      }
      
      if (!is.null(data$CTtable) && !is.null(data$recordTable)) {
        return(NULL)
      }
      
      version_info <- check_version()
      version_alert <- if (!is.null(version_info)) {
        tags$div(
          class = "alert alert-warning",
          icon("info-circle"), 
          version_info$text
        )
      }
      
      shinydashboard::box(
        width = 12,
        status = "info",
        solidHeader = TRUE,
        title = "Welcome to the Camera Trap Survey Dashboard",
        
        if (!is.null(version_info)) version_alert,
        
        tags$div(
          style = "padding: 20px;",
          tags$h4("Getting Started"),
          tags$p("To begin analyzing your camera trap data, you'll need to import:"),
          tags$ul(
            tags$li(tags$strong("Camera Trap Table:"), " Contains deployment information for each station"),
            tags$li(tags$strong("Record Table:"), " Contains species detection records")
          ),
          
          tags$h4("Import Options"),
          tags$ul(
            tags$li(tags$strong("CSV Import:"), " Upload data directly from CSV files"),
            tags$li(tags$strong("Wildlife Insights:"), " Import from Wildlife Insights export"),
            tags$li(tags$strong("camtrapDP:"), " Import camera trap data package")
          ),
          
          tags$hr(),
          
          tags$p("Use the Import Data menu on the left to get started."),
          tags$p("Once your data is imported, you'll have access to:",
                 tags$ul(
                   tags$li("Data summaries and visualizations"),
                   tags$li("Interactive maps"),
                   tags$li("Species activity patterns"),
                   tags$li("Occupancy modeling"),
                   tags$li("And more...")
                 )
          )
        )
      )
    })
    
    
    
    # Tab: Import data ----
    
    ## Import from csv ----
    # Reactive value to store the current max file size
    current_max_size <- shiny::reactiveVal(5)
    
    # Observer to update max file size
    shiny::observeEvent(input$update_max_size, {
      new_max_size <- input$max_file_size * 1024^2  # Convert MB to bytes
      options(shiny.maxRequestSize = new_max_size)
      current_max_size(input$max_file_size)
      shiny::showNotification(paste("Maximum file size updated to", input$max_file_size, "MB"), type = "message")
    })
    
    
    
    
    data <- shiny::reactiveValues(
      CTtable = CTtable,
      CTtable_sf = NULL,
      aggregated_CTtable = NULL,
      recordTable = recordTable,
      stationCol = stationCol,
      cameraCol = cameraCol, 
      xcol = xcol,
      ycol = ycol,
      crs = crs,
      setupCol = setupCol,
      retrievalCol = retrievalCol,
      hasProblems = hasProblems,
      CTdateFormat = CTdateFormat,
      camerasIndependent = camerasIndependent,
      speciesCol = speciesCol,
      recordDateTimeCol = recordDateTimeCol,
      recordDateTimeFormat = recordDateTimeFormat,
      
      timeZone = timeZone,
      exclude = exclude,
      
      prediction_raster = NULL,
      original_rasters = NULL,
      
      study_area = NULL,
      study_area_buffer = NULL,
      
      # Temporary storage for imported data
      CTtable_temp = NULL,
      recordTable_temp = NULL,
      
      # for scaled covariate information and prediction rasters
      scaling_params = NULL,
      aggregated_CTtable_scaled = NULL,
      prediction_raster_scaled = NULL
      
    )
    
    # check if required data were loaded
    output$data_loaded <- renderText({
      as.character(!is.null(data$CTtable) && !is.null(data$recordTable))
    })
    outputOptions(output, 'data_loaded', suspendWhenHidden = FALSE)
    
    
    # Function to generate data preview
    generate_preview <- function(data, max_rows = 5) {
      if (is.null(data)) return(NULL)
      preview <- head(data, max_rows)
      DT::datatable(preview, options = list(scrollX = TRUE, dom = 't'))
    }
    
    # Observer for CT file upload
    shiny::observeEvent(input$ct_file, {
      req(input$ct_file)
      data$CTtable_temp <- read.csv(input$ct_file$datapath, stringsAsFactors = FALSE)
      updateSelectInput(session, "stationCol", choices = names(data$CTtable_temp))
      updateSelectInput(session, "cameraCol", choices = c("", names(data$CTtable_temp)))
      updateSelectInput(session, "xcol", choices = names(data$CTtable_temp))
      updateSelectInput(session, "ycol", choices = names(data$CTtable_temp))
      updateSelectInput(session, "setupCol", choices = names(data$CTtable_temp))
      updateSelectInput(session, "retrievalCol", choices = names(data$CTtable_temp))
      
      # Generate preview
      output$ct_preview <- DT::renderDT({
        generate_preview(data$CTtable_temp)
      })
    })
    
    # Observer for record file upload
    shiny::observeEvent(input$record_file, {
      req(input$record_file)
      data$recordTable_temp <- read.csv(input$record_file$datapath, stringsAsFactors = FALSE)
      updateSelectInput(session, "speciesCol", choices = names(data$recordTable_temp))
      updateSelectInput(session, "recordDateTimeCol", choices = names(data$recordTable_temp))
      
      # Generate preview
      output$record_preview <- DT::renderDT({
        generate_preview(data$recordTable_temp)
      })
    })
    
    # Observer for CT "Done" button
    shiny::observeEvent(input$ct_done, {
      req(data$CTtable_temp)
      data$CTtable <- data$CTtable_temp
      data$stationCol <- input$stationCol
      data$cameraCol <- if(input$cameraCol != "") input$cameraCol else NULL
      data$xcol <- input$xcol
      data$ycol <- input$ycol
      data$crs <- input$crs
      data$setupCol <- input$setupCol
      data$retrievalCol <- input$retrievalCol
      data$CTdateFormat <- input$CTdateFormat
      data$hasProblems <- input$hasProblems
      shiny::showNotification("Camera Trap data updated", type = "message")
    })
    
    # Observer for Record "Done" button
    shiny::observeEvent(input$record_done, {
      req(data$recordTable_temp)
      data$recordTable <- data$recordTable_temp
      data$speciesCol <- input$speciesCol
      data$recordDateTimeCol <- input$recordDateTimeCol
      data$recordDateTimeFormat <- input$recordDateTimeFormat
      data$timeZone <- input$timeZone
      data$exclude <- if (input$exclude != "") strsplit(input$exclude, ",")[[1]] else NULL
      shiny::showNotification("Record data updated", type = "message")
    })
    
    
    
    # reactive expression for CT table as sf object
    CTtable_sf <- shiny::reactive({
      req(data$CTtable, data$xcol, data$ycol, data$crs)
      sf::st_as_sf(data$CTtable, coords = c(data$xcol, data$ycol), crs = data$crs, remove = FALSE)
    })
    
    # Update CTtable_sf when CT table is updated
    observe({
      data$CTtable_sf <- CTtable_sf()
    })
    
    # Createreactive expression for aggregated_CTtable
    aggregated_CTtable <- shiny::reactive({
      req(data$CTtable_sf, data$stationCol)
      aggregateCTtableByStation(df = data$CTtable_sf, stationCol = data$stationCol)
    })
    
    # Update aggregated_CTtable when CTtable_sf changes
    observe({
      data$aggregated_CTtable <- aggregated_CTtable()
    })
    
    
    
    ## Import from Wildlife Insights ----
    
    
    # Reactive value to store imported Wildlife Insights data
    wi_data <- reactiveVal(NULL)
    
    # Observer for Wildlife Insights import
    observeEvent(input$wi_import_button, {
      req(input$wi_import_type)
      
      withProgress(message = 'Importing Wildlife Insights data...', value = 0, {
        tryCatch({
          if (input$wi_import_type == "zip") {
            req(input$wi_zip_file)
            wi_imported <- readWildlifeInsights(zipfile = input$wi_zip_file$datapath)
          } else if (input$wi_import_type == "csv") {
            req(input$wi_deployment_file, input$wi_detection_file)
            wi_imported <- readWildlifeInsights(
              deployment_file = input$wi_deployment_file$datapath,
              image_file = input$wi_detection_file$datapath
            )
          } else if (input$wi_import_type == "directory") {
            req(input$wi_directory)
            if (!dir.exists(input$wi_directory)) {
              stop("The specified directory does not exist.")
            }
            wi_imported <- readWildlifeInsights(directory = input$wi_directory)
          }
          
          # Store the imported data
          wi_data(wi_imported)
          
          # Update the main data reactive values
          data$CTtable <- wi_imported$CTtable
          data$CTtable_sf <- sf::st_as_sf(wi_imported$CTtable, 
                                          coords = c("longitude", "latitude"), 
                                          crs = 4326)
          data$aggregated_CTtable <- wi_imported$CTtable_aggregated
          data$recordTable <- wi_imported$recordTable
          
          # Update other necessary reactive values
          data$stationCol <- "placename"
          data$cameraCol <- "camera_id"
          data$xcol <- "longitude"
          data$ycol <- "latitude"
          data$crs <- 4326
          data$setupCol <- "start_date"
          data$retrievalCol <- "end_date"
          data$speciesCol <- "common_name"
          data$recordDateTimeCol <- "timestamp"
          data$camerasIndependent <- input$wi_cameras_independent
          data$recordDateTimeFormat = "ymd HMS"
          data$CTdateFormat <- "ymd HMS"
          
          
          # Show success message
          showNotification("Wildlife Insights data imported successfully", type = "message")
          
          
        }, error = function(e) {
          showNotification(paste("Error importing Wildlife Insights data:", e$message), type = "error")
        })
      })
    })
    
    # Render Wildlife Insights import status
    output$wi_import_status <- renderText({
      if (is.null(wi_data())) {
        "No data imported yet."
      } else {
        paste("Data imported successfully.",
              "Number of deployments:", nrow(wi_data()$CTtable),
              "Number of images:", nrow(wi_data()$recordTable))
      }
    })
    
    # Render Wildlife Insights data previews
    output$wi_deployment_preview <- DT::renderDT({
      req(wi_data())
      datatable(head(wi_data()$CTtable, 100), options = list(scrollX = TRUE))
    })
    
    output$wi_detection_preview <- DT::renderDT({
      req(wi_data())
      datatable(head(wi_data()$recordTable, 100), options = list(scrollX = TRUE))
    })
    
    # Update other inputs based on imported data
    
    observe({
      req(wi_data())
      
      # Update column selection inputs
      updateSelectInput(session, "stationCol", choices = names(wi_data()$CTtable), selected = "placename")
      updateSelectInput(session, "cameraCol", choices = c("", names(wi_data()$CTtable)), selected = "camera_id")
      updateSelectInput(session, "xcol", choices = names(wi_data()$CTtable), selected = "longitude")
      updateSelectInput(session, "ycol", choices = names(wi_data()$CTtable), selected = "latitude")
      updateSelectInput(session, "setupCol", choices = names(wi_data()$CTtable), selected = "start_date")
      updateSelectInput(session, "retrievalCol", choices = names(wi_data()$CTtable), selected = "end_date")
      updateSelectInput(session, "speciesCol", choices = names(wi_data()$recordTable), selected = "common_name")
      updateSelectInput(session, "recordDateTimeCol", choices = names(wi_data()$recordTable), selected = "timestamp")
      
      # Update other inputs as needed
      updateTextInput(session, "crs", value = "4326")
      updateTextInput(session, "CTdateFormat", value = "ymd HMS")
      updateTextInput(session, "recordDateTimeFormat", value = "ymd HMS")
    })
    
    
    
    update_species_inputs <- function() {
      req(data$recordTable, data$speciesCol)
      
      species_list <- sort(unique(data$recordTable[[data$speciesCol]]))
      if (!is.null(data$exclude)) {
        species_list <- species_list[!species_list %in% data$exclude]
      }
      if (length(species_list) == 0) {
        species_list <- character(0)  # Return empty character vector
      }
      
      # Update species list for species detection maps
      updateSelectInput(session, "species_for_map", 
                        choices = c("n_species", species_list),
                        selected = "n_species")
      
      # Update species selection for activity plots
      updateSelectInput(session, "ad_species", 
                        choices = species_list,
                        selected = species_list[1])
      updateSelectInput(session, "speciesA", 
                        choices = species_list,
                        selected = species_list[1])
      updateSelectInput(session, "speciesB", 
                        choices = species_list,
                        selected = species_list[min(2, length(species_list))])
      
      # Update species selection for single species occupancy models
      updateSelectInput(session, "species_dethist", 
                        choices = species_list,
                        selected = if(length(species_list) > 0) species_list[1] else NULL)
      
      return(species_list)
    }
    
    # Create a reactive value for the species list
    current_species_list <- reactiveVal()
    
    # Observer to update species inputs when recordTable changes
    observe({
      req(data$recordTable)
      species_list <- update_species_inputs()
      # cat("Updated species list:", paste(species_list, collapse=", "), "\n")
      current_species_list(species_list)
    })
    
    
    ## import shapefile ----
    
    observeEvent(input$aoi_done, {
      req(input$aoi_path)
      
      tryCatch({
        # Get directory and layer name from the specified path
        dsn <- dirname(input$aoi_path)
        layer <- tools::file_path_sans_ext(basename(input$aoi_path))
        
        # Read the shapefile
        study_area <- sf::st_read(dsn = dsn,
                                  layer = layer,
                                  quiet = T)
        
        # Check geometry type
        geom_types <- unique(sf::st_geometry_type(study_area))
        if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
          showNotification(
            "Study area must be a polygon or multipolygon shapefile. Points or lines are not supported.",
            type = "error"
          )
          return()
        }
        
        
        # Transform to match camera trap CRS if needed
        if (!is.null(data$CTtable_sf) && 
            sf::st_crs(study_area) != sf::st_crs(data$CTtable_sf)) {
          study_area <- sf::st_transform(study_area, sf::st_crs(data$CTtable_sf))
        }
        
        # Store in reactive values
        data$study_area <- st_make_valid(study_area)
        
        # Create preview map
        output$aoi_preview <- leaflet::renderLeaflet({
          req(data$study_area)
          
          base_map <- mapview::mapview(st_buffer(data$study_area, 0),
                                       col.regions = "transparent",
                                       color = "red",
                                       lwd = 2,
                                       layer.name = "Study Area",
                                       map.types = "OpenStreetMap")
          
          if (!is.null(data$CTtable_sf)) {
            base_map <- base_map +
              mapview::mapview(data$CTtable_sf,
                               cex = 3,
                               label = data$stationCol,
                               col.regions = "blue",
                               layer.name = "Camera Stations")
          }
          
          base_map@map
        })
        
        # Display study area information
        output$aoi_info <- renderPrint({
          req(data$study_area)
          
          cat("Study Area Properties:\n")
          cat("CRS:", sf::st_crs(data$study_area)$input, "\n")
          cat("Area:", round(as.numeric(sf::st_area(data$study_area))/1e6, 2), "km\U00B2\n")
          cat("Number of polygons:", nrow(data$study_area), "\n")
        })
        
        showNotification("Study area shapefile imported successfully", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error importing study area shapefile:", e$message), 
                         type = "error")
      })
    })
    
    
    # camera operation matrix ----
    
    # Reactive expression for camop_args
    camop_args <- shiny::reactive({
      req(data$CTtable, data$setupCol, data$retrievalCol, data$CTdateFormat, data$stationCol)
      args <- list(
        CTtable = data$CTtable,
        setupCol = data$setupCol,
        retrievalCol = data$retrievalCol,
        dateFormat = data$CTdateFormat,
        hasProblems = data$hasProblems,
        stationCol = data$stationCol
      )
      
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        args <- c(args,
                  cameraCol = data$cameraCol,
                  byCamera = FALSE,
                  allCamsOn = FALSE,
                  camerasIndependent = data$camerasIndependent
        )
      }
      args
    })
    
    # Reactive expression for camop
    camop <- shiny::reactive({
      args <- camop_args()
      tryCatch({
        result <- do.call(cameraOperation, args)
        if (is.null(result) || nrow(result) == 0) {
          warning("cameraOperation returned NULL or empty result")
        }
        result
      }, error = function(e) {
        warning(paste("Error in cameraOperation:", e$message))
        NULL
      })
    })
    
    # Render the plotly output
    output$camop <- plotly::renderPlotly({
      
      req(camop(), data$stationCol)
      
      camop_df <- as.data.frame(camop())
      camop_df[, data$stationCol] <- rownames(camop_df)
      
      camop_df <- reshape2::melt(camop_df, id.vars = data$stationCol)
      colnames(camop_df)[colnames(camop_df) == "variable"] <- "Date"
      colnames(camop_df)[colnames(camop_df) == "value"] <- "Effort"
      
      
      plotly::layout(
        p = plotly::plot_ly(
          camop_df,
          x = ~Date, 
          y = ~get(data$stationCol), 
          z = ~Effort,
          type = "heatmap",
          colors = hcl.colors(100, palette = "viridis", rev = TRUE)
        ),
        yaxis = list(
          title = data$stationCol,
          categoryorder = "category descending"
        )
      )
    })
    
    
    
    
    # define function for aggregating by station (collapsing cameras at station)
    aggregateCTtableByStation <- function(df, stationCol) {
      
      if(inherits(df, "sf")) df <- sf::st_drop_geometry(df)
      
      # return original table if all station values are unique
      if(all(table(df[, stationCol]) == 1)) return(df)
      
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
      
      if(inherits(df, "tbl")) {
        df_agg <- aggregate(df, 
                            by = list(df[[stationCol]]), 
                            FUN = agg_fun)
        df_agg[, stationCol] <- NULL
      } else {
        df_agg <- aggregate(df, 
                            by = list(df[,stationCol]), 
                            FUN = agg_fun)
        df_agg[, stationCol] <- NULL
      }
      
      
      
      colnames(df_agg) <- c(stationCol, 
                            names(df_agg)[-1])
      
      return(df_agg)
      # factors are converted to characters. Problematic?
    }
    
    
    
    
    df_covariates <- observe({
      req(data$CTtable_aggregated, camop())
      stopifnot(rownames(camop()) == data$CTtable_aggregated[, data$stationCol])
      data$CTtable_aggregated
    })
    
    
    
    # remove excluded records (silently at the moment)
    if (!is.null(exclude)) {
      num_images_excluded <- sum(data$recordTable[, speciesCol] %in% exclude)
      data$recordTable <- recordTable[!data$recordTable[, speciesCol] %in% exclude, ]
    } else {
      num_images_excluded <- 0
    }
    
    
    # Calculate the number of unique stations and species
    # Reactive expressions for calculations
    num_stations <- shiny::reactive({
      req(data$CTtable, data$stationCol)
      length(unique(data$CTtable[[data$stationCol]]))
    })
    
    num_species <- shiny::reactive({
      req(data$recordTable, data$speciesCol)
      length(unique(data$recordTable[[data$speciesCol]]))
    })
    
    date_range_min <- shiny::reactive({
      req(data$CTtable, data$setupCol, data$CTdateFormat)
      as.Date(min(lubridate::parse_date_time(data$CTtable[[data$setupCol]], orders = data$CTdateFormat)))
    })
    
    date_range_max <- shiny::reactive({
      req(data$CTtable, data$retrievalCol, data$CTdateFormat)
      as.Date(max(lubridate::parse_date_time(data$CTtable[[data$retrievalCol]], orders = data$CTdateFormat)))
    })
    
    trap_nights <- shiny::reactive({
      req(camop())
      round(sum(camop(), na.rm = TRUE), 1)
    })
    
    trap_nights_avg <- shiny::reactive({
      req(camop())
      round(mean(apply(camop(), 1, sum, na.rm = TRUE)), 1)
    })
    
    num_images <- shiny::reactive({
      req(data$recordTable)
      nrow(data$recordTable)
    })
    
    avg_records_per_station <- shiny::reactive({
      req(num_images(), num_stations())
      round(num_images() / num_stations(), 1)
    })
    
    
    
    # Render the value boxes
    output$num_stations <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_stations(),
        subtitle = "Stations",
        icon = shiny::icon("map-marker")
      )
    })
    
    output$num_images <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_images(),
        subtitle = "Images",
        icon = shiny::icon("camera")
      )
    })
    
    output$num_images_removed <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_images_excluded(),
        subtitle = "Images",
        icon = shiny::icon("user-minus")
      )
    })
    
    output$num_species <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = num_species(),
        subtitle = "Species detected",
        icon = shiny::icon("paw")
      )
    })
    
    output$avg_records_per_station <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = avg_records_per_station(),
        subtitle = "Avg records per station",
        icon = shiny::icon("table")
      )
    })
    
    output$date_range_min <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = date_range_min(),
        subtitle = "First survey day",
        icon = shiny::icon("calendar-plus")
      )
    })
    
    output$date_range_max <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = date_range_max(),
        subtitle = "Last survey day",
        icon = shiny::icon("calendar-minus"),
        width = NULL
      )
    })
    
    output$trap_nights <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = trap_nights(),
        subtitle = "Trap nights (total)",
        icon = shiny::icon("calendar-days"),
        width = NULL
      )
    })
    
    output$trap_nights_avg <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = trap_nights_avg(),
        subtitle = "Trap nights (average per station)",
        icon = shiny::icon("calendar-day"),
        width = NULL
      )
    })
    
    
    # plot number of records (by species)
    
    species_order <- reactive({
      req(data$recordTable, data$speciesCol)
      rev(levels(stats::reorder(
        factor(data$recordTable[[data$speciesCol]]),
        -table(data$recordTable[[data$speciesCol]])[factor(data$recordTable[[data$speciesCol]])]
      )))
    })
    
    
    
    df_n_records <- reactive({
      req(data$recordTable, data$speciesCol, species_order())
      df <- as.data.frame(table(data$recordTable[, data$speciesCol]))
      colnames(df) <- c(data$speciesCol, "Count")
      df[[data$speciesCol]] <- factor(df[[data$speciesCol]], levels = species_order())
      df
    })
    
    
    # null column names used in ggplot call to avoid CRAN notes
    # find a better solution later, e.g. using .data
    Count <- NULL
    n_species <- NULL
    n_stations <- NULL
    
    output$plot_n_records <- plotly::renderPlotly({
      plotly::ggplotly(
        ggplot(df_n_records(), aes(y = !!ggplot2::sym(data$speciesCol), x = Count)) +
          geom_col() +
          theme_bw() +
          labs(y = data$speciesCol) +
          ggtitle(label = "Number of records by species")
      )
    })
    
    df_n_species <- reactive({
      req(data$recordTable, data$stationCol, data$speciesCol)
      df <- as.data.frame(tapply(data$recordTable[, data$speciesCol], 
                                 INDEX = data$recordTable[, data$stationCol], 
                                 FUN = function(x) {length(unique(x))}))
      colnames(df) <- "n_species"
      df[, data$stationCol] <- rownames(df)
      
      # Order stations by number of species
      station_order <- order(df[, "n_species"], decreasing = FALSE)
      df[[data$stationCol]] <- factor(df[[data$stationCol]], levels = df[[data$stationCol]][station_order])
      
      df
    })
    
    output$plot_n_species <- plotly::renderPlotly({
      req(df_n_species())
      plotly::ggplotly(
        ggplot(df_n_species(), aes(y = !!ggplot2::sym(data$stationCol), x = n_species)) +
          geom_col() +
          theme_bw() +
          labs(y = data$stationCol, x = "Observed species") +
          ggtitle(label = "Number of observed species by station")
      )
    })
    
    df_n_stations <- reactive({
      req(data$recordTable, data$stationCol, data$speciesCol, species_order())
      df <- as.data.frame(tapply(data$recordTable[, data$stationCol], 
                                 INDEX = data$recordTable[, data$speciesCol], 
                                 FUN = function(x) {length(unique(x))}))
      colnames(df) <- "n_stations"
      df[, data$speciesCol] <- rownames(df)
      df[[data$speciesCol]] <- factor(df[[data$speciesCol]], levels = species_order())
      df[order(df[, "n_stations"], decreasing = FALSE), ]
    })
    
    
    
    output$plot_n_stations <- plotly::renderPlotly({
      species_order <- df_n_stations()[order(df_n_stations()[, "n_stations"], decreasing = F), data$speciesCol]
      
      plotly::ggplotly(
        ggplot(df_n_stations(), aes(y = !!ggplot2::sym(data$speciesCol), x = n_stations)) +
          geom_col() +
          theme_bw() +
          labs(y = data$speciesCol, x = "Number of stations") +
          scale_y_discrete(limits = species_order) +
          ggtitle(label = "Number of stations with detections (by species)")
      )
    })
    
    
    # Update the max value of occasionLength slider based on camop
    observe({
      req(camop())
      updateSliderInput(session, "occasionLength_single_species", max = ncol(camop()))
      updateSliderInput(session, "occasionLength_community", max = ncol(camop()))
    })
    
    
    
    # Tab: camera traps table     ####
    output$current_camera_traps_table <- DT::renderDT({
      req(data$CTtable_sf)
      DT::datatable(
        sf::st_drop_geometry(data$CTtable_sf),
        # data$CTtable,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    
    
    ## Tab: camera traps table (aggregated)     ####
    output$aggregated_camera_traps_table <- DT::renderDT({
      req(data$aggregated_CTtable)
      DT::datatable(
        sf::st_drop_geometry(data$aggregated_CTtable),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # Tab: record table      ####
    output$record_table <- DT::renderDT({
      DT::datatable(
        data$recordTable,
        options = list(pageLength = 10,
                       scrollX = TRUE)
      )
    })
    
    
    
    # Tab: Overview maps      ####
    
    # Reactive expressions for MCP calculations
    mcp <- shiny::reactive({
      req(data$CTtable_sf)
      tryCatch({
        sf::st_convex_hull(sf::st_union(data$CTtable_sf))
      }, error = function(e) {
        message("Error in MCP calculation: ", e$message)
        NULL
      })
    })
    
    
    area_mcp_km2 <- shiny::reactive({
      req(mcp())
      tryCatch({
        area_mcp_m2 <- sf::st_area(mcp())
        units::set_units(area_mcp_m2, "km^2")
      }, error = function(e) {
        message("Error in area calculation: ", e$message)
        NULL
      })
    })
    
    # Render the area MCP value box
    output$area_mcp <- shinydashboard::renderValueBox({
      req(area_mcp_km2())
      shinydashboard::valueBox(
        value = paste(round(as.numeric(area_mcp_km2()), 2), "km\U00B2"),
        subtitle = "Survey area (MCP)",
        icon = shiny::icon("vector-square")
      )
    })
    
    
    
    
    
    # Render the overview map
    output$overview_map <- leaflet::renderLeaflet({
      req(data$CTtable_sf, data$stationCol, mcp(), area_mcp_km2())
      
      # Start with camera stations
      map_view <- mapview::mapview(
        data$CTtable_sf,
        xcol = data$xcol,
        ycol = data$ycol,
        label = data$CTtable_sf[[data$stationCol]],
        legend = FALSE,
        cex = 10,
        layer.name = "Camera trap stations"
      )
      
      # Add study area if available
      if (!is.null(data$study_area)) {
        map_view <- map_view + 
          mapview::mapview(data$study_area,
                           col.regions = "transparent",
                           color = "red",
                           lwd = 2,
                           layer.name = "Study Area")
      }
      
      # Add MCP
      map_view <- map_view + 
        mapview::mapview(mcp(),
                         col.regions = "firebrick",
                         color = "firebrick",
                         alpha.regions = 0.1,
                         layer.name = paste0(
                           "Minimum convex polygon (",
                           paste(round(as.numeric(area_mcp_km2()), 2), "km\U00B2"),
                           ")"
                         ),
                         hide = TRUE
        )
      
      map_view@map
    })
    
    
    
    # Tab: Species maps      ####
    
    # Reactive expression for detectionMaps
    detmaps <- shiny::reactive({
      req(data$CTtable, data$recordTable, data$stationCol, data$speciesCol, data$xcol, data$ycol)
      suppressWarnings(detectionMaps(
        CTtable = data$CTtable,
        recordTable = data$recordTable,
        stationCol = data$stationCol,
        speciesCol = data$speciesCol,
        Xcol = data$xcol,
        Ycol = data$ycol,
        plotR = FALSE
      ))
    })
    
    # Reactive expression for sf object
    detmaps_sf <- shiny::reactive({
      req(detmaps(), data$xcol, data$ycol, data$crs)
      sf::st_as_sf(
        detmaps(),
        coords = c(data$xcol, data$ycol),
        crs = sf::st_crs(data$crs)
      )
    })
    
    # Render the species map
    output$maps <- leaflet::renderLeaflet({
      req(detmaps_sf(), input$species_for_map, input$no_record_more_transparent, input$scale_size)
      
      if (input$species_for_map != "n_species") {
        species_tmp <- gsub("[[:space:][:punct:]]+", ".", input$species_for_map)
        layer.name <- input$species_for_map
      } else {
        species_tmp <- input$species_for_map
        layer.name <- "Species richness (observed)"
      }
      
      detmaps_sf_data <- detmaps_sf()
      detmaps_sf_logi <- detmaps_sf_data
      detmaps_sf_logi[[species_tmp]] <- detmaps_sf_logi[[species_tmp]] >= 1
      detmaps_sf_logi <- detmaps_sf_logi[detmaps_sf_logi[[species_tmp]], ]
      
      # make stations without records more transparent
      if(input$no_record_more_transparent) {
        alpha <- ifelse(detmaps_sf_data[[species_tmp]] >= 1, 0.9, 0.85)
      } else {
        alpha <- 0.9
      }
      detmaps_sf_data$alpha <- alpha
      
      map_view <- mapview::mapview(
        detmaps_sf_data,
        xcol = data$xcol,
        ycol = data$ycol,
        zcol = species_tmp,
        label = detmaps_sf_data[[data$stationCol]],
        color = hcl.colors(100, "viridis"),
        cex = ifelse(input$scale_size, species_tmp, 10),
        alpha.regions = "alpha",
        layer.name = layer.name
      )
      
      # Add study area if available
      if (!is.null(data$study_area)) {
        map_view <- map_view + 
          mapview::mapview(data$study_area,
                           col.regions = "transparent",
                           color = "red",
                           lwd = 2,
                           layer.name = "Study Area")
      }
      
      # add little black dots at stations with detections
      if (input$species_for_map != "n_species" & !input$scale_size) {
        map_view <- map_view + mapview::mapview(
          detmaps_sf_logi,
          xcol = data$xcol,
          ycol = data$ycol,
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
      map_view@map
    })
    
    
    
    ## Tab: Filter Stations ####
    
    # Store original unfiltered data
    original_data <- reactiveVal(list(
      CTtable_sf = NULL,
      recordTable = NULL,
      aggregated_CTtable = NULL
    ))
    
    # Initialize filtered data reactive value
    filtered_data <- reactiveVal(NULL)
    
    # Reactive value to store active filters
    active_filters <- reactiveVal(list())
    
    # Store original data when app starts
    observe({
      req(data$CTtable_sf, data$recordTable)
      
      if (is.null(original_data()$CTtable_sf)) {
        original_data(list(
          CTtable_sf = data$CTtable_sf,
          recordTable = data$recordTable,
          aggregated_CTtable = data$aggregated_CTtable
        ))
        # Initialize filtered data
        filtered_data(data$CTtable_sf)
      }
    })
    
    # Update column choices
    observe({
      req(data$CTtable_sf)
      updateSelectInput(session, "filterColumn",
                        choices = setdiff(
                          names(sf::st_drop_geometry(data$CTtable_sf)),
                          c(data$stationCol, data$setupCol, data$retrievalCol, "geometry")
                        )
      )
    })
    
    # Dynamic filter controls based on column type
    output$filterControls <- renderUI({
      req(input$filterColumn, data$CTtable_sf)
      column_data <- data$CTtable_sf[[input$filterColumn]]
      
      if (is.numeric(column_data)) {
        # Numeric filter controls
        tagList(
          selectInput("numericOperator", "Condition:",
                      choices = c(
                        "Greater than" = "gt",
                        "Less than" = "lt",
                        "Equal to" = "eq",
                        "Between" = "between"
                      )
          ),
          conditionalPanel(
            condition = "input.numericOperator != 'between'",
            numericInput("numericValue", "Value:", 
                         value = round(mean(column_data, na.rm = TRUE)),
                         min = min(column_data, na.rm = TRUE),
                         max = max(column_data, na.rm = TRUE)
            )
          ),
          conditionalPanel(
            condition = "input.numericOperator == 'between'",
            numericInput("numericValueMin", "Minimum:", 
                         value = round(min(column_data, na.rm = TRUE)),
                         min = min(column_data, na.rm = TRUE),
                         max = max(column_data, na.rm = TRUE)
            ),
            numericInput("numericValueMax", "Maximum:", 
                         value = round(max(column_data, na.rm = TRUE)),
                         min = min(column_data, na.rm = TRUE),
                         max = max(column_data, na.rm = TRUE)
            )
          )
        )
      } else {
        # Categorical filter controls
        unique_values <- sort(unique(as.character(column_data)))
        tagList(
          checkboxGroupInput("categoryValues", "Select values:",
                             choices = unique_values,
                             selected = unique_values
          ),
          shiny::actionButton("selectAllCat", "Select All"),
          shiny::actionButton("deselectAllCat", "Deselect All")
        )
      }
    })
    
    # Handle select/deselect all for categorical filters
    observeEvent(input$selectAllCat, {
      req(input$filterColumn)
      unique_values <- sort(unique(as.character(data$CTtable_sf[[input$filterColumn]])))
      updateCheckboxGroupInput(session, "categoryValues", selected = unique_values)
    })
    
    observeEvent(input$deselectAllCat, {
      updateCheckboxGroupInput(session, "categoryValues", selected = character(0))
    })
    
    
    # Modify the apply filter observer
    observeEvent(input$applyFilter, {
      req(input$filterColumn, data$CTtable_sf)
      
      # Create filter definition
      filter_def <- list(
        column = input$filterColumn,
        type = if(is.numeric(data$CTtable_sf[[input$filterColumn]])) "numeric" else "categorical"
      )
      
      if (filter_def$type == "numeric") {
        filter_def$operator <- input$numericOperator
        if (input$numericOperator == "between") {
          filter_def$value <- c(input$numericValueMin, input$numericValueMax)
        } else {
          filter_def$value <- input$numericValue
        }
      } else {
        filter_def$values <- input$categoryValues
      }
      
      # Apply filter based on data type
      if (filter_def$type == "numeric") {
        if (filter_def$operator == "between") {
          filtered <- data$CTtable_sf[
            data$CTtable_sf[[input$filterColumn]] >= input$numericValueMin & 
              data$CTtable_sf[[input$filterColumn]] <= input$numericValueMax,
          ]
        } else {
          filtered <- switch(input$numericOperator,
                             "gt" = data$CTtable_sf[data$CTtable_sf[[input$filterColumn]] > input$numericValue, ],
                             "lt" = data$CTtable_sf[data$CTtable_sf[[input$filterColumn]] < input$numericValue, ],
                             "eq" = data$CTtable_sf[data$CTtable_sf[[input$filterColumn]] == input$numericValue, ]
          )
        }
      } else {
        filtered <- data$CTtable_sf[data$CTtable_sf[[input$filterColumn]] %in% input$categoryValues, ]
      }
      
      # Safety check - prevent empty filter results
      if (nrow(filtered) == 0) {
        showNotification("Filter would remove all stations! Filter not applied.", type = "error")
        return()
      }
      
      # Store the filter
      current_filters <- active_filters()
      current_filters[[input$filterColumn]] <- filter_def
      active_filters(current_filters)
      
      filtered_data(filtered)
      
      # Apply filters to all relevant data
      filtered_stations <- filtered[[data$stationCol]]
      
      # Filter record table based on filtered stations
      filtered_records <- original_data()$recordTable[
        original_data()$recordTable[[data$stationCol]] %in% filtered_stations, 
      ]
      
      # Update all relevant data
      data$CTtable_sf <- filtered
      data$recordTable <- filtered_records
      data$aggregated_CTtable <- aggregateCTtableByStation(filtered, data$stationCol)
      
      showNotification(sprintf("Filtered to %d stations and %d records", 
                               length(filtered_stations), nrow(filtered_records)), 
                       type = "message"
      )
    })
    
    
    
    # Display active filters
    output$activeFilters <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) {
        return(p("No active filters"))
      }
      
      tagList(
        h4("Active Filters:"),
        div(
          class = "well",
          lapply(names(filters), function(col_name) {
            filter <- filters[[col_name]]
            filter_text <- if (filter$type == "numeric") {
              if (filter$operator == "between") {
                paste("between", filter$value[1], "and", filter$value[2])
              } else {
                paste(
                  switch(filter$operator,
                         "gt" = "greater than",
                         "lt" = "less than",
                         "eq" = "equal to"
                  ),
                  filter$value
                )
              }
            } else {
              paste("is", paste(filter$values, collapse = " OR "))
            }
            
            div(
              style = "margin-bottom: 5px; padding: 5px; background-color: #f8f9fa; border-radius: 4px;",
              tags$b(col_name), ": ", filter_text,
              shiny::actionButton(
                inputId = paste0("remove_", col_name),
                label = "x",
                class = "btn-danger btn-xs",
                style = "margin-left: 10px; padding: 0px 6px;"
              )
            )
          })
        )
      )
    })
    
    
    # Handle individual filter removal
    observe({
      filters <- active_filters()
      lapply(names(filters), function(col_name) {
        observeEvent(input[[paste0("remove_", col_name)]], {
          current_filters <- active_filters()
          current_filters[[col_name]] <- NULL
          active_filters(current_filters)
          
          # Reapply remaining filters
          if (length(current_filters) == 0) {
            filtered_data(data$CTtable_sf)
          } else {
            # Apply all remaining filters
            filtered <- data$CTtable_sf
            for (filter in current_filters) {
              if (filter$type == "numeric") {
                if (filter$operator == "between") {
                  filtered <- filtered[filtered[[filter$column]] >= filter$value[1] & 
                                         filtered[[filter$column]] <= filter$value[2], ]
                } else {
                  filtered <- switch(filter$operator,
                                     "gt" = filtered[filtered[[filter$column]] > filter$value, ],
                                     "lt" = filtered[filtered[[filter$column]] < filter$value, ],
                                     "eq" = filtered[filtered[[filter$column]] == filter$value, ],
                                     filtered
                  )
                }
              } else {
                filtered <- filtered[filtered[[filter$column]] %in% filter$values, ]
              }
            }
            filtered_data(filtered)
          }
          
          showNotification(paste("Filter removed:", col_name), type = "message")
        })
      })
    })
    
    
    
    #  clear filters functionality - only clears filters without restoring data
    observeEvent(input$clearAllFilters, {
      active_filters(list())
      data$CTtable_sf <- original_data()$CTtable_sf
      data$recordTable <- original_data()$recordTable
      data$aggregated_CTtable <- original_data()$aggregated_CTtable
      filtered_data(original_data()$CTtable_sf)
      showNotification("All filters cleared", type = "message")
    })
    
    
    # Render filtered data table
    output$filtered_ct_table <- DT::renderDT({
      req(data$CTtable_sf)
      DT::datatable(
        sf::st_drop_geometry(data$CTtable_sf),
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
    
    # Filter summary
    output$filterSummary <- renderUI({
      req(data$CTtable_sf, data$recordTable, original_data())
      
      orig <- original_data()
      curr_stations <- nrow(data$CTtable_sf)
      orig_stations <- nrow(orig$CTtable_sf)
      curr_records <- nrow(data$recordTable)
      orig_records <- nrow(orig$recordTable)
      
      div(
        class = "well",
        tags$p(
          "Stations: ", 
          tags$strong(curr_stations), " of ", tags$strong(orig_stations),
          sprintf(" (%.1f%%)", curr_stations/orig_stations * 100)
        ),
        tags$p(
          "Records: ",
          tags$strong(curr_records), " of ", tags$strong(orig_records),
          sprintf(" (%.1f%%)", curr_records/orig_records * 100)
        )
      )
    })
    
    
    
    
    # Map output showing filtered vs excluded stations
    output$filterMap <- leaflet::renderLeaflet({
      req(data$CTtable_sf, original_data())
      
      # Get all stations and currently filtered stations
      all_stations <- original_data()$CTtable_sf
      all_stations <- st_transform(all_stations, 4326)
      filtered_stations <- data$CTtable_sf[[data$stationCol]]
      
      # Split into included/excluded stations
      included <- all_stations[all_stations[[data$stationCol]] %in% filtered_stations, ]
      excluded <- all_stations[!all_stations[[data$stationCol]] %in% filtered_stations, ]
      
      # Create map
      map <- leaflet() %>%
        addTiles()
      
      # Add excluded stations
      if(nrow(excluded) >= 1) {
        map <- map %>% 
          addCircleMarkers(
            data = excluded,
            label = ~get(data$stationCol),
            color = "gray",
            fillColor = "gray",
            fillOpacity = 0.3,
            opacity = 0.5,
            radius = 8,
            group = "Excluded stations"
          ) 
      }
      
      # Add included stations
      if(nrow(included) >= 1) {
        map <- map %>% 
          addCircleMarkers(
            data = included,
            label = ~get(data$stationCol),
            color = "blue",
            fillColor = "blue",
            fillOpacity = 0.8,
            opacity = 1,
            radius = 8,
            group = "Included stations"
          )
      }

      # Add layer control
      map <- map %>% addLayersControl(
        overlayGroups = c("Included stations", "Excluded stations"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
      # Add study area if available - add reactive dependency to study_area
      if (!is.null(data$study_area)) {
        study_area_4326 <- st_transform(data$study_area, 4326)
        map <- map %>%
          addPolygons(
            data = study_area_4326,
            fillColor = "transparent",
            color = "red",
            weight = 2,
            group = "Study area"
          )
        
        # Update layers control to include study area
        map <- map %>% addLayersControl(
          overlayGroups = c("Included stations", "Excluded stations", "Study area"),
          options = layersControlOptions(collapsed = FALSE)
        )
      }
      
      
      map
    })
    
    # observer to invalidate the map when study area changes
    observe({
      req(data$study_area)
      leafletProxy("filterMap") %>% clearGroup("Study area")
      if (!is.null(data$study_area)) {
        study_area_4326 <- st_transform(data$study_area, 4326)
        leafletProxy("filterMap") %>%
          addPolygons(
            data = study_area_4326,
            fillColor = "transparent",
            color = "red",
            weight = 2,
            group = "Study area"
          )
      }
    })
    
    ## Tab: Filter records ####
    
    output$camerasIndependentUI <- renderUI({
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        checkboxInput("camerasIndependent", "Cameras are independent", value = FALSE)
      }
    })
    
    # Add this to store the original record table
    original_record_table <- reactiveVal(NULL)
    
    # Initialize the original_record_table when the app starts
    observe({
      req(data$recordTable)
      if (is.null(original_record_table())) {
        original_record_table(data$recordTable)
      }
    })
    
    # Handle the restore button click
    observeEvent(input$restoreOriginalRecordTable, {
      req(original_record_table())
      
      # Restore the original record table
      data$recordTable <- original_record_table()
      
      showNotification("Original record table has been restored", type = "message")
    })
    
    
    
    # Modify the existing filtering logic to use the original_record_table
    observeEvent(input$runTemporalFilter, {
      req(original_record_table())
      
      withProgress(message = 'Applying temporal filter...', value = 0, {
        tryCatch({
          # Prepare the arguments for filterRecordTable
          filter_args <- list(
            recordTable = original_record_table(),  # Use the original table for filtering
            minDeltaTime = input$minDeltaTime,
            deltaTimeComparedTo = input$deltaTimeComparedTo,
            speciesCol = data$speciesCol,
            stationCol = data$stationCol,
            recordDateTimeCol = data$recordDateTimeCol,
            recordDateTimeFormat = data$recordDateTimeFormat,
            removeDuplicateRecords = input$removeDuplicateRecords,
            timeZone = data$timeZone,
            quiet = TRUE
          )
          
          # Add cameraCol and camerasIndependent only if cameraCol is defined
          if (!is.null(data$cameraCol) && data$cameraCol != "") {
            filter_args$cameraCol <- data$cameraCol
            filter_args$camerasIndependent <- input$camerasIndependent
          }
          
          # Add optional arguments only if they are not NULL or empty
          if (!is.null(data$exclude) && length(data$exclude) > 0) {
            filter_args$exclude <- data$exclude
          }
          
          # Apply the filter
          filtered_records <- do.call(filterRecordTable, filter_args)
          
          # Update the record table in the reactive values
          data$recordTable <- filtered_records
          
          showNotification("Temporal filtering completed successfully", type = "message")
        }, error = function(e) {
          showNotification(paste("Error in temporal filtering:", e$message), type = "error")
        })
      })
    })
    
    # Update the filtered record table output
    output$filteredRecordTable <- DT::renderDT({
      req(data$recordTable)
      DT::datatable(
        data$recordTable,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    
    # Tab: extract covariates   ####
    
    
    # Reactive expression for aggregated CT table as sf object
    aggregated_CTtable_sf <- reactive({
      req(data$aggregated_CTtable, data$xcol, data$ycol, data$CTtable_sf)
      sf::st_as_sf(data$aggregated_CTtable, 
                   coords = c(data$xcol, data$ycol), 
                   crs = sf::st_crs(data$CTtable_sf))
    })
    
    
    
    # Function to safely check raster size
    safe_check_raster_size <- function(raster, max_allowed_cells = 2e8) {
      tryCatch({
        raster_size <- terra::ncell(raster)
        raster_size <= max_allowed_cells
      }, error = function(e) {
        warning("Error checking raster size: ", e$message)
        FALSE
      })
    }
    # Function to get color palette
    get_color_palette <- function(palette_name, n = 100, invert = FALSE) {
      colors <- grDevices::hcl.colors(n, palette = palette_name)
      if (invert) {
        colors <- rev(colors)
      }
      colors
    }
    
    # Function to render raster map
    render_raster_map <- function(raster, raster_name, is_prediction = FALSE) {
      req(raster)
      
      # Check if raster is too large to safely process
      if (!safe_check_raster_size(raster)) {
        return(
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::addControl(
              html = "Raster is too large to safely display. Please use a smaller raster.",
              position = "topright"
            )
        )
      }
      
      tryCatch({
        if (is_prediction) {
          req(raster, input$predictionRasterBand, input$colorPalettePrediction)
          color_by <- input$ctColorByPrediction
          point_size <- input$ctPointSizePrediction
          color_palette <- input$colorPalettePrediction
          invert_colors <- input$invertColorsPrediction
          # full_resolution <- input$fullResolutionPrediction
        } else {
          req(raster, input$rasterBand, input$colorPalette)
          # full_resolution <- input$fullResolution
          color_by <- input$ctColorBy
          point_size <- input$ctPointSize
          color_palette <- input$colorPalette
          invert_colors <- input$invertColors
        }
        
        # Get value range from raster
        value_range <- terra::minmax(raster)
        
        # max_pixels <- get_max_pixels(raster, full_resolution)
        n_colors <- 20
        colors <- get_color_palette(color_palette, n = n_colors, invert = invert_colors)
        
        # Use mapview's built-in handling for large rasters
        if(is_prediction) {
          max_pixels <- input$maxPixelsPrediction * 1e6
        } else {
          max_pixels <- input$maxPixels * 1e6
        }
        
        breaks <- seq(value_range[1], value_range[2], length.out = length(colors))
        
        m <- mapview::mapview(raster, 
                              layer.name = raster_name,
                              maxpixels = max_pixels,
                              col.regions = colors,
                              at = breaks)
        
        
        ct_sf <- aggregated_CTtable_sf()
        if (!is.null(ct_sf)) {
          if (color_by == "raster") {
            m <- m + mapview::mapview(ct_sf, 
                                      zcol = raster_name,
                                      col.regions = colors,
                                      at = breaks,
                                      legend = FALSE,
                                      label = data$stationCol,
                                      cex = point_size,
                                      layer.name = "Camera Trap Stations")
          } else {
            m <- m + mapview::mapview(ct_sf, 
                                      col.regions = "white",
                                      color = "black",
                                      label = data$stationCol,
                                      cex = point_size,
                                      layer.name = "Camera Trap Stations")
          }
        } else {
          m <- m + leaflet::addControl(html = "Error: Could not add camera trap stations", position = "topright")
        }
        
        # Add study area if available
        if (!is.null(data$study_area)) {
          m <- m + 
            mapview::mapview(data$study_area,
                             col.regions = "transparent",
                             color = "red",
                             lwd = 2,
                             layer.name = "Study Area")
        }
        
        m@map
      }, error = function(e) {
        leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addControl(html = paste("Error:", e$message), position = "topright")
      })
    }
    
    # Update choices for original raster band selection
    observe({
      if (is.null(data$original_rasters)) {
        updateSelectInput(session, "rasterBand", choices = NULL)
      } else {
        updateSelectInput(session, "rasterBand", choices = names(data$original_rasters))
      }
    })
    
    # Update choices for prediction raster band selection
    observe({
      if (is.null(data$prediction_raster)) {
        updateSelectInput(session, "predictionRasterBand", choices = NULL)
      } else {
        updateSelectInput(session, "predictionRasterBand", choices = names(data$prediction_raster))
      }
    })
    
    # Render original covariate raster map
    output$originalCovariatePlot <- leaflet::renderLeaflet({
      req(data$original_rasters, input$rasterBand)
      selected_raster <- data$original_rasters[[input$rasterBand]]
      render_raster_map(selected_raster, input$rasterBand, is_prediction = FALSE)
    })
    
    # Render prediction raster map
    output$predictionRasterPlot <- leaflet::renderLeaflet({
      req(data$prediction_raster, input$predictionRasterBand)
      selected_raster <- data$prediction_raster[[input$predictionRasterBand]]
      render_raster_map(selected_raster, input$predictionRasterBand, is_prediction = TRUE)
    })
    
    # Observers to re-render maps when relevant inputs change
    observe({
      input$maxPixels
      input$colorPalette
      input$invertColors
      input$ctColorBy
      input$ctPointSize
      input$clearAllCovariates
      leaflet::leafletProxy("originalCovariatePlot") %>% leaflet::clearGroup("all")
    })
    
    observe({
      input$maxPixelsPrediction
      input$colorPalettePrediction
      input$invertColorsPrediction
      input$ctColorByPrediction
      input$ctPointSizePrediction
      input$clearAllCovariates
      leaflet::leafletProxy("predictionRasterPlot") %>% leaflet::clearGroup("all")
    })
    
    
    # Server logic to handle the clipping
    observeEvent(input$preview_extent, {
      req(data$CTtable_sf)
      
      # Create camera grid extent
      if (input$clip_to_grid) {
        # First create overall extent of cameras
        grid_extent <- data$CTtable_sf %>%
          sf::st_union() %>%           # Combine all points
          sf::st_convex_hull() %>%     # Create hull around points
          sf::st_buffer(dist = input$grid_buffer)  # Buffer the hull
      }
      
      # Create study area extent
      if (input$clip_to_study_area) {
        if (is.null(data$study_area)) {
          showNotification("No study area imported yet.", type = "error")
          return()
        }
        study_extent <- data$study_area %>%
          sf::st_buffer(dist = input$study_area_buffer)
      }
      
      # Create final extent
      final_extent <- if (input$clip_to_grid && input$clip_to_study_area) {
        sf::st_intersection(grid_extent, study_extent)
      } else if (input$clip_to_grid) {
        grid_extent
      } else if (input$clip_to_study_area) {
        study_extent
      }
      
      # Preview map
      output$extent_preview <- leaflet::renderLeaflet({
        map_view <- mapview::mapview(final_extent,
                                     col.regions = "transparent",
                                     color = "red",
                                     lwd = 2,
                                     alpha.regions = 0.2,
                                     layer.name = "Prediction Extent")
        
        # Add original camera hull without buffer for reference
        if (input$clip_to_grid) {
          original_hull <- data$CTtable_sf %>%
            sf::st_union() %>%
            sf::st_convex_hull()
          
          map_view <- map_view + 
            mapview::mapview(original_hull,
                             col.regions = "transparent",
                             color = "blue",
                             lwd = 2,
                             layer.name = "Camera Grid Extent")
        }
        
        # Add camera stations
        map_view <- map_view + 
          mapview::mapview(data$CTtable_sf,
                           cex = 3,
                           col.regions = "blue",
                           layer.name = "Camera Stations")
        
        # Add study area if available
        if (!is.null(data$study_area)) {
          map_view <- map_view + 
            mapview::mapview(data$study_area,
                             col.regions = "transparent",
                             color = "darkred",
                             lwd = 2,
                             layer.name = "Study Area")
        }
        
        map_view@map
      })
    })
    
    
    
    # Render updated CT table
    output$updatedCTTable <- DT::renderDT({
      req(data$CTtable_sf)
      DT::datatable(sf::st_drop_geometry(data$CTtable_sf),
                    options = list(scrollX = TRUE))
    })
    
    
    
    # Tab: Elevation & terrain data ----
    
    
    
    # Calculate pixel size at latitude
    calculate_pixel_size <- function(latitude) {
      # At zoom level 12
      # Approximate pixel size = 156543.03 * cos(latitude) / 2^zoom meters
      pixel_size <- 156543.03 * cos(latitude * pi/180) / 2^input$elevationZoom
      return(round(pixel_size, 1))
    }
    
    # Helper function to clip / mask prediction rasters
    clip_prediction_rasters <- function(rasters, prediction_extent) {
      if (is.null(prediction_extent)) return(rasters)
      
      # Transform extent to match raster CRS
      extent_transformed <- sf::st_transform(prediction_extent, terra::crs(rasters))
      
      
      # First crop to bounding box for efficiency
      rasters_cropped <- terra::crop(rasters, terra::vect(extent_transformed))
      
      # Then mask to actual polygon shape
      terra::mask(rasters_cropped, terra::vect(extent_transformed))
    }
    
    
    
    # notify user is study area is not available
    observeEvent(input$predictionExtent, {
      if (input$predictionExtent %in% c("study_area", "intersection") & is.null(data$study_area)) {
        showNotification(
          "Study area data is not available. Please load study area shapefile under 'Import data', or select a different prediction extent.",
          type = "warning"
        )
        # Reset the selection to a default value
        updateSelectInput(session, "predictionExtent", selected = "grid")
      }
    })
    
    
    # Initialize tracking of original columns when CTtable is first loaded
    observeEvent(data$CTtable_sf, {
      if (is.null(data$original_columns)) {
        original_columns_tmp <- names(data$CTtable_sf)
        data$original_columns <- original_columns_tmp[!grepl("geometry", original_columns_tmp)]
        
      }
    })
    
    
    
    # Observer for processing local rasters
    observeEvent(input$processLocalRasters, {
      req(data$CTtable_sf)
      
      withProgress(message = 'Processing local rasters...', value = 0, {
        tryCatch({
          # Prepare basic parameters for createCovariates
          covariate_args <- list(
            CTtable = data$CTtable_sf,
            buffer_ct = input$bufferCT,
            bilinear = input$bilinear
          )
          
          # Add input source parameters
          if (input$inputType == "directory") {
            req(input$directory)
            covariate_args$directory <- input$directory
            covariate_args$recursive <- input$recursive
          } else {
            req(input$filenames)
            covariate_args$filenames <- unlist(strsplit(input$filenames, ",\\s*"))
          }
          
          # Handle raster template/resolution
          if (!is.null(input$rasterTemplate) && !is.null(input$rasterTemplate$datapath)) {
            covariate_args$raster_template <- terra::rast(input$rasterTemplate$datapath)
          } else if (!is.null(input$resolution) && !is.na(input$resolution) && input$resolution > 0) {
            covariate_args$resolution <- input$resolution
          }
          
          # Extract covariates
          covariates_extract_list <- do.call(createCovariates, covariate_args)
          
          # Get prediction extent if clipping is requested
          prediction_extent <- if (input$predictionExtent != "none") {
            get_prediction_extent(
              points_sf = data$CTtable_sf,
              study_area = data$study_area,
              extent_type = input$predictionExtent,
              buffer = input$bufferPrediction
            )
          } else NULL
          
          # Update CTtable
          new_cols <- setdiff(names(covariates_extract_list$CTtable), names(data$CTtable_sf))
          data$CTtable_sf <- cbind(data$CTtable_sf, 
                                   st_drop_geometry(covariates_extract_list$CTtable)[, new_cols, drop = FALSE])
          
          
          # Update original rasters
          data$original_rasters <- c(data$original_rasters, covariates_extract_list$originalRaster)
          
          
          # Only update prediction rasters if they were created
          if (!is.null(covariates_extract_list$predictionRaster)) {
            # Apply clipping if requested and prediction rasters exist
            pred_rasts <- if (input$predictionExtent != "none") {
              prediction_extent <- get_prediction_extent(
                points_sf = data$CTtable_sf,
                study_area = data$study_area,
                extent_type = input$predictionExtent,
                buffer = input$bufferPrediction
              )
              clip_prediction_rasters(covariates_extract_list$predictionRaster, prediction_extent)
            } else {
              covariates_extract_list$predictionRaster
            }
            
            # Update prediction rasters
            data$prediction_raster <- if (is.null(data$prediction_raster)) {
              pred_rasts
            } else {
              c(data$prediction_raster, pred_rasts)
            }
          } else {
            showNotification(
              "No prediction rasters were created because neither a raster template nor resolution was specified. 
            To create prediction rasters, please specify either a template raster or a resolution value.
                  To start over, click 'Clear all covariates'",
              type = "warning",
              duration = 12
            )
          }
          
          
          showNotification("Local rasters processed successfully", type = "message")
        }, error = function(e) {
          showNotification(paste("Error processing local rasters:", e$message), type = "error")
        })
      })
    })
    
    
    
    # Observer for processing elevation data
    observeEvent(input$processElevation, {
      req(data$CTtable_sf)
      
      
      withProgress(message = 'Processing elevation data...', value = 0, {
        tryCatch({
          
          # Create buffered polygon from points
          buffered_sf <- sf::st_buffer(data$CTtable_sf, dist = min(1000, input$bufferPrediction))
          
          # Get bounding box and transform to EPSG:4326 if needed
          if (sf::st_crs(buffered_sf) != sf::st_crs(4326)) {
            buffered_sf_4326 <- sf::st_transform(buffered_sf, 4326)
          } else {
            buffered_sf_4326 <- buffered_sf
          }
          
          elevation_rast <- elevatr::get_elev_raster(
            locations = buffered_sf_4326,
            z = 11,
            source = "aws",
            clip = "bbox"
          )
          
          if(inherits(elevation_rast, "RasterLayer")) elevation_rast <- terra::rast(elevation_rast)
          
          
          # Calculate terrain indices on original resolution data
          terrain_rasts <- list(elevation = elevation_rast)
          
          if ("slope" %in% input$terrainMeasures) {
            terrain_rasts$slope <- terra::terrain(elevation_rast, "slope", unit = "degrees")
          }
          if ("aspect" %in% input$terrainMeasures) {
            terrain_rasts$aspect <- terra::terrain(elevation_rast, "aspect", unit = "degrees")
          }
          if ("TRI" %in% input$terrainMeasures) {
            terrain_rasts$TRI <- terra::terrain(elevation_rast, "TRI")
          }
          if ("TPI" %in% input$terrainMeasures) {
            terrain_rasts$TPI <- terra::terrain(elevation_rast, "TPI")
          }
          if ("roughness" %in% input$terrainMeasures) {
            terrain_rasts$roughness <- terra::terrain(elevation_rast, "roughness")
          }
          
          # Extract values from original resolution rasters
          terrain_values <- terra::extract(
            terra::rast(terrain_rasts),
            sf::st_transform(data$CTtable_sf, terra::crs(elevation_rast))
          )
          
          # Update CTtable (excluding ID column from extract)
          new_cols <- names(terrain_values)[-1]
          data$CTtable_sf <- cbind(data$CTtable_sf, terrain_values[, -1, drop = FALSE])
          
          
          
          # Get the median location of cameras as representative point
          center_lat <- stats::median(st_coordinates(buffered_sf_4326)[, "Y"]) 
          center_lon <- stats::median(st_coordinates(buffered_sf_4326)[, "X"]) 
          
          # derive UTM zone
          utm_epsg <- ifelse(center_lat > 0,
                             32600 + floor((center_lon + 180)/6) + 1,  # Northern
                             32700 + floor((center_lon + 180)/6) + 1)  # Southern
          
          elevation_rast_utm <- terra::project(elevation_rast, paste0("EPSG:", utm_epsg))
          
          
          # Now handle prediction rasters separately
          prediction_rasts <- if (!is.null(input$rasterTemplate) && !is.null(input$rasterTemplate$datapath)) {
            # Use provided template
            template <- terra::rast(input$rasterTemplate$datapath)
            
            resample(rast(terrain_rasts), template)
            
          } else if (!is.null(data$prediction_raster)) {
            # Use existing prediction rasters as template
            resample(rast(terrain_rasts), data$prediction_raster)
            
          } else if (!is.null(input$resolution) && !is.na(input$resolution) && input$resolution > 0) {
            
            # Create new rasters with specified resolution in corresponding UTM zone
            template <- terra::rast(
              extent = terra::ext(elevation_rast_utm),
              resolution = input$resolution,
              crs = terra::crs(elevation_rast_utm)
            )
            
            project(rast(terrain_rasts), template)
            
          } else {
            # Use elevation raster properties as is
            rast(terrain_rasts)
            
          }
          
          # Handle clipping if requested
          if (input$predictionExtent != "none") {
            prediction_extent <- get_prediction_extent(
              points_sf = data$CTtable_sf,
              study_area = data$study_area,
              extent_type = input$predictionExtent,
              buffer = input$bufferPrediction
            )
            
            prediction_rasts <- clip_prediction_rasters(prediction_rasts, prediction_extent)
          }
          
          
          # Update raster collections
          data$original_rasters <- c(data$original_rasters, terrain_rasts)
          data$prediction_raster <- if (is.null(data$prediction_raster)) {
            prediction_rasts
          } else {
            c(data$prediction_raster, prediction_rasts)
          }
          
          showNotification("Elevation data processed successfully", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error processing elevation data:", e$message), type = "error")
        })
      })
    })
    
    
    # Helper function to get prediction extent
    get_prediction_extent <- function(points_sf, study_area, extent_type, buffer) {
      
      if (extent_type == "none") {
        return(NULL)  # No clipping requested
      }
      
      # Create initial extent based on type
      extent <- switch(extent_type,
                       "grid" = st_convex_hull(st_union(points_sf)),
                       "study_area" = study_area,
                       "intersection" = {
                         grid_extent <- st_convex_hull(st_union(points_sf))
                         if (!is.null(study_area)) {
                           st_intersection(grid_extent, study_area)
                         } else {
                           grid_extent
                         }
                       }
      )
      
      # Apply buffer if specified
      if (!is.null(extent) && !is.null(buffer) && buffer > 0) {
        extent <- st_buffer(extent, dist = buffer)
      }
      
      return(extent)
    }
    
    
    # Preview for camera trap buffers
    output$ctBufferPreview <- leaflet::renderLeaflet({
      req(data$CTtable_sf)
      
      # Transform to lat/long for leaflet if needed
      ct_points <- if (sf::st_crs(data$CTtable_sf) != sf::st_crs(4326)) {
        sf::st_transform(data$CTtable_sf, 4326)
      } else {
        data$CTtable_sf
      }
      
      # Create buffer if specified
      buffered_points <- if (input$bufferCT > 0) {
        # Buffer in original CRS for accurate distances, then transform to 4326
        sf::st_transform(
          sf::st_buffer(data$CTtable_sf, dist = input$bufferCT),
          4326
        )
      }
      
      # Create map
      m <- leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = ct_points,
          radius = 2,
          color = "red",
          fillOpacity = 1,
          stroke = FALSE,
          label = ~get(data$stationCol)
        )
      
      # Add buffer if present
      if (input$bufferCT > 0) {
        m <- m %>%
          addPolygons(
            data = buffered_points,
            fillColor = "red",
            fillOpacity = 0.2,
            weight = 1,
            color = "red"
          )
      }
      
      m
    })
    
    # Preview for prediction extent
    output$predictionExtentPreview <- leaflet::renderLeaflet({
      req(data$CTtable_sf)
      
      # Transform camera points to lat/long
      ct_points <- if (sf::st_crs(data$CTtable_sf) != sf::st_crs(4326)) {
        sf::st_transform(data$CTtable_sf, 4326)
      } else {
        data$CTtable_sf
      }
      
      # Base map with camera points
      m <- leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = ct_points,
          radius = 2,
          color = "red",
          fillOpacity = 1,
          stroke = FALSE,
          label = ~get(data$stationCol)
        )
      
      # Get prediction extent
      prediction_extent <- get_prediction_extent(
        points_sf = data$CTtable_sf,
        study_area = data$study_area,
        extent_type = input$predictionExtent,
        buffer = input$bufferPrediction
      )
      
      if (!is.null(prediction_extent)) {
        # Transform to lat/long
        extent_4326 <- sf::st_transform(prediction_extent, 4326)
        
        # For "no clipping" option, show the bounding box
        if (input$predictionExtent == "none") {
          m <- m %>%
            addPolygons(
              data = extent_4326,
              fillColor = "gray",
              fillOpacity = 0.2,
              weight = 2,
              color = "gray",
              label = sprintf("Buffer: %dm", min(1000, input$bufferPrediction))
            )
        } else {
          # Add different colored polygons based on extent type
          extent_color <- switch(input$predictionExtent,
                                 "grid" = "blue",
                                 "study_area" = "green",
                                 "intersection" = "purple"
          )
          
          m <- m %>%
            addPolygons(
              data = extent_4326,
              fillColor = extent_color,
              fillOpacity = 0.2,
              weight = 2,
              color = extent_color
            )
          
          # Show relevant boundaries
          if (input$predictionExtent %in% c("grid", "intersection")) {
            # Show unbuffered grid extent
            grid_extent <- sf::st_transform(
              sf::st_convex_hull(sf::st_union(data$CTtable_sf)),
              4326
            )
            m <- m %>%
              addPolygons(
                data = grid_extent,
                fillColor = "transparent",
                weight = 2,
                color = "blue",
                dashArray = "5,5"
              )
          }
          
          if (input$predictionExtent %in% c("study_area", "intersection")) {
            # Show original study area
            study_area_4326 <- sf::st_transform(data$study_area, 4326)
            m <- m %>%
              addPolygons(
                data = study_area_4326,
                fillColor = "transparent",
                weight = 2,
                color = "green",
                dashArray = "5,5"
              )
          }
        }
      }
      
      m
    })
    
    
    # clear function
    observeEvent(input$clearAllCovariates, {
      if (!is.null(data$CTtable_sf) && !is.null(data$original_columns)) {
        # Keep only the original columns
        data$CTtable_sf <- data$CTtable_sf[, data$original_columns]
      }
      
      # Clear raster collections
      data$original_rasters <- NULL
      data$prediction_raster <- NULL
      
      showNotification("All covariates cleared", type = "message")
    })
    
    
    # Tab: Covariate correlation ----
    
    
    # Function to check package availability
    check_package <- function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        return(FALSE)
      }
      return(TRUE)
    }
    
    # Basic correlation plot using base R
    create_base_correlation_plot <- function(cor_matrix) {
      # Create correlation plot using base R graphics
      old_par <- par(mar = c(8, 8, 2, 2))
      on.exit(par(old_par))
      
      # Create color palette similar to corrplot default
      cols <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                 "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                 "#4393C3", "#2166AC", "#053061"))(200)
      
      # Plot correlation matrix
      image(cor_matrix,
            axes = FALSE,
            col = cols,
            main = "Correlation Matrix")
      
      # Add variable names
      axis(1, at = seq(0, 1, length.out = ncol(cor_matrix)),
           labels = colnames(cor_matrix),
           las = 2)
      axis(2, at = seq(0, 1, length.out = nrow(cor_matrix)),
           labels = rownames(cor_matrix),
           las = 2)
      
      # Add correlation values
      for(i in 1:nrow(cor_matrix)) {
        for(j in 1:ncol(cor_matrix)) {
          if(i != j) {  # Skip diagonal
            text(x = (j-1)/(ncol(cor_matrix)-1),
                 y = (nrow(cor_matrix)-i)/(nrow(cor_matrix)-1),
                 labels = round(cor_matrix[i,j], 2),
                 cex = 0.7)
          }
        }
      }
    }
    
    # Basic scatter plot matrix using base R
    create_base_pairs_plot <- function(data) {
      # pairs(data,
      #       lower.panel = NULL,  # Show only upper panel
      #       upper.panel = function(x, y) {
      #         points(x, y, pch = 20, cex = 0.5)
      #         abline(lm(y ~ x), col = "red", lwd = 1)
      #       })
      
      # First set up the plotting parameters
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      
      # Increase margins and text size
      par(mar = c(4, 4, 1, 1),    # Smaller margins
          cex.axis = 1.2,         # Larger axis text
          cex.labels = 1.2)       # Larger labels
      
      # Create the enhanced pairs plot
      pairs(data,
            lower.panel = NULL,  # Show only upper panel
            upper.panel = function(x, y) {
              # Add points
              points(x, y, pch = 16, cex = 0.8, col = scales::alpha("navy", 0.5))
              
              # Add correlation coefficient
              cor_val <- stats::cor(x, y, method = input$correlationMethod, use = "pairwise.complete.obs")
              text(mean(range(x, na.rm = TRUE)), 
                   mean(range(y, na.rm = TRUE)),
                   round(cor_val, 2),
                   cex = 1.2,
                   col = if(abs(cor_val) > input$correlationThreshold) "red" else "black",
                   font = if(abs(cor_val) > input$correlationThreshold) 2 else 1)
              
              # Add smoothed line
              try({
                smooth <- stats::loess(y ~ x, span = 0.75)
                ord <- order(x)
                lines(x[ord], predict(smooth)[ord], 
                      col = "red", 
                      lwd = 2)
              }, silent = TRUE)
            },
            labels = substring(colnames(data), 1, 15),  # Truncate long names
            gap = 0.3,        # Increase space between plots
            cex.labels = 1.3  # Larger variable labels
      )
    }
    
    # Calculate correlations and update outputs
    observe({
      req(data$CTtable_sf, input$correlationThreshold, input$correlationMethod)
      
      # Get covariates from CTtable
      covariates_df <- sf::st_drop_geometry(data$CTtable_sf)
      
      # Remove non-covariate columns
      exclude_cols <- c(data$stationCol, data$cameraCol, data$setupCol, data$retrievalCol)
      exclude_cols <- c(exclude_cols, 
                        grep("^Problem[0-9]+_(from|to)$", names(covariates_df), value = TRUE))
      covariates_df <- covariates_df[, !names(covariates_df) %in% exclude_cols]
      
      # Handle non-numeric columns based on user preference
      if(input$excludeNonNumeric) {
        numeric_cols <- sapply(covariates_df, is.numeric)
        covariates_df <- covariates_df[, numeric_cols, drop = FALSE]
      } else {
        # Convert factors to numeric if possible
        for(col in names(covariates_df)) {
          if(is.factor(covariates_df[[col]])) {
            if(length(levels(covariates_df[[col]])) >= 2) {
              covariates_df[[col]] <- as.numeric(covariates_df[[col]]) - 1
            }
          }
        }
        # Keep only numeric columns after conversion
        numeric_cols <- sapply(covariates_df, is.numeric)
        covariates_df <- covariates_df[, numeric_cols, drop = FALSE]
      }
      
      # Check if we have at least 2 numeric columns
      if(ncol(covariates_df) < 2) {
        output$correlationWarning <- renderText({
          "Insufficient numeric covariates for correlation analysis"
        })
        return(NULL)
      }
      
      # remove columns with only a single unique value
      n_values_by_column <- sapply(covariates_df, FUN = function (x) length(unique(x)))
      covariates_df <- covariates_df [, n_values_by_column >= 2]
      
      # Calculate correlation matrix
      cor_matrix <- stats::cor(covariates_df, 
                        use = "pairwise.complete.obs", 
                        method = input$correlationMethod)
      
      # Find highly correlated pairs
      high_cor <- which(abs(cor_matrix) > input$correlationThreshold & 
                          abs(cor_matrix) < 1, arr.ind = TRUE)
      
      if(nrow(high_cor) > 0) {
        # Keep only one entry per pair
        high_cor <- high_cor[high_cor[,1] < high_cor[,2], , drop = FALSE]
        
        # Create data frame of highly correlated pairs
        cor_pairs <- data.frame(
          Variable1 = rownames(cor_matrix)[high_cor[,1]],
          Variable2 = rownames(cor_matrix)[high_cor[,2]],
          Correlation = cor_matrix[high_cor],
          stringsAsFactors = FALSE
        )
        
        # Sort by absolute correlation value
        cor_pairs <- cor_pairs[order(abs(cor_pairs$Correlation), decreasing = TRUE),]
        
        # Round correlation values
        cor_pairs$Correlation <- round(cor_pairs$Correlation, 3)
        
        # Update table
        output$correlationTable <- DT::renderDT({
          DT::datatable(cor_pairs,
                        options = list(pageLength = 10,
                                       order = list(list(2, 'desc'))),
                        rownames = FALSE)
        })
        
        # Show warning if highly correlated variables exist
        output$correlationWarning <- renderText({
          paste0("Found ", nrow(cor_pairs), 
                 " pairs of highly correlated variables (|r| > ", 
                 input$correlationThreshold, ")")
        })
      } else {
        output$correlationTable <- DT::renderDT({
          data.frame(
            Variable1 = character(0),
            Variable2 = character(0),
            Correlation = numeric(0)
          )
        })
        
        output$correlationWarning <- renderText({
          paste0("No variable pairs with correlation above threshold (|r| > ", 
                 input$correlationThreshold, ")")
        })
      }
      
      # Check package availability and render appropriate plot
      output$correlationPlot <- renderPlot({
        
        if(input$plotType == "matrix") {
          if(check_package("corrplot")) {
            output$correlationPlotWarning <- renderText(NULL)
            
            corrplot::corrplot(cor_matrix,
                               method = input$corrplotMethod,
                               order = input$corrplotOrder,
                               type = "upper",
                               addCoef.col = "black",
                               tl.col = "black",
                               tl.srt = 45,
                               number.cex = 1,
                               tl.cex = 1,
                               diag = FALSE)
            
          } else {
            output$correlationPlotWarning <- renderText({
              "Note: Using basic correlation plot. Install 'corrplot' package for enhanced visualization."
            })
            create_base_correlation_plot(cor_matrix)
          }
        } else {
          if(check_package("psych")) {
            output$correlationPlotWarning <- renderText(NULL)
            # GGally::ggpairs(covariates_df,
            #                 progress = FALSE) +
            #   theme_bw() +
            #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
            #         strip.background = element_rect(fill = "white"))
            
            # psych::pairs.panels is faster than GGally and provides correlation coefficients,
            # histograms, and scatterplots with smooth lines
            psych::pairs.panels(
              covariates_df,
              method = input$correlationMethod,
              hist.col = "#75AADB",  # Using a nice blue color
              density = TRUE,        # Show density plots on diagonal
              ellipses = TRUE,      # Show correlation ellipses
              smooth = TRUE,        # Add loess smoothers
              cex.cor = 1.3,                    # Default is 1, increased for better readability
              ci = FALSE,                        # Default is FALSE, added confidence intervals
              cex = 1              # point size
              # lwd = 2              # Thicker lines
            )
            
          } else {
            output$correlationPlotWarning <- renderText({
              # "Note: Using basic scatter plot matrix. Install 'GGally' package for enhanced visualization."
              "Note: Using basic scatter plot matrix. Install 'psych' package for enhanced visualization."
            })
            create_base_pairs_plot(covariates_df)
          }
        }
      })
    })
    
    # Tab: detectionHistory     ####
    
    
    
    # container for saving reactive objects
    my_object <- shiny::reactiveValues()
    
    
    detection_hist <- reactive({
      req(input$species_dethist, input$occasionLength_single_species, input$outputType, input$day1)
      
      # Check if current_species_list() is a vector
      if (!is.vector(current_species_list())) {
        warning("current_species_list() is not a vector")
        return(NULL)
      }
      
      # Check if selected species is in the list
      if (!(input$species_dethist %in% current_species_list())) {
        warning("Selected species not in current species list")
        return(NULL)
      }
      
      tryCatch({
        detectionHistory(
          recordTable = data$recordTable,
          camOp = camop(),
          species = input$species_dethist,
          occasionLength = input$occasionLength_single_species,
          day1 = input$day1,
          output = input$outputType,
          scaleEffort = FALSE,
          timeZone = data$timeZone,
          stationCol = data$stationCol,
          speciesCol = data$speciesCol,
          recordDateTimeCol = data$recordDateTimeCol,
          recordDateTimeFormat = data$recordDateTimeFormat
        )
      }, error = function(e) {
        warning("Error in detectionHistory: ", e$message)
        NULL
      })
    })
    
    # get detection history
    dh1_df <- shiny::reactive({
      req(detection_hist())
      df <- as.data.frame(detection_hist()[[1]])
      df[, data$stationCol] <- rownames(df)
      df <- reshape2::melt(df, id.vars = data$stationCol)
      colnames(df)[colnames(df) == "variable"] <- "Occasion"
      colnames(df)[colnames(df) == "value"] <- "Detection"
      df
    })
    
    # get effort matrix
    dh2_df <- shiny::reactive({
      req(detection_hist())
      df <- as.data.frame(detection_hist()[[2]])
      df[, data$stationCol] <- rownames(df)
      df <- reshape2::melt(df, id.vars = data$stationCol)
      colnames(df)[colnames(df) == "variable"] <- "Occasion"
      colnames(df)[colnames(df) == "value"] <- "Effort"
      df
    })
    
    
    output$detectionHistory <- plotly::renderPlotly({
      req(dh1_df(), dh2_df())
      
      fig1 <- plotly::layout(plotly::plot_ly(dh1_df(),
                                             x = ~Occasion, y = ~get(data$stationCol), z = ~Detection,
                                             type = "heatmap",
                                             colors = hcl.colors(length(unique(na.omit(dh1_df()$Detection))),
                                                                 palette = "Blu-Yl", rev = TRUE)
      ),
      yaxis = list(
        title = data$stationCol,
        categoryorder = "category descending"
      )
      )
      
      fig2 <- plotly::layout(plotly::plot_ly(dh2_df(),
                                             x = ~Occasion, y = ~get(data$stationCol), z = ~Effort,
                                             type = "heatmap",
                                             colors = hcl.colors(10, palette = "viridis", rev = TRUE)
      ),
      yaxis = list(
        title = data$stationCol,
        categoryorder = "category descending"
      )
      )
      
      plotly::layout(p = plotly::subplot(fig1, fig2,
                                         shareY = TRUE, shareX = TRUE,
                                         titleX = TRUE, titleY = TRUE
      ),
      annotations = list(
        list(x = 0.2, y = 1.05, text = "Detections", showarrow = FALSE, xref = "paper", yref = "paper"),
        list(x = 0.8, y = 1.05, text = "Effort", showarrow = FALSE, xref = "paper", yref = "paper")
      )
      )
    })
    
    output$dethist_n_records <- shinydashboard::renderValueBox({
      req(data$recordTable, input$species_dethist)
      shinydashboard::valueBox(
        value = sum(data$recordTable[, data$speciesCol] == input$species_dethist),
        subtitle = "Records",
        icon = shiny::icon("hashtag")
      )
    })
    
    output$dethist_n_detections <- shinydashboard::renderValueBox({
      req(dh1_df())
      shinydashboard::valueBox(
        value = sum(dh1_df()$Detection, na.rm = TRUE),
        subtitle = "Detections in matrix",
        icon = shiny::icon("filter")
      )
    })
    
    output$dethist_n_stations <- shinydashboard::renderValueBox({
      req(dh1_df(), data$stationCol)
      shinydashboard::valueBox(
        value = sum(tapply(dh1_df()$Detection, INDEX = dh1_df()[, data$stationCol], sum, na.rm = TRUE) >= 1),
        subtitle = "Stations with detections",
        icon = shiny::icon("location-dot")
      )
    })
    
    output$dethist_percent_1s <- shinydashboard::renderValueBox({
      req(dh1_df())
      shinydashboard::valueBox(
        value = paste(round(mean(dh1_df()$Detection >= 1, na.rm = TRUE) * 100, 1), "%"),
        subtitle = "Occasions with detection",
        icon = shiny::icon("percent")
      )
    })
    
    shiny::observeEvent(input$return_dethist, {
      object_name <- paste0("dethist", "_", 
                            gsub(" ", "_", input$species_dethist), "_",
                            input$occasionLength_single_species, "d", "_", 
                            input$outputType, "_",
                            "day1_", input$day1)
      
      assign_to_global_dh <- function(pos=1) {
        assign(object_name, detection_hist(), envir=as.environment(pos))
      }
      
      assign_to_global_dh()
      
      shiny::showModal(shiny::modalDialog(
        title = "Object Saved",
        paste("The detection history object has been saved to the workspace as:", object_name)
      ))
    })
    
    
    
    # Tab: Occupancy  ----
    
    ##  General ----
    
    # Observer to update covariate choices for all occupancy workflows
    observe({
      req(data$CTtable_sf)
      
      # Get all column names
      all_cols <- names(sf::st_drop_geometry(data$CTtable_sf))
      
      # Exclude specific columns
      exclude_cols <- c(data$stationCol, data$cameraCol, data$setupCol, data$retrievalCol)
      exclude_cols <- c(exclude_cols, grep("^Problem[0-9]+_(from|to)$", all_cols, value = TRUE))
      
      # Filter covariate choices
      covariate_choices <- setdiff(all_cols, exclude_cols)
      
      # Update choices for basic single-species workflow
      updateSelectizeInput(session, "basic_det_covs", choices = covariate_choices, server = TRUE)
      updateSelectizeInput(session, "basic_occ_covs", choices = covariate_choices, server = TRUE)
      
      # Update choices for advanced single-species workflow
      updateSelectizeInput(session, "detectionCovariate", choices = c("Choose covariate" = "", covariate_choices), server = TRUE)
      updateSelectizeInput(session, "occupancyCovariate", choices = c("Choose covariate" = "", covariate_choices), server = TRUE)
      
      # Update choices for community occupancy workflow
      # Detection covariates
      updateSelectizeInput(session, "detCovFixed", choices = covariate_choices, server = TRUE)
      updateSelectizeInput(session, "detCovRanef", choices = covariate_choices, server = TRUE)
      updateSelectizeInput(session, "detCovIndep", choices = covariate_choices, server = TRUE)
      
      # Occupancy covariates
      updateSelectizeInput(session, "occuCovFixed", choices = covariate_choices, server = TRUE)
      updateSelectizeInput(session, "occuCovRanef", choices = covariate_choices, server = TRUE)
      updateSelectizeInput(session, "occuCovIndep", choices = covariate_choices, server = TRUE)
    })
    
    
    # set default number of cores to use for ubms
    
    output$basic_ubms_cores_input <- renderUI({
      max_cores <- min(parallel::detectCores() - 1, 7)
      safe_cores <- min(max_cores, input$basic_ubms_chains)
      
      numericInput("basic_ubms_cores", "Number of cores:", 
                   value = safe_cores,
                   min = 1, 
                   max = safe_cores)
    })
    
    output$adv_ubms_cores_input <- renderUI({
      max_cores <- min(parallel::detectCores() - 1, 7)
      safe_cores <- min(max_cores, input$adv_ubms_chains)
      
      numericInput("adv_ubms_cores", "Number of cores:", 
                   value = safe_cores,
                   min = 1, 
                   max = safe_cores)
    })
    
    
    # Create unmarked frame
    umf <- reactive({
      req(detection_hist())
      
      # Create unmarkedFrameOccu from detection history
      # detection_hist() returns a list with [[1]] = detection matrix, [[2]] = effort matrix
      tryCatch({
        unmarked::unmarkedFrameOccu(
          y = detection_hist()[[1]],          # Detection/non-detection matrix
          siteCovs = data$aggregated_CTtable, # Site covariates
          obsCovs = list(                     # Observation covariates
            effort = detection_hist()[[2]]     # Effort matrix from detection history
          )
        )
      }, error = function(e) {
        showNotification(paste("Error creating unmarkedFrame:", e$message), type = "error")
        return(NULL)
      })
    })
    
    # Add summary output for unmarkedFrame
    output$umf_summary <- renderPrint({
      req(umf())
      summary(umf())
    })
    
    # Add action button to return unmarkedFrame object
    observeEvent(input$return_umf, {
      object_name <- paste0("umf", "_", 
                            gsub(" ", "_", input$species_dethist), "_",
                            input$occasionLength_single_species, "d", "_", 
                            input$outputType, "_",
                            "day1_", input$day1)
      
      assign_to_global_umf <- function(pos=1) {
        umf_export <- umf()
        assign(object_name, umf_export, envir=as.environment(pos))
      }
      
      assign_to_global_umf()
      
      showModal(modalDialog(
        title = "Object Saved",
        paste("The unmarkedFrame has been saved to the workspace as:", object_name)
      ))
    })
    
    
    # Helper function to get available covariates
    getAvailableCovariates <- function(data, stationCol, cameraCol, setupCol, retrievalCol) {
      req(data)
      
      # Get all column names
      all_cols <- names(sf::st_drop_geometry(data))
      
      # Exclude specific columns
      exclude_cols <- c(stationCol, cameraCol, setupCol, retrievalCol)
      exclude_cols <- c(exclude_cols, grep("^Problem[0-9]+_(from|to)$", all_cols, value = TRUE))
      
      # Return filtered choices
      setdiff(all_cols, exclude_cols)
    }
    
    # create UI elements for covariate choice
    output$detectionCovariatesUI <- renderUI({
      covariate_choices <- getAvailableCovariates(
        data = data$CTtable_sf,
        stationCol = data$stationCol,
        cameraCol = data$cameraCol,
        setupCol = data$setupCol,
        retrievalCol = data$retrievalCol
      )
      
      selectInput("detectionCovariate", 
                  "Select covariate:", 
                  choices = c("Choose covariate" = "", covariate_choices),
                  selected = ""
      )
    })
    
    
    output$occupancyCovariatesUI <- renderUI({
      covariate_choices <- getAvailableCovariates(
        data = data$CTtable_sf,
        stationCol = data$stationCol,
        cameraCol = data$cameraCol,
        setupCol = data$setupCol,
        retrievalCol = data$retrievalCol
      )
      
      selectInput("occupancyCovariate", 
                  "Select covariate:", 
                  choices = c("Choose covariate" = "", covariate_choices),
                  selected = ""
      )
    })
    
    
    # dynamically set the layer names to plot in the UI
    output$basic_prediction_layer_choices <- renderUI({
      layer_choices <- if(input$basic_model_package == "unmarked") {
        c("Occupancy probability" = "Predicted",
          "Standard error" = "SE",
          "Lower CI" = "lower",
          "Upper CI" = "upper")
      } else {
        c("Occupancy probability" = "Predicted",
          "Standard deviation" = "SD",  # we keep "SE" as internal value for consistency
          "Lower CI" = "Lower",
          "Upper CI" = "Upper")
      }
      
      selectInput("basic_prediction_layer", 
                  "Select prediction layer:",
                  choices = layer_choices,
                  selected = "Predicted")
    })
    
    # dynamically set the layer names to plot in the UI
    output$adv_prediction_layer_choices <- renderUI({
      layer_choices <- if(input$adv_model_package == "unmarked") {
        c("Occupancy probability" = "Predicted",
          "Standard error" = "SE",
          "Lower CI" = "lower",
          "Upper CI" = "upper")
      } else {
        c("Occupancy probability" = "Predicted",
          "Standard deviation" = "SE",  # we keep "SE" as internal value for consistency
          "Lower CI" = "lower",
          "Upper CI" = "upper")
      }
      
      selectInput("adv_prediction_layer", 
                  "Select prediction layer:",
                  choices = layer_choices,
                  selected = "Predicted")
    })
    
    
    
    # Helper function to update prediction maps
    updatePredictionMap <- function(map_id, predictions, input_prefix) {
      output[[map_id]] <- leaflet::renderLeaflet({
        # Get current layer based on user selection
        layer_input <- paste0(input_prefix, "_prediction_layer")
        palette_input <- paste0(input_prefix, "_predictionColorPalette")
        invert_input <- paste0(input_prefix, "_invertPredictionColors")
        # raster01 <- paste(input_prefix, "_basic_raster01")
        
        # Select appropriate layer from predictions
        current_layer <- predictions[[input[[layer_input]]]]
        
        # Get color palette
        colors <- get_color_palette(
          input[[palette_input]], 
          n = 100, 
          invert = input[[invert_input]]
        )
        
        # Create base map
        m <- mapview::mapview(
          current_layer,
          layer.name = paste0(input[[layer_input]], " - ", names(current_layer)),
          col.regions = colors,
          na.color = "transparent"
        )
        
        # Add study area if available
        if (!is.null(data$study_area)) {
          m <- m + mapview::mapview(
            data$study_area,
            col.regions = "transparent",
            color = "red",
            lwd = 2,
            layer.name = "Study Area"
          )
        }
        
        m@map
      })
    }
    
    # Helper function for color palettes
    get_color_palette <- function(palette_name, n = 100, invert = FALSE) {
      colors <- switch(palette_name,
                       "Viridis" = viridisLite::viridis(n),
                       "Plasma" = viridisLite::plasma(n),
                       "Inferno" = viridisLite::inferno(n),
                       "Rocket" = viridisLite::rocket(n),
                       viridisLite::viridis(n)  # default to viridis
      )
      
      if (invert) {
        colors <- rev(colors)
      }
      
      return(colors)
    }
    
    
    # New reactive values to track models for each workflow
    basic_model <- reactiveVal(NULL)
    advanced_model <- reactiveVal(NULL)
    
    
    # Basic workflow state tracking and clearing
    observeEvent(c(
      input$basic_model_package,
      input$basic_model_type,
      input$species_dethist,
      input$occasionLength_single_species,
      input$day1,
      input$outputType
    ), {
      # Clear the model if it exists
      if (!is.null(basic_model()) || length(my_object$basic_modList) > 0) {
        basic_model(NULL)
        my_object$basic_modList <- list()
        output$basic_model_selection <- renderTable({ NULL })
        output$basic_prediction_map <- leaflet::renderLeaflet({ NULL })
        shiny::showNotification("Basic model cleared due to input changes", type = "warning")
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Advanced workflow state tracking and clearing
    observeEvent(c(
      input$adv_model_package,
      input$adv_model_type,
      input$species_dethist,
      input$occasionLength_single_species,
      input$day1,
      input$outputType
    ), {
      # Clear the model if it exists
      if (!is.null(advanced_model()) || length(my_object$adv_modList) > 0) {
        advanced_model(NULL)
        my_object$adv_modList <- list()
        output$adv_model_selection <- renderTable({ NULL })
        output$adv_prediction_map <- leaflet::renderLeaflet({ NULL })
        shiny::showNotification("Advanced model cleared due to input changes", type = "warning")
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Clear both models when data changes
    observeEvent(c(data$CTtable_sf, data$recordTable), {
      
      something_to_clear <- FALSE
      if (!is.null(basic_model()) || length(my_object$basic_modList) > 0) {
        something_to_clear <- TRUE
        basic_model(NULL)
        my_object$basic_modList <- list()
        output$basic_model_selection <- renderTable({ NULL })
        output$basic_prediction_map <- leaflet::renderLeaflet({ NULL })
      }
      if (!is.null(advanced_model()) || length(my_object$adv_modList) > 0) {
        something_to_clear <- TRUE
        advanced_model(NULL)
        my_object$adv_modList <- list()
        output$adv_model_selection <- renderTable({ NULL })
        output$adv_prediction_map <- leaflet::renderLeaflet({ NULL })
      }
      if(something_to_clear) shiny::showNotification("Models and predictions cleared due to data changes", type = "warning")
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    
    ## Basic workflow server logic   ----
    
    observeEvent(input$basic_run_model, {
      req(umf())
      
      # Create detection formula
      det_formula <- {
        covs <- if (input$basic_scale_covariates && length(input$basic_det_covs) > 0) {
          paste0("scale(", input$basic_det_covs, ")", collapse = " + ")
        } else {
          paste(input$basic_det_covs, collapse = " + ")
        }
        
        effort_term <- if (input$basic_effort_on_detection) {
          if (input$basic_scale_covariates) "scale(effort)" else "effort"
        } else {
          NULL
        }
        
        if (covs != "" && !is.null(effort_term)) paste(covs, effort_term, sep = " + ")
        else if (covs != "") covs
        else if (!is.null(effort_term)) effort_term
        else "1"
      }
      
      # Create occupancy formula
      occ_formula <- {
        if (length(input$basic_occ_covs) == 0) "1"
        else if (input$basic_scale_covariates) 
          paste0("scale(", input$basic_occ_covs, ")", collapse = " + ")
        else 
          paste(input$basic_occ_covs, collapse = " + ")
      }
      
      # Combine formulas
      formula_tmp <- stats::formula(paste0("~", det_formula, " ~", occ_formula))
      
      withProgress(message = 'Fitting basic model...', value = 0, {
        tryCatch({
          model <- switch(
            paste(input$basic_model_package, input$basic_model_type, sep = "_"),
            "unmarked_Occupancy" = unmarked::occu(formula = formula_tmp, data = umf()),
            "unmarked_Royle-Nichols" = unmarked::occuRN(formula = formula_tmp, data = umf()),
            "ubms_Occupancy" = ubms::stan_occu(
              formula = formula_tmp, 
              data = umf(), 
              chains = input$basic_ubms_chains, 
              iter = input$basic_ubms_iter, 
              warmup = floor(input$basic_ubms_iter/2), 
              thin = input$basic_ubms_thin, 
              cores = input$basic_ubms_cores,
              refresh = 0
            ),
            "ubms_Royle-Nichols" = ubms::stan_occuRN(
              formula = formula_tmp, 
              data = umf(), 
              chains = input$basic_ubms_chains, 
              iter = input$basic_ubms_iter, 
              warmup = floor(input$basic_ubms_iter/2), 
              thin = input$basic_ubms_thin, 
              cores = input$basic_ubms_cores,
              refresh = 0
            )
          )
          
          # Store the model in basic workflow reactive
          basic_model(model)
          shiny::showNotification("Basic model fitted successfully", type = "message")
          
        }, error = function(e) {
          shiny::showNotification(paste("Error fitting basic model:", e$message), type = "error")
        })
      })
    })
    
    ## Advanced workflow server logic  ----
    observeEvent(input$runAdvancedModel, {
      req(umf(), modelEffects())
      
      # Generate formula using existing formula generation function
      formula_tmp <- generateFormula(modelEffects(), input$adv_model_package)
      
      withProgress(message = 'Fitting advanced model...', value = 0, {
        tryCatch({
          model <- switch(
            paste(input$adv_model_package, input$adv_model_type, sep = "_"),
            "unmarked_Occupancy" = unmarked::occu(formula = formula_tmp, data = umf()),
            "unmarked_Royle-Nichols" = unmarked::occuRN(formula = formula_tmp, data = umf()),
            "ubms_Occupancy" = ubms::stan_occu(
              formula = formula_tmp, 
              data = umf(), 
              chains = input$adv_ubms_chains, 
              iter = input$adv_ubms_iter, 
              warmup = floor(input$adv_ubms_iter/2), 
              thin = input$adv_ubms_thin, 
              cores = input$adv_ubms_cores,
              refresh = 0
            ),
            "ubms_Royle-Nichols" = ubms::stan_occuRN(
              formula = formula_tmp, 
              data = umf(), 
              chains = input$adv_ubms_chains, 
              iter = input$adv_ubms_iter, 
              warmup = floor(input$adv_ubms_iter/2), 
              thin = input$adv_ubms_thin, 
              cores = input$adv_ubms_cores,
              refresh = 0
            )
          )
          
          # Store the model in advanced workflow reactive
          advanced_model(model)
          shiny::showNotification("Advanced model fitted successfully", type = "message")
          
        }, error = function(e) {
          shiny::showNotification(paste("Error fitting advanced model:", e$message), type = "error")
        })
      })
    })
    
    
    
    ## Model summaries ----
    output$basic_model_summary <- renderPrint({
      req(basic_model())
      if (input$basic_model_package == "unmarked") {
        summary(basic_model())
      } else {
        print(basic_model())
      }
    })
    
    output$adv_model_summary <- renderPrint({
      req(advanced_model())
      if (input$adv_model_package == "unmarked") {
        summary(advanced_model())
      } else {
        print(advanced_model())
      }
    })
    
    
    ## Parameter estimates   ----
    
    
    
    # Basic workflow parameter estimates
    output$basic_coef_det_header <- renderText({
      "Detection submodel:"
    })
    
    output$basic_coef_det <- renderPrint({
      req(basic_model())
      if (input$basic_model_package == "unmarked") {
        ci <- round(unmarked::confint(basic_model(), 
                                      type = "det", 
                                      level = input$basic_pval),
                    input$basic_digits)
        ci
        # cbind(ci, 
        #       Significant = ifelse(apply(confint(basic_model(), type = "det"), 
        #                                  1, 
        #                                  FUN = function(x) all(x < 0) | all(x > 0)), "*", ""))
      } else {
        round(summary(basic_model(), "det")[, c("2.5%", "97.5%")], input$basic_digits)
      }
    })
    
    output$basic_coef_state_header <- renderText({
      "Occupancy submodel:"
    })
    
    output$basic_coef_state <- renderPrint({
      req(basic_model())
      if (input$basic_model_package == "unmarked") {
        ci <- round(unmarked::confint(basic_model(), type = "state", level = input$basic_pval), input$basic_digits)
        ci
        # cbind(ci, 
        #       Significant = ifelse(apply(confint(basic_model(), type = "state"), 
        #                                  1, 
        #                                  FUN = function(x) all(x < 0) | all(x > 0)), "*", ""))
      } else {
        round(summary(basic_model(), "state")[, c("2.5%", "97.5%")], input$basic_digits)
      }
    })
    
    
    # Advanced workflow parameter estimates
    output$adv_coef_det_header <- renderText({
      "Detection submodel:"
    })
    
    output$adv_coef_det <- renderPrint({
      req(advanced_model())
      if (input$adv_model_package == "unmarked") {
        round(unmarked::confint(advanced_model(), type = "det", level = input$adv_pval), input$adv_digits)
      } else {
        round(summary(advanced_model(), "det")[, c("2.5%", "97.5%")], input$adv_digits)
      }
    })
    
    output$adv_coef_state_header <- renderText({
      "Occupancy submodel:"
    })
    
    output$adv_coef_state <- renderPrint({
      req(advanced_model())
      if (input$adv_model_package == "unmarked") {
        round(unmarked::confint(advanced_model(), type = "state", level = input$adv_pval), input$adv_digits)
      } else {
        round(summary(advanced_model(), "state")[, c("2.5%", "97.5%")], input$adv_digits)
      }
    })
    
    
    # Response plots  ----
    
    # Helper function to convert symbols to character
    symbols_to_char_or_null <- function(x) {
      if (is.null(x) || length(x) == 0) return(NULL)
      sapply(x, as.character)
    }
    
    
    # Response plots - Basic workflow
    output$basic_response_plot <- renderPlot({
      req(basic_model())
      
      # Get current model and plot type
      model <- basic_model()
      is_detection <- input$basic_plot_type == "Detection covariates"
      
      # Get covariates based on plot type
      # covs <- if(is_detection) input$basic_det_covs else input$basic_occ_covs
      if(is_detection) {
        covs <- c(symbols_to_char_or_null(input$basic_det_covs), if(input$basic_effort_on_detection) "effort")
      } else {
        covs <- c(symbols_to_char_or_null(input$basic_occ_covs))
      }
      
      if (length(covs) == 0) {
        return(NULL)
      }
      
      type <- if(is_detection) "det" else "state"
      ylab <- if(is_detection) {
        "Detection probability"
      } else {
        if(input$basic_model_type == "Occupancy") {
          "Occupancy probability"
        } else {
          "Number of animals"
        }
      }
      
      if (input$basic_model_package == "unmarked") {
        # Create plot data
        plot_data <- lapply(covs, function(i) {
          unmarked::plotEffectsData(model, 
                                    covariate = i, 
                                    type = type,
                                    level = input$basic_ci_level)
        })
        
        # Create plots
        plots <- lapply(seq_along(plot_data), function(i) {
          x <- plot_data[[i]]
          p <- ggplot(x, aes(x = covariateValue, y = Predicted)) 
          
          if (is.numeric(x$covariateValue)) {
            p <- p +
              geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
              geom_line(linewidth = 1)
          }
          if (is.factor(x$covariateValue)) {
            p <- p +
              geom_pointrange(aes(ymin = lower, ymax = upper), fatten = 5, linewidth = 1)
          }
          
          p <- p + 
            theme_bw() +
            theme(panel.grid.minor = element_blank()) +
            labs(title = covs[i],
                 y = ylab,
                 x = covs[i])
          
          if(input$basic_model_type == "Occupancy") p <- p + ylim(0,1) 
          
          p
        })
        
        # Arrange plots
        # do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
        Reduce("+", plots)
        
      } else {
        # For ubms models, use built-in plotting
        submodel <- if(is_detection) "det" else "state"
        ubms::plot_marginal(model, 
                      submodel = submodel)
      }
    })
    
    # Response plots - Advanced workflow
    output$adv_response_plot <- renderPlot({
      req(advanced_model())
      
      # Get current model and effect
      model <- advanced_model()
      effect <- input$adv_plot_effect
      
      if (is.null(effect)) {
        return(NULL)
      }
      
      if (input$adv_model_package == "unmarked") {
        # Create plot data with effect type handling
        plot_data <- createAdvancedEffectPlot(
          model = model,
          effect = effect,
          submodel = tolower(input$adv_plot_submodel),
          ci_level = input$adv_ci_level,
          show_data = input$adv_show_data
        )
        
        print(plot_data)
        
      } else {
        # For ubms models
        ubms::plot_marginal(model, 
                      submodel = tolower(input$adv_plot_submodel),
                      param = effect)
      }
    })
    
    # Helper function for advanced effect plots
    createAdvancedEffectPlot <- function(model, effect, submodel, ci_level = 0.95, show_data = TRUE) {
      # Implementation depends on effect type (linear, quadratic, interaction)
      # This is a placeholder - implement based on your specific needs
    }
    
    
    # Spatial predictions  ----
    
    # Spatial predictions - Basic workflow
    observeEvent(input$basic_run_prediction, {
      req(basic_model())
      
      
      # Get prediction raster
      pred_raster <- if (input$basic_pred_source == "extracted") {
        req(data$prediction_raster, 
            message = "No extracted covariates available. Please extract covariates first.")
        data$prediction_raster
      } else {
        req(input$basic_custom_raster,
            message = "Please upload a custom raster.")
        terra::rast(input$basic_custom_raster$datapcath)
      }
      
      # ensure that all cells have NA at the same locations
      na_mask <- !is.na(prod(pred_raster))
      pred_raster <- mask(pred_raster, na_mask, maskvalues = FALSE)
      
      
      withProgress(message = 'Generating predictions...', value = 0, {
        tryCatch({
          # Generate predictions based on model type
          if (input$basic_model_package == "unmarked") {
            predictions <- predict(basic_model(), 
                                   type = input$basic_pred_type,
                                   newdata = pred_raster)
          } else {
            pred_raster <- raster::stack(pred_raster)
            
            predictions <- ubms::predict(object = basic_model(),
                                         submodel = input$basic_pred_type,
                                         newdata = pred_raster)
            predictions <- rast(predictions)
          }
          
          # Update map
          updatePredictionMap("basic_prediction_map", predictions, "basic")
          
        }, error = function(e) {
          showNotification(paste("Error generating predictions:", e$message), type = "error")
        })
      })
    })
    
    # Spatial predictions - Advanced workflow
    observeEvent(input$adv_run_prediction, {
      req(advanced_model())
      
      # Similar to basic workflow but with additional MCMC handling for ubms models
      # Implementation follows same pattern as basic predictions with added complexity
    })
    
    
    # Update MCMC diagnostics for ubms models
    observe({
      req(advanced_model(), input$adv_model_package == "ubms")
      
      # Get parameter names
      params <- rownames(summary(advanced_model())$summary)
      
      # Update parameter selection dropdown
      updateSelectInput(session, "adv_parameter_trace",
                        choices = params)
    })
    
    # Render trace plots for UBMS models
    output$adv_trace_plot <- renderPlot({
      req(advanced_model(), 
          input$adv_model_package == "ubms",
          input$adv_parameter_trace)
      
      # Create trace plot for selected parameter
      bayesplot::mcmc_trace(advanced_model(),
                            pars = input$adv_parameter_trace)
    })
    
    
    
    
    # Model selection table updates
    
    observeEvent(input$basic_add_to_modsel, {
      req(basic_model())
      my_object$basic_modList <- c(my_object$basic_modList, list(basic_model()))
    })
    
    observeEvent(input$basic_clear_modsel, {
      my_object$basic_modList <- list()
    })
    
    output$basic_model_selection <- renderTable({
      req(length(my_object$basic_modList) > 0)
      createModelSelectionTable(my_object$basic_modList, input$basic_model_package)
    })
    
    output$adv_model_selection <- renderTable({
      req(length(my_object$adv_modList) > 0)
      createModelSelectionTable(my_object$adv_modList, input$adv_model_package)
    })
    
    # Helper function for model selection tables
    createModelSelectionTable <- function(model_list, package) {
      if (package == "unmarked") {
        fl <- unmarked::fitList(fits = model_list, autoNames = "formula")
        ms <- unmarked::modSel(fl)
        df_ms <- round(ms@Full[, c("nPars", "AIC", "delta", "AICwt", "cumltvWt", "Rsq")], 2)
        df_ms <- cbind(formula = ms@Full$formula, df_ms)
      } else {
        fl <- ubms::fitList(fits = model_list)
        ms <- ubms::modSel(fl)
        df_ms <- round(ms@Full[, c("nPars", "WAIC", "delta", "weight")], 2)
        df_ms <- cbind(formula = ms@Full$formula, df_ms)
      }
      return(df_ms)
    }
    
    
    
    
    # Tab: Multi-species occupancy   ----
    
    
    ### Functions to prepare covariates and prediction raster ----
    
    # Function to scale numeric columns and store scaling parameters
    scale_covariates <- function(df) {
      # Identify numeric columns
      numeric_cols <- sapply(df, function(x) {
        is.numeric(x) && !all(is.na(x))
      })
      
      
      # Initialize lists to store scaling parameters
      scaling_params <- list(
        means = list(),
        sds = list()
      )
      
      # Create scaled version of the dataframe
      df_scaled <- df
      
      # Scale each numeric column and store parameters
      for (col in names(df)[numeric_cols][-1]) {
        # Calculate mean and sd
        col_mean <- mean(df[[col]], na.rm = TRUE)
        col_sd <- sd(df[[col]], na.rm = TRUE)
        
        # Store parameters
        scaling_params$means[[col]] <- col_mean
        scaling_params$sds[[col]] <- col_sd
        
        # Scale the column
        if (col_sd > 0) {  # Only scale if there's variation
          df_scaled[[col]] <- (df[[col]] - col_mean) / col_sd
        } else {
          df_scaled[[col]] <- df[[col]] - col_mean
        }
      }
      
      return(list(
        scaled_data = df_scaled,
        scaling_params = scaling_params
      ))
    }
    
    
    
    # Function to scale a prediction raster using stored parameters
    scale_prediction_raster <- function(pred_raster, scaling_params) {
      # Create a copy of the input raster
      scaled_raster <- pred_raster
      
      # Scale each layer that has corresponding parameters
      for (layer_name in names(pred_raster)) {
        if (layer_name %in% names(scaling_params$means)) {
          mean_val <- scaling_params$means[[layer_name]]
          sd_val <- scaling_params$sds[[layer_name]]
          
          if (sd_val > 0) {
            scaled_raster[[layer_name]] <- (pred_raster[[layer_name]] - mean_val) / sd_val
          } else {
            scaled_raster[[layer_name]] <- pred_raster[[layer_name]] - mean_val
          }
        }
      }
      
      return(scaled_raster)
    }
    
    
    # observer for scaling data when aggregated_CTtable changes
    observe({
      req(data$aggregated_CTtable)
      
      # Scale the covariates
      scaling_result <- scale_covariates(data$aggregated_CTtable)
      
      # Store results
      data$scaling_params <- scaling_result$scaling_params
      data$aggregated_CTtable_scaled <- scaling_result$scaled_data
      
      # Scale prediction raster if it exists
      if (!is.null(data$prediction_raster)) {
        data$prediction_raster_scaled <- scale_prediction_raster(data$prediction_raster, 
                                                                 data$scaling_params)
      }
    })
    
    # observer for scaling prediction raster when it changes
    observe({
      req(data$prediction_raster, data$scaling_params)
      data$prediction_raster_scaled <- scale_prediction_raster(data$prediction_raster, 
                                                               data$scaling_params)
    })
    
    
    
    ### Species selection ####
    
    # Create species table
    species_summary <- reactive({
      req(data$recordTable, data$speciesCol, data$stationCol)
      data$recordTable %>%
        group_by(!!sym(data$speciesCol)) %>%
        summarize(
          Records = n(),
          Stations = n_distinct(!!sym(data$stationCol))
        )
    })
    
    
    output$speciesTable <- DT::renderDT({
      DT::datatable(species_summary(), 
                    selection = 'multiple',
                    options = list(pageLength = 10))
    })
    
    # Function to get species meeting criteria
    getSpeciesMeetingCriteria <- function(minStations, minRecords) {
      req(species_summary())
      species_summary() %>%
        dplyr::filter(Records >= minRecords, Stations >= minStations) %>%
        pull(!!sym(data$speciesCol))
    }
    
    # Handle Select All button
    observeEvent(input$selectAllSpecies, {
      proxy <- DT::dataTableProxy("speciesTable")
      DT::selectRows(proxy, input$speciesTable_rows_all)
    })
    
    # Handle Deselect All button
    observeEvent(input$deselectAllSpecies, {
      proxy <- DT::dataTableProxy("speciesTable")
      DT::selectRows(proxy, NULL)
    })
    
    # Handle Select by Min Stations button
    observeEvent(input$selectByStations, {
      species_to_select <- getSpeciesMeetingCriteria(input$minStations, 1)
      updateSpeciesSelection(species_to_select)
    })
    
    # Handle Select by Min Records button
    observeEvent(input$selectByRecords, {
      species_to_select <- getSpeciesMeetingCriteria(1, input$minRecords)
      updateSpeciesSelection(species_to_select)
    })
    
    # Handle Select by Both Criteria button
    observeEvent(input$selectByBoth, {
      species_to_select <- getSpeciesMeetingCriteria(input$minStations, input$minRecords)
      updateSpeciesSelection(species_to_select)
    })
    
    # Function to update species selection in the table
    updateSpeciesSelection <- function(species_to_select) {
      req(species_summary())
      if (length(species_to_select) > 0) {
        proxy <- DT::dataTableProxy("speciesTable")
        rows_to_select <- which(species_summary()[[data$speciesCol]] %in% species_to_select)
        DT::selectRows(proxy, rows_to_select)
      }
    }
    
    
    
    # Display selected species count
    output$selectedSpeciesCount <- renderText({
      req(input$speciesTable_rows_selected, species_summary())
      total_species <- nrow(species_summary())
      selected_count <- length(input$speciesTable_rows_selected)
      paste("Selected", selected_count, "out of", total_species, "Species")
    })
    
    
    
    # Reactive value to store the commOccu object
    commOccu_model <- reactiveVal(NULL)
    
    # Observer to create commOccu object
    observeEvent(input$createCommunityModel, {
      
      req(data$aggregated_CTtable_scaled)
      
      # Get the selected rows from the species table
      selected_rows <- input$speciesTable_rows_selected
      
      # Check if any species are selected
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("Please select at least one species", type = "warning")
        return()
      }
      
      # Get the actual species names
      selected_species <- sort(species_summary()[[data$speciesCol]][selected_rows])
      
      
      # Create detection history list for selected species
      ylist_full <- lapply(selected_species, function(sp) {
        detection_hist <- detectionHistory(
          recordTable = data$recordTable[data$recordTable[, data$speciesCol] == sp, ],
          camOp = camop(),
          stationCol = data$stationCol,
          speciesCol = data$speciesCol,
          recordDateTimeCol = data$recordDateTimeCol,
          species = sp,
          occasionLength = input$occasionLength_community,
          day1 = "survey",
          datesAsOccasionNames = FALSE,
          includeEffort = TRUE,
          scaleEffort = TRUE,
          timeZone = data$timeZone
        )
        return(detection_hist)
      })
      names(ylist_full) <- selected_species
      
      ylist <- lapply(ylist_full, FUN = function(x) x$detection_history)
      
      
      # Prepare data_list for communityModel
      data_list <- list(
        ylist = ylist,
        siteCovs = data$aggregated_CTtable_scaled,      # Use scaled covariates
        obsCovs = list(effort = ylist_full[[1]][[2]])   # effort matrix of first species
      )
      
      
      
      # Prepare model arguments
      model_args <- list(
        data_list = data_list,
        model = input$communityModelType,
        occuCovs = list(
          fixed = symbols_to_char_or_null(input$occuCovFixed),
          independent = symbols_to_char_or_null(input$occuCovIndep),
          ranef = symbols_to_char_or_null(input$occuCovRanef)
        ),
        detCovs = list(
          fixed = symbols_to_char_or_null(input$detCovFixed),
          ranef = symbols_to_char_or_null(input$detCovRanef),
          independent = symbols_to_char_or_null(input$detCovIndep)
        ),
        detCovsObservation = list(
          fixed = NULL,
          ranef = NULL
        ),
        speciesSiteRandomEffect = list(
          det = input$speciesSiteRandomEffectDet,
          occu = FALSE  # Not implemented in communityModel function
        ),
        intercepts = list(
          det = input$detIntercept,
          occu = input$occuIntercept
        ),
        richnessCategories = if (is.null(input$richnessCategories) || input$richnessCategories == "") NULL else input$richnessCategories,
        augmentation = if (input$augmentationType != "none") {
          stats::setNames(list(input$augmentationValue), input$augmentationType)
        } else {
          NULL
        },
        modelFile = if (input$modelFile == "") tempfile(fileext = ".txt") else input$modelFile,
        nimble = input$useNimble
      )
      # Add effort as detection covariate if selected
      if (input$useEffortAsDetCov) {
        if (input$effortDetCovType == "fixed") {
          model_args$detCovsObservation$fixed <- c(model_args$detCovsObservation$fixed, "effort")
        } else {
          model_args$detCovsObservation$ranef <- c(model_args$detCovsObservation$ranef, "effort")
        }
      }
      
      
      # Create commOccu object
      tryCatch({
        comm_model <- do.call(communityModel, model_args)
        commOccu_model(comm_model)
        showNotification("Community model created successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Error creating community model:", e$message), type = "error")
        print(paste("Detailed error:", e))  # print the full error message to the console
      })
    })
    
    # Render model summary
    output$communityModelSummary <- renderPrint({
      req(commOccu_model())
      summary(commOccu_model())
    })
    
    
    
    
    ### Fit model ----
    
    # automatically set burnin to half of iterations, when iterations change
    observeEvent(input$niter, {
      updateNumericInput(session, "nburn", value = floor(input$niter / 2))
    })
    
    # Create a reactive value to store console output
    consoleOutput <- reactiveVal("")
    
    # Render the console output
    output$consoleOutput <- renderPrint({
      cat(consoleOutput())
    })
    
    
    observeEvent(input$fitCommunityModel, {
      req(commOccu_model())
      
      withProgress(message = 'Fitting model...', value = 0, {
        tryCatch({
          fitted_model <- fit(commOccu_model(),
                              n.iter = input$niter,
                              thin = input$nthin,
                              n.burnin = input$nburn,
                              chains = input$nchains)
          
          # Store the fitted model in a reactive value
          fitted_comm_model(fitted_model)
          
          # Calculate and store the summary
          model_summary(summary(fitted_model))
          
          showNotification("Model run completed", type = "message")
        }, error = function(e) {
          showNotification(paste("Error fitting model:", e$message), type = "error")
          print(paste("Detailed error:", e))
        })
      })
    })
    
    
    
    
    observeEvent(input$fitCommunityModel_background, {
      req(commOccu_model())
      
      # Show a notification that the model is running
      showNotification("Model fitting has started. This may take a while...", 
                       type = "message", duration = NULL, id = "fitting")
      
      # Define the function to run in the background
      fit_func <- function() {
        fit(commOccu_model(),
            n.iter = input$niter,
            thin = input$nthin,
            n.burnin = input$nburn,
            chains = input$nchains)
      }
      
      # Run the fit function in the background
      bg_process <- callr::r_bg(
        func = fit_func,
        args = list(),
        package = TRUE  # This ensures the function environment is preserved
      )
      
      # Set up a repeating timer to check process status
      timer <- reactiveTimer(1000)  # Check every second
      
      observe({
        timer()
        if (!bg_process$is_alive()) {
          # Process has finished
          removeNotification("fitting")
          tryCatch({
            fitted_model <- bg_process$get_result()
            
            # Store the fitted model in a reactive value
            fitted_comm_model(fitted_model)
            
            # Calculate and store the summary
            model_summary(summary(fitted_model))
            
            consoleOutput(paste(consoleOutput(), "Model run completed successfully.\n", sep = "\n"))
            showNotification("Model run completed", type = "message")
          }, error = function(e) {
            consoleOutput(paste(consoleOutput(), "Error fitting model:", e$message, sep = "\n"))
            showNotification(paste("Error fitting model:", e$message), type = "error")
          })
          
          # Stop the timer
          timer$destroy()
        }
      })
    })
    
    
    # Reactive value to store the fitted model
    fitted_comm_model <- reactiveVal(NULL)
    
    # Reactive value to store the model summary
    model_summary <- reactiveVal(NULL)
    
    
    
    # Render parameter estimates
    output$parameterEstimates <- DT::renderDT({
      req(model_summary())
      summary_data <- model_summary()
      round(as.data.frame(summary_data$statistics), digits = 4)
    }) 
    
    # Render parameter quantiles
    output$parameterQuantiles <- DT::renderDT({
      req(model_summary())
      summary_data <- model_summary()
      round(as.data.frame(summary_data$quantiles), digits = 4)
    })
    
    
    
    ### Diagnostics etc. 
    
    
    # Function to process Gelman diagnostics into a data frame
    process_gelman_diag <- function(gd) {
      # Extract point estimates and upper CI
      estimates <- gd$psrf[, 1]
      upper_ci <- gd$psrf[, 2]
      
      # Create data frame
      data.frame(
        Parameter = rownames(gd$psrf),
        Point_Est = round(estimates, 3),
        Upper_CI = round(upper_ci, 3),
        Converged = ifelse(upper_ci < 1.1, "Yes", "No"),
        stringsAsFactors = FALSE
      )
    }
    
    # Add convergence diagnostics output
    output$convergenceDiagnostics <- renderPrint({
      req(fitted_comm_model())
      
      
      # Calculate Gelman-Rubin diagnostics
      gelman_diag <- coda::gelman.diag(fitted_comm_model(), multivariate = FALSE)
      
      # Convert to data frame for display
      diag_df <- process_gelman_diag(gelman_diag)
      
      # Initialize DT output
      output$gelman_diagnostics_table <- DT::renderDT({
        DT::datatable(
          diag_df,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip'
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'Converged',
            backgroundColor = DT::styleEqual(
              c("Yes", "No"),
              c('#e6ffe6', '#ffe6e6')  # Light green for Yes, light red for No
            )
          )
      })
      
      # Print summary statistics
      cat("Gelman-Rubin Convergence Diagnostics Summary:\n\n")
      cat("Total parameters:", nrow(diag_df), "\n")
      cat("Converged parameters:", sum(diag_df$Converged == "Yes", na.rm = T), "\n")
      cat("Non-converged parameters:", sum(diag_df$Converged == "No", na.rm = T), "\n")
      cat("\nPotential scale reduction factors:\n")
      print(summary(gelman_diag$psrf[,1]))
      cat("\nNote: Values close to 1 indicate convergence.\n")
      cat("Values > 1.1 suggest lack of convergence.\n")
    })
    
    
    
    
    
    # Update trace plot parameter choices
    observe({
      req(fitted_comm_model())
      param_names <- dimnames(fitted_comm_model()[[1]])[[2]]
      updateSelectInput(session, "trace_parameter",
                        choices = param_names
      )
    })
    
    # Render trace plot
    output$trace_plot <- renderPlot({
      req(fitted_comm_model(), input$trace_parameter)
      coda::traceplot(fitted_comm_model()[, input$trace_parameter],
                      main = input$trace_parameter,
                      col = scales::hue_pal()(length(fitted_comm_model())),
                      ylab = "Value")
    })
    
    
    
    ### Plot responses
    
    
    
    
    
    # Reactive values for plot settings
    plot_settings <- reactiveValues(
      submodel = "state",
      response = "occupancy",
      speciesSubset = NULL,
      draws = 1000,
      level = c(outer = 0.95, inner = 0.75),
      ordered = TRUE,
      combine = FALSE,
      scales = "free_y",
      community_lines = FALSE
    )
    
    
    # Update species choices when model is available
    observe({
      req(commOccu_model())
      
      # Get species names from the model's ylist
      species_names <- names(commOccu_model()@input$ylist)
      
      # Update the species selection dropdown
      updateSelectInput(session, "plotSpeciesSubset",
                        choices = species_names,
                        selected = character(0))  # Start with none selected
    })
    
    
    
    
    # Clear plot selection when submodel changes
    observeEvent(input$plotSubmodel, {
      effect_plots(NULL)  # Clear stored plots
      updateSelectInput(session, "selectedPlot", choices = NULL)  # Clear selection
    })
    
    
    
    
    # Store plots in reactive values
    effect_plots <- reactiveVal(NULL)
    coef_plot <- reactiveVal(NULL)
    
    
    # Generate plots when settings are updated
    observeEvent(input$updatePlotSettings, {
      req(fitted_comm_model(), input$selectedPlot)
      
      # Create base plot arguments
      base_args <- list(
        object = commOccu_model(),
        mcmc.list = fitted_comm_model(),
        submodel = input$plotSubmodel,
        draws = input$plotDraws
      )
      
      # Add response only for RN models with state submodel
      if (input$communityModelType == "RN" && input$plotSubmodel == "state") {
        base_args$response <- input$plotResponse
      }
      
      # Add species subset if any species are selected
      if (!is.null(input$plotSpeciesSubset) && length(input$plotSpeciesSubset) > 0) {
        base_args$speciesSubset <- input$plotSpeciesSubset
      }
      
      # Generate response curve plots (single confidence level)
      effect_args <- base_args
      effect_args$level <- input$plotLevelOuter
      plots <- do.call(camtrapR::plot_effects, effect_args)
      effect_plots(plots)
      
      # Generate coefficient plot (both confidence levels)
      coef_args <- base_args
      coef_args$level <- c(input$plotLevelOuter, input$plotLevelInner)
      coef_args$ordered <- input$orderByEffect
      coef_plot(do.call(camtrapR::plot_coef, coef_args))
    })
    
    
    
    # Function to get covariate names from model attributes
    get_covariate_names <- function(model, submodel) {
      req(model)
      
      # Get covariate info from model attributes
      covariate_info <- attributes(model)$covariate_info
      
      # Filter for parameters (not intercepts) in the specified submodel
      covariates <- covariate_info[covariate_info$param == "param" & 
                                     covariate_info$submodel == submodel, ]
      
      # Return covariate names
      unique(covariates$covariate)
    }
    
    
    
    # Update plot selection choices when submodel changes
    observe({
      req(commOccu_model(), input$plotSubmodel)
      
      # Get covariate names for current submodel
      covariate_names <- get_covariate_names(commOccu_model(), input$plotSubmodel)
      
      # Update the selection input
      updateSelectInput(session, "selectedPlot",
                        choices = covariate_names,
                        selected = if(length(covariate_names) > 0) covariate_names[1] else NULL)
    })
    # Render the selected response curve plot
    output$responseCurvePlot <- renderPlot({
      req(effect_plots(), input$selectedPlot)
      
      # Get the plot and modify its theme
      plot <- effect_plots()[[input$selectedPlot]] +
        theme_update(
          text = element_text(size = 12 * input$plotScale),
          axis.text = element_text(size = 11 * input$plotScale),
          axis.title = element_text(size = 12 * input$plotScale),
          strip.text = element_text(size = 12 * input$plotScale),
          legend.text = element_text(size = 11 * input$plotScale),
          legend.title = element_text(size = 12 * input$plotScale)
        )
      
      print(plot)
    })
    
    # Render the coefficient plot
    output$coefficientPlot <- renderPlot({
      req(coef_plot())
      
      # Get the plot and modify its theme
      plot <- coef_plot()[[input$selectedPlot]] +
        theme_update(
          text = element_text(size = 12 * input$plotScale),
          axis.text = element_text(size = 11 * input$plotScale),
          axis.title = element_text(size = 12 * input$plotScale),
          strip.text = element_text(size = 12 * input$plotScale),
          legend.text = element_text(size = 11 * input$plotScale),
          legend.title = element_text(size = 12 * input$plotScale)
        )
      
      print(plot)
    })
    
    
    
    
    
    
    
    
    
    ## Spatial predictions (community models) ----
    
    # Reactive values to store prediction results
    spatial_predictions_community <- reactiveValues(
      occupancy = NULL,
      richness = NULL,
      pao = NULL
    )
    
    # Validate input data before running predictions
    validate_prediction_inputs <- function() {
      if (input$prediction_raster_source == "extracted") {
        if (is.null(data$prediction_raster)) {
          showNotification("No extracted covariates available. Please extract covariates first.", 
                           type = "error")
          return(FALSE)
        }
      } else {
        if (is.null(input$covariate_raster)) {
          showNotification("Please upload a custom raster.", type = "error")
          return(FALSE)
        }
      }
      
      if (is.null(fitted_comm_model()) || is.null(commOccu_model())) {
        showNotification("Please fit the community model first.", type = "error")
        return(FALSE)
      }
      
      return(TRUE)
    }
    
    # Get covariate raster based on user selection
    get_covariate_raster <- function() {
      if (input$prediction_raster_source == "extracted") {
        return(data$prediction_raster)
      } else {
        return(terra::rast(input$covariate_raster$datapath))
      }
    }
    
    # Observer for species occupancy predictions
    observeEvent(input$runOccupancyPrediction, {
      if (!validate_prediction_inputs()) return()
      
      withProgress(message = 'Generating occupancy predictions...', value = 0, {
        tryCatch({
          predictions <- predict(
            object = commOccu_model(),
            mcmc.list = fitted_comm_model(),
            type = "psi",
            draws = input$predictionDraws,
            level = input$predictionLevel,
            interval = "confidence",
            x = get_covariate_raster(),
            batch = if(input$predictionBatch) input$batchSize else FALSE,
            seed = input$predictionSeed
          )
          
          # Store predictions
          spatial_predictions_community$occupancy <- predictions
          
          showNotification("Occupancy predictions completed", type = "message")
        }, error = function(e) {
          showNotification(paste("Error in occupancy predictions:", e$message), type = "error")
        })
      })
    })
    
    # Observer for richness predictions
    observeEvent(input$runRichnessPrediction, {
      if (!validate_prediction_inputs()) return()
      
      withProgress(message = 'Generating richness predictions...', value = 0, {
        tryCatch({
          predictions <- predict(
            object = commOccu_model(),
            mcmc.list = fitted_comm_model(),
            type = "richness",
            draws = input$predictionDraws,
            level = input$predictionLevel,
            interval = "confidence",
            x = get_covariate_raster(),
            batch = if(input$predictionBatch) input$batchSize else FALSE,
            seed = input$predictionSeed
          )
          
          # Store predictions
          spatial_predictions_community$richness <- predictions
          
          showNotification("Richness predictions completed", type = "message")
        }, error = function(e) {
          showNotification(paste("Error in richness predictions:", e$message), type = "error")
        })
      })
    })
    
    # Observer for PAO predictions
    observeEvent(input$runPAOPrediction, {
      if (!validate_prediction_inputs()) return()
      
      withProgress(message = 'Calculating PAO predictions...', value = 0, {
        tryCatch({
          predictions <- predict(
            object = commOccu_model(),
            mcmc.list = fitted_comm_model(),
            type = "pao",
            draws = input$predictionDraws,
            level = input$predictionLevel,
            x = get_covariate_raster(),
            batch = if(input$predictionBatch) input$batchSize else FALSE,
            seed = input$predictionSeed
          )
          
          # Store predictions
          spatial_predictions_community$pao <- predictions
          
          showNotification("PAO predictions completed", type = "message")
        }, error = function(e) {
          showNotification(paste("Error in PAO predictions:", e$message), type = "error")
        })
      })
    })
    
    # Render occupancy map
    observe({
      req(spatial_predictions_community$occupancy, input$occupancyMapType)
      
      # Get current layer based on map type selection
      current_layer <- switch(input$occupancyMapType,
                              "mean" = spatial_predictions_community$occupancy$mean,
                              "sd" = spatial_predictions_community$occupancy$sd,
                              "lower" = spatial_predictions_community$occupancy$lower,
                              "upper" = spatial_predictions_community$occupancy$upper
      )
      
      # Select specific species layer if needed
      if (terra::nlyr(current_layer) > 1) {
        req(input$occupancySpecies)
        current_layer <- current_layer[[input$occupancySpecies]]
      }
      
      # Create map
      m <- mapview::mapview(
        current_layer,
        layer.name = paste0("Species Occupancy - ", input$occupancyMapType),
        col.regions = viridisLite::viridis(100),
        na.color = "transparent"
      )
      
      # Add study area if available
      if (!is.null(data$study_area)) {
        m <- m + mapview::mapview(data$study_area,
                                  col.regions = "transparent",
                                  color = "red",
                                  lwd = 2,
                                  layer.name = "Study Area"
        )
      }
      
      output$occupancyMap <- leaflet::renderLeaflet({ m@map })
    })
    
    # Render richness map
    observe({
      req(spatial_predictions_community$richness, input$richnessType)
      
      current_layer <- switch(input$richnessType,
                              "mean" = spatial_predictions_community$richness$mean,
                              "sd" = spatial_predictions_community$richness$sd,
                              "lower" = spatial_predictions_community$richness$lower,
                              "upper" = spatial_predictions_community$richness$upper
      )
      
      m <- mapview::mapview(
        current_layer,
        layer.name = paste0("Species Richness - ", input$richnessType),
        col.regions = viridisLite::viridis(100),
        na.color = "transparent"
      )
      
      if (!is.null(data$study_area)) {
        m <- m + mapview::mapview(data$study_area,
                                  col.regions = "transparent",
                                  color = "red",
                                  lwd = 2,
                                  layer.name = "Study Area"
        )
      }
      
      output$richnessMap <- leaflet::renderLeaflet({ m@map })
    })
    
    # Render PAO plot and table
    observe({
      req(spatial_predictions_community$pao)
      
      # Render violin plot
      output$paoPlot <- renderPlot({
        ggplot(spatial_predictions_community$pao$pao_df, 
               aes(x = stats::reorder(Species, PAO, FUN = median), y = PAO)) +
          geom_violin(fill = "lightblue", alpha = 0.5) +
          geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) +
          theme_bw() +
          theme(
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          ) +
          labs(
            title = "Percentage of Area Occupied by Species",
            y = "PAO",
            x = NULL
          ) 
      })
      
      # Render summary table
      output$paoTable <- DT::renderDT({
        DT::datatable(
          round(spatial_predictions_community$pao$pao_summary, 2),
          options = list(
            pageLength = 10,
            scrollX = TRUE
          )
        )
      })
    })
    
    
    # Update species selection choices
    observe({
      req(commOccu_model())
      species_names <- rownames(commOccu_model()@data$y)
      updateSelectInput(session, "occupancySpecies",
                        choices = species_names,
                        selected = species_names[1]
      )
    })
    
    
    
    
    # Tab: ActivityDensity     ####
    output$activity_density_plot <- shiny::renderPlot({
      req(data$recordTable, input$ad_species, data$speciesCol, data$recordDateTimeCol, data$recordDateTimeFormat)
      
      tryCatch({
        activityDensity(
          recordTable = data$recordTable,
          species = input$ad_species,
          speciesCol = data$speciesCol,
          recordDateTimeCol = data$recordDateTimeCol,
          recordDateTimeFormat = data$recordDateTimeFormat
        )
      }, warning = function(e) {
        if (grepl("Bandwidth estimation failed", e$message)) {
          plot.new()
          text(0.5, 0.5, 
               paste("Unable to generate activity density plot for", input$ad_species, "\n",
                     "There may be insufficient data (e.g., only one record)"),
               cex = 1.2, col = "red", adj = 0.5)
        } else {
          stop(e)  # Re-throw the error if it's not the one we're expecting
        }
      })
    })
    
    # Tab: Two-Species Overlap     ####
    output$actOverlapPlot <- shiny::renderPlot({
      req(data$recordTable, input$speciesA, input$speciesB, data$speciesCol, data$recordDateTimeCol, data$recordDateTimeFormat)
      
      tryCatch({
        activityOverlap(
          recordTable = data$recordTable,
          speciesA = input$speciesA,
          speciesB = input$speciesB,
          speciesCol = data$speciesCol,
          recordDateTimeCol = data$recordDateTimeCol,
          recordDateTimeFormat = data$recordDateTimeFormat,
          main = paste0("Activity overlap: ", input$speciesA, " - ", input$speciesB)
        )
      }, error = function(e) {
        if (grepl("Bandwidth estimation failed", e$message)) {
          plot.new()
          text(0.5, 0.5, 
               paste("Unable to generate activity overlap plot for", 
                     input$speciesA, "and", input$speciesB, "\n",
                     "There may be insufficient data for one or both species"),
               cex = 1.2, col = "red", adj = 0.5)
        } else {
          stop(e)  # Re-throw the error if it's not the one we're expecting
        }
      })
    })
    
    
    
    # Data export functionality    ----
    
    export_tracker <- reactiveVal(list(
      last_export = NULL,
      tables = NULL,
      detection_history = NULL,
      models = list(),
      plots = NULL,
      predictions = NULL,
      community_models = NULL
    ))
    
    # Function to update modification time
    update_mod_time <- function(item) {
      tracker <- export_tracker()
      tracker[[item]] <- Sys.time()
      export_tracker(tracker)
    }
    
    # Update modification times when relevant actions occur
    observeEvent(data$CTtable_sf, { update_mod_time("tables") })
    observeEvent(data$recordTable, { update_mod_time("tables") })
    observeEvent(detection_hist(), { update_mod_time("detection_history") })
    # observeEvent(current_model(), { 
    #   tracker <- export_tracker()
    #   tracker$models[[length(tracker$models) + 1]] <- Sys.time()
    #   tracker$plots <- Sys.time()  # Plots change with new model
    #   export_tracker(tracker)
    # })
    observeEvent(data$prediction_raster, { update_mod_time("predictions") })
    observeEvent(commOccu_model(), { update_mod_time("community_models") })
    observeEvent(fitted_comm_model(), { update_mod_time("community_models") })
    
    
    
    observeEvent(input$export_all_data, {
      tracker <- export_tracker()
      last_export <- tracker$last_export
      
      showModal(modalDialog(
        title = "Select Data to Export",
        size = "l",
        
        # Data Tables section
        h4("Data Tables"),
        checkboxGroupInput("export_tables", "Select tables to export:",
                           choices = c("Camera Trap Table" = "ct_table",
                                       "Aggregated Camera Trap Table" = "ct_table_agg",
                                       "Record Table" = "record_table"),
                           selected = c("ct_table", "record_table")),
        radioButtons("table_format", "Table export format:",
                     choices = c("CSV" = "csv", "RData" = "rdata"),
                     selected = "csv", inline = TRUE),
        
        tags$hr(),
        
        # Spatial Data section
        h4("Spatial Data"),
        checkboxGroupInput("export_spatial", "Select spatial data to export:",
                           choices = c("Camera Trap Stations" = "ct_stations_sf",
                                       "Species Detections" = "species_detections_sf"),
                           selected = c("ct_stations_sf")),
        # checkboxInput("export_geopackage", "Save to GeoPackage", value = TRUE),
        
        tags$hr(),
        
        # Occupancy Models section
        h4("Occupancy Models"),
        checkboxInput("export_occupancy", "Export Occupancy Model Data", 
                      value = is.null(last_export) || any(sapply(tracker$models, function(x) is.null(x) || x > last_export))),
        conditionalPanel(
          condition = "input.export_occupancy == true",
          checkboxGroupInput("occupancy_components", "Select components to export:",
                             choices = c("unmarkedFrameOccu" = "umf",
                                         "Model Object" = "model",
                                         "Model Summary" = "summary",
                                         "Response Plots" = "plots",
                                         "Spatial Predictions" = "predictions"),
                             selected = c("umf", "model", "summary", "plots", "predictions"))
        ),
        
        tags$hr(),
        
        # Community Occupancy Models section
        h4("Community Occupancy Models"),
        checkboxInput("export_community_models", "Export Community Occupancy Model Data", 
                      value = is.null(last_export) || is.null(tracker$community_models) || tracker$community_models > last_export),
        conditionalPanel(
          condition = "input.export_community_models == true",
          checkboxGroupInput("comm_model_components", "Select components to export:",
                             choices = c("Model object" = "object",
                                         "Fitted model" = "fitted",
                                         "Summary" = "summary",
                                         "Plots" = "plots",
                                         "Spatial Predictions" = "predictions"),
                             selected = c("object", "fitted", "summary"))
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          shiny::actionButton("confirm_export", "Export Selected Data")
        )
      ))
    })
    
    
    
    
    
    observeEvent(input$confirm_export, {
      removeModal()
      
      # Prompt user to select a base directory
      base_dir <- rstudioapi::selectDirectory(
        caption = "Select folder for data export",
        label = "Select"
      )
      
      if (is.null(base_dir)) return()  # User cancelled
      
      # Create folder structure
      export_dir <- file.path(base_dir, paste0("camtrapR_export_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(export_dir)
      
      
      
      withProgress(message = 'Exporting data...', value = 0, {
        
        # Export Data Tables
        if (length(input$export_tables) > 0) {
          dir.create(file.path(export_dir, "data_tables"))
          if ("ct_table" %in% input$export_tables) {
            export_table(data$CTtable_sf, "camera_trap_table", input$table_format, file.path(export_dir, "data_tables"))
          }
          if ("ct_table_agg" %in% input$export_tables && !is.null(data$aggregated_CTtable)) {
            export_table(data$aggregated_CTtable, "aggregated_camera_trap_table", input$table_format, file.path(export_dir, "data_tables"))
          }
          if ("record_table" %in% input$export_tables) {
            export_table(data$recordTable, "record_table", input$table_format, file.path(export_dir, "data_tables"))
          }
        }
        
        # Export Spatial Data
        if (length(input$export_spatial) > 0) {
          dir.create(file.path(export_dir, "spatial_data"))
          if ("ct_stations_sf" %in% input$export_spatial) {
            sf::st_write(data$CTtable_sf, file.path(export_dir, "spatial_data", "camera_trap_stations.gpkg"))
          }
          if ("species_detections_sf" %in% input$export_spatial) {
            # Assuming you have a function to create this sf object
            species_detections <- create_species_detections_sf(data$recordTable, data$CTtable_sf)
            sf::st_write(species_detections, file.path(export_dir, "spatial_data", "species_detections.gpkg"))
          }
        }
        
        # Export Occupancy Models
        if (input$export_occupancy) {
          dir.create(file.path(export_dir, "occupancy_models"))
          for (species in names(all_models())) {
            species_dir <- file.path(export_dir, "occupancy_models", make.names(species))
            dir.create(species_dir)
            
            if ("umf" %in% input$occupancy_components) {
              saveRDS(umf(), file.path(species_dir, "unmarkedFrameOccu.rds"))
            }
            if ("model" %in% input$occupancy_components) {
              saveRDS(all_models()[[species]], file.path(species_dir, "model_object.rds"))
            }
            if ("summary" %in% input$occupancy_components) {
              capture.output(summary(all_models()[[species]]), file = file.path(species_dir, "model_summary.txt"))
            }
            if ("plots" %in% input$occupancy_components) {
              # Assuming you have functions to generate these plots
              ggsave(file.path(species_dir, "effect_plot_detection.png"), plot = plot_effect_det(all_models()[[species]]))
              ggsave(file.path(species_dir, "effect_plot_occupancy.png"), plot = plot_effect_occu(all_models()[[species]]))
            }
            if ("predictions" %in% input$occupancy_components) {
              # Assuming you have a function to generate spatial predictions
              predictions <- generate_spatial_predictions(all_models()[[species]], data$prediction_raster)
              terra::writeRaster(predictions, file.path(species_dir, "spatial_predictions.tif"), overwrite=TRUE)
            }
          }
        }
        
        # Export Community Occupancy Models
        if (input$export_community_models) {
          dir.create(file.path(export_dir, "community_occupancy_models"))
          if ("object" %in% input$comm_model_components) {
            saveRDS(commOccu_model(), file.path(export_dir, "community_occupancy_models", "community_model_object.rds"))
          }
          if ("fitted" %in% input$comm_model_components) {
            saveRDS(fitted_comm_model(), file.path(export_dir, "community_occupancy_models", "fitted_community_model.rds"))
          }
          if ("summary" %in% input$comm_model_components) {
            capture.output(summary(fitted_comm_model()), 
                           file = file.path(export_dir, "community_occupancy_models", "community_model_summary.txt"))
          }
          # Placeholders for plots and spatial predictions (to be implemented)
          # if ("plots" %in% input$comm_model_components) { ... }
          # if ("predictions" %in% input$comm_model_components) { ... }
        }
      })
      
      showNotification(paste("Data exported to", export_dir), type = "message", duration = NULL)
    })
    
    
    
    # Save / Restore app state ----
    
    output$stateMenu <- renderMenu({
      dropdownMenu(
        type = "tasks", 
        headerText = "App State",
        icon = icon("floppy-disk"),
        badgeStatus = NULL,
        
        # Save state menu item
        tags$li(
          tags$a(
            href = "#",
            onclick = "Shiny.setInputValue('saveState', true, {priority: 'event'});",
            icon("save"),
            "Save current state"
          ),
          class = "menuitem"
        ),
        
        # Load state menu item
        tags$li(
          tags$a(
            href = "#",
            onclick = "Shiny.setInputValue('loadState', true, {priority: 'event'});",
            icon("upload"),
            "Load saved state"
          ),
          class = "menuitem"
        )
      )
    })
    
    
    
    # Function to collect all app state
    collectAppState <- function(data, my_object, #current_model, 
                                commOccu_model, 
                                fitted_comm_model, model_summary, modelEffects) {    
      list(
        # Data tables
        CTtable = data$CTtable,
        CTtable_sf = data$CTtable_sf,
        aggregated_CTtable = data$aggregated_CTtable,
        recordTable = data$recordTable,
        
        # Column specifications
        stationCol = data$stationCol,
        cameraCol = data$cameraCol,
        xcol = data$xcol,
        ycol = data$ycol,
        crs = data$crs,
        setupCol = data$setupCol,
        retrievalCol = data$retrievalCol,
        hasProblems = data$hasProblems,
        CTdateFormat = data$CTdateFormat,
        camerasIndependent = data$camerasIndependent,
        speciesCol = data$speciesCol,
        recordDateTimeCol = data$recordDateTimeCol,
        recordDateTimeFormat = data$recordDateTimeFormat,
        timeZone = data$timeZone,
        exclude = data$exclude,
        
        # Camera operation and filtering
        camop = camop(),
        cameras_independent = data$cameras_independent,
        filter_states = list(
          active_filters = active_filters(),
          filtered_data = filtered_data(),
          original_data = original_data()
        ),
        
        # Raster data
        prediction_raster = data$prediction_raster,
        original_rasters = data$original_rasters,
        
        # Models and results
        # current_model = current_model(),
        modList = my_object$modList,
        commOccu_model = commOccu_model(),
        fitted_comm_model = fitted_comm_model(),
        model_summary = model_summary(),
        modelEffects = modelEffects(),
        
        
        # Covariate and prediction data
        covariate_states = list(
          scaling_params = data$scaling_params,
          aggregated_CTtable_scaled = data$aggregated_CTtable_scaled,
          prediction_raster_scaled = data$prediction_raster_scaled,
          original_columns = data$original_columns
        ),
        
        # Detection history and occupancy
        detection_states = list(
          detection_hist = detection_hist(),
          dh1_df = dh1_df(),
          dh2_df = dh2_df()
        ),
        
        prediction_states = list(
          spatial_predictions = spatial_predictions(),
          community_predictions = list(
            occupancy = spatial_predictions_community$occupancy,
            richness = spatial_predictions_community$richness,
            pao = spatial_predictions_community$pao
          )
        ),
        
        # Study area
        study_area_states = list(
          study_area = data$study_area,
          study_area_buffer = data$study_area_buffer
        ),
        
        # Add UI state
        ui_state = list(
          selected_species = list(
            species_for_map = input$species_for_map,
            ad_species = input$ad_species,
            species_dethist = input$species_dethist,
            speciesA = input$speciesA,
            speciesB = input$speciesB
          ),
          occasion_length = list(
            single_species = input$occasionLength_single_species,
            community = input$occasionLength_community
          ),
          output_type = input$outputType,
          day1 = input$day1,
          # Camera settings
          camera_settings = list(
            minDeltaTime = input$minDeltaTime,
            deltaTimeComparedTo = input$deltaTimeComparedTo,
            removeDuplicateRecords = input$removeDuplicateRecords
          )
        ),
        
        # Add reactive values
        current_species_list = current_species_list(),
        
        
        # Extract settings
        extract_settings = list(
          bufferCT = input$bufferCT,
          bilinear = input$bilinear,
          predictionExtent = input$predictionExtent,
          bufferPrediction = input$bufferPrediction
        ),
        
        # Display settings
        display_settings = list(
          scale_size = input$scale_size,
          no_record_more_transparent = input$no_record_more_transparent,
          maxPixels = input$maxPixels,
          colorPalette = input$colorPalette,
          invertColors = input$invertColors,
          ctColorBy = input$ctColorBy,
          ctPointSize = input$ctPointSize,
          predictionColorPalette = input$predictionColorPalette,
          invertPredictionColors = input$invertPredictionColors,
          maxPixelsPrediction = input$maxPixelsPrediction
        )
      )
      
    }
    
    # Modified restoreAppState function with correct ordering
    restoreAppState <- function(saved_state, data, my_object, #current_model, 
                                commOccu_model, fitted_comm_model, model_summary, 
                                modelEffects, session) {
      
      # 1. First restore column specifications
      data$stationCol <- saved_state$stationCol
      data$cameraCol <- saved_state$cameraCol
      data$xcol <- saved_state$xcol
      data$ycol <- saved_state$ycol
      data$crs <- saved_state$crs
      data$setupCol <- saved_state$setupCol
      data$retrievalCol <- saved_state$retrievalCol
      data$hasProblems <- saved_state$hasProblems
      data$CTdateFormat <- saved_state$CTdateFormat
      data$camerasIndependent <- saved_state$camerasIndependent
      data$speciesCol <- saved_state$speciesCol
      data$recordDateTimeCol <- saved_state$recordDateTimeCol
      data$recordDateTimeFormat <- saved_state$recordDateTimeFormat
      data$timeZone <- saved_state$timeZone
      data$exclude <- saved_state$exclude
      
      # 2. Validate that we have all required column specifications
      required_cols <- c("stationCol", "xcol", "ycol", "setupCol", "retrievalCol", "speciesCol", "recordDateTimeCol")
      missing_cols <- required_cols[!sapply(required_cols, function(col) !is.null(data[[col]]))]
      if (length(missing_cols) > 0) {
        stop("Missing required column specifications: ", paste(missing_cols, collapse = ", "))
      }
      
      # 3. Then restore data tables
      data$CTtable <- saved_state$CTtable
      data$CTtable_sf <- saved_state$CTtable_sf
      data$aggregated_CTtable <- saved_state$aggregated_CTtable
      data$recordTable <- saved_state$recordTable
      
      # 4. Validate that specified columns exist in restored tables
      if (!data$stationCol %in% names(data$CTtable)) {
        stop(paste("stationCol", data$stationCol, "not found in CTtable"))
      }
      if (!data$speciesCol %in% names(data$recordTable)) {
        stop(paste("speciesCol", data$speciesCol, "not found in recordTable"))
      }
      
      # 5. Update species list immediately after table restoration
      species_list <- update_species_inputs()
      
      
      
      # Restore camera operation and filtering states
      if (!is.null(saved_state$filter_states)) {
        active_filters(saved_state$filter_states$active_filters)
        filtered_data(saved_state$filter_states$filtered_data)
        original_data(saved_state$filter_states$original_data)
      }
      
      # 6. Restore UI state with validation
      if (!is.null(saved_state$ui_state)) {
        # Get current valid species list
        valid_species <- sort(unique(data$recordTable[, data$speciesCol]))
        if (!is.null(data$exclude)) {
          valid_species <- valid_species[!valid_species %in% data$exclude]
        }
        
        # Helper function to safely update input
        safeUpdateInput <- function(inputId, value, available_choices) {
          if (!is.null(value) && value %in% available_choices) {
            updateSelectInput(session, inputId, selected = value)
          } else if (!is.null(value) && value == "n_species" && inputId == "species_for_map") {
            updateSelectInput(session, inputId, selected = value)
          } else {
            # Select first available species if saved selection is not valid
            updateSelectInput(session, inputId, selected = available_choices[1])
          }
        }
        
        # Update species selections with validation
        safeUpdateInput("species_for_map", 
                        saved_state$ui_state$selected_species$species_for_map,
                        c("n_species", valid_species))
        
        safeUpdateInput("ad_species",
                        saved_state$ui_state$selected_species$ad_species,
                        valid_species)
        
        safeUpdateInput("species_dethist",
                        saved_state$ui_state$selected_species$species_dethist,
                        valid_species)
        
        safeUpdateInput("speciesA",
                        saved_state$ui_state$selected_species$speciesA,
                        valid_species)
        
        safeUpdateInput("speciesB",
                        saved_state$ui_state$selected_species$speciesB,
                        valid_species)
        
        # Update other UI elements
        updateNumericInput(session, "occasionLength_single_species",
                           value = saved_state$ui_state$occasion_length$single_species)
        
        updateNumericInput(session, "occasionLength_community",
                           value = saved_state$ui_state$occasion_length$community)
        
        updateSelectInput(session, "outputType",
                          selected = saved_state$ui_state$output_type)
        
        updateSelectInput(session, "day1",
                          selected = saved_state$ui_state$day1)
      }
      
      # 7. Restore raster data
      data$prediction_raster <- saved_state$prediction_raster
      data$original_rasters <- saved_state$original_rasters
      
      # 8. Restore models and results
      # current_model(saved_state$current_model)
      my_object$modList <- saved_state$modList
      commOccu_model(saved_state$commOccu_model)
      fitted_comm_model(saved_state$fitted_comm_model)
      model_summary(saved_state$model_summary)
      modelEffects(saved_state$modelEffects)
      
      # 9. Update current_species_list if it was saved
      if (!is.null(saved_state$current_species_list)) {
        current_species_list(saved_state$current_species_list)
      }
    }
    
    # Observers for saving and loading app state
    observeEvent(input$saveState, {
      suggested_filename <- paste0("app_state_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      showModal(modalDialog(
        title = "Save App State",
        textInput("stateFileName", "File name:", value = suggested_filename),
        footer = tagList(
          modalButton("Cancel"),
          shiny::actionButton("confirmSave", "Save")
        )
      ))
    })
    
    observeEvent(input$confirmSave, {
      removeModal()
      
      # Collect current state
      app_state <- collectAppState(data, my_object, #current_model, 
                                   commOccu_model, 
                                   fitted_comm_model, model_summary, modelEffects)
      
      # Use system file dialog with suggested name and default extension
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        file_path <- rstudioapi::selectFile(
          caption = "Save App State",
          label = "Save",
          existing = FALSE,
          filter = "R Data Files (*.rds)",
          path = input$stateFileName
        )
      } else {
        file_path <- file.choose(new = TRUE)
      }
      
      if (!is.null(file_path)) {
        # Ensure .rds extension
        if (!grepl("\\.rds$", file_path, ignore.case = TRUE)) {
          file_path <- paste0(file_path, ".rds")
        }
        
        # Save state
        saveRDS(app_state, file = file_path)
        showNotification("App state saved successfully", type = "message")
      }
    })
    
    
    # Modify the load observer to include better error handling
    observeEvent(input$loadState, {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        file_path <- rstudioapi::selectFile(
          caption = "Load App State",
          label = "Load",
          filter = "R Data Files (*.rds)"
        )
      } else {
        file_path <- file.choose(new = FALSE)
      }
      
      if (!is.null(file_path)) {
        tryCatch({
          # Load state
          saved_state <- readRDS(file_path)
          
          # Validate basic structure of saved state
          required_components <- c("CTtable", "recordTable", "stationCol", "speciesCol")
          missing_components <- required_components[!sapply(required_components, function(comp) !is.null(saved_state[[comp]]))]
          
          if (length(missing_components) > 0) {
            stop("Invalid state file: missing components: ", paste(missing_components, collapse = ", "))
          }
          
          # Restore state with session
          restoreAppState(saved_state, data, my_object, #current_model, 
                          commOccu_model, fitted_comm_model, model_summary, 
                          modelEffects, session)
          
          showNotification("App state loaded successfully", type = "message")
        }, error = function(e) {
          showNotification(paste("Error loading app state:", e$message), 
                           type = "error", duration = NULL)
        })
      }
    })
    
    
    # Add custom CSS to style the menu items
    tags$head(
      tags$style(HTML("
        .menuitem {
            padding: 8px 20px;
            display: block;
            color: #444;
            font-weight: 400;
        }
        .menuitem:hover {
            background-color: #f4f4f4;
            text-decoration: none;
            cursor: pointer;
        }
        .menuitem i {
            margin-right: 10px;
        }
    "))
    )
    
    
  }
  
  shiny::shinyApp(ui, server)    #### 
  
}
