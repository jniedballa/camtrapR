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
#'   \item camtrap DP data import
#'   \item Study area import from shapefile
#'   \item Save/restore functionality for app state
#'   \item Export functionality to save data from dashboard
#' }
#'
#' \strong{Data Processing:}
#' \itemize{
#'   \item Flexible station filtering with multiple criteria
#'   \item Temporal record filtering with independence criteria
#'   \item Filtering species records by species name
#'   \item Automated covariate extraction from local rasters or online 
#'   elevation data
#'   \item Covariate correlation analysis with visualization
#'   \item Species accumulation curves
#' }
#'
#' \strong{Basic Analysis:}
#' \itemize{
#'   \item Basic summary statistics
#'   \item Interactive overview and species detection maps
#'   \item Activity pattern analysis (single species and two-species overlap)
#'   \item Camera operation visualization
#' }
#'
#' \strong{Occupancy Modeling:}
#' \itemize{
#'   \item Basic workflow for simple model specification (linear effects)
# #'   \item Advanced workflow for complex models (linear, quadratic, interaction, 
# #'   random effects)
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
#'   \item Species occupancy, richness and PAO predictions
#' }
#'
#'
#' @note 
#' \itemize{
#'   \item Interactive maps with multiple basemap options
#'   \item Covariate scaling is performed automatically if requested (includes
#'    automatic scaling of prediction rasters)
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
#' @importFrom graphics layout pairs plot.new title
#' @importFrom lubridate is.Date parse_date_time
#' @importFrom utils read.csv str unzip sessionInfo
#' @importFrom shiny renderUI renderText outputOptions req observe observeEvent reactiveVal reactiveValues renderTable renderPrint renderPlot updateSelectInput updateSelectizeInput updateTextInput updateNumericInput updateSliderInput updateCheckboxInput updateCheckboxGroupInput updateActionButton removeNotification showNotification showModal removeModal modalDialog modalButton HTML tags tabsetPanel tabPanel actionButton checkboxInput checkboxGroupInput fileInput numericInput radioButtons selectInput sliderInput textInput uiOutput verbatimTextOutput plotOutput textOutput wellPanel withProgress fluidRow column div hr h4 conditionalPanel helpText tagList tableOutput reactive reactiveTimer varSelectizeInput icon h1 h2 h3 isolate need validate
#' @importFrom shinydashboard dropdownMenu dropdownMenuOutput renderMenu
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom dplyr %>% group_by summarize n n_distinct pull sym
#' @importFrom sf st_buffer st_convex_hull st_drop_geometry st_intersection st_transform st_union st_make_valid
#' @importFrom terra rast vect project resample nlyr values<- mask
#' @importFrom leaflet leaflet leafletOutput renderLeaflet addTiles addCircleMarkers addLayersControl layersControlOptions addPolygons leafletProxy clearGroup
#' @importFrom ggplot2 theme_update element_text element_rect geom_violin geom_boxplot geom_point geom_abline
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
    "shinyjs",
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
  
  
  # function to locate help files
  help_text <- function(filename) {
    # When in package
    pkg_file <- system.file("dashboard_help", filename, package = "camtrapR")
    
    # When in development (assuming standard package structure)
    dev_file <- file.path("inst/dashboard_help", filename)
    
    if (file.exists(pkg_file)) {
      readLines(pkg_file)
    } else if (file.exists(dev_file)) {
      readLines(dev_file)
    } else {
      stop("Help file not found")
    }
  }
  
  
  # Data checks
  if(inherits(CTtable, "tbl")) CTtable <- as.data.frame(CTtable)
  if(inherits(recordTable, "tbl")) recordTable <- as.data.frame(recordTable)
  
  
  # first remove all empty columns in CTtable 
  CTtable <- CTtable[, sapply(CTtable, FUN = function(x) !all(is.na(x)))]
  
  
  # styling of buttons
  tags$head(
    tags$style(HTML("
  .sidebar-button-container {
    padding: 5px 15px;
    margin-top: 10px;
    text-align: center;
  }
  #export_all_data, #resetAllFilters {
    width: 100%;
    white-space: normal;
    height: auto;
    min-height: 44px;
  }
  .sidebar-divider {
    margin: 10px 15px;
    border-top: 1px solid #4b646f;
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
                                 shinydashboard::menuSubItem("Species Filtering", tabName = "filterSpecies"),
                                 shinydashboard::menuSubItem("Extract Covariates", tabName = "extract"),
                                 shinydashboard::menuSubItem("Covariate Correlation", tabName = "covariateCorrelation"),
                                 shinydashboard::menuSubItem("Camera operation matrix", tabName = "CameraOperation"),
                                 shinydashboard::menuSubItem("Species Accumulation", tabName = "speciesAccumulation")
        ),
        
        
        shinydashboard::menuItem("Single-species Occupancy", icon = shiny::icon("calculator"),
                                 shinydashboard::menuSubItem("Detection History", tabName = "DetectionHistory"),
                                 shinydashboard::menuSubItem("Occupancy models", tabName = "Occupancy")
        ),
        shinydashboard::menuItem("Community Occupancy Models", tabName = "CommunityOccupancy", icon = shiny::icon("users")),
        
        
        # Add the divider
        tags$div(
          tags$hr(class = "sidebar-divider")
        ),
        
        # Add Reset All Filters button
        tags$div(
          class = "sidebar-button-container",
          actionButton("resetAllFilters", "Reset All Filters", 
                       icon = icon("sync"),
                       class = "btn-danger")
        ),
        
        # Add export button
        tags$div(
          class = "sidebar-button-container",
          actionButton("export_all_data", "Export Data", 
                       icon = icon("download"))
        )
      )
    ),
    shinydashboard::dashboardBody(
      
      
      shiny::uiOutput("welcome_screen"),
      
      shinydashboard::tabItems(
        
        
        ## Tab: Import data ----
        
        ###  csv ----
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
                              shiny::uiOutput("camerasIndependentImportUI"),
                              shiny::selectInput("xcol", "X Coordinate Column", choices = NULL),
                              shiny::selectInput("ycol", "Y Coordinate Column", choices = NULL),
                              # shiny::textInput("crs", "Coordinate Reference System (e.g., 'EPSG:4326')"),
                              shiny::textInput("crs", "Coordinate Reference System",
                                               value = "",
                                               placeholder = "e.g. EPSG:4326 or EPSG:32648 (no quotes needed)"),
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
        
        ###  WildlifeInsights ----
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
                fluidRow(
                  column(8,
                         textInput("wi_directory", "Path to Wildlife Insights data directory", 
                                   placeholder = "C:/path/to/wildlife_insights_directory")
                  ),
                  column(4,
                         style = "margin-top: 25px;",
                         actionButton("browse_wi_directory", "Browse...", 
                                      icon = icon("folder-open"))
                  )
                )
              ),
              shiny::checkboxInput("wi_cameras_independent", "Cameras are independent", value = FALSE),
              shiny::actionButton("wi_import_button", "Import Data", class = "btn-primary btn-lg"),
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
        
        ### camtrap DP ----
        shinydashboard::tabItem(
          tabName = "import_camtrapdp",
          fluidRow(
            shinydashboard::box(
              title = "Import from camtrapDP format",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              # Directory selection
              fluidRow(
                column(8,
                       textInput("camtrapdp_directory", "Path to camtrapDP Directory:", 
                                 placeholder = "C:/path/to/camtrapdp_directory")
                ),
                column(4,
                       style = "margin-top: 25px;",
                       actionButton("browse_camtrapdp", "Browse...", 
                                    icon = icon("folder-open"))
                )
              ),
              
              # Import options
              fluidRow(
                column(6,
                       h4("Basic Import Options"),
                       numericInput("camtrapdp_min_gap_hours", 
                                    "Min. gap for camera interruption (hours)", 
                                    value = 24, min = 1),
                       radioButtons("camtrapdp_filter_observations", 
                                    "Filter observations:", 
                                    choices = c("All observation types" = "none",
                                                "Animal observations only" = "animals"),
                                    selected = "none"),
                       checkboxInput("camtrapdp_cameras_independent", 
                                     "Cameras are independent (applies only if multiple cameras per station exist)",
                                     value = FALSE),
                       checkboxInput("camtrapdp_remove_na", 
                                     "Remove columns with only NA values", value = TRUE),
                       checkboxInput("camtrapdp_remove_empty", 
                                     "Remove columns with only empty values", value = TRUE)
                ),
                column(6,
                       h4("Advanced Options"),
                       textInput("camtrapdp_custom_filter", 
                                 "Custom observation types to include (comma-separated):",
                                 placeholder = "e.g., animal,human,vehicle")
                )
              ),
              
              # Action buttons
              fluidRow(
                column(12,
                       div(
                         style = "margin-top: 20px;",
                         actionButton("camtrapdp_import", "Import Data", 
                                      class = "btn-primary btn-lg")
                       )
                )
              ),
              
              # Status and validation
              fluidRow(
                column(12,
                       style = "margin-top: 20px;",
                       h4("Import Status"),
                       verbatimTextOutput("camtrapdp_status")
                )
              )
            )
          ),
          
          # Preview tabs with added Project Info tab
          fluidRow(
            shinydashboard::box(
              title = "Data Preview",
              status = "info", 
              width = 12,
              tabsetPanel(
                tabPanel("Project Information", uiOutput("camtrapdp_project_info")),
                tabPanel("Deployments (CTtable)", DT::dataTableOutput("camtrapdp_preview_deployments")),
                tabPanel("Observations (recordTable)", DT::dataTableOutput("camtrapdp_preview_observations"))
              )
            )
          )
        ),
        
        ### Import AOI ----
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
        
        ### File size control ----
        shinydashboard::tabItem(
          tabName = "file_size_control",
          shinydashboard::box(
            title = "File Size Control",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            shiny::numericInput("max_file_size", "Maximum file size (MB)", value = 10, min = 5, max = 100),
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
                         shiny::actionButton("clearAllFilters", "Clear All Camera Trap Filters", class = "btn-warning")
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
        
        ## Tab: Filter records temporally ----
        
        # shinydashboard::tabItem(
        #   tabName = "filterRecords",
        #   fluidRow(
        #     shinydashboard::box(
        #       title = "Temporal Filtering Settings", width = 12, status = "primary",
        #       numericInput("minDeltaTime", "Minimum time difference (minutes)", value = 0, min = 0),
        #       selectInput("deltaTimeComparedTo", "Compare delta time to:", 
        #                   choices = c("Last independent record" = "lastIndependentRecord", 
        #                               "Last record" = "lastRecord")),
        #       uiOutput("camerasIndependentUI"),  # This will be conditionally rendered
        #       checkboxInput("removeDuplicateRecords", "Remove duplicate records", value = TRUE),
        #       shiny::actionButton("runTemporalFilter", "Apply Temporal Filter"),
        #       shiny::actionButton("restoreOriginalRecordTable", "Restore Original Record Table", class = "btn-warning")
        #       
        #     )
        #   ),
        #   fluidRow(
        #     shinydashboard::box(
        #       title = "Filtered Record Table", width = 12, status = "info",
        #       DT::dataTableOutput("filteredRecordTable")
        #     )
        #   )
        # ),
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
          
          # Add filter summary box
          fluidRow(
            shinydashboard::box(
              title = "Filter Summary", width = 12, status = "info",
              uiOutput("temporalFilterSummary")
            )
          ),
          
          # Add temporal filtering by species table
          fluidRow(
            shinydashboard::box(
              title = "Records by Species", width = 12, status = "info",
              DT::dataTableOutput("temporalFilterTable")
            )
          ),
          
          # # Show filtered record table
          # doesn't print reliably for whatever reason - remove for now, redundant anyways
          # fluidRow(
          #   shinydashboard::box(
          #     title = "Filtered Record Table", width = 12, status = "info",
          #     DT::dataTableOutput("filteredRecordTable")
          #   )
          # )
        ),
        
        
        
        ## Tab: Species filter ----
        shinydashboard::tabItem(
          tabName = "filterSpecies",
          fluidRow(
            shinydashboard::box(
              title = "Species Filtering", width = 12, status = "primary",
              solidHeader = TRUE,
              fluidRow(
                column(8,
                       # Species table
                       DT::dataTableOutput("speciesFilterTable")
                ),
                column(4,
                       # Control panel
                       wellPanel(
                         h4("Filtering Actions"),
                         div(class = "btn-group-vertical btn-block",
                             shiny::actionButton("keepSelectedSpecies", "Keep Selected Species", 
                                                 class = "btn-success btn-block",
                                                 icon = icon("check")),
                             shiny::actionButton("removeSelectedSpecies", "Remove Selected Species", 
                                                 class = "btn-danger btn-block",
                                                 icon = icon("trash")),
                             tags$hr(),
                             shiny::actionButton("resetSpeciesFilter", "Reset All Species", 
                                                 class = "btn-warning btn-block",
                                                 icon = icon("sync"))
                         ),
                         
                         tags$hr(),
                         h4("Filter Summary"),
                         uiOutput("speciesFilterSummary")
                       )
                )
              )
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = "Filtered Records Preview", width = 12, status = "info",
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
                                                      shiny::HTML(paste(help_text("covariateExtraction_help.html"), collapse = "\n"))
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
                                                                              value = 1000, min = 0, step = 100)
                                                               ),
                                                               
                                                               fileInput("rasterTemplate", "Raster Template (optional)", 
                                                                         accept = c(".tif")),
                                                               
                                                               conditionalPanel(
                                                                 condition = "!input.rasterTemplate",
                                                                 numericInput("resolution", "Resolution (meters)", 
                                                                              value = 500, min = 0, step = 10)
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
                                                                                         "Zoom 10 (~80m)" = 10,
                                                                                         "Zoom 9 (~160m)" = 9
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
                                shiny::HTML(paste(help_text("covariateCorrelation_help.html"), collapse = "\n"))
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
        
        ## Tab: Species Accumulation Curves ----
        
        shinydashboard::tabItem(
          tabName = "speciesAccumulation",
          fluidRow(
            # Left column with settings and species selection
            column(3,
                   # Tabs for settings and species selection
                   tabsetPanel(
                     # Settings tab
                     tabPanel("Settings",
                              wellPanel(
                                h4("Basic Settings", class = "text-primary"),
                                
                                numericInput("acc_q", 
                                             "Diversity order (q):",
                                             value = 0,
                                             min = 0,
                                             max = 2,
                                             step = 1
                                ),
                                
                                selectInput("acc_x_unit", 
                                            "Sampling unit:",
                                            choices = c(
                                              "Stations" = "station",
                                              "Survey Days" = "survey_day",
                                              "Station Days" = "station_day"
                                            ),
                                            selected = "station"
                                ),
                                
                                hr(),
                                h4("Curve Settings", class = "text-primary"),
                                
                                
                                numericInput("acc_knots",
                                             "Number of points on curve:",
                                             value = 40,
                                             min = 10,
                                             max = 100,
                                             step = 5
                                ),
                                
                                hr(),
                                h4("Bootstrap Settings", class = "text-primary"),
                                sliderInput("acc_conf",
                                            "Confidence level:",
                                            min = 0.8,
                                            max = 0.99,
                                            value = 0.95,
                                            step = 0.01
                                ),
                                
                                numericInput("acc_nboot",
                                             "Number of bootstrap replicates:",
                                             value = 50,
                                             min = 10,
                                             max = 200,
                                             step = 10
                                ),
                                
                                hr(),
                                h4("Plot Settings", class = "text-primary"),
                                numericInput("acc_plot_scale", 
                                             "Plot size scale:", 
                                             value = 1.5, 
                                             min = 0.5, 
                                             max = 3, 
                                             step = 0.1
                                ),
                                
                                hr(),
                                
                                # Run analysis button at the bottom of settings
                                actionButton("runAccumulation", "Run Analysis", 
                                             class = "btn-primary btn-lg btn-block")
                              )
                     ),
                     
                     
                     # Species Selection tab
                     tabPanel("Species",
                              wellPanel(
                                h4("Species Selection", class = "text-primary"),
                                # Species table
                                DT::dataTableOutput("acc_speciesTable"),
                                
                                hr(),
                                # Selection controls
                                fluidRow(
                                  column(6, actionButton("acc_selectAll", "Select All", class = "btn-block")),
                                  column(6, actionButton("acc_deselectAll", "Deselect All", class = "btn-block"))
                                ),
                                
                                hr(),
                                h4("Filtering", class = "text-primary"),
                                numericInput("acc_minStations", "Min Stations:", value = 1, min = 1),
                                numericInput("acc_minRecords", "Min Records:", value = 1, min = 1),
                                fluidRow(
                                  column(6, actionButton("acc_selectByStations", "By Stations", class = "btn-block")),
                                  column(6, actionButton("acc_selectByRecords", "By Records", class = "btn-block"))
                                ),
                                actionButton("acc_selectByBoth", "Select by Both Criteria", 
                                             class = "btn-block")
                              )
                     )
                   )
            ),
            
            # Right column with output
            column(9,
                   # Results tabs
                   tabsetPanel(
                     selected = "All Curves",
                     
                     tabPanel("Instructions",
                              fluidRow(
                                shinydashboard::box(
                                  title = "Species Accumulation Curves", 
                                  width = 12, 
                                  status = "info",
                                  shiny::HTML(paste(help_text("speciesAccum_help.html"), collapse = "\n"))
                                )
                              )
                     ),
                     
                     # Add new tab for combined plots view:
                     tabPanel("All Curves",
                              fluidRow(
                                div(
                                  style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: space-between;",
                                  div(
                                    style = "flex: 1; min-width: 300px;",
                                    plotOutput("acc_rarefaction_plot_combined", height = "600px")
                                  ),
                                  div(
                                    style = "flex: 1; min-width: 300px;",
                                    plotOutput("acc_coverage_plot_combined", height = "600px")
                                  ),
                                  div(
                                    style = "flex: 1; min-width: 300px;",
                                    plotOutput("acc_richness_plot_combined", height = "600px")
                                  )
                                )
                              )
                     ),
                     
                     tabPanel("Rarefaction/Extrapolation Curve",
                              plotOutput("acc_rarefaction_plot", height = "600px")
                     ),
                     tabPanel("Sample Coverage",
                              plotOutput("acc_coverage_plot", height = "600px")
                     ),
                     tabPanel("Coverage-based Curve",
                              plotOutput("acc_richness_plot", height = "600px")
                     ),
                     tabPanel("Summary Statistics",
                              DTOutput("acc_summary")
                     )
                   )
            )
          )
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
                                shiny::HTML(paste(help_text("detectionHistories_help.html"), collapse = "\n"))
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
                                    "Basic - Simple model specification with standard options" = "basic"  #,
                                    # "Advanced - Full control over model structure and effects" = "advanced"
                                  ),
                                  selected = "basic"
                     ),
                     collapsible = TRUE, 
                     collapsed = TRUE #,
                     # helpText("Basic workflow is recommended for most analyses. Advanced workflow provides more control but requires deeper understanding of model structure.")
                   )
            )
          ),
          
          # Basic Workflow UI
          conditionalPanel(
            condition = "input.occupancy_workflow == 'basic'",
            tabsetPanel(
              id = "basic_workflow_tabs",
              selected = "Model Setup",
              
              # Instructions tab
              tabPanel("Instructions",
                       fluidRow(
                         shinydashboard::box(
                           title = "Single-Species Occupancy Models", 
                           width = 12, 
                           status = "info",
                           shiny::HTML(paste(help_text("singleSpeciesOccupancy_help.html"), collapse = "\n"))
                         )
                       )
              ),
              
              
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
                                  checkboxInput("basic_scale_covariates", "Scale covariates", value = FALSE),
                                  
                                  # ubms Settings
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
              selected = "Model Setup",
              
              # Instructions tab
              tabPanel("Instructions",
                       fluidRow(
                         shinydashboard::box(
                           title = "Single-Species Occupancy Models", 
                           width = 12, 
                           status = "info",
                           shiny::HTML(paste(help_text("singleSpeciesOccupancy_help.html"), collapse = "\n"))
                         )
                       )
              ),
              
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
                       ) #,
                       # fluidRow(
                       #   shinydashboard::box(
                       #     title = "MCMC Diagnostics",
                       #     status = "warning",
                       #     width = 12,
                       #     conditionalPanel(
                       #       condition = "input.adv_model_package == 'ubms'",
                       #       selectInput("adv_parameter_trace", "Select parameter:",
                       #                   choices = NULL
                       #       ),
                       #       plotOutput("adv_trace_plot")
                       #     )
                       #   )
                       # )
              ),
              
              # Advanced Response Plots tab
              # tabPanel("Response Plots",
              #          fluidRow(
              #            column(
              #              width = 3,
              #              selectInput("adv_plot_submodel", "Submodel:",
              #                          choices = c("Detection", "Occupancy")
              #              ),
              #              selectInput("adv_plot_effect", "Effect:",
              #                          choices = NULL
              #              ),
              #              numericInput("adv_ci_level", "Confidence level:",
              #                           value = 0.95, min = 0, max = 1, step = 0.01
              #              ),
              #              checkboxInput("adv_show_data", "Show data points", value = TRUE)
              #            ),
              #            column(
              #              width = 9,
              #              plotOutput("adv_response_plot", height = "600px")
              #            )
              #          )
              # ),
              #  Response Plots -  placeholder
              tabPanel("Response Plots",
                       fluidRow(
                         column(
                           width = 12,
                           shinydashboard::box(
                             title = "Effect Plots - Under Construction", 
                             status = "warning",
                             solidHeader = TRUE,
                             width = NULL,
                             div(
                               style = "text-align: center; padding: 40px 20px;",
                               icon("tools", "fa-4x"),
                               h3("This feature is currently under development"),
                               p("Response plot functionality for advanced models will be available in the future."),
                               p("This will allow visualization of complex effects including quadratic terms, interactions, and random effects.")
                             )
                           )
                         )
                       )
              ),
              
              # # Advanced Spatial Predictions tab
              # tabPanel("Spatial Predictions",
              #          fluidRow(
              #            column(
              #              width = 3,
              #              selectInput("adv_pred_type", "Prediction type:",
              #                          choices = c(
              #                            "Occupancy probability" = "state",
              #                            "Detection probability" = "det"
              #                          )
              #              ),
              #              selectInput("adv_pred_source", "Covariate source:",
              #                          choices = c(
              #                            "Use extracted covariates" = "extracted",
              #                            "Upload custom raster" = "custom"
              #                          )
              #              ),
              #              conditionalPanel(
              #                condition = "input.adv_pred_source == 'custom'",
              #                fileInput("adv_custom_raster", "Upload raster:", accept = c(".tif"))
              #              ),
              #              # MCMC settings for UBMS predictions
              #              conditionalPanel(
              #                condition = "input.adv_model_package == 'ubms'",
              #                numericInput("adv_pred_draws", "Number of draws:", value = 1000, min = 100),
              #                numericInput("adv_pred_chains", "Number of chains:", value = 3, min = 1)
              #              ),
              #              actionButton("adv_run_prediction", "Generate Predictions", class = "btn-primary")
              #            ),
              #            column(
              #              width = 9,
              #              leaflet::leafletOutput("adv_prediction_map", height = "600px")
              #            )
              #          )
              # )
              # Replace the current Spatial Predictions tab content with this placeholder
              tabPanel("Spatial Predictions",
                       fluidRow(
                         column(
                           width = 12,
                           shinydashboard::box(
                             title = "Spatial Predictions - Under Construction", 
                             status = "warning",
                             solidHeader = TRUE,
                             width = NULL,
                             div(
                               style = "text-align: center; padding: 40px 20px;",
                               icon("map-marked-alt", "fa-4x"),
                               h3("This feature is currently under development"),
                               p("Spatial prediction functionality for advanced models will be available in the future."),
                               p("This will allow prediction of occupancy patterns across the landscape based on your model.")
                             )
                           )
                         )
                       )
              ),
            )
          )
        ),
        
        ## Tab: Community Occupancy ----
        shinydashboard::tabItem(
          tabName = "CommunityOccupancy",
          tabsetPanel(
            selected = "Species Selection",
            
            tabPanel("Instructions",
                     fluidRow(
                       shinydashboard::box(
                         title = "Community Occupancy Model Workflow", 
                         width = 12, 
                         status = "info",
                         shiny::HTML(paste(help_text("communityModels_help.html"), collapse = "\n"))
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
                                              selected = "ranef"),
                                  varSelectizeInput("detCovFixed", "Fixed Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("detCovRanef", "Species Random Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("detCovIndep", "Independent Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  checkboxInput("speciesSiteRandomEffectDet", "Species-Site Random Effect on Detection", value = FALSE),
                                  h4("Effort as Detection Covariate"),
                                  checkboxInput("useEffortAsDetCov", "Use Effort as Detection Covariate", value = FALSE),
                                  conditionalPanel(
                                    condition = "input.useEffortAsDetCov == true",
                                    radioButtons("effortDetCovType", "Effort Effect Type:",
                                                 choices = c("Fixed (contant across species)" = "fixed", "Species random effect" = "ranef"),
                                                 selected = "fixed")
                                  )
                           ),
                           column(6,
                                  h4("Occupancy Covariates"),
                                  selectInput("occuIntercept", "Occupancy Intercept:", 
                                              choices = c("fixed", "ranef", "independent"),
                                              selected = "ranef"),
                                  varSelectizeInput("occuCovFixed", "Fixed Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("occuCovRanef", "Species Random Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE)),
                                  varSelectizeInput("occuCovIndep", "Independent Effects", data = NULL, multiple = TRUE, options = list(selectize = TRUE))
                           )
                         )
                       )
                     ),
                     fluidRow(
                       column(12, align = "left",
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
                       
                       
                       # Modified Goodness of Fit tabPanel
                       tabPanel("Goodness of Fit",
                                fluidRow(
                                  # Settings panel - stays fixed on the left
                                  column(width = 3,
                                         wellPanel(
                                           h4("GoF Settings", class = "text-primary"),
                                           numericInput("gof_draws", "Number of posterior draws:", 
                                                        value = 1000, min = 100, max = 10000, step = 100),
                                           checkboxInput("gof_z_cond", "Condition on occupancy (z)", 
                                                         value = TRUE),
                                           selectInput("gof_residual_type", "Residual type:",
                                                       choices = c("Freeman-Tukey" = "FT",
                                                                   "Pearson Chi-squared" = "PearChi2",
                                                                   "Deviance" = "Deviance"),
                                                       selected = "FT"),
                                           tags$div(
                                             style = "margin-top: 20px;",
                                             actionButton("run_gof", "Run Goodness of Fit Test", 
                                                          class = "btn-primary btn-lg btn-block"),
                                             actionButton("run_gof_background", "Run in Background",
                                                          class = "btn-info btn-block",
                                                          style = "margin-top: 10px;")
                                           )
                                         )
                                  ),
                                  
                                  # Results area with tabs on the right
                                  column(width = 9,
                                         tabsetPanel(
                                           # Model Fit Results tab
                                           tabPanel("Model Fit Results",
                                                    # Overall fit box
                                                    shinydashboard::box(
                                                      title = "Overall Model Fit",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      div(
                                                        style = "text-align: center; padding: 20px;",
                                                        shiny::h2("Community-level Bayesian p-value",
                                                                  style = "margin-bottom: 20px;"),
                                                        shiny::h1(textOutput("gof_community_pvalue", inline = TRUE),
                                                                  style = "font-size: 48px; font-weight: bold;"),
                                                        tags$div(
                                                          class = "interpretBox",
                                                          style = "margin: 15px; padding: 15px; border-radius: 5px; font-size: 24px;",
                                                          uiOutput("gof_interpretation")
                                                        )
                                                      )
                                                    ),
                                                    
                                                    # Species-level results
                                                    shinydashboard::box(
                                                      title = "Species-level Results",
                                                      width = NULL,
                                                      status = "info",
                                                      solidHeader = TRUE,
                                                      DT::dataTableOutput("gof_species_table")
                                                    )
                                           ),
                                           
                                           # Residual Plots tab
                                           tabPanel("Residual Plots",
                                                    shinydashboard::box(
                                                      title = "Residual Plots",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      
                                                      # Plot controls
                                                      fluidRow(
                                                        column(4,
                                                               numericInput("gof_plot_columns", 
                                                                            "Number of columns:", 
                                                                            value = 3, min = 1, max = 5)
                                                        ),
                                                        column(4,
                                                               checkboxInput("gof_plot_free_scales", 
                                                                             "Use free scales", 
                                                                             value = FALSE)
                                                        ),
                                                        column(4,
                                                               numericInput("gof_plot_scale", 
                                                                            "Plot size scale:", 
                                                                            value = 1.5, min = 0.5, max = 3, 
                                                                            step = 0.1)
                                                        )
                                                      ),
                                                      
                                                      plotOutput("gof_residual_plot", height = "auto")
                                                    )
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
                                                            ),
                                                            
                                                            selectInput("occupancyColorPalette", "Color Palette:",
                                                                        choices = c("Viridis", "Plasma", "Inferno", "Rocket"),
                                                                        selected = "Viridis",
                                                                        width = "200px"),
                                                            checkboxInput("invertOccupancyColors", "Invert Color Ramp", value = FALSE)
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
                                                            ),
                                                            selectInput("richnessColorPalette", "Color Palette:",
                                                                        choices = c("Viridis", "Plasma", "Inferno", "Rocket"),
                                                                        selected = "Viridis",
                                                                        width = "200px"),
                                                            checkboxInput("invertRichnessColors", "Invert Color Ramp", value = FALSE)
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
                                tabPanel("Percentage of Area Occupied (PAO)",
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
    
    
    # Initialize reactive values ----
    
    restoration_mode <- reactiveVal(FALSE)  # Tracks if we're currently restoring state
    
    wi_data <- reactiveVal(NULL)  #    Reactive value to store imported Wildlife Insights data
    
    current_species_list <- reactiveVal()  #  Create a reactive value for the species list
    
    # for camera trap filter
    # Store original unfiltered data
    original_data <- reactiveVal(list(
      CTtable_sf = NULL,
      recordTable = NULL,
      aggregated_CTtable = NULL
    ))
    
    
    # # store the original record table
    original_record_table <- reactiveVal(NULL)
    # 
    # # Initialize filtered data reactive value
    filtered_data <- reactiveVal(NULL)
    # 
    # # Reactive value to store active filters
    active_filters <- reactiveVal(list())
    # 
    # # Store the observer references in a reactive value
    filter_removal_observers <- reactiveVal(list())
    # 
    # # Create reactive value to store filtered species
    filtered_species <- reactiveVal(NULL)
    # 
    # Create reactive value to store selected species
    # necessary so user doesn't need to open species tab first to run analysis
    selected_species <- reactiveVal(NULL)
    
    
    # container for all active filters
    filter_state <- reactiveVal(list(
      camera_trap = NULL,  # Store camera filter parameters
      temporal = NULL,     # Store temporal filter parameters  
      species = NULL       # Store species filter parameters
    ))
    
    
    # initialize x_label reactive container (species accumulation)
    # x_label <- shiny::reactiveVal("")
    species_accumulation_objects <- reactiveVal(list())
    
    # single species occupancy
    #  reactive values to track models for each workflow
    basic_model <- reactiveVal(NULL)
    advanced_model <- reactiveVal(NULL)
    modelEffects <- reactiveVal(list(detection = list(), occupancy = list()))
    
    # community occupancy
    commOccu_model <- reactiveVal(NULL)
    consoleOutput <- reactiveVal("")
    fitted_comm_model <- reactiveVal(NULL)
    model_summary <- reactiveVal(NULL)
    effect_plots <- reactiveVal(NULL)
    coef_plot <- reactiveVal(NULL)
    gof_results <- reactiveVal(NULL)
    
    
    
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
    
    # ensure original_record_table is properly initialized when the app starts
    observe({
      req(data$recordTable)
      if (is.null(original_record_table())) {
        original_record_table(data$recordTable)
      }
    })
    
    
    observe({
      # Only run this when we have both tables
      req(data$CTtable, data$recordTable)
      
      # Skip during restoration
      if (isolate(restoration_mode())) {
        return()
      }
      
      # Check column names in camera trap table
      ct_cols <- names(data$CTtable)
      ct_cols_lower <- tolower(ct_cols)
      
      # Required column lists
      required_ct_cols <- c(data$stationCol, data$xcol, data$ycol, data$setupCol, data$retrievalCol)
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        required_ct_cols <- c(required_ct_cols, data$cameraCol)
      }
      
      # Check for case mismatches in CT table
      for (col in required_ct_cols) {
        if (!col %in% ct_cols && tolower(col) %in% ct_cols_lower) {
          actual_case <- ct_cols[which(ct_cols_lower == tolower(col))]
          showNotification(paste("Column case mismatch in camera trap table:", col, 
                                 "specified but found as", actual_case), 
                           type = "warning", duration = 10)
        } else if (!col %in% ct_cols) {
          showNotification(paste("Required column missing in camera trap table:", col), 
                           type = "error", duration = NULL)
        }
      }
      
      # Check column names in record table
      record_cols <- names(data$recordTable)
      record_cols_lower <- tolower(record_cols)
      
      # Required columns for record table
      required_record_cols <- c(data$stationCol, data$speciesCol, data$recordDateTimeCol)
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        required_record_cols <- c(required_record_cols, data$cameraCol)
      }
      
      # Check for case mismatches in record table
      for (col in required_record_cols) {
        if (!col %in% record_cols && tolower(col) %in% record_cols_lower) {
          actual_case <- record_cols[which(record_cols_lower == tolower(col))]
          showNotification(paste("Column case mismatch in record table:", col, 
                                 "specified but found as", actual_case), 
                           type = "warning", duration = 10)
        } else if (!col %in% record_cols) {
          showNotification(paste("Required column missing in record table:", col), 
                           type = "error", duration = NULL)
        }
      }
    })
    
    # Tab: Import data ----
    
    ## Import from csv ----
    
    
    
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
    
    
    # output$camerasIndependentImportUI <- renderUI({ 
    #   req(data$CTtable_temp, input$stationCol, input$cameraCol)
    #   
    #   # Only show camerasIndependent if camera column is selected and multiple cameras exist
    #   if (input$cameraCol != "") {
    #     n_cameras_per_station <- table(data$CTtable_temp[[input$stationCol]])
    #     has_multiple_cameras <- any(n_cameras_per_station > 1)
    #     
    #     if (has_multiple_cameras) {
    #       checkboxInput("camerasIndependentImport", 
    #                     "Cameras are independent", 
    #                     value = FALSE)
    #     }
    #   }
    # })
    output$camerasIndependentUI <- renderUI({
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        # Show a warning if camerasIndependent is not defined in data
        if (is.null(data$camerasIndependent)) {
          tagList(
            checkboxInput("camerasIndependent", "Cameras are independent", value = FALSE),
            div(
              class = "alert alert-warning",
              style = "padding: 5px 10px; margin-top: 5px;",
              icon("exclamation-triangle"), 
              "This setting is required when using camera ID column."
            )
          )
        } else {
          checkboxInput("camerasIndependent", "Cameras are independent", value = data$camerasIndependent)
        }
      }
    })
    
    # validate input coordinate system
    observe({
      req(input$crs)
      
      # Clean up CRS input
      clean_crs <- gsub("['\"]", "", input$crs)  # Remove quotes if present
      
      # Validate CRS format
      if (!grepl("^EPSG:\\d+$", clean_crs, ignore.case = TRUE)) {
        showNotification("CRS should be in format 'EPSG:number' (e.g. EPSG:4326)", 
                         type = "warning",
                         duration = 10)
        return()
      }
      
      # Try to create CRS object to validate
      tryCatch({
        sf::st_crs(clean_crs)
        # Store clean CRS in data if valid
        data$crs <- clean_crs
      }, error = function(e) {
        showNotification("Invalid CRS specified. Please check the EPSG code.", 
                         type = "error")
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
      data$crs <- data$crs  # This uses the cleaned/validated CRS from the observer above
      data$setupCol <- input$setupCol
      data$retrievalCol <- input$retrievalCol
      data$CTdateFormat <- input$CTdateFormat
      data$hasProblems <- input$hasProblems
      data$camerasIndependent <- if(!is.null(input$camerasIndependentImport)) input$camerasIndependentImport else FALSE
      
      # Reset original_data() when new data is loaded
      observe({
        req(data$CTtable_sf)     # Need to make sure CTtable_sf is updated first
        original_data(list(
          CTtable_sf = data$CTtable_sf,
          recordTable = data$recordTable,
          aggregated_CTtable = data$aggregated_CTtable
        ))
      })
      
      # Clear active filters
      active_filters(list())
      
      # reset app state (explicit to pass reactive values to function even within observe environment)
      resetAppState(
        # Pass all reactive values
        restoration_mode = restoration_mode,
        wi_data = wi_data,
        current_species_list = current_species_list,
        selected_species = selected_species,
        filtered_species = filtered_species,
        original_data = original_data,
        filtered_data = filtered_data,
        active_filters = active_filters,
        filter_removal_observers = filter_removal_observers,
        original_record_table = original_record_table,
        # x_label = x_label,
        species_accumulation_objects = species_accumulation_objects,
        basic_model = basic_model,
        advanced_model = advanced_model,
        modelEffects = modelEffects,
        commOccu_model = commOccu_model,
        consoleOutput = consoleOutput,
        fitted_comm_model = fitted_comm_model,
        model_summary = model_summary,
        effect_plots = effect_plots,
        coef_plot = coef_plot,
        gof_results = gof_results,
        output = output,
        single_species_occu_objects = single_species_occu_objects,
        spatial_predictions_community = spatial_predictions_community
      )
      
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
      
      
      # Reset original_data() when new data is loaded
      observe({
        req(data$CTtable_sf, data$recordTable)
        original_data(list(
          CTtable_sf = data$CTtable_sf,
          recordTable = data$recordTable,
          aggregated_CTtable = data$aggregated_CTtable
        ))
      })
      
      # Clear active filters
      active_filters(list())
      
      # reset app state (explicit to pass reactive values to function even within observe environment)
      resetAppState(
        # Pass all reactive values
        restoration_mode = restoration_mode,
        wi_data = wi_data,
        current_species_list = current_species_list,
        selected_species = selected_species,
        filtered_species = filtered_species,
        original_data = original_data,
        filtered_data = filtered_data,
        active_filters = active_filters,
        filter_removal_observers = filter_removal_observers,
        original_record_table = original_record_table,
        # x_label = x_label,
        species_accumulation_objects = species_accumulation_objects,
        basic_model = basic_model,
        advanced_model = advanced_model,
        modelEffects = modelEffects,
        commOccu_model = commOccu_model,
        consoleOutput = consoleOutput,
        fitted_comm_model = fitted_comm_model,
        model_summary = model_summary,
        effect_plots = effect_plots,
        coef_plot = coef_plot,
        gof_results = gof_results,
        output = output,
        single_species_occu_objects = single_species_occu_objects,
        spatial_predictions_community = spatial_predictions_community
      )
      
      shiny::showNotification("Record data updated", type = "message")
    })
    
    
    
    
    
    ## Import from Wildlife Insights ----
    
    
    # Add observer for Wildlife Insights browse button
    observeEvent(input$browse_wi_directory, {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        path <- rstudioapi::selectDirectory(
          caption = "Select Wildlife Insights Data Directory",
          label = "Select"
        )
        if (!is.null(path)) {
          updateTextInput(session, "wi_directory", value = path)
        }
      } else {
        showNotification("Directory selection dialog not available. Please enter the path manually.", 
                         type = "warning")
      }
    })
    
    
    # Observer for Wildlife Insights import
    observeEvent(input$wi_import_button, {
      req(input$wi_import_type)
      
      # Clean up previously imported data
      wi_data(NULL)  # Clear the reactive value
      
      # Reset main data reactive values to NULL
      data$CTtable <- NULL
      data$CTtable_sf <- NULL 
      data$aggregated_CTtable <- NULL
      data$aggregated_CTtable_sf <- NULL
      data$recordTable <- NULL
      
      # Reset column specifications to NULL
      data$stationCol <- NULL
      data$cameraCol <- NULL
      data$xcol <- NULL
      data$ycol <- NULL
      data$crs <- NULL
      data$setupCol <- NULL
      data$retrievalCol <- NULL
      data$speciesCol <- NULL
      data$recordDateTimeCol <- NULL
      
      output$filterMap <- leaflet::renderLeaflet(NULL)
      
      # if (!is.null(data$recordTable)) {
      original_record_table(data$recordTable)
      # }
      
      
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
          
          # Reset original_data with the new data
          original_data(list(
            CTtable_sf = data$CTtable_sf,
            recordTable = data$recordTable,
            aggregated_CTtable = data$aggregated_CTtable
          ))
          
          # Clear active filters
          active_filters(list())
          
          
          # Update other necessary reactive values
          data$stationCol <- "placename"
          data$cameraCol <- "camera_id"
          data$xcol <- "longitude"
          data$ycol <- "latitude"
          data$crs <- "EPSG:4326"
          data$setupCol <- "start_date"
          data$retrievalCol <- "end_date"
          data$speciesCol <- "common_name"
          data$recordDateTimeCol <- "timestamp"
          data$camerasIndependent <- input$wi_cameras_independent
          data$recordDateTimeFormat = "ymd HMS"
          data$CTdateFormat <- "ymd HMS"
          
          # Explicitly update the previews
          output$wi_deployment_preview <- DT::renderDT({
            req(wi_imported$CTtable)
            DT::datatable(head(wi_imported$CTtable, 100), 
                          options = list(scrollX = TRUE))
          })
          
          output$wi_detection_preview <- DT::renderDT({
            req(wi_imported$recordTable)
            DT::datatable(head(wi_imported$recordTable, 100), 
                          options = list(scrollX = TRUE))
          })
          
          refreshSpeciesTable()
          
          # reset app state (explicit to pass reactive values to function even within observe environment)
          resetAppState(
            # Pass all reactive values
            restoration_mode = restoration_mode,
            wi_data = wi_data,
            current_species_list = current_species_list,
            selected_species = selected_species,
            filtered_species = filtered_species,
            original_data = original_data,
            filtered_data = filtered_data,
            active_filters = active_filters,
            filter_removal_observers = filter_removal_observers,
            original_record_table = original_record_table,
            # x_label = x_label,
            species_accumulation_objects = species_accumulation_objects,
            basic_model = basic_model,
            advanced_model = advanced_model,
            modelEffects = modelEffects,
            commOccu_model = commOccu_model,
            consoleOutput = consoleOutput,
            fitted_comm_model = fitted_comm_model,
            model_summary = model_summary,
            effect_plots = effect_plots,
            coef_plot = coef_plot,
            gof_results = gof_results,
            output = output,
            single_species_occu_objects = single_species_occu_objects,
            spatial_predictions_community = spatial_predictions_community
          )
          
          # Show success message
          showNotification("Wildlife Insights data imported successfully", type = "message")
          
          
        }, error = function(e) {
          showNotification(paste("Error importing Wildlife Insights data:", e$message), type = "error", duration = NULL)
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
      updateTextInput(session, "crs", value = "EPSG:4326")
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
    
    
    
    # Observer to update species inputs when recordTable changes
    observe({
      req(data$recordTable)
      species_list <- update_species_inputs()
      # cat("Updated species list:", paste(species_list, collapse=", "), "\n")
      current_species_list(species_list)
    })
    
    ## import camtrap-DP ----
    
    # Browse button handler
    observeEvent(input$browse_camtrapdp, {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        path <- rstudioapi::selectDirectory(
          caption = "Select camtrapDP Directory",
          label = "Select"
        )
        if (!is.null(path)) {
          updateTextInput(session, "camtrapdp_directory", value = path)
        }
      } else {
        showNotification("Directory selection dialog not available. Please enter the path manually.", 
                         type = "warning")
      }
    })
    
    # Create a project info summary from metadata
    create_project_info <- function(metadata) {
      # Handle case when metadata is missing
      if (is.null(metadata)) {
        return(HTML("<div class='alert alert-warning'>No metadata available in datapackage.json</div>"))
      }
      
      # Extract key metadata fields with fallbacks
      title <- metadata$title %||% "Untitled Project"
      description <- metadata$description %||% "No description available"
      version <- metadata$version %||% "Unknown"
      
      license <- "Unknown license"
      if (!is.null(metadata$licenses)) {
        # Check if it's a data frame
        if (is.data.frame(metadata$licenses)) {
          # Use data frame syntax for licenses
          if ("name" %in% names(metadata$licenses)) {
            license_names <- metadata$licenses$name
            license <- paste(license_names, collapse = ", ")
          }
        } else if (is.list(metadata$licenses)) {
          # Original code for list type (kept for compatibility)
          license <- paste(sapply(metadata$licenses, function(l) l$name %||% "Unknown license"), collapse = ", ")
        }
      }
      
      
      # Extract temporal coverage
      temporal_start <- "Unknown"
      temporal_end <- "Unknown"
      if (!is.null(metadata$temporal) && !is.null(metadata$temporal$start) && !is.null(metadata$temporal$end)) {
        temporal_start <- metadata$temporal$start
        temporal_end <- metadata$temporal$end
      }
      
      # Extract spatial coverage
      spatial_coverage <- "Unknown"
      if (!is.null(metadata$spatial)) {
        # if (!is.null(metadata$spatial$description)) {
        # spatial_coverage <- metadata$spatial$description
        # } else 
        if (!is.null(metadata$spatial$bbox)) {
          extent <- metadata$spatial$bbox
          spatial_coverage <- sprintf("Lon: %s to %s, Lat: %s to %s",
                                      extent[1], extent[3], extent[2], extent[4])
        }
      }
      
      # Extract authors/contributors
      authors <- "Unknown"
      if (!is.null(metadata$contributors)) {
        # Check if it's a data frame
        if (is.data.frame(metadata$contributors)) {
          # Use data frame syntax for contributors
          author_names <- apply(metadata$contributors, 1, function(row) {
            # Based on structure in mica_small, use 'title' instead of 'name'
            name <- if ("title" %in% names(metadata$contributors)) row["title"] else ""
            
            # alternative approach, seems more reasonable, but doesn't include rightsHolder, publisher
            # name <- if ("lastName" %in% names(metadata$contributors)) paste(row["firstName"], row["lastName"]) else ""
            
            org <- if ("organization" %in% names(metadata$contributors)) row["organization"] else ""
            
            if (name != "" && !is.na(name) && org != "" && !is.na(org)) {
              paste0(name, " (", org, ")")
            } else if (name != "" && !is.na(name)) {
              name
            } else if (org != "" && !is.na(org)) {
              org
            } else {
              "Unknown contributor"
            }
          })
          authors <- paste(author_names, collapse = ", ")
        } else if (is.list(metadata$contributors)) {
          # Original code for list type (kept for compatibility)
          author_names <- sapply(metadata$contributors, function(c) {
            name <- c$name %||% ""
            org <- c$organization %||% ""
            if (name != "" && org != "") {
              paste0(name, " (", org, ")")
            } else if (name != "") {
              name
            } else if (org != "") {
              org
            } else {
              "Unknown contributor"
            }
          })
          authors <- paste(author_names, collapse = ", ")
        }
      }
      
      
      # Extract taxonomic coverage
      taxonomic_info <- "No taxonomic information available"
      if (!is.null(metadata$taxonomic)) {
        tax_data <- metadata$taxonomic
        if (is.data.frame(tax_data) || is.list(tax_data)) {
          n_species <- length(tax_data$scientificName %||% tax_data$species %||% character(0))
          taxonomic_info <- sprintf("%d animal species in dataset", n_species)
        }
      }
      
      # Keywords
      keywords <- "None"
      if (!is.null(metadata$keywords) && length(metadata$keywords) > 0) {
        keywords <- paste(metadata$keywords, collapse = ", ")
      }
      
      # Create HTML output
      html_output <- tags$div(
        class = "project-info",
        style = "padding: 15px;",
        
        tags$h2(title, style = "color: #2c3e50; margin-bottom: 20px;"),
        
        tags$div(
          class = "row",
          
          # Left column - Basic info
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "panel panel-primary",
              tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Project Information")),
              tags$div(
                class = "panel-body",
                tags$p(tags$strong("Description: "), description),
                tags$p(tags$strong("Version: "), version),
                tags$p(tags$strong("License: "), license),
                tags$p(tags$strong("Keywords: "), keywords)
              )
            ),
            
            tags$div(
              class = "panel panel-primary",
              tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Contributors")),
              tags$div(
                class = "panel-body",
                tags$p(authors)
              )
            )
          ),
          
          # Right column - Coverage info
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "panel panel-primary",
              tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Coverage")),
              tags$div(
                class = "panel-body",
                tags$p(tags$strong("Temporal: "), paste(temporal_start, "to", temporal_end)),
                tags$p(tags$strong("Spatial: "), spatial_coverage),
                tags$p(tags$strong("Taxonomic: "), taxonomic_info)
              )
            ),
            
            tags$div(
              class = "panel panel-primary",
              tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Dataset Summary")),
              tags$div(
                class = "panel-body",
                tags$p(tags$strong("Deployments: "), metadata$deployment_count %||% "Unknown"),
                tags$p(tags$strong("Observations: "), metadata$observation_count %||% "Unknown"),
                tags$p(tags$strong("Media Files: "), metadata$media_count %||% "Unknown")
              )
            )
          )
        )
      )
      
      return(html_output)
    }
    
    # Main import handler
    observeEvent(input$camtrapdp_import, {
      req(input$camtrapdp_directory)
      
      # Reset output areas
      output$camtrapdp_status <- renderText("")
      output$camtrapdp_preview_deployments <- DT::renderDT(NULL)
      output$camtrapdp_preview_observations <- DT::renderDT(NULL)
      output$camtrapdp_project_info <- renderUI(NULL)
      
      # Validate directory exists
      if (!dir.exists(input$camtrapdp_directory)) {
        output$camtrapdp_status <- renderText("Error: Directory does not exist")
        return()
      }
      
      # Check required files
      required_files <- c("deployments.csv", "observations.csv", "media.csv", "datapackage.json")
      missing_files <- required_files[!file.exists(file.path(input$camtrapdp_directory, required_files))]
      
      if (length(missing_files) > 0) {
        output$camtrapdp_status <- renderText(paste("Error: Missing required files:", 
                                                    paste(missing_files, collapse = ", ")))
        return()
      }
      
      # Prepare filter_observations parameter
      filter_observations <- switch(input$camtrapdp_filter_observations,
                                    "none" = NULL,
                                    "animals" = TRUE)
      
      # If custom filter is provided, use it instead
      if (!is.null(input$camtrapdp_custom_filter) && 
          nchar(trimws(input$camtrapdp_custom_filter)) > 0) {
        filter_observations <- unlist(strsplit(trimws(input$camtrapdp_custom_filter), "\\s*,\\s*"))
      }
      
      # Import with progress indicator
      withProgress(message = 'Importing camtrapDP data...', value = 0, {
        tryCatch({
          # Set file paths
          deployments_file <- file.path(input$camtrapdp_directory, "deployments.csv")
          observations_file <- file.path(input$camtrapdp_directory, "observations.csv")
          # media_file <- file.path(input$camtrapdp_directory, "media.csv")
          datapackage_file <- file.path(input$camtrapdp_directory, "datapackage.json")
          
          # Call readcamtrapDP function
          imported_data <- readcamtrapDP(
            deployments_file = deployments_file,
            observations_file = observations_file,
            # media_file = media_file,
            datapackage_file = datapackage_file,
            min_gap_hours = input$camtrapdp_min_gap_hours,
            removeNA = input$camtrapdp_remove_na,
            removeEmpty = input$camtrapdp_remove_empty,
            remove_bbox = TRUE, #input$camtrapdp_remove_bbox,
            add_file_path = FALSE,   #input$camtrapdp_add_file_path,
            filter_observations = filter_observations
          )
          
          # Add deployment and observation counts to metadata if not already present
          metadata <- imported_data$metadata
          if (!is.null(metadata)) {
            metadata$deployment_count <- nrow(imported_data$CTtable)
            metadata$observation_count <- nrow(imported_data$recordTable)
            metadata$media_count <- length(unique(imported_data$recordTable$mediaID))
          }
          
          # Store imported data
          data$CTtable <- imported_data$CTtable
          data$recordTable <- imported_data$recordTable
          
          # Set column specifications based on imported data
          data$stationCol <- "locationName" #  "Station"
          
          # camera ID column is optional in camtrapDP
          if ("cameraID" %in% colnames(data$CTtable)) {
            data$cameraCol <- "cameraID"
          } else {
            data$cameraCol <- NULL
          }
          
          # Location coordinates
          data$xcol <- "longitude"
          data$ycol <- "latitude"
          
          # Set coordinate system (default to WGS84 if not specified)
          data$crs <- "EPSG:4326"  # WGS84
          
          # Date columns
          data$setupCol <- "Setup_date"
          data$retrievalCol <- "Retrieval_date"
          data$CTdateFormat <- "ymd HMS"
          
          # Record table columns
          data$speciesCol <- if ("vernacularName_en" %in% colnames(data$recordTable)) {
            "vernacularName_en"
          } else if (any(grepl("^vernacularName_", colnames(data$recordTable)))) {
            grep("^vernacularName_", colnames(data$recordTable), value = TRUE)[1]
          } else {
            "scientificName"
          }
          
          data$recordDateTimeCol <- "DateTimeOriginal"
          data$recordDateTimeFormat <- "ymd HMS"
          data$timeZone <- "UTC"
          
          # Clear exclude filter unless it's specifically set
          data$exclude <- NULL
          
          # Set problem columns
          data$hasProblems <- any(grepl("^Problem[0-9]+_(from|to)$", colnames(data$CTtable)))
          
          # Update camera independence setting
          if ("cameraID" %in% colnames(data$CTtable)) {
            # Check if there are actually multiple cameras per station
            cameras_per_station <- table(data$CTtable$Station)
            has_multiple_cameras <- any(cameras_per_station > 1)
            
            if (has_multiple_cameras) {
              # Apply user setting
              data$camerasIndependent <- input$camtrapdp_cameras_independent
              
              # Notify user
              showNotification(
                "Multiple cameras per station detected. Camera independence setting applied.", 
                type = "message"
              )
            } else {
              # Set to FALSE since no station has multiple cameras
              data$camerasIndependent <- FALSE
              
              if (input$camtrapdp_cameras_independent) {
                # Notify user if they selected independence but it's not applicable
                showNotification(
                  "Camera independence setting not applied - no station has multiple cameras.", 
                  type = "warning"
                )
              }
            }
          } else {
            # No cameraID column exists
            data$camerasIndependent <- FALSE
            
            if (input$camtrapdp_cameras_independent) {
              # Notify user if they selected independence but it's not applicable
              showNotification(
                "Camera independence setting not applied - no camera ID column in data.", 
                type = "warning"
              )
            }
          }
          
          # Create sf object from CTtable
          data$CTtable_sf <- sf::st_as_sf(data$CTtable, 
                                          coords = c(data$xcol, data$ycol), 
                                          crs = data$crs, 
                                          remove = FALSE)
          
          # Create aggregated CT table
          data$aggregated_CTtable <- aggregateCTtableByStation(data$CTtable_sf, data$stationCol)
          
          # Update data previews
          output$camtrapdp_preview_deployments <- DT::renderDT({
            DT::datatable(head(data$CTtable, 100), 
                          options = list(scrollX = TRUE, pageLength = 10),
                          rownames = FALSE)
          })
          
          output$camtrapdp_preview_observations <- DT::renderDT({
            DT::datatable(head(data$recordTable, 100), 
                          options = list(scrollX = TRUE, pageLength = 10),
                          rownames = FALSE)
          })
          
          # Create and render project info
          output$camtrapdp_project_info <- renderUI({
            create_project_info(metadata)
          })
          
          # Update status
          output$camtrapdp_status <- renderText({
            paste0("Import successful!\n",
                   "- Stations: ", nrow(data$CTtable), "\n",
                   "- Records: ", nrow(data$recordTable), "\n",
                   "- Species column: ", data$speciesCol, "\n",
                   "- Species count: ", length(unique(data$recordTable[[data$speciesCol]])))
          })
          
          # Notify user of success
          showNotification("camtrapDP data imported successfully", type = "message")
          
          # Reset original_data with the new data
          original_data(list(
            CTtable_sf = data$CTtable_sf,
            recordTable = data$recordTable,
            aggregated_CTtable = data$aggregated_CTtable
          ))
          
          # Clear active filters
          # active_filters(list())
          
          # Update species list
          update_species_inputs()
          
          
          # reset app state (explicit to pass reactive values to function even within observe environment)
          resetAppState(
            # Pass all reactive values
            restoration_mode = restoration_mode,
            wi_data = wi_data,
            current_species_list = current_species_list,
            selected_species = selected_species,
            filtered_species = filtered_species,
            original_data = original_data,
            filtered_data = filtered_data,
            active_filters = active_filters,
            filter_removal_observers = filter_removal_observers,
            original_record_table = original_record_table,
            # x_label = x_label,
            species_accumulation_objects = species_accumulation_objects,
            basic_model = basic_model,
            advanced_model = advanced_model,
            modelEffects = modelEffects,
            commOccu_model = commOccu_model,
            consoleOutput = consoleOutput,
            fitted_comm_model = fitted_comm_model,
            model_summary = model_summary,
            effect_plots = effect_plots,
            coef_plot = coef_plot,
            gof_results = gof_results,
            output = output,
            single_species_occu_objects = single_species_occu_objects,
            spatial_predictions_community = spatial_predictions_community
          )
          
        }, error = function(e) {
          output$camtrapdp_status <- renderText(paste("Error importing data:", e$message))
          showNotification(paste("Error importing camtrapDP data:", e$message), type = "error")
        })
      })
    })
    
    # Helper function for null coalescing
    "%||%" <- function(x, y) {
      if (is.null(x)) y else x
    }
    
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
        data$study_area <- sf::st_make_valid(study_area)
        
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
    
    
    
    
    # reactive expression for CT table as sf object

    CTtable_sf <- shiny::reactive({
      req(data$CTtable, data$xcol, data$ycol, data$crs)
      
      # Verify the columns actually exist in the data frame
      if (!data$xcol %in% names(data$CTtable) || !data$ycol %in% names(data$CTtable)) {
        showNotification(paste("Error: Coordinate columns", data$xcol, "and/or", data$ycol, 
                               "not found in current data table"), type = "error")
        return(NULL)
      }
      
      sf::st_as_sf(data$CTtable, 
                   coords = c(data$xcol, data$ycol), 
                   crs = data$crs, 
                   remove = FALSE)
    })
    
    # Update CTtable_sf when CT table is updated
    observe({
      data$CTtable_sf <- CTtable_sf()
    })
    
    # Create reactive expression for aggregated_CTtable
    aggregated_CTtable <- shiny::reactive({
      req(data$CTtable_sf, data$stationCol)
      aggregateCTtableByStation(df = data$CTtable_sf, stationCol = data$stationCol)
    })
    
    # Update aggregated_CTtable when CTtable_sf changes
    observe({
      data$aggregated_CTtable <- aggregated_CTtable()
    })
    
    ## Upload file size ----
    
    # Reactive value to store the current max file size
    current_max_size <- shiny::reactiveVal(10)
    
    # Observer to update max file size
    shiny::observeEvent(input$update_max_size, {
      new_max_size <- input$max_file_size * 1024^2  # Convert MB to bytes
      options(shiny.maxRequestSize = new_max_size)
      current_max_size(input$max_file_size)
      shiny::showNotification(paste("Maximum file size updated to", input$max_file_size, "MB"), type = "message")
    })
    
    
    # camera operation matrix ----
    
    
    # # Reactive value for camop
    camop <- shiny::reactiveVal(NULL)
    
    # Create a function to compute camera operation
    compute_camop <- function() {
      req(data$CTtable, data$setupCol, data$retrievalCol, data$CTdateFormat, data$stationCol)
      
      # Show a notification to let users know computation is happening
      id <- showNotification("Computing camera operation matrix...", type = "message", duration = NULL)
      on.exit(removeNotification(id), add = TRUE)
      
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
    }
    
    
    # Render the plotly output for the camera operation matrix
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
    
    
    # Trigger computation of camera operation matrix only when required
    
    # 1. When viewing the Camera Operation tab, and
    #  - camop doesn't exist, or doesn't match camera trap table
    observeEvent(input$tabs, {
      if (input$tabs == "CameraOperation" && is.null(camop())) # ||
      # input$tabs == "CameraOperation" && nrow(camop() != length(unique(data[, data$stationCol])))) 
      {
        camop(compute_camop())
      }
    }, ignoreNULL = TRUE)
    
    # 2. When creating detection history for occupancy models
    observeEvent(input$species_dethist, {
      if (!is.null(input$species_dethist) && input$species_dethist != "" && is.null(camop())) {
        camop(compute_camop())
      }
    }, ignoreNULL = TRUE)
    
    # 3. When creating community occupancy models
    observeEvent(input$createCommunityModel, {
      if (is.null(camop())) {
        camop(compute_camop())
      }
    }, ignoreNULL = TRUE)
    
    # 4. Clear cached camera operation when CTtable or key parameters change
    observeEvent(c(data$CTtable, data$setupCol, data$retrievalCol, data$CTdateFormat, data$stationCol, data$cameraCol, data$camerasIndependent), {
      camop(NULL)
    }, ignoreNULL = TRUE)
    
    # 5. If input$applyFilter is clicked, we need to update camop
    observeEvent(input$applyFilter, {
      camop(NULL)  # Clear first
      # Only recompute if we're currently viewing the CameraOperation tab
      # this would make sense but the commented if() below crashed the app with: 
      # Warning: Error in if: argument is of length zero don't know why
      # now it calculate the camera operation matrix every time the camera traps are filtered. Acceptable for now.
      
      # if(input$tabName == "CameraOperation") {    # this line causes crash with
      camop(compute_camop())
      # }
    }, ignoreNULL = TRUE)
    
    
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
          ggtitle(label = "Number of stations with detections (by species)") +
          geom_vline(xintercept = num_stations(), alpha = 0.5, linetype = 3)
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
    
    
    
    # Tab: camera traps table (aggregated)     ####
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
        mcp <- sf::st_as_sf(sf::st_convex_hull(sf::st_union(data$CTtable_sf)))
        mcp$name <- "Minimum Convex Polygon"
        mcp$n_stations <- num_stations()   #length(unique(data$CTtable_sf[[stationCol]]))
        mcp$area_km2 <- round(sf::st_area(mcp) / 1e6, 3)
        mcp
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
                         label = paste(mcp()[["name"]], "around", num_stations(), "camera trap stations"),
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
        crs = sf::st_crs(data$crs), 
        remove = FALSE
      )
    })
    
    # # Render the species map
    # Modify the existing map observer
    output$maps <- leaflet::renderLeaflet({
      req(detmaps_sf(), input$species_for_map)
      
      # Invalidate when checkboxes change
      input$scale_size
      input$no_record_more_transparent
      
      # Force recreation of the map
      shiny::isolate({
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
        alpha <- if(input$no_record_more_transparent) {
          ifelse(detmaps_sf_data[[species_tmp]] >= 1, 0.9, 0.85)
        } else {
          0.9
        }
        detmaps_sf_data$alpha <- alpha
        
        # Create map
        map_view <- mapview::mapview(
          detmaps_sf_data,
          xcol = data$xcol,
          ycol = data$ycol,
          zcol = species_tmp,
          label = detmaps_sf_data[[data$stationCol]],
          color = hcl.colors(100, "viridis"),
          cex = if(input$scale_size) species_tmp else 10,
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
    })
    
    
    # Helper functions ----
    
    applyAllFilters <- function() {
      # Start with original data
      if (is.null(isolate(original_data())$CTtable_sf) || is.null(isolate(original_data())$recordTable)) {
        showNotification("Cannot apply filters: Original data is incomplete", type = "error")
        return()
      }
      
      # Get original data
      original_CT <- isolate(original_data())$CTtable_sf
      original_records <- original_record_table()
      
      # Get current filter state
      current_filters <- filter_state()
      
      # 1. Apply camera trap filter to get filtered stations
      filtered_CT <- original_CT
      
      if (!is.null(current_filters$camera_trap)) {
        filter_def <- current_filters$camera_trap
        
        tryCatch({
          # Apply filter based on data type
          if (filter_def$type == "numeric") {
            if (filter_def$operator == "between") {
              filtered_CT <- filtered_CT[
                filtered_CT[[filter_def$column]] >= filter_def$value[1] & 
                  filtered_CT[[filter_def$column]] <= filter_def$value[2],
              ]
            } else {
              filtered_CT <- switch(filter_def$operator,
                                    "gt" = filtered_CT[filtered_CT[[filter_def$column]] > filter_def$value, ],
                                    "lt" = filtered_CT[filtered_CT[[filter_def$column]] < filter_def$value, ],
                                    "eq" = filtered_CT[filtered_CT[[filter_def$column]] == filter_def$value, ]
              )
            }
          } else {
            filtered_CT <- filtered_CT[filtered_CT[[filter_def$column]] %in% filter_def$values, ]
          }
          
          # Safety check
          if (nrow(filtered_CT) < 2) {
            showNotification("Filter would remove too many stations! Reverting to original data.", type = "error")
            filtered_CT <- original_CT
          }
        }, error = function(e) {
          showNotification(paste("Error applying camera trap filter:", e$message), type = "error")
          filtered_CT <- original_CT  # Revert to original on error
        })
      }
      
      # Get filtered station list
      filtered_stations <- filtered_CT[[data$stationCol]]
      
      # 2. First filter records by station (performance optimization)
      filtered_records <- original_records[original_records[[data$stationCol]] %in% filtered_stations, ]
      
      # 3. Then apply temporal filter to station-filtered records
      if (!is.null(current_filters$temporal)) {
        tryCatch({
          # Copy temporal filter parameters but use the station-filtered records
          temporal_params <- current_filters$temporal
          temporal_params$recordTable <- filtered_records
          
          # Apply temporal filter
          filtered_records <- do.call(filterRecordTable, temporal_params)
        }, error = function(e) {
          showNotification(paste("Error applying temporal filter:", e$message), type = "error")
        })
      }
      
      # 4. Apply species filter if active
      if (!is.null(current_filters$species)) {
        tryCatch({
          # Filter out excluded species
          filtered_records <- filtered_records[!filtered_records[[data$speciesCol]] %in% current_filters$species, ]
        }, error = function(e) {
          showNotification(paste("Error applying species filter:", e$message), type = "error")
        })
      }
      
      # Update all data structures
      data$CTtable_sf <- filtered_CT
      data$CTtable <- sf::st_drop_geometry(filtered_CT)
      data$recordTable <- filtered_records
      
      # Safely create aggregated table
      tryCatch({
        data$aggregated_CTtable <- aggregateCTtableByStation(filtered_CT, data$stationCol)
        
        # Only create aggregated_CTtable_sf if needed and xcol/ycol are available
        if (!is.null(data$xcol) && !is.null(data$ycol)) {
          if (data$xcol %in% names(data$aggregated_CTtable) && 
              data$ycol %in% names(data$aggregated_CTtable)) {
            data$aggregated_CTtable_sf <- sf::st_as_sf(
              data$aggregated_CTtable,
              coords = c(data$xcol, data$ycol),
              crs = sf::st_crs(filtered_CT), 
              remove = FALSE
            )
          }
        }
      }, error = function(e) {
        showNotification(paste("Error creating aggregated table:", e$message), type = "error")
      })
      
      # Update species inputs after filtering
      update_species_inputs()
    }
    
    
    
    # Reset all filters button handler
    observeEvent(input$resetAllFilters, {
      # Reset filter state
      filter_state(list(camera_trap = NULL, temporal = NULL, species = NULL))
      
      # Reset to original data
      orig_data <- isolate(original_data())
      data$CTtable_sf <- orig_data$CTtable_sf
      data$CTtable <- sf::st_drop_geometry(orig_data$CTtable_sf)
      data$recordTable <- orig_data$recordTable
      data$aggregated_CTtable <- orig_data$aggregated_CTtable
      
      # Reset filtered species
      filtered_species(NULL)
      
      # Reset active filters
      active_filters(list())
      
      # Update species inputs
      update_species_inputs()
      
      showNotification("All filters have been reset", type = "message")
    })
    
    update_species_inputs <- function() {
      req(data$recordTable, data$speciesCol)
      species_list <- sort(unique(data$recordTable[[data$speciesCol]]))
      # Filter out excluded species
      if (!is.null(data$exclude)) {
        species_list <- species_list[!species_list %in% data$exclude]
      }
      
      # Update species list for species detection maps
      updateSelectInput(session, "species_for_map", 
                        choices = c("n_species", species_list),
                        selected = if("n_species" %in% input$species_for_map) "n_species" else species_list[1])
      
      # Update species selection for activity plots
      updateSelectInput(session, "ad_species", 
                        choices = species_list,
                        selected = if(input$ad_species %in% species_list) input$ad_species else species_list[1])
      
      # Update species selection for activity overlap plots
      updateSelectInput(session, "speciesA", 
                        choices = species_list,
                        selected = if(input$speciesA %in% species_list) input$speciesA else species_list[1])
      
      updateSelectInput(session, "speciesB", 
                        choices = species_list,
                        selected = if(input$speciesB %in% species_list) input$speciesB else 
                          species_list[min(2, length(species_list))])
      
      # Update species selection for detection history
      updateSelectInput(session, "species_dethist", 
                        choices = species_list,
                        selected = if(input$species_dethist %in% species_list) input$species_dethist else species_list[1])
      
      # Update species selection for species accumulation curves
      updateSelectInput(session, "acc_speciesSubset", 
                        choices = species_list)
      
      # Update species selection for single-species occupancy models
      # This uses the same species_dethist dropdown above, but may need to trigger recalculation
      
      # Update species selection for community occupancy models
      # This doesn't directly update a dropdown but refreshes the table that's used for selection
      if (exists("speciesFilterTable") && !is.null(output$speciesFilterTable)) {
        proxy <- DT::dataTableProxy("speciesFilterTable")
        if (!is.null(proxy)) {
          DT::replaceData(proxy, species_summary_for_filter(), rownames = FALSE)
        }
      }
      
      return(species_list)
    }
    
    
    
    
    # Tab: Filter Stations ####
    
    
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
                          c(#data$stationCol,    # uncomment to allow filtering by station name
                            data$setupCol, 
                            data$retrievalCol, 
                            "geometry")
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
    
    
    
    # Camera Trap Filtering
    
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
        
        # Pre-check if the filter would remove all stations
        filtered_CT <- NULL
        if (filter_def$operator == "between") {
          filtered_CT <- data$CTtable_sf[
            data$CTtable_sf[[filter_def$column]] >= filter_def$value[1] & 
              data$CTtable_sf[[filter_def$column]] <= filter_def$value[2],
          ]
        } else {
          filtered_CT <- switch(filter_def$operator,
                                "gt" = data$CTtable_sf[data$CTtable_sf[[filter_def$column]] > filter_def$value, ],
                                "lt" = data$CTtable_sf[data$CTtable_sf[[filter_def$column]] < filter_def$value, ],
                                "eq" = data$CTtable_sf[data$CTtable_sf[[filter_def$column]] == filter_def$value, ]
          )
        }
        
        # Check if filter would remove all stations
        if (nrow(filtered_CT) < 2) {
          showNotification("Filter would remove too many stations! Filter not applied.", type = "error")
          return()
        }
      } else {
        filter_def$values <- input$categoryValues
        
        # Pre-check if the categorical filter would remove all stations
        filtered_CT <- data$CTtable_sf[data$CTtable_sf[[filter_def$column]] %in% filter_def$values, ]
        
        if (nrow(filtered_CT) < 2) {
          showNotification("Filter would remove too many stations! Filter not applied.", type = "error")
          return()
        }
      }
      
      # Store the filter definition in both filter_state and active_filters
      current_filters <- filter_state()
      current_filters$camera_trap <- filter_def
      filter_state(current_filters)
      
      # Update active_filters for display
      current_active_filters <- active_filters()
      current_active_filters[[input$filterColumn]] <- filter_def
      active_filters(current_active_filters)
      
      # Apply all filters
      applyAllFilters()
      
      
      # Show notification
      filtered_stations <- length(unique(data$CTtable_sf[[data$stationCol]]))
      original_stations <- length(unique(original_data()$CTtable_sf[[data$stationCol]]))
      filtered_records <- nrow(data$recordTable)
      original_records <- nrow(original_data()$recordTable)
      
      showNotification(sprintf("Filtered to %d stations (%.1f%%) and %d records (%.1f%%)", 
                               filtered_stations, 
                               100 * filtered_stations / original_stations,
                               filtered_records,
                               100 * filtered_records / original_records), 
                       type = "message")
    })
    
    
    
    
    
    # Clear camera trap filters
    observeEvent(input$clearAllFilters, {
      active_filters(list())
      
      # Update filter state
      current_filters <- filter_state()
      current_filters$camera_trap <- NULL
      filter_state(current_filters)
      
      # Apply all filters
      applyAllFilters()
      
      showNotification("Camera trap filters cleared", type = "message")
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
    
    
    
    # # Handle individual filter removal
    createFilterRemovalObservers <- function() {
      # Get current filters
      current_filters <- active_filters()
      current_filter_names <- names(current_filters)
      
      # Get existing observer names
      existing_observers <- names(filter_removal_observers())
      
      # Remove observers for filters that don't exist anymore
      observers_to_keep <- filter_removal_observers()[names(filter_removal_observers()) %in% current_filter_names]
      
      # Create new observers for new filters
      new_observers <- list()
      for (col_name in current_filter_names) {
        if (!col_name %in% existing_observers) {
          new_observers[[col_name]] <- observeEvent(input[[paste0("remove_", col_name)]], {
            # This code runs when the remove button is clicked
            
            # Update filter_state
            filters <- filter_state()
            filters$camera_trap <- NULL
            filter_state(filters)
            
            # Update active_filters (original functionality)
            updated_filters <- active_filters()
            updated_filters[[col_name]] <- NULL
            active_filters(updated_filters)
            
            # Apply all filters
            applyAllFilters()
            
            showNotification(paste("Filter removed:", col_name), type = "message")
          }, ignoreInit = TRUE, once = FALSE)
        }
      }
      
      # Combine existing and new observers
      all_observers <- c(observers_to_keep, new_observers)
      filter_removal_observers(all_observers)
    }
    
    # Observer for when active filters change - calls the function above
    observe({
      req(active_filters())
      createFilterRemovalObservers()
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
      curr_stations <- length(unique(data$CTtable_sf[[data$stationCol]])) #nrow(data$CTtable_sf)
      orig_stations <- length(unique(orig$CTtable_sf[[data$stationCol]])) #nrow(orig$CTtable_sf)
      curr_records <- nrow(data$recordTable)
      orig_records <- nrow(orig$recordTable)
      
      # Calculate filtered out counts
      filtered_stations <- orig_stations - curr_stations
      filtered_records <- orig_records - curr_records
      
      # Safety check for valid percentages
      station_percent <- if(orig_stations > 0) {
        sprintf(" (%.1f%%)", curr_stations/orig_stations * 100)
      } else {
        " (N/A)"
      }
      
      record_percent <- if(orig_records > 0) {
        sprintf(" (%.1f%%)", curr_records/orig_records * 100)
      } else {
        " (N/A)"
      }
      
      div(
        class = "well",
        tags$p(
          "Stations: ", 
          tags$strong(curr_stations), " of ", tags$strong(orig_stations),
          station_percent
        ),
        tags$p(
          "Records: ",
          tags$strong(curr_records), " of ", tags$strong(orig_records),
          record_percent
        ),
        tags$p(
          "Filtered out: ",
          tags$strong(filtered_stations), " stations and ",
          tags$strong(filtered_records), " records"
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
    
    # Tab: Filter records temporally ####
    
    output$camerasIndependentUI <- renderUI({
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        checkboxInput("camerasIndependent", "Cameras are independent", value = FALSE)
      }
    })
    
    
    # Initialize the original_record_table when the app starts
    observe({
      req(data$recordTable)
      if (is.null(original_record_table())) {
        original_record_table(data$recordTable)
      }
    })
    
    
    # # Handle the restore button click
    observeEvent(input$restoreOriginalRecordTable, {
      req(original_record_table())
      
      # Update filter state to remove temporal filter
      current_filters <- filter_state()
      current_filters$temporal <- NULL
      filter_state(current_filters)
      
      # Apply all filters
      applyAllFilters()
      
      showNotification("Temporal filter removed", type = "message")
    })
    
    
    
    
    
    
    # Temporal Filtering
    observeEvent(input$runTemporalFilter, {
      req(original_record_table())
      
      # Basic column checks
      required_cols <- c(data$stationCol, data$speciesCol, data$recordDateTimeCol)
      missing_cols <- required_cols[!required_cols %in% names(original_record_table())]
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Error: Missing required columns:", paste(missing_cols, collapse=", ")), 
                         type = "error")
        return()
      }
      
      # Create temporal filter parameters
      temporal_params <- list(
        recordTable = original_record_table(),  # Start with original table
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
      
      # Add cameraCol and camerasIndependent only if cameraCol is defined and not empty
      if (!is.null(data$cameraCol) && data$cameraCol != "") {
        if (!data$cameraCol %in% names(original_record_table())) {
          showNotification(paste("Error: Camera column", data$cameraCol, "not found"), type = "error")
          return()
        }
        
        temporal_params$cameraCol <- data$cameraCol
        
        # Check if camerasIndependent is available in input or data
        if (!is.null(input$camerasIndependent)) {
          temporal_params$camerasIndependent <- input$camerasIndependent
        } else if (!is.null(data$camerasIndependent)) {
          temporal_params$camerasIndependent <- data$camerasIndependent
        } else {
          showNotification(
            "Error: camerasIndependent parameter is required when using camera ID column.", 
            type = "error"
          )
          return()
        }
      }
      
      # Add optional arguments only if they are not NULL or empty
      if (!is.null(data$exclude) && length(data$exclude) > 0) {
        temporal_params$exclude <- data$exclude
      }
      
      # Update filter state
      current_filters <- filter_state()
      current_filters$temporal <- temporal_params
      filter_state(current_filters)
      
      # Show progress during filtering
      withProgress(message = 'Applying temporal filter...', value = 0, {
        tryCatch({
          # Store original record count for comparison
          orig_count <- nrow(original_record_table())
          
          # Apply all filters (this will update data$recordTable)
          applyAllFilters()
          
          # Get filtered record count
          filtered_count <- nrow(data$recordTable)
          
          # Show success notification with statistics
          showNotification(sprintf("Temporal filtering: %d of %d records retained (%.1f%%)", 
                                   filtered_count, 
                                   orig_count,
                                   100 * filtered_count / orig_count), 
                           type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error in temporal filtering:", e$message), type = "error")
        })
      })
    })
    
    
    # Handle the restore button click
    observeEvent(input$restoreOriginalRecordTable, {
      req(original_record_table())
      
      # Update filter state to remove temporal filter
      current_filters <- filter_state()
      current_filters$temporal <- NULL
      filter_state(current_filters)
      
      # Apply all filters
      applyAllFilters()
      
      showNotification("Temporal filter removed", type = "message")
    })
    
    # Update the filtered record table output
    output$filteredRecordTable <- DT::renderDT({
      req(data$recordTable)
      
      # Return the filtered records table
      DT::datatable(
        data$recordTable,
        options = list(
          pageLength = 10, 
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    
    
    # create temporal filter summary
    output$temporalFilterSummary <- renderUI({
      # More explicit requirements that will react to changes
      req(data$recordTable, original_record_table(), filter_state())
      
      # Get counts
      orig_records <- nrow(original_record_table())
      curr_records <- nrow(data$recordTable)
      removed_records <- orig_records - curr_records
      
      # Calculate percentage
      record_percent <- if(orig_records > 0) {
        sprintf(" (%.1f%%)", curr_records/orig_records * 100)
      } else {
        " (N/A)"
      }
      
      div(
        class = "well",
        tags$p(
          "Records: ", 
          tags$strong(curr_records), " of ", tags$strong(orig_records),
          record_percent
        ),
        tags$p(
          "Filtered out: ",
          tags$strong(removed_records), " records",
          if(orig_records > 0) sprintf(" (%.1f%%)", removed_records/orig_records * 100) else " (N/A)"
        ),
        # Add current filter settings if applied
        if(!is.null(filter_state()$temporal)) {
          tags$p(
            "Active filter: ", 
            tags$strong(paste0(filter_state()$temporal$minDeltaTime, " minutes minimum, ",
                               "compared to ", filter_state()$temporal$deltaTimeComparedTo)),
            style = "color: #337ab7;"
          )
        } else {
          tags$p("No temporal filtering applied.", style = "color: #777;")
        }
      )
    })
    
    
    # temporal filter table to show impact by species
    
    output$temporalFilterTable <- DT::renderDT({
      req(data$recordTable, original_record_table(), data$speciesCol)
      
      # Count records per species in original data
      original_counts <- table(original_record_table()[[data$speciesCol]])
      
      # Count records per species in filtered data
      filtered_counts <- table(data$recordTable[[data$speciesCol]])
      
      # Get all species from both tables
      species_names <- sort(unique(c(names(original_counts), names(filtered_counts))))
      
      # Create summary dataframe
      summary_df <- data.frame(
        Species = species_names,
        Original_Records = sapply(species_names, function(sp) {
          if(sp %in% names(original_counts)) as.vector(original_counts[sp]) else 0
        }),
        Filtered_Records = sapply(species_names, function(sp) {
          if(sp %in% names(filtered_counts)) as.vector(filtered_counts[sp]) else 0
        })
      )
      
      # Calculate removed records and percentages
      summary_df$Removed_Records <- summary_df$Original_Records - summary_df$Filtered_Records
      summary_df$Percent_Retained <- ifelse(
        summary_df$Original_Records > 0,
        round(summary_df$Filtered_Records / summary_df$Original_Records * 100, 1),
        NA
      )
      
      # Sort by original record count
      summary_df <- summary_df[order(-summary_df$Original_Records), ]
      
      # Create datatable
      DT::datatable(
        summary_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # NULL coalesce  %||% operator
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }
    
    
    # Tab: Species filter ####
    
    
    
    # Initialize filtered species when data changes
    observe({
      req(data$recordTable, data$speciesCol)
      # Reset filtered species when data changes
      filtered_species(NULL)
    })
    
    # Create species summary table for filtering
    species_summary_for_filter <- reactive({
      # Add explicit dependencies on key data changes
      data_key <- list(
        record_table_rows = if (!is.null(data$recordTable)) nrow(data$recordTable) else 0,
        species_col = data$speciesCol,
        station_col = data$stationCol #,
        # original_table_id = if (!is.null(original_record_table())) digest::digest(head(original_record_table(), 5)) else NULL
      )
      
      # Validate inputs more strictly
      shiny::validate(
        shiny::need(!is.null(data$recordTable) && nrow(data$recordTable) > 0, "No record data available"),
        shiny::need(!is.null(data$speciesCol), "Species column not defined"),
        shiny::need(!is.null(data$stationCol), "Station column not defined"),
        shiny::need(!is.null(original_record_table()) && nrow(original_record_table()) > 0, "Original record table not available")
      )
      
      # Check if species column exists in both data frames
      if (!data$speciesCol %in% names(data$recordTable) || 
          !data$speciesCol %in% names(original_record_table())) {
        return(data.frame(
          Species = character(0),
          Total_Records = numeric(0),
          Stations = numeric(0),
          Percent_of_Records = numeric(0),
          Current_Records = numeric(0),
          Status = character(0)
        ))
      }
      
      # Use base R for safer grouping operations
      species_list <- unique(original_record_table()[[data$speciesCol]])
      
      # Create data frame with summary info
      result <- data.frame(
        Species = species_list,
        Total_Records = sapply(species_list, function(s) {
          sum(original_record_table()[[data$speciesCol]] == s)
        }),
        Stations = sapply(species_list, function(s) {
          length(unique(original_record_table()[original_record_table()[[data$speciesCol]] == s, data$stationCol]))
        }),
        stringsAsFactors = FALSE
      )
      
      # Calculate percentages
      result$Percent_of_Records <- round(result$Total_Records / nrow(original_record_table()) * 100, 2)
      
      # Count records in current filtered data
      result$Current_Records <- sapply(species_list, function(s) {
        sum(data$recordTable[[data$speciesCol]] == s)
      })
      
      # Calculate filtering status
      result$Status <- ifelse(result$Current_Records == 0, "Filtered Out", "Active")
      
      # Set filtered status based on filtered_species() reactiveVal
      if (!is.null(filtered_species())) {
        excluded <- filtered_species()
        result$Status <- ifelse(result$Species %in% excluded, "Filtered Out", "Active")
      }
      
      # Sort by total records
      result <- result[order(-result$Total_Records), ]
      
      return(result)
    })
    
    refreshSpeciesTable <- function() {
      # Reset filterSpecies interface
      filtered_species(NULL)
      
      # Force refresh of species table
      shinyjs::reset("speciesFilterTable_rows_selected")
      
      # Use proxy to reload table data if available
      if (exists("speciesFilterTable", output) && !is.null(output$speciesFilterTable)) {
        proxy <- DT::dataTableProxy("speciesFilterTable")
        if (!is.null(proxy)) {
          DT::replaceData(proxy, species_summary_for_filter(), rownames = FALSE)
        }
      }
    }
    
    
    # Render the species filter table
    output$speciesFilterTable <- DT::renderDT({
      req(species_summary_for_filter())
      
      species_data <- species_summary_for_filter()
      
      DT::datatable(
        species_data,
        selection = 'multiple',
        options = list(
          pageLength = 15,
          order = list(list(1, 'desc'))  # Sort by Total_Records by default
        ),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          'Status',
          target = 'row',
          backgroundColor = DT::styleEqual(
            c("Active", "Filtered Out"),
            c('#e6ffe6', '#ffe6e6')  # Light green for active, light red for filtered out
          )
        )
    })
    
    
    output$speciesFilterSummary <- renderUI({
      req(species_summary_for_filter(), original_record_table())
      
      summary_df <- species_summary_for_filter()
      total_species <- nrow(summary_df)
      active_species <- sum(summary_df$Status == "Active")
      filtered_species_count <- sum(summary_df$Status == "Filtered Out")
      
      total_records <- sum(summary_df$Total_Records)
      active_records <- sum(summary_df$Current_Records)
      removed_records <- total_records - active_records
      
      # Calculate percentages
      species_percent <- if(total_species > 0) {
        sprintf(" (%.1f%%)", active_species/total_species * 100)
      } else {
        " (N/A)"
      }
      
      records_percent <- if(total_records > 0) {
        sprintf(" (%.1f%%)", active_records/total_records * 100)
      } else {
        " (N/A)"
      }
      
      div(
        class = "well",
        tags$p(
          "Species: ", 
          tags$strong(active_species), " of ", tags$strong(total_species),
          species_percent
        ),
        tags$p(
          "Records: ",
          tags$strong(active_records), " of ", tags$strong(total_records),
          records_percent
        ),
        tags$p(
          "Filtered out: ",
          tags$strong(filtered_species_count), " species and ",
          tags$strong(removed_records), " records"
        )
      )
    })
    
    
    # Species Filtering - Keep selected species
    observeEvent(input$keepSelectedSpecies, {
      req(species_summary_for_filter(), input$speciesFilterTable_rows_selected)
      
      # Get selected species
      selected_idx <- input$speciesFilterTable_rows_selected
      selected_species <- species_summary_for_filter()$Species[selected_idx]
      
      if (length(selected_species) == 0) {
        showNotification("No species selected", type = "warning")
        return()
      }
      
      # Get all species
      all_species <- species_summary_for_filter()$Species
      
      # Get species to exclude (those not selected)
      exclude_species <- setdiff(all_species, selected_species)
      
      # Update filtered species
      filtered_species(exclude_species)
      
      # Update filter state
      current_filters <- filter_state()
      current_filters$species <- exclude_species
      filter_state(current_filters)
      
      # Apply all filters
      applyAllFilters()
      
      showNotification(
        paste("Keeping", length(selected_species), "species,", 
              length(exclude_species), "species removed"), 
        type = "message"
      )
    })
    
    # Species Filtering - Remove selected species
    observeEvent(input$removeSelectedSpecies, {
      req(species_summary_for_filter(), input$speciesFilterTable_rows_selected)
      
      # Get selected species
      selected_idx <- input$speciesFilterTable_rows_selected
      selected_species <- species_summary_for_filter()$Species[selected_idx]
      
      if (length(selected_species) == 0) {
        showNotification("No species selected", type = "warning")
        return()
      }
      
      # Update filtered species list
      current_filtered <- filtered_species()
      if (is.null(current_filtered)) {
        new_filtered <- selected_species
      } else {
        new_filtered <- union(current_filtered, selected_species)
      }
      filtered_species(new_filtered)
      
      # Update filter state
      current_filters <- filter_state()
      current_filters$species <- new_filtered
      filter_state(current_filters)
      
      # Apply all filters
      applyAllFilters()
      
      showNotification(
        paste("Removed", length(selected_species), "species"), 
        type = "message"
      )
    })
    
    # Reset species filtering
    observeEvent(input$resetSpeciesFilter, {
      req(original_record_table())
      
      # Reset filtered species
      filtered_species(NULL)
      
      # Update filter state
      current_filters <- filter_state()
      current_filters$species <- NULL
      filter_state(current_filters)
      
      # Apply all filters
      applyAllFilters()
      
      showNotification("Species filters cleared", type = "message")
    })
    
    # Tab: extract covariates   ####
    
    
    # Reactive expression for aggregated CT table as sf object
    aggregated_CTtable_sf <- reactive({
      req(data$aggregated_CTtable, data$xcol, data$ycol, data$CTtable_sf)
      sf::st_as_sf(data$aggregated_CTtable, 
                   coords = c(data$xcol, data$ycol), 
                   crs = sf::st_crs(data$CTtable_sf), 
                   remove = FALSE)
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
        
        ct_sf <- aggregated_CTtable_sf()
        
        # Get value range from raster and camera traps
        value_range_raster <- terra::minmax(raster)
        value_range_station <- range(ct_sf[[input$predictionRasterBand]])
        # get combined value range
        value_range <- c(min(value_range_raster, value_range_station),
                         max(value_range_raster, value_range_station))
        
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
    
    
    # Helper function to clip/mask prediction rasters
    clip_prediction_rasters <- function(rasters, prediction_extent) {
      if (is.null(prediction_extent)) return(rasters)
      
      # Safely handle transformations and clipping
      tryCatch({
        # Get CRS information
        raster_crs <- terra::crs(rasters)
        extent_crs <- sf::st_crs(prediction_extent)
        
        # Check if we're transforming between geographic and projected systems
        is_raster_geo <- grepl("\\+proj=longlat", raster_crs) || grepl("geographic", raster_crs, ignore.case = TRUE)
        is_extent_geo <- grepl("\\+proj=longlat", extent_crs$proj4string) || grepl("geographic", extent_crs$input, ignore.case = TRUE)
        
        # Special handling for transformation between geographic and projected systems
        if (is_raster_geo != is_extent_geo) {
          # Create a temporary buffer around the extent in its own CRS
          # This helps ensure overlap after transformation
          prediction_extent_buffered <- sf::st_buffer(prediction_extent, dist = if(is_extent_geo) 0.1 else 10000)
          
          # Get the bounding box of the prediction extent
          bbox <- sf::st_bbox(prediction_extent_buffered)
          bbox_poly <- sf::st_as_sfc(bbox, crs = extent_crs)
          
          # Safety check: ensure coordinates are reasonable
          bbox_coords <- sf::st_coordinates(bbox_poly)[,1:2]
          if (any(!is.finite(bbox_coords))) {
            return(rasters)  # Return original rasters if coordinates invalid
          }
          
          # Transform the bbox to raster CRS
          bbox_transformed <- sf::st_transform(bbox_poly, raster_crs)
          
          # Check for valid transformation
          transformed_coords <- sf::st_coordinates(bbox_transformed)[,1:2]
          if (any(!is.finite(transformed_coords))) {
            return(rasters)  # Return original rasters if transformation failed
          }
          
          # Extract the bbox from the transformed geometry
          t_bbox <- sf::st_bbox(bbox_transformed)
          
          # Create an extent object for cropping
          crop_ext <- terra::ext(t_bbox["xmin"], t_bbox["xmax"], t_bbox["ymin"], t_bbox["ymax"])
          
          # Crop using the bbox extent
          return(terra::crop(rasters, crop_ext))
          
        } else {
          # Standard approach when both are in similar coordinate systems
          # Transform the prediction extent to match the raster's CRS
          extent_transformed <- sf::st_transform(prediction_extent, raster_crs)
          
          # Convert to a terra vector object for cropping and masking
          extent_vect <- terra::vect(extent_transformed)
          
          # First crop to bounding box for efficiency
          rasters_cropped <- terra::crop(rasters, extent_vect)
          
          # Then mask to actual polygon shape
          return(terra::mask(rasters_cropped, extent_vect))
        }
      }, error = function(e) {
        # If clipping fails, return original rasters
        warning(paste("Failed to clip rasters:", e$message))
        return(rasters)
      })
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
            # Normalize directory path to use forward slashes
            covariate_args$directory <- gsub("\\", "/", input$directory, fixed = TRUE)
            covariate_args$recursive <- input$recursive
          } else {
            req(input$filenames)
            # Split by comma and optional whitespace, then normalize each path
            raw_filenames <- unlist(strsplit(input$filenames, ",\\s*"))
            covariate_args$filenames <- gsub("\\", "/", raw_filenames, fixed = TRUE)
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
          
          # 1. Handle CTtable updates
          if (!is.null(covariates_extract_list$CTtable)) {
            # Identify new covariate columns by comparing with original columns
            original_cols <- data$original_columns
            new_cols <- setdiff(names(sf::st_drop_geometry(covariates_extract_list$CTtable)), original_cols)
            
            # Find columns that already exist in the current table
            existing_cols <- intersect(names(data$CTtable_sf), new_cols)
            
            if (length(existing_cols) > 0) {
              # Notify user about replacement
              showNotification(
                paste("Replacing existing covariate columns:", paste(existing_cols, collapse=", ")),
                type = "warning",
                duration = 10
              )
              
              # Remove existing covariate columns
              data$CTtable_sf <- data$CTtable_sf[, !names(data$CTtable_sf) %in% existing_cols]
            }
            
            # Add new covariate columns
            data$CTtable_sf <- cbind(data$CTtable_sf, 
                                     sf::st_drop_geometry(covariates_extract_list$CTtable)[, new_cols, drop = FALSE])
          }
          
          # 2. Handle original rasters
          if (!is.null(covariates_extract_list$originalRaster)) {
            new_orig_rasters <- covariates_extract_list$originalRaster
            
            if (!is.null(data$original_rasters)) {
              # Get names from existing and new rasters
              existing_orig_names <- names(data$original_rasters)
              new_orig_names <- names(new_orig_rasters)
              
              # Identify overlapping names
              overlapping_orig_names <- intersect(existing_orig_names, new_orig_names)
              
              if (length(overlapping_orig_names) > 0) {
                # Notify user about replacement
                showNotification(
                  paste("Replacing existing original rasters:", paste(overlapping_orig_names, collapse=", ")),
                  type = "warning",
                  duration = 10
                )
                
                # Remove overlapping rasters from existing list
                data$original_rasters <- data$original_rasters[!names(data$original_rasters) %in% overlapping_orig_names]
              }
              
              # Combine lists (non-overlapping existing + all new)
              data$original_rasters <- c(data$original_rasters, new_orig_rasters)
            } else {
              data$original_rasters <- new_orig_rasters
            }
          }
          
          # 3. Handle prediction rasters
          if (!is.null(covariates_extract_list$predictionRaster)) {
            # Apply clipping if requested
            pred_rasts <- if (input$predictionExtent != "none" && !is.null(prediction_extent)) {
              clip_prediction_rasters(covariates_extract_list$predictionRaster, 
                                      prediction_extent)
            } else {
              covariates_extract_list$predictionRaster
            }
            
            if (!is.null(data$prediction_raster)) {
              # Get names from existing and new rasters
              existing_pred_names <- names(data$prediction_raster)
              new_pred_names <- names(pred_rasts)
              
              # Find overlapping layers
              overlapping_pred_names <- intersect(existing_pred_names, new_pred_names)
              
              if (length(overlapping_pred_names) > 0) {
                # Notify user about replacement
                showNotification(
                  paste("Replacing existing prediction layers:", paste(overlapping_pred_names, collapse=", ")),
                  type = "warning",
                  duration = 10
                )
                
                # Keep only non-overlapping layers from existing raster
                keep_layers <- which(!existing_pred_names %in% new_pred_names)
                
                if (length(keep_layers) > 0) {
                  # Combine non-overlapping existing layers with new layers
                  data$prediction_raster <- c(data$prediction_raster[[keep_layers]], pred_rasts)
                } else {
                  # If all existing layers overlap, just use new layers
                  data$prediction_raster <- pred_rasts
                }
              } else {
                # No overlap, just combine
                data$prediction_raster <- c(data$prediction_raster, pred_rasts)
              }
            } else {
              data$prediction_raster <- pred_rasts
            }
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
          buffered_sf <- sf::st_buffer(data$CTtable_sf, dist = input$bufferPrediction)
          
          # Get bounding box and transform to EPSG:4326 if needed
          if (sf::st_crs(buffered_sf) != sf::st_crs(4326)) {
            buffered_sf_4326 <- sf::st_transform(buffered_sf, 4326)
          } else {
            buffered_sf_4326 <- buffered_sf
          }
          
          elevation_rast <- elevatr::get_elev_raster(
            locations = buffered_sf_4326,
            z = as.numeric(input$elevationZoom),
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
          
          # Get names of new terrain columns
          new_terrain_cols <- names(terrain_values)[-1]  # Exclude ID column
          
          # 1. Handle CTtable updates - check for existing columns
          existing_cols <- intersect(names(data$CTtable_sf), new_terrain_cols)
          if (length(existing_cols) > 0) {
            # Notify user about replacement
            showNotification(
              paste("Replacing existing terrain columns:", paste(existing_cols, collapse=", ")),
              type = "warning",
              duration = 10
            )
            
            # Remove existing terrain columns
            data$CTtable_sf <- data$CTtable_sf[, !names(data$CTtable_sf) %in% existing_cols]
          }
          
          # Add new terrain columns
          data$CTtable_sf <- cbind(data$CTtable_sf, terrain_values[, -1, drop = FALSE])
          
          # Get the median location of cameras as representative point
          center_lat <- stats::median(st_coordinates(buffered_sf_4326)[, "Y"]) 
          center_lon <- stats::median(st_coordinates(buffered_sf_4326)[, "X"]) 
          
          # derive UTM zone
          utm_epsg <- ifelse(center_lat > 0,
                             32600 + floor((center_lon + 180)/6) + 1,  # Northern
                             32700 + floor((center_lon + 180)/6) + 1)  # Southern
          
          elevation_rast_utm <- terra::project(elevation_rast, paste0("EPSG:", utm_epsg))
          
          # 2. Handle original rasters
          if (!is.null(data$original_rasters)) {
            # Get names from existing and new rasters
            existing_orig_names <- names(data$original_rasters)
            new_orig_names <- names(terrain_rasts)
            
            # Identify overlapping names
            overlapping_orig_names <- intersect(existing_orig_names, new_orig_names)
            
            if (length(overlapping_orig_names) > 0) {
              # Notify user about replacement
              showNotification(
                paste("Replacing existing terrain rasters:", paste(overlapping_orig_names, collapse=", ")),
                type = "warning",
                duration = 10
              )
              
              # Remove overlapping rasters from existing list
              data$original_rasters <- data$original_rasters[!names(data$original_rasters) %in% overlapping_orig_names]
            }
            
            # Combine lists (non-overlapping existing + all new)
            data$original_rasters <- c(data$original_rasters, terrain_rasts)
          } else {
            data$original_rasters <- terrain_rasts
          }
          
          # 3. Now handle prediction rasters separately
          # Create prediction rasters based on template or settings
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
            # Use elevation raster properties as is - not a good idea, may result in very large rasters eating RAM
            # rast(terrain_rasts)
            
            template <- terra::rast(
              extent = terra::ext(elevation_rast_utm),
              resolution = input$resolution,
              crs = terra::crs(elevation_rast_utm)
            )
            project(rast(terrain_rasts), template)
            
          }
          
          # Handle clipping if requested
          if (input$predictionExtent != "none") {
            prediction_extent <- get_prediction_extent(
              points_sf = data$CTtable_sf,
              study_area = data$study_area,
              extent_type = input$predictionExtent,
              buffer = input$bufferPrediction
            )
            
            prediction_rasts <- clip_prediction_rasters(prediction_rasts, 
                                                        prediction_extent)
          }
          
          # Handle prediction raster updates
          if (!is.null(data$prediction_raster)) {
            # Get names from existing and new rasters
            existing_pred_names <- names(data$prediction_raster)
            new_pred_names <- names(prediction_rasts)
            
            # Find overlapping layers
            overlapping_pred_names <- intersect(existing_pred_names, new_pred_names)
            
            if (length(overlapping_pred_names) > 0) {
              # Notify user about replacement
              showNotification(
                paste("Replacing existing terrain prediction layers:", paste(overlapping_pred_names, collapse=", ")),
                type = "warning",
                duration = 10
              )
              
              # Keep only non-overlapping layers from existing raster
              keep_layers <- which(!existing_pred_names %in% overlapping_pred_names)
              
              if (length(keep_layers) > 0) {
                # Combine non-overlapping existing layers with new layers
                data$prediction_raster <- c(data$prediction_raster[[keep_layers]], prediction_rasts)
              } else {
                # If all existing layers overlap, just use new layers
                data$prediction_raster <- prediction_rasts
              }
            } else {
              # No overlap, just combine
              data$prediction_raster <- c(data$prediction_raster, prediction_rasts)
            }
          } else {
            data$prediction_raster <- prediction_rasts
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
      
      # Store original CRS for reference
      original_crs <- sf::st_crs(points_sf)
      
      # Create initial extent based on type
      extent <- switch(extent_type,
                       "grid" = {
                         # Ensure valid geometry with explicit st_union
                         grid_extent <- points_sf
                         if (nrow(grid_extent) > 1) {
                           grid_extent <- sf::st_union(grid_extent)
                         }
                         sf::st_convex_hull(grid_extent)
                       },
                       "study_area" = {
                         if(is.null(study_area)) {
                           warning("Study area requested but not available. Using camera grid instead.")
                           # Fall back to grid if study area not available
                           grid_extent <- points_sf
                           if (nrow(grid_extent) > 1) {
                             grid_extent <- sf::st_union(grid_extent)
                           }
                           sf::st_convex_hull(grid_extent)
                         } else {
                           # Ensure study area is valid and in the same CRS as points_sf
                           study_area_valid <- sf::st_make_valid(study_area)
                           if (sf::st_crs(study_area_valid) != original_crs) {
                             study_area_valid <- sf::st_transform(study_area_valid, original_crs)
                           }
                           study_area_valid
                         }
                       },
                       "intersection" = {
                         if(is.null(study_area)) {
                           warning("Intersection requested but study area not available. Using camera grid instead.")
                           # Fall back to grid if study area not available
                           grid_extent <- points_sf
                           if (nrow(grid_extent) > 1) {
                             grid_extent <- sf::st_union(grid_extent)
                           }
                           sf::st_convex_hull(grid_extent)
                         } else {
                           # Create grid extent
                           grid_extent <- points_sf
                           if (nrow(grid_extent) > 1) {
                             grid_extent <- sf::st_union(grid_extent)
                           }
                           grid_extent <- sf::st_convex_hull(grid_extent)
                           
                           # Ensure both geometries are valid
                           grid_extent <- sf::st_make_valid(grid_extent)
                           study_area_valid <- sf::st_make_valid(study_area)
                           
                           # Transform study area to match grid CRS if needed
                           if (sf::st_crs(study_area_valid) != original_crs) {
                             study_area_valid <- sf::st_transform(study_area_valid, original_crs)
                           }
                           
                           # Safely compute intersection
                           sf::st_intersection(grid_extent, study_area_valid)
                         }
                       }
      )
      
      # Apply buffer if specified and ensure result is valid
      if (!is.null(extent) && !is.null(buffer) && buffer > 0) {
        extent <- sf::st_buffer(extent, dist = buffer)
        extent <- sf::st_make_valid(extent)
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
      
      # Update the UI select inputs with empty choices
      updateSelectInput(session, "rasterBand", choices = NULL)
      updateSelectInput(session, "predictionRasterBand", choices = NULL)
      
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
      
      # Check if we have enough data before proceeding
      if (nrow(covariates_df) < 2) {
        output$correlationWarning <- renderText({
          "Insufficient data for correlation analysis after filtering"
        })
        output$correlationTable <- DT::renderDT({
          data.frame(
            Variable1 = character(0),
            Variable2 = character(0),
            Correlation = numeric(0)
          )
        })
        output$correlationPlot <- renderPlot({
          plot.new()
          graphics::title("Insufficient data for correlation analysis")
        })
        return(NULL)
      }
      
      # Remove non-covariate columns
      exclude_cols <- c(data$stationCol, data$cameraCol, data$setupCol, data$retrievalCol)
      exclude_cols <- c(exclude_cols, 
                        grep("^Problem[0-9]+_(from|to)$", names(covariates_df), value = TRUE))
      covariates_df <- covariates_df[, !names(covariates_df) %in% exclude_cols, drop = FALSE]
      
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
      
      
      # Now safely calculate the correlation matrix
      tryCatch({
        
        # print(covariates_df)
        
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
              
              psych::pairs.panels(
                covariates_df,
                method = input$correlationMethod,
                hist.col = "#75AADB",  # Using a nice blue color
                density = TRUE,        # Show density plots on diagonal
                ellipses = TRUE,       # Show correlation ellipses
                smooth = TRUE,         # Add loess smoothers
                cex.cor = 1.3,         # Default is 1, increased for better readability
                ci = FALSE,            # no confidence intervals
                cex = 1                # point size
                # lwd = 2              # Thicker lines
              )
              
            } else {
              output$correlationPlotWarning <- renderText({
                "Note: Using basic scatter plot matrix. Install 'psych' package for enhanced visualization."
              })
              create_base_pairs_plot(covariates_df)
            }
          }
        })
        
        
      }, error = function(e) {
        output$correlationWarning <- renderText({
          paste("Error in correlation analysis:", e$message)
        })
      })
    })
    
    
    # Tab: species accumulation curves  ----
    
    
    # Species table for accumulation curves
    output$acc_speciesTable <- DT::renderDT({
      
      req(data$recordTable, data$speciesCol, data$stationCol)
      
      # Create summary table
      species_summary <- data$recordTable %>%
        dplyr::group_by(!!sym(data$speciesCol)) %>%
        dplyr::summarize(
          Records = n(),
          Stations = n_distinct(!!sym(data$stationCol))
        )
      
      DT::datatable(species_summary, 
                    selection = list(
                      mode = 'multiple',
                      selected = seq_len(nrow(species_summary))  # Select all rows
                    ),
                    options = list(pageLength = 10)
      )
    })
    
    
    # Handle species selection buttons
    observeEvent(input$acc_selectAll, {
      proxy <- DT::dataTableProxy("acc_speciesTable")
      DT::selectRows(proxy, input$acc_speciesTable_rows_all)
    })
    
    observeEvent(input$acc_deselectAll, {
      proxy <- DT::dataTableProxy("acc_speciesTable")
      DT::selectRows(proxy, NULL)
    })
    
    # Function to get species meeting criteria
    getAccSpeciesMeetingCriteria <- function(minStations, minRecords) {
      req(data$recordTable, data$speciesCol, data$stationCol)
      
      data$recordTable %>%
        dplyr::group_by(!!sym(data$speciesCol)) %>%
        dplyr::summarize(
          Records = n(),
          Stations = n_distinct(!!sym(data$stationCol))
        ) %>%
        dplyr::filter(Records >= minRecords, Stations >= minStations) %>%
        dplyr::pull(!!sym(data$speciesCol))
    }
    
    # Handle filtering buttons
    observeEvent(input$acc_selectByStations, {
      species_to_select <- getAccSpeciesMeetingCriteria(input$acc_minStations, 1)
      updateAccSpeciesSelection(species_to_select)
    })
    
    observeEvent(input$acc_selectByRecords, {
      species_to_select <- getAccSpeciesMeetingCriteria(1, input$acc_minRecords)
      updateAccSpeciesSelection(species_to_select)
    })
    
    observeEvent(input$acc_selectByBoth, {
      species_to_select <- getAccSpeciesMeetingCriteria(
        input$acc_minStations, 
        input$acc_minRecords
      )
      updateAccSpeciesSelection(species_to_select)
    })
    
    # Function to update species selection
    updateAccSpeciesSelection <- function(species_to_select) {
      req(data$recordTable, data$speciesCol)
      
      if (length(species_to_select) > 0) {
        proxy <- DT::dataTableProxy("acc_speciesTable")
        species_summary <- data$recordTable %>%
          dplyr::group_by(!!sym(data$speciesCol)) %>%
          dplyr::summarize(
            Records = n(),
            Stations = n_distinct(!!sym(data$stationCol))
          )
        rows_to_select <- which(species_summary[[data$speciesCol]] %in% species_to_select)
        DT::selectRows(proxy, rows_to_select)
      }
    }
    
    
    
    
    # Initialize selected species when data loads
    observe({
      req(data$recordTable, data$speciesCol)
      if (is.null(selected_species())) {
        all_species <- unique(data$recordTable[[data$speciesCol]])
        selected_species(all_species)
      }
    })
    
    # Update selected species when table selection changes
    observeEvent(input$acc_speciesTable_rows_selected, {
      species_summary <- data$recordTable %>%
        dplyr::group_by(!!sym(data$speciesCol)) %>%
        dplyr::summarize(
          Records = n(),
          Stations = n_distinct(!!sym(data$stationCol))
        )
      
      selected_species(species_summary[[data$speciesCol]][input$acc_speciesTable_rows_selected])
    })
    
    
    
    # Run accumulation analysis
    observeEvent(input$runAccumulation, {
      
      
      # Check for iNEXT package
      if (!requireNamespace("iNEXT", quietly = TRUE)) {
        showNotification(
          "Package 'iNEXT' is required. Please install it with: install.packages('iNEXT')", 
          type = "error",
          duration = NULL
        )
        return()
      }
      
      req(data$recordTable, data$CTtable, data$stationCol, data$speciesCol, selected_species())
      
      current_objects <- species_accumulation_objects()
      
      # Set x_label based on current selection
      if(input$acc_x_unit == "station") {
        current_objects$x_label <- "Number of sampling stations"
      } else {
        if(input$acc_x_unit == "survey_day") {
          current_objects$x_label <- "Number of sampling days (station)"
        } else {
          current_objects$x_label <- "Number of sampling days (survey)"
        }
      }
      
      # Get selected species
      species_summary <- data$recordTable %>%
        dplyr::group_by(!!sym(data$speciesCol)) %>%
        dplyr::summarize(
          Records = n(),
          Stations = n_distinct(!!sym(data$stationCol))
        )
      selected_species <- species_summary[[data$speciesCol]][input$acc_speciesTable_rows_selected]
      
      # Filter recordTable to selected species
      filtered_records <- data$recordTable[data$recordTable[[data$speciesCol]] %in% selected_species(),]
      
      
      # Set assemblageCol parameter
      # assemblageCol <- if(input$acc_assemblageCol != "") input$acc_assemblageCol else NULL
      
      withProgress(message = 'Running analysis...', value = 0, {
        # Run iNEXT analysis
        tryCatch({
          current_objects$results <- speciesAccum(
            CTtable = data$CTtable,
            recordTable = filtered_records,
            speciesCol = data$speciesCol,
            recordDateTimeCol = data$recordDateTimeCol,
            setupCol = data$setupCol,
            stationCol = data$stationCol,
            assemblageCol = NULL, #assemblageCol,
            q = as.numeric(input$acc_q),
            x_unit = input$acc_x_unit,
            knots = input$acc_knots,
            conf = input$acc_conf,
            nboot = input$acc_nboot
          )
          
          
          # Generate plots
          output$acc_rarefaction_plot <- renderPlot({
            
            iNEXT::ggiNEXT(current_objects$results, type = 1, color.var= "Order.q") +
              theme_bw() +
              theme(
                text = element_text(size = 12 * input$acc_plot_scale),
                axis.text = element_text(size = 11 * input$acc_plot_scale),
                axis.title = element_text(size = 12 * input$acc_plot_scale),
                strip.text = element_text(size = 12 * input$acc_plot_scale),
                legend.text = element_text(size = 11 * input$acc_plot_scale),
                legend.title = element_text(size = 12 * input$acc_plot_scale)
              ) +
              labs(
                title = "Sample-size-based R/E curve",
                x = current_objects$x_label
              )
          })
          
          output$acc_coverage_plot <- renderPlot({
            
            iNEXT::ggiNEXT(current_objects$results, type = 2, color.var="Order.q") +
              theme_bw() +
              theme(
                text = element_text(size = 12 * input$acc_plot_scale),
                axis.text = element_text(size = 11 * input$acc_plot_scale),
                axis.title = element_text(size = 12 * input$acc_plot_scale),
                strip.text = element_text(size = 12 * input$acc_plot_scale),
                legend.text = element_text(size = 11 * input$acc_plot_scale),
                legend.title = element_text(size = 12 * input$acc_plot_scale)
              ) +
              labs(
                title = "Sample completeness curve",
                x = current_objects$x_label
              )
          })
          
          output$acc_richness_plot <- renderPlot({
            
            iNEXT::ggiNEXT(current_objects$results, type = 3, color.var="Order.q") +
              theme_bw() +
              theme(
                text = element_text(size = 12 * input$acc_plot_scale),
                axis.text = element_text(size = 11 * input$acc_plot_scale),
                axis.title = element_text(size = 12 * input$acc_plot_scale),
                strip.text = element_text(size = 12 * input$acc_plot_scale),
                legend.text = element_text(size = 11 * input$acc_plot_scale),
                legend.title = element_text(size = 12 * input$acc_plot_scale)
              ) +
              labs(
                title = "Coverage-based R/E curve",
                x = current_objects$x_label
              )
          })
          
          
          
          
          # Generate combined plots
          
          output$acc_rarefaction_plot_combined <- renderPlot({
            
            iNEXT::ggiNEXT(current_objects$results, type = 1, color.var="Order.q") +
              theme_bw() +
              theme(
                text = element_text(size = 10 * input$acc_plot_scale),
                axis.text = element_text(size = 9 * input$acc_plot_scale),
                axis.title = element_text(size = 10 * input$acc_plot_scale),
                strip.text = element_text(size = 10 * input$acc_plot_scale),
                legend.text = element_text(size = 9 * input$acc_plot_scale),
                legend.title = element_text(size = 10 * input$acc_plot_scale)
              ) +
              labs(
                title = "Sample-size-based R/E curve",
                x = current_objects$x_label
              )
          })
          
          output$acc_coverage_plot_combined <- renderPlot({
            
            iNEXT::ggiNEXT(current_objects$results, type = 2, color.var="Order.q") +
              theme_bw() +
              theme(
                text = element_text(size = 10 * input$acc_plot_scale),
                axis.text = element_text(size = 9 * input$acc_plot_scale),
                axis.title = element_text(size = 10 * input$acc_plot_scale),
                strip.text = element_text(size = 10 * input$acc_plot_scale),
                legend.text = element_text(size = 9 * input$acc_plot_scale),
                legend.title = element_text(size = 10 * input$acc_plot_scale)
              ) +
              labs(
                title = "Sample completeness curve",
                x = current_objects$x_label
              )
          })
          
          output$acc_richness_plot_combined <- renderPlot({
            
            iNEXT::ggiNEXT(current_objects$results, type = 3, color.var="Order.q") +
              theme_bw() +
              theme(
                text = element_text(size = 10 * input$acc_plot_scale),
                axis.text = element_text(size = 9 * input$acc_plot_scale),
                axis.title = element_text(size = 10 * input$acc_plot_scale),
                strip.text = element_text(size = 10 * input$acc_plot_scale),
                legend.text = element_text(size = 9 * input$acc_plot_scale),
                legend.title = element_text(size = 10 * input$acc_plot_scale)
              ) +
              labs(
                title = "Coverage-based R/E curve",
                x = current_objects$x_label
              )
          })
          
          
          
          print_AsyEst <- current_objects$results$AsyEst
          print_AsyEst[, -c(1,2)] <- round(print_AsyEst[, -c(1,2)], 2)
          current_objects$summary_table <- print_AsyEst
          
          # Display summary statistics
          output$acc_summary <- renderDT({
            # print_AsyEst <- current_objects$results$AsyEst
            # print_AsyEst[, -c(1,2)] <- round(print_AsyEst[, -c(1,2)], 2) 
            # print(print_AsyEst)
            DT::datatable(print_AsyEst)
          })
          
          
          # # Save objects
          # current_objects$acc_summary <- output$acc_summary
          # current_objects$acc_rarefaction_plot <- output$acc_rarefaction_plot
          # current_objects$acc_rarefaction_plot_combined <- output$acc_rarefaction_plot_combined
          # current_objects$acc_coverage_plot <- output$acc_coverage_plot
          # current_objects$acc_coverage_plot_combined <- output$acc_coverage_plot_combined
          # current_objects$acc_richness_plot_combined <- output$acc_richness_plot_combined
          # current_objects$acc_richness_plot_combined_combined <- output$acc_richness_plot_combined
          
          # print(str(current_objects))
          
          species_accumulation_objects(current_objects)
          
          showNotification("Analysis completed successfully", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error in species accumulation:", e$message), type = "error")
        })
      })
    })
    
    
    # clear species accumulation curves if input data change
    observeEvent(c(data$CTtable, data$recordTable), {
      output$acc_rarefaction_plot <- NULL
      output$acc_coverage_plot <- NULL
      output$acc_richness_plot <- NULL
      output$acc_summary <- NULL
      output$acc_rarefaction_plot_combined <- NULL
      output$acc_coverage_plot_combined <- NULL
      output$acc_richness_plot_combined <- NULL
      
      # x_label("")  # Reset the x-axis label
      species_accumulation_objects <- reactiveVal(list())
      
      # showNotification("Species accumulation plots cleared due to data changes", type = "warning")
    })
    
    
    
    # Tab: detectionHistory     ####
    
    # container for saving reactive objects
    single_species_occu_objects <- shiny::reactiveValues()
    
    
    detection_hist <- reactive({
      req(input$species_dethist, input$occasionLength_single_species, input$outputType, input$day1)
      
      # Check if current_species_list() is valid
      if (!is.vector(current_species_list()) || length(current_species_list()) == 0) {
        warning("current_species_list() is not a valid vector")
        return(NULL)
      }
      
      # Check if selected species is in the list
      if (!(input$species_dethist %in% current_species_list())) {
        warning("Selected species not in current species list")
        return(NULL)
      }
      
      # Check if there are records for the selected species
      species_records <- data$recordTable[data$recordTable[, data$speciesCol] == input$species_dethist, ]
      if (nrow(species_records) == 0) {
        warning("No records for selected species after filtering")
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
                       "Terrain" = hcl.colors(n, palette = "Terrain2"),
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
    
    
    # Basic workflow state tracking and clearing
    observeEvent(c(
      input$basic_model_package,
      input$basic_model_type,
      input$species_dethist,
      input$occasionLength_single_species,
      input$day1,
      input$outputType
    ), {
      
      # Skip if we're in restoration mode
      if (isolate(restoration_mode())) {
        return(NULL)  # Use explicit return(NULL) to force early exit
      }
      
      # Clear the model if it exists
      if (!is.null(basic_model()) || length(single_species_occu_objects$basic_modList) > 0) {
        basic_model(NULL)
        single_species_occu_objects$basic_modList <- list()
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
      
      # Skip if we're in restoration mode
      if (isolate(restoration_mode())) {
        return(NULL)  # Use explicit return(NULL) to force early exit
      }
      
      # Clear the model if it exists
      if (!is.null(advanced_model()) || length(single_species_occu_objects$adv_modList) > 0) {
        advanced_model(NULL)
        single_species_occu_objects$adv_modList <- list()
        output$adv_model_selection <- renderTable({ NULL })
        output$adv_prediction_map <- leaflet::renderLeaflet({ NULL })
        shiny::showNotification("Advanced model cleared due to input changes", type = "warning")
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Clear both models when data changes
    observeEvent(c(data$CTtable_sf, data$recordTable), {
      
      # Skip cleanup if we're in restoration mode
      if (restoration_mode()) {
        return()
      }
      
      something_to_clear <- FALSE
      if (!is.null(basic_model()) || length(single_species_occu_objects$basic_modList) > 0) {
        something_to_clear <- TRUE
        basic_model(NULL)
        
        single_species_occu_objects$basic_modList <- list()
        output$basic_model_selection <- renderTable({ NULL })
        output$basic_prediction_map <- leaflet::renderLeaflet({ NULL })
      }
      if (!is.null(advanced_model()) || length(single_species_occu_objects$adv_modList) > 0) {
        something_to_clear <- TRUE
        advanced_model(NULL)
        single_species_occu_objects$adv_modList <- list()
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
        covs <- if (length(input$basic_det_covs) > 0) {
          if (input$basic_scale_covariates) {
            # Use scale() only for covariates that were scaled before
            processed_covs <- sapply(input$basic_det_covs, function(cov) {
              cov <- symbols_to_char_or_null(cov)
              if (cov %in% names(data$scaling_params$means)) {
                paste0("scale(", cov, ")")
              } else {
                cov
              }
            })
            paste(processed_covs, collapse = " + ")
          } else {
            paste(input$basic_det_covs, collapse = " + ")
          }
        } else {
          ""
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
        else if (input$basic_scale_covariates) {
          processed_covs <- sapply(input$basic_occ_covs, function(cov) {
            cov <- symbols_to_char_or_null(cov)
            if (cov %in% names(data$scaling_params$means)) {
              paste0("scale(", cov, ")")
            } else {
              cov
            }
          })
          paste(processed_covs, collapse = " + ")
        } else 
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
    
    # Generate formula for the model
    generateFormula <- function(effects, package = "unmarked") {
      
      # Helper function to generate term
      generate_term <- function(effect) {
        if(effect$type == "random") {
          if(package == "ubms") {
            # For ubms, use proper random effect syntax
            paste0("(1|", effect$covariates[2], ")")
          } else {
            # For unmarked, convert to interaction since it doesn't support random effects
            warning("Random effects not supported in unmarked, converting to interaction")
            paste(effect$covariates, collapse = " * ")
          }
        } else {
          switch(effect$type,
                 "linear" = effect$covariates[1],
                 "quadratic" = paste0(effect$covariates[1], " + I(", effect$covariates[1], "^2)"),
                 "interaction" = paste(effect$covariates, collapse = " * ")
          )
        }
      }
      
      
      # Process detection terms
      det_terms <- if(length(effects$detection) > 0) {
        vapply(effects$detection, generate_term, character(1))
      } else {
        character(0)
      }
      det_formula <- if(length(det_terms) > 0) paste(det_terms, collapse = " + ") else "1"
      
      # Process occupancy terms
      occu_terms <- if(length(effects$occupancy) > 0) {
        vapply(effects$occupancy, generate_term, character(1))
      } else {
        character(0)
      }
      occu_formula <- if(length(occu_terms) > 0) paste(occu_terms, collapse = " + ") else "1"
      
      # Return the full formula
      formula(paste0("~", det_formula, " ~", occu_formula))
    }
    
    # # Preview the formula with debugging
    # output$formulaPreview <- renderPrint({
    #   req(modelEffects())
    #   
    #   # current_effects <- modelEffects()
    #   # cat("=== Formula Preview Debug ===\n")
    #   # cat("Raw modelEffects structure:\n")
    #   # str(current_effects)
    #   # 
    #   # cat("\nDetection effects:\n")
    #   # print(length(current_effects$detection))
    #   # str(current_effects$detection)
    #   # 
    #   # cat("\nOccupancy effects:\n")
    #   # print(length(current_effects$occupancy))
    #   # str(current_effects$occupancy)
    #   
    #   cat("\nGenerated Formula:\n")
    #   formula <- generateFormula(current_effects, input$model_package)
    #   print(formula)
    #   # cat("===========================\n")
    # })
    
    
    # Debugging functions
    logModelEffects <- function(trigger, effects) {
      cat("\n=== Model Effects Update ===\n")
      cat("Trigger:", trigger, "\n")
      cat("Current Effects Structure:\n")
      str(effects)
      cat("========================\n")
    }
    
    # Validation function for effects
    validateEffects <- function(effects) {
      if (!is.list(effects)) return(FALSE)
      if (!all(c("detection", "occupancy") %in% names(effects))) return(FALSE)
      if (!is.list(effects$detection) || !is.list(effects$occupancy)) return(FALSE)
      return(TRUE)
    }
    
    
    # Safe update function for modelEffects
    safeUpdateModelEffects <- function(new_effects, trigger) {
      if (validateEffects(new_effects)) {
        modelEffects(new_effects)
        # logModelEffects(trigger, new_effects)
      } else {
        warning("Invalid effects structure detected in ", trigger)
        logModelEffects(paste("INVALID -", trigger), new_effects)
      }
    }
    
    
    
    
    # Handle adding detection effects
    observeEvent(input$confirmDetectionEffect, {
      req(input$detectionCovariate, input$detectionCovariate != "")
      
      new_effect <- list(
        type = input$detectionEffectType,
        covariates = input$detectionCovariate
      )
      
      if (input$detectionEffectType %in% c("interaction", "random")) {
        req(input$detectionSecondCovariate, input$detectionSecondCovariate != "")
        new_effect$covariates <- c(new_effect$covariates, input$detectionSecondCovariate)
      }
      
      # # Debug print
      # cat("Adding detection effect:\n")
      # str(new_effect)
      
      current_effects <- modelEffects()
      current_effects$detection <- c(current_effects$detection, list(new_effect))
      safeUpdateModelEffects(current_effects, "add detection effect")
      
      # Reset inputs
      updateSelectInput(session, "detectionCovariate", selected = "")
      if (input$detectionEffectType %in% c("interaction", "random")) {
        updateSelectInput(session, "detectionSecondCovariate", selected = "")
      }
      updateActionButton(session, "addDetectionEffect", label = "Add Effect")
    })
    
    # Handle adding occupancy effects
    observeEvent(input$confirmOccupancyEffect, {
      req(input$occupancyCovariate, input$occupancyCovariate != "")
      
      new_effect <- list(
        type = input$occupancyEffectType,
        covariates = input$occupancyCovariate
      )
      
      if (input$occupancyEffectType %in% c("interaction", "random")) {
        req(input$occupancySecondCovariate, input$occupancySecondCovariate != "")
        new_effect$covariates <- c(new_effect$covariates, input$occupancySecondCovariate)
      }
      
      # # Debug print
      # cat("Adding occupancy effect:\n")
      # str(new_effect)
      
      current_effects <- modelEffects()
      current_effects$occupancy <- c(current_effects$occupancy, list(new_effect))
      safeUpdateModelEffects(current_effects, "add occupancy effect")
      
      # Reset inputs
      updateSelectInput(session, "occupancyCovariate", selected = "")
      if (input$occupancyEffectType %in% c("interaction", "random")) {
        updateSelectInput(session, "occupancySecondCovariate", selected = "")
      }
      updateActionButton(session, "addOccupancyEffect", label = "Add Effect")
    })
    
    # Helper function to create effect labels
    makeEffectLabel <- function(effect) {
      switch(effect$type,
             "linear" = effect$covariates[1],
             "quadratic" = paste0(effect$covariates[1], " + I(", effect$covariates[1], "^2)"),
             "interaction" = paste(effect$covariates, collapse = " × "),
             "random" = paste0(effect$covariates[1], " + (1|", effect$covariates[2], ")")
      )
    }
    
    # Second Covariate Selection (detection)
    observe({
      req(input$detectionEffectType %in% c("interaction", "random"))
      
      # Get covariate choices
      covariate_choices <- getAvailableCovariates(
        data = data$CTtable_sf,
        stationCol = data$stationCol,
        cameraCol = data$cameraCol,
        setupCol = data$setupCol,
        retrievalCol = data$retrievalCol
      )
      
      # Update second covariate dropdown
      updateSelectInput(session, "detectionSecondCovariate", 
                        choices = c("Choose covariate" = "", covariate_choices))
    })
    
    # Second Covariate Selection (occupancy)
    observe({
      req(input$occupancyEffectType %in% c("interaction", "random"))
      
      # Get covariate choices
      covariate_choices <- getAvailableCovariates(
        data = data$CTtable_sf,
        stationCol = data$stationCol,
        cameraCol = data$cameraCol,
        setupCol = data$setupCol,
        retrievalCol = data$retrievalCol
      )
      
      # Update second covariate dropdown
      updateSelectInput(session, "occupancySecondCovariate", 
                        choices = c("Choose covariate" = "", covariate_choices))
    })
    
    
    
    # Function to create removal button ID
    makeRemovalButtonId <- function(type, index) {
      paste0("remove", type, "Effect_", index)
    }
    
    # Display selected detection effects
    output$selectedDetectionEffectsUI <- renderUI({
      effects <- modelEffects()$detection
      if (length(effects) == 0) return(NULL)
      
      tags$div(
        class = "selected-effects",
        lapply(seq_along(effects), function(i) {
          effect <- effects[[i]]
          tags$div(
            class = "effect-item",
            style = "margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 4px;",
            tags$span(
              style = "margin-right: 10px;",
              makeEffectLabel(effect)
            ),
            actionButton(
              inputId = makeRemovalButtonId("Det", i),
              label = "×",
              class = "btn-danger btn-xs",
              style = "padding: 0px 6px;"
            )
          )
        })
      )
    })
    
    # Display selected occupancy effects
    output$selectedOccupancyEffectsUI <- renderUI({
      effects <- modelEffects()$occupancy
      if (length(effects) == 0) return(NULL)
      
      tags$div(
        class = "selected-effects",
        lapply(seq_along(effects), function(i) {
          effect <- effects[[i]]
          tags$div(
            class = "effect-item",
            style = "margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 4px;",
            tags$span(
              style = "margin-right: 10px;",
              makeEffectLabel(effect)
            ),
            actionButton(
              inputId = makeRemovalButtonId("Occu", i),
              label = "×",
              class = "btn-danger btn-xs",
              style = "padding: 0px 6px;"
            )
          )
        })
      )
    })
    
    # Handle removal of detection effects
    observeEvent(input[["removeDetEffect_1"]], {
      current_effects <- modelEffects()
      current_effects$detection <- current_effects$detection[-1]
      safeUpdateModelEffects(current_effects, "remove detection effect 1")
    })
    
    observeEvent(input[["removeDetEffect_2"]], {
      current_effects <- modelEffects()
      current_effects$detection <- current_effects$detection[-2]
      safeUpdateModelEffects(current_effects, "remove detection effect 2")
    })
    
    observeEvent(input[["removeDetEffect_3"]], {
      current_effects <- modelEffects()
      current_effects$detection <- current_effects$detection[-3]
      safeUpdateModelEffects(current_effects, "remove detection effect 3")
    })
    
    observeEvent(input[["removeDetEffect_4"]], {
      current_effects <- modelEffects()
      current_effects$detection <- current_effects$detection[-4]
      safeUpdateModelEffects(current_effects, "remove detection effect 4")
    })
    
    observeEvent(input[["removeDetEffect_5"]], {
      current_effects <- modelEffects()
      current_effects$detection <- current_effects$detection[-5]
      safeUpdateModelEffects(current_effects, "remove detection effect 5")
    })
    
    # Handle removal of occupancy effects
    observeEvent(input[["removeOccuEffect_1"]], {
      current_effects <- modelEffects()
      current_effects$occupancy <- current_effects$occupancy[-1]
      safeUpdateModelEffects(current_effects, "remove occupancy effect 1")
    })
    
    observeEvent(input[["removeOccuEffect_2"]], {
      current_effects <- modelEffects()
      current_effects$occupancy <- current_effects$occupancy[-2]
      safeUpdateModelEffects(current_effects, "remove occupancy effect 2")
    })
    
    observeEvent(input[["removeOccuEffect_3"]], {
      current_effects <- modelEffects()
      current_effects$occupancy <- current_effects$occupancy[-3]
      safeUpdateModelEffects(current_effects, "remove occupancy effect 3")
    })
    
    observeEvent(input[["removeOccuEffect_4"]], {
      current_effects <- modelEffects()
      current_effects$occupancy <- current_effects$occupancy[-4]
      safeUpdateModelEffects(current_effects, "remove occupancy effect 4")
    })
    
    observeEvent(input[["removeOccuEffect_5"]], {
      current_effects <- modelEffects()
      current_effects$occupancy <- current_effects$occupancy[-5]
      safeUpdateModelEffects(current_effects, "remove occupancy effect 5")
    })
    
    # # Generate formula for the model
    # generateFormula <- function(effects, package = "unmarked") {
    #   # Helper function to generate term
    #   generate_term <- function(effect) {
    #     if(effect$type == "random") {
    #       if(package == "ubms") {
    #         # For ubms, use proper random effect syntax
    #         paste0("(1|", effect$covariates[2], ")")
    #       } else {
    #         # For unmarked, convert to interaction since it doesn't support random effects
    #         warning("Random effects not supported in unmarked, converting to interaction")
    #         paste(effect$covariates, collapse = " * ")
    #       }
    #     } else {
    #       switch(effect$type,
    #              "linear" = effect$covariates[1],
    #              "quadratic" = paste0(effect$covariates[1], " + I(", effect$covariates[1], "^2)"),
    #              "interaction" = paste(effect$covariates, collapse = " * ")
    #       )
    #     }
    #   }
    #   
    #   # Process detection terms
    #   det_terms <- if(length(effects$detection) > 0) {
    #     vapply(effects$detection, generate_term, character(1))
    #   } else {
    #     character(0)
    #   }
    #   det_formula <- if(length(det_terms) > 0) paste(det_terms, collapse = " + ") else "1"
    #   
    #   # Process occupancy terms
    #   occu_terms <- if(length(effects$occupancy) > 0) {
    #     vapply(effects$occupancy, generate_term, character(1))
    #   } else {
    #     character(0)
    #   }
    #   occu_formula <- if(length(occu_terms) > 0) paste(occu_terms, collapse = " + ") else "1"
    #   
    #   # Return the full formula
    #   formula(paste0("~", det_formula, " ~", occu_formula))
    # }
    
    # Preview the formula with debugging
    output$formulaPreview <- renderPrint({
      req(modelEffects())
      
      current_effects <- modelEffects()
      # cat("=== Formula Preview Debug ===\n")
      # cat("Raw modelEffects structure:\n")
      # str(current_effects)
      # 
      # cat("\nDetection effects:\n")
      # print(length(current_effects$detection))
      # str(current_effects$detection)
      # 
      # cat("\nOccupancy effects:\n")
      # print(length(current_effects$occupancy))
      # str(current_effects$occupancy)
      
      cat("\nGenerated Formula:\n")
      formula <- generateFormula(current_effects, input$model_package)
      print(formula)
      # cat("===========================\n")
    })
    
    
    
    
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
    # Response plots - Basic workflow
    output$basic_response_plot <- renderPlot({
      req(basic_model())
      
      # Get current model and plot type
      model <- basic_model()
      is_detection <- input$basic_plot_type == "Detection covariates"
      
      # Get covariates based on plot type
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
        
        # Return the plot(s)
        if (length(plots) == 1) {
          # If only one covariate, return the single plot
          plots[[1]]
        } else if (length(plots) > 1) {
          # For multiple covariates, use patchwork if available
          if (!requireNamespace("patchwork", quietly = TRUE)) {
            warning("Package 'patchwork' is required to display multiple plots. Please install it with install.packages('patchwork')")
            return(NULL)
          }
          # Arrange in a single row
          patchwork::wrap_plots(plots, ncol = length(plots))
        } else {
          # No plots (shouldn't happen given the req() above)
          NULL
        }
        
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
        # plot_data <- createAdvancedEffectPlot(
        #   model = model,
        #   effect = effect,
        #   submodel = tolower(input$adv_plot_submodel),
        #   ci_level = input$adv_ci_level,
        #   show_data = input$adv_show_data
        # )
        
        # print(plot_data)
        
      } else {
        # For ubms models
        ubms::plot_marginal(model, 
                            submodel = tolower(input$adv_plot_submodel),
                            param = effect)
      }
    })
    
    # # Helper function for advanced effect plots
    # createAdvancedEffectPlot <- function(model, effect, submodel, ci_level = 0.95, show_data = TRUE) {
    #   # Implementation depends on effect type (linear, quadratic, interaction)
    #   # This is a placeholder 
    # }
    
    
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
    
    # # Spatial predictions - Advanced workflow
    # observeEvent(input$adv_run_prediction, {
    #   req(advanced_model())
    #   
    #   # Similar to basic workflow but with additional MCMC handling for ubms models
    #   # Implementation follows same pattern as basic predictions with added complexity
    # })
    
    
    # # Update MCMC diagnostics for ubms models
    # observe({
    #   req(advanced_model(), input$adv_model_package == "ubms")
    #   
    #   # Get parameter names
    #   params <- rownames(summary(advanced_model())$summary)
    #   
    #   # Update parameter selection dropdown
    #   updateSelectInput(session, "adv_parameter_trace",
    #                     choices = params)
    # })
    # 
    # # Render trace plots for UBMS models
    # output$adv_trace_plot <- renderPlot({
    #   req(advanced_model(), 
    #       input$adv_model_package == "ubms",
    #       input$adv_parameter_trace)
    #   
    #   # Create trace plot for selected parameter
    #   bayesplot::mcmc_trace(advanced_model(),
    #                         pars = input$adv_parameter_trace)
    # })
    
    
    
    
    # Model selection ----
    
    ## basic
    observeEvent(input$basic_add_to_modsel, {
      req(basic_model())
      single_species_occu_objects$basic_modList <- c(single_species_occu_objects$basic_modList, list(basic_model()))
    })
    
    observeEvent(input$basic_clear_modsel, {
      single_species_occu_objects$basic_modList <- list()
    })
    
    output$basic_model_selection <- renderTable({
      req(length(single_species_occu_objects$basic_modList) > 0)
      createModelSelectionTable(single_species_occu_objects$basic_modList, input$basic_model_package)
    })
    
    # advanced
    
    observeEvent(input$adv_add_to_modsel, {
      req(advanced_model())
      single_species_occu_objects$adv_modList <- c(single_species_occu_objects$adv_modList, list(advanced_model()))
    })
    
    # clear modsel not implemented yet for advanced model builder
    # observeEvent(input$adv_clear_modsel, {
    #   single_species_occu_objects$adv_modList <- list()
    # })
    
    output$adv_model_selection <- renderTable({
      req(length(single_species_occu_objects$adv_modList) > 0)
      createModelSelectionTable(single_species_occu_objects$adv_modList, input$adv_model_package)
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
      
      # Check if df has enough data
      if (is.null(df) || nrow(df) < 2) {
        return(list(
          scaled_data = df,
          scaling_params = list(means = list(), sds = list())
        ))
      }
      
      # Identify numeric columns
      numeric_cols <- sapply(df, function(x) {
        is.numeric(x) && !all(is.na(x))
      })
      
      # Check if we have any numeric columns
      if (sum(numeric_cols) == 0) {
        return(list(
          scaled_data = df,
          scaling_params = list(means = list(), sds = list())
        ))
      }
      
      
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
      
      # Process each layer by index to avoid issues with duplicate names
      for (i in 1:terra::nlyr(pred_raster)) {
        layer_name <- names(pred_raster)[i]
        
        # Skip layers without scaling parameters
        if (!layer_name %in% names(scaling_params$means)) {
          next
        }
        
        mean_val <- scaling_params$means[[layer_name]]
        sd_val <- scaling_params$sds[[layer_name]]
        
        if (sd_val > 0) {
          # Use numeric indexing to avoid issues with duplicate names
          scaled_raster[[i]] <- (pred_raster[[i]] - mean_val) / sd_val
        } else {
          scaled_raster[[i]] <- pred_raster[[i]] - mean_val
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
      #   can't use vectorized input to species since (probably) the reactive 
      #   camop() doesn't play nice with match.call()
      
      
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
        showNotification("Community model created successfully. Proceed under 'Model fitting' tab.", type = "message")
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
    
    
    
    ## Goodness of fit (community models) ----
    
    
    
    # Run GOF test
    observeEvent(input$run_gof, {
      req(fitted_comm_model(), commOccu_model())
      
      
      
      withProgress(message = 'Running Goodness of Fit test...', value = 0, {
        tryCatch({
          seed <- as.numeric(Sys.time())
          
          
          # Get predictions for p and psi using subset of MCMC draws
          p_pred <- predict(
            object = commOccu_model(),
            mcmc.list = fitted_comm_model(),
            type = "p_array",
            draws = input$gof_draws,
            seed = seed
          )
          
          psi_pred <- predict(
            object = commOccu_model(),
            mcmc.list = fitted_comm_model(),
            type = "psi_array",
            draws = input$gof_draws,
            seed = seed
          )
          
          # Run community PPC
          results <- PPC.community(
            p = p_pred,
            psi = psi_pred,
            y = commOccu_model()@input$ylist,
            model = input$communityModelType,
            type = input$gof_residual_type,
            z.cond = input$gof_z_cond
          )
          
          # Store results
          gof_results(results)
          
          print("GOF Finished")
          showNotification("Goodness of fit test completed", type = "message")
        }, error = function(e) {
          showNotification(paste("Error in GOF test:", e$message), type = "error")
        })
      })
    })
    
    # Render community p-value
    output$gof_community_pvalue <- renderText({
      req(gof_results())
      sprintf("%.3f", gof_results()$BP$BP[nrow(gof_results()$BP)])  # Last row is community-level
    })
    
    # Render community p-value interpretation
    output$gof_interpretation <- renderText({
      req(gof_results())
      bp <- gof_results()$BP$BP[nrow(gof_results()$BP)]
      print(get_fit_interpretation(bp))
    })
    
    # Consistent interpretation function for both community and species level
    get_fit_interpretation <- function(bp) {
      if (bp >= 0.45 && bp <= 0.55) {
        "Excellent fit"
      } else if (bp >= 0.25 && bp <= 0.75) {
        "Good fit"
      } else if (bp >= 0.1 && bp <= 0.9) {
        "Moderate fit"
      } else {
        "Lack of fit"
      }
    }
    
    # Get dispersion pattern 
    get_dispersion_pattern <- function(bp) {
      if (bp >= 0.35 && bp <= 0.65) {
        ""  # Empty for good/excellent fit
      } else if (bp < 0.35) {
        "Underdispersed"
      } else {
        "Overdispersed"
      }
    }
    
    
    
    
    # Render species table
    output$gof_species_table <- DT::renderDT({
      req(gof_results())
      
      # Create enhanced table with interpretation
      species_results <- gof_results()$BP[-nrow(gof_results()$BP),]  # Remove community row
      species_results$Deviation <- abs(species_results$BP - 0.5)
      species_results$Interpretation <- sapply(species_results$BP, get_fit_interpretation)
      species_results$Pattern <- sapply(species_results$BP, get_dispersion_pattern)
      
      
      
      DT::datatable(
        species_results,
        options = list(
          pageLength = 10,
          order = list(list(2, 'desc'))  # Sort by deviation
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(c("BP", "Deviation"), digits = 3) %>%
        DT::formatStyle(
          'BP',
          backgroundColor = DT::styleInterval(
            c(0.1, 0.25, 0.45, 0.55, 0.75, 0.9),
            c('#ff6666',              # Lack of fit: red
              '#e6f5c9',              # Moderate fit: very light green
              '#d9f0b3',              # Good fit: light green
              '#99cc99',              # Excellent fit: medium-light green
              '#d9f0b3',              # Good fit: light green
              '#e6f5c9',              # Moderate fit: very light green
              '#ff6666')              # Lack of fit: red
          )
        )
    })
    
    # Helper function to get fill color based on p-value
    get_fit_color <- function(bp) {
      if(bp < 0.1 || bp > 0.9) {
        "#ff6666"  # Red for lack of fit
      } else if(bp >= 0.45 && bp <= 0.55) {
        "#cceb99"  # Medium-light green for excellent fit
      } else if(bp >= 0.35 && bp <= 0.65) {
        "#d9f0b3"  # Light green for good fit
      } else {
        "#e6f5c9"  # Very light green for moderate fit
      }
    }
    
    # # Render residual plot
    output$gof_residual_plot <- renderPlot({
      req(gof_results())
      
      # Create plot data
      plot_data <- data.frame()
      
      for (species in names(gof_results()$residuals)) {
        species_data <- gof_results()$residuals[[species]]
        
        plot_data <- rbind(plot_data, data.frame(
          Species = species,
          Observed = apply(species_data$res.obs, 2, mean),
          Predicted = apply(species_data$res.new, 2, mean)
        ))
      }
      
      
      # Calculate base sizes scaled by input
      base_size <- 12 * input$gof_plot_scale
      
      
      # Create plot
      p <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
        geom_point(alpha = 0.6) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        facet_wrap(~Species,
                   ncol = input$gof_plot_columns,
                   scales = if(input$gof_plot_free_scales) "free" else "fixed") +
        theme_bw(base_size = base_size) +
        theme(
          strip.background = element_rect(fill = "lightgray"),
          strip.text = element_text(face = "bold",
                                    size = base_size),
          axis.title = element_text(size = base_size),
          axis.text = element_text(size = base_size * 0.9),
          plot.title = element_text(size = base_size * 1.2),
          plot.subtitle = element_text(size = base_size * 0.9)
        ) +
        labs(
          title = "Observed vs. Predicted Residuals by Species",
          subtitle = sprintf("Using %s residuals", input$gof_residual_type)
        )
      
      print(p)
    }, height = function()   input$gof_plot_height
    )
    
    
    
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
      
      # Use tryCatch to handle any errors gracefully
      tryCatch({
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
        
        # Get range of values and count of unique values
        value_range <- terra::minmax(current_layer)
        unique_values <- unique(stats::na.omit(terra::values(current_layer)))
        n_values <- length(unique_values)
        
        # Handle the case of a single unique value
        if (n_values <= 1) {
          # For a single value, create a simple color ramp around that value
          single_value <- unique_values[1]
          
          # Create a range around the single value to avoid the interval error
          value_range <- c(single_value - 0.001, single_value + 0.001)
          n_colors <- 3  # Simple 3-color gradient
          
          # Get color palette
          colors <- get_color_palette(input$occupancyColorPalette, n = n_colors, 
                                      invert = input$invertOccupancyColors)
          
          # Create breaks for the single value (below, at, above)
          breaks <- c(value_range[1], single_value, value_range[2])
        } else {
          # Normal case with multiple values
          n_colors <- min(n_values, 20)
          
          # Get color palette
          colors <- get_color_palette(input$occupancyColorPalette, n = n_colors, 
                                      invert = input$invertOccupancyColors)
          
          # Create breaks with one more break than colors
          breaks <- seq(value_range[1], value_range[2], length.out = length(colors) + 1)
        }
        
        # Create map
        m <- mapview::mapview(
          current_layer,
          layer.name = paste0("Species Occupancy - ", input$occupancyMapType),
          col.regions = colors,
          na.color = "transparent",
          at = breaks
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
      }, error = function(e) {
        # Show error notification
        showNotification(paste("Error rendering map:", e$message), type = "error")
        
        # Create fallback map with error message
        output$occupancyMap <- leaflet::renderLeaflet({
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::addControl(
              html = paste("Error rendering map:", e$message),
              position = "topright"
            )
        })
      })
    })
    
    
    # Render richness map
    observe({
      req(spatial_predictions_community$richness, input$richnessType)
      
      tryCatch({
        current_layer <- switch(input$richnessType,
                                "mean" = spatial_predictions_community$richness$mean,
                                "sd" = spatial_predictions_community$richness$sd,
                                "lower" = spatial_predictions_community$richness$lower,
                                "upper" = spatial_predictions_community$richness$upper
        )
        
        # Get range of values and count of unique values
        value_range <- terra::minmax(current_layer)
        unique_values <- unique(stats::na.omit(terra::values(current_layer)))
        n_values <- length(unique_values)
        
        # Handle the case of a single unique value
        if (n_values <= 1) {
          # For a single value, create a simple color ramp around that value
          single_value <- unique_values[1]
          
          # Create a range around the single value
          value_range <- c(single_value - 0.001, single_value + 0.001)
          n_colors <- 3  # Simple 3-color gradient
          
          # Get color palette
          colors <- get_color_palette(input$richnessColorPalette, n = n_colors, 
                                      invert = input$invertRichnessColors)
          
          # Create breaks for the single value
          breaks <- c(value_range[1], single_value, value_range[2])
        } else {
          # Normal case with multiple values
          n_colors <- min(n_values, 20)
          
          # Get color palette
          colors <- get_color_palette(input$richnessColorPalette, n = n_colors, 
                                      invert = input$invertRichnessColors)
          
          # Create breaks
          breaks <- seq(value_range[1], value_range[2], length.out = length(colors) + 1)
        }
        
        # Create map
        m <- mapview::mapview(
          current_layer,
          layer.name = paste0("Species Richness - ", input$richnessType),
          col.regions = colors,
          na.color = "transparent",
          at = breaks
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
      }, error = function(e) {
        # Show error notification
        showNotification(paste("Error rendering richness map:", e$message), type = "error")
        
        # Create fallback map with error message
        output$richnessMap <- leaflet::renderLeaflet({
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::addControl(
              html = paste("Error rendering richness map:", e$message),
              position = "topright"
            )
        })
      })
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
    
    
    # Function to export data tables
    export_table <- function(data, name, format = "csv", dir = ".") {
      if (inherits(data, "sf")) {
        data_to_export <- sf::st_drop_geometry(data)
      } else {
        data_to_export <- data
      }
      
      file_path <- file.path(dir, paste0(name, ifelse(format == "csv", ".csv", ".rds")))
      
      if (format == "csv") {
        utils::write.csv(data_to_export, file = file_path, row.names = FALSE)
      } else {
        saveRDS(data_to_export, file = file_path)
      }
      
      return(file_path)
    }
    
    # Function to create species detection spatial data
    create_species_detections_sf <- function(record_table, stations_sf) {
      if (is.null(record_table) || is.null(stations_sf) || nrow(record_table) == 0) {
        return(NULL)
      }
      
      # Get station column name
      station_col <- attr(stations_sf, "stationCol") 
      if (is.null(station_col)) {
        station_col <- "Station" # Default if attribute not found
      }
      
      # Get species column name
      species_col <- attr(record_table, "speciesCol")
      if (is.null(species_col)) {
        species_col <- "Species" # Default if attribute not found
      }
      
      # Join location data from stations to records
      merged_data <- merge(record_table, 
                           sf::st_drop_geometry(stations_sf), 
                           by = station_col,
                           all.x = TRUE)
      
      # If we have coordinates, create spatial features
      if ("geometry" %in% names(stations_sf)) {
        # Join with spatial data
        spatial_data <- merge(merged_data,
                              stations_sf[, c(station_col, "geometry")],
                              by = station_col)
        
        # Return as sf object
        return(spatial_data)
      } else {
        # If we don't have geometry, return the merged data frame
        return(merged_data)
      }
    }
    
    # Observers for export functionality
    observeEvent(input$confirm_export, {
      removeModal()
      
      # Prompt user to select a base directory
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        base_dir <- rstudioapi::selectDirectory(
          caption = "Select folder for data export",
          label = "Select"
        )
      } else {
        # Fallback if rstudioapi is not available
        base_dir <- dirname(file.choose())
      }
      
      if (is.null(base_dir) || base_dir == "") return()  # User cancelled
      
      # Create folder structure with timestamp
      export_dir <- file.path(base_dir, paste0("camtrapR_export_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(export_dir, recursive = TRUE)
      
      # Log file to track export results
      log_file <- file.path(export_dir, "export_log.txt")
      write(paste("camtrapR Export", Sys.time()), file = log_file)
      
      withProgress(message = 'Exporting data...', value = 0, {
        
        # Helper function to log export operations
        log_export <- function(message) {
          write(paste(Sys.time(), "-", message), file = log_file, append = TRUE)
          message(message)  # Also print to console
        }
        
        # Export Data Tables
        if (length(input$export_tables) > 0) {
          tables_dir <- file.path(export_dir, "data_tables")
          dir.create(tables_dir, recursive = TRUE)
          
          tryCatch({
            if ("ct_table" %in% input$export_tables && !is.null(data$CTtable_sf)) {
              file_path <- export_table(data$CTtable_sf, "camera_trap_table", input$table_format, tables_dir)
              log_export(paste("Exported camera trap table to", file_path))
            }
            
            if ("ct_table_agg" %in% input$export_tables && !is.null(data$aggregated_CTtable)) {
              file_path <- export_table(data$aggregated_CTtable, "aggregated_camera_trap_table", input$table_format, tables_dir)
              log_export(paste("Exported aggregated camera trap table to", file_path))
            }
            
            if ("record_table" %in% input$export_tables && !is.null(data$recordTable)) {
              file_path <- export_table(data$recordTable, "record_table", input$table_format, tables_dir)
              log_export(paste("Exported record table to", file_path))
            }
          }, error = function(e) {
            log_export(paste("Error exporting data tables:", e$message))
          })
        }
        
        # Export Spatial Data
        if (length(input$export_spatial) > 0) {
          spatial_dir <- file.path(export_dir, "spatial_data")
          dir.create(spatial_dir, recursive = TRUE)
          
          tryCatch({
            if ("ct_stations_sf" %in% input$export_spatial && !is.null(data$CTtable_sf)) {
              file_path <- file.path(spatial_dir, "camera_trap_stations.gpkg")
              sf::st_write(data$CTtable_sf, file_path, delete_dsn = TRUE)
              log_export(paste("Exported camera trap stations to", file_path))
            }
            
            if ("species_detections_sf" %in% input$export_spatial) {
              species_detections <- create_species_detections_sf(data$recordTable, data$CTtable_sf)
              if (!is.null(species_detections)) {
                file_path <- file.path(spatial_dir, "species_detections.gpkg")
                if (inherits(species_detections, "sf")) {
                  sf::st_write(species_detections, file_path, delete_dsn = TRUE)
                } else {
                  export_table(species_detections, "species_detections", "csv", spatial_dir)
                }
                log_export(paste("Exported species detections to", file_path))
              }
            }
            
            # Export study area if available
            if (!is.null(data$study_area)) {
              file_path <- file.path(spatial_dir, "study_area.gpkg")
              sf::st_write(data$study_area, file_path, delete_dsn = TRUE)
              log_export(paste("Exported study area to", file_path))
            }
          }, error = function(e) {
            log_export(paste("Error exporting spatial data:", e$message))
          })
        }
        
        # Export Occupancy Models
        if (input$export_occupancy) {
          models_dir <- file.path(export_dir, "occupancy_models")
          dir.create(models_dir, recursive = TRUE)
          
          tryCatch({
            # Export detection history
            if ("umf" %in% input$occupancy_components && !is.null(detection_hist())) {
              det_hist_path <- file.path(models_dir, "detection_history.rds")
              saveRDS(detection_hist(), file = det_hist_path)
              log_export(paste("Exported detection history to", det_hist_path))
            }
            
            # Export unmarkedFrameOccu
            if ("umf" %in% input$occupancy_components && !is.null(umf())) {
              umf_path <- file.path(models_dir, "unmarked_frame.rds")
              saveRDS(umf(), file = umf_path)
              log_export(paste("Exported unmarked frame to", umf_path))
            }
            
            # Export basic model
            if ("model" %in% input$occupancy_components && !is.null(basic_model())) {
              model_path <- file.path(models_dir, "basic_model.rds")
              saveRDS(basic_model(), file = model_path)
              log_export(paste("Exported basic model to", model_path))
              
              # Save summary
              if ("summary" %in% input$occupancy_components) {
                summary_path <- file.path(models_dir, "basic_model_summary.txt")
                capture.output(summary(basic_model()), file = summary_path)
                log_export(paste("Exported basic model summary to", summary_path))
              }
            }
            
            # Export advanced model
            if ("model" %in% input$occupancy_components && !is.null(advanced_model())) {
              model_path <- file.path(models_dir, "advanced_model.rds")
              saveRDS(advanced_model(), file = model_path)
              log_export(paste("Exported advanced model to", model_path))
              
              # Save summary
              if ("summary" %in% input$occupancy_components) {
                summary_path <- file.path(models_dir, "advanced_model_summary.txt")
                capture.output(summary(advanced_model()), file = summary_path)
                log_export(paste("Exported advanced model summary to", summary_path))
              }
            }
            
            # Export model list
            if ("model" %in% input$occupancy_components && !is.null(single_species_occu_objects$basic_modList) && length(single_species_occu_objects$basic_modList) > 0) {
              modlist_path <- file.path(models_dir, "model_list.rds")
              saveRDS(single_species_occu_objects$basic_modList, file = modlist_path)
              log_export(paste("Exported model list to", modlist_path))
              
              # Save model selection table
              if ("summary" %in% input$occupancy_components) {
                tryCatch({
                  fl <- unmarked::fitList(fits = single_species_occu_objects$basic_modList, autoNames = "formula")
                  ms <- unmarked::modSel(fl)
                  modsel_path <- file.path(models_dir, "model_selection_table.csv")
                  write.csv(ms@Full, file = modsel_path, row.names = FALSE)
                  log_export(paste("Exported model selection table to", modsel_path))
                }, error = function(e) {
                  log_export(paste("Error generating model selection table:", e$message))
                })
              }
            }
          }, error = function(e) {
            log_export(paste("Error exporting occupancy models:", e$message))
          })
        }
        
        # Export Community Occupancy Models
        if (input$export_community_models) {
          community_dir <- file.path(export_dir, "community_models")
          dir.create(community_dir, recursive = TRUE)
          
          tryCatch({
            if ("object" %in% input$comm_model_components && !is.null(commOccu_model())) {
              file_path <- file.path(community_dir, "community_model.rds")
              saveRDS(commOccu_model(), file = file_path)
              log_export(paste("Exported community model to", file_path))
            }
            
            if ("fitted" %in% input$comm_model_components && !is.null(fitted_comm_model())) {
              file_path <- file.path(community_dir, "fitted_community_model.rds")
              saveRDS(fitted_comm_model(), file = file_path)
              log_export(paste("Exported fitted community model to", file_path))
            }
            
            if ("summary" %in% input$comm_model_components && !is.null(model_summary())) {
              file_path <- file.path(community_dir, "community_model_summary.rds")
              saveRDS(model_summary(), file = file_path)
              
              # Also save as text
              text_path <- file.path(community_dir, "community_model_summary.txt")
              tryCatch({
                capture.output(model_summary(), file = text_path)
              }, error = function(e) {
                write("Error generating summary text output", file = text_path)
              })
              log_export(paste("Exported community model summary to", file_path))
            }
            
            # Export community predictions
            if ("predictions" %in% input$comm_model_components) {
              pred_dir <- file.path(community_dir, "predictions")
              dir.create(pred_dir, recursive = TRUE)
              
              # Export occupancy predictions
              if (!is.null(spatial_predictions_community$occupancy)) {
                occupancy_dir <- file.path(pred_dir, "occupancy")
                dir.create(occupancy_dir, recursive = TRUE)
                tryCatch({
                  terra::writeRaster(spatial_predictions_community$occupancy$mean, 
                                     file.path(occupancy_dir, "mean.tif"), overwrite = TRUE)
                  terra::writeRaster(spatial_predictions_community$occupancy$sd, 
                                     file.path(occupancy_dir, "sd.tif"), overwrite = TRUE)
                  terra::writeRaster(spatial_predictions_community$occupancy$lower, 
                                     file.path(occupancy_dir, "lower.tif"), overwrite = TRUE)
                  terra::writeRaster(spatial_predictions_community$occupancy$upper, 
                                     file.path(occupancy_dir, "upper.tif"), overwrite = TRUE)
                  log_export("Exported occupancy predictions")
                }, error = function(e) {
                  log_export(paste("Error exporting occupancy predictions:", e$message))
                })
              }
              
              # Export richness predictions
              if (!is.null(spatial_predictions_community$richness)) {
                richness_dir <- file.path(pred_dir, "richness")
                dir.create(richness_dir, recursive = TRUE)
                tryCatch({
                  terra::writeRaster(spatial_predictions_community$richness$mean, 
                                     file.path(richness_dir, "mean.tif"), overwrite = TRUE)
                  terra::writeRaster(spatial_predictions_community$richness$sd, 
                                     file.path(richness_dir, "sd.tif"), overwrite = TRUE)
                  terra::writeRaster(spatial_predictions_community$richness$lower, 
                                     file.path(richness_dir, "lower.tif"), overwrite = TRUE)
                  terra::writeRaster(spatial_predictions_community$richness$upper, 
                                     file.path(richness_dir, "upper.tif"), overwrite = TRUE)
                  log_export("Exported richness predictions")
                }, error = function(e) {
                  log_export(paste("Error exporting richness predictions:", e$message))
                })
              }
              
              # Export PAO predictions
              if (!is.null(spatial_predictions_community$pao)) {
                pao_path <- file.path(pred_dir, "pao_results.rds")
                saveRDS(spatial_predictions_community$pao, file = pao_path)
                
                # Also save summary as CSV
                tryCatch({
                  write.csv(spatial_predictions_community$pao$pao_summary, 
                            file.path(pred_dir, "pao_summary.csv"), row.names = FALSE)
                }, error = function(e) {
                  log_export(paste("Error exporting PAO summary:", e$message))
                })
                log_export(paste("Exported PAO predictions to", pao_path))
              }
            }
          }, error = function(e) {
            log_export(paste("Error exporting community models:", e$message))
          })
        }
        
        # Export covariates and rasters
        if (any(c("original_rasters", "prediction_raster") %in% input$export_rasters)) {
          rasters_dir <- file.path(export_dir, "rasters")
          dir.create(rasters_dir, recursive = TRUE)
          
          tryCatch({
            # Export original rasters
            if ("original_rasters" %in% input$export_rasters && !is.null(data$original_rasters)) {
              orig_dir <- file.path(rasters_dir, "original")
              dir.create(orig_dir, recursive = TRUE)
              
              for (i in 1:terra::nlyr(data$original_rasters)) {
                layer_name <- names(data$original_rasters)[i]
                file_path <- file.path(orig_dir, paste0(layer_name, ".tif"))
                terra::writeRaster(data$original_rasters[[i]], file_path, overwrite = TRUE)
                log_export(paste("Exported original raster layer", layer_name))
              }
            }
            
            # Export prediction rasters
            if ("prediction_raster" %in% input$export_rasters && !is.null(data$prediction_raster)) {
              pred_dir <- file.path(rasters_dir, "prediction")
              dir.create(pred_dir, recursive = TRUE)
              
              for (i in 1:terra::nlyr(data$prediction_raster)) {
                layer_name <- names(data$prediction_raster)[i]
                file_path <- file.path(pred_dir, paste0(layer_name, ".tif"))
                terra::writeRaster(data$prediction_raster[[i]], file_path, overwrite = TRUE)
                log_export(paste("Exported prediction raster layer", layer_name))
              }
            }
            
            # Export scaling parameters
            if (!is.null(data$scaling_params)) {
              scale_path <- file.path(rasters_dir, "scaling_parameters.rds")
              saveRDS(data$scaling_params, file = scale_path)
              log_export(paste("Exported scaling parameters to", scale_path))
            }
          }, error = function(e) {
            log_export(paste("Error exporting rasters:", e$message))
          })
        }
        
        # Create readme file with details about the export
        readme_file <- file.path(export_dir, "README.txt")
        write(paste("camtrapR Data Export from surveyDashboard function -", Sys.time(), "\n\n"), file = readme_file)
        write(paste("This export contains data from the camtrapR survey dashboard."), file = readme_file, append = TRUE)
        write(paste("\nExport directory structure:"), file = readme_file, append = TRUE)
        
        # List contents of the export directory
        dir_contents <- list.dirs(export_dir, recursive = FALSE, full.names = FALSE)
        write(paste("- ", dir_contents), file = readme_file, append = TRUE)
        
        # Write camera trap survey info
        write("\n\nCamera Trap Survey Info:", file = readme_file, append = TRUE)
        write(paste("Number of stations:", length(unique(data$CTtable[[data$stationCol]]))), file = readme_file, append = TRUE)
        write(paste("Number of species:", length(unique(data$recordTable[[data$speciesCol]]))), file = readme_file, append = TRUE)
        write(paste("Number of records:", nrow(data$recordTable)), file = readme_file, append = TRUE)
        
        write("\n\nRefer to export_log.txt for details on exported files.", file = readme_file, append = TRUE)
        
        log_export("Export completed successfully")
        
        # Write session info
        write("\n\nR Session Info:", file = readme_file, append = TRUE)
        capture.output(utils::sessionInfo(), file = readme_file, append = TRUE)
        
      })
      
      showNotification(paste("Data exported to", export_dir), type = "message", duration = NULL)
    })
    
    # Update the export_all_data observer to ensure checkbox for rasters is included
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
        
        tags$hr(),
        
        # Raster Data section
        h4("Raster Data"),
        checkboxGroupInput("export_rasters", "Select raster data to export:",
                           choices = c("Original Rasters" = "original_rasters",
                                       "Prediction Rasters" = "prediction_raster"),
                           selected = c("original_rasters", "prediction_raster")),
        
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
    collectAppState <- function() {
      list(
        # Data tables
        CTtable = data$CTtable,
        CTtable_sf = data$CTtable_sf,
        aggregated_CTtable = data$aggregated_CTtable,
        recordTable = data$recordTable,
        original_record_table = original_record_table(),
        
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
        
        # Camera operation
        camop_data = if (!is.null(try(camop(), silent = TRUE))) camop() else NULL,
        
        # Filtering
        filter_states = list(
          active_filters = active_filters(),
          filtered_data = filtered_data(),
          original_data = original_data()
        ),
        filter_state = filter_state(),
        filtered_species = filtered_species(),
        
        # Raster data
        prediction_raster = data$prediction_raster,
        original_rasters = data$original_rasters,
        
        # Detection history
        detection_hist_data = if (!is.null(try(detection_hist(), silent = TRUE))) detection_hist() else NULL,
        umf_data = if (!is.null(try(umf(), silent = TRUE))) umf() else NULL,
        
        # Models and results
        basic_model = basic_model(),
        advanced_model = advanced_model(),
        # basic_modList = single_species_occu_objects$basic_modList,
        # adv_modList = single_species_occu_objects$adv_modList,
        single_species_occu_objects = single_species_occu_objects(),
        
        
        commOccu_model = commOccu_model(),
        fitted_comm_model = fitted_comm_model(),
        model_summary = model_summary(),
        model_effects = modelEffects(),
        effect_plots = effect_plots(),
        coef_plot = coef_plot(),
        gof_results = gof_results(),
        consoleOutput = consoleOutput(),
        
        
        
        # For species accumulation:
        species_accumulation_objects = species_accumulation_objects,
        # x_label = x_label(),
        
        # Spatial predictions
        occupancy_predictions = spatial_predictions_community$occupancy,
        richness_predictions = spatial_predictions_community$richness,
        pao_predictions = spatial_predictions_community$pao,
        
        # Study area
        study_area = data$study_area,
        study_area_buffer = data$study_area_buffer,
        
        # UI state
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
          camera_settings = list(
            minDeltaTime = input$minDeltaTime,
            deltaTimeComparedTo = input$deltaTimeComparedTo,
            removeDuplicateRecords = input$removeDuplicateRecords
          )
        ),
        
        # Other reactive values
        current_species_list = current_species_list(),
        
        # Covariate and scaling data
        scaling_params = data$scaling_params,
        aggregated_CTtable_scaled = data$aggregated_CTtable_scaled,
        prediction_raster_scaled = data$prediction_raster_scaled,
        original_columns = data$original_columns,
        
        # Additional settings
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
        ),
        
        # Save timestamp
        saved_at = Sys.time()
      )
    }
    
    # Function to restore app state
    restoreAppState <- function(saved_state) {
      
      # Set restoration mode to prevent automatic cleanup
      restoration_mode(TRUE)
      
      withProgress(message = 'Restoring app state...', value = 0, {
        tryCatch({
          
          # turn off restoration mode when function ends
          on.exit(restoration_mode(FALSE))
          
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
          
          # 2. Restore data tables
          
          data$CTtable <- saved_state$CTtable
          data$CTtable_sf <- saved_state$CTtable_sf
          data$aggregated_CTtable <- saved_state$aggregated_CTtable
          data$recordTable <- saved_state$recordTable
          
          # 3. Restore filtering state
          
          if (!is.null(saved_state$filter_states)) {
            active_filters(saved_state$filter_states$active_filters)
            filtered_data(saved_state$filter_states$filtered_data)
            original_data(saved_state$filter_states$original_data)
          }
          
          # Restore filtered species
          if (!is.null(saved_state$filtered_species)) {
            filtered_species(saved_state$filtered_species)
            
            # Apply filtering to the record table if needed
            if (length(saved_state$filtered_species) > 0) {
              data$recordTable <- data$recordTable[!data$recordTable[[data$speciesCol]] %in% saved_state$filtered_species, ]
            }
          }
          
          # 4. Restore raster data
          
          data$prediction_raster <- saved_state$prediction_raster
          data$original_rasters <- saved_state$original_rasters
          
          # 5. Restore models and results
          
          if (!is.null(saved_state$basic_model)) basic_model(saved_state$basic_model)
          if (!is.null(saved_state$advanced_model)) advanced_model(saved_state$advanced_model)
          if (!is.null(saved_state$basic_modList)) single_species_occu_objects$basic_modList <- saved_state$basic_modList
          if (!is.null(saved_state$adv_modList)) single_species_occu_objects$adv_modList <- saved_state$adv_modList
          if (!is.null(saved_state$commOccu_model)) commOccu_model(saved_state$commOccu_model)
          if (!is.null(saved_state$fitted_comm_model)) fitted_comm_model(saved_state$fitted_comm_model)
          if (!is.null(saved_state$model_summary)) model_summary(saved_state$model_summary)
          if (!is.null(saved_state$model_effects)) modelEffects(saved_state$model_effects)
          
          # 6. Restore spatial predictions
          
          if (!is.null(saved_state$occupancy_predictions)) 
            spatial_predictions_community$occupancy <- saved_state$occupancy_predictions
          if (!is.null(saved_state$richness_predictions)) 
            spatial_predictions_community$richness <- saved_state$richness_predictions
          if (!is.null(saved_state$pao_predictions)) 
            spatial_predictions_community$pao <- saved_state$pao_predictions
          
          # 7. Restore study area
          
          data$study_area <- saved_state$study_area
          data$study_area_buffer <- saved_state$study_area_buffer
          
          # 8. Restore other reactive values
          
          if (!is.null(saved_state$current_species_list)) 
            current_species_list(saved_state$current_species_list)
          
          # Scaling parameters
          data$scaling_params <- saved_state$scaling_params
          data$aggregated_CTtable_scaled <- saved_state$aggregated_CTtable_scaled
          data$prediction_raster_scaled <- saved_state$prediction_raster_scaled
          data$original_columns <- saved_state$original_columns
          
          # 9. Update UI state
          
          if (!is.null(saved_state$ui_state)) {
            # Get current valid species list
            valid_species <- sort(unique(data$recordTable[, data$speciesCol]))
            if (!is.null(data$exclude)) {
              valid_species <- valid_species[!valid_species %in% data$exclude]
            }
            
            # Helper to safely update inputs
            safeUpdateInput <- function(inputId, value, available_choices) {
              if (!is.null(value) && (value %in% available_choices || 
                                      (value == "n_species" && inputId == "species_for_map"))) {
                updateSelectInput(session, inputId, selected = value)
              } else if (length(available_choices) > 0) {
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
            updateSliderInput(session, "occasionLength_single_species",
                              value = saved_state$ui_state$occasion_length$single_species)
            
            updateSliderInput(session, "occasionLength_community",
                              value = saved_state$ui_state$occasion_length$community)
            
            updateSelectInput(session, "outputType",
                              selected = saved_state$ui_state$output_type)
            
            updateSelectInput(session, "day1",
                              selected = saved_state$ui_state$day1)
            
            # Camera settings
            if (!is.null(saved_state$ui_state$camera_settings)) {
              updateNumericInput(session, "minDeltaTime", 
                                 value = saved_state$ui_state$camera_settings$minDeltaTime)
              updateSelectInput(session, "deltaTimeComparedTo", 
                                selected = saved_state$ui_state$camera_settings$deltaTimeComparedTo)
              updateCheckboxInput(session, "removeDuplicateRecords", 
                                  value = saved_state$ui_state$camera_settings$removeDuplicateRecords)
            }
            
            # Extract settings
            if (!is.null(saved_state$extract_settings)) {
              updateNumericInput(session, "bufferCT", value = saved_state$extract_settings$bufferCT)
              updateCheckboxInput(session, "bilinear", value = saved_state$extract_settings$bilinear)
              updateSelectInput(session, "predictionExtent", selected = saved_state$extract_settings$predictionExtent)
              updateNumericInput(session, "bufferPrediction", value = saved_state$extract_settings$bufferPrediction)
            }
            
            # Display settings
            if (!is.null(saved_state$display_settings)) {
              updateCheckboxInput(session, "scale_size", value = saved_state$display_settings$scale_size)
              updateCheckboxInput(session, "no_record_more_transparent", 
                                  value = saved_state$display_settings$no_record_more_transparent)
              updateSliderInput(session, "maxPixels", value = saved_state$display_settings$maxPixels)
              updateSelectInput(session, "colorPalette", selected = saved_state$display_settings$colorPalette)
              updateCheckboxInput(session, "invertColors", value = saved_state$display_settings$invertColors)
              updateSelectInput(session, "ctColorBy", selected = saved_state$display_settings$ctColorBy)
              updateSliderInput(session, "ctPointSize", value = saved_state$display_settings$ctPointSize)
              if (!is.null(saved_state$display_settings$predictionColorPalette)) {
                updateSelectInput(session, "predictionColorPalette", 
                                  selected = saved_state$display_settings$predictionColorPalette)
              }
              updateCheckboxInput(session, "invertPredictionColors", 
                                  value = saved_state$display_settings$invertPredictionColors)
              updateSliderInput(session, "maxPixelsPrediction", 
                                value = saved_state$display_settings$maxPixelsPrediction)
            }
          }
          
          return(TRUE)
        }, error = function(e) {
          restoration_mode(FALSE)
          showNotification(paste("Error restoring state:", e$message), type = "error", duration = NULL)
          return(FALSE)
        })
      })
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
    
    
    # save event handler
    observeEvent(input$confirmSave, {
      removeModal()
      
      # Collect current state without requiring parameters
      app_state <- collectAppState()
      
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
        showNotification(paste("App state saved to", file_path), type = "message", duration = 5)
        showNotification("Single species occupancy models and results will not be restored.", type = "message", duration = 5)
      }
    })
    
    # load event handler
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
        
        # Make sure restoration mode is FALSE before starting (in case of previous errors)
        restoration_mode(FALSE)
        
        tryCatch({
          # Load state
          saved_state <- readRDS(file_path)
          
          # Validate basic structure of saved state
          required_components <- c("CTtable", "recordTable", "stationCol", "speciesCol")
          missing_components <- required_components[!sapply(required_components, 
                                                            function(comp) !is.null(saved_state[[comp]]))]
          
          if (length(missing_components) > 0) {
            msg <- paste("Invalid state file: missing components:", 
                         paste(missing_components, collapse = ", "))
            showNotification(msg, type = "error", duration = NULL)
            return()
          }
          
          # Restore state
          result <- restoreAppState(saved_state)
          
          if (result) {
            showNotification(paste("App state loaded from", file_path), type = "message", duration = 5)
          }
          
        }, error = function(e) {
          # Ensure restoration mode is turned off
          restoration_mode(FALSE)
          
          showNotification(paste("Error loading app state:", e$message), 
                           type = "error", duration = NULL)
        })
      }
    })
    
    
    # Create a centralized reset function to use at import points
    # Define resetAppState function that takes all reactive values as arguments
    resetAppState <- function(
    # Restoration and import tracking
      restoration_mode = NULL,
      wi_data = NULL,
      
      # Species management
      current_species_list = NULL,
      selected_species = NULL,
      filtered_species = NULL,
      
      # Camera trap filtering
      original_data = NULL,
      filtered_data = NULL,
      active_filters = NULL,
      filter_removal_observers = NULL,
      
      # Record filtering
      original_record_table = NULL,
      
      # Species accumulation
      # x_label = NULL,
      species_accumulation_objects = NULL,
      
      # Single species occupancy
      basic_model = NULL,
      advanced_model = NULL,
      modelEffects = NULL,
      
      # Community occupancy
      commOccu_model = NULL,
      consoleOutput = NULL,
      fitted_comm_model = NULL,
      model_summary = NULL,
      effect_plots = NULL,
      coef_plot = NULL,
      gof_results = NULL,
      
      # Output references
      output = NULL,
      
      # My object container
      single_species_occu_objects = NULL,
      
      # Spatial predictions 
      spatial_predictions_community = NULL,
      
      # UI control
      notification = TRUE
    ) {
      # When this function is called in observeEvent, wrap in tryCatch for safety
      tryCatch({
        # Reset species filtering
        if (!is.null(filtered_species)) {
          filtered_species(NULL)
        }
        
        # Reset camera trap filtering
        if (!is.null(active_filters)) {
          active_filters(list())
        }
        
        # Reset species lists
        if (!is.null(selected_species)) {
          selected_species(NULL)
        }
        
        # Reset single-species occupancy models
        if (!is.null(basic_model)) {
          basic_model(NULL)
        }
        
        if (!is.null(advanced_model)) {
          advanced_model(NULL)
        }
        
        if (!is.null(modelEffects)) {
          modelEffects(list())
        }
        
        # Reset single_species_occu_objects containers if they exist
        if (!is.null(single_species_occu_objects)) {
          if (exists("basic_modList", single_species_occu_objects)) {
            single_species_occu_objects$basic_modList <- list()
          }
          if (exists("adv_modList", single_species_occu_objects)) {
            single_species_occu_objects$adv_modList <- list()
          }
          if (exists("detection_hist", single_species_occu_objects)) {
            single_species_occu_objects$detection_hist <- NULL
          }
          if (exists("umf", single_species_occu_objects)) {
            single_species_occu_objects$umf <- NULL
          }
        }
        
        # Reset community occupancy models
        if (!is.null(commOccu_model)) {
          commOccu_model(NULL)
        }
        
        if (!is.null(fitted_comm_model)) {
          fitted_comm_model(NULL)
        }
        
        if (!is.null(model_summary)) {
          model_summary(NULL)
        }
        
        if (!is.null(effect_plots)) {
          effect_plots(NULL)
        }
        
        if (!is.null(coef_plot)) {
          coef_plot(NULL)
        }
        
        if (!is.null(gof_results)) {
          gof_results(NULL)
        }
        
        if (length(species_accumulation_objects) > 0) {
          species_accumulation_objects(list())
        }
        
        # Reset spatial predictions if applicable
        if (!is.null(spatial_predictions_community)) {
          spatial_predictions_community$occupancy <- NULL
          spatial_predictions_community$richness <- NULL
          spatial_predictions_community$pao <- NULL
        }
        
        # Reset output elements if output reference provided
        if (!is.null(output)) {
          # Clear model summaries and plots
          output$basic_model_selection <- renderTable({ NULL })
          output$adv_model_selection <- renderTable({ NULL })
          output$basic_prediction_map <- leaflet::renderLeaflet({ NULL })
          output$adv_prediction_map <- leaflet::renderLeaflet({ NULL })
          
          # Clear species accumulation plots
          output$acc_rarefaction_plot <- NULL
          output$acc_coverage_plot <- NULL
          output$acc_richness_plot <- NULL
          output$acc_summary <- NULL
          output$acc_rarefaction_plot_combined <- NULL
          output$acc_coverage_plot_combined <- NULL
          output$acc_richness_plot_combined <- NULL
        }
        
        # Notification if requested
        if (notification) {
          showNotification("Application state reset due to new data import", type = "warning")
        }
      }, error = function(e) {
        # Catch any errors and log them, but don't crash
        message("Error in resetAppState: ", e$message)
        if (notification) {
          showNotification(paste("Error resetting app state:", e$message), type = "warning")
        }
      })
    }
    
    
    
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