#' Define the SFish metadata class
#'
#' @slot crs Character. The coordinate reference system
#' @slot gridType Character. Type of spatial grid (e.g., "C-square", "ICES rectangle")
#' @slot resolution Numeric. The resolution of the grid (degrees)
#' @slot temporalExtent List. Start and end dates for the data
#' @slot dataSources Character. Sources of the data
#' @slot processingNotes Character. Notes on data processing
#' @slot creationDate Date. When the object was created
#'
#' @export
setClass(
  "SFishMetadata",
  slots = c(
    crs = "character",
    gridType = "character", 
    resolution = "numeric",
    temporalExtent = "list",
    dataSources = "character",
    processingNotes = "character",
    creationDate = "Date"
  ),
  prototype = list(
    crs = "EPSG:4326",
    gridType = "C-square",
    resolution = 0.05,
    temporalExtent = list(start = NA, end = NA),
    dataSources = character(0),
    processingNotes = character(0),
    creationDate = Sys.Date()
  )
)

#' Define the fisheries-specific metadata subclass
#'
#' @slot gearTypes Character. Types of fishing gear used
#' @slot effortUnits Character. Units used for fishing effort
#' @slot catchUnits Character. Units used for catch data
#'
#' @export
setClass(
  "SFishFisheriesMetadata",
  contains = "SFishMetadata",
  slots = c(
    gearTypes = "character",
    effortUnits = "character",
    catchUnits = "character"
  ),
  prototype = list(
    gearTypes = character(0),
    effortUnits = character(0),
    catchUnits = character(0)
  )
)

#' Define the main SFish class
#'
#' @slot metadata SFishMetadata. Metadata for the dataset
#' @slot data data.frame. The spatial data with grid identifier and geometry
#'
#' @export
setClass(
  "SFish",
  slots = c(
    metadata = "SFishMetadata",
    data = "data.frame"
  )
)

#' Define the C-square specific subclass
#'
#' @export
setClass(
  "SFishCSquare",
  contains = "SFish",
  validity = function(object) {
    # Check if the data contains a csquare column
    if (!("csquare" %in% colnames(object@data))) {
      return("SFishCSquare object must have a 'csquare' column")
    }
    # Check if the gridType in metadata is C-square
    if (object@metadata@gridType != "C-square") {
      return("Metadata gridType must be 'C-square' for SFishCSquare objects")
    }
    return(TRUE)
  }
)

#' Define the ICES rectangle specific subclass
#'
#' @export
setClass(
  "SFishICESRect",
  contains = "SFish",
  validity = function(object) {
    # Check if the data contains an ices_rectangle column
    if (!("ices_rectangle" %in% colnames(object@data))) {
      return("SFishICESRect object must have an 'ices_rectangle' column")
    }
    # Check if the gridType in metadata is ICES rectangle
    if (object@metadata@gridType != "ICES rectangle") {
      return("Metadata gridType must be 'ICES rectangle' for SFishICESRect objects")
    }
    return(TRUE)
  }
)