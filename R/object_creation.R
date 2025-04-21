#' Define the main SFish class with array-based structure
#'
#' @slot metadata SFishMetadata. Metadata for the dataset
#' @slot data list. The spatial data with dimensions and values
#'
#' @export
setClass(
  "SFish",
  slots = c(
    metadata = "SFishMetadata",
    data = "list"
  )
)

#' Create an empty SFish object with array structure
#'
#' @param dimensions Named list of dimensions to include (with NULL or vector values)
#' @param gridType Type of spatial grid ("C-square" or "ICES rectangle")
#' @param resolution Grid resolution in degrees
#' @param crs Coordinate reference system
#' @param units List of units for measurement variables
#'
#' @return An empty SFish object
#' @export
create_empty_sfish <- function(
    dimensions = list(
      year = NULL,
      month = NULL,
      country = NULL,
      metier4 = NULL,  # Gear type
      metier6 = NULL,  # Full metier code
      vesselLength = NULL
    ),
    gridType = "C-square",
    resolution = ifelse(gridType == "C-square", 0.05, 1.0),
    crs = "EPSG:4326",
    units = list(
      effort = "hours",
      catch = "kg",
      value = "Euros",
      kwHours = "kw-hours"
    )
) {
  # Create metadata
  metadata <- new("SFishMetadata",
                  crs = crs,
                  gridType = gridType,
                  resolution = resolution,
                  temporalExtent = list(start = NA, end = NA),
                  dataSources = character(0),
                  processingNotes = "Empty SFish object created",
                  creationDate = Sys.Date())
  
  # Initialize spatial units list
  data <- list(
    dimensions = dimensions,
    spatialUnits = list(),  # Will store csquare/ices_rectangle and geometry
    values = list()         # Will store the array of values
  )
  
  # Create and return SFish object
  new("SFish", metadata = metadata, data = data)
}

#' Add a spatial unit to a SFish object
#'
#' @param sfish_obj SFish object
#' @param spatial_id Spatial identifier (e.g., C-square code)
#' @param geometry WKT geometry string
#'
#' @return Updated SFish object with new spatial unit
#' @export
add_spatial_unit <- function(sfish_obj, spatial_id, geometry) {
  # Check if spatial unit already exists
  if (spatial_id %in% names(sfish_obj@data$spatialUnits)) {
    # Update geometry if needed
    sfish_obj@data$spatialUnits[[spatial_id]]$geometry <- geometry
  } else {
    # Add new spatial unit
    sfish_obj@data$spatialUnits[[spatial_id]] <- list(
      geometry = geometry,
      data = list()  # Will store data values for this spatial unit
    )
  }
  
  # Update processing notes
  sfish_obj@metadata@processingNotes <- c(
    sfish_obj@metadata@processingNotes,
    paste("Added spatial unit:", spatial_id)
  )
  
  return(sfish_obj)
}

#' Add data to a SFish object
#'
#' @param sfish_obj SFish object
#' @param spatial_id Spatial identifier (e.g., C-square code)
#' @param dimension_values Named list of dimension values
#' @param data_values Named list of data values
#'
#' @return Updated SFish object with new data
#' @export
add_data <- function(sfish_obj, spatial_id, dimension_values, data_values) {
  # Check if spatial unit exists
  if (!(spatial_id %in% names(sfish_obj@data$spatialUnits))) {
    stop("Spatial unit not found: ", spatial_id)
  }
  
  # For each dimension, ensure the value exists or add it
  for (dim_name in names(dimension_values)) {
    dim_val <- dimension_values[[dim_name]]
    
    # If this dimension doesn't exist yet in our structure, initialize it
    if (is.null(sfish_obj@data$dimensions[[dim_name]])) {
      sfish_obj@data$dimensions[[dim_name]] <- character(0)
    }
    
    # If this value doesn't exist in the dimension, add it
    if (!(dim_val %in% sfish_obj@data$dimensions[[dim_name]])) {
      sfish_obj@data$dimensions[[dim_name]] <- c(
        sfish_obj@data$dimensions[[dim_name]], 
        dim_val
      )
    }
  }
  
  # Create a unique key for this combination of dimension values
  dim_key <- paste(
    sapply(names(dimension_values), function(name) {
      paste0(name, "=", dimension_values[[name]])
    }),
    collapse = "|"
  )
  
  # Store the data values under this key for this spatial unit
  sfish_obj@data$spatialUnits[[spatial_id]]$data[[dim_key]] <- data_values
  
  return(sfish_obj)
}

#' Show method for SFish objects
#'
#' @param object SFish object
#' @export
setMethod("show", "SFish", function(object) {
  cat("SFish: Fisheries Spatial Data\n")
  cat("----------------------------\n")
  
  # Grid information
  cat("Grid Type:", object@metadata@gridType, "\n")
  cat("Resolution:", object@metadata@resolution, "degrees\n")
  cat("CRS:", object@metadata@crs, "\n")
  
  # Temporal extent
  if (!is.na(object@metadata@temporalExtent$start) || !is.na(object@metadata@temporalExtent$end)) {
    cat("Temporal Extent:", 
        ifelse(is.na(object@metadata@temporalExtent$start), "?", object@metadata@temporalExtent$start),
        "to",
        ifelse(is.na(object@metadata@temporalExtent$end), "?", object@metadata@temporalExtent$end),
        "\n")
  }
  
  # Dimensions
  cat("\nDimensions:\n")
  for (dim_name in names(object@data$dimensions)) {
    dim_values <- object@data$dimensions[[dim_name]]
    cat("  ", dim_name, ": ", length(dim_values), " (", 
        paste(head(dim_values, 3), collapse=", "),
        if(length(dim_values) > 3) "...", 
        ")\n", sep="")
  }
  
  # Spatial units
  cat("\nSpatial Units:", length(object@data$spatialUnits), "\n")
  
  # Example of data values
  if (length(object@data$spatialUnits) > 0) {
    first_unit <- names(object@data$spatialUnits)[1]
    first_unit_data <- object@data$spatialUnits[[first_unit]]
    if (length(first_unit_data$data) > 0) {
      first_dim_key <- names(first_unit_data$data)[1]
      first_data_values <- first_unit_data$data[[first_dim_key]]
      
      cat("\nData Fields:", paste(names(first_data_values), collapse=", "), "\n")
    }
  }
  
  # Creation date
  cat("\nCreated:", object@metadata@creationDate, "\n")
})


#' Convert icesVMS data to SFish object with array structure
#'
#' @param vms_data Data frame from icesVMS::get_vms()
#'
#' @return SFish object containing the VMS data
#' @export
icesVMS_to_SFish <- function(vms_data) {
  # Define dimensions we want as array dimensions
  dimensions <- list(
    year = NULL,
    month = NULL,
    country = NULL,
    metier4 = NULL,
    metier6 = NULL,
    vesselLength = NULL
  )
  
  # Create empty SFish object
  sfish <- create_empty_sfish(dimensions = dimensions)
  
  # Get unique C-squares
  unique_csquares <- unique(vms_data$cSquare)
  
  # Create simple WKT polygons for each C-square as placeholders
  for (csq in unique_csquares) {
    # Parse C-square code to create WKT polygon
    parts <- strsplit(csq, ":")[[1]]
    lon <- as.numeric(paste0(substr(parts[1], 1, 2), ".", substr(parts[1], 3, 4)))
    lat <- as.numeric(paste0(substr(parts[2], 1, 2), ".", substr(parts[2], 3, 2)))
    res <- 0.05  # Default C-square resolution at this level
    
    wkt_polygon <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
                           lon, lat,
                           lon + res, lat,
                           lon + res, lat + res,
                           lon, lat + res,
                           lon, lat)
    
    # Add spatial unit
    sfish <- add_spatial_unit(sfish, csq, wkt_polygon)
  }
  
  # Add data for each row
  for (i in 1:nrow(vms_data)) {
    row <- vms_data[i, ]
    
    # Extract dimension values
    dimension_values <- list(
      year = as.character(row$year),
      month = as.character(row$month),
      country = row$country,
      metier4 = row$gearCode,
      metier6 = row$leMetLevel6,
      vesselLength = row$vesselLengthCategory
    )
    
    # Extract data values (everything that's not a dimension)
    data_values <- list(
      effort = row$fishingHours,
      catch = row$totweight,
      value = row$totvalue,
      kwHours = row$kwFishinghours,
      avgFishingSpeed = row$avgFishingSpeed,
      avgGearWidth = row$avgGearWidth,
      uniqueVessels = row$uniqueVessels,
      anonVessels = row$anonVessels
    )
    
    # Add data to SFish object
    sfish <- add_data(sfish, row$cSquare, dimension_values, data_values)
  }
  
  # Update metadata
  sfish@metadata@dataSources <- c(sfish@metadata@dataSources, 
                                  "icesVMS::get_vms")
  
  # Update temporal extent
  sfish@metadata@temporalExtent <- list(
    start = paste0(min(as.numeric(sfish@data$dimensions$year)), "-", 
                   sprintf("%02d", min(as.numeric(sfish@data$dimensions$month))), "-01"),
    end = paste0(max(as.numeric(sfish@data$dimensions$year)), "-", 
                 sprintf("%02d", max(as.numeric(sfish@data$dimensions$month))), "-28")
  )
  
  return(sfish)
}
