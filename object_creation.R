#' Create an empty SFish object
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
      metier5 = NULL,  # Target assemblage
      metier6 = NULL,  # Full metier code
      vesselLength = NULL
    ),
    gridType = "C-square",
    resolution = ifelse(gridType == "C-square", 0.05, c(1.0, 0.5)),
    crs = "EPSG:4326",
    units = list(
      effort = "hours",
      catch = "kg",
      value = "Euros",
      sweptArea = "km2",
      kwHours = "kw-hours"
    )
) {
  # Initialize dimension values
  dim_values <- list()
  dim_lengths <- c()
  
  # Process provided dimensions
  for (dim_name in names(dimensions)) {
    if (is.null(dimensions[[dim_name]])) {
      # Empty dimension - will be populated later
      dim_values[[dim_name]] <- character(0)
      dim_lengths <- c(dim_lengths, 0)
    } else {
      # Pre-populated dimension values
      dim_values[[dim_name]] <- as.character(dimensions[[dim_name]])
      dim_lengths <- c(dim_lengths, length(dimensions[[dim_name]]))
    }
  }
  
  # Create array structure (initially with 0 spatial units)
  arr_dims <- c(0, dim_lengths)
  arr_dimnames <- c(list(character(0)), dim_values)
  
  # Create empty array
  data_array <- array(list(), dim = arr_dims)
  dimnames(data_array) <- arr_dimnames
  
  # Create metadata
  metadata <- list(
    crs = crs,
    gridType = gridType,
    resolution = resolution,
    temporalExtent = list(
      start = NA,
      end = NA
    ),
    dimensions = dim_values,
    units = units,
    dataSources = character(0),
    processingNotes = c("Empty SFish object created"),
    creationDate = Sys.Date()
  )
  
  # Create SFish object
  sfish_obj <- list(
    metadata = metadata,
    data = data_array
  )
  
  # Set class for S4 objects
  class(sfish_obj) <- "SFish"
  
  return(sfish_obj)
}

#' Add a spatial unit to an empty SFish object
#'
#' @param sfish_obj SFish object
#' @param spatial_id Spatial identifier (e.g., C-square code)
#' @param geometry WKT geometry string
#'
#' @return Updated SFish object with new spatial unit
#' @export
add_spatial_unit <- function(sfish_obj, spatial_id, geometry) {
  # Get current dimensions
  curr_dims <- dim(sfish_obj$data)
  curr_dimnames <- dimnames(sfish_obj$data)
  
  # Add new spatial unit to dimnames
  new_spatial_units <- c(curr_dimnames[[1]], spatial_id)
  curr_dimnames[[1]] <- new_spatial_units
  
  # Create new array with increased dimensions
  new_dims <- c(length(new_spatial_units), curr_dims[-1])
  new_array <- array(list(), dim = new_dims)
  dimnames(new_array) <- curr_dimnames
  
  # Copy existing data to new array
  if (length(curr_dimnames[[1]]) > 0) {
    # Copy existing elements one by one
    for (i in 1:prod(curr_dims)) {
      if (!is.null(sfish_obj$data[[i]])) {
        # Get multi-dimensional index
        idx <- arrayInd(i, curr_dims)
        # Construct index for new array
        new_idx <- idx
        new_array[[new_idx]] <- sfish_obj$data[[i]]
      }
    }
  }
  
  # Update the object
  sfish_obj$data <- new_array
  
  # Add processing note
  sfish_obj$metadata$processingNotes <- c(
    sfish_obj$metadata$processingNotes,
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
  if (!(spatial_id %in% dimnames(sfish_obj$data)[[1]])) {
    stop("Spatial unit not found in SFish object. Add it first with add_spatial_unit().")
  }
  
  # Build index
  indices <- c(which(dimnames(sfish_obj$data)[[1]] == spatial_id))
  
  # Add dimension indices
  for (dim_idx in 2:length(dim(sfish_obj$data))) {
    dim_name <- names(sfish_obj$metadata$dimensions)[dim_idx - 1]
    if (dim_name %in% names(dimension_values)) {
      dim_val <- dimension_values[[dim_name]]
      possible_values <- dimnames(sfish_obj$data)[[dim_idx]]
      
      # Check if value exists in dimension
      if (!(dim_val %in% possible_values)) {
        # Add new dimension value
        new_values <- c(possible_values, dim_val)
        sfish_obj <- expand_dimension(sfish_obj, dim_name, new_values)
        # Re-calculate index after expansion
        indices <- c(which(dimnames(sfish_obj$data)[[1]] == spatial_id))
        for (prev_dim_idx in 2:(dim_idx-1)) {
          prev_dim_name <- names(sfish_obj$metadata$dimensions)[prev_dim_idx - 1]
          if (prev_dim_name %in% names(dimension_values)) {
            prev_dim_val <- dimension_values[[prev_dim_name]]
            indices <- c(indices, which(dimnames(sfish_obj$data)[[prev_dim_idx]] == prev_dim_val))
          } else {
            indices <- c(indices, 1) # Default to first value if not specified
          }
        }
      }
      
      # Add dimension index
      dim_index <- which(dimnames(sfish_obj$data)[[dim_idx]] == dim_val)
      indices <- c(indices, dim_index)
    } else {
      # Default to first value if dimension not specified
      indices <- c(indices, 1)
    }
  }
  
  # Create or update data at this location
  if (is.null(sfish_obj$data[[indices]])) {
    # Initialize with empty data frame containing geometry
    sfish_obj$data[[indices]] <- data.frame()
  }
  
  # Add data values
  for (name in names(data_values)) {
    sfish_obj$data[[indices]][[name]] <- data_values[[name]]
  }
  
  return(sfish_obj)
}

#' Expand a dimension in a SFish object to include new values
#'
#' @param sfish_obj SFish object
#' @param dimension_name Name of dimension to expand
#' @param new_values New set of values for the dimension
#'
#' @return Updated SFish object with expanded dimension
#' @export
expand_dimension <- function(sfish_obj, dimension_name, new_values) {
  # Get dimension index
  dim_idx <- which(names(sfish_obj$metadata$dimensions) == dimension_name)
  if (length(dim_idx) == 0) {
    stop("Dimension not found: ", dimension_name)
  }
  
  # Array dimension index (offset by 1 for spatial dimension)
  arr_dim_idx <- dim_idx + 1
  
  # Current dimensions
  curr_dims <- dim(sfish_obj$data)
  curr_dimnames <- dimnames(sfish_obj$data)
  
  # New dimensions
  new_dims <- curr_dims
  new_dims[arr_dim_idx] <- length(new_values)
  
  # New dimnames
  new_dimnames <- curr_dimnames
  new_dimnames[[arr_dim_idx]] <- new_values
  
  # Create new array
  new_array <- array(list(), dim = new_dims)
  dimnames(new_array) <- new_dimnames
  
  # Copy existing data
  if (prod(curr_dims) > 0) {
    for (i in 1:prod(curr_dims)) {
      if (!is.null(sfish_obj$data[[i]])) {
        # Get multi-dimensional index
        idx <- arrayInd(i, curr_dims)
        
        # Only copy if the dimension value is in the new set
        if (curr_dimnames[[arr_dim_idx]][idx[arr_dim_idx]] %in% new_values) {
          # Get new index position
          new_idx <- idx
          new_idx[arr_dim_idx] <- which(new_values == curr_dimnames[[arr_dim_idx]][idx[arr_dim_idx]])
          
          # Copy data
          new_array[new_idx] <- sfish_obj$data[[i]]
        }
      }
    }
  }
  
  # Update array
  sfish_obj$data <- new_array
  
  # Update metadata
  sfish_obj$metadata$dimensions[[dimension_name]] <- new_values
  
  # Add processing note
  sfish_obj$metadata$processingNotes <- c(
    sfish_obj$metadata$processingNotes,
    paste("Expanded dimension:", dimension_name)
  )
  
  return(sfish_obj)
}

#' Print method for SFish objects
#'
#' @param x SFish object
#' @export
print.SFish <- function(x, ...) {
  cat("SFish: Fisheries Spatial Data\n")
  cat("----------------------------\n")
  
  # Grid information
  cat("Grid Type:", x$metadata$gridType, "\n")
  cat("Resolution:", 
      ifelse(length(x$metadata$resolution) == 1, 
             paste0(x$metadata$resolution, "°"), 
             paste0(x$metadata$resolution[1], "° x ", 
                    x$metadata$resolution[2], "°")), "\n")
  cat("CRS:", x$metadata$crs, "\n")
  
  # Temporal extent
  if (!is.na(x$metadata$temporalExtent$start) || !is.na(x$metadata$temporalExtent$end)) {
    cat("Temporal Extent:", 
        ifelse(is.na(x$metadata$temporalExtent$start), "?", x$metadata$temporalExtent$start),
        "to",
        ifelse(is.na(x$metadata$temporalExtent$end), "?", x$metadata$temporalExtent$end),
        "\n")
  }
  
  # Array dimensions
  dims <- dim(x$data)
  dimnames_list <- dimnames(x$data)
  
  cat("\nDimensions:\n")
  cat("  Spatial units:", dims[1], "\n")
  
  if (length(dims) > 1) {
    dim_names <- names(x$metadata$dimensions)
    for (i in seq_along(dim_names)) {
      cat("  ", dim_names[i], ": ", dims[i+1], " (", 
          paste(head(dimnames_list[[i+1]], 3), collapse=", "),
          if(dims[i+1] > 3) "...", 
          ")\n", sep="")
    }
  }
  
  # Units
  cat("\nUnits:\n")
  for (unit_name in names(x$metadata$units)) {
    cat("  ", unit_name, ": ", x$metadata$units[[unit_name]], "\n", sep="")
  }
  
  # Creation date
  cat("\nCreated:", x$metadata$creationDate, "\n")
}