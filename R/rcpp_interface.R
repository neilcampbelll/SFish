#' Process anonymous vessels in R
#' 
#' Helper function called from C++ to handle complex string operations
#' @param values1 Data values from first object
#' @param values2 Data values from second object
#' @return Processed anonymous vessels value
#' @keywords internal
process_anon_vessels_r <- function(values1, values2) {
  vessels <- character(0)
  
  # Process first object's vessels
  if ("anonVessels" %in% names(values1) && !is.na(values1$anonVessels)) {
    if (values1$anonVessels == "not_required") {
      vessels <- c(vessels, "anon1", "anon2", "anon3")
    } else {
      vessels <- c(vessels, unlist(strsplit(as.character(values1$anonVessels), ";")))
    }
  }
  
  # Process second object's vessels
  if ("anonVessels" %in% names(values2) && !is.na(values2$anonVessels)) {
    if (values2$anonVessels == "not_required") {
      vessels <- c(vessels, "anon4", "anon5", "anon6")
    } else {
      vessels <- c(vessels, unlist(strsplit(as.character(values2$anonVessels), ";")))
    }
  }
  
  # Get unique vessels
  unique_vessels <- unique(vessels)
  
  # Apply anonymization rule
  if (length(unique_vessels) > 2) {
    return("not_required")
  } else if (length(unique_vessels) > 0) {
    return(paste(unique_vessels, collapse = ";"))
  } else {
    return(NA_character_)
  }
}


#' Smart merge function using C++ implementation
#'
#' @param values1 Data values from first SFish object
#' @param values2 Data values from second SFish object
#' @return Merged data values
#' @export
smart_merge <- function(values1, values2) {
  # Early return if either input is empty
  if (length(values1) == 0) return(values2)
  if (length(values2) == 0) return(values1)
  
  # Call C++ function to merge values
  return(merge_values_cpp(values1, values2))
}

#' Add two SFish objects together with C++ acceleration
#'
#' This function efficiently combines two SFish objects by merging their spatial units,
#' dimensions, and data values. It uses C++ for the most computationally intensive
#' operations for maximum performance.
#'
#' @param sfish1 First SFish object
#' @param sfish2 Second SFish object
#' @param merge_function Function to merge data values. Default is smart_merge.
#'
#' @return A new SFish object containing the combined data
#' @export
add_sfish <- function(sfish1, sfish2, merge_function = smart_merge) {
  # Check that both are SFish objects
  if (!is(sfish1, "SFish") || !is(sfish2, "SFish")) {
    stop("Both arguments must be SFish objects")
  }
  
  # Check that both have the same grid type
  if (sfish1@metadata@gridType != sfish2@metadata@gridType) {
    stop("Cannot combine SFish objects with different grid types: ",
         sfish1@metadata@gridType, " and ", sfish2@metadata@gridType)
  }
  
  # Efficiently merge dimensions
  new_dimensions <- merge_dimensions(sfish1@data$dimensions, sfish2@data$dimensions)
  
  # Create new SFish object
  result <- create_empty_sfish(
    dimensions = new_dimensions,
    gridType = sfish1@metadata@gridType,
    resolution = sfish1@metadata@resolution,
    crs = sfish1@metadata@crs
  )
  
  # Update metadata
  result@metadata@dataSources <- unique(c(
    sfish1@metadata@dataSources,
    sfish2@metadata@dataSources
  ))
  
  # Combine temporal extents
  result@metadata@temporalExtent <- list(
    start = min_na(sfish1@metadata@temporalExtent$start, sfish2@metadata@temporalExtent$start),
    end = max_na(sfish1@metadata@temporalExtent$end, sfish2@metadata@temporalExtent$end)
  )
  
  # Add processing notes
  result@metadata@processingNotes <- c(
    result@metadata@processingNotes,
    "Combined two SFish objects using C++ acceleration"
  )
  
  # REPLACE THIS SECTION WITH THE OPTIMIZED C++ CALL
  # Instead of processing each spatial unit one by one in R,
  # use the optimized C++ function that processes them all at once
  result@data$spatialUnits <- process_spatial_units_cpp(
    sfish1@data$spatialUnits, 
    sfish2@data$spatialUnits,
    merge_function
  )
  
  return(result)
}


#' Helper function to merge dimensions
#'
#' @param dims1 Dimensions from first SFish object
#' @param dims2 Dimensions from second SFish object
#' @return Merged dimensions
#' @keywords internal
merge_dimensions <- function(dims1, dims2) {
  result <- dims1
  
  for (name in names(dims2)) {
    if (name %in% names(result)) {
      # Combine unique values efficiently
      result[[name]] <- unique(c(result[[name]], dims2[[name]]))
    } else {
      result[[name]] <- dims2[[name]]
    }
  }
  
  return(result)
}

#' Min function that handles NA values
#'
#' @param x First value
#' @param y Second value
#' @return Minimum non-NA value, or NA if both are NA
#' @keywords internal
min_na <- function(x, y) {
  if (is.na(x)) return(y)
  if (is.na(y)) return(x)
  return(min(x, y))
}

#' Max function that handles NA values
#'
#' @param x First value
#' @param y Second value
#' @return Maximum non-NA value, or NA if both are NA
#' @keywords internal
max_na <- function(x, y) {
  if (is.na(x)) return(y)
  if (is.na(y)) return(x)
  return(max(x, y))
}