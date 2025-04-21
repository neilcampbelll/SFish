#' Add two SFish objects together with optimised performance
#'
#' This function efficiently combines two SFish objects by merging their spatial units,
#' dimensions, and data values. It uses data.table and other optimisations for improved
#' performance when working with large datasets.
#'
#' @param sfish1 First SFish object
#' @param sfish2 Second SFish object
#' @param merge_function Function to merge data values when the same spatial unit and
#'        dimension combination exists in both objects. Default is smart_merge.
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
  result@metadata@temporalExtent <- merge_temporal_extent(
    sfish1@metadata@temporalExtent,
    sfish2@metadata@temporalExtent
  )
  
  # Add processing notes
  result@metadata@processingNotes <- c(
    result@metadata@processingNotes,
    "Combined two SFish objects using optimised method"
  )
  
  # Get all spatial IDs from both objects
  all_spatial_ids <- unique(c(
    names(sfish1@data$spatialUnits),
    names(sfish2@data$spatialUnits)
  ))
  
  # Pre-allocate all spatial units in the result object
  result@data$spatialUnits <- vector("list", length(all_spatial_ids))
  names(result@data$spatialUnits) <- all_spatial_ids
  
  # Add geometries for all spatial units
  for (spatial_id in all_spatial_ids) {
    if (spatial_id %in% names(sfish1@data$spatialUnits)) {
      geometry <- sfish1@data$spatialUnits[[spatial_id]]$geometry
    } else {
      geometry <- sfish2@data$spatialUnits[[spatial_id]]$geometry
    }
    
    result@data$spatialUnits[[spatial_id]] <- list(
      geometry = geometry,
      data = list()
    )
  }
  
  # Create lookup tables for dimension keys
  sfish1_keys <- parse_dim_keys(sfish1)
  sfish2_keys <- parse_dim_keys(sfish2)
  
  # Process data in batches
  batch_size <- 100  # Adjust based on your dataset size
  
  for (i in seq(1, length(all_spatial_ids), by = batch_size)) {
    batch_end <- min(i + batch_size - 1, length(all_spatial_ids))
    batch_ids <- all_spatial_ids[i:batch_end]
    
    # Process this batch of spatial IDs
    for (spatial_id in batch_ids) {
      # Track processed keys to avoid duplicates
      processed_keys <- character(0)
      
      # First, process data from the first object
      if (spatial_id %in% names(sfish1@data$spatialUnits)) {
        unit_data1 <- sfish1@data$spatialUnits[[spatial_id]]$data
        
        for (dim_key in names(unit_data1)) {
          # Add data from first object
          result@data$spatialUnits[[spatial_id]]$data[[dim_key]] <- unit_data1[[dim_key]]
          processed_keys <- c(processed_keys, dim_key)
        }
      }
      
      # Then, process data from the second object
      if (spatial_id %in% names(sfish2@data$spatialUnits)) {
        unit_data2 <- sfish2@data$spatialUnits[[spatial_id]]$data
        
        for (dim_key in names(unit_data2)) {
          if (dim_key %in% processed_keys) {
            # This key exists in both objects, merge the data
            data1 <- result@data$spatialUnits[[spatial_id]]$data[[dim_key]]
            data2 <- unit_data2[[dim_key]]
            result@data$spatialUnits[[spatial_id]]$data[[dim_key]] <- merge_function(data1, data2)
          } else {
            # This key only exists in the second object, add it directly
            result@data$spatialUnits[[spatial_id]]$data[[dim_key]] <- unit_data2[[dim_key]]
          }
        }
      }
    }
  }
  
  return(result)
}

#' Smart merge function for SFish data values
#'
#' @param values1 Data values from first SFish object
#' @param values2 Data values from second SFish object
#' @return Merged data values
smart_merge <- function(values1, values2) {
  result <- list()
  
  # Fields that should be summed (absolute values)
  sum_fields <- c("effort", "catch", "value", "kwHours", "kwFishinghours")
  
  # Fields that should be averaged with weighting by effort
  avg_fields <- c("avgGearWidth", "averageInterval", "avgKw", "avgFishingSpeed", 
                  "icesAvgFishingSpeed", "avgOal")
  
  # Get effort values for weighting
  effort1 <- ifelse("effort" %in% names(values1), values1$effort, 0)
  effort2 <- ifelse("effort" %in% names(values2), values2$effort, 0)
  total_effort <- effort1 + effort2
  
  # Process all fields
  all_fields <- unique(c(names(values1), names(values2)))
  
  # Use data.table for faster field processing
  dt <- data.table::data.table(
    field = all_fields,
    value1 = NA_real_,
    value2 = NA_real_
  )
  
  # Fill in values where they exist
  for (i in 1:nrow(dt)) {
    field <- dt$field[i]
    if (field %in% names(values1)) dt$value1[i] <- values1[[field]]
    if (field %in% names(values2)) dt$value2[i] <- values2[[field]]
  }
  
  # Process fields by category
  for (field in all_fields) {
    # Handle fields that should be summed
    if (field %in% sum_fields) {
      val1 <- if (field %in% names(values1) && !is.na(values1[[field]])) values1[[field]] else 0
      val2 <- if (field %in% names(values2) && !is.na(values2[[field]])) values2[[field]] else 0
      result[[field]] <- val1 + val2
    }
    # Handle fields that should be averaged with weighting
    else if (field %in% avg_fields) {
      if (total_effort > 0) {
        val1 <- if (field %in% names(values1) && !is.na(values1[[field]])) values1[[field]] * effort1 else 0
        val2 <- if (field %in% names(values2) && !is.na(values2[[field]])) values2[[field]] * effort2 else 0
        result[[field]] <- (val1 + val2) / total_effort
      } else {
        # If no effort, simple average
        val1 <- if (field %in% names(values1) && !is.na(values1[[field]])) values1[[field]] else 0
        val2 <- if (field %in% names(values2) && !is.na(values2[[field]])) values2[[field]] else 0
        result[[field]] <- (val1 + val2) / (exists1 + exists2)
      }
    }
    # Special handling for uniqueVessels
    else if (field == "uniqueVessels") {
      val1 <- if (field %in% names(values1) && !is.na(values1[[field]])) values1[[field]] else 0
      val2 <- if (field %in% names(values2) && !is.na(values2[[field]])) values2[[field]] else 0
      result[[field]] <- val1 + val2
    }
    # Special handling for anonVessels
    else if (field == "anonVessels") {
      vessels1 <- if (field %in% names(values1) && !is.na(values1[[field]])) {
        if (values1[[field]] == "not_required") {
          # If already anonymized, count as 3+ vessels
          c("anon1", "anon2", "anon3")  # Placeholder to indicate 3+ vessels
        } else {
          unlist(strsplit(as.character(values1[[field]]), ";"))
        }
      } else character(0)
      
      vessels2 <- if (field %in% names(values2) && !is.na(values2[[field]])) {
        if (values2[[field]] == "not_required") {
          # If already anonymized, count as 3+ vessels
          c("anon4", "anon5", "anon6")  # Different placeholders to ensure uniqueness
        } else {
          unlist(strsplit(as.character(values2[[field]]), ";"))
        }
      } else character(0)
      
      # Combine unique vessels
      all_vessels <- unique(c(vessels1, vessels2))
      
      # Check if we need anonymization
      if (length(all_vessels) > 2) {
        result[[field]] <- "not_required"
      } else {
        result[[field]] <- paste(all_vessels, collapse = ";")
      }
    }
    # For other fields, use the value from the second object if it exists, otherwise from the first
    else {
      if (field %in% names(values2) && !is.na(values2[[field]])) {
        result[[field]] <- values2[[field]]
      } else if (field %in% names(values1) && !is.na(values1[[field]])) {
        result[[field]] <- values1[[field]]
      }
    }
  }
  
  return(result)
}

#' Helper function to efficiently merge dimensions
#'
#' @param dims1 Dimensions from first SFish object
#' @param dims2 Dimensions from second SFish object
#' @return Merged dimensions
merge_dimensions <- function(dims1, dims2) {
  # Create data.table of all dimension names and values
  dt <- data.table::data.table()
  
  # Add dimensions from first object
  for (name in names(dims1)) {
    dt_tmp <- data.table::data.table(
      dim_name = name,
      dim_value = dims1[[name]]
    )
    dt <- data.table::rbindlist(list(dt, dt_tmp))
  }
  
  # Add dimensions from second object
  for (name in names(dims2)) {
    dt_tmp <- data.table::data.table(
      dim_name = name,
      dim_value = dims2[[name]]
    )
    dt <- data.table::rbindlist(list(dt, dt_tmp))
  }
  
  # Remove duplicates
  dt <- unique(dt)
  
  # Convert back to list format
  result <- list()
  for (name in unique(dt$dim_name)) {
    result[[name]] <- dt[dim_name == name, dim_value]
  }
  
  return(result)
}

#' Helper function to merge temporal extents
#'
#' @param extent1 Temporal extent from first SFish object
#' @param extent2 Temporal extent from second SFish object
#' @return Merged temporal extent
merge_temporal_extent <- function(extent1, extent2) {
  # Combine temporal extents
  start1 <- extent1$start
  start2 <- extent2$start
  end1 <- extent1$end
  end2 <- extent2$end
  
  result <- list(
    start = if (is.na(start1) || is.na(start2)) {
      if (is.na(start1)) start2 else start1
    } else {
      min(start1, start2)
    },
    end = if (is.na(end1) || is.na(end2)) {
      if (is.na(end1)) end2 else end1
    } else {
      max(end1, end2)
    }
  )
  
  return(result)
}

#' Helper function to parse dimension keys
#'
#' @param sfish SFish object
#' @return Named list of parsed dimension keys
parse_dim_keys <- function(sfish) {
  # Use data.table for faster processing
  result <- list()
  
  # Collect all unique dimension keys
  all_keys <- character(0)
  
  for (spatial_id in names(sfish@data$spatialUnits)) {
    keys <- names(sfish@data$spatialUnits[[spatial_id]]$data)
    all_keys <- c(all_keys, keys)
  }
  
  all_keys <- unique(all_keys)
  
  # Parse each key just once
  for (key in all_keys) {
    dim_parts <- strsplit(key, "\\|")[[1]]
    parsed <- list()
    
    for (part in dim_parts) {
      kv <- strsplit(part, "=")[[1]]
      parsed[[kv[1]]] <- kv[2]
    }
    
    result[[key]] <- parsed
  }
  
  return(result)
}