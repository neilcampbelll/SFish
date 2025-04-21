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

# Get VMS data
#library(icesVMS)
#vms_data <- icesVMS::get_vms(country = "IE", year = 2022)

# Convert to SFish object
#library(SFish)
sfish_obj <- icesVMS_to_SFish(vms_data)

# View the result
sfish_obj
