#' Smart merge function for SFish data values with optimised performance
#'
#' @param values1 Data values from first SFish object
#' @param values2 Data values from second SFish object
#' @return Merged data values
smart_merge <- function(values1, values2) {
  # Early return if either input is empty
  if (length(values1) == 0) return(values2)
  if (length(values2) == 0) return(values1)
  
  # Pre-allocate result list with estimated size
  all_fields <- unique(c(names(values1), names(values2)))
  result <- vector("list", length(all_fields))
  names(result) <- all_fields
  
  # Get effort values for weighting once
  effort1 <- if ("effort" %in% names(values1) && !is.na(values1$effort)) values1$effort else 0
  effort2 <- if ("effort" %in% names(values2) && !is.na(values2$effort)) values2$effort else 0
  total_effort <- effort1 + effort2
  
  # Define field categories
  sum_fields <- c("effort", "catch", "value", "kwHours", "kwFishinghours")
  avg_fields <- c("avgGearWidth", "averageInterval", "avgKw", "avgFishingSpeed", 
                  "icesAvgFishingSpeed", "avgOal")
  
  # Pre-compute existence flags
  has_field1 <- names(values1)
  has_field2 <- names(values2)
  
  # Process fields by category using vectorisation where possible
  
  # 1. Process sum fields in a single step
  sum_to_process <- intersect(all_fields, sum_fields)
  if (length(sum_to_process) > 0) {
    for (field in sum_to_process) {
      val1 <- if (field %in% has_field1 && !is.na(values1[[field]])) values1[[field]] else 0
      val2 <- if (field %in% has_field2 && !is.na(values2[[field]])) values2[[field]] else 0
      result[[field]] <- val1 + val2
    }
  }
  
  # 2. Process average fields in a single step
  avg_to_process <- intersect(all_fields, avg_fields)
  if (length(avg_to_process) > 0) {
    if (total_effort > 0) {
      for (field in avg_to_process) {
        val1 <- if (field %in% has_field1 && !is.na(values1[[field]])) values1[[field]] * effort1 else 0
        val2 <- if (field %in% has_field2 && !is.na(values2[[field]])) values2[[field]] * effort2 else 0
        result[[field]] <- (val1 + val2) / total_effort
      }
    } else {
      # If no effort, simple mean of available values
      for (field in avg_to_process) {
        vals <- numeric(0)
        if (field %in% has_field1 && !is.na(values1[[field]])) vals <- c(vals, values1[[field]])
        if (field %in% has_field2 && !is.na(values2[[field]])) vals <- c(vals, values2[[field]])
        result[[field]] <- if (length(vals) > 0) mean(vals) else NA
      }
    }
  }
  
  # 3. Special handling for uniqueVessels - just sum
  if ("uniqueVessels" %in% all_fields) {
    val1 <- if ("uniqueVessels" %in% has_field1 && !is.na(values1$uniqueVessels)) values1$uniqueVessels else 0
    val2 <- if ("uniqueVessels" %in% has_field2 && !is.na(values2$uniqueVessels)) values2$uniqueVessels else 0
    result$uniqueVessels <- val1 + val2
  }
  
  # 4. Special handling for anonVessels - handle as hash set for faster operations
  if ("anonVessels" %in% all_fields) {
    # Use faster data structure for vessels
    vessels <- character(0)
    
    # Process first object's vessels
    if ("anonVessels" %in% has_field1 && !is.na(values1$anonVessels)) {
      if (values1$anonVessels == "not_required") {
        # Already anonymized
        vessels <- c(vessels, "anon1", "anon2", "anon3")
      } else {
        vessels <- c(vessels, unlist(strsplit(as.character(values1$anonVessels), ";")))
      }
    }
    
    # Process second object's vessels
    if ("anonVessels" %in% has_field2 && !is.na(values2$anonVessels)) {
      if (values2$anonVessels == "not_required") {
        # Already anonymized
        vessels <- c(vessels, "anon4", "anon5", "anon6")
      } else {
        vessels <- c(vessels, unlist(strsplit(as.character(values2$anonVessels), ";")))
      }
    }
    
    # Get unique vessels efficiently 
    unique_vessels <- unique(vessels)
    
    # Apply anonymization rule with fast length check
    if (length(unique_vessels) > 2) {
      result$anonVessels <- "not_required"
    } else if (length(unique_vessels) > 0) {
      result$anonVessels <- paste(unique_vessels, collapse = ";")
    } else {
      result$anonVessels <- NA
    }
  }
  
  # 5. For all other fields, use the second if available, otherwise the first
  other_fields <- setdiff(all_fields, c(sum_fields, avg_fields, "uniqueVessels", "anonVessels"))
  if (length(other_fields) > 0) {
    for (field in other_fields) {
      if (field %in% has_field2 && !is.na(values2[[field]])) {
        result[[field]] <- values2[[field]]
      } else if (field %in% has_field1 && !is.na(values1[[field]])) {
        result[[field]] <- values1[[field]]
      } else {
        result[[field]] <- NA
      }
    }
  }
  
  return(result)
}