// [[Rcpp::plugins(cpp11, openmp)]]
#include <Rcpp.h>
#include <omp.h>
#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <memory>

// For SIMD instructions if supported
#ifdef __AVX__
#include <immintrin.h>
#define USE_SIMD 1
#else
#define USE_SIMD 0
#endif

using namespace Rcpp;

// Fast string splitting without depending on Boost
std::vector<std::string> split_string(const std::string& str, char delimiter) {
  std::vector<std::string> result;
  size_t start = 0;
  size_t end = str.find(delimiter);
  
  while (end != std::string::npos) {
    result.push_back(str.substr(start, end - start));
    start = end + 1;
    end = str.find(delimiter, start);
  }
  
  result.push_back(str.substr(start));
  return result;
}

// Optimized numeric sum with SIMD if available
// [[Rcpp::export]]
NumericVector sum_numeric_vectors(NumericVector v1, NumericVector v2) {
  int n = v1.size();
  NumericVector result(n);
  
#if USE_SIMD
  // Use AVX instructions if available
  double* p1 = &v1[0];
  double* p2 = &v2[0];
  double* pr = &result[0];
  
  int i = 0;
  for (; i <= n-4; i += 4) {
    __m256d av1 = _mm256_loadu_pd(p1 + i);
    __m256d av2 = _mm256_loadu_pd(p2 + i);
    __m256d sum = _mm256_add_pd(av1, av2);
    _mm256_storeu_pd(pr + i, sum);
  }
  
  // Handle remaining elements
  for (; i < n; i++) {
    pr[i] = p1[i] + p2[i];
  }
#else
  // Standard vectorized operation
  for (int i = 0; i < n; i++) {
    result[i] = v1[i] + v2[i];
  }
#endif
  
  return result;
}

// Optimized merge of two value lists
// [[Rcpp::export]]
List merge_values_cpp(List values1, List values2) {
  // Create result list
  List result;
  
  // Get field names from both lists
  CharacterVector names1 = values1.names();
  CharacterVector names2 = values2.names();
  
  // Create a set of unique field names
  std::unordered_map<std::string, bool> field_set;
  for (int i = 0; i < names1.length(); i++) {
    field_set[as<std::string>(names1[i])] = true;
  }
  for (int i = 0; i < names2.length(); i++) {
    field_set[as<std::string>(names2[i])] = true;
  }
  
  // Convert to vector for parallel processing
  std::vector<std::string> all_fields;
  all_fields.reserve(field_set.size());
  for (const auto& pair : field_set) {
    all_fields.push_back(pair.first);
  }
  
  // Define field categories
  std::unordered_map<std::string, int> field_types;
  std::vector<std::string> sum_fields = {"effort", "catch", "value", "kwHours", "kwFishinghours"};
  std::vector<std::string> avg_fields = {"avgGearWidth", "averageInterval", "avgKw", 
    "avgFishingSpeed", "icesAvgFishingSpeed", "avgOal"};
  
  // Set field types (1 = sum, 2 = avg, 3 = uniqueVessels, 4 = anonVessels, 0 = other)
  for (const auto& field : sum_fields) {
    field_types[field] = 1;
  }
  for (const auto& field : avg_fields) {
    field_types[field] = 2;
  }
  field_types["uniqueVessels"] = 3;
  field_types["anonVessels"] = 4;
  
  // Get effort values for weighting
  double effort1 = 0.0;
  double effort2 = 0.0;
  
  if (values1.containsElementNamed("effort")) {
    SEXP val = values1["effort"];
    if (!Rf_isNull(val) && TYPEOF(val) == REALSXP && !ISNA(REAL(val)[0])) {
      effort1 = REAL(val)[0];
    }
  }
  
  if (values2.containsElementNamed("effort")) {
    SEXP val = values2["effort"];
    if (!Rf_isNull(val) && TYPEOF(val) == REALSXP && !ISNA(REAL(val)[0])) {
      effort2 = REAL(val)[0];
    }
  }
  
  double total_effort = effort1 + effort2;
  
  // Pre-allocate result List
  result = List(all_fields.size());
  CharacterVector result_names(all_fields.size());
  
  // Process fields in parallel if there are many
#pragma omp parallel for if(all_fields.size() > 10)
  for (size_t i = 0; i < all_fields.size(); i++) {
    const std::string& field = all_fields[i];
    result_names[i] = field;
    
    bool in_values1 = values1.containsElementNamed(field.c_str());
    bool in_values2 = values2.containsElementNamed(field.c_str());
    
    SEXP result_value;
    
    // Get field type
    int field_type = 0; // default type
    auto it = field_types.find(field);
    if (it != field_types.end()) {
      field_type = it->second;
    }
    
    if (field_type == 1) {
      // Sum fields
      double val1 = 0.0;
      double val2 = 0.0;
      
      if (in_values1) {
        SEXP sexp_val = values1[field];
        if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
          val1 = REAL(sexp_val)[0];
        }
      }
      
      if (in_values2) {
        SEXP sexp_val = values2[field];
        if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
          val2 = REAL(sexp_val)[0];
        }
      }
      
      result_value = Rf_ScalarReal(val1 + val2);
    }
    else if (field_type == 2) {
      // Average fields weighted by effort
      if (total_effort > 0) {
        double weighted_val1 = 0.0;
        double weighted_val2 = 0.0;
        
        if (in_values1) {
          SEXP sexp_val = values1[field];
          if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
            weighted_val1 = REAL(sexp_val)[0] * effort1;
          }
        }
        
        if (in_values2) {
          SEXP sexp_val = values2[field];
          if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
            weighted_val2 = REAL(sexp_val)[0] * effort2;
          }
        }
        
        result_value = Rf_ScalarReal((weighted_val1 + weighted_val2) / total_effort);
      } else {
        // Simple average if no effort
        std::vector<double> vals;
        
        if (in_values1) {
          SEXP sexp_val = values1[field];
          if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
            vals.push_back(REAL(sexp_val)[0]);
          }
        }
        
        if (in_values2) {
          SEXP sexp_val = values2[field];
          if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
            vals.push_back(REAL(sexp_val)[0]);
          }
        }
        
        if (!vals.empty()) {
          double sum = 0.0;
          for (double v : vals) sum += v;
          result_value = Rf_ScalarReal(sum / vals.size());
        } else {
          result_value = Rf_ScalarReal(NA_REAL);
        }
      }
    }
    else if (field_type == 3) {
      // uniqueVessels - simple sum
      double val1 = 0.0;
      double val2 = 0.0;
      
      if (in_values1) {
        SEXP sexp_val = values1[field];
        if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
          val1 = REAL(sexp_val)[0];
        }
      }
      
      if (in_values2) {
        SEXP sexp_val = values2[field];
        if (TYPEOF(sexp_val) == REALSXP && !Rf_isNull(sexp_val) && !ISNA(REAL(sexp_val)[0])) {
          val2 = REAL(sexp_val)[0];
        }
      }
      
      result_value = Rf_ScalarReal(val1 + val2);
    }
    else if (field_type == 4) {
      // anonVessels - need to handle in R
#pragma omp critical
{
  Function process_anon_vessels("process_anon_vessels_r");
  result_value = process_anon_vessels(values1, values2);
}
    }
    else {
      // Other fields - use second if available, otherwise first
      bool found = false;
      
      if (in_values2) {
        SEXP val = values2[field];
        if (!Rf_isNull(val)) {
          result_value = val;
          found = true;
        }
      }
      
      if (!found && in_values1) {
        SEXP val = values1[field];
        if (!Rf_isNull(val)) {
          result_value = val;
          found = true;
        }
      }
      
      if (!found) {
        // Default to NA
        if (field == "geometry") {
          result_value = Rf_mkString("");  // Changed from NA_STRING to empty string
        } else {
          result_value = Rf_ScalarReal(NA_REAL);
        }
      }
    }
    
#pragma omp critical
{
  result[i] = result_value;
}
  }
  
  result.names() = result_names;
  return result;
}

// Optimized key parsing
// [[Rcpp::export]]
List parse_dim_key_cpp(CharacterVector key) {
  std::string key_str = as<std::string>(key[0]);
  
  // Split by pipe character
  std::vector<std::string> parts = split_string(key_str, '|');
  
  // Prepare result
  List result(parts.size());
  CharacterVector result_names(parts.size());
  
  // Parse each key-value pair
  for (size_t i = 0; i < parts.size(); i++) {
    std::vector<std::string> kv = split_string(parts[i], '=');
    if (kv.size() == 2) {
      result_names[i] = kv[0];
      result[i] = kv[1];
    }
  }
  
  result.names() = result_names;
  return result;
}

// Batch parse dimension keys in parallel
// [[Rcpp::export]]
List batch_parse_dim_keys_cpp(CharacterVector keys) {
  int n = keys.size();
  List result(n);
  
  // Use parallel processing for large batches
#pragma omp parallel for if(n > 100)
  for (int i = 0; i < n; i++) {
    CharacterVector key_vec = CharacterVector::create(keys[i]);
    
#pragma omp critical
{
  result[i] = parse_dim_key_cpp(key_vec);
}
  }
  
  // Set names
  result.names() = keys;
  
  return result;
}

// Helper function to check if a string exists in a vector
inline bool contains(const std::vector<std::string>& vec, const std::string& val) {
  return std::find(vec.begin(), vec.end(), val) != vec.end();
}

// Cache-efficient batch processing of spatial units
// [[Rcpp::export]]
List process_spatial_units_cpp(List spatial_units1, List spatial_units2, 
                               Function merge_function, int batch_size = 100) {
  // Get all spatial IDs from both objects
  CharacterVector names1 = spatial_units1.names();
  CharacterVector names2 = spatial_units2.names();
  
  std::unordered_map<std::string, bool> id_map;
  for (int i = 0; i < names1.size(); i++) {
    id_map[as<std::string>(names1[i])] = true;
  }
  for (int i = 0; i < names2.size(); i++) {
    id_map[as<std::string>(names2[i])] = true;
  }
  
  // Convert to vector
  std::vector<std::string> all_ids;
  all_ids.reserve(id_map.size());
  for (const auto& pair : id_map) {
    all_ids.push_back(pair.first);
  }
  
  // Prepare result
  List result(all_ids.size());
  CharacterVector result_names(all_ids.size());
  
  // Process in batches for better cache efficiency
  for (size_t batch_start = 0; batch_start < all_ids.size(); batch_start += batch_size) {
    size_t batch_end = std::min(batch_start + batch_size, all_ids.size());
    
    // Process this batch in parallel
#pragma omp parallel for
    for (size_t i = batch_start; i < batch_end; i++) {
      const std::string& spatial_id = all_ids[i];
      result_names[i] = spatial_id;
      
      // Get data from both objects
      List unit_result = List::create(
        Named("geometry") = R_NilValue,
        Named("data") = List::create()
      );
      
      // Set geometry
      if (id_map.count(spatial_id) > 0) {
        if (spatial_units1.containsElementNamed(spatial_id.c_str())) {
          List unit1 = spatial_units1[spatial_id];
          unit_result["geometry"] = unit1["geometry"];
        } else if (spatial_units2.containsElementNamed(spatial_id.c_str())) {
          List unit2 = spatial_units2[spatial_id];
          unit_result["geometry"] = unit2["geometry"];
        }
      }
      
      // Get data lists
      List data1 = List::create();
      List data2 = List::create();
      
      if (spatial_units1.containsElementNamed(spatial_id.c_str())) {
        List unit1 = spatial_units1[spatial_id];
        data1 = unit1["data"];
      }
      
      if (spatial_units2.containsElementNamed(spatial_id.c_str())) {
        List unit2 = spatial_units2[spatial_id];
        data2 = unit2["data"];
      }
      
      // Merge data
      CharacterVector data1_names = data1.names();
      CharacterVector data2_names = data2.names();
      
      std::unordered_map<std::string, bool> key_map;
      for (int j = 0; j < data1_names.size(); j++) {
        key_map[as<std::string>(data1_names[j])] = true;
      }
      for (int j = 0; j < data2_names.size(); j++) {
        key_map[as<std::string>(data2_names[j])] = true;
      }
      
      // Convert to vector
      std::vector<std::string> all_keys;
      all_keys.reserve(key_map.size());
      for (const auto& pair : key_map) {
        all_keys.push_back(pair.first);
      }
      
      // Process each key
      List merged_data(all_keys.size());
      CharacterVector merged_data_names(all_keys.size());
      
      for (size_t k = 0; k < all_keys.size(); k++) {
        const std::string& dim_key = all_keys[k];
        merged_data_names[k] = dim_key;
        
        // Merge data values
        if (data1.containsElementNamed(dim_key.c_str()) && 
            data2.containsElementNamed(dim_key.c_str())) {
          // Key exists in both - merge
#pragma omp critical
{
  merged_data[k] = merge_function(data1[dim_key], data2[dim_key]);
}
        } else if (data1.containsElementNamed(dim_key.c_str())) {
          // Key only in first object
          merged_data[k] = data1[dim_key];
        } else {
          // Key only in second object
          merged_data[k] = data2[dim_key];
        }
      }
      
      merged_data.names() = merged_data_names;
      unit_result["data"] = merged_data;
      
#pragma omp critical
{
  result[i] = unit_result;
}
    }
  }
  
  result.names() = result_names;
  return result;
}