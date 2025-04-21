library(Rcpp)
library(BH)

# Compile the C++ code
sourceCpp("src/sfish_cpp.cpp")

# Source R files
source("R/class_declaration.R")
source("R/object_creation.R")
source("R/rcpp_interface.R")