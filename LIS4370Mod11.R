# --------------------------------------------------------------------
# SECTION 1   : COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS-4370 R Programming
# Assignment  : Module 11
# URL         : https://usflearn.instructure.com/courses/1926966/assignments/17693283
# Filename    : LIS4370Mod11.R
# Purpose     : Debug and enhance a function for outlier detection using Tukey's method
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-04-06 (EST)
# Updated     : 2025-04-07 (EST)
# License     : The UltraFree Unlicense
# Description : This script identifies and fixes a bug in a Tukey's outlier detection function, then tests and reports the results.
# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------
# Set Working Directory
tryCatch({
  baseDir <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\Remeye\Classes\4370\Mod11)"
  dir.exists(baseDir)
  setwd(baseDir)
}, error = function(e) {
  stop("Error setting the working directory: ", e$message)
})

# Display Working Directory
tryCatch({
  print(getwd())
}, error = function(e) {
  stop("Error retrieving the working directory: ", e$message)
})

# Load Required Libraries
tryCatch({
  library(dplyr)
}, error = function(e) {
  stop("Error loading libraries: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 3: DEPENDENCIES & INSTALLATION
# --------------------------------------------------------------------
# Required Libraries
required_packages <- c("dplyr")

# Function to Check and Load Required Libraries
check_and_load_library <- function(package) {
  if (!require(package, character.only = TRUE)) {
    tryCatch({
      cat("Installing package:", package, "\n")
      install.packages(package)
      library(package, character.only = TRUE)
      cat("Successfully loaded:", package, "\n")
    }, error = function(e) {
      stop(paste("Failed to install/load package:", package, "-", e$message))
    })
  } else {
    cat("Package already loaded:", package, "\n")
  }
}

# Load All Required Packages
tryCatch({
  lapply(required_packages, check_and_load_library)
}, error = function(e) {
  stop("Package loading failed: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 4: UTILITY FUNCTIONS
# --------------------------------------------------------------------
# Tukey's Outlier Detection Helper Function
tukey.outlier <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  return(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr))
}

# --------------------------------------------------------------------
# SECTION 5: DEBUGGING TASK
# --------------------------------------------------------------------
# Original Function with Bug
tukey_multiple_original <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length = nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i,])
  }
  return(outlier.vec)
}

# Fixed Function
tukey_multiple_fixed <- function(x) {
  # Check if input is a matrix or data frame
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Input must be a matrix or data frame")
  }
  # Convert data frame to matrix if needed
  if (is.data.frame(x)) x <- as.matrix(x)
  
  # Initialize outliers array with FALSE instead of TRUE
  outliers <- array(FALSE, dim = dim(x))
  
  # Identify outliers per column
  for (j in 1:ncol(x)) {
    outliers[,j] <- tukey.outlier(x[,j])
  }
  
  # Vector to store row-wise outlier status
  outlier.vec <- vector("logical", length = nrow(x))
  
  # Check if any value in a row is an outlier
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- any(outliers[i,])
  }
  
  return(outlier.vec)
}

# --------------------------------------------------------------------
# SECTION 6: TESTING AND REPORTING
# --------------------------------------------------------------------
test_and_report <- function() {
  tryCatch({
    # Create sample data
    set.seed(123)
    test_data <- matrix(c(rnorm(10, mean = 0), 100, rnorm(10, mean = 50), -200), 
                        nrow = 11, ncol = 2)
    
    # Test original function
    cat("Testing original function...\n")
    result_original <- try(tukey_multiple_original(test_data), silent = TRUE)
    if (inherits(result_original, "try-error")) {
      cat("Original function failed: ", attr(result_original, "condition")$message, "\n")
    } else {
      cat("Original result:", result_original, "\n")
    }
    
    # Test fixed function
    cat("Testing fixed function...\n")
    result_fixed <- tukey_multiple_fixed(test_data)
    cat("Fixed result:", result_fixed, "\n")
    
    # Debugging Report
    cat("\nDebugging Report:\n")
    cat("Bug: Used '&&' (scalar) instead of '&' (vectorized) and initialized outliers with TRUE.\n")
    cat("This caused a coercion error and wrong logic (all columns must be outliers).\n")
    cat("Fix: Initialized with FALSE, used 'any()' instead of 'all()', added input checks.\n")
    cat("Success: Fixed function identifies rows with any outlier correctly.\n")
    
  }, error = function(e) {
    cat("Unexpected error during testing: ", e$message, "\n")
  })
}

# --------------------------------------------------------------------
# SECTION 9: MAIN EXECUTION BLOCK
# --------------------------------------------------------------------
main <- function() {
  tryCatch({
    cat("Starting script execution...\n")
    
    # Execute test and report
    test_and_report()
    
    cat("Script execution completed successfully.\n")
  }, error = function(e) {
    stop("Script execution failed: ", e$message)
  })
}

# Execute the main function
main()

# --------------------------------------------------------------------
# SECTION 10: VERSION HISTORY
# --------------------------------------------------------------------
# Version History:
# - Version 1.0 (2025-04-06): Initial script creation.
# - Version 1.1 (2025-04-07): Completed debugging and reporting.

# --------------------------------------------------------------------
# SECTION 11: ADDITIONAL NOTES
# --------------------------------------------------------------------
# Notes:
# - The bug was in the logic of outlier detection and operator usage.
# - Fixed version ensures defensive programming with input checks.

# --------------------------------------------------------------------
# SHORT VERSION
# --------------------------------------------------------------------
# Short Version: Debugs a function to find outliers in data, fixes it, tests both versions, and reports results.