# =============================== #
#### Who still WFH? replication files ####
#### 0. Master run file ####
#### Aaron Mollross ####
# =============================== #


#### USER TO EDIT ####
hilda_data <- "location/" # Folder location where combined data from HILDA wave 24 (restricted release) is located

output <- "location/" # Desired location for outputs


#### GITHUB SOURCE ####
base_url <- "https://raw.githubusercontent.com/amollross/whostillWFH/main/" 


#### RUN COMMANDS ####
run_script <- function(file) {
  message("Running: ", file)
  source(paste0(base_url, file), echo = FALSE)
}

# Data combination and cleaning (pre-requisite for all other scripts)
run_script("code/01_prepare_HILDA.R")

# Main regression results (including coefficient plots)
run_script("code/02_main_results.R")

# Heterogeneity and sensitivity analysis
run_script("code/03_heterogeneity_and_sensitivity.R")

# Appendix charts and descriptive statistics
run_script("code/04_charts_and_descriptive_stats.R")


