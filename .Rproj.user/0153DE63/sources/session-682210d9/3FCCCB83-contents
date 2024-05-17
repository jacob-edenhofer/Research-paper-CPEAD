
################################
# Preliminaries
################################

# Load packages
library(tidyverse)  # includes dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)     # For reading Excel files
library(data.table) # For fast data manipulation
library(countrycode) # For converting country names to codes
library(tidylog)    # For enhanced error messages
library(arrow)      # Efficient data reading and writing
library(janitor)    # For clean_names()
library(here)       # For constructing paths
library(fixest)     # For fast fixed effects estimations models
library(fwlplot)    # For fixest plots
library(kableExtra) # For enhanced table output
library(patchwork)  # For combining plots
library(modelsummary) # For model output
library(marginaleffects) # For marginal effects plots
library(kableExtra) # For enhanced table output
library(scales)    # For formatting axes
library(patchwork) # For combining plots
library(haven)     # For reading Stata files



# import data 

## write function importing data 
import_data <- function(file_path) {
  tryCatch({
    ext <- tools::file_ext(file_path)
    data <- switch(ext,
                   csv = read_csv_arrow(file_path) %>% clean_names(),
                   xlsx = read_xlsx(file_path) %>% clean_names() %>% remove_empty(which = c("cols", "rows")),
                   rds = readRDS(file_path) %>% clean_names(),
                   dta = read_dta(file_path) %>% clean_names(),
                   parquet = read_parquet(file_path) %>% clean_names(),
                   stop("Unsupported file type")
    )
    # Assign data frame to the global environment, stripping extension and replacing non-alphanumeric characters
    data_name <- gsub("[^[:alnum:]_]", "", tools::file_path_sans_ext(basename(file_path)))
    assign(data_name, data, envir = .GlobalEnv)
    return(data)
  }, error = function(e) {
    message("Failed to load ", file_path, ": ", e$message)
    NULL  # Return NULL on error
  })
}


## specify folder of interest and apply function to all files in folder
data_folder <- list.files(file.path(here(), "/03 Cleaned data/OECD CAPMF/"), full.names = TRUE)
map(list.files(data_folder, full.names = T), ~import_data(.x))


################################
# Descriptive results 
################################

## Summary statistics of key dependent and independent variables 
oecd_merged %>%
  mutate(eu = ifelse(eu == 1, "EU", "Non-EU")) %>%
  select(happy_with_env_preserv, 
         obs_value, 
         obs_value1,
         corp_all, 
         corp_core,
         polconiii, 
         polconiii_vdem, 
         polconv, 
         polconv_vdem,
         grep("gdp|trade|fossil|co2|coal|eco_cip", names(.)),
         starts_with("corp"), 
         openc, 
         realgdpgr, 
         netu_ipol, 
         ag_lnd_totl_k2, 
         gc_tax_totl_gd_zs, 
         population_x, 
         concert, 
         environ_worry_interpolated,
         eu) %>%
  datasummary_skim(histogram = F, fmt = 3, by = "eu") 


## Check that ranges of variables make sense; where they do not, adjust merge_data_clean.R script accordingly













