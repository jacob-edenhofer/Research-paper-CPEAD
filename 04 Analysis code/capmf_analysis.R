
################################
# Preliminaries
################################

# load packages 
library(tidyverse)
library(data.table)
library(countrycode)
library(tidylog)
library(readxl)
library(openxlsx)
library(here)
library(janitor)
library(arrow)
library(furrr)
library(WDI)
library(modelsummary)
library(OECD)
library(eurostat)
library(fixest)
library(fwlplot)
library(marginaleffects)
library(kableExtra)
library(haven)
library(scales)
library(patchwork)


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

















