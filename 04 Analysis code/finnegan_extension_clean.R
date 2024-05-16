################################
# Preliminaries
################################

# Load packages
library(tidyverse)  # includes dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)
library(arrow)      # Efficient data reading and writing
library(janitor)    # For clean_names()
library(here)       # For constructing paths
library(fixest)     # For econometric models
library(kableExtra) # For enhanced table output
library(patchwork)  # For combining plots
library(modelsummary) # For model output
library(marginaleffects) # For marginal effects plots

# Set up path
data_path <- here("03 Cleaned data", "Finnegan extended", "finnegan_merged.rds")
finnegan_merged <- readRDS(data_path)

################################
# Helper Functions
################################

# Function to run models and handle errors
run_model <- function(formula, data, model_name) {
  tryCatch({
    model <- feols(fml = formula, data = data)
  }, error = function(e) {
    message("Error in model for ", model_name, ": ", e$message)
    NULL
  })
}



################################
# Analysis: Extending Finnegan's Analysis
################################

# Define variable names and models
variables <- c("openc.x", "ind_valueadd", "elect_comp")
dvs_v <- c("lambda_mean_wghtd", "lambda_mean_wghtd_con", "lambda_mean_wghtd_prod", "lambda_mean_wghtd_comp")
corporatism <- c("ri", "corp_all", "corp_allsm", "corp_core", "corp_cor_esm", "corpo_f_cor_esm")

# Loop to run models and generate plots
models_list <- list()

for (var1 in corporatism) {
  for (dv in dvs_v) {
    for (var2 in variables) {
      formula <- as.formula(paste0(dv, "~", var1, "*", var2, " + fossfuel_prodpercap + realgdpgr.x + unemp + ja20f_v2 | countryid + year"))
      model_name <- paste(dv, var1, var2, sep = "__")
      models_list[[model_name]] <- run_model(formula, data = finnegan_merged, model_name)
    }
  }
}


# Output models and plots
lapply(models_list, summary)

# Generate me plots 
plots_list <- list()

for (model_name in names(models_list)) {
  parts <- str_split(model_name, "__", simplify = TRUE)
  dv <- parts[1]
  var1 <- parts[2]
  var2 <- parts[3]
  print(c(dv, var1, var2))  # This will print the variables being used for debugging
  
  if (!is.null(models_list[[model_name]])) {
    tryCatch({
      plot <- plot_slopes(models_list[[model_name]],
                          variables = var1,
                          by = var2) +  
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(title = paste("Marginal Effect of", var1, "on", dv, "by", var2),
             x = var2) +
        theme_minimal()
      plots_list[[model_name]] <- plot
    }, error = function(e) {
      message("Failed to generate plot for ", model_name, ": ", e$message)
      NULL
    })
  }
}




