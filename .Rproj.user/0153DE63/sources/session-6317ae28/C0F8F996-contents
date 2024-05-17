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
library(haven)     # For reading Stata files

# Set up path
data_path <- here("03 Cleaned data", "Finnegan extended", "finnegan_merged.rds")
finnegan_merged <- readRDS(data_path)
finnegan_merged <- finnegan_merged %>% zap_labels()


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
corporatism <- c("ri", "corp_all", "corp_allsm", "corp_core", "corp_cor_esm", "corpo_f_cor_esm", "bc", "tc", "ud", "nec_fs", "nuc_fs", "adj_cov_hist")

# Factorise some of the corporatism variables
finnegan_merged <- finnegan_merged %>%
  mutate(bc = factor(bc), 
         tc = factor(tc))

# Loop to run models and generate plots
models_list <- list()
plots_list <- list()

for (var1 in corporatism) {
  for (dv in dvs_v) {
    for (var2 in variables) {
      formula <- as.formula(paste0(dv, "~", var1, "*", var2, " + fossfuel_prodpercap + realgdpgr.x + unemp + ja20f_v2 | countryid + year"))
      model_name <- paste(dv, var1, var2, sep = "__")
      models_list[[model_name]] <- run_model(formula, data = finnegan_merged, model_name)
      plots_list[[model_name]] <- plot_slopes(models_list[[model_name]], rug = T,
                                             variables = var1,
                                             by = var2) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(title = paste("Marginal Effect of", var1, "on", dv, "by", var2),
             x = var2) +
        theme_bw()
      print(plots_list[[model_name]])
      ggsave(paste0(here(), "/06 Figures and tables/Figures/ME exploratory/", model_name, ".png"), 
             width = 9, height = 6, unit = "in", dpi = 300)
    }
  }
}

# Output models and plots
lapply(models_list, summary)
lapply(plots_list, print)





