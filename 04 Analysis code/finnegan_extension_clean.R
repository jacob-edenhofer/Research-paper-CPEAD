################################
# Preliminaries
################################

# Load packages
library(tidyverse)  # includes dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)     # For reading Excel files
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

## variable dictionary 
var_dict_finnegan <- c("lambda_mean_wghtd" = "Overall stringency",
                       "lambda_mean_wghtd_con" = "Costs for consumers",
                       "lambda_mean_wghtd_prod" = "Costs for producers",
                       "lambda_mean_wghtd_comp" = "Consumer vs. producer costs",
                       "openc" = "Openness to trade",
                       "ind_valueadd" = "Industry value added",
                       "fossfuel_prodpercap" = "Fossil fuel production per capita",
                       "realgdpgr.x" = "Real GDP growth",
                       "realgdpgr" = "Real GDP growth",
                       "unemp" = "Unemployment rate",
                       "elect_comp" = "Electoral competitiveness",
                       "pr_ingov_mean_annual" = "Green party CIP",
                       "happy_with_env_preserv" = "Happiness with environmental preservation",
                       "unhappy_with_env_preserv" = "Unhappiness with environmental preservation",
                       "ja10f" = "Left-right dimension (Jahn)",
                       "ri" = "Concertation",
                       "trade_co2_share" = "Trade CO2 share",
                       "dis_gall.y" = "Gallgher's disproportionality index",
                       "elderly" = "Share of >65",
                       "corp_all" = "Corporatism (all)",
                       "corp_allsm" = "Corporatism (all, smoothed)",
                       "corp_core" = "Corporatism (core)",
                       "corp_cor_esm" = "Corporatism (core, smoothed)",
                       "corpo_f_cor_esm" = "Corporatism (ESM)",
                       "bc" = "Bipartite concertation",
                       "tc" = "Tripartite concertation",
                       "ud" = "Union density",
                       "nec_fs" = "number of employer confederations",
                       "nuc_fs" = "number of union confederations",
                       "ja20f_v2" = "Green vs. growth government preferences",
                       "carbon_inten1" = "Carbon intensity of energy supply",
                       "adj_cov_hist" = "Adjusted coverage (historical)",
                       "countryid" = "Country", 
                       "year" = "Year")


# fixest table setup 
setFixest_etable(postprocess.tex = NULL,
                 fitstat = ~ n + r2,
                 style.tex = style.tex("base",
                                       line.top = "simple",
                                       line.bottom = "simple",
                                       fixef.title = "\\emph{Fixed effects}",
                                       fixef.suffix = "",
                                       stats.title = "\\midrule \\emph{Fit statistics}",
                                       yesNo = "x"), 
                 depvar = T, 
                 family = T,
                 digits = "r3",
                 digits.stats = 3)

## fixest setup 
setFixest_estimation(
  data = NULL,
  panel.id = NULL,
  fixef.rm = "perfect",
  fixef.tol = 1e-09,
  fixef.iter = 10000,
  collin.tol = 1e-12,
  lean = TRUE,
  verbose = 0,
  warn = TRUE,
  combine.quick = NULL,
  demeaned = FALSE,
  mem.clean = TRUE,
  glm.iter = 25,
  glm.tol = 1e-09,
  reset = FALSE)


################################
# Helper function
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


##########################################
# Analysis: Partial replication 
#########################################

# Define variable names and models
variables <- c("openc", "ind_valueadd", "elect_comp", "carbon_inten1", "trade_co2_share", "pr_ingov_mean_annual", "ja10f")
dvs_v <- c("lambda_mean_wghtd", "lambda_mean_wghtd_con", "lambda_mean_wghtd_prod", "lambda_mean_wghtd_comp")
corporatism <- c("ri", "corp_all", "corp_allsm", "corp_core", "corp_cor_esm", "bc", "tc")


# models table 1

## create list of models for table 1
models_tab1 <- list()
map(dvs_v, ~{
  formula <- as.formula(paste0(.x, " ~ ri + csw0(realgdpgr.x, fossfuel_prodpercap, ja20f_v2) | countryid + year"))
  ## save model in list 
  models_tab1[[.x]] <- feols(fml = formula, data = finnegan_merged, vcov = ~countryid)
  models_tab1[[paste0("robust_se_", .x)]] <- feols(fml = formula, data = finnegan_merged, vcov = "hetero")
  
  ## save model using etable()
  etable(models_tab1[[.x]], 
         dict = var_dict_finnegan,
         tex = T,
         replace = T,
         title = paste0("Model for ", var_dict_finnegan[.x]),
         file = here("06 Figures and tables", "Tables", "Finnegan", paste0("main_effect_", .x, ".tex"))
  )
  
  ## save model using robust standard errors
  etable(models_tab1[[paste0("robust_se_", .x)]], 
         dict = var_dict_finnegan,
         tex = T,
         replace = T,
         title = paste0("Model for ", var_dict_finnegan[.x], " (robust SE)"),
         file = here("06 Figures and tables", "Tables", "Finnegan", paste0("main_effect_", .x, "_robust_se.tex"))
  )
})


##########################################
# Analysis: Extending Finnegan's Analysis
##########################################


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
      formula <- as.formula(paste0(dv, "~", var1, "*", var2, " + fossfuel_prodpercap + realgdpgr.x + unemp + elderly + ja20f_v2 | countryid + year"))
      model_name <- paste(dv, var1, var2, sep = "__")
      models_list[[model_name]] <- feols(fml = formula, data = finnegan_merged, lean = F)
      
      ## save regression table
      table_path <- here("06 Figures and tables", "Tables", "Finnegan", paste0(model_name, ".tex"))
      # formula_table <- as.formula(paste0(dv, "~", var1, "*", var2, " + csw0(fossfuel_prodpercap, realgdpgr.x + unemp, ja20f_v2) | countryid + year"))
      formula_table <- as.formula(paste0(dv, "~", var1, " + csw0( ", var2, ",", var1, ":", var2, ", fossfuel_prodpercap, realgdpgr.x, unemp, elderly, ja20f_v2) | countryid + year"))
      model_table <- feols(fml = formula_table, data = finnegan_merged)
      etable(model_table, 
             tex = T, 
             dict = var_dict_finnegan,
             replace = T, 
             title = paste("Examining", var_dict_finnegan[dv]),
             file = table_path)
      
      ## me plot
      plots_list[[model_name]] <- plot_slopes(models_list[[model_name]], rug = T,
                                             variables = var1,
                                             by = var2) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(title = paste("Marginal effect of", var_dict_finnegan[var1], "\non", var_dict_finnegan[dv], "by", var_dict_finnegan[var2]),
             x = var_dict_finnegan[var2]) +
        theme_bw()
      print(plots_list[[model_name]])
      ggsave(paste0(here(), "/06 Figures and tables/Figures/ME Finnegan/", model_name, ".png"), 
             width = 9, height = 6, unit = "in", dpi = 300)
    }
  }
}

# Output models and plots
lapply(models_list, summary)
lapply(plots_list, print)





