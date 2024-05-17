
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
  glm.tol = 1e-08,
  reset = FALSE)


## variable dictionary, with var name the key and the value the description
var_dict <- c(
  "happy_with_env_preserv" = "Happiness with environmental preservation",
  "obs_value" = "Stringency value",
  "obs_value1" = "Stringency value modified",
  "corp_all" = "Corporatism (all)",
  "corp_core" = "Corporatism (core)",
  "polconiii" = "Political constraints index III",
  "polconiii_vdem" = "Political constraints index III (VDEM)",
  "polconv" = "Political constraints index V",
  "polconv_vdem" = "Political constraints index V (VDEM)",
  "ud_ipol" = "Union density (interpolated)",
  "ud_s" = "Union density (survey data)",
  "ud_hist" = "Union density (historical data)",
  "ud_female" = "Union density (female)", 
  "ud_s_female" = "Union density, female (survey data)",
  "ud_male" = "Union density (Male)", 
  "ud_s_male" = "Union density, male (survey data)",
  "ud_private" = "Union density (private sector)",
  "ud_public" = "Union density (public sector)",
  "ud_s_parttime" = "Union density (part-time workers)",
  "ud_s_fulltime" = "Union density (full-time workers)",
  "ud_s_temp" = "Union density (temporary workers)",
  "ud_s_perm" = "Union density (permanent workers)",
  "ud_s_public" = "Union density, public sector (survey data)",
  "ud_s_private" = "Union density, private sector (survey data)",
  "new_cov" = "Bargaining coverage of newly concluded agreements",
  "unadj_cov" = "Unadjusted bargaining coverage",
  "unadj_cov_s" = "Unadjusted bargaining coverage (survey data)",
  "adj_cov" = "Adjusted bargaining coverage",
  "adj_cov_s" = "Adjusted bargaining coverage (survey data)",
  "adj_cov_hist" = "Adjusted bargaining coverage (historical data)",
  "cov_priv" = "Bargaining coverage of private sector",
  "cov_priv_s" = "Bargaining coverage of private sector (survey data)",
  "cov_pub" = "Bargaining coverage of public sector",
  "cov_pub_s" = "Bargaining coverage of public sector (survey data)",
  "fossil_share_elec" = "Fossil share electricity",
  "fossil_elec_per_capita" = "Fossil electricity per capita",
  "fossil_share_energy" = "Fossil share energy",
  "fossil_energy_per_capita" = "Fossil share energy per capita",
  "co2_per_gdp" = "CO2 emissions per GDP",
  "co2_including_luc_per_gdp" = "CO2 emissions including land use change per GDP",
  "consumption_co2_per_gdp" = "Consumption CO2 per GDP",
  "cement_co2_per_capita" = "Cement CO2 per capita",
  "co2_including_luc_per_capita" = "CO2 emissions including land use change per capita",
  "co2_per_capita" = "CO2 emissions per capita",
  "coal_co2_per_capita" = "Coal CO2 per capita",
  "consumption_co2_per_capita" = "Consumption CO2 per capita",
  "flaring_co2_per_capita" = "Flaring CO2 per capita",
  "gas_co2_per_capita" = "Gas CO2 per capita",
  "land_use_change_co2_per_capita" = "Land use change CO2 per capita",
  "oil_co2_per_capita" = "Oil CO2 per capita",
  "other_co2_per_capita" = "Other CO2 per capita",
  "share_global_cement_co2" = "Share of global cement CO2",
  "share_global_co2" = "Share of global CO2",
  "share_global_co2_including_luc" = "Share of global CO2 including land use change",
  "share_global_coal_co2" = "Share of global coal CO2",
  "share_global_cumulative_cement_co2" = "Share of global cumulative cement CO2",
  "share_global_cumulative_co2" = "Share of global cumulative CO2",
  "share_global_cumulative_co2_including_luc" = "Share of global cumulative CO2 including land use change",
  "share_global_cumulative_coal_co2" = "Share of global cumulative coal CO2",
  "share_global_cumulative_gas_co2" = "Share of global cumulative gas CO2",
  "share_global_cumulative_oil_co2" = "Share of global cumulative oil CO2",
  "share_global_gas_co2" = "Share of global gas CO2",
  "share_global_oil_co2" = "Share of global oil CO2",
  "share_global_other_co2" = "Share of global other CO2",
  "trade_co2_share" = "Trade CO2 share",
  "coal_share_elec" = "Coal share electricity",
  "coal_cons_per_capita" = "Coal consumption per capita",
  "coal_elec_per_capita" = "Coal electricity per capita",
  "coal_prod_per_capita" = "Coal production per capita",
  "coal_share_energy" = "Coal share energy",
  "fossil_energy_per_capita" = "Fossil energy per capita",
  "eco_cip_gross" = "Green party CIP (Gross)",
  "eco_cip_net" = "Green party CIP (Net)",
  "openc" = "Openness to trade",
  "nomgdpgr" = "Nominal GDP growth",
  "realgdpgr" = "Real GDP growth",
  "netu_ipol" = "Net union density (interpolated)",
  "ag_lnd_totl_k2" = "Agricultural land",
  "gc_tax_totl_gd_zs" = "Total tax revenue",
  "population_x" = "Population",
  "concert" = "Concertation indicator",
  "bc" = "Bipartite concertation",
  "tc" = "Tripartite concertation",
  "eu" = "EU membership",
  "environ_worry_interpolated" = "Environmental worry (interpolated)"
)



################################
# Descriptive results 
################################


## Summary statistics of key dependent and independent variables 
oecd_merged %>%
  mutate(eu = ifelse(eu == 1, "EU", "Non-EU")) %>%
  # select the variables on the left-hand side of the variable dictionary 
  select(all_of(names(var_dict))) %>%
  rename_with(~ var_dict[.], .cols = everything()) %>%
  datasummary(All(.) ~ Factor(`EU membership`)*(Mean + SD + Min + Max + N + PercentMissing + NUnique),
              data = .)
  # datasummary_skim(histogram = F, fmt = 3, by = "eu") 


## Check that ranges of variables make sense; where they do not, adjust merge_data_clean.R script accordingly











