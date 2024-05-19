
################################
# Preliminaries
################################

# load packages
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
import_data <- function(file_path) {
  # Calculate the intended data name based on file name, stripping extension and special characters
  data_name <- gsub("[^[:alnum:]_]", "", tools::file_path_sans_ext(basename(file_path)))
  
  # Check if the dataset already exists in the global environment
  if (exists(data_name, envir = .GlobalEnv)) {
    message("Dataset '", data_name, "' already loaded.")
    return(get(data_name, envir = .GlobalEnv))
  }
  
  # Proceed with loading if the dataset does not exist
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
    
    # Assign data frame to the global environment
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
  "obs_value" = "Stringency value",
  "obs_value1" = "Stringency value modified",
  "obs_value1_lag1" = "Stringency value modified, one-year lag",
  "obs_value1_lag2" = "Stringency value modified, two-year lag",
  "obs_value1_lag3" = "Stringency value modified, three-year lag",
  "obs_value1_lag4" = "Stringency value modified, four-year lag",
  "democracy" = "BMR democracy dummy",
  "polityfrompolity" = "Polity score",
  "polconiii" = "Political constraints index III",
  "polconiii_vdem" = "Political constraints index III (VDEM)",
  "polconv" = "Political constraints index V",
  "polconv_vdem" = "Political constraints index V (VDEM)",
  "corp_all" = "Corporatism (all)",
  "corp_allsm" = "Corporatism (all), sm",
  "corp_core" = "Corporatism (core)",
  "corp_cor_esm" = "Corporatism (core), sm",
  "corpo_f_cor_esm" = "Corporatism (core), sm f",
  "concert" = "Concertation indicator",
  "bc" = "Bipartite concertation",
  "tc" = "Tripartite concertation",
  "ud_ipol" = "Union density (interpolated)",
  "ud_s" = "Union density (survey data)",
  "ud_hist" = "Union density (historical data)",
  "ud_female" = "Union density (female)", 
  "ud_s_female" = "Union density, female (survey data)",
  "ud_male" = "Union density (Male)", 
  "ud_s_male" = "Union density, male (survey data)",
  "ud_private" = "Union density (private sector)",
  "ud_s_private" = "Union density, private sector (survey data)",
  "ud_public" = "Union density (public sector)",
  "ud_s_public" = "Union density, public sector (survey data)",
  "ud_s_parttime" = "Union density (part-time workers)",
  "ud_s_fulltime" = "Union density (full-time workers)",
  "ud_s_temp" = "Union density (temporary workers)",
  "ud_s_perm" = "Union density (permanent workers)",
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
  "nec_fs" = "Number of employer confederations",
  "nuc_fs" = "Number of union confederations",
  "netu_ipol" = "Net union density (interpolated)",
  "ag_lnd_totl_k2" = "Land area (sq. km)",
  "bg_gsr_nfsv_gd_zs" = "Trade in services (% of GDP)",
  "en_atm_co2e_pc" = "CO2 emissions per capita (metric tonnes)",
  "gc_tax_intt_rv_zs" = "Tax on int'l trade (% of revenue)",
  "gc_tax_totl_gd_zs" = "Tax revenue (% of GDP)",
  "ne_trd_gnfs_zs" = "Trade (% of GDP)",
  "nv_ind_manf_zs" = "Manufacturing value added (% of GDP)",
  "nv_ind_totl_kd" = "Industry value added (constant 2015 US$)",
  "nv_ind_totl_zs" = "Industry value added (% of GDP)",
  "ny_gdp_coal_rt_zs" = "Coal rents (% of GDP)",
  "ny_gdp_frst_rt_zs" = "Forest rents (% of GDP)",
  "ny_gdp_minr_rt_zs" = "Mineral rents (% of GDP)",
  "ny_gdp_ngas_rt_zs" = "Natural gas rents (% of GDP)",
  "ny_gdp_petr_rt_zs" = "Oil rents (% of GDP)",
  "ny_gdp_pcap_cd" = "GDP per capita (current US$)",
  "ny_gdp_pcap_kd" = "GDP per capita (constant 2015 US$)",
  "ny_gdp_pcap_kd_zg" = "GDP per capita growth (% annual)",
  "ny_ttf_gnfs_kn" = "Terms of trade adjustment (constant LCU)",
  "sp_pop_totl" = "Population total",
  "population_x" = "Population",
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
  "elec_diff_abs" = "Absolute avg poll error",
  "elec_diff_abs_top2" = "Absolute avg poll error top 2 parties",
  "elec_diff_abs_top3" = "Absolute avg poll error top 3 parties",
  "elec_diff_abs_last2" = "Absolute avg poll error last 2 polls",
  "elec_diff_abs_last3" = "Absolute avg poll error last 3 polls",
  "elec_diff_abs_last10d" = "Absolute avg poll error last 10 days",
  "elec_diff_abs_last15d" = "Absolute avg poll error last 15 days",
  "elec_diff_populist" = "Absolute avg populist shock",
  "elec_diff_state" = "Absolute avg pro-state shock",
  "elec_diff_market" = "Absolute avg pro-market shock",
  "elec_diff_lib" = "Absolute avg pro-liberal shock",
  "elec_diff_auth" = "Absolute avg pro-authoritarian shock",
  "elec_diff_aeu" = "Absolute avg anti-EU shock",
  "elec_diff_proeu" = "Absolute avg pro-EU shock",
  "elec_diff_incumbent" = "Mean elec incumbent",
  "elec_diff_winner" = "Mean elec winner",
  "incumbent" = "Incumbent party stays on (dummy)",
  "reshuffle_shock" = "Reshuffle shock",
  "lpr" = "Loss probability",
  "lprsq" = "Loss probability squared",
  "openc" = "Openness to trade",
  "nomgdpgr" = "Nominal GDP growth",
  "realgdpgr" = "Real GDP growth",
  "eu" = "EU membership",
  "elderly" = "Share of population >65",
  "rae_ele" = "Rae electoral fractionalisation",
  "rae_leg" = "Rae legislative fractionalisation",
  "dis_abso" = "Index of absolute disproportionality",
  "dis_rel" = "Index of relative disproportionality",
  "dis_gall" = "Gallagher index of disproportionality",
  "training_pmp" = "Labour market training as % of GDP",
  "happy_with_env_preserv" = "Happiness with environmental preservation",
  "environ_worry_interpolated" = "Environmental worry (interpolated)",
  "half_decade" = "Half decade",
  "clim_act_pol" = "Type of climate action/measure",
  "iso3c" = "Country",
  "time_period" = "Year"
)


################################
# Descriptive analysis 
################################

## write descriptive stats function 
descrip_stats <- function(dataset, dataset_name, var_dict){
  data_cleaned <- dataset %>%
    mutate(eu = ifelse(eu == 1, "EU", "Non-EU")) %>%
    select(all_of(names(var_dict))) %>%
    rename_with(~ var_dict[.], .cols = everything())
  
  # Attempt to handle tinytable conversion here if needed:
  main_table <- data_cleaned %>%
    datasummary(All(.) ~ (Mean + SD + Min + P25 + P75 + Max + N + PercentMissing),
                data = ., output = "data.frame")  # Ensure output as data frame
  
  # Save main table using kable and save_kable
  kable_path <- paste0(here(), "/06 Figures and tables/Tables/Summary statistics/main_table_", dataset_name, ".tex")
  save_kable(kable(main_table, format = "latex", longtable = TRUE, booktabs = TRUE), file = kable_path)
  
  # create list for splitting tables by dichotomous variables 
  additional_tables <- list()
  
  # Generate specific summaries for binary factors
  for(i in names(data_cleaned)){
    no_levels <- nlevels(factor(data_cleaned[[i]]))
    if(no_levels == 2){
      formula_string <- sprintf("All(data_cleaned) ~ Factor(`%s`)*(Mean + SD + Min + Max + N + PercentMissing + NUnique)", i)
      formula <- as.formula(formula_string)
      table <- datasummary(formula, data = data_cleaned, output = "data.frame")  # Ensure output as data frame
      additional_tables[[i]] <- table
      
      # Saving each table
      kable_path <- paste0(here(), "/06 Figures and tables/Tables/Summary statistics/table_", i, "_", dataset_name, ".tex")
      save_kable(kable(table, format = "latex", longtable = TRUE, booktabs = TRUE), file = kable_path)
    }
  }
  
  return(list(main_table = main_table, additional_tables = additional_tables))
}

# Filter only data frames for processing
list_of_datasets <- mget(ls(), envir = globalenv())
list_of_datasets <- list_of_datasets[sapply(list_of_datasets, is.data.frame)]

# Apply the function safely using purrr::safely to capture and handle errors
safe_descrip_stats <- purrr::safely(function(dataset, name) descrip_stats(dataset, name, var_dict))

# Prepare the results using map2 to pass both dataset and dataset names
results <- purrr::map2(list_of_datasets, names(list_of_datasets), ~safe_descrip_stats(.x, .y))



################################################
# Regressions (takes a rather long time to run)
################################################

## modify var_dict to include the interaction terms between the variables listed below 
corporatism <- c("corp_all", "corp_allsm", "corp_core", "bc", "tc", "nec_fs", "nuc_fs")
variables <- c("openc", "ne_trd_gnfs_zs", "eco_cip_net", "eco_cip_gross", "happy_with_env_preserv", 
               "dis_gall", "elec_diff_populist", "elec_diff_market", 
               "elec_diff_incumbent", "elec_diff_lib", "nv_ind_manf_zs", 
               "nv_ind_totl_zs", "trade_co2_share")

### loop to combine these
for (k in corporatism){
  for (h in variables){
    var_dict[paste0(k, "*", h)] <- paste0(var_dict[k], " x ", var_dict[h])
  }
}


# list to store models 
models_list <- list()

# define function to run regression 
reg_capmf <- function(data_type, dataset, var_dict) {
  if (grepl("adoption", data_type)) {
    dataset <- dataset %>% mutate(obs_value1 = ifelse(obs_value1 > 0, 1, 0))
  }
  for (j in corporatism) {
    for (i in variables) {
      # vector of lagged variables 
      lagged_cols <- c(j, i, "co2_per_capita", "ny_gdp_pcap_kd", "polconiii_vdem", "gc_tax_totl_gd_zs", "sp_pop_totl", "elderly")
      # create lagged values 
      dataset <- dataset %>%
        mutate(across(
          .cols = c(j, i, "co2_per_capita", "ny_gdp_pcap_kd", "polconiii_vdem", "gc_tax_totl_gd_zs", "sp_pop_totl", "elderly"),
          .fns = ~ lag(.x, 1, order_by = time_period),
          .names = "{.col}_lag1"  # Names the new columns with a _lag1 suffix
        )) %>%
        ungroup()
      
      # update var_dict 
      # Update var_dict with new lagged variables
      for (col in lagged_cols) {
        new_var_name <- paste0(col, "_lag1")
        var_dict[new_var_name] <- paste(var_dict[col], "lagged by 1 year")
      }
      
      # create interaction term
      interaction_term <- paste(j, i, sep = ":")
      formula <- as.formula(paste0("obs_value1 ~ ", interaction_term, " + csw0(co2_per_capita_lag1, ny_gdp_pcap_kd_lag1, polconiii_vdem_lag1, 
                                   gc_tax_totl_gd_zs_lag1, sp_pop_totl_lag1, elderly_lag1, obs_value1_lag4) | iso3c + half_decade + clim_act_pol"))
      model_name <- paste(data_type, j, i, sep = "_")
      model <- feols(formula, data = dataset)
      models_list[[model_name]] <- model
      
      # Extract summary and check p-value of the interaction term
      coef_table <- coeftable(models_list[[model_name]]) %>% clean_names()
      interaction_p_value <- coef_table[coef_table$coefficient == interaction_term, "pr_t"]
      
      # potentially, replace with all() condition 
      if (any(interaction_p_value < 0.1, na.rm = TRUE)) {
        print(paste("Significant interaction found in model:", model_name))
        table_path <- file.path(here("06 Figures and tables", "Tables", "Regressions", "Simpler"), paste(model_name, "tex", sep = "."))
        etable(model, 
               tex = TRUE, 
               replace = TRUE, 
               dict = var_dict,
               file = table_path)
      }
    }
  }
  return(models_list)
}

## define safe reg function 
safe_reg_capmf <- purrr::safely(reg_capmf)
## list of datasets to apply function to 
list_reg <- list_of_datasets[!grepl("merged", names(list_of_datasets))]
## apply function to datasets
safe_reg_capmf("adoptionLEV1", list_reg[[1]], var_dict)
safe_reg_capmf("adoptionLEV2", list_reg[[1]], var_dict)
safe_reg_capmf("stringencyLEV1", list_reg[[3]], var_dict)
safe_reg_capmf("stringencyLEV2", list_reg[[4]], var_dict)
safe_reg_capmf("stringencyLEV3", list_reg[[5]], var_dict)
safe_reg_capmf("stringencyLEV4", list_reg[[6]], var_dict)


