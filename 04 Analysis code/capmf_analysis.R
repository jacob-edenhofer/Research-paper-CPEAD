
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
  glm.tol = 1e-09,
  reset = FALSE)


## variable dictionary, with var name the key and the value the description
var_dict <- c(
  "obs_value" = "Stringency value",
  "obs_value1" = "Stringency value modified",
  "obs_value1_lag1" = "Stringency value modified, one-year lag",
  "obs_value1_lag2" = "Stringency value modified, two-year lag",
  "obs_value1_lag3" = "Stringency value modified, three-year lag",
  "obs_value1_lag4" = "Stringency value modified, four-year lag",
  "sector" = "Sector",
  "instrument_type" = "Instrument type",
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
  "tc_bc_dummy" = "Concertation dummy (TC or BC)",
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
  "en_atm_co2e_pc" = "CO2 emissions p.c. (metric tonnes)",
  "gc_tax_intt_rv_zs" = "Tax on int'l trade (% of revenue)",
  "gc_tax_totl_gd_zs" = "Tax revenue (% of GDP)",
  "ne_trd_gnfs_zs" = "Trade (% of GDP)",
  "nv_ind_manf_zs" = "Manufacturing value added (% of GDP)",
  "nv_ind_totl_kd" = "Industry value added (constant 2015 USD)",
  "nv_ind_totl_zs" = "Industry value added (% of GDP)",
  "ny_gdp_coal_rt_zs" = "Coal rents (% of GDP)",
  "ny_gdp_frst_rt_zs" = "Forest rents (% of GDP)",
  "ny_gdp_minr_rt_zs" = "Mineral rents (% of GDP)",
  "ny_gdp_ngas_rt_zs" = "Natural gas rents (% of GDP)",
  "ny_gdp_petr_rt_zs" = "Oil rents (% of GDP)",
  "ny_gdp_pcap_cd" = "GDP p.c. (current USD)",
  "ny_gdp_pcap_kd" = "GDP p.c. (constant 2015 USD)",
  "ny_gdp_pcap_kd_zg" = "GDP p.c. growth (% annual)",
  "ny_ttf_gnfs_kn" = "Terms of trade adjustment (constant LCU)",
  "sp_pop_totl" = "Population total",
  "population_x" = "Population",
  "fossil_share_elec" = "Fossil share electricity",
  "fossil_elec_per_capita" = "Fossil electricity p.c.",
  "fossil_share_energy" = "Fossil share energy",
  "fossil_energy_per_capita" = "Fossil share energy p.c.",
  "co2_per_gdp" = "CO2 emissions per GDP",
  "co2_including_luc_per_gdp" = "CO2 emissions including land use change per GDP",
  "consumption_co2_per_gdp" = "Consumption CO2 per GDP",
  "cement_co2_per_capita" = "Cement CO2 p.c.",
  "co2_including_luc_per_capita" = "CO2 emissions including land use change p.c.",
  "co2_per_capita" = "CO2 emissions p.c.",
  "coal_co2_per_capita" = "Coal CO2 p.c.",
  "consumption_co2_per_capita" = "Consumption CO2 p.c.",
  "flaring_co2_per_capita" = "Flaring CO2 p.c.",
  "gas_co2_per_capita" = "Gas CO2 p.c.",
  "land_use_change_co2_per_capita" = "Land use change CO2 p.c.",
  "oil_co2_per_capita" = "Oil CO2 p.c.",
  "other_co2_per_capita" = "Other CO2 p.c.",
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
  "coal_cons_per_capita" = "Coal consumption p.c.",
  "coal_elec_per_capita" = "Coal electricity p.c.",
  "coal_prod_per_capita" = "Coal production p.c.",
  "coal_share_energy" = "Coal share energy",
  "eco_cip_gross" = "Green party CIP (Gross)",
  "eco_cip_net" = "Green party CIP (Net)",
  "pr_ingov_mean_annual" = "Green party CIP",
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
  "openc" = "Openness of economy",
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
  "unhappy_with_env_preserv" = "Unhappy with environmental preservation",
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
# Regressions -- Main analysis
################################################


## modify var_dict to include the interaction terms between the variables listed below 
corporatism <- c("corp_all", "corp_allsm", "bc", "tc")
## add  "nec_fs", "nuc_fs" if needed
variables <- c("openc", "ne_trd_gnfs_zs", "eco_cip_net", "eco_cip_gross", "pr_ingov_mean_annual", "unhappy_with_env_preserv",
               "happy_with_env_preserv", "elec_diff_incumbent", "nv_ind_manf_zs", "nv_ind_totl_zs", "trade_co2_share")

## modify var_dict to include interaction terms 
for (k in corporatism){
  for (h in variables){
    var_dict[paste0(k, "*", h)] <- paste0(var_dict[k], " x ", var_dict[h])
  }
}

## define vector of covariates to include in the baseline specifications
covariates <- c("co2_per_capita", "ny_gdp_pcap_kd", "gc_tax_totl_gd_zs", 
                "elderly", "dis_gall", "obs_value1_lag3")


########################## Main effects ####################################

reg_main_effects <- function(data_type, dataset, var_dict) {
  if (grepl("adoption", data_type)) {
    dataset <- dataset %>% mutate(obs_value1 = ifelse(obs_value1 > 0, 1, 0))
  }
  
  lagged_cols <- character(0)
  for (i in corporatism) {
    lagged_cols <- c(lagged_cols, i, covariates)
  }
  
  # create lagged values 
  dataset <- dataset %>%
    # group by country and climate policy area and use order_by to incorporate time dimensions
    group_by(iso3c, clim_act_pol) %>%
    mutate(across(
      .cols = c(lagged_cols),
      .fns = ~ dplyr::lag(.x, 1, order_by = time_period),
      .names = "{.col}_lag1"
    )) %>%
    ungroup()
  
  # Update var_dict with new lagged variables
  for (col in lagged_cols) {
    new_var_name <- paste0(col, "_lag1")
    var_dict[[new_var_name]] <- paste0(var_dict[[col]], ", 1-year lag")
  }
  
  # create list to store models 
  models_main_list <- list()
  
  for (i in corporatism) {
    # define formula 
    if (grepl("adoption", data_type)) {
      if (grepl("LEV2", data_type)) {
        formula <- as.formula(paste0("obs_value1 ~ ", i, "_lag1 + csw0(", paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + instrument_type"))
        model <- feglm(fml = formula, family = "logit", vcov = ~iso3c, data = dataset)
      } else {
        formula <- as.formula(paste0("obs_value1 ~ ", i, "_lag1 + csw0(", paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + clim_act_pol"))
        model <- feglm(fml = formula, family = "logit", vcov = ~iso3c, data = dataset)
      }
    } else {
      if (grepl("LEV2", data_type)) {
        formula <- as.formula(paste0("obs_value1 ~ ", i, "_lag1 + csw0(", paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + instrument_type"))
        model <- feols(formula, vcov = ~iso3c, data = dataset)
      } else {
        formula <- as.formula(paste0("obs_value1 ~ ", i, "_lag1 + csw0(", paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + clim_act_pol"))
        model <- feols(formula, vcov = ~iso3c, data = dataset)
      }
    }
    
    # store model 
    models_main_list[[i]] <- model
    
    # save as latex table 
    main_table_path <- here::here("06 Figures and tables/Tables/Regressions/Main effect", paste0(i, "_", data_type, ".tex"))
    etable(model, 
           dict = var_dict,
           tex = TRUE, 
           replace = TRUE, 
           title = paste0("Main effect of ", var_dict[[i]]),
           file = main_table_path)
    
    ## create and save coefficient plot
    main_plot_path <- here::here("06 Figures and tables/Figures/Coefficient plots/Main effect", paste0(i, "_", data_type, ".png"))
    names(model) <- as.character(seq(1, length(model), 1))
    p <- modelplot(model, 
                   coef_map = var_dict) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_colour_brewer("Model", palette = "Set2") +
      scale_y_discrete(labels = label_wrap(30)) +
      labs(title = paste0("Main effect of ", var_dict[[i]])) +
      theme_bw() +
      theme(legend.position = "bottom")
    ggsave(filename = main_plot_path, plot = p, width = 9, height = 6, units = "in", dpi = 300)
  }
  
  return(models_main_list)
}


## define safe version of this function 
safe_reg_main_effects <- safely(reg_main_effects)

## list of datasets to apply function to 
list_reg <- list_of_datasets[!grepl("merged", names(list_of_datasets))]

## run the function
safe_reg_main_effects("adoptionLEV1", list_of_datasets[[1]], var_dict)
safe_reg_main_effects("adoptionLEV2", list_of_datasets[[2]], var_dict)
safe_reg_main_effects("stringencyLEV1", list_of_datasets[[4]], var_dict)
safe_reg_main_effects("stringencyLEV2", list_of_datasets[[5]], var_dict)



######################## Interaction effects ############################

# define function to run regression 
reg_capmf <- function(data_type, dataset, var_dict) {
  # list to store models 
  models_list <- list()
  if (grepl("adoption", data_type)) {
    dataset <- dataset %>% mutate(obs_value1 = ifelse(obs_value1 > 0, 1, 0))
  }
  for (j in corporatism) {
    for (i in variables) {
      # vector of lagged variables 
      lagged_cols <- c(j, i, covariates)
      # create lagged values 
      dataset <- dataset %>%
        # group by country and climate policy area and use order_by to incorporate time dimensions
        group_by(iso3c, clim_act_pol) %>%
        mutate(across(
          .cols = c(lagged_cols),
          .fns = ~ dplyr::lag(.x, 1, order_by = time_period),
          .names = "{.col}_lag1"  # Names the new columns with a _lag1 suffix
        )) %>%
        ungroup()
      
      # Update var_dict with new lagged variables
      for (col in lagged_cols) {
        new_var_name <- paste0(col, "_lag1")
        var_dict[new_var_name] <- paste0(var_dict[col], ", 1-year lag")
      }
      
      # create interaction term
      interaction_term <- paste(paste0(j, "_lag1"), paste0(i, "_lag1"), sep = "*")
      interaction_term_dict <- paste(paste0(j, "_lag1"), paste0(i, "_lag1"), sep = ":")
      var_dict[interaction_term_dict] <- paste0(var_dict[paste0(j, "_lag1")], " x ", var_dict[paste0(i, "_lag1")])
      
      # if statement to apply different FEs for LEV2 datasets
      if(grepl("LEV2", data_type)){
        formula <- as.formula(paste0("obs_value1 ~ ", interaction_term, " + csw0(", paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + instrument_type"))
      } else {
        formula <- as.formula(paste0("obs_value1 ~ ", interaction_term, " + csw0(", paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + clim_act_pol"))
      }
      # print formula
      print(formula)
      
      # name and run model
      model_name <- paste(data_type, j, i, sep = "_")
      model <- feols(formula, vcov = ~iso3c, data = dataset)
      models_list[[model_name]] <- model
      
      # extract summary and check p-value of the interaction term 
      coef_table <- coeftable(models_list[[model_name]]) %>% clean_names()
      print(coef_table)
      interaction_term_mod <- paste(paste0(j, "_lag1"), paste0(i, "_lag1"), sep = ":")
      interaction_p_value <- coef_table[coef_table$coefficient == interaction_term_mod, "pr_t"]
      print(interaction_p_value)
      
      # adjust condition as you see fit (either use all() or decrease p-value threshold)
      if (any(interaction_p_value[3:5] <= 0.05)) {
        print(paste("Significant interaction found in model:", model_name))
        table_path_full <- file.path(here("06 Figures and tables", "Tables", "Regressions", "Baseline"), paste(model_name, "tex", sep = "."))
        table_path_pruned <- file.path(here("06 Figures and tables", "Tables", "Regressions", "Baseline pruned"), paste(model_name, "tex", sep = "."))
        
        ## rows to omit 
        rows_to_omit <- paste0("%", covariates, "_lag1")
        
        ## pruned tables 
        etable(model,
               drop = rows_to_omit,
               extralines = list("CO2 p.c." = c(" ", "X", "X", "X", "X", "X", "X"),
                                 "GDP p.c." = c(" ", " ", "X", "X", "X", "X", "X"),
                                 "Total tax revenues as % of GDP" = c(" ", " ", " ", "X", "X", "X", "X"),
                                 "Share of elderly population" = c(" ", " ", " ", " ", "X", "X", "X"),
                                 "Gallagher disproportionality index" = c(" ", " ", " ", " ", " ", "X", "X"),
                                 "Lagged dependent variable" = c(" ", " ", " ", " ", " ", " ", "X")),
               tex = TRUE,
               replace = TRUE,
               dict = var_dict,
               title = paste0("Marginal effect of ", var_dict[[j]], " by ", var_dict[[i]]),
               file = table_path_pruned)
        
        ## full table 
        etable(model,
               tex = TRUE,
               replace = TRUE,
               dict = var_dict,
               title = paste0("Marginal effect of ", var_dict[[j]], " by ", var_dict[[i]]),
               file = table_path_full)
        
        ## save coefficient plots 
        file_path_base_plot <- here("06 Figures and tables", "Figures", "Coefficient plots", "Baseline", model_name)
        names(models_list[[model_name]]) <- as.character(seq(1, length(models_list[[model_name]]), 1))
        modelsummary::modelplot(models_list[[model_name]], 
                                coef_map = var_dict) +
          geom_vline(xintercept = 0, linetype = "dashed") +
          scale_colour_brewer("Model",
                              palette = "Set2") +
          scale_y_discrete(labels = label_wrap(30)) +
          labs(title = paste("Baseline model for", var_dict[j], "and", var_dict[i])) +
          theme_bw() +
          theme(legend.position = "bottom", 
                plot.title = element_text(size = 11))
        ggsave(paste0(file_path_base_plot, ".png"), width = 9, height = 6, units = "in", dpi = 300)
      }
    }
  }
  return(models_list)
}

## define safe reg_capmf function 
safe_reg_capmf <- purrr::safely(reg_capmf)

## list to store models 
models_list <- list()

## apply function to datasets
models_list$adoptionLEV1 <- safe_reg_capmf("adoptionLEV1", list_reg[[1]], var_dict)
models_list$adoptionLEV2 <- safe_reg_capmf("adoptionLEV2", list_reg[[2]], var_dict)
models_list$stringencyLEV1 <- safe_reg_capmf("stringencyLEV1", list_reg[[3]], var_dict)
models_list$stringencyLEV2 <- safe_reg_capmf("stringencyLEV2", list_reg[[4]], var_dict)
### Only run, if interested -- for my purposes, these are too granular
# safe_reg_capmf("stringencyLEV3", list_reg[[5]], var_dict)
# safe_reg_capmf("stringencyLEV4", list_reg[[6]], var_dict)



## Key insight from regs with sector FEs (where variation comes from variation between instruments)
## present regressions without interaction terms in appendix



###################################
# Summary of exploratory analysis
###################################


## extract regressions with significant interaction terms from capmf_analysis_exploratory script
regs_sign_list <- list.files(here("06 Figures and tables", "Tables", "Regressions", "Baseline"))
modified_strings <- gsub("\\.tex", "", regs_sign_list)
modified_strings1 <- gsub("((stringencyLEV[1-4])|(adoptionLEV[1-4]))", "\\1-", modified_strings)


## create a dataframe containing the interaction terms from the exploratory regressions
exploratory_sum_df <- tibble(id = modified_strings1) %>%
  separate(id, into = c("dataset", "interaction_term"), sep = "-_") %>%
  group_by(interaction_term) %>%
  # defined over all datasets
  mutate(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>% 
  # split interaction terms into two using the corporatism and variables vector to match the names
  mutate(corporatism_var = str_extract(interaction_term, paste(corporatism, collapse = "|")),
         corporatism_var = ifelse(grepl("corp_allsm", interaction_term), "corp_allsm", corporatism_var),
         variable_var = str_extract(interaction_term, paste(variables, collapse = "|"))) 

## save data frame
write_csv_arrow(exploratory_sum_df, here("06 Figures and tables", "Tables", "exploratory_sum_df.csv"))
## import 
exploratory_sum_df <- read_csv_arrow(here("06 Figures and tables", "Tables", "exploratory_sum_df.csv"))

## Notes 

## Then, say that one potential objection might be that the corp_all measure does not really capture 
### concertation. For at at least one of the concertation variables, I can show that the patterns are robust for 
### adoption. Why adoption? Because the measure vary relatively little over time -- so they are unlikely to pick up movements in stringency.
### But this shows that the patterns are robust to concertation. 
### Note: Concertation variables are most predictive at adoption level 2 (sectoral variation).


################################################
# Interaction terms -- Plots 
################################################


map(1:nrow(exploratory_sum_df), function(i) {
  
  ## extract dataset
  dataset <- exploratory_sum_df$dataset[i]
  dataset <- paste0("oecd_", gsub("(LEV)", "_\\1", dataset))
  print(dataset)
  
  ## extract variable and corporatism measure
  variable <- exploratory_sum_df$variable_var[i]
  corporatism <- exploratory_sum_df$corporatism_var[i]
  
  ## extract data
  data <- list_reg[[which(names(list_reg) == dataset)]]
  
  ## save lagged cols
  lagged_cols <- c(corporatism, variable, covariates)
  
  ## create lagged variables
  data <- data %>%
    # group by country and climate policy area and use order_by to incorporate time dimensions
    group_by(iso3c, clim_act_pol) %>%
    mutate(across(
      .cols = c(lagged_cols),
      .fns = ~ dplyr::lag(.x, 1, order_by = time_period),
      .names = "{.col}_lag1"  # Names the new columns with a _lag1 suffix
    )) %>%
    ungroup()
  
  # Update var_dict with new lagged variables
  for (col in lagged_cols) {
    new_var_name <- paste0(col, "_lag1")
    var_dict[new_var_name] <- paste0(var_dict[col], ", 1-year lag")
  }
  
  ## update labels
  interaction_term <- paste(paste0(corporatism, "_lag1"), paste0(variable, "_lag1"), sep = "*")
  interaction_term_dict <- paste(paste0(corporatism, "_lag1"), paste0(variable, "_lag1"), sep = ":")
  var_dict[interaction_term_dict] <- paste0(var_dict[paste0(corporatism, "_lag1")], " x ", var_dict[paste0(variable, "_lag1")])
  
  ## formula 
  if(grepl("LEV2", dataset)){
    formula <- as.formula(paste0("obs_value1 ~ ", interaction_term, " + ", paste(paste0(covariates, "_lag1"), collapse = " + "), " | iso3c + half_decade + instrument_type"))
  } else {
    formula <- as.formula(paste0("obs_value1 ~ ", interaction_term, " + ", paste(paste0(covariates, "_lag1"), collapse = " + "), " | iso3c + half_decade + clim_act_pol"))
  }
  
  ## run model
  me_model <- feols(formula, data = data, lean = F, 
                    vcov = ~iso3c)
  
  ## split model by EU membership dummy and save as regression table 
  robust_region <- feols(formula, 
                         data = data, 
                         lean = F, 
                         fsplit = ~region23,
                         vcov = ~iso3c)
  
  ## path 
  robust_folder <- here("06 Figures and tables", "Tables", "Regressions", "Robustness")
  file_path_robust <- paste0(robust_folder, "/", dataset, "_", corporatism, "_", variable, ".tex")
  
  ## save as tex table
  etable(robust_region,
         dict = var_dict,
         tex = TRUE,
         replace = TRUE,
         title = paste("Marginal effect of", var_dict[corporatism], "by", var_dict[variable]),
         file = file_path_robust)

  ## plot
  plot_slopes(me_model,
              rug = T,
              variables = paste0(corporatism, "_lag1"),
              condition = paste0(variable, "_lag1")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Marginal effect of", var_dict[corporatism], "by", var_dict[variable]),
         caption = paste0("Note: The model includes country, half-decade, and instrument type fixed effects as well as controls for\n",
                          paste(var_dict[covariates], collapse = ", "), "."),
         x = var_dict[variable],
         y = paste0("Marginal effect of corporatism measure (", var_dict[corporatism], ")")) +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.caption = element_text(size = 7, hjust = 0),
          plot.title = element_text(size = 11))

  ## save plot
  ggsave(here("06 Figures and tables", "Figures", "Interaction plots",
              paste0("interaction_", dataset, "_", variable, "_", corporatism, ".png")),
         width = 9, height = 6, units = "in", dpi = 300)
})


################################################
# Regressions -- Robustness 
################################################


# Multiple imputation 



## run the same models without lagging the independent variables
### include different lags
### different set of controls as robustness check?


# different standard errors for baseline specification 



# jacknife procedure (drop countries and then policy areas); produce histogram of estimates 


jacknife_function <- function(dataset, data_type, var_dict, corporatism, variables, covariates) {
  # Check for necessary columns and types
  necessary_columns <- c("iso3c", "clim_act_pol", "time_period")
  if (!all(necessary_columns %in% names(dataset))) {
    stop("Dataset must contain the necessary columns: iso3c, clim_act_pol, and time_period.")
  }
  
  # Data modification based on 'data_type'
  if (grepl("adoption", data_type)) {
    dataset <- dataset %>%
      mutate(obs_value1 = ifelse(obs_value1 > 0, 1, 0))
  }
  
  # Create lagged variables for specified columns
  all_vars <- unique(c(corporatism, variables, covariates))
  dataset <- dataset %>%
    group_by(iso3c, clim_act_pol) %>%
    mutate(across(
      .cols = all_vars,
      .fns = ~dplyr::lag(.x, 1, order_by = time_period),
      .names = "{.col}_lag1"
    )) %>%
    ungroup()
  
  # Update var_dict with new lagged variables
  # Update var_dict with new lagged variables
  for (col in all_vars) {
    new_var_name <- paste0(col, "_lag1")
    var_dict[new_var_name] <- paste0(var_dict[col], ", 1-year lag")
  }
  
  # Prepare the list to store models
  models_list <- list()
  
  # Iterate through each pair of corporatism and variables
  for(j in corporatism) {
    for(i in variables) {
      # Create interaction term and update dictionary
      interaction_term <- paste(paste0(j, "_lag1"), paste0(i, "_lag1"), sep = "*")
      interaction_term_dict <- paste(paste0(j, "_lag1"), paste0(i, "_lag1"), sep = ":")
      var_dict[interaction_term_dict] <- paste0(var_dict[paste0(j, "_lag1")], " x ", var_dict[paste0(i, "_lag1")])
      
      # Formula construction
      if (grepl("LEV2", data_type)) {
        formula <- as.formula(paste("obs_value1 ~", interaction_term, "+", paste(paste0(covariates, "_lag1"), collapse = " + "), "| iso3c + half_decade + instrument_type"))
      } else {
        formula <- as.formula(paste("obs_value1 ~", interaction_term, "+", paste(paste0(covariates, "_lag1"), collapse = " + "), "| iso3c + half_decade + clim_act_pol"))
      }
      
      # Debug: Print the formula to ensure it is constructed correctly
      print(interaction_term_dict)
      print(formula)
      
      # # Perform jackknife estimation
      coef_jack <- list()
      # for (country in unique(dataset$iso3c)) {
      #   data_jacknife <- dataset %>% filter(iso3c != country)
      #   model <- feols(formula, data = data_jacknife, vcov = ~iso3c)
      #   ## extract coefficients 
      #   coefs <- coeftable(model) %>% clean_names() %>% as_tibble()
      #   coefs$coefficient <- dimnames(coeftable(model))[[1]]
      #   coefs$type <- "country"
      #   
      #   coef_jack[[country]] <- coefs %>%
      #     filter(coefficient == interaction_term_dict)
      # 
      #   # Debug: Print coefficients for each country to check results
      #   print(coef_jack[[country]])
      # }
      # 
      # jacknife estimatation for year
      for (year in unique(dataset$time_period)) {
        data_jacknife <- dataset %>% filter(time_period != year)
        model <- feols(formula, data = data_jacknife, vcov = ~iso3c)
        ## extract coefficients
        coefs <- coeftable(model) %>% clean_names() %>% as_tibble()
        coefs$coefficient <- dimnames(coeftable(model))[[1]]
        coefs$type <- "year"

        coef_jack[[year]] <- coefs %>%
          filter(coefficient == interaction_term_dict)

        # Debug: Print coefficients for each country to check results
        # print(coef_jack[[year]])
      }

      ## do this for half-decade
      # for (hc in unique(dataset$half_decade)) {
      #   data_jacknife <- dataset %>% filter(time_period != hc)
      #   model <- feols(formula, data = data_jacknife, vcov = ~iso3c)
      #   ## extract coefficients
      #   coefs <- coeftable(model) %>% clean_names() %>% as_tibble()
      #   coefs$coefficient <- dimnames(coeftable(model))[[1]]
      #   coefs$type <- "half_decade"
      #   
      #   coef_jack[[hc]] <- coefs %>%
      #     filter(coefficient == interaction_term_dict)
      #   
      #   # Debug: Print coefficients for each country to check results
      #   # print(coef_jack[[hc]])
      # }
      
      ## jacknife estimate for observations (takes too long)
      # for(row_no in 1:nrow(dataset)) {
      #   data_jacknife <- dataset[-row_no,]
      #   model <- feols(formula, data = data_jacknife, vcov = ~iso3c)
      #   ## extract coefficients
      #   coefs <- coeftable(model) %>% clean_names() %>% as_tibble()
      #   coefs$coefficient <- dimnames(coeftable(model))[[1]]
      #   coefs$type <- "observation"
      # 
      #   coef_jack[[row_no]] <- coefs %>%
      #     filter(coefficient == interaction_term_dict)
      # 
      #   # Debug: Print coefficients for each country to check results
      #   print(coef_jack[[row_no]])
      # }
      
      ## bind together the results
      coef_jack <- bind_rows(coef_jack)
      # print(coef_jack)
      
      ## plot histograms 
      print(coef_jack %>%
        ggplot(aes(x = estimate)) +
        geom_histogram(aes(y = after_stat(density)), 
                       alpha = 0.8,
                       colour = "white") + 
        geom_density(linewidth = 0.8) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red", 
                   linewidth = 1) +
        geom_vline(aes(xintercept = mean(estimate)), linetype = "dashed", 
                   color = "darkblue", linewidth = 1) +
        expand_limits(x = 0) +
        # facet_wrap(~type) +
        # labelling doesn't work (need to modify dictionary in loop above)
        labs(title = paste0("Jacknife estimates for ", var_dict[interaction_term_dict]),
             x = "Estimate",
             y = "Density") +
        theme_bw())
      ## save plots
      ggsave(paste0(here(), "/06 Figures and Tables/Figures/Jacknife/", data_type, var_dict[interaction_term_dict], ".png"),
             plot = last_plot(),
             width = 9, height = 6, dpi = 300)
    }
  }
  
  return(models_list)
}


jacknife_function(dataset = oecd_stringency_LEV1, 
                  data_type = "stringency_LEV1", 
                  var_dict = var_dict, 
                  corporatism = corporatism, 
                  variables = variables,
                  covariates = covariates)



##########################
# Done 
#########################

## openness measures: openc, trade_co2_share, ne_trd_gnfs_zs


# lagged dependent variables (different lags)


# different lags of independent variables 


# different covariates (with less theoretical motivation)


# logit vs lmp for adoption regressions 









