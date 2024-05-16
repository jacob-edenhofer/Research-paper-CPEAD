
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
finnegan <- readRDS(paste0(here(), "/03 Cleaned data/Finnegan extended/finnegan_merged.rds"))


################################
# Extending Finnegan's analysis
################################


## Descriptive statistics 
dvs <- grep("^lambda", names(finnegan_merged))
climate <- grep("fossfuel|carbon", names(finnegan_merged))
macroeconomic <- grep("open|gdp|inflation|gini|ind|unemp", names(finnegan_merged))
welfare_inst <- grep("ud|netu|Corp|ri", names(finnegan_merged))
pol_inst <- grep("polconiii|elect", names(finnegan_merged))
indices <- c(dvs, climate, macroeconomic, welfare_inst)

### create table 
datasummary_skim(finnegan_merged[, indices],
                 histogram = F, output = "kableExtra")



# Let us do some preparation for the subsequent regressions
dvs_v <- c("lambda_mean_wghtd", "lambda_mean_wghtd_con", 
           "lambda_mean_wghtd_prod", "lambda_mean_wghtd_comp")

covariates_names <- c("ri" = "Concertation", 
                      "as.factor(eu.x)" = "EU", 
                      "inst_env2" = "Institutional constraints", 
                      "ja20f_v2" = "Green policy preferences", 
                      "realgdpr.x" = "Real GDP growth", 
                      "fossfuel_prodpercap" = "Fossil fuel Production", 
                      "(Intercept)" = "Constant")


## Concertation models
concertation_models <- list()
me_concertation <- list()
for(t in c("ind_valueadd", "openc", "elect_comp")){
  for(k in dvs_v){
    formula_finnegan0 <- as.formula(paste(k, "~ csw(", t, "*ri, fossfuel_prodpercap, realgdpgr.x, unemp, factor(eu.x), ja20f_v2) | countryid + year"))
    reg_name <- paste("reg", t, k, sep = "_")
    concertation_models[[reg_name]] <- feols(fml = formula_finnegan0,
                                             data = finnegan_merged)
    # print regression tables
    print(modelsummary(concertation_models[[reg_name]],
                       estimate = "{estimate}{stars}",
                       # coef_map = covariates_names,
                       title = paste("Examining", k),
                       output = "kableExtra"))
    # create marginal effects plots
    concertation_models[[paste("me", t, k, sep = "_")]] <- feols(fml = as.formula(paste(k, "~", t, "*ri + fossfuel_prodpercap + realgdpgr.x + unemp + factor(eu.x) + ja20f_v2 | countryid + year")),
                                                                 data = finnegan_merged)
    me_concertation[[t]][[k]] <- plot_slopes(concertation_models[[paste("me", t, k, sep = "_")]], rug = T,
                                             variables = t, by = "ri") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_y_continuous(paste("Marginal effect of", t),
                         breaks = seq(-0.1, 0.4, 0.1)) +
      #expand_limits(y = c(-0.1, 0.4)) +
      labs(x = "Concertation",
           title = paste0("Marginal effect of\n", t, " on ", k)) +
      theme_bw() +
      theme(plot.title = element_text(size = 9))
  }
  # print marginal effect plots for each dependent variable
  print(wrap_plots(me_concertation[[t]][[1]],
                   me_concertation[[t]][[2]],
                   me_concertation[[t]][[3]],
                   me_concertation[[t]][[4]],
                   nrow = 2))
}


## New corporatism measure I
corporatism_models <- list()
me_corporatism <- list()
for(t in c("openc", "ind_valueadd", "elect_comp")){
  for(k in dvs_v){
    formula_finnegan1 <- as.formula(paste(k, "~  csw(", t, "*CorpAll, fossfuel_prodpercap, realgdpgr.x, unemp, factor(eu.x), ja20f_v2, carbon_inten1) | countryid + year"))
    reg_name <- paste("reg", t, k, sep = "_")
    corporatism_models[[reg_name]] <- feols(fml = formula_finnegan1, 
                                            data = finnegan_merged)  
    
    # print regression tables 
    print(modelsummary(corporatism_models[[reg_name]],
                       estimate = "{estimate}{stars}",
                       # coef_map = covariates_names,
                       title = paste("Examining", k),
                       output = "kableExtra"))
    # create marginal effects plots
    corporatism_models[[paste("me", t, k, sep = "_")]] <- feols(fml = as.formula(paste(k, "~", t, "*CorpAll + fossfuel_prodpercap + realgdpgr.x + unemp + factor(eu.x) + ja20f_v2 + carbon_inten1 | countryid + year")),
                                                                data = finnegan_merged)
    me_corporatism[[t]][[k]] <- plot_slopes(corporatism_models[[paste("me", t, k, sep = "_")]], 
                                            rug = T,
                                            variables = t, by = "CorpAll") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_y_continuous(paste("Marginal effect of", t),
                         breaks = seq(-0.1, 0.4, 0.1)) +
      #expand_limits(y = c(-0.1, 0.4)) +
      labs(x = "Corporatism",
           title = paste0("Marginal effect of\n", t, " on ", k)) +
      theme_bw() +
      theme(plot.title = element_text(size = 9))
  }
  # print marginal effect plots for each dependent variable
  print(wrap_plots(me_corporatism[[t]][[1]],
                   me_corporatism[[t]][[2]],
                   me_corporatism[[t]][[3]],
                   me_corporatism[[t]][[4]],
                   nrow = 2))
}


## New corporatism measure II: new corporatism measure (use CorpCORE instead of CorpAll)
corporatism_models_core <- list()
me_corporatism_core <- list()
for(t in c("openc", "ind_valueadd", "elect_comp")){
  for(k in dvs_v){
    formula_finnegan1 <- as.formula(paste(k, "~  csw(", t, "*CorpCORE, fossfuel_prodpercap, realgdpgr.x, unemp, factor(eu.x), ja20f_v2, carbon_inten1) | countryid + year"))
    reg_name <- paste("reg", t, k, sep = "_")
    corporatism_models_core[[reg_name]] <- feols(fml = formula_finnegan1, 
                                                 data = finnegan_merged)  
    
    # print regression tables 
    print(modelsummary(corporatism_models_core[[reg_name]],
                       estimate = "{estimate}{stars}",
                       # coef_map = covariates_names,
                       title = paste("Examining", k),
                       output = "kableExtra"))
    # create marginal effects plots
    corporatism_models_core[[paste("me", t, k, sep = "_")]] <- feols(fml = as.formula(paste(k, "~", t, "*CorpCORE + fossfuel_prodpercap + realgdpgr.x + unemp + factor(eu.x) + ja20f_v2 + carbon_inten1 | countryid + year")),
                                                                     data = finnegan_merged)
    me_corporatism_core[[t]][[k]] <- plot_slopes(corporatism_models_core[[paste("me", t, k, sep = "_")]], 
                                                 rug = T,
                                                 variables = t, by = "CorpCORE") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_y_continuous(paste("Marginal effect of", t),
                         breaks = seq(-0.1, 0.4, 0.1)) +
      #expand_limits(y = c(-0.1, 0.4)) +
      labs(x = "Corporatism",
           title = paste0("Marginal effect of\n", t, " on ", k)) +
      theme_bw() +
      theme(plot.title = element_text(size = 9))
  }
  # print marginal effect plots for each dependent variable
  print(wrap_plots(me_corporatism_core[[t]][[1]],
                   me_corporatism_core[[t]][[2]],
                   me_corporatism_core[[t]][[3]],
                   me_corporatism_core[[t]][[4]],
                   nrow = 2))
}


## New corporatism measure III: new corporatism measure (use Corp_fCORE instead of CorpAll)
corporatism_models_fcore <- list()
me_corporatism_fcore <- list()
for(t in c("openc", "ind_valueadd", "elect_comp")){
  for(k in dvs_v){
    formula_finnegan1 <- as.formula(paste(k, "~  csw(", t, "*Corp_fCORE, fossfuel_prodpercap, realgdpgr.x, unemp, factor(eu.x), ja20f_v2, carbon_inten1) | countryid + year"))
    reg_name <- paste("reg", t, k, sep = "_")
    corporatism_models_fcore[[reg_name]] <- feols(fml = formula_finnegan1, 
                                                  data = finnegan_merged)  
    
    # print regression tables 
    print(modelsummary(corporatism_models_fcore[[reg_name]],
                       estimate = "{estimate}{stars}",
                       # coef_map = covariates_names,
                       title = paste("Examining", k),
                       output = "kableExtra"))
    # create marginal effects plots
    corporatism_models_fcore[[paste("me", t, k, sep = "_")]] <- feols(fml = as.formula(paste(k, "~", t, "*Corp_fCORE + fossfuel_prodpercap + realgdpgr.x + unemp + factor(eu.x) + ja20f_v2 + carbon_inten1 | countryid + year")),
                                                                      data = finnegan_merged)
    me_corporatism_fcore[[t]][[k]] <- plot_slopes(corporatism_models_fcore[[paste("me", t, k, sep = "_")]], 
                                                  rug = T,
                                                  variables = t, by = "Corp_fCORE") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_y_continuous(paste("Marginal effect of", t),
                         breaks = seq(-0.1, 0.4, 0.1)) +
      #expand_limits(y = c(-0.1, 0.4)) +
      labs(x = "Corporatism",
           title = paste0("Marginal effect of\n", t, " on ", k)) +
      theme_bw() +
      theme(plot.title = element_text(size = 9))
  }
  # print marginal effect plots for each dependent variable
  print(wrap_plots(me_corporatism_fcore[[t]][[1]],
                   me_corporatism_fcore[[t]][[2]],
                   me_corporatism_fcore[[t]][[3]],
                   me_corporatism_fcore[[t]][[4]],
                   nrow = 2))
}


## Open-economy models 
open_economy_models <- list()
me_open_economy <- list()
for(t in c("ri", "CorpAll", "CorpAllsm", "elect_comp", "CorpCORE", "Corp_fCORE")){
  for(k in dvs_v){
    formula_finnegan1 <- as.formula(paste(k, "~  csw(", t, "*openc,    
                                fossfuel_prodpercap, realgdpgr.x, ind_valueadd, unemp, factor(eu.x), ja20f_v2, carbon_inten1) | countryid + year"))
    reg_name <- paste("reg", t, k, sep = "_")
    open_economy_models[[reg_name]] <- feols(fml = formula_finnegan1, 
                                             data = finnegan_merged)  
    
    # print regression tables 
    print(modelsummary(open_economy_models[[reg_name]],
                       estimate = "{estimate}{stars}", 
                       # coef_map = covariates_names,
                       title = paste("Examining", k),
                       output = "kableExtra"))
    # create marginal effect plots 
    open_economy_models[[paste("me", t, k, sep = "_")]] <- feols(fml = as.formula(paste(k, "~", t, "*openc + ja20f_v2 + fossfuel_prodpercap +
                                                                                          realgdpgr.x + unemp + factor(eu.x) + inst_env2 | countryid + year")), 
                                                                 data = finnegan_merged)  
    me_open_economy[[t]][[k]] <- plot_slopes(open_economy_models[[paste("me", t, k, sep = "_")]], rug = T, 
                                             variables = t, by = "openc") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_continuous("Openness of economy") +
      scale_y_continuous(paste("Marginal effect of", t), 
                         breaks = seq(-0.1, 0.4, 0.1)) +
      #expand_limits(y = c(-0.1, 0.4)) +
      labs(title = paste0("Marginal effect of\n", t, " on ", k)) +
      theme_bw() +
      theme(plot.title = element_text(size = 9))
  }
  # print marginal effect plots for each dependent variable
  print(wrap_plots(me_open_economy[[t]][[1]], me_open_economy[[t]][[2]],
                   me_open_economy[[t]][[3]], me_open_economy[[t]][[4]], 
                   nrow = 2))
}





