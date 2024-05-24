

## Use exploratory_sum_df to run the baseline specifications for the relevant interactions terms 
## write loop to run the baseline specifications for the relevant interaction terms and datasets
baseline_models <- list()
for(i in 1:nrow(exploratory_sum_df)){
  var1 <- exploratory_sum_df$corporatism_var[i]
  var2 <- exploratory_sum_df$variable_var[i]
  dataset_string <- exploratory_sum_df$dataset[i]
  formatted_strings <- sub("^(adoption|stringency)(LEV)(\\d+)$", "\\1_\\2\\3", dataset_string)
  dataset_name <- paste0("oecd_", formatted_strings)
  model_name <- paste(str_replace_all(dataset_name, "oecd_", ""), var1, var2, sep = "_")
  file_path_base_reg <- here("06 Figures and tables", "Tables", "Regressions", 
                             "Baseline", model_name)
  
  # check whether regression file already exists 
  if (!file.exists(paste0(file_path_base_reg, ".tex"))){
    dataset <- list_oecd[[dataset_name]]
    # mutate for adoption data frames 
    if(grepl("adoption", dataset_name)){
      dataset <- dataset %>% mutate(obs_value1 = ifelse(obs_value1 > 0, 1, 0))
    }
    # lag all covariates
    lagged_cols <- c(var1, var2, covariates)
    dataset <- dataset %>%
      group_by(iso3c, clim_act_pol) %>%
      # check that this works 
      mutate(across(
        .cols = lagged_cols,
        .fns = ~ lag(.x, 1, order_by = time_period),
        .names = "{.col}_lag1"  # Names the new columns with a _lag1 suffix
      )) %>%
      ungroup()
    
    # update var_dict with lagged variables
    for (col in lagged_cols) {
      new_var_name <- paste0(col, "_lag1")
      if (!new_var_name %in% names(var_dict)) {  # Check if entry does not exist and add it
        var_dict[new_var_name] <- paste0(var_dict[col], ", 1-year lag")
      }
    }
    # Add interaction terms to var_dict for lagged variables
    interaction_lagged_name <- paste0(var1, "_lag1*", var2, "_lag1")
    if (!interaction_lagged_name %in% names(var_dict)) {  # Check if entry does not exist and add it
      var_dict[interaction_lagged_name] <- paste0(var_dict[var1], " x ", var_dict[var2], ", both lagged by 1 year")
    }
    # create regression formula
    formula_base <- as.formula(paste0("obs_value1 ~ ", paste0(var1, "_lag1"), "*", 
                                      paste0(var2, "_lag1"), " + csw0(", 
                                      paste(paste0(covariates, "_lag1"), collapse = ", "), ") | iso3c + half_decade + clim_act_pol"))
    baseline_models[[model_name]] <-  feols(formula_base, vcov = ~iso3c, data = dataset)
    
    ## save baseline models using etable()
    etable(baseline_models[[model_name]],
           replace = T,
           tex = T,
           dict = var_dict, 
           title = paste("Baseline model for", var_dict[var1], "and", var_dict[var2]), 
           file = paste0(file_path_base_reg, ".tex"))
  } 
  ## also save coefficient plot 
  file_path_base_plot <- here("06 Figures and tables", "Figures", "Coefficient plots", "Baseline", model_name)
  names(baseline_models[[model_name]]) <- as.character(seq(1, length(baseline_models[[i]]), 1))
  if (!file.exists(paste0(file_path_base_plot, ".png"))){
    modelsummary::modelplot(baseline_models[[model_name]]) +
      # add coef_map = var_dict later
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_colour_brewer("Model",
                          palette = "Set1") +
      scale_y_discrete(labels = label_wrap(30)) +
      labs(title = paste("Baseline model for", var_dict[var1], "and", var_dict[var2])) +
      theme_bw() +
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 11))
    ggsave(paste0(file_path_base_plot, ".png"), width = 9, height = 6, units = "in", dpi = 300)
  }
}

