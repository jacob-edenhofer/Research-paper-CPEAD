
#############################
# Preliminaries 
#############################

# Turn off scientific notation
options(scipen = 999)

# Load libraries, custom functions, and the variable dictionary
source(here::here("02 Cleaning data code", "Custom scripts", "custom_packages.R"))
source(here::here("02 Cleaning data code", "Custom scripts", "custom_functions.R"))
source(here::here("02 Cleaning data code", "Custom scripts", "var_dictionary.R"))

# Define directories 
raw_data_dir <- file.path(here(), "01 Raw data")
cleaned_data_dir <- file.path(here(), "03 Cleaned data")
output_dir <- file.path(here(), "06 Figures and tables")

# Set number of cores (this depends on your working system; on windows, use 1; on MAC, use parallel::detectCores() and set the appropriate number)
cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, parallel::detectCores() - 1)


#############################
# Import data
#############################

# List cleaned data 
cleaned_data <- list.files(cleaned_data_dir, recursive = T, full.names = T)

# Import data
raw_data <- mclapply(cleaned_data, function(x){
  import_data(x)
}, mc.cores = cores) %>%
  setNames(
    nm = tools::file_path_sans_ext(basename(cleaned_data))
  ) 

#############################
# Data wrangling 
#############################

# Extract oecd_stringency_LEV1 as data.table object from the list 
oecd_merged <- as.data.table(raw_data[["oecd_stringency_LEV1"]])

# Columns to clean 
col_clean <- names(oecd_merged)[grepl("(_x|_y)$", names(oecd_merged))]

# Keep only one of the columns that have the save name, except for the suffix and delete the other one 
for(j in col_clean){
  col_new_name <- gsub("_x|_y", "", j)
  print(col_new_name)
  
  # Check if the column already exists
  if(col_new_name %in% names(oecd_merged)){
    oecd_merged <- oecd_merged[, (j) := NULL]
  } else {
    oecd_merged <- oecd_merged[, (col_new_name) := get(j)]
    oecd_merged <- oecd_merged[, (j) := NULL]
  }
}

# Extract different columns for the dependent, moderating, and independent variables
dv_var <- names(oecd_merged)[grepl("obs_value", names(oecd_merged))]
corp_var <- names(oecd_merged)[grepl("corp|ud_|netu_|um_|bc|tum|cov|nec|nuc_", names(oecd_merged))]
mod_var <- names(oecd_merged)[grepl("pr_ingov_mean|openc|eco_cip", names(oecd_merged))]
control_var <- names(oecd_merged)[grepl("gdp|dis|renewable|co2|ind|clim_act|pop|measure_|year|country", names(oecd_merged))]
selected_columns <- c(dv_var, corp_var, mod_var, control_var)


#############################
# Descriptive statistics 
#############################

# Prune data 
oecd_merged_table <- oecd_merged[, ..selected_columns]

# Rename columns
names(oecd_merged_table) <- var_dict[names(oecd_merged_table)]

# Table  
oecd_merged_table %>%
  datasummary(All(.) ~ (N + PercentMissing + Mean + SD + Min + P25 + Median + P75 + Max), 
              coef_map = var_dict,
              data = ., fmt = "%.3f")


#############################
# Descriptive plots 
#############################

# Define function for visualising basic variation 
variation_plots <- function(data, variables, group_variables, text_size = NULL, output_dir = NULL){
  # Set up loop over variables 
  for(var in variables){
    # Extract variable name
    var_name <- var_dict[var]
    
    # Compute change in var by group_variables
    data_plot <- data[, .SD, .SDcols = c(group_variables, var)]
    
    # Extract all group_variables except for time_period and half_decade
    group_variables_mod1 <- group_variables[!grepl("time_period|half_decade", group_variables)]
    
    # Check if variable is numeric
    if(is.numeric(data_plot[[var]]) == FALSE){
      next
      log_warning(paste("Variable", var_name, "is not numeric. Skipping."))
    }
    
    # Calculate the delta by group
    data_plot[, delta := get(var) - shift(get(var), type = "lag"), by = group_variables_mod1]
    
    # Create new grouping variable which deletes only time_period 
    group_variables_mod2 <- group_variables[!grepl("time_period", group_variables)]
    
    # Calculate the half-decade average change
    data_plot <- data_plot[, delta_hc := mean(delta, na.rm = TRUE), by = group_variables_mod2]
    
    # Select unique values for plotting
    data_plot <- unique(data_plot[, c(group_variables_mod2, "delta_hc"), with = FALSE])
    
    # Remove any NA values
    data_plot <- data_plot[!is.na(delta_hc)]
    
    # If group_variables contains climate, then add facet_wrap; otherwise set to NULL
    if(any(grepl("climate", group_variables))){
      facet_wrap <- facet_wrap(~climate_actions_and_policies)
    } else {
      facet_wrap <- NULL
    }
    
    # Plot
    tryCatch({
      p <- data_plot %>%
        ggplot(aes(y = half_decade, x = reorder(reference_area, delta_hc), fill = delta_hc)) +
        geom_tile() +
        geom_text(aes(label = round(delta_hc, 2)), size = 2.1) +
        scale_fill_gradient2(low = "blue", mid = "gray95", high = "red", midpoint = 0, na.value = "white") +
        coord_flip() +
        facet_wrap +
        labs(title = paste("Change in", var_name, "by half-decade"),
             x = "",
             y = "Half-decade") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 7), 
              strip.background = element_blank(),
              axis.text.x = element_text(size = 7),
              axis.title.y = element_text(size = 8),
              plot.title = element_text(hjust = 0.5, size = 14),
              legend.position = "bottom"
        ) +
        guides(fill = guide_colourbar(title = "Average year-on-year difference", barwidth = 25, 
                                      title.position = "top", title.hjust = 0.5))
      
      # Create clean file name 
      file_name <- gsub("[,()]", "", var_name)        # Remove commas and parentheses
      file_name <- gsub(" ", "_", file_name)            # Replace spaces with underscores
      file_name <- tolower(file_name)                   # Convert to lowercase
      file_name <- gsub("_+$", "", file_name)           # Remove trailing underscores
      
      # Save plot
      save_plot(plot = p, path = paste0(output_dir, "/", file_name, ".png"))
      },error = function(e) {
        message(paste("Error in creating plot for", var_name))
    })
      
  }
}


# Create list of variables for the dependent, moderating, and independent variables
list_plot_var <- list(
  "CAPMF" = dv_var, 
  "Corporatism" = corp_var,
  "Moderators" = mod_var,
  "Controls" = control_var
)


# Loop through this list and apply the variation_plots function
mclapply(names(list_plot_var), function(x) {
  
  # If condition to set group variables
  if(x == "CAPMF"){
    group_variables <- c("reference_area", "climate_actions_and_policies", "time_period", "half_decade")
  } else {
    group_variables <- c("reference_area", "time_period", "half_decade")
  }
  
  # Apply function 
  variation_plots(data = oecd_merged, 
                  variables = list_plot_var[[x]], 
                  group_variables = group_variables, 
                  output_dir = paste0(output_dir, "/Figures/Descriptive plots/Variation/", x))
}, mc.cores = cores)


##################### To-dos #####################

# Corporatism plot by LEV1 sectors 
mclapply(corp_var, function(corporatism){
  
  # Create column names
  above_median_col <- paste0(corporatism, "_above_median")
  
  # Step 1: Compute median values 
  oecd_merged[, paste0(corporatism, "_median") := median(get(corporatism), na.rm = TRUE), by = time_period]
  oecd_merged[, (above_median_col) := factor(ifelse(get(corporatism) > get(paste0(corporatism, "_median")), "Above median", "Below median")), 
              by = .(reference_area, time_period)]
  
  # Step 2: Compute climate stringency by corporatism dummy variable
  oecd_merged[, "mean_simple_stringency" := mean(get("obs_value1"), na.rm = TRUE), by = .(time_period, get(above_median_col))]
  oecd_merged[, "mean_disagg_stringency" := mean(get("obs_value1"), na.rm = TRUE), by = .(time_period, get(above_median_col), climate_actions_and_policies)]
  
  # Step 3: Select unique combinations
  data1 <- unique(oecd_merged[, .(time_period, get(above_median_col), mean_simple_stringency)])
  data2 <- unique(oecd_merged[, .(time_period, climate_actions_and_policies, get(above_median_col), mean_disagg_stringency)])
  
  # Set names 
  setnames(data1, "V2", above_median_col)
  setnames(data2, "V3", above_median_col)
  
  # Debugging
  print(data1)
  print(names(data2))
  
  # Extract variable name 
  corp_var_name <- var_dict[corporatism]
  
  # Create file name 
  file_name <- gsub("[,()]", "", corp_var_name)        # Remove commas and parentheses
  file_name <- gsub(" ", "_", file_name)            # Replace spaces with underscores
  file_name <- tolower(file_name)                   # Convert to lowercase
  file_name <- gsub("_+$", "", file_name)
  
  # Plot the relationship between the new corporatism dummy variable and the mean climate stringency
  p1 <- data1 %>%
    filter(!is.na(!!sym(above_median_col))) %>%
    ggplot(aes(x = time_period, y = mean_simple_stringency, colour = !!sym(above_median_col))) +
    geom_line(linewidth = 1) +
    scale_colour_manual("Corporatism score", values = c("red", "blue")) +
    expand_limits(y = 0) +
    labs(title = str_wrap(paste0("Climate stringency by ", corp_var_name, ", above/below median (in a given year)")),
         x = "Year",
         y = "Mean climate stringency (theoretical range, 0-10)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          legend.position = "bottom"
          )
    print(p1)
    # Save plot 
    save_plot(plot = p1, path = paste0(output_dir, "/Figures/Descriptive plots/Variation/TS/", file_name, "_simple.png"))
    
    p2 <- data2 %>%
      filter(!is.na(!!sym(above_median_col))) %>%
      ggplot(aes(x = time_period, y = mean_disagg_stringency, colour = !!sym(above_median_col))) +
      geom_line(linewidth = 1) +
      scale_colour_manual("Corporatism score", values = c("red", "blue")) +
      expand_limits(y = 0) +
      facet_wrap(~climate_actions_and_policies) +
      labs(title = str_wrap(paste0("Climate stringency by ", corp_var_name, ", above/below median (in a given year)")),
           x = "Year",
           y = "Mean climate stringency (theoretical range, 0-10)") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 14), 
            legend.position = "bottom"
      )
    print(p2)
    # Save plot
    save_plot(plot = p2, path = paste0(output_dir, "/Figures/Descriptive plots/Variation/TS/", file_name, "_disagg.png"))
  
}, mc.cores = cores)


# Corporatism plot by instruments 


############# Reesidualised scatter plots #############

oecd_merged[, decade_fe := case_when(half_decade %in% c("1990-1994", "1995-1999") ~ "1990s",
                                     half_decade %in% c("2000-2004", "2005-2009") ~ "2000s",
                                     half_decade %in% c("2010-2014", "2015-2019") ~ "2010s",
                                     half_decade %in% c("2020-2022") ~ "2020s",
                                     TRUE ~ "Other")]

# Within-country variation over time 
wct <- fwlplot::fwl_plot(fml = obs_value ~ corp_all + co2_per_capita | reference_area + climate_actions_and_policies,
                         vcov = "HC1",
                         data = oecd_merged,
                         ggplot = T
                  )

# Clean plot 
wct_cleaned <- wct +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Corporatism score (residualised)",
       # title = "Within-country variation in climate stringency and CO2 emissions",
       y = "Climate stringency (residualised)",
       color = "Climate policy instrument",
       shape = "Climate policy instrument") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom"
        )

# Save plot
save_plot(plot = wct_cleaned, path = paste0(output_dir, "/Figures/Descriptive plots/Variation/Within_country.png"))


# Cross-sectional variation in a given half-decade/year
cvt <- fwlplot::fwl_plot(fml = obs_value ~ corp_all + co2_per_capita | climate_actions_and_policies + time_period,
                         vcov = "HC1", 
                         data = oecd_merged, 
                  ggplot = T
                  )

# Clean plot
cvt_cleaned <- cvt +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Corporatism score (residualised)",
       # title = "Cross-sectional variation in climate stringency and CO2 emissions",
       y = "Climate stringency (residualised)",
       color = "Climate policy instrument",
       shape = "Climate policy instrument") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom"
        )

# Save plot
save_plot(plot = cvt_cleaned, path = paste0(output_dir, "/Figures/Descriptive plots/Variation/Cross_sectional.png"))

