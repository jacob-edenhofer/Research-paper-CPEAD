
################################
# Preliminaries
################################

# Load packages
library(tidyverse)  # includes dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)     # For reading Excel files
library(scales)    # For formatting numbers and dates
library(arrow)      # Efficient data reading and writing
library(janitor)    # For clean_names()
library(here)       # For constructing paths
library(kableExtra) # For enhanced table output
library(patchwork)  # For combining plots
library(haven)     # For reading Stata files

# Set up path
data_folder <- list.files(file.path(here("03 Cleaned data", "OECD CAPMF")), full.names = TRUE)

# import data
oecd_overall <- readRDS(paste0(data_folder[grepl("Overall", data_folder)], "/oecd_merged.rds"))
oecd_adoption1 <- readRDS(paste0(data_folder[grepl("Adopt", data_folder)], "/oecd_adoption_LEV1.rds"))
oecd_adoption2 <- readRDS(paste0(data_folder[grepl("Adopt", data_folder)], "/oecd_adoption_LEV2.rds"))
oecd_stringency1 <- readRDS(paste0(data_folder[grepl("Stringency", data_folder)], "/oecd_stringency_LEV1.rds"))
oecd_stringency2 <- readRDS(paste0(data_folder[grepl("Stringency", data_folder)], "/oecd_stringency_LEV2.rds"))
oecd_stringency3 <- readRDS(paste0(data_folder[grepl("Stringency", data_folder)], "/oecd_stringency_LEV3.rds"))
oecd_stringency4 <- readRDS(paste0(data_folder[grepl("Stringency", data_folder)], "/oecd_stringency_LEV4.rds"))



################################
# Descriptive figures
################################


## Figure 1 ##

### adoption data 
corp_adopt1 <- oecd_adoption1 %>%
  filter(!ref_area %in% c("USA", "BRA"), 
         !is.na(reference_area), 
         !is.na(corp_all)) %>%
  group_by(time_period) %>%
  mutate(median_corp_all = median(corp_all, na.rm = T)) %>%
  ungroup() %>%
  mutate(corp_all_group = ifelse(corp_all > median_corp_all, "Above median", "Below median")) %>%
  group_by(time_period, corp_all_group) %>%
  summarise(mean_policy_adopt = mean(obs_value1, na.rm = T)) %>%
  ungroup() 

### stringency data
corp_str1 <- oecd_stringency1 %>%
  filter(!ref_area %in% c("USA", "BRA"), 
         !is.na(reference_area), 
         !is.na(corp_all)) %>%
  group_by(time_period) %>%
  mutate(median_corp_all = median(corp_all, na.rm = T)) %>%
  ungroup() %>%
  mutate(corp_all_group = ifelse(corp_all > median_corp_all, "Above median", "Below median")) %>%
  group_by(time_period, corp_all_group) %>%
  summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>%
  ungroup() 
  
## bind together
corp_all1 <- bind_cols(corp_adopt1, corp_str1) %>%
  clean_names() %>%
  select(-c(time_period_4, corp_all_group_5)) %>%
  rename(time_period = time_period_1, 
         corp_all_group = corp_all_group_2) %>%
  pivot_longer(cols = c(mean_policy_adopt, mean_stringency), 
               names_to = "indicator", 
               values_to = "value") 

## plot
corp_all1 %>%
  ggplot(aes(x = time_period, y = value, colour = corp_all_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~indicator, 
             labeller = labeller(indicator = c("mean_policy_adopt" = "Mean number of adopted policies", 
                                               "mean_stringency" = "Mean stringency of policies")),
             scales = "free_y") +
  scale_colour_brewer("Corporatism score", palette = "Set1") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  expand_limits(y = 0) +
  labs(y = "", 
       title = "Adoption and stringency of climate policies by corporatism grouping, 1990 - 2018") +
  theme_bw() +
  theme(legend.text = element_text(size = 10),
        axis.title = element_text(size = 9),
        strip.background = element_blank(),
        plot.title = element_text(size = 13, hjust = 0.5),
        strip.text.x = element_text(size = 10),
        legend.position = "bottom")

## save plot 
ggsave(here("06 Figures and tables", "Figures", "corp_all_climate.png"), 
       width = 9, height = 6, dpi = 300)


###########################
# Appendix - tables
##########################


## Table A1 ##

corp_adopt1_1 <- oecd_adoption1 %>%
  group_by(ref_area, time_period) %>%
  mutate(obs_value1_cy = mean(obs_value1, na.rm = T)) %>%
  ungroup() %>%
  distinct(ref_area, time_period, obs_value1_cy, .keep_all = T) %>%
  select(-c(clim_act_pol, climate_actions_and_policies)) %>%
  filter(!ref_area %in% c("USA", "BRA"), 
         !is.na(reference_area), 
         !is.na(corp_all)) %>%
  group_by(time_period) %>%
  mutate(median_corp_all = median(corp_all, na.rm = T)) %>%
  ungroup() %>%
  ungroup() %>%
  mutate(corp_all_group = ifelse(corp_all > median_corp_all, "Above median", "Below median"))


adopt_balance_df <- corp_adopt1_1 %>%
  select("Number of adopted policies" = obs_value1_cy, 
         corp_all_group, 
         "Manufacturing value added (% of GDP)" = nv_ind_manf_zs, 
         "Industry value added (% of GDP)" = nv_ind_totl_zs, 
         "CO2 emissions per capita" = co2_per_capita, 
         "Fossil share electricity" = fossil_share_elec, 
         "Fossil share energy" = fossil_share_energy,
         "Trade CO2 share" = trade_co2_share, 
         "Openness of economy" = openc, 
         "Gallagher's disproportionality index" = dis_gall) %>%
  modelsummary::datasummary_balance(~corp_all_group, 
                                    dinm_statistic = "p.value", output = "dataframe",
                                    data = .)

### do the same for stringency
corp_str1_1 <- oecd_stringency1 %>%
  group_by(ref_area, time_period) %>%
  mutate(obs_value1_cy = mean(obs_value1, na.rm = T)) %>%
  ungroup() %>%
  distinct(ref_area, time_period, obs_value1_cy, .keep_all = T) %>%
  select(-c(clim_act_pol, climate_actions_and_policies)) %>%
  filter(!ref_area %in% c("USA", "BRA"), 
         !is.na(reference_area), 
         !is.na(corp_all)) %>%
  group_by(time_period) %>%
  mutate(median_corp_all = median(corp_all, na.rm = T)) %>%
  ungroup() %>%
  mutate(corp_all_group = ifelse(corp_all > median_corp_all, "Above median", "Below median"))


str_balance_df <- corp_str1_1 %>%
  select("Stringency" = obs_value1_cy, 
         corp_all_group) %>%
  modelsummary::datasummary_balance(~corp_all_group, 
                                    dinm_statistic = "p.value", output = "dataframe",
                                    data = .)

## merge into one dataframe
balance_df <- bind_rows(adopt_balance_df, str_balance_df) 


### use kableExtra to create a table
balance_df %>%
  kable(format = "latex", 
        escape = T, 
        booktabs = T,
        caption = "Balance table for Figure \ref{fig:intro-motivation-figure} by corporatism grouping",
        label = "appendix-balance-table-motivation",
        col.names = c("Variable", "Mean", "Std.Dev.", "Mean", "Std.Dev", "Diff in means", "p.value")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  # row_spec(0, bold = T) %>% ## for making column names bold
  add_header_above(c(" ", "Above median" = 2, "Below median" = 2, " ", " ")) %>%
  add_header_above(c(" ", "Corporatism score" = 4, " ", " ")) %>%
  save_kable(here("06 Figures and tables", "Tables", "balance_table_motivation.tex"))


###########################
#  Appendix - figures 
###########################
  
## Figure A1 ##

## level and growth rate of overall adoption 
adoption_plot1 <- oecd_adoption1 %>%
  group_by(time_period) %>%
  summarise(sum_adopted = sum(obs_value1, na.rm = T)) %>% 
  mutate(growth_rate = (sum_adopted/dplyr::lag(sum_adopted, 1) - 1)*100) %>%
  ungroup() %>%
  pivot_longer(cols = c(sum_adopted, growth_rate), 
               names_to = "indicator", values_to = "value") %>%
  ggplot(aes(x = time_period, y = value)) +
  geom_line(colour = "red", linetype = "dashed") +
  geom_rect(aes(xmin = 1995, xmax = 2009, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.02) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_x_continuous("", breaks = seq(1990, 2020, 5)) +
  facet_wrap(~indicator, 
             labeller = labeller(indicator = c("sum_adopted" = "Sum of adopted policies",
                                               "growth_rate" = "Annual growth of adopted policies (%)")),
             scales = "free_y") +
  labs(y = "") +
  theme_bw() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))


## level and growth rate of overall stringency 
stringency1_plot <- oecd_stringency1 %>%
  group_by(time_period) %>%
  summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>%
  mutate(lag_value = lag(mean_stringency, 1),
         lag_value = ifelse(lag_value == 0 & mean_stringency != 0, NA, lag_value),
         growth_rate = (mean_stringency/lag_value - 1)*100) %>% 
  pivot_longer(cols = c(mean_stringency, growth_rate), 
               names_to = "indicator", values_to = "value") %>% 
  ggplot(aes(x = time_period, y = value)) +
  geom_line(colour = "red", linetype = "dashed") +
  geom_rect(aes(xmin = 1995, xmax = 2009, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.02) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  expand_limits(y = 0) +
  facet_wrap(~indicator, 
             labeller = labeller(indicator = c("mean_stringency" = "Mean stringency",
                                               "growth_rate" = "Annual stringency growth (%)")),
             scales = "free_y") +
  labs(y = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))


## patch together 
patched_plot <- adoption_plot1 + stringency1_plot + 
  plot_layout(ncol = 1) +
  plot_annotation(title = "Climate policy adoption and stringency, in levels and growth rates, 1990 - 2022", 
                  theme = theme(plot.title = element_text(size = 13, hjust = 0.5)))

## save plot
ggsave(filename = here("06 Figures and tables", "Figures", "adoption_stringency.png"), 
       plot = patched_plot,
       width = 9, height = 6, units = "in", dpi = 300)


#### Figure A2 ######

## adoption data
overall1_adopt <- oecd_adoption1 %>%
  filter(!ref_area %in% c("USA", "BRA"), 
         !is.na(reference_area)) %>%
  select(reference_area, time_period, obs_value1, climate_actions_and_policies)

## stringency data
overall1_str <- oecd_stringency1 %>%
  filter(!ref_area %in% c("USA", "BRA"), 
         !is.na(reference_area)) %>%
  select(reference_area, time_period, obs_value1, climate_actions_and_policies)

## bind together
overall1 <- overall1_adopt %>%
  left_join(overall1_str, 
            by = c("reference_area", "time_period", "climate_actions_and_policies"), 
            suffix = c("_adopted", "_stringency")) %>%
  clean_names() %>%
  mutate(obs_value1_adopted_re = scales::rescale(obs_value1_adopted, 
                                                 to = range(obs_value1_stringency, na.rm = T),
                                                 from = range(obs_value1_adopted, na.rm = T))) %>%
  pivot_longer(cols = c(obs_value1_adopted_re, obs_value1_stringency),
               names_to = "indicator", values_to = "value")

## plot adoption and stringency of all policies by country (y-axis) and year (x-axis)
overall1 %>%
  ggplot(aes(x = time_period, y = reference_area, fill = value)) +
  geom_tile() +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  scale_fill_viridis_c(option = "mako",
                       breaks = seq(0, 10, 2),
                       direction = -1) +
  facet_grid(indicator~climate_actions_and_policies, 
             labeller = labeller(indicator = c("obs_value1_adopted_re" = "Adoption of policies (rescaled)",
                                               "obs_value1_stringency" = "Stringency of policies")),
             scales = "fixed") +
  labs(y = "",
       title = "Adoption and stringency of climate policies by type of policy, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 13, hjust = 0.5),
        axis.title.y = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 9)) +
  guides(fill = guide_colourbar(title = "Adoption and stringency of policies",
                                title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 15, barheight = 1))
## save plot 
ggsave(here("06 Figures and tables", "Figures", "adoption_stringency_combined.png"),
       plot = last_plot(),
       width = 12, height = 8, dpi = 300)


#### Figure A3 ######

## plot adoption and stringency of cross-sectoral policies 

# adoption of cross-sectoral policies over time 
cross_adopt <- oecd_adoption2 %>% 
  filter(grepl("^LEV2_CROSS_", clim_act_pol)) %>%
  group_by(time_period, climate_actions_and_policies) %>%
  summarise(sum_adopted = sum(obs_value1, na.rm = T)) %>%
  ungroup()


# stringency of cross-sectoral policies over time
cross_str <- oecd_stringency2 %>% 
  filter(grepl("^LEV2_CROSS_", clim_act_pol)) %>%
  group_by(time_period, climate_actions_and_policies) %>%
  summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>%
  ungroup()


## bind together 
cross_merged <- bind_cols(cross_adopt, cross_str) %>%
  clean_names() %>%
  select(-c(time_period_4, climate_actions_and_policies_5)) %>%
  rename(time_period = time_period_1, 
         climate_actions_and_policies = climate_actions_and_policies_2)

### pivot long
cross_merged_long <- cross_merged %>%
  pivot_longer(cols = c(sum_adopted, mean_stringency), 
               names_to = "indicator", values_to = "value") 


## plot
cross_merged_long %>%
  ggplot(aes(x = time_period, y = value,
             colour = climate_actions_and_policies)) +
  geom_line(linewidth = 1) +
  # geom_smooth(method = "loess", se = F, span = 0.6) +
  scale_colour_brewer("", palette = "Set2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_grid(indicator~climate_actions_and_policies, 
             scales = "free_y",
             labeller = labeller(indicator = c("mean_stringency" = "Mean stringency", 
                                              "sum_adopted" = "Sum of adopted policies"))) +
  # expand_limits(y = c(0, 8)) +
  labs(y = "Value",
       title = "Adoption and mean stringency of cross-sectoral climate policies, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 13),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))

## save plot 
ggsave(filename = here("06 Figures and tables", "Figures", "cross_sectoral_adoption_stringency.png"), 
       plot = last_plot(),
       width = 9, height = 6, units = "in", dpi = 300)


#### Figure A4 ######


## plot adoption and stringency by sector and instrument type 
adopt_sectr_inst <- oecd_adoption2 %>%
  filter(grepl("^LEV2_SEC_", clim_act_pol)) %>%
  mutate(sector = str_extract(climate_actions_and_policies, "\\w+"),
         instrument_type = as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*"))) %>% 
  group_by(time_period, sector, instrument_type) %>%
  summarise(sum_adopted = sum(obs_value1, na.rm = T)) %>% 
  ungroup()


string_sectr_inst <- oecd_stringency2 %>%
  filter(grepl("^LEV2_SEC_", clim_act_pol)) %>%
  mutate(sector = str_extract(climate_actions_and_policies, "\\w+"),
         instrument_type = as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*"))) %>% 
  group_by(time_period, sector, instrument_type) %>%
  summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>%
  ungroup()

merged <- bind_cols(adopt_sectr_inst, string_sectr_inst) %>%
  clean_names() %>%
  select(-c(time_period_5, sector_6, instrument_type_7)) %>%
  rename(time_period = time_period_1, sector = sector_2, instrument_type = instrument_type_3)

merged_long <- merged %>%
  pivot_longer(cols = c(sum_adopted, mean_stringency), names_to = "variable", values_to = "value") 

## plot 
merged_long %>%
  ggplot(aes(x = time_period, y = value, colour = instrument_type)) +
  geom_line(linetype = "dashed") +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_colour_brewer("", palette = "Set1") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_grid(variable~sector, 
             scales = "free_y",
             labeller = labeller(variable = c("mean_stringency" = "Mean stringency", 
                                              "sum_adopted" = "Sum of adopted policies"))) +
  labs(y = "Value", 
       title = "Adoption and stringency by sector and instrument type, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 13),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))

## save plot 
ggsave(filename = here("06 Figures and tables", "Figures", "adoption_stringency_sectr_inst.png"), 
       plot = last_plot(),
       width = 9, height = 6, units = "in", dpi = 300)


