


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
# Additional figures 
################################


## Adoption of climate policies

# overall adoption 
oecd_adoption1 %>%
  group_by(time_period) %>%
  summarise(sum_adopted = sum(obs_value, na.rm = T)) %>% 
  ggplot(aes(x = time_period, y = sum_adopted)) +
  geom_line(alpha = 0.3) +
  geom_rect(aes(xmin = 1998, xmax = 2014, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.02) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  expand_limits(y = 2000) +
  labs(y = "Number of adopted policies", 
       title = "Sum of adopted policies, 1990 - 2022") +
  theme_bw()




## level and growth rate of adoption of cross-sectoral policies
oecd_adoption2 %>% 
  filter(grepl("^LEV2_CROSS_", clim_act_pol)) %>%
  group_by(time_period, climate_actions_and_policies) %>%
  summarise(sum_adopted = sum(obs_value, na.rm = T)) %>% 
  ungroup() %>%
  group_by(climate_actions_and_policies) %>%
  mutate(lag_value = lag(sum_adopted, 1),
         lag_value = ifelse(lag_value == 0 & sum_adopted != 0, NA, lag_value),
         growth_rate = (sum_adopted/lag_value - 1)*100) %>%
  pivot_longer(cols = c(sum_adopted, growth_rate), 
               names_to = "indicator", values_to = "value") %>% 
  filter(!grepl("GHG emission targets", climate_actions_and_policies)) %>%
  ggplot(aes(x = time_period, y = value)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_grid(indicator~climate_actions_and_policies, 
             labeller = labeller(indicator = c("sum_adopted" = "Sum of adopted policies",
                                               "growth_rate" = "Growth rate of adopted policies (%)")),
             scales = "free_y") +
  labs(y = "", 
       title = "Sum of and annual growth rate of adopted cross-sectoral policies, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 7))

# adoption of international policies over time 
oecd_adoption2 %>% 
  filter(grepl("^LEV2_INT_", clim_act_pol)) %>%
  group_by(time_period, climate_actions_and_policies) %>%
  summarise(sum_adopted = sum(obs_value, na.rm = T)) %>% 
  ggplot(aes(x = time_period, y = sum_adopted, colour = climate_actions_and_policies)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.6) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  labs(y = "Number of adopted policies", 
       title = "Sum of adopted international policies, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8))



## level and growth rate of adoption of sectoral policies
oecd_adoption2 %>% 
  filter(grepl("^LEV2_SEC_", clim_act_pol)) %>%
  mutate(sector = str_extract(climate_actions_and_policies, "\\w+"),
         instrument_type = as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*"))) %>% 
  group_by(time_period, sector, instrument_type) %>%
  summarise(sum_adopted = sum(obs_value, na.rm = T)) %>% 
  ungroup() %>%
  group_by(sector, instrument_type) %>%
  mutate(lag_value = lag(sum_adopted, 1),
         lag_value = ifelse(lag_value == 0 & sum_adopted != 0, NA, lag_value),
         growth_rate = (sum_adopted/lag_value - 1)*100) %>% 
  pivot_longer(cols = c(sum_adopted, growth_rate), 
               names_to = "indicator", values_to = "value") %>% 
  ggplot(aes(x = time_period, y = value, colour = instrument_type)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  scale_colour_brewer("", palette = "Dark2") +
  facet_grid(indicator~sector, 
             labeller = labeller(indicator = c("sum_adopted" = "Sum of adopted policies",
                                               "growth_rate" = "Growth rate of adopted policies (%)")),
             scales = "free_y") +
  labs(y = "", 
       title = "Sum of and annual growth rate of adopted sectoral policies, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 7))


## Stringency of climate policies

# overall stringency
oecd_stringency1 %>%
  group_by(time_period) %>%
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>%
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  ggplot(aes(x = time_period, y = mean_stringency)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  expand_limits(y = 0) +
  labs(y = "Mean stringency", 
       title = "Mean stringency, 1990 - 2022") +
  theme_bw()



### adoption by instrument type at sectoral level
oecd_adoption2 %>%
  filter(grepl("^LEV2_SEC_", clim_act_pol)) %>%
  mutate(sector = str_extract(climate_actions_and_policies, "\\w+"),
         instrument_type = as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*"))) %>% 
  group_by(time_period, sector, instrument_type) %>%
  summarise(sum_adopted = sum(obs_value1, na.rm = T)) %>% 
  ggplot(aes(x = time_period, y = sum_adopted, colour = instrument_type)) +
  geom_line(linetype = "dashed") +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_colour_brewer("", palette = "Set1") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_wrap(~sector) +
  labs(y = "Sum of adopted policies", 
       title = "Adopted policies by sector and instrument type, 1990 - 2022") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))



## level and growth rate of stringency of cross-sectoral policies
oecd_stringency2 %>% 
  filter(grepl("^LEV2_CROSS_", clim_act_pol)) %>%
  group_by(time_period, climate_actions_and_policies) %>%
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>%
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  ungroup() %>%
  group_by(climate_actions_and_policies) %>%
  mutate(lag_value = lag(mean_stringency, 1),
         lag_value = ifelse(lag_value == 0 & mean_stringency != 0, NA, lag_value),
         growth_rate = (mean_stringency/lag_value - 1)*100) %>% 
  pivot_longer(cols = c(mean_stringency, growth_rate), 
               names_to = "indicator", values_to = "value") %>% 
  ggplot(aes(x = time_period, y = value, 
             colour = climate_actions_and_policies)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.6) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_grid(indicator~climate_actions_and_policies, 
             labeller = labeller(indicator = c("mean_stringency" = "Mean stringency",
                                               "growth_rate" = "Growth rate of stringency (%)")),
             scales = "free_y") +
  labs(y = "", 
       title = "Mean stringency and annual growth rate of stringency of cross-sectoral policies, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 7))

# stringency of international policies over time
oecd_stringency2 %>% 
  filter(grepl("^LEV2_INT_", clim_act_pol)) %>%
  group_by(time_period, climate_actions_and_policies) %>%
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>%
  ggplot(aes(x = time_period, y = mean_stringency, 
             colour = climate_actions_and_policies)) +
  geom_line(linewidth = 1) +
  # geom_smooth(method = "loess", se = F, span = 0.6) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  expand_limits(y = c(0, 8)) +
  labs(y = "Mean stringency", 
       title = "Mean stringency of international policies, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8))



## level and growth rate of stringency by instrument type at sectoral level 
oecd_stringency2 %>%
  filter(grepl("^LEV2_SEC_", clim_act_pol)) %>%
  mutate(sector = str_extract(climate_actions_and_policies, "\\w+"),
         instrument_type = as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*"))) %>% 
  group_by(time_period, sector, instrument_type) %>%
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>%
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  ungroup() %>%
  group_by(sector, instrument_type) %>%
  mutate(lag_value = lag(mean_stringency, 1),
         lag_value = ifelse(lag_value == 0 & mean_stringency != 0, NA, lag_value),
         growth_rate = (mean_stringency/lag_value - 1)*100) %>% 
  pivot_longer(cols = c(mean_stringency, growth_rate), 
               names_to = "indicator", values_to = "value") %>% 
  ggplot(aes(x = time_period, y = value, 
             colour = instrument_type)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_grid(indicator~sector, 
             labeller = labeller(indicator = c("mean_stringency" = "Mean stringency",
                                               "growth_rate" = "Growth rate of stringency (%)")),
             scales = "free_y") +
  expand_limits(y = c(0, 8)) +
  labs(y = "", 
       title = "Mean stringency and annual\ngrowth rate of stringency of policies by sector and instrument type, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 7),
        title = element_text(size = 9))

# stringency by sector for specific instruments 
oecd_stringency3 %>%
  mutate(sector = factor(str_trim(as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*")), side = "both")),
         policy = factor(str_trim(str_extract(climate_actions_and_policies, "^[^-]+"),
                                  side = "both")),
         policy = fct_collapse(policy, 
                               "Fossil fuel subsidies" = c("Fossil Fuel Subsidies",
                                                           "Fossil fuels subsidies"),
                               "Carbon tax" = c("Carbon tax", "Carbon Tax"))) %>%
  filter(grepl("Buildings|Electricity|Transport|Industry", sector)) %>%
  group_by(time_period, sector, policy) %>%
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>%
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  ggplot(aes(x = time_period, y = mean_stringency, colour = policy)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_wrap(~sector) +
  expand_limits(y = c(0, 8)) +
  labs(y = "Mean stringency", 
       title = "Mean stringency of policies by sector and instrument type, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

### stringency by instrument type at sectoral level
oecd_stringency2 %>%
  filter(grepl("^LEV2_SEC_", clim_act_pol)) %>%
  mutate(sector = str_extract(climate_actions_and_policies, "\\w+"),
         instrument_type = as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*"))) %>% 
  group_by(time_period, sector, instrument_type) %>%
  summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>%
  ggplot(aes(x = time_period, y = mean_stringency, colour = instrument_type)) +
  geom_line(linetype = "dashed") +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_colour_brewer("", palette = "Set1") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_wrap(~sector) +
  expand_limits(y = c(0, 8)) +
  labs(y = "Mean stringency", 
       title = "Mean stringency of policies by sector and instrument type, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))

## level and growth rate of stringency by sector for specific instruments
oecd_stringency3 %>%
  mutate(sector = factor(str_trim(as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*")), side = "both")),
         policy = factor(str_trim(str_extract(climate_actions_and_policies, "^[^-]+"),
                                  side = "both")),
         policy = fct_collapse(policy, 
                               "Fossil fuel subsidies" = c("Fossil Fuel Subsidies",
                                                           "Fossil fuels subsidies"),
                               "Carbon tax" = c("Carbon tax", "Carbon Tax"))) %>%
  filter(grepl("Buildings|Electricity|Transport|Industry", sector)) %>%
  group_by(time_period, sector, policy) %>%
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>%
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  ungroup() %>%
  group_by(sector, policy) %>%
  mutate(lag_value = lag(mean_stringency, 1), 
         lag_value = ifelse(lag_value == 0 & mean_stringency != 0, NA, lag_value),
         stringency_growth = (mean_stringency/lag_value - 1)*100) %>%
  pivot_longer(c(mean_stringency, stringency_growth), 
               names_to = "indicator", values_to = "value") %>%
  ggplot(aes(x = time_period, y = value, colour = policy)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = F, span = 0.5) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
  facet_grid(indicator~sector, 
             labeller = labeller(indicator = c("mean_stringency" = "Mean stringency", 
                                               "stringency_growth" = "Stringency growth rate p.a. (%)")),
             scale = "free_y") +
  expand_limits(y = c(0, 8)) +
  labs(y = "Mean stringency", 
       title = "Mean stringency of policies by sector and instrument type, 1990 - 2022") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90))


# stringency by sector for *even more* specific instruments
lev4_stringency <- oecd_stringency4 %>%
  mutate(instrument = factor(str_trim(as.character(str_extract_all(climate_actions_and_policies, "(?<= - ).*")), side = "both")),
         sector = factor(str_trim(str_extract(climate_actions_and_policies, "^[^-]+"),
                                  side = "both"))) %>%
  group_by(time_period, sector, instrument) %>%
  summarise(mean_stringency = mean(obs_value, na.rm = T)) %>% 
  # summarise(mean_stringency = mean(obs_value1, na.rm = T)) %>% 
  ungroup()

## loop over sectors 
for(h in unique(lev4_stringency$sector)){
  print(lev4_stringency %>%
          filter(sector == h) %>%
          ggplot(aes(x = time_period, y = mean_stringency, colour = instrument)) +
          geom_line(linewidth = 1) +
          scale_colour_brewer("", palette = "Dark2") +
          scale_x_continuous("Year", breaks = seq(1990, 2020, 5)) +
          expand_limits(y = c(0, 8)) +
          labs(y = "Mean stringency", 
               title = "Mean stringency by policy area, 1990 - 2022",
               subtitle = paste("Policy:", h)) +
          theme_bw() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)))
}