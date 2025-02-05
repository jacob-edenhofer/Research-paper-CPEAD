
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
library(tools)
library(arrow)
library(haven)
library(patchwork)


# import data 
data_folder <- list.files(paste0(here(), "/01 Raw data"))
## select all elements, except for Codebooks
data_folder <- data_folder[!grepl("Codebooks", data_folder)]

## write function to import data from the folders
import_data <- function(folder_path){
  if(grepl("csv", folder_path)){
    if(grepl("lprdata_", folder_path)){
      data <- read_delim(folder_path, delim = ";") %>%
        clean_names()
    } else if(grepl("ICTWSS", folder_path)){
      data <- read_csv(folder_path, na = c("-99", "-88", "Missing", "NA")) %>%
        remove_empty(which = c("cols", "rows")) %>%
        clean_names() 
    } else {
    data <- read_csv_arrow(folder_path) %>%
      clean_names()
    }
  } else if(grepl("xlsx", folder_path)){
    if(grepl("GallupAnalytics", folder_path)){
      data <- read_xlsx(folder_path, skip = 6) %>%
        clean_names() %>%
        remove_empty(which = c("cols", "rows"))
    } else {
    data <- read_xlsx(folder_path) %>%
      clean_names()
    }
  } else if(grepl("rds|RDS", folder_path)){
    data <- readRDS(folder_path) %>%
      clean_names()
  } else if(grepl("dta", folder_path)){
    data <- read_dta(folder_path) %>%
      clean_names()
  } else {
    next
  }
}


# import data
for(i in 1:length(data_folder)){
  list_data <- list.files(paste0(here(), "/01 Raw data/", data_folder[i]), full.names = T)
  list_data <- list_data[!grepl("CIP_dynamic", list_data)]
  map(list_data, ~{
    # extract file names 
    file_name <- file_path_sans_ext(basename(.x))
    data_name <- str_to_lower(str_replace_all(gsub("\\..*", "", file_name), "-", "_"))
    # import data
    import_data(.x) %>%
      ## save this in global environment using assign() function and name it as the file name, but delete suffix (.csv, .rds, and so on)
      assign(data_name, ., envir = globalenv())
    })
}


# import kayser/rehmert cip data 
## specify folder path 
folder_kayser <- list.files(here("01 Raw data", "Controls and other", "CIP_dynamic"))
## read in the data
map_dfr(folder_kayser, ~{
  read_delim(here("01 Raw data", "Controls and other", "CIP_dynamic", .x), delim = ";", 
             locale = locale(decimal_mark = ",")) %>%
    clean_names() %>%
    mutate(excluding_parlgov_id = as.numeric(as.character(excluding_parlgov_id))) %>%
    remove_empty(which = c("rows", "cols")) 
}) -> kayser_cip

## save data
# write_csv(kayser_cip, here("01 Raw data", "Controls and other", "cip_dynamic_all.csv"))

#################################
# Data wrangling
#################################

## 1. CAPMF data

## add new column
capmf_official <- capmf_official %>% 
  mutate(obs_value1 = ifelse(is.na(obs_value), 0.000000, obs_value))

# # replace zeroes with missing values 
# is.na(capmf_official$obs_value1) <- capmf_official$obs_value == 0.000000


# select only meaningful columns
for(i in names(capmf_official)){
  levels <- nlevels(factor(capmf_official[[i]]))
  if(levels == 1 | sum(is.na(capmf_official[[i]])) == nrow(capmf_official)){
    capmf_official <<- capmf_official %>% select(-i)
  } else{
    next
  }
}


# create new variables (for fixed effects) 
capmf_official <- capmf_official %>% 
  # delete values for European Union 
  filter(!grepl("European Union", reference_area)) %>%
  mutate(region23 = countrycode(reference_area, "country.name", "region23"),
         iso3c = countrycode(reference_area, "country.name", "iso3c"),
         # for fixed effects estimations in different project 
         half_decade = case_when(time_period >= 1990 & time_period < 1995 ~ "1990-1994",
                                 time_period >= 1995 & time_period < 2000 ~ "1995-1999",
                                 time_period >= 2000 & time_period < 2005 ~ "2000-2004",
                                 time_period >= 2005 & time_period < 2010 ~ "2005-2009",
                                 time_period >= 2010 & time_period < 2015 ~ "2010-2014",
                                 time_period >= 2015 & time_period < 2020 ~ "2015-2019",
                                 time_period >= 2020 & time_period <= 2022 ~ "2020-2022",
                                 TRUE ~ NA),
         finnegan_time_dummy = ifelse(time_period %in% c(1998, 2014), 1, 0)) %>%
  # lagged values for dependent variables 
  group_by(ref_area, clim_act_pol, measure_2) %>%
  mutate(obs_value1_lag1 = dplyr::lag(obs_value1, 1, order_by = time_period),
         obs_value1_lag2 = dplyr::lag(obs_value1, 2, order_by = time_period),
         obs_value1_lag3 = dplyr::lag(obs_value1, 3, order_by = time_period),
         obs_value1_lag4 = dplyr::lag(obs_value1, 4, order_by = time_period)) %>%
  ungroup() 
    

## 2. Corporatism data
corporatismusall <- corporatismusall %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  select(country, iso3c, year, everything())

## same for corporatism_core
corporatismus_core <- corporatismus_core %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  select(country, iso3c, year, everything())

## post 90 version 
corporatism_post90 <- corporatismusall %>%
  filter(year >= 1990)

## post 90 version for corporatism_core
corporatism_core_post90 <- corporatismus_core %>%
  filter(year >= 1990)


## 3. WDI 

# to be able to delete pre-classified groups of countries, create new iso3c identifier
wdi_data <- wdi_data %>%
  mutate(iso3c_new = countrycode(country, "country.name", "iso3c"))

wdi_post90 <- wdi_data %>%
  # restrict data to individual countries, rather than pre-classified groups of countries
  filter(year >= 1990, !is.na(iso3c))


## 4. BMR data
bmr <- democracy_v4 %>%
  mutate(iso_code = countrycode(country, "country.name", "iso3c"))
## The usual suspects prove somewhat difficult to assign an iso3 code to.
bmr_post90 <- bmr %>%
  filter(year >= 1990, !is.na(iso_code), country != "GERMANY, WEST")

## 5. Polcon data 
polcon_pruned <- polcon_2021_vdem %>%
  select(-grep("^(party|j|f|l[1-2]|p1[0-5]|p[1-9]|p[1-9][0-9]|laworder|executive|prime|align|tsu|tsl|leg|cyear|ccode|cnts|icrg|ctry|country_text_id)", 
               names(polcon_2021_vdem))) %>%
  mutate(iso3c = countrycode(country_name, "country.name", "iso3c")) %>%
  select(country_name, iso3c, year, everything())

polcon_post90 <- polcon_pruned %>%
  filter(year >= 1990) %>%
  mutate(across(.cols = everything(), 
                .fns = ~ifelse(.x == -88, NA, .x)))

## 6. CPDS data
cpds <- cpds_1960_2021_update_2023 %>%
  select(country, year, eu, emu, grep("^ud|netu|openc|gdp|dis_|rae_|elderly|training", 
                                      names(cpds_1960_2021_update_2023))) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  select(country, iso3c, year, everything())

## post 90 version
cpds_post90 <- cpds %>%
  filter(year >= 1990) 


## 7. Finnegan data 
country <- c("1" = "Australia", 
             "2" = "Austria", 
             "3" = "Belgium",
             "4" = "Canada", 
             "5" = "Denmark",
             "6" = "Finland", 
             "7" = "France",
             "8" = "Germany",
             "9" = "Greece",
             "10" = "Ireland", 
             "11" = "Italy", 
             "12" = "Japan",
             "13" = "Netherlands",
             "14" = "New Zealand",
             "15" = "Norway",
             "16" = "Portugal",
             "17" = "Spain",
             "18" = "Sweden",
             "19" = "Switzerland",
             "20" = "United Kingdom",
             "21" = "United States")

# create new variable 
finnegan <- long_term_policymaking %>%
  mutate(country_long = factor(countryid, 
                               levels = c("1", "2",
                                          "3", "4", "5",
                                          "6", "7", "8",
                                          "9", "10", "11",
                                          "12", "13", "14",
                                          "15", "16", "17",
                                          "18", "19", "20",
                                          "21"),
                               labels = country),
         iso3c = countrycode(country_long, "country.name.en", "iso3c")) %>%
  select(countryid, country_long, iso3c, everything())


## 8. OWID 
### energy data
owid_energy_pruned <- owid_energy %>%
  select(country, year, iso_code, population, gdp, contains("share"), 
         contains("per_capita"))

### co2 data 
owid_co2_pruned <- owid_co2 %>%
  select(country, year, iso_code, population, gdp, contains("per_gdp"), 
         contains("per_capita"), contains("share"))

# post 90 versions
owid_energy_post90 <- owid_energy_pruned %>%
  filter(year >= 1990)

owid_co2_post90 <- owid_co2_pruned %>%
  filter(year >= 1990)


## 9. Fetzer/Yotzo data 
fetzer_yotzo <- election_shocks_20230924 %>%
  mutate(iso3c = countrycode(isocntry, "iso2c", "iso3c"), 
         cntry_name = countrycode(isocntry, "iso2c", "country.name.en")) %>%
  select(cntry_name, isocntry, iso3c, year, everything())

fetzer_yotzo %>% filter(year >= 1990) -> fetzer_yotzo_post90

fetzer_yotzo_post90_pruned <- fetzer_yotzo_post90 %>%
  # for some observations, multiple close elections occur within a year
  # very rudimentary approach: only keep one of these (not perfect, but ok)
  distinct(iso3c, year, .keep_all = T) 


## 10. trade_union data 
trade_union <- oecd_trade_union %>%
  mutate(cntry_name = countrycode(location, "iso3c", "country.name.en")) %>%
  rename("share_employees_trade_union" = obs_value) %>%
  select(cntry_name, location, time, everything())

trade_union %>% filter(time >= 1990) -> trade_union_post90


## 11. collective_bar data (same as for trade_union data)
collective_bar <- oecd_collective_bargaining %>%
  mutate(cntry_name = countrycode(cou, "iso3c", "country.name.en")) %>%
  rename("share_employees_collective_bar" = obs_value) %>%
  select(cntry_name, cou, time, everything())

collective_bar %>% filter(time >= 1990) -> collective_bar_post90


## 12. Kayser/Linnstaedt data 
kayser_comp <- lprdata_distrib_augmented_2015 %>%
  mutate(cntry_name = countrycode(isocode, "iso3c", "country.name.en"),
         cntry_name = ifelse(isocode == "AUL", "Australia", cntry_name)) %>%
  select(case, cnum, isocode, cntry_name, elecyr, everything())

kayser_comp %>% filter(elecyr >= 1990) -> kayser_comp_post90


## 13. Kayser/Rehmert wrangling 
kayser_rehmert <- data_eps_kayser_rehmert %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  select(country, iso3, year, everything())

## follow kayser et al. by adding the following variables 
kayser_rehmert <- kayser_rehmert %>%
  mutate(decades = case_when((year >= 1990 & year <= 1994) ~ 1, 
                             (year >= 1995 & year <= 1999) ~ 2, 
                             (year >= 2000 & year <= 2004) ~ 3, 
                             (year >= 2005 & year <= 2009) ~ 4, 
                             TRUE ~ 5),
         kyoto = case_when((country == "austria" & year >= 1998) ~ 1,
                           (country == "czechia" & year >= 1998) ~ 1, 
                           (country == "denmark" & year >= 1998) ~ 1, 
                           (country == "estonia" & year >= 1998) ~ 1, 
                           (country == "finland" & year >= 1998) ~ 1, 
                           (country == "germany" & year >= 1998) ~ 1, 
                           (country == "greece" & year >= 1998) ~ 1, 
                           (country == "ireland" & year >= 1998) ~ 1, 
                           (country == "italy" & year >= 1998) ~ 1, 
                           (country == "japan" & year >= 1998) ~ 1, 
                           (country == "netherlands" & year >= 1998) ~ 1, 
                           (country == "new zealand" & year >= 1998) ~ 1, 
                           (country == "norway" & year >= 1998) ~ 1, 
                           (country == "portugal" & year >= 1998) ~ 1, 
                           (country == "slovakia" & year >= 1998) ~ 1, 
                           (country == "slovenia" & year >= 1998) ~ 1, 
                           (country == "spain" & year >= 1998) ~ 1, 
                           (country == "sweden" & year >= 1998) ~ 1, 
                           TRUE ~ 0),
         fukushima = ifelse(year >= 2011, 1, 0)) %>%
  filter(year >= 1990)


## 14. Kayser/Rehmert wrangling -- full dataset
## modify germany variable 
kayser_cip <- kayser_cip %>%
  mutate(country = ifelse(is.na(country) & !is.na(germany), "Germany", country),
         date = as.Date(as.character(sub("^(..)(..)(....)$", "\\1-\\2-\\3", date)), "%d-%m-%Y"),
         iso3c = countrycode(country, "country.name.en", "iso3c")) %>%
  select(-germany)

## merge with parlgov dataset 
kayser_cip_parlgov <- kayser_cip %>%
  left_join(parlgov_parties, by = c("parlgov_id" = "party_id")) 

## convert to annual dataset
kayser_cip_annual <- kayser_cip_parlgov %>%
  group_by(country, year, family_name) %>%
  mutate(across(.cols = c(starts_with("pr_ingov"), "polls"), 
                .fns = ~mean(.x, na.rm = TRUE),
                .names = "{.col}_annual")) %>%
  ungroup() %>%
  select(-c(pr_ingov_mean, pr_ingov_lower, pr_ingov_upper, polls, date)) %>%
  distinct(country, year, family_name, pr_ingov_mean_annual, .keep_all = TRUE) %>%
  select(country, iso3c, year, dplyr::everything())

## filter for green parties
kayser_cip_green <- kayser_cip_annual %>%
  filter(grepl("Green", family_name)) 


## 15. ICTWSS

### somw wrangling
oecd_aias_ictwss_csv_v1_pruned <- oecd_aias_ictwss_csv_v1 %>%
  select(country, iso3c, year, tc, bc, concert, nec_fs, ed, e_dpriv, nuc_fs, tum,  
         grep("num|ud|um|cov", names(.)))

## define tc_bc_dummy 
oecd_aias_ictwss_csv_v1_pruned <- oecd_aias_ictwss_csv_v1_pruned %>%
  mutate(tc_mod = ifelse(tc != 0 & !is.na(tc), 1, 0), 
         tc_bc_dummy = ifelse(tc_mod == 1 & bc != 1 & !is.na(bc), 1, 0)) %>%
  select(country, iso3c, year, tc, tc_mod, bc, tc_bc_dummy, everything())

### post 1990 version 
ictwsspost90 <- oecd_aias_ictwss_csv_v1_pruned %>% filter(year > 1990)


## 16. Gallup data
gallup_merged <- gallupanalytics_export_20240515_104000 %>%
  select(-contains("demographic")) %>%
  left_join(gallupanalytics_export_20240515_104510 %>% select(-contains("demographic")), 
            by = c("geography", "time")) %>%
  left_join(gallupanalytics_export_20240515_104556 %>% select(-contains("demographic")),
            by = c("geography", "time")) %>%
  rename("country" = geography, 
         "year" = time, 
         "happy_with_env_preserv" = satisfied, 
         "unhappy_with_env_preserv" = dissatisfied) %>%
  mutate(iso3c = countrycode(country, "country.name.en", "iso3c"),
         year = as.numeric(as.character(year))) %>%
  select(country, iso3c, year, everything()) 



#################################
# Merging and saving data
#################################

# 1. CAPMF data and other 

## merge oecd with host of other data
oecd_merged <- capmf_official %>%
  left_join(wdi_post90, by = c("iso3c" = "iso3c_new", "time_period" = "year")) %>%
  left_join(corporatism_post90, 
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(corporatism_core_post90, 
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(cpds_post90,
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(bmr_post90, 
            by = c("iso3c" = "iso_code", "time_period" = "year")) %>%
  left_join(polcon_pruned,
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(fetzer_yotzo_post90_pruned,
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(kayser_comp_post90,
            by = c("iso3c" = "isocode", "time_period" = "elecyr")) %>%
  left_join(owid_energy_post90,
            by = c("iso3c" = "iso_code", "time_period" = "year")) %>%
  left_join(owid_co2_post90,
            by = c("iso3c" = "iso_code", "time_period" = "year")) %>%
  left_join(trade_union_post90,
            by = c("iso3c" = "location", "time_period" = "time")) %>%
  left_join(collective_bar_post90,
            by = c("iso3c" = "cou", "time_period" = "time")) %>%
  left_join(ictwsspost90, 
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(kayser_rehmert, 
            by = c("iso3c" = "iso3", "time_period" = "year")) %>%
  left_join(kayser_cip_green,
            by = c("iso3c", "time_period" = "year")) %>%
  left_join(gallup_merged, 
            by = c("iso3c", "time_period" = "year")) %>%
  zap_labels()

## Check merge worked 
nrow(oecd_merged) == nrow(capmf_official) ## should be TRUE

## remove some unncessary columns
vars_to_remove <- names(oecd_merged)[grepl(".x.x|.y.y", names(oecd_merged))]
oecd_merged <- oecd_merged %>% select(-vars_to_remove)


# create two separate datasets for adoption and stringency
oecd_stringency <- oecd_merged %>% filter(grepl("Policy stringency", measure_2))
oecd_adoption <- oecd_merged %>% filter(grepl("Adopted", measure_2))

## create separate datasets for different levels
## lists
oecd_adoption_list <- list()
oecd_stringency_list <- list()

## loop
for(k in c("LEV1", "LEV2", "LEV3", "LEV4")){
  # prune adoption data
  oecd_adoption_list[[k]] <- oecd_adoption %>%
    filter(grepl(paste0("^", k), clim_act_pol))
  # prune stringency data
  oecd_stringency_list[[k]] <- oecd_stringency %>%
    filter(grepl(paste0("^", k), clim_act_pol))
}


## create sector and instrument type FEs for LEV2 adoption and stringency datasets
create_sector_and_instrument_type <- function(df) {
  df %>% 
    mutate(
      sector = str_trim(str_extract(climate_actions_and_policies, "^[^-]+"), side = "both"),
      sector = ifelse(grepl("governance|targets|Fossil fuel|RD&D|International", sector), 
                       "Cross-sectoral and/or international", sector),
      instrument_type = str_extract(climate_actions_and_policies, "(?<=- ).*$"),
      instrument_type = str_replace_all(instrument_type, " instruments", ""),
      instrument_type = ifelse(grepl("governance|targets|Fossil fuel|RD&D|International", climate_actions_and_policies),
                               climate_actions_and_policies, instrument_type)
    )
}

# Apply the function to the LEV2 element in both lists
oecd_adoption_list[["LEV2"]] <- create_sector_and_instrument_type(oecd_adoption_list[["LEV2"]])
oecd_stringency_list[["LEV2"]] <- create_sector_and_instrument_type(oecd_stringency_list[["LEV2"]])

             
# 2. finnegan with cpds and corporatism 
finnegan_merged <- finnegan %>%
  left_join(cpds, 
            by = c("iso3c", "year")) %>%
  left_join(corporatismusall, 
            by = c("iso3c", "year")) %>%
  left_join(corporatismus_core,
            by = c("iso3c", "year")) %>%
  left_join(oecd_aias_ictwss_csv_v1_pruned, 
            by = c("iso3c", "year")) %>%
  left_join(gallup_merged, 
            by = c("iso3c", "year")) %>%
  left_join(kayser_cip_green,
            by = c("iso3c", "year")) %>%
  left_join(owid_co2_post90,
            by = c("iso3c" = "iso_code", "year")) 


# 3. save data

## Finnegan data
write_rds(finnegan_merged, paste0(here(), "/03 Cleaned data/Finnegan extended/finnegan_merged.rds"))


## CAMPF-merged data

## write function for simple saving 
save_data <- function(data, name){
  if(grepl("adoption", name)){
    file_path_csv <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Adoption/", name, ".csv")
    file_path_parquet <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Adoption/", name, ".parquet")
    file_path_rds <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Adoption/", name, ".rds")
  } else if(grepl("stringency", name)){
    file_path_csv <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Stringency/", name, ".csv")
    file_path_parquet <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Stringency/", name, ".parquet")
    file_path_rds <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Stringency/", name, ".rds")
  } else {
    file_path_csv <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Overall/", name, ".csv")
    file_path_parquet <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Overall/", name, ".parquet")
    file_path_rds <- paste0(here(), "/03 Cleaned data/OECD CAPMF/Overall/", name, ".rds")
  }
  
  # Save CSV only if data size is less than 100 MB
  data_size <- object.size(data)
  size_limit <- 104857600  # 100 MB in bytes
  
  if (data_size < size_limit) {
    write_csv(data, file_path_csv)
  } else {
    cat("CSV not saved for", name, "- file exceeds 100MB\n")
  }
  
  # Save other formats without size check
  write_parquet(data, file_path_parquet)
  saveRDS(data, file_path_rds)
}

## apply function to merged data
save_data(oecd_merged, "oecd_merged")

## save adoption and stringency data
map(c("adoption", "stringency"), function(x){
  if(grepl("adoption", x)){
    map(c("LEV1", "LEV2"), function(y){
      save_data(get(paste0("oecd_", x, "_list"))[[y]], paste0("oecd_", x, "_", y))
    })
  } else {
  map(c("LEV1", "LEV2", "LEV3", "LEV4"), function(y){
    save_data(get(paste0("oecd_", x, "_list"))[[y]], paste0("oecd_", x, "_", y))
  })
  }
})









