# Author: Donata Stonkute
#########################
# This analysis uses data or information from the Harmonized Survey of Health, 
# Ageing and Retirement in Europe dataset and Codebook, Version F as of February 
# 2023 developed by the Gateway to Global Aging Data. The development of the 
# Harmonized Survey of Health, Ageing and Retirement in Europe was funded by the National Institute on Aging 
# (R01 AG030153, RC2 AG036619, 1R03AG043052). For more information, please refer to www.g2aging.org
#########################

Sys.setenv(LANG = "en")
library(tidyverse)
library(data.table)
library(labelled)
library(haven)
# Set working directory
# setwd("your_directory")

# Read the Stata file 
## For prior steps in preparation of the data, please go to Stata file "w1-8_keep.do"

# --- mydt_wide <- read_dta("add_address_here.dta") --- #

# I create this for easier identification of column numbers for variables of interest
col.names <- data.frame(colnames(mydt_wide))


# save variables that time-constant (gender, education, country, etc.)
constant_vars <- mydt_wide[,c(1,2,3,26,34,35)]

# Age ---------------------------------------------------------------------
# This function helps to identify which columns contain a character string of interest
which(grepl("agey", col.names$colnames.mydt_wide.))

# We retrieve those next to the unique merge ID 
mydt_age <- mydt_wide[,c(1, 27:33)]

# And convert sub-data from wide to long format
mydt_age <- mydt_age %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "age",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, age)

# Next, we add the converted sub-data to the constant variables 
mydt_long <- merge(constant_vars, mydt_age, by="mergeid", all=TRUE)

remove(constant_vars)  
remove(mydt_age)

# Now we will repeat the same steps for all variables of interest


# Interview status -----------------------------------------------------
which(grepl("iwstat", col.names$colnames.mydt_wide.))

mydt_iwstat <- mydt_wide[,c(1, 4:11)]

mydt_iwstat <- mydt_iwstat %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "iwstat",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, iwstat)

mydt_long <- merge(mydt_long, mydt_iwstat, by=c("mergeid","wave"), all=TRUE)

remove(mydt_iwstat)

# Person-level analysis weights -------------------------------------------
which(grepl("wtresp", col.names$colnames.mydt_wide.))
mydt_tresp <- mydt_wide [,c(1, 12:18)]

mydt_tresp <- mydt_tresp %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "wtresp",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, wtresp)

mydt_long <- merge(mydt_long, mydt_tresp, by=c("mergeid","wave"), all=TRUE)

remove(mydt_tresp)


# Proxy -------------------------------------------
which(grepl("proxy", col.names$colnames.mydt_wide.))
mydt_proxy <- mydt_wide [,c(1, 19:25)]

mydt_proxy <- mydt_proxy %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "proxy",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, proxy)

mydt_long <- merge(mydt_long, mydt_proxy, by=c("mergeid","wave"), all=TRUE)

remove(mydt_proxy)

# SR Hearing Quality -------------------------------------------
which(grepl("hearing", col.names$colnames.mydt_wide.))
mydt_hear <- mydt_wide [,c(1, 44:50)]

mydt_hear <- mydt_hear %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "hearing",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, hearing)

mydt_long <- merge(mydt_long, mydt_hear, by=c("mergeid","wave"), all=TRUE)

remove(mydt_hear)


# Hearing Aid use -------------------------------------------
which(grepl("hearaid", col.names$colnames.mydt_wide.))
mydt_hearaid <- mydt_wide [,c(1, 51:57)]

mydt_hearaid <- mydt_hearaid %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "hearaid",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, hearaid)

mydt_long <- merge(mydt_long, mydt_hearaid, by=c("mergeid","wave"), all=TRUE)

remove(mydt_hearaid)


# Hearing aid ownership ---------------------------------------------------

## Adding a variable available only in waves 7+, non-harmonized data. 
## Please check prior steps in Stata file "w8_keep.do"
## Non-harmonized data files can be accessed from https://share-eric.eu/data/

w8 <- read_dta("U:/A2_Hearing/Data/w8_keep.dta")

w8 <- rename(w8, own_aid = ph745_) 

w8$wave <- 8

mydt_long$wave <- as.numeric(mydt_long$wave)

mydt_long <- full_join(mydt_long, w8, by = c("mergeid", "wave"))



# Cosmetics ---------------------------------------------------------------

## Sorting
mydt_long <- mydt_long[order(mydt_long$mergeid, mydt_long$wave),]

mydt_long$country <-    recode(
  mydt_long$country,
  "11" = "Austria",
  "12" = "Germany",
  "13" = "Sweden",
  "14" = "Netherlands",
  "15" = "Spain",
  "16" = "Italy",
  "17" = "France",
  "18" = "Denmark",
  "19" = "Greece",
  "20" = "Switzerland",
  "23" = "Belgium",
  "25" = "Israel",
  "28" = "Czechia",
  "29" = "Poland",
  "30" = "Ireland",
  "31" = "Luxembourg",
  "32" = "Hungary",
  "33" = "Portugal",
  "34" = "Slovenia",
  "35" = "Estonia",
  "47" = "Croatia",
  "48" = "Lithuania",
  "51" = "Bulgaria",
  "53" = "Cyprus",
  "55" = "Finland",
  "57" = "Latvia",
  "59" = "Malta",
  "61" = "Romania",
  "63" = "Slovakia"
)

## Exclude Israel 
mydt_long_2 <- mydt_long %>% 
  filter(country!="Israel")

## exclude waves 3 and 7
dt_present <- mydt_long_2 %>% 
  filter(wave != 3, wave !=7)
nrow(dt_present_2)

# excluding not yet identified (=0), alive but did not respond (=4), 
# died between the previous and the current wave (=5),
# died before last week (=6), not known (=9) 

## with drop-outs
dt_present_2 <- dt_present %>% 
  filter(iwstat %in% c(1,4)) 
nrow(dt_present)

## drop-outs excluded
dt_present_3 <- dt_present_2 %>% 
  filter(iwstat == 1)
nrow(dt_present_3)


# Omitting missing values -------------------------------------------------

dt_50_above <- dt_present_3 %>% 
  filter(age>=50)

dt_miss_age <- dt_50_above %>% 
  filter(!is.na(age))

dt_miss_hearing <- dt_miss_age %>%
  filter(!is.na(hearing)) 

         
dt_miss_aids <- dt_miss_hearing  %>%
  filter(!is.na(hearaid)) 

dt_miss_edu <- dt_miss_aids  %>%
  filter(!is.na(raeducl)) 
dt_miss_country <- dt_miss_aids  %>%
  filter(!is.na(country)) 
dt_miss_gender <- dt_miss_aids  %>%
  filter(!is.na(ragender)) 

dt_miss_wt <- dt_miss_gender  %>%
  filter(!is.na(wtresp))

dt_complete <- dt_miss_wt

# Renaming ----------------------------------------------------------------

unique(dt_complete$ragender)
unique(dt_complete$raeducl)

dt_analytics <- dt_complete %>%
  mutate(
    gender = ifelse(ragender == 1, "man", "woman"),
    edu = case_when(raeducl == 1 ~ "low",
                    raeducl == 2 ~ "medium",
                    raeducl == 3 ~ "high")
  ) %>% 
  rename(birth_y = "rabyear") %>% 
  select(mergeid, country, wave, wtresp, proxy, gender, edu, birth_y, age, hearing, hearaid, own_aid)



# Construct variables -----------------------------------------------------

# Define a function to map countries to regions
get_european_region <- function(country) {
  region_mapping <- list(
    "Western Europe" = c(
      "Austria",
      "Belgium",
      "France",
      "Germany",
      "Luxembourg",
      "Netherlands",
      "Switzerland", 
      "Ireland"
    ),
    
    "Southern Europe" = c("Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain"),
    
    "Northern Europe" = c("Denmark", "Finland", "Sweden"),
    
    "Eastern Europe" = c(
      "Bulgaria",
      "Croatia",
      "Czechia",
      "Estonia",
      "Hungary",
      "Latvia",
      "Lithuania",
      "Poland",
      "Romania",
      "Slovakia",
      "Slovenia"
    )
  )
  
  for (region in names(region_mapping)) {
    if (country %in% region_mapping[[region]]) {
      return(region)
    }
  }
  
  return("Other")
}



# Add a new column for the European region
groups_5 <- c(paste(seq(50, 80, by = 5), seq(54, 84, by = 5),sep="-"),paste(85, "+", sep = ""))
groups_10 <- c(paste(seq(50, 70, by = 10), seq(59, 79, by = 10),sep="-"),paste(80, "+", sep = ""))

head(dt_analytics, 15)
min(dt_analytics$birth_y)
max(dt_analytics$birth_y)

c_gr <- seq(1,7,1) # this will be needed to identify cohort - for bootstrap resampling 

dta_r <- dt_analytics %>%
  mutate(region = map_chr(country, get_european_region),
         HL = ifelse(hearing > 3, 1, 0), # hearing less than good
         own_aid = ifelse(own_aid == 1, 1, 0),
         edu = factor(edu, levels = c("low", "medium", "high")),
         gender = factor(gender, levels=c("man","woman"), 
                         labels = c("Men", "Women")),
         age_5 = cut(
           age,
           breaks = c(seq(50, 85, by = 5), 120),
           labels = groups_5,
           right = FALSE),
         age_10 = cut(
           age,
           breaks = c(seq(50, 80, by = 10), 120),
           labels = groups_10,
           right = FALSE),
         cohort = cut(
           birth_y,
           breaks = c(seq(1900, 1970, by = 10)),
           labels = c_gr,
           right = TRUE))


dta_r %>%  filter(is.na(cohort))
table(dta_r$birth_y, dta_r$cohort)
table(dta_r$hearing, dta_r$HL)
table(dta_r$country, dta_r$region)


## Exporting
# --- write.csv(dta_r, file = "your_data_address.csv", row.names = FALSE) --- #



