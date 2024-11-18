# Author: Donata Stonkute
#########################
# This code helps replicating Relative Index of Inequality (RII) estimates.
# The code shows an example for hearing loss (HL) and population 50+. If you are 
# interested in hearing aid (HA) use and/or a different subgroup, make sure to
# correspondingly change the outcome variable and analytical sub-sample in question. 
#########################


# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(dplyr)
library(spatstat.utils)
library(eurostat)
library(purrr)
library(countrycode)
library(boot)
library(dplyr, warn.conflicts = FALSE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Sources -----------------------------------------------------------------
# setwd("your_directory")
Sys.setenv(LANG = "en")


# Data --------------------------------------------------------------------
# euro_pop <- read.csv("your_directory/ESP2013.csv") 
# dta <- fread("your_directory/analytical_sample.csv") 
head(dta)

dtm <- dta %>% filter(gender=="Men")
dtf <- dta %>% filter(gender=="Women")

# Get a standard population 
## Remember to use age intervals according to the population sub-groups in question !!!
euro_pop <- euro_pop %>% 
  mutate(pop = ESP * 100000) %>% 
  filter(Age >= 50) %>% 
  mutate(total = sum(pop),
         ESP = pop/total, 
         ESP = ifelse(Age==85, revcumsum(ESP), ESP)) %>% 
  filter(Age <= 85) %>% 
  select(Age, ESP)

sum(euro_pop$ESP)

## If you are interested in age restriction, e.g., 50-64, follow the code below instead: !!!
# euro_pop <- euro_pop %>% 
#   mutate(pop = ESP * 100000) %>% 
#   filter(Age >= 50 & Age < 65) %>% 
#   mutate(total = sum(pop),
#          ESP = pop/total) %>% 
#   select(Age, ESP)
# 
# sum(euro_pop$ESP)


# Bootstrapping -----------------------------------------------------------

HL_ftion <- function(data, euro_pop, indices) {
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  
  d <- data[indices,]
  
  weighted_prevalence_r <- d %>%
    group_by(region, gender, edu, age_5) %>%
    summarise(
      # make sure you are looking at the outcome of interest and your denominator is correct
      cases = sum(HL),
      weighted_HL_count = sum(HL * wtresp, na.rm = TRUE),
      weighted_count = sum(wtresp, na.rm = TRUE),
    ) %>%
    mutate(weighted_prevalence = (weighted_HL_count / weighted_count)) %>% 
    ungroup() 


  ## Had you been interested replicating Supplementary Figure 1A, follow the following code instead: !!!
#  weighted_prevalence_r <- d %>%
#    group_by(region, gender, edu, age_5) %>%
#    mutate(HL_new = ifelse(hearaid == 1 | HL == 1, 1, 0)) %>%
#    summarise(
#    cases = sum(HL_new),
#    weighted_HL_count = sum(HL_new * wtresp, na.rm = TRUE),
#    weighted_count = sum(wtresp, na.rm = TRUE),
#  ) %>%
#  mutate(weighted_prevalence = (weighted_HL_count / weighted_count)) %>% 
#  ungroup()

  
  asp_r <- weighted_prevalence_r %>%
    separate(age_5, c("Age", "second"), sep = "-") %>% 
    mutate(Age = as.numeric(ifelse(Age == "85+", 85, Age))) %>% 
    select(-second) %>% 
    left_join(euro_pop, by = "Age") %>%
    group_by(region, gender, edu) %>%
    summarize(
      cases = sum(cases),
      age_std_prev = sum(weighted_prevalence * ESP*100)
    ) 
  
  d$edu <- factor(d$edu, levels = c("low", "medium", "high"), ordered = TRUE)
  
  dt_rii <- d %>% 
    select(region, gender, edu)
  
  sub_df <- dt_rii %>%
    group_by(region, gender, edu) %>%
    summarize(count = n())
  
  total_df <- sub_df %>%
    group_by(region, gender) %>%
    summarize(total_count = sum(count)) 
  
  edu_df <- sub_df %>%
    left_join(total_df, by = c("region", "gender")) %>%
    mutate(proportion = round(count / total_count, 2))
  
  cml_df <- edu_df %>%
    group_by(region, gender) %>%
    mutate(cml_prop = cumsum(proportion),
           midpoint = proportion/2)
  
  r_edu <- cml_df %>%
    group_by(region, gender) %>%
    mutate(r_edu = ifelse(edu == "low",
                          yes = midpoint, 
                          no = lag(cml_prop) + midpoint),
           edu = factor(edu, levels = c("low", "medium", "high"))) %>% 
    select(region, gender, edu, r_edu) 
  
  r_edu_2 <- r_edu %>% 
    pivot_wider(names_from = "edu", values_from = "r_edu") %>% 
    rename(r_low = "low", r_medium = "medium", r_high = "high")
  
  asp_r <- asp_r %>% 
    mutate(edu = factor(edu, levels = c("low", "medium", "high")))
  
  asp_r_2 <- asp_r %>% 
    pivot_wider(names_from = "edu", values_from = age_std_prev) %>% 
    rename(asp_low = "low", asp_medium = "medium", asp_high = "high")
  
  asp_rii <- merge(asp_r, r_edu, by.x = c("region", "gender", "edu")) 
  asp_rii_2 <- merge(asp_r_2, r_edu_2, by.x = c("region", "gender")) 
  

  regression_model <- lm(age_std_prev ~ r_edu, data = asp_rii)
  
  ranks <- seq(0,1, 0.1)
  predictions <- data.frame(r_edu = ranks)
  predictions$probabilities <- predict(regression_model, newdata = predictions, type = "response")
  
  pred_asp <- predictions %>% 
    rename(pred_asp = "probabilities")
  
  
  slopes_men <- pred_asp
  rii_men <- pred_asp %>% 
    filter(r_edu %in% c(0,1)) %>% 
    mutate(r_edu = case_when(
      r_edu == 0 ~ "low",
      r_edu == 1 ~ "high"
    )) %>% 
    pivot_wider(names_from = "r_edu", values_from = "pred_asp") %>% 
    mutate(RII = low/high,
           SII = low - high) 
  
  ## Change this to rii_men$SII if you want to obtain absolute inequalities (see sensitivity analyses) !!!
  results <- rii_men$RII 
  # print(results)

  return(results)
}

EE_m <- dta %>% filter(region == "Eastern Europe", gender == "Men")
NE_m <- dta %>% filter(region == "Northern Europe", gender == "Men")
SE_m <- dta %>% filter(region == "Southern Europe", gender == "Men")
WE_m <- dta %>% filter(region == "Western Europe", gender == "Men")

EE_f <- dta %>% filter(region == "Eastern Europe", gender == "Women")
NE_f <- dta %>% filter(region == "Northern Europe", gender == "Women")
SE_f <- dta %>% filter(region == "Southern Europe", gender == "Women")
WE_f <- dta %>% filter(region == "Western Europe", gender == "Women")


# EE_m --------------------------------------------------------------------

HL_ftion(data = EE_m, euro_pop = euro_pop)

HL_boots <- boot(data = EE_m, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_m$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Eastern Europe"
gender <- "man"

HL_RII_EE_m <- data.frame(region,
                     gender,
                     RII_estimate,
                     lower,
                     higher)
# NE_m --------------------------------------------------------------------

HL_ftion(data = NE_m, euro_pop = euro_pop)

HL_boots <- boot(data = NE_m, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_m$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Northern Europe"
gender <- "man"

HL_RII_NE_m <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)

# SE_m --------------------------------------------------------------------

HL_ftion(data = SE_m, euro_pop = euro_pop)

HL_boots <- boot(data = SE_m, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_m$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Southern Europe"
gender <- "man"

HL_RII_SE_m <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)


# WE_m --------------------------------------------------------------------

HL_ftion(data = WE_m, euro_pop = euro_pop)

HL_boots <- boot(data = WE_m, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_m$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Western Europe"
gender <- "man"

HL_RII_WE_m <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)


# EE_f --------------------------------------------------------------------

HL_ftion(data = EE_f, euro_pop = euro_pop)

EE_f[is.na(EE_f$cohort)]
# Filter out rows with NA values in the strata variable
EE_f_filtered <- EE_f[complete.cases(EE_f$cohort), ]

# Run bootstrapping with the filtered data
HL_boots <- boot(data = EE_f_filtered, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_f_filtered$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Eastern Europe"
gender <- "woman"

HL_RII_EE_f <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)

# NE_f --------------------------------------------------------------------

HL_ftion(data = NE_f, euro_pop = euro_pop)

NE_f[is.na(NE_f$cohort)]
# Filter out rows with NA values in the strata variable
NE_f_filtered <- NE_f[complete.cases(NE_f$cohort), ]

# Run bootstrapping with the filtered data
HL_boots <- boot(data = NE_f_filtered, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_f_filtered$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Northern Europe"
gender <- "woman"

HL_RII_NE_f <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)

# SE_f --------------------------------------------------------------------

HL_ftion(data = SE_f, euro_pop = euro_pop)

SE_f[is.na(SE_f$cohort)]
# Filter out rows with NA values in the strata variable
SE_f_filtered <- SE_f[complete.cases(SE_f$cohort), ]

# Run bootstrapping with the filtered data
HL_boots <- boot(data = SE_f_filtered, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_f_filtered$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Southern Europe"
gender <- "woman"

HL_RII_SE_f <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)

# WE_f --------------------------------------------------------------------

HL_ftion(data = WE_f, euro_pop = euro_pop)

WE_f[is.na(WE_f$cohort)]
# Filter out rows with NA values in the strata variable
WE_f_filtered <- WE_f[complete.cases(WE_f$cohort), ]

# Run bootstrapping with the filtered data
HL_boots <- boot(data = WE_f_filtered, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_f_filtered$cohort,
                 euro_pop = euro_pop)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "bca",
                    L = 250)

lower <- conf_int$bca[,4]
higher <- conf_int$bca[,5]
RII_estimate <- HL_boots$t0
region <- "Western Europe"
gender <- "woman"

HL_RII_WE_f <- data.frame(region,
                          gender,
                          RII_estimate,
                          lower,
                          higher)


result_df_HL_total <- bind_rows(
  HL_RII_EE_m,
  HL_RII_NE_m,
  HL_RII_SE_m,
  HL_RII_WE_m,
  HL_RII_EE_f,
  HL_RII_NE_f,
  HL_RII_SE_f,
  HL_RII_WE_f
)

# write.csv(result_df_HL_total, "output_your_directory.csv", row.names = FALSE)

