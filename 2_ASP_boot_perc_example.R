# Author: Donata Stonkute
#########################
# This code helps replicating Age-standardized prevalence (ASP) estimates.
# The code shows an example for hearing loss (HL) and population 50+. If you are 
# interested in hearing aid (HA) use and/or a different subgroup, make sure to
# correspondingly change the outcome variable and analytical sub-sample in question. 
# Useful comments regarding HA use are provided.
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

## Had you been interested in hearing aid (HA) use prevalence, it is important to think about the denominator.
## In the case of this study, it is not the whole population.
## We consider HA use among 1) HA users; 2) those who self-report HL.
## Follow the code below instead if you are estimating HA use prevalence, and change labels later accordingly.

# dta_aid <- dta %>% 
#   mutate(HL_new = ifelse(hearaid == 1 | HL == 1, 1, 0)) %>% 
#   filter(HL_new == 1) %>% 
#   select(-HL_new)

dtm <- dta %>% filter(gender=="Men")
dtf <- dta %>% filter(gender=="Women")

# Get a standard population 
## remember to use age intervals according to population sub-groups in question
euro_pop <- euro_pop %>% 
  mutate(pop = ESP * 100000) %>% 
  filter(Age >= 50) %>% 
  mutate(total = sum(pop),
         ESP = pop/total, 
         ESP = ifelse(Age==85, revcumsum(ESP), ESP)) %>% 
  filter(Age <= 85) %>% 
  select(Age, ESP)

sum(euro_pop$ESP)

# Bootstrapping -----------------------------------------------------------

HL_ftion <- function(data, euro_pop, indices) {
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  
  d <- data[indices,]
  
  weighted_prevalence_r <- d %>%
    group_by(region, gender, edu, age_5) %>%
    summarise(
      # make sure you are looking at the outcome of interest and your denominator is correct
      cases = sum(HL, na.rm = TRUE),
      weighted_cases = sum(HL * wtresp, na.rm = TRUE),
      weighted_count = sum(wtresp, na.rm = TRUE),
    ) %>%
    mutate(weighted_prevalence = (weighted_cases / weighted_count)) %>% 
    ungroup() 

  
  asp_r <- weighted_prevalence_r %>%
    separate(age_5, c("Age", "second"), sep = "-") %>% 
    mutate(Age = as.numeric(ifelse(Age == "85+", 85, Age))) %>% 
    select(-second) %>% 
    left_join(euro_pop, by = "Age") %>%
    group_by(region, gender, edu) %>%
    summarize(
      age_std_prev = sum(weighted_prevalence * ESP)
    ) %>% 
    mutate(edu = factor(edu, levels = c("low", "medium", "high")))

  
  results <- asp_r$age_std_prev
  # print(results)

  return(results)
}

EE_m_low <- dta %>% filter(region == "Eastern Europe", gender == "Men", edu == "low")
EE_m_mid <- dta %>% filter(region == "Eastern Europe", gender == "Men", edu == "medium")
EE_m_high <- dta %>% filter(region == "Eastern Europe", gender == "Men", edu == "high")

NE_m_low <- dta %>% filter(region == "Northern Europe", gender == "Men", edu == "low")
NE_m_mid <- dta %>% filter(region == "Northern Europe", gender == "Men", edu == "medium")
NE_m_high <- dta %>% filter(region == "Northern Europe", gender == "Men", edu == "high")

SE_m_low <- dta %>% filter(region == "Southern Europe", gender == "Men", edu == "low")
SE_m_mid <- dta %>% filter(region == "Southern Europe", gender == "Men", edu == "medium")
SE_m_high <- dta %>% filter(region == "Southern Europe", gender == "Men", edu == "high")

WE_m_low <- dta %>% filter(region == "Western Europe", gender == "Men", edu == "low")
WE_m_mid <- dta %>% filter(region == "Western Europe", gender == "Men", edu == "medium")
WE_m_high <- dta %>% filter(region == "Western Europe", gender == "Men", edu == "high")


EE_f_low <- dta %>% filter(region == "Eastern Europe", gender == "Women", edu == "low")
EE_f_mid <- dta %>% filter(region == "Eastern Europe", gender == "Women", edu == "medium")
EE_f_high <- dta %>% filter(region == "Eastern Europe", gender == "Women", edu == "high")

NE_f_low <- dta %>% filter(region == "Northern Europe", gender == "Women", edu == "low")
NE_f_mid <- dta %>% filter(region == "Northern Europe", gender == "Women", edu == "medium")
NE_f_high <- dta %>% filter(region == "Northern Europe", gender == "Women", edu == "high")

SE_f_low <- dta %>% filter(region == "Southern Europe", gender == "Women", edu == "low")
SE_f_mid <- dta %>% filter(region == "Southern Europe", gender == "Women", edu == "medium")
SE_f_high <- dta %>% filter(region == "Southern Europe", gender == "Women", edu == "high")

WE_f_low <- dta %>% filter(region == "Western Europe", gender == "Women", edu == "low")
WE_f_mid <- dta %>% filter(region == "Western Europe", gender == "Women", edu == "medium")
WE_f_high <- dta %>% filter(region == "Western Europe", gender == "Women", edu == "high")



set.seed(1234)

# EE_m_low --------------------------------------------------------------------

HL_ftion(data = EE_m_low, euro_pop = euro_pop)

HL_boots <- boot(data = EE_m_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_m_low$cohort,
                 euro_pop = euro_pop,
                 # make sure your servers can carry parallel computing, otherwise skip
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Eastern Europe"
gender <- "man"
edu <- "low"

asp_EE_m_low <- data.frame(region,
                     gender,
                     edu,
                     asp,
                     lower,
                     higher)

# EE_m_mid --------------------------------------------------------------------

HL_ftion(data = EE_m_mid, euro_pop = euro_pop)

HL_boots <- boot(data = EE_m_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_m_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Eastern Europe"
gender <- "man"
edu <- "medium"

asp_EE_m_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# EE_m_high --------------------------------------------------------------------

HL_ftion(data = EE_m_high, euro_pop = euro_pop)

HL_boots <- boot(data = EE_m_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_m_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Eastern Europe"
gender <- "man"
edu <- "high"

asp_EE_m_high <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)


# NE_m_low --------------------------------------------------------------------

HL_ftion(data = NE_m_low, euro_pop = euro_pop)

HL_boots <- boot(data = NE_m_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_m_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Northern Europe"
gender <- "man"
edu <- "low"

asp_NE_m_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# NE_m_mid --------------------------------------------------------------------

HL_ftion(data = NE_m_mid, euro_pop = euro_pop)

HL_boots <- boot(data = NE_m_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_m_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Northern Europe"
gender <- "man"
edu <- "medium"

asp_NE_m_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# NE_m_high --------------------------------------------------------------------

HL_ftion(data = NE_m_high, euro_pop = euro_pop)

HL_boots <- boot(data = NE_m_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_m_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)

conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Northern Europe"
gender <- "man"
edu <- "high"

asp_NE_m_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)


# SE_m_low --------------------------------------------------------------------

HL_ftion(data = SE_m_low, euro_pop = euro_pop)

HL_boots <- boot(data = SE_m_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_m_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Southern Europe"
gender <- "man"
edu <- "low"

asp_SE_m_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# SE_m_mid --------------------------------------------------------------------

HL_ftion(data = SE_m_mid, euro_pop = euro_pop)

HL_boots <- boot(data = SE_m_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_m_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Southern Europe"
gender <- "man"
edu <- "medium"

asp_SE_m_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# SE_m_high --------------------------------------------------------------------

HL_ftion(data = SE_m_high, euro_pop = euro_pop)

HL_boots <- boot(data = SE_m_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_m_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Southern Europe"
gender <- "man"
edu <- "high"

asp_SE_m_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)


# WE_m_low --------------------------------------------------------------------

HL_ftion(data = WE_m_low, euro_pop = euro_pop)

HL_boots <- boot(data = WE_m_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_m_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Western Europe"
gender <- "man"
edu <- "low"

asp_WE_m_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# WE_m_mid --------------------------------------------------------------------

HL_ftion(data = WE_m_mid, euro_pop = euro_pop)

HL_boots <- boot(data = WE_m_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_m_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Western Europe"
gender <- "man"
edu <- "medium"

asp_WE_m_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# WE_m_high --------------------------------------------------------------------

HL_ftion(data = WE_m_high, euro_pop = euro_pop)

HL_boots <- boot(data = WE_m_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_m_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Western Europe"
gender <- "man"
edu <- "high"

asp_WE_m_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)

# EE_f_low --------------------------------------------------------------------

HL_ftion(data = EE_f_low, euro_pop = euro_pop)

HL_boots <- boot(data = EE_f_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_f_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Eastern Europe"
gender <- "woman"
edu <- "low"

asp_EE_f_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# EE_f_mid --------------------------------------------------------------------

HL_ftion(data = EE_f_mid, euro_pop = euro_pop)

HL_boots <- boot(data = EE_f_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_f_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Eastern Europe"
gender <- "woman"
edu <- "medium"

asp_EE_f_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# EE_f_high --------------------------------------------------------------------

HL_ftion(data = EE_f_high, euro_pop = euro_pop)

HL_boots <- boot(data = EE_f_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = EE_f_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Eastern Europe"
gender <- "woman"
edu <- "high"

asp_EE_f_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)


# NE_f_low --------------------------------------------------------------------

HL_ftion(data = NE_f_low, euro_pop = euro_pop)

HL_boots <- boot(data = NE_f_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_f_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Northern Europe"
gender <- "woman"
edu <- "low"

asp_NE_f_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# NE_f_mid --------------------------------------------------------------------

HL_ftion(data = NE_f_mid, euro_pop = euro_pop)

HL_boots <- boot(data = NE_f_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_f_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Northern Europe"
gender <- "woman"
edu <- "medium"

asp_NE_f_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# NE_f_high --------------------------------------------------------------------

HL_ftion(data = NE_f_high, euro_pop = euro_pop)

HL_boots <- boot(data = NE_f_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = NE_f_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Northern Europe"
gender <- "woman"
edu <- "high"

asp_NE_f_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)


# SE_f_low --------------------------------------------------------------------

HL_ftion(data = SE_f_low, euro_pop = euro_pop)

HL_boots <- boot(data = SE_f_low, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_f_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Southern Europe"
gender <- "woman"
edu <- "low"

asp_SE_f_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# SE_f_mid --------------------------------------------------------------------

HL_ftion(data = SE_f_mid, euro_pop = euro_pop)

HL_boots <- boot(data = SE_f_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_f_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Southern Europe"
gender <- "woman"
edu <- "medium"

asp_SE_f_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# SE_f_high --------------------------------------------------------------------

HL_ftion(data = SE_f_high, euro_pop = euro_pop)

HL_boots <- boot(data = SE_f_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = SE_f_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Southern Europe"
gender <- "woman"
edu <- "high"

asp_SE_f_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)


# WE_f_low --------------------------------------------------------------------

HL_ftion(data = WE_f_low, euro_pop = euro_pop)

HL_boots <- boot(data = WE_f_low, 
                 statistic = HL_ftion,  
                 R = 10000, 
                 strata = WE_f_low$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Western Europe"
gender <- "woman"
edu <- "low"

asp_WE_f_low <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# WE_f_mid --------------------------------------------------------------------

HL_ftion(data = WE_f_mid, euro_pop = euro_pop)

HL_boots <- boot(data = WE_f_mid, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_f_mid$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Western Europe"
gender <- "woman"
edu <- "medium"

asp_WE_f_mid <- data.frame(region,
                           gender,
                           edu,
                           asp,
                           lower,
                           higher)

# WE_f_high --------------------------------------------------------------------

HL_ftion(data = WE_f_high, euro_pop = euro_pop)

HL_boots <- boot(data = WE_f_high, 
                 statistic = HL_ftion,  
                 R = 1000, 
                 strata = WE_f_high$cohort,
                 euro_pop = euro_pop,
                 parallel = "multicore", 
                 ncpus = 10)


conf_int <- boot.ci(HL_boots,
                    conf = 0.95,
                    type = "perc")

lower <- conf_int$perc[,4]
higher <- conf_int$perc[,5]
asp <- HL_boots$t0
region <- "Western Europe"
gender <- "woman"
edu <- "high"

asp_WE_f_high <- data.frame(region,
                            gender,
                            edu,
                            asp,
                            lower,
                            higher)


# Bind --------------------------------------------------------------------

result_df_nl <- bind_rows(
  asp_EE_m_low,
  asp_EE_m_mid,
  asp_EE_m_high,
  asp_NE_m_low,
  asp_NE_m_mid,
  asp_NE_m_high,
  asp_SE_m_low,
  asp_SE_m_mid,
  asp_SE_m_high,
  asp_WE_m_low,
  asp_WE_m_mid,
  asp_WE_m_high,
  asp_EE_f_low,
  asp_EE_f_mid,
  asp_EE_f_high,
  asp_NE_f_low,
  asp_NE_f_mid,
  asp_NE_f_high,
  asp_SE_f_low,
  asp_SE_f_mid,
  asp_SE_f_high,
  asp_WE_f_low,
  asp_WE_f_mid,
  asp_WE_f_high
)

# write.csv(result_df_nl, "output_your_directory.csv", row.names = FALSE)

