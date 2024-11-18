# Author: Donata Stonkute
#########################
# This code helps replicating Figures presented in the paper, both main text and Supplementary material
#########################


# libraries
library(tidyverse)
library(viridis)
library(cowplot)

# Figure 1 Age trends ------------------------------------------------------------

# required data
setwd("your_directory")

HL_dta <- read.csv("HL_analytical_sample.csv") 
HA_dta <- read.csv("HA_analytical_sample.csv") 


# hearing loss prevalence
wp_est_hl <- dta %>%
  group_by(edu, gender, age_5) %>%
  summarise(
    cases = sum(HL),
    weighted_HL_count = sum(HL * wtresp, na.rm = TRUE),
    weighted_count = sum(wtresp, na.rm = TRUE),
  ) %>%
  mutate(wtp = (weighted_HL_count / weighted_count)) %>%
  ungroup() %>%
  separate(age_5, c("Age", "second"), sep = "-") %>%
  mutate(Age = as.numeric(ifelse(Age == "85+", 85, Age)),
         edu = factor(edu, levels = c("low","medium","high")),
         measure = "Hearing loss") %>% 
  select(edu, gender, Age, wtp, measure)



# hearing aid use prevalence
wp_est_ha <- HA_dta %>% # make sure to use the right data set => the right denominator
  group_by(edu, gender, age_5) %>%
  summarise(
    cases = sum(hearaid),
    weighted_HA_cases = sum(hearaid * wtresp, na.rm = TRUE),
    weighted_count = sum(wtresp, na.rm = TRUE),
  ) %>%
  mutate(weighted_prevalence = (weighted_HA_cases / weighted_count)) %>%
  ungroup() %>%
  separate(age_5, c("Age", "second"), sep = "-") %>%
  mutate(
    Age = as.numeric(ifelse(Age == "85+", 85, Age)),
    edu = factor(edu, levels = c("low","medium","high")),
    measure = "Hearing aid use",
    wtp = weighted_prevalence
  ) %>%
  select(edu, gender, Age, wtp, measure)


# merge 
wp_reg <- full_join(wp_est_ha, wp_est_hl) %>% 
  mutate(measure = factor(measure, levels = c("Hearing loss", "Hearing aid use")),
         Age = ifelse(Age!=85, Age +2.5, Age))

# plotting
p1 <- wp_reg %>% filter(Age <85) %>% 
  ggplot(aes(x = Age, y = wtp*100, colour = edu)) + 
  geom_line(linewidth = 1) +
  facet_grid(measure~gender, scales = 'free') +
  labs(x = "Age",
       y = "Prevalence (%)",
       colour = "Education") + 
  theme_minimal() +
  scale_x_continuous(
    limits = c(50, 85),
    breaks = c(seq(50, 85, 5))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + # Adjust expand argument
  scale_color_viridis(discrete = TRUE, option = "D") +  
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom") +
  theme(panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(margin = margin(b = 20)))

# desired behaviour is to position labels using x and y of overall plot area, as per positioning of legend 
ggdraw(p1) + 
  draw_plot_label(x = c(.4500,.440), y = c(.95,.560), ## !!! Depends on the window ratio !!!
                            label =  c("Hearing loss",  
                                       "Hearing aid use"),
                            hjust = 0, size = 12) 

# saving
ggsave("your_directory/Figure_1.svg", dpi = 350, width = 170, units = "mm")
  

# Supplement: Figure 1A. Comparing Fitted Trends in Educational Inequality ------------

# required data 
## these are slopes of original RII
slopes_old <- read.csv("slopes_original.csv") 
## these are slopes of RII based on recoded HL outcome
slopes_new <- read.csv("slopes_recoded.csv")


slopes_old <- slopes_old %>% 
  mutate(Sample = "Hearing quality")

slopes_new <- slopes_new %>% 
  mutate(Sample = "Hearing quality + Hearing aid use")

slopes <- rbind(slopes_old, slopes_new)


ggplot() +
  geom_line(
    data = slopes_old %>% mutate(region = factor(
      region,
      levels = c(
        "Eastern Europe",
        "Southern Europe",
        "Western Europe",
        "Northern Europe"
      )
    )),
    aes(
      x = r_edu,
      y = pred_asp * 100,
      colour = region,
      linetype = Sample
    ),
    size = 1.5
  ) +
  geom_line(
    data = slopes_new %>% mutate(region = factor(
      region,
      levels = c(
        "Eastern Europe",
        "Southern Europe",
        "Western Europe",
        "Northern Europe"
      )
    )),
    aes(
      x = r_edu,
      y = pred_asp * 100,
      colour = region,
      linetype = Sample
    ),
    size = 1.5,
    alpha = 0.5
  ) +
  facet_grid(. ~ gender) +
  labs(x = "Ranked Education",
       y = "Age-Standardized \nPrevalence",
       colour = "Region") +
  theme_minimal() +
  scale_colour_viridis(discrete = TRUE, option = "D")  +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin()) +
  guides(colour = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) 

# saving        
ggsave("your_directory/Figure_1A.svg", dpi = 350, width = 170, height = 150, units = "mm", bg = "white")


# Supplement: Figure 2A. Age-Restricted Analysis of Hearing Loss Inequalities ------------

# required data
## these are RII estimates of for open-ended age category 65+
dta_65plus <- read.csv("hl_rii_65+.csv")
## these are RII estimates of age interval [65-80)
dta_65_80 <- read.csv("sensitivity_age_censoring.csv")


dta_65to80 <- dta_65_80 %>% 
  mutate(Age = "[65,80)")

dta_65plus <- dta_65plus %>% 
  mutate(Age = "65+")

compare_age_cens <- rbind(dta_65to80, dta_65plus)

compare_age_cens <- compare_age_cens %>% 
  mutate(Age = factor(Age, levels = c("[65,80)", "65+")))

compare_age_cens %>% 
  ggplot(aes(x = region, y = as.numeric(RII_estimate))) + 
  geom_point(aes(colour = Age), data = subset(compare_age_cens, Age == "65+"),
             size = 3.5) +
  geom_errorbar(aes(ymin = lower, ymax = higher, colour = Age),
                data = subset(compare_age_cens, Age == "65+"),
                width = 0.2) +  
  geom_point(aes(colour = Age), data = subset(compare_age_cens, Age == "[65,80)"), 
             size = 3) +  
  geom_errorbar(aes(ymin = lower, ymax = higher, colour = Age),
                data = subset(compare_age_cens, Age == "[65,80)"),
                width = 0.2) + 
  facet_grid(.~gender) +
  labs(x = NULL, 
       y = "Relative Index of Inequality",
       colour = "Age groups:") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # Split region labels into two lines
  scale_colour_manual(values = c("[65,80)" = "navyblue", "65+" = "#D2042D")) +  # Manually set colours
  theme_minimal()

# saving
ggsave("your_directory/Figure_2A.svg", dpi = 350, width = 170, units = "mm")




