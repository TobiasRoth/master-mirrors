rm(list=ls(all=TRUE))

#------------------------------------------------------------------------------
# Settings
#------------------------------------------------------------------------------
# Libraries
library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------
# Load and prepare data
#------------------------------------------------------------------------------
# Load data
d <- read_excel("Data/Data_combined.xlsx") 

# Correct data
d$Object_Type[d$Object_Type == "blank" & !is.na(d$Object_Type)] <- "Blank"

# Aggregate data
dat <- d %>% 
  group_by(Day, Species) %>% 
  summarise(n_obs_tot = n(),
            Activity = sum(Interaction == "Y"),
            Blank = sum(Object_Type == "Blank", na.rm = TRUE),
            Human = sum(Object_Type == "Human", na.rm = TRUE),
            Mirror = sum(Object_Type == "Mirror", na.rm = TRUE),
            Monkey = sum(Object_Type == "Monkey", na.rm = TRUE)) %>%
  gather("Device", "nobs", Blank, Human, Mirror, Monkey)

#------------------------------------------------------------------------------
# Some descriptive statistics
#------------------------------------------------------------------------------
n_distinct(dat$Species)


#------------------------------------------------------------------------------
# Make a figure / test per species 
#------------------------------------------------------------------------------
spec = "Ateles"
spec = "Macaca"

ggplot(dat %>% filter(Species == spec), aes(x = Day, y = nobs, col = Device)) +
  geom_line() +
  geom_point() + 
  labs(y = "Proportion of times the device was used",
       title = paste(spec))
  
#------------------------------------------------------------------------------
# Test per species
#------------------------------------------------------------------------------
mod1 <- glm(cbind(nobs, n_obs_tot-nobs) ~ Device * Day, family = binomial,
    data = dat %>% filter(Species == spec)) 
mod2 <- glm(cbind(nobs, n_obs_tot-nobs) ~ Device + Day, family = binomial,
            data = dat %>% filter(Species == spec)) 
anova(mod1, mod2, test = "Chisq")
summary(mod2)
