# Dante Torio
# Code:
# Processing eelgrass sediment core data
#
#########################################

# Clear R Environment
rm(list = ls())

# Part 1: Plot 210Pb readings and dry bulk density with depth
## Load required libraries
library(readr)
library(ggplot2)
library(tidyr)

## Load dataset
Core1 <- read_csv("Core1.csv")
## display column names

head(Core1)

## Plot using ggplot 
ggplot(Core1, aes(x = depth_cm, y = Pb210_Bqkg)) +
  geom_point() +
  geom_line(color="blue",size=1, linetype=1)+
  scale_x_reverse()+
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(
    aes(
      ymin = Pb210_Bqkg - sd_210Pb, 
      ymax = Pb210_Bqkg + sd_210Pb)
  )+
  labs(title = "Density by depth",
       x="Depth(cm)",
       y = "Pb210 (Bq/kg)")+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()


Core1 %>%
  ggplot(aes(depth_cm, density_g_cm3))+
  geom_point(size=3)+
  geom_smooth()+
  scale_x_reverse()+
  scale_y_continuous(position="right")+
  coord_flip()+
  labs(title = "Density by depth",
       x="Depth(cm)",
       y = "Density g/cm^3")+
  theme(plot.title = element_text(hjust = 0.5))
  
## References, https://stackoverflow.com/questions
# /19754365/plotting-a-depth-profile-with-ggplot

#-------------------------------------------------- 

# Part 2.  Estimate age using rPlum

## 1. Core 1: Marginal eelgrass beds
# Install latest rplum from github
# install.packages('devtools')
remotes::install_github("Maarten14C/IntCal", force=TRUE)
remotes::install_github("Maarten14C/rbacon")
remotes::install_github("Maarten14C/rplum")

#remove.packages("IntCal")
#remove.packages("rbacon")
#emove.packages("rplum")

# Create a sub-directory called Plum_runs and a sub directory called Core1 from within
# Save a copy of the core1 data following the recommended column naming convention

# core1_plum <- core1 %>%
#  transmute(
#   'Depth' = depthCm,
#   '210Pb' = activityPb210,
#   'sd'    = sdPb210,
#    'Thickness' = thicknessCm3
#  )

# dir.create("Plum_runs/Core1", recursive = TRUE)
# write.csv(c01Data, "Plum_runs/Core1/core1_plum.csv")

# Clear R Environment
rm(list = ls())

# Analyse the age of core 1
library(rplum)

# Run core with 210Pb measurements formatted as suggested 
# Here I'm running all 13 measurements with no supplementary data, (ra.case=0)
# Using last 3 readings as background value (na.supp=3)
# year is equal to 2023
# all hard coded in the settings column in the core_1.csv file

# Initial run
Plum('Core1_0')

# Clean environment
rm(list = ls())
agedepth()
dev.off()

# Running plum with refined (different) prior distribution
Plum('Core1_0', acc.mean = 3, acc.shape = 1.5, mem.mean = 0.5)
#Plum('Core1_0', acc.mean = 5, acc.shape = 1.5, mem.mean = 0.3)
#Plum('Core1_0', mem.mean = 0.1, mem.strength = 10)


# Running Plum when you have both 210Pb data and radiocarbon dating data
# Radiocarbon data is formatted following Bacon as save as csv(e.g., core1_c.csv)
# save the radiocarbon data on the same folder as core1.csv

# Plum('Core1_0', thick=5, otherdates = 'core1_0_c.csv')

#Accumulation rates
## To simulate a clean start we clean the r environment
rm(list = ls())
agedepth()
dev.off()



### Plotting accumulation rates for the whole model
# by depth

accrate.depth.ghost(cmyr = T, acc.lim = c(0, 2))
accrate.depth.ghost(cmyr = T, acc.lim = c(0,2), d.lim = c(0,20))

# by age

accrate.age.ghost(cmyr = T, acc.lim = c(0,2))
accrate.age.ghost(cmyr = T, age.lim = c(-73, 40), acc.lim = c(0, 1.0))

## Plotting accumulation rates for specific depth
accrate_d0 <- accrate.depth(d=0, cmyr = TRUE)
summary(accrate_d0)
head(accrate_d0)
mean(accrate_d0);var(accrate_d0)
plot(density(accrate_d0), main = "Accumulation rate at 0 cm depth yr/cm", 
     xlab ='yr/cm',
     ylab='')

accrate_d19 <- accrate.depth(d=19, cmyr = TRUE)
head(accrate_d19)
summary(accrate_d19)
mean(accrate_d19);var(accrate_d19)
plot(density(accrate_d19), main = "Accumulation rate at 19 cm depth cm/yr ", 
     xlab ='cm/yr',
     ylab='')



## Plotting accumulation rate by age
accrate22 <- accrate.age(age =-22.4, cmyr = TRUE) # -22.4 BP is equivalent to April 1972
summary(accrate22)
mean(accrate22);var(accrate22)
plot(density(accrate22), main = 'Accumulation rate at age -22.5 BP or April 1972',
     xlab = 'yr/cm',
     ylab = '')

acc_rate_21 <- accrate.age(age = -21.5, cmyr = TRUE)
summary(acc_rate_21)
mean(acc_rate_21);var(acc_rate_21)
plot(density(acc_rate_21), main = 'Accumulation rate (cm/yr) at age -21.5 BP or 1971',
     xlab = 'cm/yr',
     ylab = '')


## 2. Core 4: Reference eelgrass beds

# Analyse the age of core 1
library(rplum)

# Run core with 210Pb measurements formatted as suggested 
# Here I'm running all 13 measurements with no supplementary data, (ra.case=0)
# Using last 3 readings as background value (na.supp=3)
# year is equal to 2023
# all hard coded in the settings column in the core_1.csv file

# Initial run
Plum('Core2_0')

# Running plum with refined (different) prior distribution
Plum('Core2_0', acc.mean = 3.0, acc.shape = 1.5, mem.mean = 0.5)


agedepth()
accrate.depth.ghost(cmyr = T, acc.lim = c(0, 0.5))
accrate.age.ghost(cmyr = T, acc.lim = c(0,.5))

## Plotting accumulation rates for specific depth
accrate_d0 <- accrate.depth(d=0, cmyr = TRUE)
summary(accrate_d0)
head(accrate_d0)
mean(accrate_d0);var(accrate_d0)
plot(density(accrate_d0), main = "Accumulation rate at 0 cm depth cm/yr", 
     xlab ='yr/cm',
     ylab='')

accrate_d17 <- accrate.depth(d=17, cmyr = TRUE)
summary(accrate_d17)
head(accrate_d17)
mean(accrate_d17);var(accrate_d17)
plot(density(accrate_d17), main = "Accumulation rate at 15 cm depth cm/yr", 
     xlab ='yr/cm',
     ylab='')


accrate40 <- accrate.age(age=-50, cmyr = TRUE) # -40 BP is equivalent to 1990
summary(accrate40)
mean(accrate40);var(accrate40)
plot(density(accrate40), main = 'Accumulation rate at age -40 BP or 1990 CE',
     xlab = 'yr/cm',
     ylab = '')



# Analyse core subsection
rm(list = ls())
dev.off()
###
# Reference: https://www.youtube.com/watch?v=cRuUysJ3WKY
###

