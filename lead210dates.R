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
headr(Core1)

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
      ymax = Pb210_Bqkg + sd_210Pb
    )
  )

Core1 %>%
  ggplot(aes(depth_cm, density_g_cm3))+
  geom_point(size=3)+
  geom_smooth()+
  scale_x_reverse()+
  scale_y_continuous(position="right")+
  coord_flip()+
  labs(title = "Density by depth",
       x="Density g/cm^3",
       y = "Depth(cm)")+
  theme(plot.title = element_text(hjust = 0.5))
  
## References, https://stackoverflow.com/questions
# /19754365/plotting-a-depth-profile-with-ggplot

#-------------------------------------------------- 

# Part 2.  Estimate age using rPlum

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

# Analyse the age
library(rplum)

# Run core with 210Pb measurements formatted as suggested 
# Here I'm running all 13 measurements with no supplementary data, (ra.case=0)
# Using last 3 readings as background value (na.supp=1)
# year is equal to 2023
# all hard coded in the settings column in the core_1.csv file

Plum('Core1_0')

# Running plum with different prior distribution
Plum('Core1_0', acc.mean = 12.5, acc.shape = 2)
Plum('Core1_0', mem.mean = 0.1, mem.strength = 10)


# Running Plum when you have both 210Pb data and radiocarbon dating data
# Radiocarbon data is formatted following Bacon as save as csv(e.g., core1_c.csv)
# save the radiocarbon data on the same folder as core1.csv

# Plum('Core1_0', thick=5, otherdates = 'core1_0_c.csv')

#Accumulation rates
## To simulate a clean start we clean the r environment
rm(list = ls())
Plum('Core1_0')
agedepth()
dev.off()

## Plotting accumulation rates for specific depth
accrate_d6 <- accrate.depth(d=6)
head(accrate_d6)
mean(accrate_d6);var(accrate_d6)
plot(density(accrate_d6), main = "Accumulation rate at 6 cm depth yr/cm", 
     xlab ='yr/cm',
     ylab='')

accrate_d6cmyr <- accrate.depth(d=6, cmyr = TRUE)
head(accrate_d6cmyr)
summary(accrate_d6cmyr)
mean(accrate_d6cmyr);var(accrate_d6cmyr)
plot(density(accrate_d6cmyr), main = "Accumulation rate at 10cm depth cm/yr ", 
     xlab ='cm/yr',
     ylab='')


## Plotting accumulation rate by age
accrate_aN21 <- accrate.age(age =-21.5)
head(accrate_aN21)
summary(accrate_aN21)
mean(accrate_aN21);var(accrate_aN21)
plot(density(accrate_aN21), main = 'Accumulation rate at age -21.5 BP or 1971',
     xlab = 'yr/cm',
     ylab = '')

acc_rate_21 <- accrate.age(age = -21.5, cmyr = TRUE)
summary(acc_rate_21)
mean(acc_rate_21);var(acc_rate_21)
plot(density(acc_rate_21), main = 'Accumulation rate (cm/yr) at age -21.5 BP or 1971',
     xlab = 'cm/yr',
     ylab = '')


### Plotting accumulation rates for the whole model
# by depth

accrate.depth.ghost(cmyr = T, acc.lim = c(0, 0.5))
accrate.depth.ghost(acc.lim = c(0,5), d.lim = c(0,50))

# by age

accrate.age.ghost(cmyr = T, acc.lim = c(0,.5))
accrate.age.ghost(cmyr = T, age.lim = c(-73, 86), acc.lim = c(0, 0.4))






###
# Reference: https://www.youtube.com/watch?v=cRuUysJ3WKY
###















