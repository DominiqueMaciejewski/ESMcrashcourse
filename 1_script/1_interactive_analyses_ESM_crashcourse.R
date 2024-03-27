# ---------------------------
# Script name:  Interactive analyses for ESM crashcourse 
#
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2023-10-26; updated 27-3-2024
# 
# Copyright (c) Dominique Maciejewski, 
# License: CC BY 4.0 | Attribution 4.0 International 
# Email: d.f.maciejewski@tilburguniversity.edu

# Step 1: Setup and Packages ----------------------------------------------

## Set-up

options(scipen = 999)  # To prevent scientific notation
sessionInfo() # For reproducibility (gives version numbers of packages)
here::i_am("1_script/1_interactive_analyses_ESM_crashcourse.R") # Set location of script

## install packages if needed
# install.packages("here")
# install.packages("remotes")
# remotes::install_github("wviechtb/esmpack")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("tidyr")
# install.packages("Hmisc")
# install.packages("misty")
# install.packages("nlme")
# install.packages("lmerTest")

## load packages
library(here) # for relative paths (try avoiding absolute paths!)
library(esmpack) # helps with data management of ESM data
library(ggplot2) # for visualization
library(dplyr) # for data wrangling
library(psych) # for descriptives
library(tidyr) # for datawrangling
library(Hmisc) # for descriptives
library(misty) # for multilevel correlation
library(nlme) # for multilevel models
library(lmerTest) # so that p-values are given for multilevel models

# Step 2: Load in Data ----------------------------------------------
data <- read.csv(here("0_data", "TYM_VNOP2023_raw.csv"), header=TRUE, stringsAsFactors=FALSE)

head(data) #inspect first 6 rows

# Step 3: Data checking/cleaning----------------------------------------------
# Total number of participants and observations
nsub(participant.ID, data) #Number of participants
sum(data$filledin == 1) #Number of observations


# Exclude participants with zero variance 
data <- check.timeinvar(n.ev.int, participant.ID, data, out = 3)
data <- check.timeinvar(n.er.rum, participant.ID, data, out = 3)

# Exclude participants that took too long to fill in the questionnaire
data <- data %>%  dplyr::filter(duration_sec_start_stop >=60 | is.na(duration_sec_start_stop)) 

# EXERCISE: HOW MANY PARTICIPANTS AND OBSERVATIONS DO WE HAVE NOW?

# Step 4: Visualization ----------------------------------------------

# Plot the data of 5 participants for negative event intensity
ts.n.ev.int.sub <- ggplot(data = data[which(data$participant.ID<=5),], 
                          aes(x = day, y = n.ev.int, group = participant.ID, color=factor(participant.ID))) +
  geom_point(show.legend = FALSE) + 
  geom_line(data=data[which(data$participant.ID<=5 & data$n.ev.int !="NA"),], show.legend = FALSE) +
  xlab("Day") + 
  ylab("Negative event intensity") + 
  ylim(0,100) +
  scale_x_continuous(breaks=seq(0,61,by=10)) 

# show figure
print(ts.n.ev.int.sub)

# save figure
ggsave(here("2_figures", "ts.n.ev.int.sub.png"), ts.n.ev.int.sub, width = 10, height = 5)


# EXERCISE: Now plot rumination (n.er.rum) for the first 10 participants

# Step 5: Multilevel models ----------------------------------------------

## Calculate centered variables -----------------------------                   

### Person-mean centering 
data$n.ev.int.c<-calc.mcent(n.ev.int, participant.ID, data=data)

### Grand-mean centering 
# aggregate depression data to subject level
groupmean<-aggreg(data=data, id=participant.ID, vars="Dep", grep=FALSE, na.rm=TRUE)

# calculate mean of depression score in this new dataset
mean_Dep <- mean(groupmean[,1], na.rm = TRUE)

# Subtract each score from the grandmean we just calculated
data <- data %>% 
  dplyr::mutate(Dep.c = Dep-mean_Dep)

## Save cleaned datafile -----------------------------   
write.csv(data, here("0_data", "TYM_VNOP2023_cleaned.csv"), row.names=FALSE)

## Research question 1: within-person association between negative event intensity and rumination
fit.lme.rq1 <- lme(n.er.rum ~ 1 + n.ev.int.c + day, 
                   random = ~ 1 + n.ev.int.c | participant.ID, 
                   correlation = corAR1(),
                   data = data, 
                   na.action = na.exclude, 
                   method = 'REML',
                   control = lmeControl(opt='optim'))

summary(fit.lme.rq1)

# EXERCISE: Now test the association between negative event intensity and relaxation (n.er.rel)
fit.lme.rq1 <- lme(n.er.rel ~ 1 + n.ev.int.c + day, 
                   random = ~ 1 + n.ev.int.c | participant.ID, 
                   correlation = corAR1(),
                   data = data, 
                   na.action = na.exclude, 
                   method = 'REML',
                   control = lmeControl(opt='optim'))

summary(fit.lme.rq1)

## Research question 2: Do inter-individual differences in depressive symptoms moderate this momentary relation?
fit.lme.rq2 <- lme(n.er.rum ~ 1 + n.ev.int.c + day + Dep.c + n.ev.int.c:Dep.c, 
                   random = ~ 1 + n.ev.int.c | participant.ID, 
                   correlation = corAR1(),
                   data = data, 
                   na.action = na.exclude, 
                   method = 'REML',
                   control = lmeControl(opt='optim'))

summary(fit.lme.rq2)

# EXERCISE: Now test the moderation of depression on the association between negative event intensity and relaxation (n.er.rel)

################################################################################
###################################### THE END #################################
################################################################################