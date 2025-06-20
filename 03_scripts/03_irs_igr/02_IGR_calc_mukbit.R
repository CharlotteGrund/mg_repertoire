
# Individual gesturing rates summary ---------------------------------

# load packages
library(tidyverse)
library(stringr)
library(dplyr)

# load data from bitukura and mukiza (anonymised)
data <- read.csv('02_data_clean/r0A_gesture_data_mukbit.csv')
# df from IRS calculation (01a)
irs_summary <- read.csv('02_data_clean/r0A_irs_summary_mukbit.csv')

  # add total observation h per individual (social unit follows)
irs_igs_summary <- irs_summary
irs_igs_summary$Obs_h <- NA

# anonymisation of signallers
unit2_signallers <- unique(data$Signaller[data$Social_unit == "UNIT2"])
unit1_signallers <- unique(data$Signaller[data$Social_unit == "UNIT1"])

# adding observation hours for each social unit
# unit2 = 220 h; unit1 = 225.8 h
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller %in% unit2_signallers, 
                                220, irs_igs_summary$Obs_h)
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller %in% unit1_signallers, 
                                225.8, irs_igs_summary$Obs_h)
# adjustments of observation time for emigration, birth
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller == "ID34", 
                                87.9, irs_igs_summary$Obs_h)
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller == "ID18", 
                                220.9, irs_igs_summary$Obs_h)
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller == "ID33", 
                                124.05, irs_igs_summary$Obs_h)
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller == "ID27", 
                                155.25, irs_igs_summary$Obs_h)
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller == "ID07", 
                                95.58, irs_igs_summary$Obs_h)
irs_igs_summary$Obs_h <- ifelse(irs_igs_summary$Signaller == "ID26", 
                                130.25, irs_igs_summary$Obs_h)

# calculating IGR / observation hour
irs_igs_summary$IGR_obs <- NA
irs_igs_summary$IGR_obs <- irs_igs_summary$n_tokens/irs_igs_summary$Obs_h

# overall mg gesturing rate irrespective of signaller
sum(irs_igs_summary$n_tokens)/(220+225.8) # 6.45

# IGR_obs mean, range, sd
mean(irs_igs_summary$IGR_obs) # 0.49
range(irs_igs_summary$IGR_obs) # 0.03 - 1.21
sd(irs_igs_summary$IGR_obs) # 0.32

