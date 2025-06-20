

# Individual repertoire sizes summary ---------------------------------

# load packages
library(tidyverse)
library(stringr)
library(dplyr)

# only data from bitukura and mukiza

data <- read.csv('02_data_clean/r0A_gesture_data_mukbit.csv')

# dataset sample sizes
signallers <- unique(data$Signaller)
data_gtokens <- unique(data$Recording_number)
data_gactions <- unique(data$Gesture_action)

length(signallers) # 27 signallers
length(data_gtokens) # 2877 gesture tokens
length(data_gactions) # 60 gesture actions (note: at least 3 observations/gesture action in datasubset Muk+Bit)

# calculate individual repertoire sizes
ind_rep <- function(data) {
  signaller_stats <- data %>%
    group_by(Signaller) %>%
    summarize(
      IRS_1 = n_distinct(Gesture_action),
      IRS_3 = sum(table(Gesture_action) >= 3),
      Perc_IRS1 = n_distinct(Gesture_action) / n_distinct(data$Gesture_action) * 100,
      n_tokens = n_distinct(Recording_number),
      n_coms = n_distinct(Communication_number)
    )
  
  return(signaller_stats)
}

irs_summary <- ind_rep(data)
head(irs_summary)

write_csv(irs_summary, file = '02_data_clean/r0A_irs_summary_mukbit.csv')


# IRS 1 (without threshold)
mean(irs_summary$IRS_1) # 25.19
sd(irs_summary$IRS_1) # 11.23
range(irs_summary$IRS_1) # 3-45

# IRS 3 (with threshold)
mean(irs_summary$IRS_3) # 11.96
sd(irs_summary$IRS_3) # 7.12
range(irs_summary$IRS_3) # 1-26


# Correlation test for individual rep and gesture tokens --------

# correlation test without threshold (GA n=1/signaller)
# selecting columns
tokens <- irs_summary$n_tokens
actions_1 <- irs_summary$IRS_1
# Performing the correlation test
correlation_test <- cor.test(tokens, actions_1)
# Printing the test statistic and p-value
print(correlation_test) # 0.917

# correlation test with threshold (GA n =3 /signaller)
# selecting columns
tokens <- irs_summary$n_tokens
actions_3 <- irs_summary$IRS_3
# Performing the correlation test
correlation_test <- cor.test(tokens, actions_3)
# Printing the test statistic and p-value
print(correlation_test) # 0.948

