
library(tidyverse)

# load gesture token data
data <- read.csv('02_data_clean/r0A_ga_morph_tokens.csv')

# only gesture actions with at least 3 observations
xx1 <- data %>% group_by(Gesture_record) %>% summarise (n = n()) %>% filter (n>=3)
ga.include <- xx1$Gesture_record
xx <- data %>% filter(Gesture_record %in% ga.include)

new_select <- c("Gesture_record_28",
                "Gesture_record_1",
                "Gesture_record_24",
                "Gesture_record_32",
                "Gesture_record_48",
                "Gesture_record_64",
                "Gesture_record_59",
                "Gesture_record_33",
                "Gesture_record_44",
                "Gesture_record_15",
                "Gesture_record_30",
                "Gesture_record_29",
                "Gesture_record_38",
                "Gesture_record_47",
                "Gesture_record_4")

selection <- xx %>% filter(Gesture_record %in% new_select)

nrow(selection)
total_n <- nrow(xx)

table <- selection %>% select(Gesture_record, Recording_number) %>%
  group_by(Gesture_record) %>% summarise(n = n())

table$perc_token_contribution <- (table$n/total_n)*100
sum(table$perc_token_contribution) # 11
