
# Token observations of gesture actions and morphs ----------------- 

library(tidyverse)

# load gesture token data
data <- read.csv('02_data_clean/r0A_ga_morph_tokens.csv')

# select relevant variables (record, gesture action, morph)
xx <- data %>% select(Recording_number, Gesture_record, Morph) 

## Gesture actions -------------------

# filter gesture action occuring at least 3 times
table <- xx %>%
  group_by(Gesture_record) %>%
  summarise(n = n()) %>% filter(n >= 3)

# calculate percentage contribution
table$Perc.contribution <- round(table$n/(sum(table$n))*100, 1)

# arrange table after highest perc contribution
table <- table %>% arrange(desc(Perc.contribution))

# calculate cumulative sum
table$Cumulative <- NA

table$Cumulative[1] <- table$Perc.contribution[1]
for (i in 2:nrow(table)) {
  table$Cumulative[i] <- table$Perc.contribution[i] + table$Cumulative[i-1]
}

# calculate deviation from the mean
table$Deviation <- round(table$n-(sum(table$n))/length(unique(table$Gesture_record)),0)

action.table.70 <- table %>% 
  filter(Cumulative < 70) %>% 
  select(Gesture_record, n, Perc.contribution, Cumulative, Deviation)
sum <- nrow(xx) - sum(action.table.70$n)
perc.cont <- round((sum/nrow(xx))*100, 1)

# rest units
new_row_values <- c(Gesture_record = "Rest", 
                    n = sum, 
                    Perc.contribution = perc.cont, 
                    Cumulative = 100,
                    Deviation = '-')
# final table for gesture actions
action.table.70 <- rbind(action.table.70, new_row_values)



## Morphs ------

# only gesture actions with at least 3 observations
xx1 <- data %>% group_by(Gesture_record) %>% summarise (n = n()) %>% filter (n>=3)
ga.include <- xx1$Gesture_record
xx <- data %>% filter(Gesture_record %in% ga.include)

table <- xx %>%
  group_by(Morph) %>%
  summarise(n = n()) %>% drop_na(Morph)

# calculate percentage contribution
table$Perc.contribution <- round(table$n/(sum(table$n))*100, 1)

# arrange table after highest perc contribution
table <- table %>% arrange(desc(Perc.contribution))

# calculate cumulative sum
table$Cumulative <- NA

table$Cumulative[1] <- table$Perc.contribution[1]
for (i in 2:nrow(table)) {
  table$Cumulative[i] <- table$Perc.contribution[i] + table$Cumulative[i-1]
}

# calculate deviation from the mean
table$Deviation <- round(table$n-(sum(table$n))/length(unique(table$Morph)),0)

morph.table.70 <- table %>% 
  filter(Cumulative < 70) %>% 
  select(Morph, n, Perc.contribution, Cumulative, Deviation)
sum <- nrow(xx) - sum(morph.table.70$n)
perc.cont <- round((sum/nrow(xx))*100, 1)

# rest units
new_row_values <- c(Morph = "Rest", 
                    n = sum, 
                    Perc.contribution = perc.cont, 
                    Cumulative = 100,
                    Deviation = "-")

# final table for morphs
morph.table.70 <- rbind(morph.table.70, new_row_values)

