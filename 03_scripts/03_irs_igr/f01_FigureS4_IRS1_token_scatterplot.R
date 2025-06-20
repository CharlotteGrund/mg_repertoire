
# Electronic supplementary material

# Figure S4: Relationship between repertoire size and gesture tokens -----------
# (n = 27 signallers, n = 2877 gesture tokens, n = 60 gesture actions, n = 2 social units)

library(tidyverse)
library(ggplot2)

# data
data <- read.csv('02_data_clean/r0A_irs_summary_mukbit.csv')

# IRS 1  - Without inclusion threshold (1 token/GA/signaller)
data <- data.frame(tokens = data$n_tokens, actions_1 = data$IRS_1)
# Plotting (scatterplot, regression line, conf. intervals)
ggplot(data, aes(x = tokens, y = actions_1)) +
  geom_point(color = "black", size = 1.5) +  
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  
  labs(
    title = "",
    x = "Gesture tokens",
    y = "IRS 1"
  ) +
  theme_minimal() +  
  theme(axis.text = element_text(size = 15),            
        axis.title.x = element_text(size = 16, margin = margin(t = 15)), 
        axis.title.y = element_text(size = 16, margin = margin(r = 15))  
  )
