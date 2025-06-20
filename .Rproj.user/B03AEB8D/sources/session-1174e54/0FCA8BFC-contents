
library(tidyverse)
library(ggplot2)

# Figure S3. Detection asymptote for gesture morphs --------

# load gesture token data
data <- read.csv('02_data_clean/r0A_ga_morph_tokens.csv')

# only gesture actions with at least 3 observations
xx1 <- data %>% group_by(Gesture_record) %>% summarise (n = n()) %>% filter (n>=3)
ga.include <- xx1$Gesture_record
xx <- data %>% filter(Gesture_record %in% ga.include)

# select relevant variables (record, morph)
xx <- xx %>% select(Recording_number, Morph) 
xx <- xx %>% drop_na(Morph)

# convert gesture_record (getsure action) to factor if it's not already
xx$Morph <- as.factor(xx$Morph)

# empty vector to store cumulative counts
cumulative_counts <- numeric(length(levels(xx$Morph)))

# empty vector to store cumulative recording numbers
cumulative_recording_numbers <- numeric(nrow(xx))

# empty vector to store cumulative unique Gesture record levels
cumulative_unique_levels <- numeric(nrow(xx))

# loop through each recording number
for (i in 1:nrow(xx)) {
  # update the cumulative counts for the current Gesture_record level
  cumulative_counts[xx$Morph[i]] <- 1
  
  # update the cumulative unique Gesture record levels
  cumulative_unique_levels[i] <- sum(cumulative_counts)
  
  # store cumulative recording number
  cumulative_recording_numbers[i] <- i
}

# Create a data frame for plotting
plot_data <- data.frame(Cumulative_Recording_Number = cumulative_recording_numbers,
                        Cumulative_Unique_Gesture_Levels = cumulative_unique_levels)

# Find the index where all 63 levels of Gesture_record have been observed
asymptote_index <- min(which(cumulative_unique_levels == 126))
asymptote_index <- 1395

# Plot using ggplot
ggplot(plot_data, aes(x = Cumulative_Recording_Number, y = Cumulative_Unique_Gesture_Levels)) +
  geom_point(shape = 16, size = 2, color = "orange", alpha = 0.4) +  # Increase size and make points transparent
  geom_vline(xintercept = cumulative_recording_numbers[asymptote_index], color = "black", linetype = "dashed") +
  annotate("text", x = cumulative_recording_numbers[asymptote_index], y = max(plot_data$Cumulative_Unique_Gesture_Levels) * 0.9,
           label = "1395", color = "black", hjust = 1, vjust = 1, angle = 90, size = 6) +  # Increase size of label
  labs(x = "Gesture tokens",
       y = "Repertoire size (morphs)") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(-100, 3500 * 1.1)) +  # Adjust x-axis limits
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 130 * 1.1)) +  # Adjust y-axis limits
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18),
        axis.line = element_line(color = "black"),  # Add x and y axes
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # Remove grid lines
        axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label position
        axis.title.y = element_text(margin = margin(r = 20)),  # Adjust y-axis label position
        axis.ticks = element_line(color = "black")) +  # Add ticks on both axes
  geom_smooth(method = "nls", formula = y ~ SSlogis(x, Asym, xmid, scal), se = FALSE, color = "orange", size=0.5, alpha=0.1)  # Add logistic model line


