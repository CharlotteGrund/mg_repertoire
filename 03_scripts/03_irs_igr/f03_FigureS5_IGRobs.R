

# Electronic supplementary material

# Figure S5: Individual gesturing rates (IGR_obs) across sex and maturation classes -----

# libraries
library(tidyverse)
library(ggplot2)
library(ggtext)  
library(hrbrthemes) 
library(dplyr)  

# dataset
data <- read_csv('02_data_clean/r0A_irs_igr_data_mukbit.csv')

# Combine both datasets into one (with an additional column indicating the source)
data_combined <- bind_rows(
  data %>% mutate(Source = "Mat_cat"),
  data %>% mutate(Source = "Sgn_sex")  
)

# Create the combined plot
combined_plot <- ggplot(data_combined, aes(x = Mat_cat, y = IGR_obs)) +
  # Boxplot: Separate fill based on 'Mat_cat' for the first dataset (Mat_cat)
  geom_boxplot(aes(fill = ifelse(Source == "Mat_cat", Mat_cat, Sgn_sex)), 
               position = position_dodge(width = 0.8), alpha = 0.5) +
  
  # Jitter: Separate jitter points by 'Sgn_sex' when it's from the second plot (Sgn_sex)
  geom_jitter(aes(color = ifelse(Source == "Sgn_sex", Sgn_sex, "black")), size = 1.6, alpha = 0.7, 
              position = position_jitterdodge(dodge.width = 0.8), show.legend = FALSE) +
  
  scale_color_manual(values = c("black", "#A2B5CD", "#CDAD00")) +  # Black for Mat_cat points and custom colors for Sgn_sex
  scale_fill_manual(values = c("bisque1", "bisque2", "bisque3", "bisque4", "#A2B5CD", "#CDAD00")) +  # Custom fill colors
  theme_ipsum() +
  theme(
    legend.position = "none",  # No legend
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),  # Move x-axis title away
    axis.title.y = element_text(size = 18, margin = margin(r = 15), hjust = 0.5),  # Move y-axis label away
    axis.text.x = element_markdown(size = 18, angle = 45, hjust = 1),  # Rotate labels 45 degrees
    axis.text.y = element_text(size = 18),
    strip.text = element_blank(),  # Remove facet titles (titles for each plot)
    axis.ticks.x = element_line(size = 0.5, color = "grey"),  # Change the color of the x-axis ticks to grey
    axis.ticks.length = unit(0.4, "cm")  # Set the length of the ticks
  ) +
  scale_x_discrete(labels = c(
    "cat-1" = "<b></b><br><i>infants</i>",  
    "cat-2" = "<b></b><br><i>juveniles</i>", 
    "cat-3" = "<b></b><br><i>saf & nf + </i><br><i> sam & bb</i>", 
    "cat-4" = "<b></b><br><i>af + am</i>"
  )) +
  scale_y_continuous(limits = c(0, 2), expand = expansion(mult = c(0, 0.1))) +  # Ensure y-axis starts at 0
  labs(fill = "") +
  ggtitle("") +
  xlab("") +       
  ylab("IGR (g tokens / h obs)") +
  facet_wrap(~ Source, ncol = 2, scales = "free_y")  # Facet the plot by Source (Mat_cat vs Sgn_sex)

# Print the combined plot
combined_plot
