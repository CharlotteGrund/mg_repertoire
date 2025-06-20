
# Figure 5: Individual repertoire sizes (IRS_1) across sex and maturation classes -----

# libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# data
data <- read_csv('02_data_clean/r0A_irs_igr_data_mukbit.csv')

# Combine both datasets into one (with an additional column indicating the source)
data_combined <- bind_rows(
  data %>% mutate(Source = "Mat_cat"),
  data %>% mutate(Source = "Sgn_sex")  # Using 'data_adjust' for the second plot
)

# Reverse factor levels so infants appear at the top
data_combined <- data_combined %>%
  mutate(Mat_cat = factor(Mat_cat, levels = rev(c("cat-1", "cat-2", "cat-3", "cat-4"))))
data_combined <- data_combined %>%
  mutate(
    Sgn_sex = factor(Sgn_sex, levels = c("male", "female")),             # For plotting (female on top)
    Sgn_sex_legend = factor(Sgn_sex, levels = c("female", "male"))       # For legend (female listed first)
  )
# Create the plot
sgn_sex_plot <- data_combined %>%
  filter(Source == "Sgn_sex") %>%
  ggplot(aes(x = Mat_cat, y = IRS_1)) +
  
  # Boxplot and jitter
  geom_boxplot(aes(fill = Sgn_sex), position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_jitter(aes(color = Sgn_sex), size = 2, alpha = 0.7,
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2),
              show.legend = FALSE) +

  # Color scales
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("#EEE0E5", "#528B8B")) +
  
  # Flip coordinates
  coord_flip() +
  
  # Theme adjustments (ticks longer, axis texts moved further away)
  theme_ipsum() +
  theme(
    legend.position = c(0.9, 0.86),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.box.background = element_rect(color = "black", size = 0.5),
    
    axis.title.x = element_text(size = 20, margin = margin(t = 25, l = 30), vjust = 1),
    axis.title.y = element_text(size = 22, margin = margin(r = 15), hjust = 0.5),
    
    axis.text.x = element_text(size = 22, margin = margin(t = 15)),
    axis.text.y = element_text(size = 20, margin = margin(r = 15)),
    
    axis.ticks.length = unit(0.6, "cm"),
    axis.ticks.y = element_line(size = 0.7, color = "white"),
    axis.ticks.x = element_line(size = 0.7, color = "grey70"),
    
    axis.line = element_line(color = "grey60", size = 0.8),
    
    panel.grid.major = element_blank(),  # Turn off major grid lines
    panel.grid.minor = element_blank(),  # Turn off minor grid lines
    
    plot.clip = "off"
  )+
  
  # Category labels
  scale_x_discrete(labels = c(
    "cat-1" = "infants",
    "cat-2" = "juveniles",
    "cat-3" = "subadult & nulliparous females\n+                      \nsubadult males & blackbacks",
    "cat-4" = "parous (adult) females\n+         \nadult males"
  )) +
  
  # IRS1 axis
  scale_y_continuous(limits = c(0, 60), expand = expansion(mult = c(0, 0.05))) +
  
  labs(fill = "Sgn_sex") +
  ggtitle("") +
  xlab("") +       
  ylab("Individual repertoire size (IRS_1)                                     ") +
  guides(fill = guide_legend(reverse = TRUE))


# Load grid for drawing
library(grid)
# Draw the plot first
print(sgn_sex_plot)


# Define start and end y-positions in npc units
# y_start <- 0.18  # Approximate location of the x-axis (adjust as needed)
# y_end <- 0.90    # Upper margin
# 
# # Calculate 5 equally spaced y-positions between start and end
# y_positions <- seq(y_start, y_end, length.out = 5)
y_positions <- c(0.195, 0.372, 0.55, 0.725, 0.902)

# Draw dashed lines across the full width at each y-position
for (y in y_positions) {
  grid.lines(x = unit(c(0.07, 0.34), "npc"),
             y = unit(c(y, y), "npc"),
             gp = gpar(col = "grey50", alpha = 0.4, lty = "solid", lwd = 5))
}

y_positions <- c(0.195, 0.372, 0.55, 0.725, 0.902)

# Draw dashed lines across the full width at each y-position
for (y in y_positions) {
  grid.lines(x = unit(c(0.37, 0.8), "npc"),
             y = unit(c(y, y), "npc"),
             gp = gpar(col = "grey50", alpha = 0.4, lty = "solid", lwd = 5))
}

