

# Figure 4: Distribution of Signaller's context (n=10) following n = 1456 gestural communications ----

# libraries
library(tidyverse)
library(ggplot2)

plot_data <- read.csv('02_data_clean/r0A_context_summary.csv')

ggplot(plot_data, aes(x = reorder(Context_post_signaller, -percentage), y = percentage)) +
  geom_col(
    aes(fill = ifelse(Context_post_signaller %in% c("Other", "Unknown"), "grey", "original")),
    alpha = 0.7, color = "black", size = 0.5, width = 0.6
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f", percentage),
      color = ifelse(Context_post_signaller %in% c("Other", "Unknown"), "grey50", "black")
    ),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      grey = "grey70",
      original = "#8B8B00"
    ),
    guide = FALSE
  ) +
  scale_color_identity() +  
  labs(
    title = "",
    x = "",
    y = "Gestural communications (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # <- This removes all gridlines
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 18, margin = margin(r = 10)),
    axis.title.y = element_text(size = 18, margin = margin(r = 20)),
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 15)),
    axis.ticks.length = unit(0.6, "cm"),
    axis.ticks.y = element_line(size = 0.7, color = "grey70"),
    axis.ticks.x = element_line(size = 0.7, color = "grey70"),
    axis.line = element_line(color = "grey60", size = 0.8)
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, 5),
    expand = expansion(mult = c(0, 0.05))
  )
