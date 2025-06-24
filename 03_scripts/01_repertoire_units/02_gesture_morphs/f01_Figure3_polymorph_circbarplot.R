

library(tidyverse)
library(RColorBrewer)

# Figure 3: Circular barplot of morphs of polymorph gesture actions -------------------------------

## load morph table with assigned morph names (see script morph table)
xx <- read.csv('02_data_clean/gesture.data.morphs2.csv')

# Filter values containing ".1_"
df <- xx[grep(".1_", xx$Morph), ]
gesture_records_lca <- df$Gesture_record

xx$lca <- ifelse(xx$Gesture_record %in% gesture_records_lca, "lca", "plain")
xx$Morph <- ifelse(is.na(xx$Morph) & xx$Gesture_record %in% gesture_records_lca, paste0(xx$Gesture_record, ".NA"), xx$Morph)
xx$lca <- ifelse(is.na(xx$Morph), "excluded", xx$lca)

unimorph <- xx[grep(".1_1", xx$Morph), ]
plain <- xx[grep("plain", xx$lca), ]
excluded <- xx[grep(".NA", xx$Morph), ]

#####
all_data <- xx %>% drop_na(Morph)
#####


# for plot only morphs of polymorph gesture actions
data_filtered <- subset(xx, !(grepl(".1_1", Morph) | grepl(".0", Morph)))
xx <- data_filtered

# frequencies of morph table
table <- xx %>%
  group_by(Morph, Gesture_record) %>%
  summarise(n = n()) %>% drop_na(Morph)

table_clean <-  table[!grepl(".NA", table$Morph), ]
sum(table$n)


df <- table_clean
# df from morph wrangling
df <- as.data.frame(df)

morph_names <- df$Morph

xx <- df

xx <- xx %>% select(Gesture_record, n) %>% arrange(xx$Gesture_record, desc(xx$n))
xx$numbering <- ave(xx$n,
                    xx$Gesture_record,
                    FUN = seq_along)

xx <- xx %>% unite("Action_morph_figure", c(Gesture_record, numbering), sep = ".", na.rm = TRUE, remove = FALSE)
xx$Morph <- morph_names

xx$id=seq(1, nrow(xx))
data <- xx

pp <- data %>% select(Morph, Gesture_record, n, id) %>%
  rename (individual = Morph, group= Gesture_record, value = n, id=id)

data <- pp

colourCount = length(unique(data$group))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# relabel morphs outside the scale 

data$individual<- ifelse(data$individual == "ObjectMove.1_3",
                         "+",
                         data$individual)


group <- unique(data$group)
na.0 <- data.frame(group, NA, 1)
na.0 <- na.0 %>% rename(value = NA., individual = X1)
trial <- data %>% select (individual, value, group)
vv <- rbind(trial, na.0)
data <- vv %>% arrange(group)
data$id <- seq(1, nrow(data))


# Get the name and the y position of each label
label_data <- data
label_data$ind <- paste(label_data$individual, " (", label_data$value,")", sep="")

number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-160,160) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=value+10, label=ind, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=6, angle= label_data$angle, inherit.aes = FALSE ) +
  scale_fill_manual(values = getPalette(colourCount))

p

####

# Save the plot with higher resolution and bigger size
# ggsave(filename = "circular_polymorph_plot.pdf", plot = p,
#        width = 20, height = 20, dpi = 300, bg = "white")