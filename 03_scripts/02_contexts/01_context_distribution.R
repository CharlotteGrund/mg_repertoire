
# Signaller context following gestural communications (successful and unsuccessful) ----

# library
library(tidyverse)

# data
data <- read.csv('02_data_clean/r0A_contexts.csv')

context_summary <- data %>%
  group_by(Context_post_signaller) %>%
  summarise(
    unique_communications = n_distinct(Communication_number)
  ) %>%
  arrange(desc(unique_communications))

# percentages
context_summary <- context_summary %>%
  mutate(percentage = unique_communications / sum(unique_communications) * 100)

write_csv(context_summary, '02_data_clean/r0A_context_summary.csv')