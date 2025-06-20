
# takes the gesture data with the Signaller.matcat column and calculates
# per signaller.matcat the unique number of gesture actions, tokens, data_days etc.

repertoire_matcat_table <- function(data) {
  library(dplyr)
  
  signaller_stats <- data %>%
    group_by(Signaller.matcat) %>%
    summarize(
      # Mean_age = mean(Sgn_age),
      IRS_1 = n_distinct(Gesture_action),
      Perc_IRS1 = n_distinct(Gesture_action) / n_distinct(data$Gesture_action) * 100,
      n_tokens = n_distinct(Recording_number),
      n_coms = n_distinct(Communication_number),
      Sgn_sex = first(Sgn_sex),
      n_obs_days = n_distinct(Date),
      Season = paste(unique(Season), collapse = ",") ,
      IRS_3 = sum(table(Gesture_action) >= 3)  # Store unique Seasons as a list
    )
  
  return(signaller_stats)
}