
# NOTE: adjustment of add morph function to the morph detection approach in mountain gorillas
# currently the assignment works for the morph names, not the modifiers, but that's ok, 
# I will do the rest in another function

# add the table with the rules for the morphs (saved in morph_rules.RData) and your data set

assign_morph_all <- function(fill_data, morph_rules){
  # Lateral_use
  fill_data$Lateral_use <- ifelse(fill_data$Lateral_use %in% c("Left", "Right"), 
                                  "unimanual", fill_data$Lateral_use)
  fill_data$Lateral_use <- ifelse(is.na(fill_data$Lateral_use), 
                                  "NotValid", fill_data$Lateral_use)
  
  # Repetition
  fill_data$Repetition <- ifelse(is.na(fill_data$Repetition), 
                                 "NotValid", fill_data$Repetition)
  
  # Contact_recipient
  fill_data$Contact_recipient <- ifelse(is.na(fill_data$Contact_recipient), 
                                        "NotValid", fill_data$Contact_recipient)
  
  # Body_part
  fill_data$Body_part <- ifelse(is.na(fill_data$Body_part), 
                                "NotValid", fill_data$Body_part)
  
  
  # This line creates a new variable called "morph_assignment"
  # It applies a function to each row of the "fill_data" dataframe using the "lapply" function
  morph_assignment <- lapply(1:nrow(fill_data), function(i){
    
    # This line creates a new dataframe called "sub_ga"
    # It filters the "morph_rules" dataframe based on the values in the current row of "fill_data"
    # Specifically, it filters by the gesture action in the current row, and by the body part signaller, body part contact, repetition, and laterality columns in the current row
    # If a column value is missing (NA), the filter allows for any value in that column
    sub_ga <-
      morph_rules %>%
      filter(gesture_action == fill_data$Gesture_record[i]) %>%
      filter(str_detect(Body_part, fill_data$Body_part[i]) | is.na(Body_part)) %>%
      filter(str_detect(Contact_recipient, fill_data$Contact_recipient[i]) | is.na(Contact_recipient)) %>%
      filter(str_detect(Repetition, fill_data$Repetition[i]) | is.na(Repetition)) %>%
      filter(str_detect(Lateral_use, fill_data$Lateral_use[i]) | is.na(Lateral_use))
    
    # If the "sub_ga" dataframe has no rows and laterality is missing (NA), this block of code is executed
    if(nrow(sub_ga) == 0 & is.na(fill_data$Lateral_use[i])){
      # This line filters the "morph_rules" dataframe again with similar criteria as before, but also includes a condition for 'NV' values for laterality column
      sub_ga <-
        morph_rules %>%
        filter(gesture_action == fill_data$Gesture_record[i]) %>%
        filter(str_detect(Body_part, fill_data$Body_part[i]) | is.na(Body_part)) %>%
        filter(str_detect(Contact_recipient, fill_data$Contact_recipient[i]) | is.na(Contact_recipient)) %>%
        filter(str_detect(Repetition, fill_data$Repetition[i]) | is.na(Repetition)) %>% 
        filter(str_detect(Lateral_use, fill_data$Lateral_use[i]) | is.na(Lateral_use) | Lateral_use == 'NV')
    }
    
    # If the "sub_ga" dataframe has more than one row, this block of code is executed
    if(nrow(sub_ga) > 1){
      # This line filters the "sub_ga" dataframe to exclude rows where all columns are missing (NA)
      sub_ga <-
        sub_ga %>%
        filter(!(is.na(Body_part) & is.na(Contact_recipient) &
                   is.na(Repetition) & 
                   is.na(Lateral_use)))
    }
    
    if(nrow(sub_ga) == 1){
      return(sub_ga$morph_name)
    } else {
      return(NA)
    }
  })
  
  return(morph_assignment)
}
