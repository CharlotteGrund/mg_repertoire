# function to add clip length to gesture data

add_clip_length <- function(gesture_data, clip_data) {
  gesture_data$Clip_length <- NA  # Initialize the column with NA values
  
  for (i in 1:nrow(gesture_data)) {
    clip_name <- substring(gesture_data$Clip_name[i], 1, 31)  # Extract the first 31 characters
    
    # Find the matching row in clip_data based on fuzzy matching
    match_row <- clip_data[str_detect(clip_data$Clip_name_dir, clip_name), ]
    
    if (nrow(match_row) > 0) {
      gesture_data$Clip_length[i] <- match_row$Clip_length[1]  # Assign the Clip_length value
    }
  }
  
  return(gesture_data)
}