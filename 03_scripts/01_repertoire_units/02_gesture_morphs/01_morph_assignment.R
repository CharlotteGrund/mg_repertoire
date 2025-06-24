
### Morph assignment 2024


#### workspace to assign morphs to dataframe  ----------------------

# Load packages

library(tidyverse)

source('04_functions/add_morphs_all.R')


# First check the names of the modifier names of the morph_rules table

# set data path
data_path <- '02_data_clean/'

# read morph rule table
morph_rules.mg_5 <- read_csv(paste0(data_path, 'morph_rules.mg_5.csv'))

# read cleaned gesture data
gesture.data <- read_csv(paste0(data_path, 'g.data.mg.csv'))
# only gesture action >= 3 observations
gesture.data <- gesture.data %>%
  group_by(Gesture_record) %>%
  filter(n() >= 3) %>%
  ungroup()

### Match column names -----
# rename relevant columns in the morph_rules dataframe
# to their respective names in the gesturedata df
# (important that col names are the same)
# morph_rules.mg_5 <- morph_rules.mg_5 %>%
#   rename(#
#     )

### Match modifier level lumping -----

# levels within the relevant columns should have the same lumping stage

# # Need to do renaming of levels in the Contact_recipient tier of gesture.data
# xx <- gesture.data
# 
# # Body part levels
# 
# xx$Body_part <-
#   ifelse(xx$Body_part == "BackShoulder",
#          'Body',
#          xx$Body_part)
# xx$Body_part <-
#   ifelse(xx$Body_part == "Face",
#          'Head',
#          xx$Body_part)
# 
# extremity <- c('Arm', 'Leg',
#                'Hand', 'Fist',
#                'Foot', 'Knuckles',
#                'Fingers','HandFoot')
# 
# torso.back <- c('Back','BackShoulder',
#                 'Body', 'Bottom')
# 
# torso.front <- c('BodyFront', 'BodyChest')
# 
# head <- c('Head', 'Face')
# 
# # Extremity
# xx$Contact_recipient <-
#   ifelse((xx$Contact_recipient %in% extremity),
#          'Extremity',
#          xx$Contact_recipient)
# 
# # TorsoBack
# xx$Contact_recipient <-
#   ifelse((xx$Contact_recipient %in% torso.back),
#          'TorsoBack',
#          xx$Contact_recipient)
# 
# #TorsoFront
# xx$Contact_recipient <-
#   ifelse((xx$Contact_recipient %in% torso.front),
#          yes = 'TorsoFront',
#          no = xx$Contact_recipient)
# 
# # Head
# xx$Contact_recipient <-
#   ifelse((xx$Contact_recipient %in% head),
#          'Head',
#          xx$Contact_recipient)
# 
# # make "Pull with arm" into Push - coding error, just ca 3 cases or so
# xx$Gesture_record <-
#   ifelse(
#     test = (xx$Gesture_record %in% c('Pull') & xx$Body_part %in% c('Arm')),
#     yes = 'Push',
#     no = xx$Gesture_record
#   ) 
# 
# gesture.data <- xx
# 

# Assignment function -----------------------------------------------------

# Run assignment function
with_morphs <- assign_morph_all(
  fill_data = gesture.data, morph_rules = morph_rules.mg_5)

# convert assignment into vector/column
morph_row <- with_morphs

# add to morph column to gesture dataframe
gesture.data$morph_name <- morph_row


# Checking if assignment has worked ---------------------------------------

# variables to relocate/isolate for checking if assignment seems to have worked
vector_var_check1 <- c("Gesture_record",
                       "morph_name",
                       "Body_part",
                       "Contact_recipient",
                       "Lateral_use",
                       "Repetition")

# check assignment df
df <- gesture.data
df <- df %>% relocate (vector_var_check1)

# check structure of morphname column
str(df)

# convert into character
df$morph_name <- sapply(df$morph_name, as.character)

# vector of all unique morphs
x <- sort(unique(df$morph_name))

gesture.data <- df

assigned.data <- df

# Dealing with NAs - adding some rules manually ------------------------------

# There are NAs in the morph_name columns, it appears not all gesture instances
# were assigned morphs successfully -- assignment function did not work for some

# filter all entries that do not have a morph assigned
check.morph.NA <- df %>% filter (is.na(morph_name))

# vector of all gesture actions that have NA morphs (also if only partially)
filter_morph <- unique(check.morph.NA$Gesture_record)

# vector of variables for visual check - relocation
vector_var_check2 <- c("gesture_action",
                       "morph_name",
                       "Body_part",
                       "Contact_recipient",
                       "Lateral_use",
                       "Repetition")

# filter morph rules of gesture actions that have NA morphs
xx <- morph_rules.mg_5 %>%
  relocate (vector_var_check2) %>%
  filter(gesture_action %in% filter_morph)
# relocate relevant information
morph_rules.to.assign <- xx %>%
  rename(Gesture_record = gesture_action) %>%
  relocate (morph_name, count)

# relevant data selection to check (NA morphs in gesture dataframe)
morph.assignment.data <- check.morph.NA %>%
  select(all_of(vector_var_check1),
         Gesture_record,
         Recording_number,
         # Communication_number
         ) %>%
  relocate (morph_name)


# ASSIGNMENT of rules to NA morphs (function failure) -----------------------------------------

xx <- morph.assignment.data

## select script depending on which modifiers were chosen

## Bite morphs -----------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "Bite" &
                           xx$Contact_recipient == "TorsoBack"),
                        "Bite.3", xx$morph_name)

## Embrace morphs --------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "Embrace" &
                           xx$Contact_recipient == "TorsoBack" &
                           xx$Lateral_use == "Unimanual"),
                        "Embrace.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Embrace" &
                           xx$Contact_recipient == "Head" &
                           is.na(xx$Lateral_use)),
                        "Embrace.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record ==
                           "Embrace" &
                           xx$Lateral_use == "Bimanual"),
                        "Embrace.3", xx$morph_name)

## Grab morphs ---------------------------------------

xx$morph_name <- ifelse((xx$Gesture_record == "Grab" &
                           xx$Body_part == "Hand"  &
                           xx$Contact_recipient == "Extremity"),
                        "Grab.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Grab" &
                           xx$Body_part == "Hand"  &
                           xx$Contact_recipient == "TorsoBack"),
                        "Grab.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Grab" &
                           xx$Body_part == "Fingers"),
                        "Grab.3", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Grab" &
                           xx$Body_part == "Hand" &
                           xx$Contact_recipient == "Head"),
                        "Grab.4", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Grab"  &
                           xx$Body_part == "Hand" &
                           xx$Contact_recipient == "TorsoFront"),
                        "Grab.5", xx$morph_name)


## GrabHOLd morphs ---------------------------------------

xx$morph_name <- ifelse((xx$Gesture_record == "GrabHold" &
                           xx$Body_part == "Hand"  &
                           xx$Contact_recipient == "TorsoBack"),
                        "Grab.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "GrabHold" &
                           xx$Body_part == "Hand"  &
                           xx$Contact_recipient == "Extremity"),
                        "Grab.2", xx$morph_name)


## HitObject morphs ---------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "HitObject" &
                           xx$Lateral_use == "Bimanual" &
                           xx$Repetition== "Repeated"),
                        "HitObject.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitObject" &
                           xx$Lateral_use == "Unimanual" &
                           xx$Repetition== "Singular"),
                        "HitObject.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitObject" &
                           xx$Lateral_use == "Bimanual" &
                           xx$Repetition== "Singular"),
                        "HitObject.3", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitObject" &
                           xx$Lateral_use == "Unimanual" &
                           xx$Repetition== "Repeated"),
                        "HitObject.4", xx$morph_name)

# ## HitRecipient morphs ----------------------------------------

xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipient" &
                           xx$Body_part == "Hand" &
                           xx$Lateral_use == "Unimanual" &
                           xx$Repetition== "Singular"),
                        "HitRecipient.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipient" &
                           xx$Lateral_use == "Bimanual" &
                           xx$Repetition== "Repeated"),
                        "HitRecipient.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipient" &
                           xx$Lateral_use == "Unimanual" &
                           xx$Repetition== "Repeated"),
                        "HitRecipient.3", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipient" &
                           xx$Body_part != "Hand" &
                           xx$Body_part != "Arm" &
                           xx$Repetition== "Singular" &
                           xx$Lateral_use== "Unimanual"),
                        "HitRecipient.4", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipient" &
                           xx$Lateral_use == "Bimanual" &
                           xx$Repetition== "Singular"),
                        "HitRecipient.5", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipient" &
                           xx$Body_part == "Arm" &
                           xx$Lateral_use== "Unimanual"),
                        "HitRecipient.6", xx$morph_name)


## HitRecipientSoft morphs ----------------------------------------

xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipientSoft" &
                           xx$Body_part == "Fingers" &
                           xx$Lateral_use == "Unimanual"),
                        "HitRecipientSoft.1", xx$morph_name)

xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipientSoft" &
                           xx$Body_part == "Hand" &
                           xx$Lateral_use == "Unimanual"),
                        "HitRecipientSoft.2", xx$morph_name)

xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipientSoft" &
                           xx$Body_part == "Knuckles"),
                        "HitRecipientSoft.3", xx$morph_name)

xx$morph_name <- ifelse((xx$Gesture_record == "HitRecipientSoft" &
                           xx$Body_part == "Hand"&
                           xx$Lateral_use == "Bimanual"),
                        "HitRecipientSoft.4", xx$morph_name)

## HitSelf morphs ---------------------------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "HitSelf" &
                           xx$Body_part == "Hand" &
                           # xx$Contact_signaller == "BodyChest" &
                           xx$Repetition == "Repeated"),
                        "HitSelf.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitSelf" &
                           xx$Repetition == "Singular"),
                        "HitSelf.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitSelf" &
                           xx$Body_part == "Fist"),
                        "HitSelf.3", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "HitSelf" &
                           xx$Body_part == "Hand" &
                           # xx$Contact_signaller == "BodyFront" &
                           xx$Repetition == "Repeated"),
                        "HitSelf.4", xx$morph_name)

## Jab morphs -------------------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "Jab" &
                           xx$Contact_recipient != "TorsoBack"),
                        "Jab.2", xx$morph_name)


## Kiss morphs -------------------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "Kiss" &
                           xx$Contact_recipient == "Mouth"),
                        "Kiss.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Kiss" &
                           xx$Contact_recipient == "Head"),
                        "Kiss.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Kiss" &
                           xx$Contact_recipient == "TorsoBack"),
                        "Kiss.3", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Kiss" &
                           xx$Contact_recipient == "Extremity"),
                        "Kiss.4", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "Kiss" &
                           xx$Contact_recipient == "TorsoFront"),
                        "Kiss.5", xx$morph_name)


## LeanIn morphs ---------------------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "LeanIn" &
                           xx$Body_part == "Body" &
                           xx$Contact_recipient == "TorsoBack"),
                        "LeanIn.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "LeanIn" &
                           xx$Body_part == "Body" &
                           xx$Contact_recipient == "TorsoFront"),
                        "LeanIn.2", xx$morph_name)


## ObjectMove morphs -----------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectMove" &
                           xx$Lateral_use == "Unimanual"),
                        "ObjectMove.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectMove" &
                           xx$Lateral_use == "Bimanual"),
                        "ObjectMove.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectMove" &
                           xx$Body_part == "Body"),
                        "ObjectMove.3", xx$morph_name)

## ObjectShake -------------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectShake" &
                           xx$Lateral_use == "Unimanual" &
                           xx$Repetition == "Repeated"),
                        "ObjectShake.1", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectShake" &
                           xx$Lateral_use == "Unimanual" &
                           xx$Repetition == "Singular"),
                        "ObjectShake.2", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectShake" &
                           xx$Lateral_use == "Bimanual" &
                           xx$Repetition == "Singular"),
                        "ObjectShake.3", xx$morph_name)
xx$morph_name <- ifelse((xx$Gesture_record == "ObjectShake" &
                           xx$Lateral_use == "Bimanual" &
                           xx$Repetition == "Repeated"),
                        "ObjectShake.4", xx$morph_name)


## Present ---------------------------------------------------------------
xx$morph_name <- ifelse((xx$Gesture_record == "Present" &
                           xx$Body_part == "Body"),
                        "Present.3", xx$morph_name)


## Raise morphs -------------------------------------------------------

xx$morph_name <- ifelse((xx$Gesture_record == "Raise" &
                           xx$Body_part != "Hand"),
                        "Raise.2", xx$morph_name)


## Stroke morphs -------------------------------------------------------

xx$morph_name <- ifelse((xx$Gesture_record == "Stroke" &
                           xx$Body_part == "Fingers" &
                           xx$Contact_recipient != "Extremity"&
                           xx$Contact_recipient != "TorsoBack"),
                        "Stroke.4", xx$morph_name)

# ------

morph.assignment.data <- xx

# ------

# data with only successfully new assigned morphs
new.assinged <- morph.assignment.data %>%
  filter (!is.na(xx$morph_name))

# assignemnet data morph name and rec number
rec.morph <- data.frame(morph.assignment.data$morph_name,
                        morph.assignment.data$Recording_number)

# change the column names of the new dataframe to match original data
colnames(rec.morph) <- c("morph_name", "Recording_number")

# select only the ones where moprh name is not NA (so successful assignments)
rec.morph <- rec.morph %>% filter(!is.na(rec.morph$morph_name))
# vector of recod numbers
rec.morph.vec <- rec.morph$Recording_number


# Add new morph assignments to original dataset -------------------------
# original dataset
df <- gesture.data

# relocate Rec number in original dataset
df <- df %>% relocate(Recording_number)

# rename morphs (numbering) of gesture actions with missing assignments
df$morph_name <- ifelse(df$morph_name == "HitRecipient.4",
                        "HitRecipient.6", df$morph_name)
# df$morph_name <- ifelse(df$morph_name == "Bite.1",
#                         "Bite.2",df$morph_name)
# df$morph_name <- ifelse(df$morph_name == "Bite.3",
#                         "Bite.1",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "HitTap.1",
                        "HitTap.2",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "HitTap.3",
                        "HitTap.1",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "LeanIn.2",
                        NA,df$morph_name)
# df$morph_name <- ifelse(df$morph_name == "OverStance.1",
#                         "OverStance.2",df$morph_name)
# df$morph_name <- ifelse(df$morph_name == "OverStance.3",
#                         "OverStance.1",df$morph_name)
# df$morph_name <- ifelse(df$morph_name == "PlaceOnObject.2",
#                         "PlaceOnObject.1",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "PlaceOnObject.3",
                        "PlaceOnObject.2",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "Present.2",
                        "Present.4",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "Stroke.4",
                        "Stroke.3",df$morph_name)
df$morph_name <- ifelse(df$morph_name == "Present.3",
                        "Present.2",df$morph_name)
# df$morph_name <- ifelse(df$Gesture_record == "Shake",
#                         NA,df$morph_name)
# df$morph_name <- ifelse(df$Gesture_record == "Shake" &
#                           df$Body_part =="Head",
#                         "Shake.1",df$morph_name)
df$morph_name <- ifelse(df$Gesture_record == "StompObject" &
                          df$Body_part =="Fist",
                        "StompObject.2",df$morph_name)


# vec1 <- c("Arm","Fingers","Knuckles","Other")
# vec2 <- c("Arm","Fingers","Knuckles")
# 
# df$morph_name <- ifelse((df$Gesture_record == "Push" &
#                            df$Body_part == "Hand" &
#                            df$Contact_recipient == "Extremity"),
#                         "Push.1", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push" &
#                            df$Body_part %in% vec1 &
#                            df$Contact_recipient == "Extremity"),
#                         "Push.2", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push" &
#                            df$Body_part == "Hand" &
#                            df$Contact_recipient == "TorsoBack"),
#                         "Push.3", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push" &
#                            df$Body_part %in% vec1 &
#                            df$Contact_recipient == "TorsoBack"),
#                         "Push.4", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push"&
#                            df$Body_part == "Hand" &
#                            df$Contact_recipient == "Head"),
#                         "Push.5", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push" &
#                            df$Body_part %in% vec2 &
#                            df$Contact_recipient == "Head"),
#                         "Push.6", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push"&
#                            df$Contact_recipient == "TorsoFront"),
#                         "Push.7", df$morph_name)
# df$morph_name <- ifelse((df$Gesture_record == "Push"&
#                            df$Body_part == "Foot" &
#                            df$Contact_recipient == "Extremity"),
#                         "Push.8", df$morph_name)





# Find the indices of matching Recording_number values in rec.morph
match_idx <- match(df$Recording_number, rec.morph$Recording_number)

# Replace NAs in df$morph_name with corresponding values from rec.morph$morph_name
df$morph_name[!is.na(match_idx)] <- rec.morph$morph_name[match_idx[!is.na(match_idx)]]


# some basic checks
gesture.data.morphs <- df

xx <- gesture.data.morphs %>%
  filter(is.na(gesture.data.morphs$morph_name))
nrow(xx)

# renaming
gesture.data.morphs <- gesture.data.morphs %>%
  rename(Morph_name = morph_name)

length(unique(gesture.data.morphs$Morph_name))

# save morph dataframe: gesture.data.morphs
write_csv(gesture.data.morphs, file =
            paste0(data_path, 'gesture.data.morphs.csv'))
