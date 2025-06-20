
# IRS, tokens, data days etc per Signaller.matcat --------------
library(tidyverse)

# load data (gesture_data_mukbit.csv)

data <- read_csv('02_data_clean/r0A_gesture_data_mukbit.csv')  
obs_data <- read_csv('02_data_clean/r0A_obs_data.csv') 
clip_data <- read_csv('02_data_clean/r0A_clip_tokens_mukbit.csv')
  
# call function repertoire matcat table data = gesture data
source("04_functions/repertoire_matcat_table.R")
matcat_irs <- repertoire_matcat_table(data)
matcat_irs <- matcat_irs %>% relocate(
  n_obs_days, Season, IRS_3)
unique(matcat_irs$Signaller.matcat)

# make observation hours per season
obs_min_day <- obs_data # data with observation minutes for each day (and social unit)
obs_min_day$Date <- as.Date(obs_min_day$Date)

xFS1 <- filter(obs_min_day, Date >= as.Date("2019-11-30"), Date <= as.Date("2020-03-18"))
xFS1$Season <- "FS1"
xFS2 <- filter(obs_min_day, Date >= as.Date("2021-06-30"), Date <= as.Date("2021-09-01"))
xFS2$Season <- "FS2"
xFS3 <- filter(obs_min_day, Date >= as.Date("2022-01-01"), Date <= as.Date("2022-03-30"))
xFS3$Season <- "FS3"
xPilot <- filter(obs_min_day, Date >= as.Date("2019-07-30"), Date <= as.Date("2019-09-01"))
xPilot$Season <- "Pilot"

length(unique(xFS1[["Date"]])) # 59
length(unique(xFS2[["Date"]])) # 45
length(unique(xFS3[["Date"]])) # 30
length(unique(xPilot[["Date"]])) # 17
obs_min_day <- rbind(xFS1, xFS2, xFS3, xPilot)

# Ensure only relevant social units are considered in obs_min_day
obs_min_day <- obs_min_day %>%
  filter(Social_unit %in% c("Muk", "Bit"))

# Summarize observation minutes per Season and Social Unit
obs_h_season_unit <- obs_min_day %>%
  group_by(Season, Social_unit) %>%
  summarise(Obs_h = sum(Obs_min) / 60, .groups = "drop")

# Parse the seasons in age_rep_size so each season is a separate row
matcat_rep_size_long <- matcat_irs %>%
  separate_rows(Season, sep = ",\\s*")

# Make a copy of the original dataframe
xdata <- matcat_rep_size_long

# Extract the signaller name from Signaller.matcat (everything before ".")
xdata <- xdata %>%
  mutate(Signaller = str_extract(Signaller.matcat, "^[^.]+")) 

# Assign Social Unit based on signaller name
unit2_signallers <- unique(data$Signaller[data$Social_unit == "UNIT2"])
unit1_signallers <- unique(data$Signaller[data$Social_unit == "UNIT1"])

xdata <- xdata %>%
  mutate(Social_unit = case_when(
    Signaller == "ID18" ~ "Both",  # Special case for "BT"
    Signaller %in% unit2_signallers ~ "Bit",
    Signaller %in% unit1_signallers ~ "Muk",
    TRUE ~ NA_character_
  ))

# Ensure each season is a separate row
xdata <- xdata %>%
  separate_rows(Season, sep = ",\\s*")

# Join with Obs_h data based on Season and Social Unit
xdata <- xdata %>%
  left_join(obs_h_season_unit, by = c("Season", "Social_unit"))

# Summarize observation time per Signaller.matcat
obs_time_sgn_matcat <- xdata %>%
  group_by(Signaller.matcat) %>%
  summarise(Observation_time = sum(Obs_h, na.rm = TRUE), .groups = "drop") %>%
  mutate(Observation_time = ifelse(Signaller.matcat == "ID18.cat-4", 210, Observation_time))  # Force 210 for ID18

# Join Observation_time from obs_time_sgn_matcat to matcat_irs, renaming it to Obs_h
matcat_irs <- matcat_irs %>%
  left_join(obs_time_sgn_matcat %>% rename(Obs_h = Observation_time), by = "Signaller.matcat")

matcat_irs$IGR_obs <- matcat_irs$n_tokens/matcat_irs$Obs_h

write.csv(matcat_irs, file = '02_data_clean/r0A_matcat_irs.csv')




# Add Signaller.matcat info to the clip data gtokens ----------------------------------
# Aim: calculate clip length for each signaller matcat for the IGR vid (tokens/h vid)

# gesture data with Signaller.matcat, Signaller, Season etc. (gesture_data_mukbit)
df_gesture <- data %>% select(Signaller, Signaller.matcat, Season, Date, Mat_cat)
df_gesture=df_gesture[!duplicated(df_gesture), ] # remove duplicated rows

# clip directory data for MUK and BIT with clip length per individual etc
df <- clip_data
df$Signaller.matcat <- NA
df$Mat_cat <- NA
df <- df %>% rename(
  Signaller = Individual) %>% # 
  relocate (Signaller.matcat, Mat_cat)

## add Signaller.matcat from the gesture data -------------------------------
# (will only do it for the dates with gestural data)

# Loop through each row in 'x' gesture dataframe
x <- df_gesture
df_clip <- df

for (i in 1:nrow(x)) {
  # Find matching rows in 'df_clip' based on Date and Signaller
  matching_rows <- df_clip$Date == x$Date[i] & df_clip$Signaller == x$Signaller[i]
  
  # Update 'Signaller.age' in 'df_clip' with 'Signaller.age' from 'x'
  df_clip$Signaller.matcat[matching_rows] <- x$Signaller.matcat[i]
  
  # Update 'Mat_cat' in 'df_clip' with 'Mat_cat' from 'x'
  df_clip$Mat_cat[matching_rows] <- x$Mat_cat[i]
}

# Fill missing values in Signaller.age with corresponding values 
# based on Signaller and Season
df_clip <- df_clip %>%
  group_by(Signaller, Season) %>%
  fill(Signaller.matcat, .direction = "downup") %>%
  ungroup()

clip_data <- df_clip

write_csv(clip_data, "02_data_clean/r0A_clip_length_min_sgn.csv")


# Group by Signaller.matcat and calculate Clip_length/ Signaller.matcat
vid_sgn.matcat <- clip_data %>%
  group_by(Signaller.matcat) %>%
  summarize(Total_Clip_length = sum(Clip_length, na.rm = TRUE)) # all vid data

vid_sgn.matcat <- vid_sgn.matcat %>% drop_na(Signaller.matcat)

# mean_age <- matcat_irs %>% select(Mean_age, Signaller.matcat) # see avove
df_irs <- left_join(vid_sgn.matcat, matcat_irs, by = "Signaller.matcat")
df_irs$hour_vid <- df_irs$Total_Clip_length/60
df_irs$IGR_vid <- (df_irs$n_tokens)/(df_irs$Total_Clip_length/60)
df_irs <- df_irs %>%
  mutate(Mat_cat = str_extract(Signaller.matcat, "cat-\\d+"))
# df_irs <- left_join(df_irs, mean_age, by = "Signaller.matcat")
df_irs$IGR_obs <- as.numeric(df_irs$IGR_obs)
# df_irs$Mean_age.x <- as.numeric(df_irs$Mean_age.x)

# final table with signaller matcat table with irs and frequencies
irs_igr_sgn.matcat <- df_irs %>% 
  rename(min_vid_directory = Total_Clip_length,
         hour_obs = Obs_h,
         # Mean_age = Mean_age.x,
         Seasons = Season)

columns <- c("Signaller.matcat",
             # "Mean_age",
             "Mat_cat",
             "Sgn_sex",
             "IRS_1",
             "IRS_3",
             "IGR_vid",
             "IGR_obs",
             "hour_vid",
             "hour_obs",
             "Perc_IRS1",
             "n_tokens",
             "n_coms",
             "Seasons")

irs_igr_sgn.matcat <- irs_igr_sgn.matcat %>% select(all_of(columns))
write_csv(irs_igr_sgn.matcat, '02_data_clean/r0A_irs_igr_data_mukbit.csv')


## IRS_1, IRS_3, IGR_obs, IGR_vid across maturation and sex categories -----------------------------

xx <- irs_igr_sgn.matcat %>% 
  select(Mat_cat, IGR_obs, IGR_vid, IRS_1, IRS_3, Signaller.matcat, Sgn_sex, n_tokens) %>%
  group_by(Mat_cat)

# Table S14. Individual repertoire size and gesturing rate across maturation categories --------

# (n = 27 signallers, n = 2877 gesture tokens; n = 60 gesture actions, n = 2 social units (Bit, Muk)).  
# Mat. Cat-1: infants (0-3 years, both sexes); 
# Mat. Cat-2: juveniles (> 3 – 6 years, both sexes), 
# Mat. Cat-3: subadults (> 6 – 8 years, both sexes) + young nulliparous females (females 8 > 12 years without infants) + blackbacks (males: > 8 – 12 years); 
# Mat. Cat-4: adults (males > 12 years; parous females; adult nulliparous females > 12 years).

table_matcat <- xx %>%
  group_by(Mat_cat) %>%
  summarise(
    mean_IGR_obs = mean(IGR_obs, na.rm = TRUE),
    sd_IGR_obs = sd(IGR_obs, na.rm = TRUE),
    min_IGR_obs = min(IGR_obs, na.rm = TRUE),
    max_IGR_obs = max(IGR_obs, na.rm = TRUE),
    
    mean_IGR_vid = mean(IGR_vid, na.rm = TRUE),
    sd_IGR_vid = sd(IGR_vid, na.rm = TRUE),
    min_IGR_vid = min(IGR_vid, na.rm = TRUE),
    max_IGR_vid = max(IGR_vid, na.rm = TRUE),
    
    mean_IRS_1 = mean(IRS_1, na.rm = TRUE),
    sd_IRS_1 = sd(IRS_1, na.rm = TRUE),
    min_IRS_1 = min(IRS_1, na.rm = TRUE),
    max_IRS_1 = max(IRS_1, na.rm = TRUE),
    
    mean_IRS_3 = mean(IRS_3, na.rm = TRUE),
    sd_IRS_3 = sd(IRS_3, na.rm = TRUE),
    min_IRS_3 = min(IRS_3, na.rm = TRUE),
    max_IRS_3 = max(IRS_3, na.rm = TRUE),
    
    tokens = sum(n_tokens, na.rm = TRUE),
    n_signallers = n_distinct(Signaller.matcat)
  )

# Table S15. Female and male IRS and IGR across maturation classes ---------
# (n = 27 signallers, n = 2877 gesture tokens; n = 60 gesture actions, n = 2 social units (Bit, Muk)). 
# Mat. Cat-1: infants (0-3 years, both sexes); 
# Mat. Cat-2: juveniles (> 3 – 6 years, both sexes), 
# Mat. Cat-3: subadults (> 6 – 8 years, both sexes) + young nulliparous females (females 8 > 12 years without infants) + blackbacks (males: > 8 – 12 years); 
# Mat. Cat-4: adults (males > 12 years; parous females; adult nulliparous females > 12 years).

table_matcat_sex <- xx %>%
  group_by(Mat_cat, Sgn_sex) %>%
  summarise(
    mean_IGR_obs = mean(IGR_obs, na.rm = TRUE),
    sd_IGR_obs = sd(IGR_obs, na.rm = TRUE),
    min_IGR_obs = min(IGR_obs, na.rm = TRUE),
    max_IGR_obs = max(IGR_obs, na.rm = TRUE),
    
    mean_IGR_vid = mean(IGR_vid, na.rm = TRUE),
    sd_IGR_vid = sd(IGR_vid, na.rm = TRUE),
    min_IGR_vid = min(IGR_vid, na.rm = TRUE),
    max_IGR_vid = max(IGR_vid, na.rm = TRUE),
    
    mean_IRS_1 = mean(IRS_1, na.rm = TRUE),
    sd_IRS_1 = sd(IRS_1, na.rm = TRUE),
    min_IRS_1 = min(IRS_1, na.rm = TRUE),
    max_IRS_1 = max(IRS_1, na.rm = TRUE),
    
    mean_IRS_3 = mean(IRS_3, na.rm = TRUE),
    sd_IRS_3 = sd(IRS_3, na.rm = TRUE),
    min_IRS_3 = min(IRS_3, na.rm = TRUE),
    max_IRS_3 = max(IRS_3, na.rm = TRUE),
    
    tokens = sum(n_tokens, na.rm = TRUE),
    n_signallers = n_distinct(Signaller.matcat)
  )
