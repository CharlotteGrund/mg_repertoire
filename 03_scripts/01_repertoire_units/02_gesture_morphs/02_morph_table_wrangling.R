

library(tidyverse)


morph.data <- read.csv('02_data_clean/gesture.data.morphs.csv')
morph_rules.mg_5 <- read.csv('02_data_clean/morph_rules.mg_5.csv')

data_filtered <- morph.data %>% filter (Gesture_record != "Locomote" &
                                          Gesture_record != "PotentiallyNew" &
                                          Gesture_record != "Unclear")
xx <- data_filtered

xx$Morph_name <- ifelse(is.na(xx$Morph_name), "na", xx$Morph_name)

xx$Morph <- NA
xx$Morph <- ifelse(xx$Morph_name == "Beckon.1", "Beckon.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Beckon.2", "Beckon.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Bite.1", "Bite.1_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Bite.2", "Bite.2_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Bite.3", "Bite.3_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "BiteThreat.1", "BiteThreat.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "BodyCross.1", "BodyCross.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "BumpInto.1", "BumpInto.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ChestBeat.1", "ChestBeat.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ChestBeatInformal.1", "ChestBeatInformal.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Dangle.1", "Dangle.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Embrace.1", "Embrace.1_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Embrace.2", "Embrace.2_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Embrace.3", "Embrace.3_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Fling.1", "Fling.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "GazeStance.1", "GazeStance.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Grab.1", "Grab.1_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Grab.2", "Grab.2_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Grab.3", "Grab.3_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Grab.4", "Grab.4_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Grab.5", "Grab.5_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "GrabHold.1", "GrabHold.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "GrabHold.2", "GrabHold.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HeadAvert.1", "HeadAvert.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HeadStand.1", "HeadStand.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitFake.1", "HitFake.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitNonRecipient.1", "HitNonRecipient.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitFake.1", "HitFake.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitObject.1", "HitObject.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitObject.2", "HitObject.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitObject.3", "HitObject.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitObject.4", "HitObject.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitObjectObject.1", "HitObjectObject.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipient.1", "HitRecipient.1_6", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipient.2", "HitRecipient.2_6", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipient.3", "HitRecipient.3_6", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipient.4", "HitRecipient.4_6", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipient.5", "HitRecipient.5_6", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipient.6", "HitRecipient.6_6", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipientSoft.1", "HitRecipientSoft.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipientSoft.2", "HitRecipientSoft.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipientSoft.3", "HitRecipientSoft.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitRecipientSoft.4", "HitRecipientSoft.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitSelf.1", "HitSelf.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitSelf.2", "HitSelf.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitSelf.3", "HitSelf.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitSelf.4", "HitSelf.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitTap.1", "HitTap.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "HitTap.2", "HitTap.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Jab.1", "Jab.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Jab.2", "Jab.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Jump.1", "Jump.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Kiss.1", "Kiss.1_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Kiss.2", "Kiss.2_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Kiss.3", "Kiss.3_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Kiss.4", "Kiss.4_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Kiss.5", "Kiss.5_5", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LayOn.1", "LayOn.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LeanIn.1", "LeanIn.1_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LeanIn.2", "LeanIn.2_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LeanIn.3", "LeanIn.3_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LocomoteBipedal.1", "LocomoteBipedal.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LocomoteGallop.1", "LocomoteGallop.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LocomoteRecipient.1", "LocomoteRecipient.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LocomoteStiffRun.1", "LocomoteStiffRun.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "LocomoteStiffWalk.1", "LocomoteStiffWalk.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Lunge.1", "Lunge.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectDrop.1", "ObjectDrop.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectMouth.1", "ObjectMouth.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectMove.1", "ObjectMove.1_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectMove.2", "ObjectMove.2_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectMove.3", "ObjectMove.3_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectMoveFiddle.1", "ObjectMoveFiddle.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectShake.1", "ObjectShake.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectShake.2", "ObjectShake.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectShake.3", "ObjectShake.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectShake.4", "ObjectShake.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ObjectStance.1", "ObjectStance.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "OverStance.1", "OverStance.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "OverStance.2", "OverStance.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Pivot.1", "Pivot.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "PlaceOnObject.1", "PlaceOnObject.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "PlaceOnObject.2", "PlaceOnObject.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Present.1", "Present.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Present.2", "Present.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Present.3", "Present.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Present.4", "Present.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Pull.1", "Pull.1_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Pull.2", "Pull.2_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Pull.3", "Pull.3_3", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.1", "Push.1_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.2", "Push.2_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.3", "Push.3_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.4", "Push.4_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.5", "Push.5_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.6", "Push.6_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.7", "Push.7_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Push.8", "Push.8_8", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Raise.1", "Raise.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Raise.2", "Raise.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "RakeObject.1", "RakeObject.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Reach.1", "Reach.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "RollOver.1", "RollOver.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Rub.1", "Rub.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Shake.1", "Shake.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "SpinPirouette.1", "SpinPirouette.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "SpinPirouette.2", "SpinPirouette.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "SpinRoulade.1", "SpinRoulade.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "StanceBipedal.1", "StanceBipedal.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "StiffStance.1", "StiffStance.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "StompObject.1", "StompObject.1_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "StompObject.2", "StompObject.2_2", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Stroke.1", "Stroke.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Stroke.2", "Stroke.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Stroke.3", "Stroke.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Stroke.4", "Stroke.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Swing.1", "Swing.1_1", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "ThrowObject.1", "ThrowObject.0", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Touch.1", "Touch.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Touch.2", "Touch.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Touch.3", "Touch.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Touch.4", "Touch.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "TouchLong.1", "TouchLong.1_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "TouchLong.2", "TouchLong.2_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "TouchLong.3", "TouchLong.3_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "TouchLong.4", "TouchLong.4_4", xx$Morph)
xx$Morph <- ifelse(xx$Morph_name == "Turn.1", "Turn.0", xx$Morph)


# n gesture actions

table_ga <- morph.data %>% select(Gesture_record) %>% group_by(Gesture_record) %>% summarize(n = n())


# Filter values containing ".1_"
df <- xx[grep(".1_", xx$Morph), ]
gesture_records_lca <- df$Gesture_record
#

xx$lca <- ifelse(xx$Gesture_record %in% gesture_records_lca, "lca", "plain")

xx$Morph <- ifelse(is.na(xx$Morph) & xx$Gesture_record %in% gesture_records_lca, paste0(xx$Gesture_record, ".NA"), xx$Morph)
xx$lca <- ifelse(is.na(xx$Morph), "excluded", xx$lca)


unimorph <- xx[grep(".1_1", xx$Morph), ]
length(unique(unimorph$Morph))
length(unique(unimorph$Gesture_record))

plain <- xx[grep("plain", xx$lca), ]

excluded <- xx[grep(".NA", xx$Morph), ]

polymorph <- subset(xx, !(grepl(".1_1", Morph) | grepl(".0", Morph)))
polymorph <- subset(polymorph, !(grepl("excluded", lca)))
polymorph <- polymorph %>% drop_na(Morph)

poly.morphs <- subset(polymorph, !(grepl(".NA", Morph)))
length(unique(poly.morphs$Morph))
length(unique(poly.morphs$Gesture_record))
na.data <- subset(polymorph, (grepl(".NA", Morph)))

lca_data <- subset(xx, !(grepl(".0", Morph)))
lca_data <- lca_data %>% drop_na(Morph)
na.data <- subset(lca_data, (grepl(".NA", Morph)))


#####
all_data <- xx %>% drop_na(Morph)
#####


# frequencies of morph table
table <- xx %>%
  group_by(Morph, Gesture_record) %>%
  summarise(n = n()) %>% drop_na(Morph)

table_clean <-  table[!grepl(".NA", table$Morph), ]
sum(table$n)

df <- table_clean
##


morph_configuration <- xx %>% select(Morph,
                                     Morph_name,
                                     Gesture_record,
                                     Body_part,
                                     Lateral_use,
                                     Contact_recipient,
                                     Repetition, lca)


morph_rules.mg_5 <- morph_rules.mg_5 %>%
  relocate(gesture_action, morph_name, cluster, count,rule_complexity,
           Body_part, Lateral_use,
           Contact_recipient,
           Repetition)


morph_assign <- morph_configuration %>% select(Morph, Morph_name, lca)

merged_data <- merge(morph_assign, morph.data, by = "Morph_name", all.y = TRUE)

library(dplyr)

distinct_morph_assign <- distinct(morph_assign, Morph_name, .keep_all = TRUE)
merged_data <- left_join(morph.data, distinct_morph_assign, by = "Morph_name") %>% relocate(Morph)

write_csv(merged_data, file = '02_data_clean/gesture.data.morphs2.csv')


str(morph_assign)
str(morph.data)
str(merged_data)
