library(tidyverse)
library(arsenal)

#Import data
recruitment_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA_recruitment_files_2019-09-12.csv"
minikid_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_MINI5KID-BASIC_DIGEST.csv"
mini5_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_MINI5-BASIC_DIGEST.csv"
scamp_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_SCAMP_SELF-BASIC_DIGEST.csv"
sdq_child_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_SDQ_CHILD-BASIC_DIGEST.csv"
sdq_adult_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_SDQ_ADULT-BASIC_DIGEST.csv"
sdq_parent_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_SDQ_PARENT-BASIC_DIGEST.csv"
sdim_path <- "C:/Users/advsp/OneDrive - Imperial College London/PhD/cVEDA/Data/cVEDA-cVEDA_SDIM-BASIC_DIGEST.csv"

recruitment <- read.csv(recruitment_path, fileEncoding = "UTF-8-BOM")
minikid <- read.csv(minikid_path, fileEncoding = "UTF-8-BOM")
mini5 <- read.csv(mini5_path, fileEncoding = "UTF-8-BOM")
scamp <- read.csv(scamp_path, fileEncoding = "UTF-8-BOM")
sdq_child <- read.csv(sdq_child_path, fileEncoding = "UTF-8-BOM")
sdq_adult <- read.csv(sdq_adult_path, fileEncoding = "UTF-8-BOM")
sdq_parent <- read.csv(sdq_parent_path, fileEncoding = "UTF-8-BOM")
sdim <- read.csv(sdim_path, fileEncoding = "UTF-8-BOM")

#EXPOSURE DATA PREP

#First create functions to convert mobile phone categories to midpoint in mins

#Convert calls to continuous midpoints
calls_self_mins_convert <- function(ownership, category) {
  midpoint <- case_when(
    ownership <= 0 | ownership == 1 ~ 0,
    category == 1 ~ 2.5,
    category == 2 ~ 10,
    category == 3 ~ (15+30)/2,
    category == 4 ~ 45,
    category == 5 ~ 60, # COULD USE HARMONIC MEAN
    category < 0 ~ NA_real_,
    TRUE ~ NA_real_
  )
}
#create function to convert internet usage on mobile to midpoint in mins
internet_midpoint_mins_convert <- function(ownership, internet_capable, category) {
  midpoint <- case_when(
    ownership <= 0 | ownership == 1 | internet_capable >= 4 ~ 0,
    category == 0 ~ 5,
    category == 1 ~ 20,
    category == 2 ~ 45,
    category == 3 ~ 75,
    category == 4 ~ 105, 
    category == 5 ~ 120, # could use harmonic mean?
    category < 0 ~ NA_real_,
    TRUE ~ NA_real_
  )
}
#create function to convert SNS usage on mobile to midpoint in mins
sns_mins_convert <- function(ownership, category) {
  midpoint <- case_when(
    ownership <= 0 | ownership == 1 ~ 0,
    category == 0 ~ 5,
    category == 1 ~ 20,
    category == 2 ~ 45,
    category == 3 ~ 90,
    category == 4 ~ 120, # could use harmonic mean?
    category < 0 ~ NA_real_,
    TRUE ~ NA_real_
  )
}
#create function to convert calls on other devices to midpoint in mins
calls_other_mins_convert <- function(ever_used, category){
  midpoint <- case_when(
    ever_used == 0 ~ 0,
    category == 1 ~ 2.5,
    category == 2 ~ 10,
    category == 3 ~ (15+30)/2,
    category == 4 ~ 45,
    category == 5 ~ 60, # COULD USE HARMONIC MEAN
    category < 0 ~ NA_real_,
    TRUE ~ NA_real_
  )
}

#Process SCAMP data
scamp_processed <- scamp %>%
  mutate(calls_self_weekday = calls_self_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q10_1)) %>%
  mutate(calls_self_weekend = calls_self_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q10_2)) %>%
  mutate(im_weekday = sns_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q16_1)) %>%
  mutate(im_weekend = sns_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q16_2)) %>%
  mutate(sns_weekday = sns_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q17_1)) %>%
  mutate(sns_weekend = sns_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q17_2)) %>%
  mutate(internet_weekday = internet_midpoint_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q18, .$SCAMP_S_q20_1)) %>%
  mutate(internet_weekend = internet_midpoint_mins_convert(.$SCAMP_S_q3, .$SCAMP_S_q18, .$SCAMP_S_q20_2)) %>%
  mutate(calls_other_weekday = calls_other_mins_convert(.$SCAMP_S_q1, .$SCAMP_S_q26_1)) %>%
  mutate(calls_other_weekend = calls_other_mins_convert(.$SCAMP_S_q1, .$SCAMP_S_q26_2)) %>%
  mutate(total_mp_weekday = .$calls_self_weekday + .$internet_weekday + .$calls_other_weekday) %>%
  mutate(total_mp_weekend = .$calls_self_weekend + .$internet_weekend + .$calls_other_weekend) %>%
  mutate(total_mp_full_week = round((5 * .$total_mp_weekday + 2 * .$total_mp_weekend) / 7, digits = 1)) %>%
  mutate(sns_full_week = round((5 * .$sns_weekday + 2 * .$sns_weekend) / 7, digits = 1)) %>%
  mutate(im_full_week = round((5 * .$im_weekday + 2 * .$im_weekend) / 7, digits = 1)) %>%
  mutate(internet_full_week = round((5 * .$internet_weekday + 2 * .$internet_weekend) / 7, digits = 1)) %>%
  select(-Age.band, -Iteration, -Language)

#OUTCOME DATA PREP

#SDQ
#Combine C2 SDQ_CHILD and C3 SDQ_ADULT into one dataframe
sdq_all <- rbind(
  select(sdq_child, Age.band, User.code, Iteration, Language, SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL, SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES),
  select(sdq_adult, Age.band, User.code, Iteration, Language, SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL, SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES)
)

#MINI & MINIKID

#Process MINIKID
minikid_processed <- minikid %>%
  mutate(anxiety = case_when(
    MINI_KID_H_SPC == 1 | MINI_KID_U_GADC == 1 ~ 1,
    MINI_KID_H_SPC < 0 & MINI_KID_U_GADC < 0 ~ NA_real_,
    TRUE ~ 0
  )) %>%
  rename(depress = MINI_KID_A_MDEC) %>%
  rename(soc.anxiety = MINI_KID_H_SPC) %>%
  rename(gen.anxiety = MINI_KID_U_GADC)

#Process MINI5
mini5_processed <- mini5 %>%
  mutate(anxiety = case_when(
    MINI_G_SPC == 1 | MINI_O_GADC == 1 ~ 1,
    MINI_G_SPC < 0 & MINI_O_GADC < 0 ~ NA_real_,
    TRUE ~ 0
  )) %>%
  rename(depress = MINI_A_MDEC) %>%
  rename(soc.anxiety = MINI_G_SPC) %>%
  rename(gen.anxiety = MINI_O_GADC) # remember to change -666 to NA

all_mini <- rbind(
  select(minikid_processed, User.code, anxiety, depress, soc.anxiety, gen.anxiety), 
  select(mini5_processed, User.code, anxiety, depress, soc.anxiety, gen.anxiety)
)

##COVARIATES

## COVARIATES / CONFOUNDERS
#Housing Quality
floor_qual_convert <- function(category) {
  floor_qual <- case_when(
    category == 1 | category == 2 ~ 0,
    category == 3 | category == 4 | category == 5 | category == 6 ~ 1,
    TRUE ~ NA_real_
  )
  return(floor_qual)
} # Consider making MARBLE and GRANITE as high-income floor?

#Wall Quality
wall_qual_convert <- function(category) {
  wall_qual <- case_when(
    category == 1 | category == 2 | category == 3 | category == 4 ~ 0,
    category == 5 | category == 6 | category == 7 | category == 8 ~ 1,
    TRUE ~ NA_real_
  )
  return(wall_qual)
}

#Wall Quality
roof_qual_convert <- function(category) {
  roof_qual <- case_when(
    category == 1 | category == 2 | category == 3 | category == 4 ~ 0,
    category == 5 | category == 6 | category == 7 | category == 8 ~ 1,
    TRUE ~ NA_real_
  )
  return(roof_qual)
}

##HOUSING
sdim_housing <- sdim %>%
  mutate(floorqual = floor_qual_convert(.$SDI_24)) %>%
  mutate(wallqual = wall_qual_convert(.$SDI_25)) %>%
  mutate(roofqual = roof_qual_convert(.$SDI_26)) %>%
  mutate(housing = case_when(
    roofqual == 1 & wallqual == 1 & floorqual == 1 ~ 0,  #replace housing=0 if roofqual==1 & wallqual==1 & floorqual==1
    roofqual==0 | wallqual==0 | floorqual==0 ~ 1, #replace housing=1 if roofqual==0 | wallqual==0 | floorqual==0
    TRUE ~ NA_real_#replace housing=666 if roofqual==999 & wallqual==999 & floorqual==999
  )) %>%
##URBAN/RURAL/SLUM
  mutate(urbanisation = case_when(
    sdim$SDI_13 == 1  | sdim$SDI_12 == 1 ~ "slum/rural",
    sdim$SDI_12 == 2 ~ "urban",
    TRUE ~ NA_character_
  ) %>% as.factor() %>% relevel(., ref = "urban")) %>%
##HOME OWNERSHIP - if you own a house (0), if you don't (1) - based on sdi_11 from the sdim questionnaire
  mutate(homeown = case_when(
    sdim$SDI_11 == 2 ~ 1,
    sdim$SDI_11 == 1 ~ 0,
    TRUE ~ NA_real_
  ) %>% as.factor())

#MERGE Exposure, Outcomes & Covariates with Recruitment Data (C2 and C3 subset only)
cveda <- recruitment %>%
  filter(age.band == "C2" | age.band == "C3") %>%
  left_join(scamp_processed, by = c("PSC2" = "User.code")) %>%
  left_join(sdq_all, by = c("PSC2" = "User.code")) %>%
  left_join(all_mini, by = c("PSC2" = "User.code")) %>%
  left_join(sdim_housing, by = c("PSC2" = "User.code")) %>%
  mutate(housing = as.factor(housing))

#Convert missing numbers to NA
cveda[cveda == -666] <- NA
cveda[cveda == -777] <- NA

#Select columns for analysis
cveda <- cveda %>%
  select(PSC2, recruitment.centre, sex, age.band, baseline.assessment.age.in.days, 
        calls_self_weekday, calls_self_weekend, im_weekday, im_weekend, sns_weekday, sns_weekend,
        internet_weekday, internet_weekend, calls_other_weekday,calls_other_weekend, total_mp_weekday, 
        total_mp_weekend, total_mp_full_week, sns_full_week, im_full_week, internet_full_week,
        SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL,                  
        SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES,
        anxiety, depress, soc.anxiety, gen.anxiety,
        floorqual, wallqual, roofqual,                       
        housing, urbanisation, homeown)

write.csv(cveda, "cveda_stata_import.csv")







