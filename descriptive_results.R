source("data_prep2.R")
cveda[cveda == -666] <- NA
cveda[cveda == -777] <- NA

cveda$sex <- relevel(cveda$sex, "M")
cveda <- cveda %>%
  mutate_at(c("anxiety", "depress", "soc.anxiety", "gen.anxiety", "suicide"), list(as.factor)) %>%
  mutate_at(vars(contains("SCAMP")), list(as.factor))

#Sociodemographics Table
##Calculate Age Range & Median
summary(cveda$baseline.assessment.age.in.days/365.25)
summary(cveda$baseline.assessment.age.in.days[cveda$age.band == "C2"]/365.25)
summary(cveda$baseline.assessment.age.in.days[cveda$age.band == "C3"]/365.25)
summary(cveda$baseline.assessment.age.in.days[cveda$sex == "F"]/365.25)
summary(cveda$baseline.assessment.age.in.days[cveda$sex == "M"]/365.25)

#Calculate geometrics
table(cveda$urbanisation, useNA = "ifany")
prop.table(table(cveda$urbanisation, useNA = "ifany"))

table(cveda$urbanisation[cveda$sex == "M"], useNA = "ifany")
prop.table(table(cveda$urbanisation[cveda$sex == "M"], useNA = "ifany"))

table(cveda$urbanisation[cveda$sex == "F"], useNA = "ifany")
prop.table(table(cveda$urbanisation[cveda$sex == "M"], useNA = "ifany"))

table(cveda$urbanisation[cveda$age.band == "C2"], useNA = "ifany")
prop.table(table(cveda$urbanisation[cveda$age.band == "C2"], useNA = "ifany"))

table(cveda$urbanisation[cveda$age.band == "C3"], useNA = "ifany")
prop.table(table(cveda$urbanisation[cveda$age.band == "C3"], useNA = "ifany"))

#Housing
table(cveda$housing, useNA = "ifany")
table(cveda$housing[cveda$sex == "M"], useNA = "ifany")
table(cveda$housing[cveda$sex == "F"], useNA = "ifany")

table(cveda$housing[cveda$age.band == "C2"], useNA = "ifany")
table(cveda$housing[cveda$age.band == "C3"], useNA = "ifany")

#Home Ownership
table(cveda$homeown, useNA = "ifany")
table(cveda$homeown[cveda$sex == "M"], useNA = "ifany")
table(cveda$homeown[cveda$sex == "F"], useNA = "ifany")

table(cveda$homeown[cveda$age.band == "C2"], useNA = "ifany")
table(cveda$homeown[cveda$age.band == "C3"], useNA = "ifany")

#Extract Age Data by Recruitment Site
age_centre_data <- matrix(ncol=16, nrow=7)
i = 0
for (centre in unique(cveda$recruitment.centre)){
  i = i + 1
  centre_summary <- summary(cveda$baseline.assessment.age.in.days[cveda[,"recruitment.centre"]==centre]/365.25) %>% round(., 2)
  centre_male_summary <- summary(cveda$baseline.assessment.age.in.days[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="M"]/365.25) %>% round(., 2)
  centre_female_summary <- summary(cveda$baseline.assessment.age.in.days[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="F"]/365.25) %>% round(., 2)
  centre_c2_summary <- summary(cveda$baseline.assessment.age.in.days[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C2"]/365.25) %>% round(., 2)
  centre_c3_summary <- summary(cveda$baseline.assessment.age.in.days[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C3"]/365.25) %>% round(., 2)
  centre_row <- c(
    # TOTAL AGE STATS
    centre, 
    paste0(centre_summary[1], " - ", centre_summary[6]), 
    centre_summary[3], 
    paste0(centre_summary[2], " - ", centre_summary[5]),
    # MALE AGE STATS
    paste0(centre_male_summary[1], " - ", centre_male_summary[6]), 
    centre_male_summary[3], 
    paste0(centre_male_summary[2], " - ", centre_male_summary[5]),
    #FEMALE AGE STATS
    paste0(centre_female_summary[1], " - ", centre_female_summary[6]), 
    centre_female_summary[3], 
    paste0(centre_female_summary[2], " - ", centre_female_summary[5]),
    #C2 AGE STATS
    paste0(centre_c2_summary[1], " - ", centre_c2_summary[6]), 
    centre_c2_summary[3], 
    paste0(centre_c2_summary[2], " - ", centre_c2_summary[5]),
    #C3 AGE STATS
    paste0(centre_c3_summary[1], " - ", centre_c3_summary[6]), 
    centre_c3_summary[3], 
    paste0(centre_c3_summary[2], " - ", centre_c3_summary[5])
                  )
  age_centre_data[i,] <- centre_row
}

#Extract Geometrics by Recruitment Site
urban_centre_data <- list()
for (centre in unique(cveda$recruitment.centre)){
  print(centre)
  i = i + 1
  #urban / ruralorslum / missing  
  geometrics <- table(cveda$urbanisation[cveda[,"recruitment.centre"]==centre], useNA = "always") 
  geometrics_male <- table(cveda$urbanisation[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="M"], useNA = "always") 
  geometrics_female <- table(cveda$urbanisation[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="F"], useNA = "always") 
  geometrics_c2 <- table(cveda$urbanisation[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C2"], useNA = "always")
  geometrics_c3 <- table(cveda$urbanisation[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C3"], useNA = "always")
  # housing - high qual, low qual / missing
  housing_quality <- table(cveda$housing[cveda[,"recruitment.centre"]==centre], useNA = "always") %>% as.vector()
  housing_quality_male <- table(cveda$housing[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="M"], useNA = "always")
  housing_quality_female <- table(cveda$housing[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="F"], useNA = "always")
  housing_quality_c2 <- table(cveda$housing[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C2"], useNA = "always")
  housing_quality_c3 <- table(cveda$housing[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C3"], useNA = "always")
  # homeownership - own / rent / missing
  homeown <- table(cveda$homeown[cveda[,"recruitment.centre"]==centre], useNA = "always") %>% as.vector()
  homeown_male <- table(cveda$homeown[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="M"], useNA = "always")
  homeown_female <- table(cveda$homeown[cveda[,"recruitment.centre"]==centre & cveda[,"sex"]=="M"], useNA = "always") 
  homeown_c2 <- table(cveda$homeown[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C2"], useNA = "always")
  homeown_c3 <- table(cveda$homeown[cveda[,"recruitment.centre"]==centre & cveda[,"age.band"]=="C3"], useNA = "always")
  urban_centre_data[[centre]] <- data.frame(
    centre = rep(centre, 9),
    covariate = c("Urban", "Slum/Rural", "Missing", "High Quality", "Low Quality", "Missing", "Own Home", "Rent", "Missing"),
    n_total = c(geometrics, housing_quality, homeown),
    prop_total = c(prop.table(geometrics), prop.table(housing_quality), prop.table(homeown)) %>% round(.,4),
    n_male = c(geometrics_male, housing_quality_male, homeown_male),
    prop_male = c(prop.table(geometrics_male), prop.table(housing_quality_male), prop.table(homeown_male)) %>% round(.,4),
    n_female = c(geometrics_female, housing_quality_female, homeown_female),
    prop_female = c(prop.table(geometrics_female), prop.table(housing_quality_female), prop.table(homeown_female)) %>% round(.,4),
    n_c2 = c(geometrics_c2, housing_quality_c2, homeown_c2),
    prop_c2 = c(prop.table(geometrics_c2), prop.table(housing_quality_c2), prop.table(homeown_c2)) %>% round(.,4),
    n_c3 = c(geometrics_c3, housing_quality_c3, homeown_c3),
    prop_c3 = c(prop.table(geometrics_c3), prop.table(housing_quality_c3), prop.table(homeown_c3)) %>% round(.,4)
  )
}

ses_centre <- rbind(
  urban_centre_data[["IMPHAL"]], 
  urban_centre_data[["KOLKATA"]], 
  urban_centre_data[["MYSORE"]], 
  urban_centre_data[["NIMHANS"]],
  urban_centre_data[["PGIMER"]], 
  urban_centre_data[["RISHIVALLEY"]],
  urban_centre_data[["SJRI"]])

#Use arsenal package to create sociodemographic tables

#Create SDQ Descriptive Tables

#Whole Sample
sdq <- cveda %>%
  select(recruitment.centre, sex, SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL, 
         SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES)

sdq_sd <- sapply(sdq[,-c(1,2)], function(x) sd(x, na.rm = TRUE))

sdq_summary <- t(sapply(sdq[,-c(1,2)], summary)) %>%
  cbind(., Sd=sdq_sd)

#SDQ Breakdown by sex and age group only
my_controls <- arsenal::tableby.control(
  numeric.stats = c("meansd", "Nmiss"),
  cat.stats=c("countpct", "Nmiss"),
  digits = 2
)
sdq_sex_table <- arsenal::tableby(sex ~ SDQ_EMO_PROB + SDQ_COND_PROB + SDQ_HYPER + SDQ_PEER_PROB + SDQ_PROSOCIAL + SDQ_TOTAL_DIFFICULTIES,
                                  data=cveda, control = my_controls)
summary(sdq_sex_table, title = "Strengths and Difficulties Questionnaire", text = TRUE)

#SDQ Breakdown by age group
sdq_band_table <- arsenal::tableby(age.band ~ SDQ_EMO_PROB + SDQ_COND_PROB + SDQ_HYPER + SDQ_PEER_PROB + SDQ_PROSOCIAL + SDQ_TOTAL_DIFFICULTIES,
                                  data=cveda, control = my_controls)
summary(sdq_band_table, title = "Strengths and Difficulties Questionnaire", text = TRUE)

#SDQ Breakdown by recruitment site
sdq_centre_table <- arsenal::tableby(recruitment.centre ~ SDQ_EMO_PROB + SDQ_COND_PROB + SDQ_HYPER + SDQ_PEER_PROB + SDQ_PROSOCIAL + SDQ_TOTAL_DIFFICULTIES,
                                   data=cveda, control = my_controls)
summary(sdq_centre_table, title = "Strengths and Difficulties Questionnaire Results by Recruitment Centre", text = TRUE)

# sdq_sex_csv <- summary(sdq_sex_table, title = "Strengths and Difficulties Questionnaire", text = NULL)
# sdq_band_csv <- summary(sdq_band_table, title = "Strengths and Difficulties Questionnaire", text = NULL)
# sdq_centre_csv <- summary(sdq_centre_table, title = "Strengths and Difficulties Questionnaire", text = NULL)
# write.csv(sdq_sex_csv, "Outputs/sdq_sex.csv")
# write.csv(sdq_band_csv, "Outputs/sdq_ageband.csv")
# write.csv(sdq_centre_csv, "Outputs/sdq_centre.csv")

#SDQ Descriptive Breakdown by Sex and then looped by recruitment site
sdq_by_sex_centre <- list()
for (centre in unique(cveda$recruitment.centre)){
  temp_table <- arsenal::tableby(
    sex ~ SDQ_EMO_PROB + SDQ_COND_PROB + SDQ_HYPER + SDQ_PEER_PROB + SDQ_PROSOCIAL + SDQ_TOTAL_DIFFICULTIES,
    data=cveda[cveda[,"recruitment.centre"]==centre,], 
    control = my_controls)
  sdq_by_sex_centre[[centre]] <- summary(temp_table, text=NULL)
}

#Anxiety / Depression Prevalence

#Whole Sample
mini <- cveda %>%
  select(recruitment.centre, sex, anxiety, depress, gen.anxiety, soc.anxiety)

mini_sex_table <- arsenal::tableby(sex ~ anxiety + depress + gen.anxiety + soc.anxiety, control = my_controls, data= mini)
summary(mini_sex_table, title = "Internalizing Disorder Prevalence", text = TRUE)

#Int Disorder Breakdown by age group
mini_band_table <- arsenal::tableby(age.band ~ anxiety + depress + gen.anxiety + soc.anxiety,
                                   data=cveda, control = my_controls)
summary(mini_band_table, title = "Internalizing Disorder Prevalence by Age Band", text = TRUE)

#Int Disorder by recruitment site
mini_centre_table <- arsenal::tableby(recruitment.centre ~ anxiety + depress + gen.anxiety + soc.anxiety,
                                     data=cveda, control = my_controls)
summary(mini_centre_table, title = "Internalizing Disorder Prevalence by Recruitment Centre", text = TRUE)

# mini_sex_csv <- summary(sdq_sex_table, title = "Strengths and Difficulties Questionnaire", text = NULL)
# mini_band_csv <- summary(sdq_band_table, title = "Strengths and Difficulties Questionnaire", text = NULL)
# mini_centre_csv <- summary(sdq_centre_table, title = "Strengths and Difficulties Questionnaire", text = NULL)
# write.csv(mini_sex_csv, "Outputs/mini_sex.csv")
# write.csv(mini_band_csv, "Outputs/mini_ageband.csv")
# write.csv(mini_centre_csv, "Outputs/mini_centre.csv")

#Int Disorder Descriptive Breakdown by Sex and then looped by recruitment site
mini_by_sex_centre <- list()
for (centre in unique(cveda$recruitment.centre)){
  temp_table <- arsenal::tableby(
    sex ~ anxiety + depress + gen.anxiety + soc.anxiety,
    data=cveda[cveda[,"recruitment.centre"]==centre,], 
    control = my_controls)
  mini_by_sex_centre[[centre]] <- summary(temp_table, text=NULL)
}

mini_by_sex_centre2 <- tableby(list(sex, age.band) ~ anxiety + depress + gen.anxiety + soc.anxiety,
                              strata = recruitment.centre, data = cveda, control = my_controls)
summary(mini_by_sex_centre2, test = TRUE)


#EXPOSURE

#Create function  to convert variables based on participants who are not current phone owners and 
#who have never used mobile phones to 
convert_no_owners <- function(ever_used, ownership, category) {
  return(
    if_else(
      ever_used == 0 | ownership == 1 | ownership == 0,
      "do not own",
      as.character(category)
    ) %>% as.factor()
  )
}

convert_no_internet <- function(internet, category) {
  return(
    if_else(
      internet == 4 | internet == 5,
      "no internet",
      as.character(category)
    ) %>% as.factor()
  )
}

#Add labels for descriptive tables
scamp_labels <- as.data.frame(readxl::read_xlsx("SCAMP_labels.xlsx", sheet="Sheet1", col_names = TRUE)) %>%
  mutate_all(as.character)

raw_exposure <- recruitment %>%
  filter(age.band == "C2" | age.band == "C3") %>%
  left_join(scamp, by = c("PSC2" = "User.code")) %>%
  select(recruitment.centre, age.band, sex, 
         SCAMP_S_q1, SCAMP_S_q3, SCAMP_S_q10_1, SCAMP_S_q10_2,
         SCAMP_S_q16_1, SCAMP_S_q16_2, SCAMP_S_q17_1, SCAMP_S_q17_2, 
         SCAMP_S_q18, SCAMP_S_q20_1, SCAMP_S_q20_2, SCAMP_S_q26_1, SCAMP_S_q26_2) %>%
  mutate(SCAMP_S_q3 = as.factor(if_else(.$SCAMP_S_q1 == 0, "never used", as.character(.$SCAMP_S_q3)))) %>%
  mutate(SCAMP_S_q26_1 = as.factor(if_else(.$SCAMP_S_q1 == 0, "never used", as.character(.$SCAMP_S_q26_1)))) %>%
  mutate(SCAMP_S_q10_1 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q10_1)) %>%
  mutate(SCAMP_S_q16_1 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q16_1)) %>%
  mutate(SCAMP_S_q17_1 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q17_1)) %>%
  mutate(SCAMP_S_q18 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q18)) %>%
  mutate(SCAMP_S_q20_1 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q20_1)) %>%
  mutate(SCAMP_S_q26_2 = as.factor(if_else(.$SCAMP_S_q1 == 0, "never used", as.character(.$SCAMP_S_q26_2)))) %>%
  mutate(SCAMP_S_q10_2 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q10_2)) %>%
  mutate(SCAMP_S_q16_2 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q16_2)) %>%
  mutate(SCAMP_S_q17_2 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q17_2)) %>%
  mutate(SCAMP_S_q20_2 = convert_no_owners(.$SCAMP_S_q1, .$SCAMP_S_q3, .$SCAMP_S_q20_2)) %>%
  mutate(SCAMP_S_q20_1 = convert_no_internet(.$SCAMP_S_q18, .$SCAMP_S_q20_1)) %>%
  mutate(SCAMP_S_q20_2 = convert_no_internet(.$SCAMP_S_q18, .$SCAMP_S_q20_2)) %>%
  mutate_at(vars(-(c("recruitment.centre", "age.band", "sex"))), as.character)

raw_exposure[raw_exposure == -777] <- NA

#Change labels of SCAMP variables and order factors in correct order (increasing exposure)
for (col in colnames(raw_exposure[-c(1:3)])){
  print(col)
  lookup <- scamp_labels[scamp_labels$Variable == col,]
  print(lookup)
  for (category in lookup$Category){
    print(category)
    raw_exposure[,col][raw_exposure[,col] == category] <- lookup$Label[lookup$Category == category]
  }
  raw_exposure[,col] <- as.factor(raw_exposure[,col])
}

raw_exposure$SCAMP_S_q3 <- forcats::fct_relevel(raw_exposure$SCAMP_S_q3, 
  c("never used", scamp_labels$Label[scamp_labels$Variable == "SCAMP_S_q3"]))

raw_exposure$SCAMP_S_q26_1 <- forcats::fct_relevel(raw_exposure$SCAMP_S_q26_1, 
  c("never used", scamp_labels$Label[scamp_labels$Variable == "SCAMP_S_q26_1"]))

raw_exposure$SCAMP_S_q26_2 <- forcats::fct_relevel(raw_exposure$SCAMP_S_q26_2, 
  c("never used", scamp_labels$Label[scamp_labels$Variable == "SCAMP_S_q26_2"]))

raw_exposure$SCAMP_S_q20_1 <- forcats::fct_relevel(raw_exposure$SCAMP_S_q20_1, 
  c("do not own", "no internet", scamp_labels$Label[scamp_labels$Variable == "SCAMP_S_q20_1"]))

raw_exposure$SCAMP_S_q20_2 <- forcats::fct_relevel(raw_exposure$SCAMP_S_q20_2, 
  c("do not own", "no internet", scamp_labels$Label[scamp_labels$Variable == "SCAMP_S_q20_2"]))

for(col in colnames(raw_exposure[c(6:11,13,14)])){
  print(col)
  raw_exposure[,col] <- forcats::fct_relevel(raw_exposure[,col], 
  c("do not own", scamp_labels$Label[scamp_labels$Variable == col]))
}

#Add labels for exposure descriptive tables
arsenal::labels(raw_exposure) <- c(
  SCAMP_S_q1 = "Ever used mobile phone?", 
  SCAMP_S_q3 = "Currently own mobile phone?",
  SCAMP_S_q18 = "Use phone to connect to internet?",
  SCAMP_S_q10_1 = "Duration of calls on own phone on weekdays, mins",
  SCAMP_S_q10_2 = "Duration of calls on own phone on weekends, mins",
  SCAMP_S_q16_1 = "Duration of instant messaging on weekdays, mins",
  SCAMP_S_q16_2 = "Duration of instant messaging on weekends, mins",
  SCAMP_S_q17_1 = "Duration of SNS use on weekdays, mins",
  SCAMP_S_q17_2 = "Duration of SNS use on weekends, mins",
  SCAMP_S_q20_1 = "Duration of online internet use on weekdays, mins",
  SCAMP_S_q20_2 = "Duration of online internet use on weekends, mins",
  SCAMP_S_q26_1 = "Duration of calls on relative's phone on weekdays, mins",
  SCAMP_S_q26_2 = "Duration of calls on relative's phone on weekends, mins"
  )

exposure_sex_table <- arsenal::tableby(sex ~
  SCAMP_S_q1 + SCAMP_S_q3 + SCAMP_S_q18 +
  SCAMP_S_q10_1 + SCAMP_S_q10_2 + 
  SCAMP_S_q16_1 + SCAMP_S_q16_2 +
  SCAMP_S_q17_1 + SCAMP_S_q17_2 + 
  SCAMP_S_q20_1 + SCAMP_S_q20_2 +
  SCAMP_S_q26_1 + SCAMP_S_q26_2,
  control = my_controls, data= raw_exposure)
summary(exposure_sex_table, title = "Exposure by Sex", text = TRUE)

exposure_band_table <- arsenal::tableby(age.band ~
                                         SCAMP_S_q1 + SCAMP_S_q3 + SCAMP_S_q18 +
                                         SCAMP_S_q10_1 + SCAMP_S_q10_2 + 
                                         SCAMP_S_q16_1 + SCAMP_S_q16_2 +
                                         SCAMP_S_q17_1 + SCAMP_S_q17_2 + 
                                         SCAMP_S_q20_1 + SCAMP_S_q20_2 +
                                         SCAMP_S_q26_1 + SCAMP_S_q26_2,
                                       control = my_controls, data= raw_exposure)
summary(exposure_band_table, title = "Exposure by Age Band", text = TRUE)

exposure_centre_table <- arsenal::tableby(recruitment.centre ~
                                          SCAMP_S_q1 + SCAMP_S_q3 + SCAMP_S_q18 +
                                          SCAMP_S_q10_1 + SCAMP_S_q10_2 + 
                                          SCAMP_S_q16_1 + SCAMP_S_q16_2 +
                                          SCAMP_S_q17_1 + SCAMP_S_q17_2 + 
                                          SCAMP_S_q20_1 + SCAMP_S_q20_2 +
                                          SCAMP_S_q26_1 + SCAMP_S_q26_2,
                                        control = my_controls, data= raw_exposure)
summary(exposure_centre_table, title = "Exposure by Recruitment Centre", text = TRUE)

#Create SDQ Tables C2 ONLY
c2 <- cveda[cveda$age.band == "C2",]
c2_descript <- c2 %>%
  select(recruitment.centre, sex, SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL, 
         SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES)

c2_summary <- t(sapply(c2_descript[,-c(1,2)], summary))
c2_sd <- sapply(c2_descript[,-c(1,2)], function(x) sd(x, na.rm = TRUE))
c2_summary <- cbind(c2_summary, sd=c2_sd)

c2_summary_sex <- c2_descript[,-1] %>%
  group_by(sex) %>%
  summarise_all(list(mean = function(x) mean(x, na.rm = TRUE), sd = function(x) sd(x, na.rm = TRUE))) %>%
  as.data.frame()

c2_summary_sex_missing <- c2_descript[,-1] %>%
  group_by(sex) %>%
  summarise_all(list(function(x) sum(is.na(x)))) %>%
  as.data.frame()

c2_summary_centre <- c2_descript[,-2] %>%
  group_by(recruitment.centre) %>%
  summarise_all(list(mean = function(x) mean(x, na.rm = TRUE), sd = function(x) sd(x, na.rm = TRUE))) %>%
  as.data.frame()

#Create SDQ Tables C3 ONLY
c3 <- cveda[cveda$age.band == "C3",]
c3_descript <- c3 %>%
  select(recruitment.centre, sex, SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL, 
         SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES)

c3_summary <- t(sapply(c3_descript[,-c(1,2)], summary))
c3_sd <- sapply(c3_descript[,-c(1,2)], function(x) sd(x, na.rm = TRUE))
c3_summary <- cbind(c3_summary, sd=c3_sd)

c3_summary_sex <- c3_descript[,-1] %>%
  group_by(sex) %>%
  summarise_all(list(mean = function(x) mean(x, na.rm = TRUE), sd = function(x) sd(x, na.rm = TRUE))) %>%
  as.data.frame()

c3_summary_sex_missing <- c3_descript[,-1] %>%
  group_by(sex) %>%
  summarise_all(list(function(x) sum(is.na(x)))) %>%
  as.data.frame()

c3_summary_centre <- c3_descript[,-2] %>%
  group_by(recruitment.centre) %>%
  summarise_all(list(mean = function(x) mean(x, na.rm = TRUE), sd = function(x) sd(x, na.rm = TRUE))) %>%
  as.data.frame()




