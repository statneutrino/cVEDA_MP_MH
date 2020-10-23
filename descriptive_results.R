source("data_prep2.R")
cveda[cveda == -666] <- NA
cveda[cveda == -777] <- NA

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

#Create SDQ Descriptive Tables
sdq <- cveda %>%
  select(recruitment.centre, sex, SDQ_EMO_PROB, SDQ_COND_PROB, SDQ_HYPER, SDQ_PEER_PROB, SDQ_PROSOCIAL, 
         SDQ_EXTERNALIZING, SDQ_INTERNALIZING, SDQ_TOTAL_DIFFICULTIES)

sdq_sd <- sapply(sdq[,-c(1,2)], function(x) sd(x, na.rm = TRUE))

sdq_summary <- t(sapply(sdq[,-c(1,2)], summary)) %>%
  cbind(., Sd=sdq_sd)


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
