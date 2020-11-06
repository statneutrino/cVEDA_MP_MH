** import data
import delimited "C:\Users\advsp\OneDrive - Imperial College London\PhD\cVEDA\Analysis\cVEDA_R_Analysis\cveda_stata_import.csv", stringcols(3 4 5 39) numericcols(7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 40)

**create age column**
gen age = baselineassessmentageindays / 365.25

**create sex dummy variable: gender**
gen gender = 0
replace gender = 1 if sex == "F"

**create geomatrics dummy var from urbanisation**
gen geometrics = 0
replace geometrics = 1 if urbanisation == "slum/rural"
replace geometrics = . if urbanisation == "NA"

**create test variable in hours
ge mp_hour = total_mp_full_week / 60

**create dummy var for age band**
gen band = 0
replace band = 1 if ageband == "C3"

** meta analysis SDQ **

**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
**child maltreatment
regress sdq_total_difficulties total_mp_full_week
bysort recruitmentcentre: regress sdq_total_difficulties total_mp_full_week

**run same model but with confounders
regress sdq_total_difficulties total_mp_full_week housing geometrics age gender
bysort recruitmentcentre: regress sdq_total_difficulties total_mp_full_week housing geometrics homeown age gender

**RUN IPD META-ANLYSIS FOR SDQ**
ipdmetan, study(recruitmentcentre) re(hk): regress sdq_total_difficulties mp_hour housing geometrics homeown age gender

** meta analysis MINI **

**First run logistic regression for Depressoin, first for unadjusted results, overall and then per site**
logit depress mp_hour housing geometrics age gender
bysort recruitmentcentre: logit depress mp_hour housing geometrics age gender homeown

**RUN IPD META-ANLYSIS FOR DEPRESSION**
ipdmetan, study(recruitmentcentre) or re(hk): logit depress mp_hour housing geometrics homeown age gender

**run again but without SJRI**
gen rc = recruitmentcentre
replace rc = "" if recruitmentcentre == "SJRI"
ipdmetan, study(rc) or re(hk): firthlogit depress mp_hour housing geometrics homeown age gender



