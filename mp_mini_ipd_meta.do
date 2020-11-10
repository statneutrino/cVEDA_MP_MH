** import data
import delimited "C:\Users\advsp\OneDrive - Imperial College London\PhD\cVEDA\Analysis\cVEDA_R_Analysis\cveda_stata_import.csv", stringcols(3 4 5 39) numericcols(7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 40)

**create age column**
gen age = baselineassessmentageindays / 365.25

**create sex dummy variable: gender- male as reference category**
gen gender = 0
replace gender = 1 if sex == "F"

**create geomatrics dummy var from urbanisation**
gen geometrics = 0
replace geometrics = 1 if urbanisation == "slum/rural"
replace geometrics = . if urbanisation == "NA"

**transform exposure to hours
gen mp_daily_avg = total_mp_full_week / 60
gen sns_daily_avg = sns_full_week / 60
gen im_daily_avg = im_full_week / 60
gen internet_daily_avg = internet_full_week / 60

**create dummy var for age band**
gen band = 0
replace band = 1 if ageband == "C3"

*change variable labels e.g. for recruitmentcentre
gen rc = "Imphal, Manipur"
replace rc = "Asansol, West Bengal" if recruitmentcentre == "KOLKATA"
replace rc = "Mysore, Karnataka" if recruitmentcentre == "MYSORE"
replace rc = "NIMHANS, Karnataka" if recruitmentcentre == "NIMHANS"
replace rc = "PGIMER, Chandigarh" if recruitmentcentre == "PGIMER"
replace rc = "Rishi Valley, Andhra Pradesh" if recruitmentcentre == "RISHIVALLEY"
replace rc = "St John's Research Institute, Karnataka" if recruitmentcentre == "SJRI"
label variable rc "Recruitment Centre"

** meta analysis MINI Outcomes for TOTAL MP USAGE**

*DEPRESSION
**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
logit depress mp_daily_avg
bysort rc: logit depress mp_daily_avg

**run same model but with confounders
logit depress mp_daily_avg housing geometrics homeown age gender
bysort rc: logit depress mp_daily_avg housing geometrics homeown age gender

**Becuase of perfect prediction - instead use penalized logistic regression - execute for each rc
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "Imphal, Manipur"
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "Asansol, West Bengal"
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "Mysore, Karnataka"
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "NIMHANS, Karnataka"
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "PGIMER, Chandigarh"
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "Rishi Valley, Andhra Pradesh"
firthlogit depress mp_daily_avg housing geometrics homeown age gender if rc == "St John's Research Institute, Karnataka"

**run IPD meta-analysis for Depression
ipdmetan, study(rc) or re(hk): firthlogit depress mp_daily_avg housing geometrics homeown age gender

*ANXIETY
**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
logit anxiety mp_daily_avg
bysort rc: logit anxiety mp_daily_avg

**run same model but with confounders
logit anxiety mp_daily_avg housing geometrics homeown age gender
bysort rc: logit anxiety mp_daily_avg housing geometrics homeown age gender

**Becuase of perfect prediction - instead use penalized logistic regression - execute for each rc
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "Imphal, Manipur"
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "Asansol, West Bengal"
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "Mysore, Karnataka"
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "NIMHANS, Karnataka"
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "PGIMER, Chandigarh"
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "Rishi Valley, Andhra Pradesh"
firthlogit anxiety mp_daily_avg housing geometrics homeown age gender if rc == "St John's Research Institute, Karnataka"

**run IPD meta-analysis for Depression
ipdmetan, study(rc) or re(hk): firthlogit anxiety mp_daily_avg housing geometrics homeown age gender



















