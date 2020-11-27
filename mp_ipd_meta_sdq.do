
import delimited "C:\Users\advsp\OneDrive - Imperial College London\PhD\cVEDA\Analysis\cVEDA_R_Analysis\cveda_stata_import.csv", numericcols(7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 43) 

**ensure homeown NA are coded as missing
destring homeown, replace force

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
gen mp_daily_avg = total_mp_full_week
gen sns_daily_avg = sns_full_week
gen im_daily_avg = im_full_week
gen internet_daily_avg = internet_full_week

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

** meta analysis SDQ for TOTAL MP USAGE**

*TOTAL DIFFICULTIES
**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
regress sdq_total_difficulties mp_daily_avg
bysort rc: regress sdq_total_difficulties mp_daily_avg

**run same model but with confounders
regress sdq_total_difficulties mp_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_total_difficulties mp_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for TOTAL DIFFICULTIES
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_total_difficulties mp_daily_avg housing geometrics homeown age gender

*EMOTIONAL PROBLEMS
**run linear regression for SDQ emotional problems, first for unadjusted results, overall and then per site**
regress sdq_emo_prob mp_daily_avg
bysort rc: regress sdq_emo_prob mp_daily_avg

**run same model but with confounders
regress sdq_emo_prob mp_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_emo_prob mp_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for EMOTIONAL PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_emo_prob mp_daily_avg housing geometrics homeown age gender

*CONDUCT PROBLEMS
**run linear regression for SDQ conduct problems, first for unadjusted results, overall and then per site**
regress sdq_cond_prob mp_daily_avg
bysort rc: regress sdq_cond_prob mp_daily_avg

**run same model but with confounders
regress sdq_cond_prob mp_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_cond_prob mp_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for CONDUCT PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_cond_prob mp_daily_avg housing geometrics homeown age gender

*HYPERACTIVITY/INATTENTION PROBLEMS
**run linear regression for SDQ emotional problems, first for unadjusted results, overall and then per site**
regress sdq_hyper mp_daily_avg
bysort rc: regress sdq_hyper mp_daily_avg

**run same model but with confounders
regress sdq_hyper mp_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_hyper mp_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for HYPERACTIVITY/INATTENTION  PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_hyper mp_daily_avg housing geometrics homeown age gender

*PEER PROBLEMS
**run linear regression for SDQ peer problems, first for unadjusted results, overall and then per site**
regress sdq_peer_prob mp_daily_avg
bysort rc: regress sdq_peer_prob mp_daily_avg

**run same model but with confounders
regress sdq_peer_prob mp_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_peer_prob mp_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for PEER PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_peer_prob mp_daily_avg housing geometrics homeown age gender

*PROSOCIAL
**run linear regression for SDQ prosocial, first for unadjusted results, overall and then per site**
regress sdq_prosocial mp_daily_avg
bysort rc: regress sdq_prosocial mp_daily_avg

**run same model but with confounders
regress sdq_prosocial mp_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_prosocial mp_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for PROSOCIAL
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_prosocial mp_daily_avg housing geometrics homeown age gender






*** NEXT EXPOSURE

** meta analysis SDQ for TOTAL SOCIAL MEDIA / SOCIAL NETWORK USAGE**

*TOTAL DIFFICULTIES
**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
regress sdq_total_difficulties sns_daily_avg
bysort rc: regress sdq_total_difficulties sns_daily_avg

**run same model but with confounders
regress sdq_total_difficulties sns_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_total_difficulties sns_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for TOTAL DIFFICULTIES
ipdmetan, study(rc) re(hk)  forestplot( range(-4 4) xlabel(-4 -2 0 2 4) ysize(3)): regress sdq_total_difficulties sns_daily_avg housing geometrics homeown age gender

*EMOTIONAL PROBLEMS
**run linear regression for SDQ emotional problems, first for unadjusted results, overall and then per site**
regress sdq_emo_prob sns_daily_avg
bysort rc: regress sdq_emo_prob sns_daily_avg

**run same model but with confounders
regress sdq_emo_prob sns_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_emo_prob sns_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for EMOTIONAL PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_emo_prob sns_daily_avg housing geometrics homeown age gender

*CONDUCT PROBLEMS
**run linear regression for SDQ conduct problems, first for unadjusted results, overall and then per site**
regress sdq_cond_prob sns_daily_avg
bysort rc: regress sdq_cond_prob sns_daily_avg

**run same model but with confounders
regress sdq_cond_prob sns_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_cond_prob sns_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for CONDUCT PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_cond_prob sns_daily_avg housing geometrics homeown age gender

*HYPERACTIVITY/INATTENTION PROBLEMS
**run linear regression for SDQ HYPERACTIVITY/INATTENTION problems, first for unadjusted results, overall and then per site**
regress sdq_hyper sns_daily_avg
bysort rc: regress sdq_hyper sns_daily_avg

**run same model but with confounders
regress sdq_hyper sns_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_hyper sns_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for HYPERACTIVITY/INATTENTION  PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-4 4) xlabel(-4 -2 0 2 4) ysize(3)): regress sdq_hyper sns_daily_avg housing geometrics homeown age gender

*PEER PROBLEMS
**run linear regression for SDQ peer problems, first for unadjusted results, overall and then per site**
regress sdq_peer_prob sns_daily_avg
bysort rc: regress sdq_peer_prob sns_daily_avg

**run same model but with confounders
regress sdq_peer_prob sns_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_peer_prob sns_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for PEER PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_peer_prob sns_daily_avg housing geometrics homeown age gender

*PROSOCIAL
**run linear regression for SDQ prosocial, first for unadjusted results, overall and then per site**
regress sdq_prosocial sns_daily_avg
bysort rc: regress sdq_prosocial sns_daily_avg

**run same model but with confounders
regress sdq_prosocial sns_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_prosocial sns_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for PROSOCIAL
ipdmetan, study(rc) re(hk)  forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_prosocial sns_daily_avg housing geometrics homeown age gender




*** NEXT EXPOSURE

** meta analysis SDQ for INSTANT MESSAGING**

*TOTAL DIFFICULTIES
**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
regress sdq_total_difficulties im_daily_avg
bysort rc: regress sdq_total_difficulties im_daily_avg

**run same model but with confounders
regress sdq_total_difficulties im_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_total_difficulties im_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for TOTAL DIFFICULTIES
ipdmetan, study(rc) re(hk)  forestplot( range(-4 4) xlabel(-4 -2 0 2 4) ysize(3)): regress sdq_total_difficulties im_daily_avg housing geometrics homeown age gender

*EMOTIONAL PROBLEMS
**run linear regression for SDQ emotional problems, first for unadjusted results, overall and then per site**
regress sdq_emo_prob im_daily_avg
bysort rc: regress sdq_emo_prob im_daily_avg

**run same model but with confounders
regress sdq_emo_prob im_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_emo_prob im_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for EMOTIONAL PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_emo_prob im_daily_avg housing geometrics homeown age gender

*CONDUCT PROBLEMS
**run linear regression for SDQ conduct problems, first for unadjusted results, overall and then per site**
regress sdq_cond_prob im_daily_avg
bysort rc: regress sdq_cond_prob im_daily_avg

**run same model but with confounders
regress sdq_cond_prob im_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_cond_prob im_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for CONDUCT PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_cond_prob im_daily_avg housing geometrics homeown age gender

*HYPERACTIVITY/INATTENTION PROBLEMS
**run linear regression for SDQ HYPERACTIVITY/INATTENTION problems, first for unadjusted results, overall and then per site**
regress sdq_hyper im_daily_avg
bysort rc: regress sdq_hyper im_daily_avg

**run same model but with confounders
regress sdq_hyper im_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_hyper im_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for HYPERACTIVITY/INATTENTION  PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-4 4) xlabel(-4 -2 0 2 4) ysize(3)): regress sdq_hyper im_daily_avg housing geometrics homeown age gender

*PEER PROBLEMS
**run linear regression for SDQ peer problems, first for unadjusted results, overall and then per site**
regress sdq_peer_prob im_daily_avg
bysort rc: regress sdq_peer_prob im_daily_avg

**run same model but with confounders
regress sdq_peer_prob im_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_peer_prob im_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for PEER PROBLEMS
ipdmetan, study(rc) re(hk) forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_peer_prob im_daily_avg housing geometrics homeown age gender

*PROSOCIAL
**run linear regression for SDQ prosocial, first for unadjusted results, overall and then per site**
regress sdq_prosocial im_daily_avg
bysort rc: regress sdq_prosocial im_daily_avg

**run same model but with confounders
regress sdq_prosocial im_daily_avg housing geometrics homeown age gender
bysort rc: regress sdq_prosocial im_daily_avg housing geometrics homeown age gender

**run IPD meta-analysis for PROSOCIAL
ipdmetan, study(rc) re(hk)  forestplot( range(-2 2) xlabel(-2 -1 0 1 2) ysize(3)): regress sdq_prosocial im_daily_avg housing geometrics homeown age gender



**ALSO INCLUDE CALLS ONLY!
 
**FIRST FOR SDQ OUTCOMES

*TOTAL DIFFICULTIES
**run linear regression for SDQ total difficulties, first for unadjusted results, overall and then per site**
regress sdq_total_difficulties calls_full_week
bysort rc: regress sdq_total_difficulties calls_full_week

**run same model but with confounders
regress sdq_total_difficulties calls_full_week housing geometrics homeown age gender
bysort rc: regress sdq_total_difficulties calls_full_week housing geometrics homeown age gender

**run IPD meta-analysis for TOTAL DIFFICULTIES
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_total_difficulties calls_full_week housing geometrics homeown age gender

*EMOTIONAL PROBLEMS
**run linear regression for SDQ emotional problems, first for unadjusted results, overall and then per site**
regress sdq_emo_prob calls_full_week
bysort rc: regress sdq_emo_prob calls_full_week

**run same model but with confounders
regress sdq_emo_prob calls_full_week housing geometrics homeown age gender
bysort rc: regress sdq_emo_prob calls_full_week housing geometrics homeown age gender

**run IPD meta-analysis for EMOTIONAL PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_emo_prob calls_full_week housing geometrics homeown age gender

*CONDUCT PROBLEMS
**run linear regression for SDQ conduct problems, first for unadjusted results, overall and then per site**
regress sdq_cond_prob calls_full_week
bysort rc: regress sdq_cond_prob calls_full_week

**run same model but with confounders
regress sdq_cond_prob calls_full_week housing geometrics homeown age gender
bysort rc: regress sdq_cond_prob calls_full_week housing geometrics homeown age gender

**run IPD meta-analysis for CONDUCT PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_cond_prob calls_full_week housing geometrics homeown age gender

*HYPERACTIVITY/INATTENTION PROBLEMS
**run linear regression for SDQ emotional problems, first for unadjusted results, overall and then per site**
regress sdq_hyper calls_full_week
bysort rc: regress sdq_hyper calls_full_week

**run same model but with confounders
regress sdq_hyper calls_full_week housing geometrics homeown age gender
bysort rc: regress sdq_hyper calls_full_week housing geometrics homeown age gender

**run IPD meta-analysis for HYPERACTIVITY/INATTENTION  PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_hyper calls_full_week housing geometrics homeown age gender

*PEER PROBLEMS
**run linear regression for SDQ peer problems, first for unadjusted results, overall and then per site**
regress sdq_peer_prob calls_full_week
bysort rc: regress sdq_peer_prob calls_full_week

**run same model but with confounders
regress sdq_peer_prob calls_full_week housing geometrics homeown age gender
bysort rc: regress sdq_peer_prob calls_full_week housing geometrics homeown age gender

**run IPD meta-analysis for PEER PROBLEMS
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_peer_prob calls_full_week housing geometrics homeown age gender

*PROSOCIAL
**run linear regression for SDQ prosocial, first for unadjusted results, overall and then per site**
regress sdq_prosocial calls_full_week
bysort rc: regress sdq_prosocial calls_full_week

**run same model but with confounders
regress sdq_prosocial calls_full_week housing geometrics homeown age gender
bysort rc: regress sdq_prosocial calls_full_week housing geometrics homeown age gender

**run IPD meta-analysis for PROSOCIAL
ipdmetan, study(rc) re(hk) forestplot(ysize(3)): regress sdq_prosocial calls_full_week housing geometrics homeown age gender







