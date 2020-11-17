library(lme4)
library(meta)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
source("data_prep2.R")
cveda[cveda == -666] <- NA
cveda[cveda == -777] <- NA

cveda$sex <- relevel(cveda$sex, "M")
cveda <- cveda %>%
  mutate_at(c("anxiety", "depress", "soc.anxiety", "gen.anxiety", "suicide"), list(as.factor)) %>%
  mutate_at(vars(contains("SCAMP")), list(as.factor))

cveda$mp <- cveda$total_mp_full_week / 60
cveda$age <- cveda$baseline.assessment.age.in.days / 365.25

## LME4 model
mp_age_lme4_model <- lmer(SDQ_TOTAL_DIFFICULTIES ~ mp + age + (mp | recruitment.centre), data=cveda)
int_lme4_model <- lmer(SDQ_TOTAL_DIFFICULTIES ~ mp + age + (1 | recruitment.centre), data=cveda)

summary(mp_age_lme4_model)
coef(mp_age_lme4_model)
confint(mp_age_lme4_model)

## STAN MODEL

cveda_no_missing <- cveda[!is.na(cveda$mp) & !is.na(cveda$age) & !is.na(cveda$SDQ_TOTAL_DIFFICULTIES),]

cveda_list <- list(
  N = dim(cveda_no_missing)[1],
  J = 7,
  y = cveda_no_missing$SDQ_TOTAL_DIFFICULTIES,
  mp = cveda_no_missing$mp,
  age = cveda_no_missing$age,
  id = as.numeric(factor(cveda_no_missing$recruitment.centre, 
                    levels=unique(cveda_no_missing$recruitment.centre)))
)

fixed_model <-  '
data {
  int<lower=0> N;
  vector[N] mp;
  vector[N] age;
  vector[N] y;
  
}
parameters {
  vector[2] beta;
  real theta;
  real<lower=0> sigma;
} 

model {
  beta[1] ~ normal(0, 10);
  y ~ normal(beta[1] + beta[2] * age + theta * mp, sigma);
}
'

# fit the model in Stan and get a summary of the posteriors
stan_dl2<-stan(model_code=fixed_model,
              data=cveda_list,
              chains=3,
              iter=5000,warmup=1000,
              cores=3)

print(stan_dl2)
traceplot(stan_dl2,pars=c('theta','beta[1]','beta[2]','sigma'))
#compare with liner model
summary(lm(SDQ_TOTAL_DIFFICULTIES ~ mp + age, data=cveda))


## STAN MODEL - random intercept ONLY
random_intercept_model <- '
data {
  int<lower=1> N;
  int<lower=1> J; //the number of groups
  int<lower=1,upper=J> id[N]; //vector of group indeces
  vector[N] mp;
  vector[N] age;
  vector[N] y;
  
}
parameters {
  real beta;
  real theta;
  real alpha;
  real alpha_bar;
  real<lower=0> sigma_e;
  real<lower=0> sigma_u;
  vector[J] u;
} 

model {
  sigma_u ~ uniform(0, 40);
  sigma_e ~ uniform(0, 40);
  alpha ~ normal(alpha_bar, 5);
  alpha_bar ~ normal(0, 20);
  u ~ normal(0, sigma_u);
  y ~ normal(alpha + u[id] + beta * age + theta * mp, sigma_e);
}
'

# fit the model in Stan and get a summary of the posteriors
r_int_stan <-stan(model_code=random_intercept_model,
               data=cveda_list,
               chains=3,
               iter=5000, warmup=1000,
               cores=3)

print(r_int_stan)
traceplot(r_int_stan,pars=c('theta','alpha','beta','sigma_e', 'sigma_u'))
traceplot(r_int_stan,pars='u')


## STAN MODEL - random treatment effect
random_intercept_model <- '
data {
  int<lower=1> N;
  int<lower=1> J; //the number of groups
  int<lower=1,upper=J> id[N]; //vector of group indeces
  vector[N] mp;
  vector[N] age;
  vector[N] y;
  
}
parameters {
  real beta;
  real theta;
  real<lower=0> sigma_e;
  real<lower=0> sigma_u;
  vector[J] alpha;
} 

model {
  theta 
  sigma_u ~ uniform(0, 40);
  sigma_e ~ uniform(0, 40);
  alpha ~ normal(12.5, 20);
  u ~ normal(0, sigma_u);
  y ~ normal(alpha[id] + beta * age + (theta + u[id]) * mp, sigma_e);
}
'




