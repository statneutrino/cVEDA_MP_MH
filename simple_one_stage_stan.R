library(lme4)
library(meta)
library(rstan)
library(tidyverse)
#library(bayesplot)
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

cveda_no_missing <- cveda[!is.na(cveda$mp) & 
                            !is.na(cveda$age) & 
                            !is.na(cveda$SDQ_TOTAL_DIFFICULTIES) &
                            !is.na(cveda$sex) &
                            !is.na(cveda$homeown) &
                            !is.na(cveda$housing) &
                            !is.na(cveda$urbanisation),]

cveda_list <- list(
  N = dim(cveda_no_missing)[1],
  J = 7,
  y = cveda_no_missing$SDQ_TOTAL_DIFFICULTIES,
  mp = cveda_no_missing$mp,
  age = cveda_no_missing$age,
  id = as.numeric(factor(cveda_no_missing$recruitment.centre, 
                    levels=unique(cveda_no_missing$recruitment.centre))),
  sex = as.numeric(cveda_no_missing$sex) - 1,
  homeown = as.numeric(as.character(cveda_no_missing$homeown)),
  housing = as.numeric(as.character(cveda_no_missing$housing)),
  urbanisation = as.numeric(cveda_no_missing$urbanisation) - 1
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

##fixed_model housing and centre ONLY
housing_model <- '
data {
  int<lower=1> N;
  int<lower=1> J; //the number of groups
  int<lower=1,upper=J> id[N]; //vector of group indeces
  vector[N] housing;
  vector[N] y;
  vector[N] sex;
  
}
parameters {
  vector[J] alpha;
  vector[J] beta;
  real beta_2;
  real<lower=0> sigma;
} 

model {
  vector[N] mu;
  sigma ~ exponential(1);
  beta ~ normal(0,2);
  alpha ~ normal(0, 10);
  mu = alpha[id] + beta[id] .* housing + beta_2 * sex;
  y ~ normal(mu, sigma);
}
'

# fit the model in Stan and get a summary of the posteriors
housing_stan <-stan(model_code=housing_model,
               data=cveda_list,
               chains=2,
               iter=2000,warmup=1000,
               cores=4)
posterior <- rstan::extract(housing_stan, permuted = FALSE)
mcmc_areas(
  posterior,
  pars = c("beta[1]", "beta[2]","beta[3]","beta[4]","beta[5]","beta[6]","beta[7]"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.95, # 99%
  point_est = "mean"
)

#compare with LME4
housing_model <- lmer(SDQ_HYPER ~ sex + housing + (housing | recruitment.centre), data=cveda)
summary(housing_model)

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
generated quantities{
  vector[N] log_lik;
  vector[N] mu;
  mu = alpha + u[id] + beta * age + theta * mp;
  for (n in 1:N){
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma_e);
  }
}
'

# fit the model in Stan and get a summary of the posteriors
r_int_stan <-stan(model_code=random_intercept_model,
               data=cveda_list,
               chains=1,
               iter=2000, warmup=1000)

print(r_int_stan)
traceplot(r_int_stan,pars=c('theta','alpha','beta','sigma_e', 'sigma_u'))
traceplot(r_int_stan,pars='u')

saveRDS(r_int_stan, "Stan Files/stanfits/intercept_only.rds")

intercept_log_lik <- loo::extract_log_lik(r_int_stan, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(intercept_log_lik)

## STAN MODEL - random treatment effect
#y ~ normal(mu, sigma)
#mu = alpha[d_id] + beta[d_id] * u
#[alpha[d_id], beta[d_id]] ~ MVN([a_bar, b_bar], S)
#S = covariance matrix i.e. 
#Hyperpriors
#alpha_bar ~ normal(0,1.5)
#beta_bar ~ normal(0,1)
#sigma ~ Exp(1)
#sigma_alpha ~ exp(1)
#sigma_beta ~ exp(1)
#R ~ LKJcorr(2)



random_slopes1_path <- "Stan Files/cveda_random_slopes.stan"
  
random_slopes1 <- stan(file = random_slopes1_path, 
       data = cveda_list,
       chains=1,
       iter=2000,warmup=1000,
       cores=4)

#random_slopes1 had 72 divergent transitions

print(random_slopes1)
lmer(SDQ_TOTAL_DIFFICULTIES ~ mp + (mp | recruitment.centre), data=cveda) %>% summary(.) #compare with lmer 

saveRDS(random_slopes1, "Stan Files/stanfits/random_slopes1.rds")

traceplot(random_slopes1, pars="Rho[1,2]")

#use loo package to extract
slopes1_log_lik <- loo::extract_log_lik(random_slopes1, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(slopes1_log_lik)

mcmc_areas(
  posterior,
  pars = c("alpha[1]", "alpha[2]","alpha[3]","alpha[4]","alpha[5]","alpha[6]","alpha[7]"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.95, # 99%
  point_est = "mean"
)

random_slopes2_path <- "Stan Files/cveda_random_slopes_cholesky.stan"

random_slopes2 <- stan(file = random_slopes2_path, 
                       data = cveda_list,
                       chains=1,
                       iter=2000,warmup=1000,
                       cores=4)

random_slopes3_path <- "Stan Files/cveda_random_slopes_confounders2.stan"

random_slopes3 <- stan(file = random_slopes3_path, 
                       data = cveda_list,
                       chains=1,
                       iter=2000,warmup=1000,
                       cores=4)
posterior <- rstan::extract(random_slopes3, permuted = FALSE)
mcmc_areas(
  posterior,
  pars = c("theta_bar", "theta[1]", "theta[2]","theta[3]","theta[4]","theta[5]","theta[6]","theta[7]"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.95, # 99%
  point_est = "mean"
)

print(random_slopes3)





