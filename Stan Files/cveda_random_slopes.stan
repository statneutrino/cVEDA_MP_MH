//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=1> N;
  int<lower=1> J; //the number of groups
  int<lower=1,upper=J> id[N]; //vector of group indices
  vector[N] mp;
  vector[N] y;
}
parameters {
  vector[J] alpha;
  vector[J] theta;
  real alpha_bar;
  real beta_bar;
  vector<lower=0>[2] tau;
  real<lower=0> sigma;
  corr_matrix[2] Rho;
}
model {
  vector[N] mu;
  vector[2] YY[J];
  vector[2] MU;
  Rho ~ lkj_corr(2);
  sigma ~ exponential(1);
  tau ~ exponential(1);
  alpha ~ normal(0, 10);
  theta ~ normal(0, 2);
  MU = [alpha_bar, beta_bar]';
  for (j in 1:J) {
    YY[j] = [alpha[j], theta[j]]';
  }
  YY ~ multi_normal(MU, quad_form_diag(Rho, tau));
  mu = alpha[id] + theta[id] .* mp;
  y ~ normal(mu, sigma);
}



