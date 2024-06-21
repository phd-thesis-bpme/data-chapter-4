data {
  int<lower=0> N;
  int<lower=0> n_strata;
  array [N] real detect_index;
  array [N] real varprop_index;
  array [N] int stratum;
}

parameters {
  real intercept;
  vector [n_strata] beta;
  real BETA;
  real<lower=0> sigma;
}

model {
  for (i in 1:N)
  {
    varprop_index[i] ~ normal(intercept + beta[stratum[i]] * detect_index[i], sigma);
  }
  
  intercept ~ normal(0,1);
  beta ~ normal(BETA, 1);
  BETA ~ normal(1, 1);
  sigma ~ exponential(1);

}
