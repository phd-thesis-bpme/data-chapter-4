data {
  int<lower=0> N;
  vector [N] fc_change;
  vector [N] trend_change;
}

parameters {
  real intercept;
  real beta;
  real<lower=0> sigma;
}

model {
  
  trend_change ~ normal(intercept + beta * fc_change, sigma);
  intercept ~ normal(0,1);
  beta ~ normal(1, 1);
  sigma ~ exponential(1);
  
}
