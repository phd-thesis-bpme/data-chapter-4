data {
  int<lower=0> N;
  vector [N] route_trend;
  vector [N] point_trend;
}

parameters {
  real intercept;
  real beta;
  real<lower=0> sigma;
}

model {
  
  route_trend ~ normal(intercept + beta * point_trend, sigma);
  intercept ~ normal(0,1);
  beta ~ normal(1, 1);
  sigma ~ exponential(1);
  
}
