data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  // covariate vectors
  vector[N] Time_days;
  // data for group-level effects of ID
  int<lower=1> NJ;
  int<lower=1> half_NJ;
  int<lower=1> J[N];
}
parameters {
  real ln_ka_amb;  // population-level effects
  real logit_ke_amb;  // population-level effects
  real logit_phi_amb; // population-level effects
  vector[half_NJ] r_ln_ka_amb;  // pond-level random effects
  vector[half_NJ] r_logit_ke_amb;  // pond-level random effects
  vector[half_NJ] r_logit_phi_amb;  // pond-level random effects

  real ln_ka_war;  // population-level effects
  real logit_ke_war;  // population-level effects
  real logit_phi_war; // population-level effects
  vector[half_NJ] r_ln_ka_war;  // pond-level random effects
  vector[half_NJ] r_logit_ke_war;  // pond-level random effects
  vector[half_NJ] r_logit_phi_war;  // pond-level random effects
  
  real<lower=0> hyper_sd_ln_ka;  // pond-level standard deviations
  real<lower=0> hyper_sd_logit_ke;  // pond-level standard deviations
  real<lower=0> hyper_sd_logit_phi;  // pond-level standard deviations
  
  real<lower=0> sigma;  // residual SD
}
transformed parameters {
  real ka_amb = exp(ln_ka_amb);
  real ka_war = exp(ln_ka_war);
  real ke_amb = 1 / (1 + exp(-logit_ke_amb));
  real ke_war = 1 / (1 + exp(-logit_ke_war));
  real phi_amb = (1 / ke_amb) / (1 + exp(-logit_phi_amb));
  real phi_war = (1 / ke_war) / (1 + exp(-logit_phi_war));
  vector[NJ] r_ka = append_row(exp(ln_ka_amb + r_ln_ka_amb), exp(ln_ka_war + r_ln_ka_war));
  vector[NJ] r_ke = append_row(1 ./ (1 + exp(-1 * (r_logit_ke_amb + logit_ke_amb))), 1 ./ (1 + exp(-1 * (r_logit_ke_war + logit_ke_war))));
  vector[NJ] r_phi = append_row((1 ./ r_ke[1:8]) ./ (1 + exp(-1 * (r_logit_phi_amb + logit_phi_amb))), (1 ./ r_ke[9:16]) ./ (1 + exp(-1 * (r_logit_phi_war + logit_phi_war))));
}
model {
  vector[N] mu;
  mu = r_phi[J] .* r_ka[J] .* r_ke[J] .* (exp(-r_ke[J] .* Time_days) - exp(-r_ka[J] .* Time_days)) ./ (r_ka[J] - r_ke[J]);
  // priors including all constants
  ln_ka_amb ~ normal(0, 1);
  logit_ke_amb ~ normal(0, 1);
  logit_phi_amb ~ normal(0, 1);
  r_ln_ka_amb ~ normal(0, hyper_sd_ln_ka);
  r_logit_ke_amb ~ normal(0, hyper_sd_logit_ke);
  r_logit_phi_amb ~ normal(0, hyper_sd_logit_phi);

  ln_ka_war ~ normal(0, 1);
  logit_ke_war ~ normal(0, 1);
  logit_phi_war ~ normal(0, 1);
  r_ln_ka_war ~ normal(0, hyper_sd_ln_ka);
  r_logit_ke_war ~ normal(0, hyper_sd_logit_ke);
  r_logit_phi_war ~ normal(0, hyper_sd_logit_phi);

  hyper_sd_ln_ka ~ gamma(2, 0.1);
  hyper_sd_logit_ke ~ gamma(2, 0.1);
  hyper_sd_logit_phi ~ gamma(2, 0.1);

  // likelihood including all constants
  Y ~ normal(mu, sigma);
  sigma ~ gamma(2, 0.1);
}
