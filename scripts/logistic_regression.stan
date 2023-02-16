//
// Logistic regression model
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> n_knots; // number of knots
  int<lower=0> n_pitches; // number of pitches in dataset
  
  matrix[n_pitches, n_knots] X; // matrix containing distances of each pitch to every knot
  int<lower=0> y[n_pitches]; // vector of outcomes (strike = 1, ball = 0)
}

parameters {
  real alpha;
  vector[n_knots] beta;
}

model {
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  y ~ bernoulli_logit(alpha + X * beta);
}

