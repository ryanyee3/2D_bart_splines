
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rstan)

# Data --------------------------------------------------------------------

load("pitches_501.RData")

# separate location and outcome data
loc <- pitches_501 %>% select(plate_x, plate_z)
y <- pitches_501$called_strike

# make grid of knots
xz_pairs <- list(seq(-1.1, 1.1, length.out=5), seq(1, 4.1, length.out=5))
knots <- expand.grid(xz_pairs)
colnames(knots) <- c("plate_x", "plate_z")

# Computing X Matrix ------------------------------------------------------

# returns L2 norm between points a and b
l2_norm <- function(a, b) {
  return(sqrt(sum((a - b)^2)))
}

# gaussian kernel, L is a hyperparameter
phi <- function(a, b, L) {
  return(-exp(l2_norm(a, b) / (2*L^2)))
}

# calculates predictors for a single row in the X matrix
calc_predictors <- function(pitch_loc, knots, L) {
  return(apply(X=knots, MARGIN=1, phi, b=pitch_loc, L = L))
}

# returns entire X matrix given a dataframe of (x, z) coordinates
get_xmat_phi <- function(pitch_loc, knots, L){
  return(t(apply(X=pitch_loc, MARGIN=1, calc_predictors, knots=knots, L=L)))
}

# Model -------------------------------------------------------------------

# stan_log_reg <- stan_model(file="logistic_regression.stan")
# save(stan_log_reg, file="stan_log_reg.RData")
load("stan_log_reg.RData")

model_inputs <- list(n_knots = nrow(knots), n_pitches = nrow(pitches), y = y)

X_l.5 <- get_xmat_phi(loc, knots, .5)
model_inputs[["X"]] <- X_l.5
fit_l.5 <- sampling(stan_log_reg, data = model_inputs)
save(fit_l.5, file="fit_l-5.RDdata")

X_l1 <- get_xmat_phi(loc, knots, 1)
model_inputs[["X"]] <- X_l1
fit_l1 <- sampling(stan_log_reg, data = model_inputs)
save(fit_l1, file="fit_l1.RData")

X_l2 <- get_xmat_phi(loc, knots, 2)
model_inputs[["X"]] <- X_l2
fit_l2 <- sampling(stan_log_reg, data = model_inputs)
save(fit_l2, file="fit_l2.RData")

# Diagnostics -------------------------------------------------------------

summary(fit_l.5)
summary(fit_l1)
summary(fit_l2)

traceplot(fit_l.5, pars = c("beta"))
traceplot(fit_l1, pars = c("beta"))
traceplot(fit_l2, pars = c("beta"))

# Visualization -----------------------------------------------------------

load("fit_l1.RData")

coef_l1 <- extract(object = fit_l1, pars = c("alpha", "beta"))
coef_l2 <- extract(object = fit_l2, pars = c("alpha", "beta"))

alpha <- coef_l1[["alpha"]] %>% mean()
beta <- coef_l1[["beta"]] %>% apply(MARGIN=2, mean)

xz_pairs <- list(seq(-1.7, 1.7, length.out=34), seq(.4, 4.7, length.out=43))
points <- expand.grid(xz_pairs)

X_pred <- get_xmat_phi(points, knots, 1)
y_pred <- alpha + X_pred %*% beta
y_prob <- 1 / (1 + exp(-y_pred))

strike_zone <- data.frame(
  x = c(-.85, -.85, .85, .85, -.85),
  z = c(1.6, 3.5, 3.5, 1.6, 1.6)
)

data.frame(plate_x = points[,1], plate_z = points[,2], prob = y_prob) %>%
  ggplot(aes(plate_x, plate_z, fill = prob)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#0009AC", "white", "#A40000"),
    values = scales::rescale(c(-0.5, -0.2, 0, 0.2, 0.5)),
    limits = c(0, 1), name = "K Prob"
  ) +
  geom_path(data = strike_zone, aes(x, z), col = "black", linewidth = 1, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Gaussian Kernel")
