
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

# radial basis kernel
radial_basis <- function(a, b) {
  d <- l2_norm(a, b)
  C <- d^2 * log(d)
  return(ifelse(is.na(C), 0, C))
}

# calculates predictors for a single row in the X matrix
calc_predictors <- function(pitch_loc, knots, kernel) {
  return(apply(X=knots, MARGIN=1, kernel, b=pitch_loc))
}

# returns entire X matrix given a dataframe of (x, z) coordinates
get_xmat_radial_basis <- function(pitch_loc, knots, kernel){
  return(t(apply(X=pitch_loc, MARGIN=1, calc_predictors, knots=knots, kernel=kernel)))
}

# Model -------------------------------------------------------------------

load("stan_log_reg.RData")

model_inputs <- list(n_knots = nrow(knots),
                     n_pitches = nrow(pitches), 
                     X = get_xmat_radial_basis(loc, knots, radial_basis), 
                     y = y)
radial_basis_fit <- sampling(stan_log_reg, data = model_inputs)
save(radial_basis_fit, file="radial_basis_fit.RData")

# Diagnostics -------------------------------------------------------------

summary(radial_basis_fit)
traceplot(radial_basis_fit, pars = c("beta"))

# Visualization -----------------------------------------------------------

load("radial_basis_fit.RData")

coef_radial_basis <- rstan::extract(object = radial_basis_fit, pars = c("alpha", "beta"))

alpha <- coef_radial_basis[["alpha"]] %>% mean()
beta <- coef_radial_basis[["beta"]] %>% apply(MARGIN=2, mean)

xz_pairs <- list(seq(-1.7, 1.7, length.out=34), seq(.4, 4.7, length.out=43))
points <- expand.grid(xz_pairs)

X_pred <- get_xmat_radial_basis(points, knots, radial_basis)
y_pred <- alpha + X_pred %*% beta
y_prob <- 1 / (1 + exp(-y_pred))

strike_zone <- data.frame(
  x = c(-.85, -.85, .85, .85, -.85),
  z = c(1.6, 3.5, 3.5, 1.6, 1.6)
)

data.frame(plate_x = points[,1], plate_z = points[,2], prob = y_prob) %>%
  ggplot(aes(plate_x, plate_z, fill = prob)) +
  # geom_point() +
  # scale_color_gradientn(
  #   colours = c("#0009AC", "white", "#A40000"),
  #   values = scales::rescale(c(-0.5, -0.2, 0, 0.2, 0.5)),
  #   limits = c(0, 1), name = "K Prob"
  # ) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#0009AC", "white", "#A40000"),
    values = scales::rescale(c(-0.5, -0.2, 0, 0.2, 0.5)),
    limits = c(0, 1), name = "K Prob"
  ) +
  geom_path(data = strike_zone, aes(x, z), col = "black", linewidth = 1, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Radial Basis")

