
# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

data <- read_csv("spline_df.csv")
colnames(data) <- c("pitch_number", "plate_x", "plate_z", "called_strike")

# sample n pitches to test with
n <- 150000
pitches <- data[sample(nrow(data), size=n), ]

# separate location and outcome data
loc <- pitches %>% select(plate_x, plate_z)
y <- pitches$called_strike

# make grid of knots
# xz_pairs <- list(seq(-1.5, 1.5, .3), seq(1, 4, .3))
xz_pairs <- list(seq(-1.7, 1.7, length.out=34), seq(.4, 4.7, length.out=43))
bins <- expand.grid(xz_pairs)
colnames(bins) <- c("plate_x", "plate_z")

# Find Nearest Bin --------------------------------------------------------

# returns L2 norm between points a and b
l2_norm <- function(a, b) {
  return(sqrt(sum((a - b)^2)))
}

# calculates bin distances
calc_distances <- function(pitch_loc, bins) {
  return(apply(X=bins, MARGIN=1, l2_norm, b = pitch_loc))
}

find_nearest_bin <- function(distances) {
  return(which(distances == min(distances))[1])
}

# returns entire X matrix given a dataframe of (x, z) coordinates
get_bins <- function(pitch_loc, bins){
  distances <- t(apply(X=pitch_loc, MARGIN=1, calc_distances, bins = bins))
  return(apply(X=distances, MARGIN=1, find_nearest_bin))
}

# Visualization -----------------------------------------------------------

y_bins <- get_bins(loc, bins) %>% unlist

bins$bin <- seq(1, nrow(bins))
strike_zone <- data.frame(
  x = c(-.85, -.85, .85, .85, -.85),
  z = c(1.6, 3.5, 3.5, 1.6, 1.6)
)

data.frame(y = y, bin = y_bins) %>%
  group_by(bin) %>%
  summarize(prob = sum(y) / length(y)) %>%
  right_join(bins, by = "bin") %>%
  ggplot(aes(plate_x, plate_z, fill = prob)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#0009AC", "white", "#A40000"),
    values = scales::rescale(c(-0.5, -0.2, 0, 0.2, 0.5)),
    limits = c(0, 1), name = "K Prob"
  ) +
  geom_path(data = strike_zone, aes(x, z), col = "black", linewidth = 1, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Emperical Zone")

