
# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

load("~/Documents/projects/baseball/plate_discipline/data/bart_df.RData")
taken_pitches <- bart_df %>% filter(swing==0)
save(taken_pitches, file="data/taken_pitches.RData")

# Initial Models ----------------------------------------------------------

pitch_501 <- taken_pitches[sample(1:nrow(taken_pitches), size = 501, replace = FALSE), ] %>%
  select(plate_x, plate_z, called_strike)
save(pitches_501, file="data/pitches_501.RData")

# Handedness Models -------------------------------------------------------

get_handedness_samples <- function(pitches, p_hand, b_hand, n) {
  c <- pitches %>% 
    filter(p_throws == p_hand, stand == b_hand) %>%
    select(plate_x, plate_z, called_strike)
  return(c[sample(1:nrow(c), size = n, replace = FALSE), ])
}

RHP_RHB_samples <- get_handedness_samples(taken_pitches, 0, 0, 501)
RHP_LHB_samples <- get_handedness_samples(taken_pitches, 0, 1, 501)
LHP_RHB_samples <- get_handedness_samples(taken_pitches, 1, 0, 501)
LHP_LHB_samples <- get_handedness_samples(taken_pitches, 1, 1, 501)

save(RHP_RHB_samples, file="data/RHP_RHP_samples.RData")
save(RHP_LHB_samples, file="data/RHP_RHP_samples.RData")
save(LHP_RHB_samples, file="data/RHP_RHP_samples.RData")
save(LHP_LHB_samples, file="data/RHP_RHP_samples.RData")
