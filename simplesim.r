## dumbed-down simulation of selection bias with noise (aka selecting on fielding of scale before fielding)

# set seed for reproducibility
set.seed(123)

# parameters
n_studies <- 100000
noise_dist <- seq(0, 0.5, 0.01)

## draw uniform distribution of alpha values
pre_alpha <- runif(n_studies)

## field if alpha >= 0.7
fielded <- ifelse(pre_alpha >= 0.7, pre_alpha, NA)

## function to add sampling noise to alphas
sampling_noise <- function(x, noise) {
  return(x + rnorm(length(x), 0, noise))
}

## estimate post-alpha
obs_count <- data.frame(row.names = noise_dist)

for (noise in noise_dist) {
    noisy_sample <- data.frame(row.names = 1:n_studies)
    noisy_sample[, as.character(noise)] <- sampling_noise(fielded, noise)
    noisy_sample <- floor(noisy_sample * 100) / 100
    noisy_sample_dist <- table(noisy_sample)
    obs_count[as.character(noise), "lower"]  <- noisy_sample_dist["0.69"]
    obs_count[as.character(noise), "upper"]  <- noisy_sample_dist["0.7"]
}

## calculate residual after kernel smoothing using their code
## (replace for now with rounding and taking difference .69 & .7)
obs_count["0", "lower"] <- 0
obs_count["noise"] <- noise_dist
obs_count["difference"] <- obs_count["upper"] - obs_count["lower"]
obs_count["difference"] <- obs_count["difference"] / n_studies

## plot differential .69-bin vs .70-bin as a function of noise
library(tidyverse)
obs_count %>% 
  ggplot(aes(noise, difference)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  xlab("SD Delta Post - Pre alpha") +
  ylab("N Obs (.7, .71] - N Obs (.69, .7]")
ggsave("plots/difference_by_noise.png", width = 6, height = 4)
