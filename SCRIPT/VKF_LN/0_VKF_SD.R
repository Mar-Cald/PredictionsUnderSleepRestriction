# Sleep-Restricted

library(rstan)

# SD
dat_sd <- readRDS("DATA/OUT/dat_sd.rds")

##  run model
sd_full <- stan(
  file = "SCRIPT/VKF_LN/model_file_complete.stan",# Stan program
  data = dat_sd, chains = 5, cores = 5, init_r = .1,
  iter = 8000)
saveRDS(sd_full, file = "MODEL/VKF_LN/sd_complete.rds")
