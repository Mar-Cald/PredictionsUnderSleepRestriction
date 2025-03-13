# Well-Rested

library(rstan)

# FS 
dat_fs = readRDS("DATA/OUT/dat_fs.rds")

##  run model
fs_full = stan(
  file = "SCRIPT/VKF_LN/model_file_complete.stan",# Stan program
  data = dat_fs, chains = 5, cores = 5, init_r = .1,
  iter = 8000)
saveRDS(fs_full, file = "MODEL/VKF_LN/fs_complete.rds")



