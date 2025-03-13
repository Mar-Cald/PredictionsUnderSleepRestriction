
# Posterior Predictive: VKF + LN model _ PGo Trajectory ------------------------------

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages = c("rstan", "tidyverse","ggpubr","ggplot2",
              "sjPlot",  "patchwork", "overlapping")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))


# 2. Well-Rested  ---------------------------------------------------------------

# Load Model
FS = readRDS("MODEL/VKF_LN/fs_complete.rds")

# Load data
# FS 
dat_fs = readRDS("DATA/OUT/dat_fs.rds")

# Load model for posterior prediction
gen_model = stan_model(file = "SCRIPT/VKF_LN/model_file_complete_pp.stan")

# Parameter values from model FS
draws_par =  as.matrix(FS)[1:500, , drop = FALSE]

# Extract model prediction
gen_data_fs = gqs(gen_model,
                   data = dat_fs,
                   draws = draws_par)


df_pred_fs = rstan::extract(gen_data_fs)$predictions %>%
  as.data.frame() %>%
  mutate(sim = 1:n()) %>%
  pivot_longer(cols = -sim,
               names_to = "obs_id",
               names_prefix = "V",
               values_to = "predictions") %>%
  mutate(obs_id = as.numeric(obs_id),
         Trial = rep(c(1:448), 64*500))


# extract real data
df_main_fs = as.data.frame(dat_fs) %>%
  dplyr::select(subj, SE) %>%
  mutate(obs_id = 1:n())

# Join with real data
df_pred_both = df_pred_fs %>%
  left_join(df_main_fs)%>%
  dplyr::bind_rows(df_main_fs)%>%
  mutate(SE = factor(ifelse(SE == 0, "Se1","Se2")),
         subj = factor(subj))






# 2. Sleep - Restriction  ---------------------------------------------------------------

# Load Model
SD = readRDS("MODEL/VKF_LN/sd_complete.rds")

# Load data
# SD
dat_sd = readRDS("DATA/OUT/dat_sd.rds")

# Load model for posterior prediction
gen_model = stan_model(file = "SCRIPT/VKF_LN/model_file_complete_pp.stan")

# Parameter values from model FS
draws_par =  as.matrix(SD)[1:500, , drop = FALSE]

# Extract model prediction
gen_data_sd = gqs(gen_model,
                   data = dat_sd,
                   draws = draws_par)

df_pred_sd = rstan::extract(gen_data_sd)$predictions %>%
  as.data.frame() %>%
  mutate(sim = 1:n()) %>%
  pivot_longer(cols = -sim,
               names_to = "obs_id",
               names_prefix = "V",
               values_to = "predictions") %>%
  mutate(obs_id = as.numeric(obs_id),
         Trial = rep(c(1:448), 62*500))


# extract real data
df_main_sd = as.data.frame(dat_sd) %>%
  dplyr::select(subj, SE) %>%
  mutate(obs_id = 1:n())

# Join with real data
df_pred_both_sd = df_pred_sd %>%
  left_join(df_main_sd)%>%
  dplyr::bind_rows(df_main_sd)%>%
  mutate(SE = factor(ifelse(SE == 0, "Se1","Se2")),
         subj = factor(subj))


# Plot ----------------------------------------------------------------------
m_pred_1_sd = subset(df_pred_both_sd, SE == "Se1")%>%group_by(sim,Trial)%>%
  summarise(predictions = median(predictions, na.rm = T))
m_pred_2_sd = subset(df_pred_both_sd, SE == "Se2")%>%group_by(sim,Trial)%>%
  summarise(predictions = median(predictions, na.rm = T))

m_pred_1_fs = subset(df_pred_both, SE == "Se1")%>%group_by(sim,Trial)%>%
  summarise(predictions = median(predictions, na.rm = T))
m_pred_2_fs = subset(df_pred_both, SE == "Se2")%>%group_by(sim,Trial)%>%
  summarise(predictions = median(predictions, na.rm = T))

p_block = data.frame(predictions = c(rep(0.2,40),rep(0.8,40),rep(0.5,32),rep(0.8,40),rep(0.2,40),rep(0.5,32),
                                     rep(0.8,40),rep(0.2,40),rep(0.5,32), rep(0.2,40),rep(0.8,40),rep(0.5,32)),
                     Trial = 1:448)

pGO1= m_pred_1_fs%>%ggplot(aes(y = predictions+0.5, x = Trial, color = sim, group = sim))+
  geom_line(data = m_pred_1_fs, aes(x = Trial, y = predictions+0.5), color = pal_okabe_ito_2[1],
            show.legend = FALSE, alpha = 0.03, lwd = 1) +
  geom_line(data = m_pred_1_sd, aes(x = Trial, y = predictions+0.5), color = pal_okabe_ito_2[2],
            show.legend = FALSE, alpha = 0.03, lwd = 1)+
  geom_line(data = p_block, aes(x = Trial, y = predictions), lwd = 2, alpha = .5,
            color = "black", inherit.aes = FALSE)+ylab("PGo")
pGO1



pGO2= m_pred_2_fs%>%ggplot(aes(y = predictions +.5, x = Trial, color = sim, group = sim))+
  geom_line(data = m_pred_2_fs, aes(x = Trial, y = predictions +.5), color = pal_okabe_ito_2[1],
            show.legend = FALSE, alpha = 0.03, lwd = 1) +
  geom_line(data = m_pred_2_sd, aes(x = Trial, y = predictions +.5), color = pal_okabe_ito_2[2],
            show.legend = FALSE, alpha = 0.03, lwd = 1)+
  geom_line(data = p_block, aes(x = Trial, y = predictions), lwd = 2, alpha = .5,
            color = "black", inherit.aes = FALSE)+ylab("PGo")

p_Go_ok = pGO1/pGO2
p_Go_ok

ggsave("PLOT/pGo_traj_sim.jpg", 
       plot = p_Go_ok, dpi = 400, 
       width = 10, height = 7)
