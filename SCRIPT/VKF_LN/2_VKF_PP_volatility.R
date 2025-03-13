
# Posterior Predictive: VKF + LN model _ Volatility Trajectory ------------------------------

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages <- c("rstan", "tidyverse","ggpubr","ggplot2",
              "sjPlot",  "patchwork", "overlapping")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))


# 2. Well-Rested  ---------------------------------------------------------------

# Load Model
FS <- readRDS("MODEL/VKF_LN/fs_complete.rds")

# Load data
# FS 
dat_fs <- readRDS("DATA/OUT/dat_fs.rds")

# Load model for posterior prediction
gen_model <- stan_model(file = "SCRIPT/VKF_LN/model_file_complete_pp.stan")

# Parameter values from model FS
draws_par <-  as.matrix(FS)[1:500, , drop = FALSE]

# Extract model prediction
gen_data_fs <- gqs(gen_model,
                   data = dat_fs,
                   draws = draws_par)


df_vol_fs <- rstan::extract(gen_data_fs)$volatility %>%
  as.data.frame() %>%
  mutate(sim = 1:n()) %>%
  pivot_longer(cols = -sim,
               names_to = "obs_id",
               names_prefix = "V",
               values_to = "volatility") %>%
  mutate(obs_id = as.numeric(obs_id),
         Trial = rep(c(1:448), 64*500))


# extract real data
df_main_fs <- as.data.frame(dat_fs) %>%
  dplyr::select(subj, SE) %>%
  mutate(obs_id = 1:n())

# Join with real data
df_vol_both <- df_vol_fs %>%
  left_join(df_main_fs)%>%
  dplyr::bind_rows(df_main_fs)%>%
  mutate(SE = factor(ifelse(SE == 0, "Se1","Se2")),
         subj = factor(subj))






# 2. Sleep - Restriction  ---------------------------------------------------------------

# Load Model
SD <- readRDS("MODEL/VKF_LN/sd_complete.rds")

# Load data
# SD
dat_sd <- readRDS("DATA/OUT/dat_sd.rds")

# Load model for posterior prediction
gen_model <- stan_model(file = "SCRIPT/VKF_LN/model_file_complete_pp.stan")

# Parameter values from model FS
draws_par <-  as.matrix(SD)[1:500, , drop = FALSE]

# Extract model prediction
gen_data_sd <- gqs(gen_model,
                   data = dat_sd,
                   draws = draws_par)

df_vol_sd <- rstan::extract(gen_data_sd)$volatility %>%
  as.data.frame() %>%
  mutate(sim = 1:n()) %>%
  pivot_longer(cols = -sim,
               names_to = "obs_id",
               names_prefix = "V",
               values_to = "volatility") %>%
  mutate(obs_id = as.numeric(obs_id),
         Trial = rep(c(1:448), 62*500))


# extract real data
df_main_sd <- as.data.frame(dat_sd) %>%
  dplyr::select(subj, SE) %>%
  mutate(obs_id = 1:n())

# Join with real data
df_vol_both_sd <- df_vol_sd %>%
  left_join(df_main_sd)%>%
  dplyr::bind_rows(df_main_sd)%>%
  mutate(SE = factor(ifelse(SE == 0, "Se1","Se2")),
         subj = factor(subj))


# Plot ----------------------------------------------------------------------
m_vol_1_sd = subset(df_vol_both_sd, SE == "Se1")%>%group_by(sim,Trial)%>%
  summarise(volatility = median(volatility, na.rm = T))
m_vol_2_sd = subset(df_vol_both_sd, SE == "Se2")%>%group_by(sim,Trial)%>%
  summarise(volatility = median(volatility, na.rm = T))

m_vol_1_fs = subset(df_vol_both, SE == "Se1")%>%group_by(sim,Trial)%>%
  summarise(volatility = median(volatility, na.rm = T))
m_vol_2_fs = subset(df_vol_both, SE == "Se2")%>%group_by(sim,Trial)%>%
  summarise(volatility = median(volatility, na.rm = T))

pvol1= m_vol_1_sd%>%ggplot(aes(y = volatility, x = Trial, color = sim, group = sim))+
  geom_line(data = m_vol_1_sd, aes(x = Trial, y = volatility), color = pal_okabe_ito_2[2],
            show.legend = FALSE, alpha = 0.07) +
  geom_line(data = m_vol_1_fs, aes(x = Trial, y = volatility), color = pal_okabe_ito_2[1],
            show.legend = FALSE, alpha = 0.07) +
  stat_summary(data = m_vol_1_sd, aes(x = Trial, y = volatility), 
               fun = median, geom = "line", color = "black", size = .8, inherit.aes = FALSE)+
  stat_summary(data = m_vol_1_fs, aes(x = Trial, y = volatility), 
               fun = median, geom = "line", color = "black", size = .8, inherit.aes = FALSE)+
  geom_vline(xintercept = 40,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 80,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 112,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 152,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 192,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 224,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 264,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 304,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 336,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 376,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 416,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 448,lwd = .5, alpha = .5)+
  scale_y_continuous(limits = c(0.2,3), breaks = seq(0.2,3, 0.4))


pvol2= m_vol_2_sd%>%ggplot(aes(y = volatility, x = Trial, color = sim, group = sim))+
  geom_line(data = m_vol_2_sd, aes(x = Trial, y = volatility), color = pal_okabe_ito_2[2],
            show.legend = FALSE, alpha = 0.07) +
  geom_line(data = m_vol_2_fs, aes(x = Trial, y = volatility), color = pal_okabe_ito_2[1],
            show.legend = FALSE, alpha = 0.07) +
  stat_summary(data = m_vol_2_sd, aes(x = Trial, y = volatility), 
               fun = median, geom = "line", color = "black", size = .8, inherit.aes = FALSE)+
  stat_summary(data = m_vol_2_fs, aes(x = Trial, y = volatility), 
               fun = median, geom = "line", color = "black", size = .8, inherit.aes = FALSE)+
  geom_vline(xintercept = 40,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 80,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 112,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 152,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 192,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 224,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 264,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 304,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 336,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 376,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 416,lwd = .5, alpha = .5)+
  geom_vline(xintercept = 448,lwd = .5, alpha = .5)+
  scale_y_continuous(limits = c(0.2,3), 
                     breaks = seq(0.2,3, 0.4))

p_vol_ok = p_omega_1+pvol1 + p_omega_2+pvol2+
  plot_layout(nrow = 2, ncol = 2,widths = c(1.5,2,1.5,2))
p_vol_ok

ggsave("PLOT/vol_traj_sim.jpg", 
       plot = p_vol_ok, dpi = 400, 
       width = 10, height = 7)

