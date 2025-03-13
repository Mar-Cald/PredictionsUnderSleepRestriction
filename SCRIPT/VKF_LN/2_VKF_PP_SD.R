
# Posterior Predictive: VKF + LN model ------------------------------

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages <- c("rstan", "tidyverse","ggpubr","ggplot2",
              "sjPlot",  "patchwork", "overlapping")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))


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

df_rt_pred_sd <- rstan::extract(gen_data_sd)$rt_pred %>%
  as.data.frame() %>%
  mutate(sim = 1:n()) %>%
  pivot_longer(cols = -sim,
               names_to = "obs_id",
               names_prefix = "V",
               values_to = "rt_pred") %>%
  mutate(obs_id = as.numeric(obs_id))

df_pe_pred_sd <- rstan::extract(gen_data_sd)$PGo %>%
  as.data.frame() %>%
  mutate(sim = 1:n()) %>%
  pivot_longer(cols = -sim,
               names_to = "obs_id",
               names_prefix = "V",
               values_to = "predictions") %>%
  mutate(obs_id = as.numeric(obs_id))

df_pred_sd = df_rt_pred_sd %>% 
  left_join(df_pe_pred_sd, by = c("sim","obs_id"))

sum(df_pred_sd$sim==1)

# extract real data
df_main_sd <- as.data.frame(dat_sd) %>%
  dplyr::select(subj, GO, SE, rt) %>%
  subset(rt != 999)%>%
  mutate(obs_id = 1:n())


# Join with real data
df_both_rt_sd <- df_pred_sd %>%
  left_join(dplyr::select(df_main_sd, -rt)) %>%
  dplyr::rename(rt = rt_pred) %>%
  dplyr::bind_rows(df_main_sd)%>%
  mutate(sim = ifelse(is.na(sim), -1,sim),
         data_type = ifelse(sim == -1, "real","pred"),
         SE = ifelse(SE == 0, "Se1","Se2"))%>%
  subset(!is.na(GO))

df_both_rt_sd$Stimulus <- as.factor(ifelse(df_both_rt_sd$GO == 1, "Go","NoGo"))
df_both_rt_sd$PGo <- as.factor(case_when(df_both_rt_sd$predictions < -.35 ~ "PGo ~20%",
                                         df_both_rt_sd$predictions > .35 ~ "PGo ~80%" ,
                                         df_both_rt_sd$predictions <= .35 & 
                                           df_both_rt_sd$predictions >= -.35 ~ "20%< PGo >80%"))
df_both_rt_sd$PGo <- c(na.omit(df_both_rt_sd$PGo), 
                       df_both_rt_sd$PGo[df_both_rt_sd$sim == 1])
table(df_both_rt_sd$PGo) 


# Overlapping and Plot: Stimulus -----------------------------

over_Go = overlap(x = list(pred = 
                             df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$Stimulus == "Go"],
                           real = 
                             df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$Stimulus == "Go"]),
                  type = "2")

over_NoGo = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & 
                                                       df_both_rt_sd$Stimulus == "NoGo"],
                             real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & 
                                                       df_both_rt_sd$Stimulus == "NoGo"]),
                    type = "2")

overStim = data.frame(index = round(c(over_Go$OV,over_NoGo$OV),3),
                      Stimulus = c("Go","NoGo"),
                      label= c("Go","NoGo"))


SD_stimulus = subset(df_both_rt_sd, rt != 999) %>% 
  ggplot(aes(x = rt, fill = data_type))+
  geom_density(alpha = 0.4)+
  facet_wrap(~Stimulus)+theme_pubr()+ylab("density")+
  geom_text(data = overStim, aes(x = 0.6, y = 4, label = index), 
            inherit.aes = FALSE)+
  ggtitle(label = "Sleep-Restricted Group")
SD_stimulus

# Overlapping and Plot: PGo ------------------------------
over_PGo80_1 = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$PGo == "PGo ~80%" &
                                                          df_both_rt_sd$SE == "Se1"],
                                real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$PGo == "PGo ~80%"&
                                                          df_both_rt_sd$SE == "Se1"]),
                       type = "2")

over_PGo80_2 = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$PGo == "PGo ~80%" &
                                                          df_both_rt_sd$SE == "Se2"],
                                real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$PGo == "PGo ~80%"&
                                                          df_both_rt_sd$SE == "Se2"]),
                       type = "2")

over_PGo20_1 = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$PGo == "PGo ~20%"&
                                                          df_both_rt_sd$SE == "Se1"],
                                real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$PGo == "PGo ~20%"&
                                                          df_both_rt_sd$SE == "Se1"]),
                       type = "2")

over_PGo20_2 = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$PGo == "PGo ~20%"&
                                                          df_both_rt_sd$SE == "Se2"],
                                real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$PGo == "PGo ~20%"&
                                                          df_both_rt_sd$SE == "Se2"]),
                       type = "2")

over_PGo8020_1 = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$PGo == "20%< PGo >80%"&
                                                            df_both_rt_sd$SE == "Se1"],
                                  real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$PGo == "20%< PGo >80%"&
                                                            df_both_rt_sd$SE == "Se1"]),
                         type = "2")

over_PGo8020_2 = overlap(x = list(pred = df_both_rt_sd$rt[df_both_rt_sd$data_type == "pred" & df_both_rt_sd$PGo == "20%< PGo >80%"&
                                                            df_both_rt_sd$SE == "Se2"],
                                  real = df_both_rt_sd$rt[df_both_rt_sd$data_type == "real" & df_both_rt_sd$PGo == "20%< PGo >80%"&
                                                            df_both_rt_sd$SE == "Se2"]),
                         type = "2")


overPGo = data.frame(index = round(c(over_PGo80_1$OV,over_PGo20_1$OV,over_PGo8020_1$OV,
                                     over_PGo80_2$OV,over_PGo20_2$OV,over_PGo8020_2$OV),3),
                     PGo = c("PGo ~80%",
                             "PGo ~20%", 
                             "20%< PGo >80%", 
                             "PGo ~80%",
                             "PGo ~20%",
                             "20%< PGo >80%"),
                     label= c("PGo ~80%",
                              "PGo ~20%",
                              "20%< PGo >80%", 
                              "PGo ~80%",
                              "PGo ~20%",
                              "20%< PGo >80%"),
                     SE = c("Se1","Se1","Se1","Se2","Se2","Se2"))


SD_PGo = subset(df_both_rt_sd, rt != 999) %>% 
  ggplot(aes(x = rt, fill = data_type))+
  geom_density(alpha = 0.4, show.legend = F)+
  facet_wrap(SE~PGo)+theme_pubr()+ylab("density")+
  geom_text(data = overPGo, aes(x = 0.6, y = 4, label = index), inherit.aes = FALSE)

SD_pp = SD_stimulus+SD_PGo +plot_layout(heights = c(1,2))
SD_pp


ggsave("PLOT/SD_pp.jpg", plot = SD_pp, dpi = 300, 
       width = 8, height = 8)



