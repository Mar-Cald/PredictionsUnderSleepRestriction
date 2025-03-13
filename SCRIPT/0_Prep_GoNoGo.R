# Pre-processing Go-NoGo 


## 1.Load Packages --------------------------------------------------------------

packages <- c("readr", "dplyr", "tidyverse", "ggplot2", "ggpubr")
sapply(packages, require, character.only = T)

theme_set(theme_pubr())

## 2.Load data --------------------------------------------------------------

d <- list.files(path = "DATA/RAW/data_exp", pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.x) %>% mutate(condition = as.character(condition),
                                  session = as.character(session))) # Combine data sets into one data set 


## 3. Prep data --------------------------------------------------------------

# DATA GNG task
dat <- subset(d, RUN >= 1) |>
        mutate(id = case_when(participant == "GNG87"~ "GNG84",
                        participant == "GN103"~ "GNG103",
                        participant == "GN100"~ "GNG100",
                        .default = participant),
         P = factor(LW_P),
         target = factor(target_col),
         C = factor(case_when(condition == "CONDIZIONE FS" ~ "FS",
                       condition == "FF" ~ "FS",
                       condition == "fs" ~ "FS",
                       condition == "sd" ~ "SD",
                       condition == "FS" ~ "FS",
                       condition == "CONDIZIONE SD" ~ "SD",
                       condition == "SD" ~ "SD",
                       condition == " SD" ~ "SD",
                       condition == " FS" ~ "FS"
         )),
         S = factor(case_when(session == "SESSIONE 1" ~ 1,
                              session == 1 ~ 1,
                              session == "SESSIONE 2" ~ 2,
                              session == 2 ~ 2)),
         R = factor(RUN),
         B = factor(B),
         rt = target_kb.rt,
         N_1 = ifelse(lag(target) == "go.png", "go.png","nogo.png"),
         N_1_rt = lag(rt),
         resp = target_kb.keys,
         Trial = TRIAL.thisN + 1)|>
  select(id, P, target, C, S, R, B, rt, resp, Trial,N_1,N_1_rt)

str(dat)

# Create Order Variable ----------------------
id_SD = subset(dat, C == "SD")|>
  select(id)|>
  unique()
id_FS = subset(dat, C == "FS" & S == 1)|>
  select(id)|>
  unique()

Order = rep("SD_FS",length(id_SD$id))
id_SD = cbind(id_SD, Order)
Order = rep("FS_FS",length(id_FS$id))
id_FS = cbind(id_FS, Order)

order_id = rbind(id_SD,id_FS)

# add data
dat = dat %>%
  left_join(.,order_id, by = "id")


subj_group = unique(dplyr::select(dat, c("id","Order")))
sum(subj_group$Order == "FS_FS", na.rm = T) # 36 tot
sum(subj_group$Order == "SD_FS", na.rm = T) # 43 tot


# Calculate accuracy -----------------------------------------------

dat$acc <- c()
dat$acc<- case_when(dat$target == "nogo.png" & dat$resp == "space" ~ 0,
                    dat$target == "nogo.png" &  is.na(dat$resp) ~ 1,
                    dat$target == "go.png" & dat$resp == "space" ~ 1,
                    dat$target == "go.png" & is.na(dat$resp) ~ 0)

sum(is.na(dat$acc)) #check
str(dat)

  
# save!
write_csv(dat, file = 'DATA/OUT/go_nogo.csv')

# Create dataset for VKF + LN -----------------------------------------------
go_nogo = dat
## Session 1
dat_clean_1 <- subset(go_nogo, S == 1 &
                        # low ACC
                        id != "GNG38" & id != "GNG59" & id != "GNG102" &
                        # PSQI = 14
                        id != "GNG82" & 
                        # not respect sleep schedule
                        id != "GNG16" & 
                        id != "GNG21" & id != "GNG40" & 
                        id != "GNG46" & id != "GNG51" &
                        id != "GNG52"& id != "GNG58" &
                        id != "GNG65" & id != "GNG72" &
                        id != "GNG75" &  id != "GNG83"&
                        id != "GNG92" & id != "GNG95"&
                        id != "GNG96" & id != "GNG105")

## Session 2
dat_clean_2 <- subset(go_nogo, S == 2 &
                        # low ACC
                        id != "GNG38" & id != "GNG59" & id != "GNG102" &
                        # PSQI = 14
                        id != "GNG82" & 
                        # not respect sleep schedule
                        id != "GNG16" & 
                        id != "GNG21" & id != "GNG40" & 
                        id != "GNG46" & id != "GNG51" &
                        id != "GNG52"& id != "GNG58" &
                        id != "GNG65" & id != "GNG72" &
                        id != "GNG75" &  id != "GNG83"&
                        id != "GNG92" & id != "GNG95"&
                        id != "GNG96" & id != "GNG105" &
                        #spec session 2
                        id != "GNG18" & id != "GNG29" &
                        id != "GNG34" & id != "GNG50")

df_clean <- rbind(dat_clean_1,dat_clean_2)

# Well-Rested -----------------------------------------------------------
dat_WR = subset(df_clean, Order == "FS_FS")|>
  dplyr::select(rt,id, P, Order, S, Trial,target)|>
  dplyr::mutate(subj = as.factor(id),
                C = as.factor(Order),
                SE = S == 1,
                GO = ifelse(target== "go.png", 1,0),
                NOGO = ifelse(target== "nogo.png", 1,0),
                TN = scale(Trial)[,1]/3,
                rt = ifelse(is.na(rt) | rt < .1 , 999, rt),
                N_subj = max(as.integer(as.factor(id))))

dat_fs = list(rt = dat_WR$rt,
              N_subj = unique(dat_WR$N_subj),
              N_tot = nrow(dat_WR),
              SE = ifelse(dat_WR$SE == 1, 0,1),
              subj = as.integer(as.factor(dat_WR$subj)),
              GO = dat_WR$GO,
              NOGO = dat_WR$NOGO,
              TN = dat_WR$TN,
              N_re = 10,
              N_trial = 448)

rt <- subset(dat_fs$rt, dat_fs$rt != 999)
dat_fs$N_rt = length(rt)

saveRDS(dat_fs,"DATA/OUT/dat_fs.rds")


# Sleep-Restricted -----------------------------------------------------------
dat_SR = subset(df_clean, Order == "SD_FS")|>
  dplyr::select(rt,id, P, Order, S, Trial,target)|>
  dplyr::mutate(subj = as.factor(id),
                C = as.factor(Order),
                SE = S, 
                GO = ifelse(target== "go.png", 1,0),
                NOGO = ifelse(target== "nogo.png", 1,0),
                TN = scale(Trial)[,1]/3,
                rt = ifelse(is.na(rt) | rt < .1 , 999, rt),
                N_subj = max(as.integer(as.factor(id))))

dat_sd = list(rt = dat_SR$rt,
              N_subj = unique(dat_SR$N_subj),
              N_tot = nrow(dat_SR),
              SE = ifelse(dat_SR$SE == 1, 0,1),
              subj = as.integer(as.factor(dat_SR$subj)),
              GO = dat_SR$GO,
              NOGO = dat_SR$NOGO,
              TN = dat_SR$TN,
              N_re = 10,
              N_trial = 448)

rt <- subset(dat_sd$rt, dat_sd$rt != 999)
dat_sd$N_rt = length(rt)

saveRDS(dat_sd,"DATA/OUT/dat_sd.rds")
