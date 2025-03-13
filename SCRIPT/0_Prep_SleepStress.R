# Pre-processing Sleepiness and Stress ratings


# 1. Import libraries ---------------------------------------------------------------

source("SCRIPT/utl/palette_plot.R")

packages = c("readr", "dplyr", "tidyverse", "ggplot2",
              "ggpubr", "patchwork")

sapply(packages, require, character.only = T)


theme_set(theme_pubr(base_size = 12))

## 2. Load data --------------------------------------------------------------

d = list.files(path = "DATA/RAW/data_exp", pattern = "*.csv", full.names = TRUE) %>%
  map_df(~read_csv(.x) %>% mutate(condition = as.character(condition),
                                  session = as.character(session))) # Combine data sets into one data set 


## 3. Prep data --------------------------------------------------------------

#select columns of interest and adjust names
dat = d |>
  select(participant, condition, session, sleep_kb.keys, stress_slide.response)|>
  mutate(id = case_when(participant == "GNG87"~ "GNG84",
                        participant == "GN103"~ "GNG103",
                        participant == "GN100"~ "GNG100",
                        .default = participant),
         C = factor(case_when(condition == "CONDIZIONE FS" ~ "WR",
                              condition == "FF" ~ "WR",
                              condition == "fs" ~ "WR",
                              condition == "sd" ~ "SR",
                              condition == "FS" ~ "WR",
                              condition == "CONDIZIONE SD" ~ "SR",
                              condition == "SD" ~ "SR",
                              condition == " SD" ~ "SR",
                              condition == " FS" ~ "WR"
         )),
         S = factor(case_when(session == "SESSIONE 1" ~ 1,
                              session == 1 ~ 1,
                              session == "SESSIONE 2" ~ 2,
                              session == 2 ~ 2)))|>
  na.omit()


# Create Order Variable for session 2 analysis ----------------------
id_SD = subset(dat, C == "SR")|>
  select(id)|>
  unique()
id_FS = subset(dat, C == "WR" & S == 1)|>
  select(id)|>
  unique()

Order = rep("SR_WR",length(id_SD$id))
id_SD = cbind(id_SD, Order)
Order = rep("WR_WR",length(id_FS$id))
id_FS = cbind(id_FS, Order)

order_id = rbind(id_SD,id_FS)

# add data
dat = dat %>%
  left_join(.,order_id, by = "id")

# Exclude participants data --------------------------------------------

## Session 1
dat_clean_1 = subset(dat, S == 1 &
                     # low ACC
                     id != "GNG38" & id != "GNG59" & id != "GNG102" &
                       #PSQI = 14 
                       id != "GNG82"&
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
dat_clean_2 = subset(dat, S == 2 &
                        # low ACC
                        id != "GNG38" & id != "GNG59" & id != "GNG102" &
                        #PSQI = 14 
                        id != "GNG82"&
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

dat_clean = rbind(dat_clean_1,dat_clean_2)

# save!
write_csv(dat_clean, file = "DATA/OUT/SleepStress.csv")


