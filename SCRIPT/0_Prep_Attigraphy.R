# Attigraphy

## 0.Load Packages --------------------------------------------------------------

source("SCRIPT/utl/palette_plot.R")

packages <- c("readr", "readxl", "dplyr", "tidyverse", "ggplot2",
              "ggpubr", "patchwork", "lubridate", "kableExtra")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))

## 1.Load data --------------------------------------------------------------

# Session 1
d_1 <- read_excel("DATA/RAW/data_att/Session_1.xlsx") %>%
  select(id, TIB_h, TST_h, SE)
d_group_id <- read_csv("DATA/OUT/group_id.csv")
dat_1 <- d_group_id %>%
  left_join(d_1, by = "id")

d_2 <- read_excel("DATA/RAW/data_att/Session_2.xlsx")%>%
  select(id, TIB_h, TST_h, SE)
dat_2 <- d_group_id %>%
  left_join(d_2, by = "id")

## 2. Prep data --------------------------------------------------------------

## Session 1
dat_clean_1 <- subset(dat_1,
                      # low ACC
                       id != "GNG38" & id != "GNG59" & id != "GNG102" &
                        # PSQI = 14
                        id != "GNG82" & 
                        # not respect sleep schedule or attigraphy problems
                        id != "GNG16" & 
                        id != "GNG21" & id != "GNG40" & 
                        id != "GNG46" & id != "GNG51" &
                        id != "GNG52"& id != "GNG58" &
                        id != "GNG65" & id != "GNG72" &
                        id != "GNG75" &  id != "GNG83"&
                        id != "GNG92" & id != "GNG95"&
                        id != "GNG96" & id != "GNG105")
## Session 2
dat_clean_2 <- subset(dat_2,
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

# 2.1 Descriptive -----------------------------------

dat_all <- rbind(dat_clean_1,dat_clean_2)
dat_all$Session <- c(rep(1,nrow(dat_clean_1)), rep(2,nrow(dat_clean_2)))
dat_all$TIB_h <- as.numeric(dat_all$TIB_h);
dat_all$TST_h<- as.numeric(dat_all$TST_h);
dat_all$SE <- as.numeric(dat_all$SE);


descriptive_att <- dat_all %>%
  group_by(Order,Session) %>%
  dplyr::summarize(TST_mean = round(mean(TST_h,na.rm = TRUE),2),
                   TST_sd = round(sd(TST_h,na.rm = TRUE),2),
                   TIB_mean = round(mean(TIB_h,na.rm = TRUE),2),
                   TIB_sd = round(sd(TIB_h,na.rm = TRUE),2),
                   SE_mean = round(mean(SE,na.rm = TRUE),2),
                   SE_sd = round(sd(SE,na.rm = TRUE),2))


# Save Descriptive-----
write.csv(descriptive_att, file = "DATA/OUT/Desc_att.csv")



