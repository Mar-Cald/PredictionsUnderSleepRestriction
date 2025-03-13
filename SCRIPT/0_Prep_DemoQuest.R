# Pre-processing Demo and Quest


# 1. Import libraries ---------------------------------------------------------------

source("SCRIPT/utl/palette_plot.R")

packages = c("readr", "dplyr", "tidyverse", "ggplot2",
              "ggpubr", "patchwork", "lubridate")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))

## 2.Load data --------------------------------------------------------------

d = read_csv("DATA/RAW/data_quest/DemoQuest.csv")
d_group_id = read_csv("DATA/OUT/group_id.csv")
dat_PSQI = read_csv("DATA/RAW/data_quest/PSQI.csv")
id_2 = read_csv("DATA/OUT/id_s2.csv")
id_2$id = as.factor(id_2$x)

dat_all = d %>%
  left_join(dat_PSQI, by = "id")%>%
  mutate(PSQI = PSQI)

dat = d_group_id %>%
  left_join(dat_all, by = "id")

## 3. Prep data --------------------------------------------------------------

## Session 1

dat_clean_1 = subset(dat,
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

# Compute Age
dat_clean_1$Current_age = time_length(interval(dmy(dat_clean_1$Age), 
                                               ymd(Sys.Date())),"year")


## Session 2
dat_clean_2 = subset(dat,
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

dat_clean_2 = subset(dat_clean_2, id%in%id_2$id)

# Compute Age
dat_clean_2$Current_age = time_length(interval(dmy(dat_clean_2$Age), 
                                               ymd(Sys.Date())),"year")
# 3.1 Descriptive -----------------------------------

dat_all = rbind(dat_clean_1,dat_clean_2)
dat_all$Session = c(rep(1,nrow(dat_clean_1)), rep(2,nrow(dat_clean_2)))

# Age
descriptive_Age = dat_all %>%
  group_by(Order,Session) %>%
  dplyr::summarize(Current_age_mean = round(mean(Current_age,na.rm = TRUE)),
                   Current_age_sd = round(sd(Current_age,na.rm = TRUE)))

descriptive_Gender = dat_all %>%
  group_by(Order,Session) %>%
  dplyr::summarize(Fem = sum(Gender == "Femmina",na.rm = TRUE),
                   Mal = sum(Gender == "Maschio",na.rm = TRUE),
                   Non = sum(Gender == "Non mi identifico con nessuno dei precedenti", na.rm = TRUE))

Desc = descriptive_Age %>%
  left_join(descriptive_Gender, by = c("Order","Session"))

colnames(Desc) = c('Group','Session','Mean Age','Sd Age', 'Females', 'Males' ,'Non-Binary')


# Data
data = data.frame(
  Group = c("SR", "SR", "WR", "WR"),
  Session = c(1, 2, 1, 2),
  Age = c("25 (3)", "25 (3)", "24 (2)", "24 (2)"),
  Female = c(22, 20, 25, 24),
  Male = c(9, 9, 7, 7),
  Non_Binary = c(1, 1, 1, 0)
)

# Create the table
table = data %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)

# Output the table
print(table)


# Save Descriptive-----
write.csv(Desc, file = "DATA/OUT/Desc.csv")

# Save Data  Questionnaires----
write_csv(dat_all, file = "DATA/OUT/Quest.csv")

Descriptive_Quest = dat_all %>%
  group_by(Order) %>%
  dplyr::summarize(Depression_m = round(mean(D,na.rm = TRUE)),
                   Depression_sd = round(sd(D,na.rm = TRUE)),
                   Anxiety_m = round(mean(A,na.rm = TRUE)),
                   Anxiety_sd = round(sd(A,na.rm = TRUE)),
                   Stress_m = round(mean(S,na.rm = TRUE)),
                   Stress_sd = round(sd(S,na.rm = TRUE)),
                   
                   MEQ_m = round(mean(MEQ,na.rm = TRUE)),
                   MEQ_sd = round(sd(MEQ,na.rm = TRUE)),
                   PSQI_m = round(mean(PSQI,na.rm = TRUE)),
                   PSQI_sd = round(sd(PSQI,na.rm = TRUE)),
                   
                   Drive_m = round(mean(Drive,na.rm = TRUE)),
                   Drive_sd = round(sd(Drive,na.rm = TRUE)),
                   Fun_m = round(mean(Fun,na.rm = TRUE)),
                   Fun_sd = round(sd(Fun,na.rm = TRUE)),
                   Reward_m = round(mean(Reward,na.rm = TRUE)),
                   Reward_sd = round(sd(Reward,na.rm = TRUE)),
                   Punish_m = round(mean(Punish,na.rm = TRUE)),
                   Punish_sd = round(sd(Punish,na.rm = TRUE)),
                   
                   BIS_m = round(mean(BIS,na.rm = TRUE)),
                   BIS_sd = round(sd(BIS,na.rm = TRUE))
                   )

write_csv(Descriptive_Quest, file = "DATA/OUT/Quest_desc.csv")
