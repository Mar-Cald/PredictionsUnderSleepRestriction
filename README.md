# PredictionsUnderSleepRestriction
Data and scripts for the paper *Predictions Under Sleep Restriction*.  
Download the folder and open *PredictionUnderSleepRestriction.Rproj* file.
Preprint:https://osf.io/preprints/psyarxiv/x7m4z_v2

### **Repository Structure** 

#### **DATA**
- **RAW**: contains all raw data
    -  data_att (data extracted from actigraphy)
    -  data_diary (data extracted from sleep diaries)
    -  data_exp (individual participant data - experimental task)
    -  data_quest (sum scores and item, questionnaires data)
####
- **OUT**: contains all processed data
    -  dat_fs.rds: well rested data GoNoGo task
    -  dat_sd.rds: sleep restricted data GoNoGo task
    -  Desc_att.csv: descriptive statistics attigraphic data
    -  Desc.csv: descriptive statistics demographic data
    -  go_nogo.csv: data from the Go/Nogo task
    -  group_id.csv: id and group identifiers
    -  Quest.csv: data from questionnaires
    -  Desc_quest:  descriptive statistics questionnaires
    -  SleepStress.csv: data from sleepiness and stress ratings
###
#### **SCRIPT**
Preprocessing (0_..) and analysis (1_..)
- **VKF_LN**:
    -  0_VKF_SD.R: fit model sleep restricted
    -  0_VKF_FS.R: fit model well rested
    -  1_VKF_Res.R: results
    -  2_VKF_PP_FS.R: posterior predictive well rested
    -  2_VKF_PP_SD.R: posterior predictive sleep restricted
    -  2_VKF_PP_Pgo.R: predicted pGo
    -  2_VKF_PP_volatility.R: predicted volatility
    -  model_file_complete.stan: stan vkf_ln model file
    -  model_file_complete_pp.stan: stan model file posterior predict
###
#### **MODEL**
Contains all bayesian models
###
#### **PLOT**
Contains all plots 
###


#### Experimental task available at: https://gitlab.pavlovia.org/mar.cald/cgng
