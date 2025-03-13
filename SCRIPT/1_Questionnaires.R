# Analysis Questionnaires

set.seed(15595)

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages <- c("rstan", "dplyr", "readr", "brms", "tidyverse", "ggpubr",
              "sjPlot", "effectsize", "bayesplot", "bayestestR",
              "patchwork", "kableExtra")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))

# 2. Load data  ---------------------------------------------------------------

dat_all <- read_csv("DATA/OUT/Quest.csv") %>%
  mutate(Group = ifelse(as.factor(Order) == "WR_WR","WR","SR")) %>%
  select(-Session)

dat_all$Group <- as.factor(dat_all$Group)
contrasts(dat_all$Group) <- contr.sum(2)/2

dat_all <- unique(dat_all)

# 2.1 Fit Model ---------------------------------------------------------------------------------

formula <- bf(mvbind(S, A, D, MEQ, Drive, Fun, Reward, Punish, BIS, PSQI)
              ~ Group, set_rescor(TRUE), family = gaussian())

# look at default priors
get_prior(formula = formula, data = dat_all)

priors <- c(  
  set_prior("student_t(2, 17, 2)", class = "Intercept", resp = "S",
            lb = 0, ub = 38),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "S",
            lb = -5, ub = 5),
  set_prior("student_t(2, 8, 2)", class = "Intercept", resp = "A",
            lb = 0, ub = 26),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "A",
            lb = -5, ub = 5),
  set_prior("student_t(2, 8, 2)", class = "Intercept", resp = "D",
            lb = 0, ub = 36),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "D",
            lb = -5, ub = 5),
  
  set_prior("student_t(2, 8, 2)", class = "Intercept", resp = "MEQ",
            lb = 7, ub = 23),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "MEQ",
            lb = -5, ub = 5),
  
  set_prior("student_t(2, 11, 2)", class = "Intercept", resp = "Drive",
            lb = 6, ub = 19),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "Drive",
            lb = -5, ub = 5),
  set_prior("student_t(2, 11, 2)", class = "Intercept", resp = "Fun",
            lb = 5, ub = 19),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "Fun",
            lb = -5, ub = 5),
  set_prior("student_t(2, 20, 2)", class = "Intercept", resp = "Reward",
            lb = 16, ub = 25),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "Reward",
            lb = -5, ub = 5),
  set_prior("student_t(2, 20, 2)", class = "Intercept", resp = "Punish",
            lb = 7, ub = 25),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "Punish",
            lb = -5, ub = 5),
  
  set_prior("student_t(2, 68, 2)", class = "Intercept", resp = "BIS",
            lb = 49, ub = 89),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "BIS",
            lb = -5, ub = 5),
  
  set_prior("student_t(2, 6, 2)", class = "Intercept", resp = "PSQI",
            lb = 1, ub = 14),
  set_prior("student_t(2, 0, 2)", class = "b", resp = "PSQI",
            lb = -5, ub = 5),
  
  set_prior("lkj(2)", class = "rescor"))


mod <- brm(formula = formula,
                 prior = priors,
                 data = dat_all, 
                 sample_prior = "yes", chains = 4,
                 save_pars = save_pars(all = TRUE),
                 cores = 4, iter = 6000)

# save 
save(mod, file = "MODEL/mod_quest.rda", 
     compress = "xz")

## 2.2 Check model --------------------------------------------------------------------
load(file = "MODEL/mod_quest.rda")


# R-hat and ESS
tail(sort(rstan::summary(mod$fit)$summary[,"Rhat"]))
head(sort(rstan::summary(mod$fit)$summary[,"n_eff"])) 
# Traces
plot(mod) 

# Posterior predictive 
brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "A",
               group = "Group")
brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "S",
               group = "Group")
brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "D",
               group = "Group")

brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "MEQ",
               group = "Group")

brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "Drive",
               group = "Group")
brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "Reward",
               group = "Group")
brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "Fun",
               group = "Group")
brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "Punish",
               group = "Group")

brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "BIS",
               group = "Group")

brms::pp_check(mod, ndraws = 100, type = "dens_overlay_grouped", 
               resp = "PSQI",
               group = "Group")


# 2.3 Results ---------------------------------------------------------------------------

summary(mod, prob = .95)

# test HP
hypothesis(mod, "S_Group1 = 0")
hypothesis(mod, "A_Group1 = 0")
hypothesis(mod, "D_Group1 = 0")

hypothesis(mod, "MEQ_Group1 = 0")

hypothesis(mod, "Drive_Group1 = 0")
hypothesis(mod, "Reward_Group1 = 0")
hypothesis(mod, "Fun_Group1 = 0")
hypothesis(mod, "Punish_Group1 = 0")

hypothesis(mod, "BIS_Group1 = 0")
hypothesis(mod, "PSQI_Group1 = 0")

p_direction(mod)


#----residual correlations ------
library(kableExtra)
corri <- summary(mod)$rescor_pars
corr <- corri[,c(1,3:4)]
kable(corr, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive"))

plot(corr$Estimate)






