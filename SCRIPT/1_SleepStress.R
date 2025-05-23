# Analysis Sleep and Stress Ratings

set.seed(15595)

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages = c("rstan", "dplyr", "readr", "brms", "tidyverse", "ggpubr",
              "sjPlot", "effectsize", "bayesplot", "bayestestR",
              "patchwork")
sapply(packages, require, character.only = T)


theme_set(theme_pubr(base_size = 12))

# 2. Load data  ---------------------------------------------------------------

dat = read_csv("DATA/OUT/SleepStress.csv")|>
  mutate(Group = factor(ifelse(as.factor(Order) == "WR_WR","WR","SR")),
         Sleep = sleep_kb.keys,
         Stress = stress_slide.response,
         Session = as.factor(ifelse(session == 1, 0,1)))|>
  select(id, Group, Session,Sleep,Stress)

hist(dat$Sleep)
hist(dat$Stress)

# 2.1 Fit Sleep model ---------------------------------------------------------------------------------
contrasts(dat$Group) = contr.sum(2)/2
contrasts(dat$Session)

formula = bf(Sleep ~ 1+  Group*Session,
              family = cumulative(link = probit))

# look at default priors
get_prior(formula = formula, data = dat)

priors = c(
  set_prior("normal(0, 1)", class = "b"))


mod_sleep = brm(formula = formula,
                 prior = priors,
                data = dat, 
                sample_prior = "yes", chains = 4,
                save_pars = save_pars(all = TRUE),
                cores = 4, iter = 6000)

# save 
save(mod_sleep, file = "MODEL/mod_sleep.rda", 
     compress = "xz")

## 2.2 Check Sleep model --------------------------------------------------------------------
load("MODEL/mod_sleep.rda")

# R-hat and ESS
tail(sort(rstan::summary(mod_sleep$fit)$summary[,"Rhat"]))
head(sort(rstan::summary(mod_sleep$fit)$summary[,"n_eff"])) 
# Traces
plot(mod_sleep) 

# Posterior predictive 
brms::pp_check(mod_sleep, ndraws = 100, type = "dens_overlay_grouped", 
               group = c("Session"))
brms::pp_check(mod_sleep, ndraws = 100, type = "dens_overlay_grouped", 
               group = "Group")


# 2.3 Results ---------------------------------------------------------------------------

summary(mod_sleep, prob = .95)
post = data.frame(mod_sleep)
round(HDInterval::hdi(post),2)
p_direction(mod_sleep)

# test HP
hypothesis(mod_sleep, "Group1 = 0")
hypothesis(mod_sleep, "Session1 = 0")
hypothesis(mod_sleep, "Group1:Session1 = 0")

conditions = make_conditions(mod_sleep, "Session")
conditions$cond__ = c("Session 1","Session 2")
sleep_p = conditional_effects(mod_sleep, categorical = TRUE, 
                    conditions = conditions)

# define count
cou=c("1", "2", "3", "4", "5", "6", "7","8")

# define 7 colours from black to red
colours = brewer.pal(n = 8, name = "Spectral")
# create values for scale colour manual
values=setNames(rev(colours), cou)

p_sleep = plot(sleep_p, plot = FALSE)[[1]]+
  scale_color_manual(values=values) +
  scale_fill_manual(values=values) +
  theme_pubr(base_size = 12)
p_sleep$labels$fill = "Sleepiness"
p_sleep$labels$colour = "Sleepiness"
p_sleep
ggsave("PLOT/Sleep.png", plot = p_sleep, dpi = 300, 
       width = 8, height = 4)

# 3.1 Fit Stress model ---------------------------------------------------------------------------------

formula = bf(Stress ~ 1 + Group*Session,
              family = gaussian())

# look at default priors
get_prior(formula = formula, data = dat)

priors = c(set_prior("normal(5, 2)", class = "Intercept", 
                     lb = 1, ub = 10),
  set_prior("normal(0, 1.5)", class = "b"))

mod_stress = brm(formula = formula,
                 data = dat, 
                 prior = priors,
                 sample_prior = "yes", 
                 chains = 4,
                 save_pars = save_pars(all = TRUE),
                 cores = 4, iter = 6000)

# save 
save(mod_stress, file = "MODEL/mod_stress.rda", 
     compress = "xz")

## 3.2 Check Sleep model --------------------------------------------------------------------
load("MODEL/mod_stress.rda")

# Model convergence  -------------------------------------
# R-hat and ESS
tail(sort(rstan::summary(mod_stress$fit)$summary[,"Rhat"]))
head(sort(rstan::summary(mod_stress$fit)$summary[,"n_eff"])) 
# Traces
plot(mod_stress) 

# Posterior predictive check 
brms::pp_check(mod_stress, ndraws = 100, type = "dens_overlay_grouped", 
               group = "Session")
brms::pp_check(mod_stress, ndraws = 100, type = "dens_overlay_grouped", 
               group = "Group")


# 3.3 Results ---------------------------------------------------------------------------

summary(mod_stress,prob = .95)
p_direction(mod_stress)
post = data.frame(mod_stress)
round(HDInterval::hdi(post),2)

# test HP
hypothesis(mod_stress, "Group1 = 0")
hypothesis(mod_stress, "Session1 = 0")
hypothesis(mod_stress, "Group1:Session1 = 0")


# visualize
p_stress = sjPlot::plot_model(mod_stress, type = "pred", terms = c("Session","Group"),
                             ci.lvl = .95, line.size = 1,dot.alpha = 0.2,
                             dot.size = 3.5,color = pal_okabe_ito_2[c(2,1)], 
                             show.data = T, jitter = .2,auto.label = FALSE,
                             axis.title = c("Session","stress ratings"),
                             pred.labels = c("1","2")) +
  scale_x_continuous(breaks = c(0,1), labels=c("1","2"))+
  ggtitle(label ="")


p_stress