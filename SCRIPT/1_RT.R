# Reaction Times

set.seed(15595)

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages <- c("rstan", "dplyr", "readr", "brms", "tidyverse","ggpubr","emmeans",
              "sjPlot", "effectsize", "bayesplot", "bayestestR", "tidybayes",
              "HDInterval", "ggplot2")

sapply(packages, require, character.only = T)

theme_set(theme_pubclean(base_size = 14))

# 2. Load data  ---------------------------------------------------------------

go_nogo <- read_csv("DATA/OUT/go_nogo.csv")
ideal_pred <- read_csv("DATA/OUT/ideal_pred_vkf.csv")

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
# Extract id s2 completed
id_s2 <- unique(dat_clean_2$id)
write.csv(id_s2, "DATA/OUT/id_s2.csv")
str(df_clean)

df_ideal <- df_clean |>
  left_join(ideal_pred, by = "Trial")


# Clean -------------------------
df_go <- subset(df_ideal,   rt >.1 )|>
  dplyr::select(rt,id, P, Order, S, Trial, predictions,target)|>
  dplyr::mutate(id = as.factor(id),
                C = as.factor(Order),
                S = as.factor(S),
                PG = predictions/3,
                TAR = as.factor(ifelse(target== "go.png", "GO","NOGO")),
                TN = scale(Trial)[,1]/3,
                rt = rt)|>
  na.omit() #exclude misses

df_go %>%group_by(TAR)%>%
  summarise(cor_trialPG = cor(PG,rt))

min(df_go$PG);max(df_go$PG); #check 
min(df_go$TN);max(df_go$TN); #check 
min(df_go$rt);max(df_go$rt); #check 
#contrast for modeling
contrasts(df_go$C) <- contr.sum(2)/2;contrasts(df_go$C) 
contrasts(df_go$TAR) <- contr.sum(2)/2;contrasts(df_go$TAR) 

str(df_go$id)
contrasts(df_go$S)


# 3. Fit ---------------------------------------------------------------------------------
formula <- bf(rt ~ 1 +  C * PG  * TN * S + TAR + (1 + PG  * TN * S + TAR |gr(id, by = C)),
              family = lognormal())
# 
get_prior(formula = formula, data = df_go)
# 

# Set weakly informative priors 
priors <- c(#ndt
  #mu
  set_prior("student_t(2, -.9, 1)", class = "Intercept", lb = -3, ub = -.3),
  set_prior("normal(0, .1)", class = "b"),
  set_prior("normal(0.1,1.5)", class = "sd",lb = 0),
  
  # sigma
  set_prior("lkj(4)", class = "cor")
)


# Run model  -------------------------------------------------

mod_rt <- brm(formula = formula,
              data = df_go, 
              prior = priors,
              sample_prior = "yes", chains = 4,
              save_pars = save_pars(all = TRUE),
              cores = 4, iter = 6000)

# save 
save(mod_rt, file = "MODEL/mod_rt_vkf.rda", 
     compress = "xz")

load("MODEL/mod_rt_vkf.rda")

## 3.3 Check model --------------------------------------------------------------------

# Model convergence -------------------------------------
# R-hat and ESS
tail(sort(rstan::summary(mod_rt$fit)$summary[,"Rhat"]))
head(sort(rstan::summary(mod_rt$fit)$summary[,"n_eff"])) 
# Traces
plot(mod_rt) 

# 3.1 Posterior predictive check ---------------------------------------------------

# Plot
brms::pp_check(mod_rt, ndraws = 100, type = "dens_overlay_grouped", 
               group = "S")

brms::pp_check(mod_rt, ndraws = 100, type = "dens_overlay_grouped", 
               group = "C")


# 4. Results ---------------------------------------------------------------------------

summary(mod_rt)


## 4.2. HP test ---------------------------------------------------
nrow(summary(mod_rt)$fixed)
# Extract the summary as a tidy dataframe
post = data.frame(mod_rt)[2:17]
tab_summary = data.frame(parameter = rownames(summary(mod_rt)$fixed)[2:17],
                         mest = rep(NA, 16), low = rep(NA,16), high = rep(NA,16),
                         MPE = rep(NA, 16), bf01 = rep(NA, 16))
for (i in 1:16){
  tab_summary$mest[i]=round(mean(post[[i]]),2)
  tab_summary$low[i]=round(hdi(post[[i]], ci = .95)[1],2)
  tab_summary$high[i]=round(hdi(post[[i]], ci = .95)[2],2)
  tab_summary$MPE[i]=ifelse(mean(post[[i]]) > 0, mean(post[[i]]>0), mean(post[[i]]<0))
  tab_summary$bf01[i]=round(hypothesis(mod_rt, paste(tab_summary$parameter[[i]], "= 0"))$hypothesis$Evid.Ratio,2)}

tab_summary %>%
  kable("html", digits = 2, col.names = c("Parameter", "Estimate","Lower", "Upper", "MPE","BF01")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = TRUE)


## 4.1. Visualization Interaction ---------------------------------------------------------------------------------

# mu
p_mu <- sjPlot::plot_model(mod_rt, type = "pred", terms = c("TN [-.5,0,.5]","PG[-.5,0,.5]", 
                                                            "S","C"),
                           ci.lvl = .95, line.size = 1.5,
                           auto.label = FALSE, show.legend = F)+
  ylab("reaction times")+
  xlab("Time-on-Task") + 
  scale_x_continuous(breaks = seq(-.5,5, .5))+
  ggtitle(label = "")

# Relabel facets for clarity
p_mu$data$panel <- factor(p_mu$data$panel, labels = c("Well-Rested","Sleep-Restricted"))
p_mu$data$facet <- factor(p_mu$data$facet, labels = c("Session 1","Session 2"))
p_mu$data$group_col = interaction(p_mu$data$group_col, p_mu$data$panel)

# Define separate color palettes for each condition
well_rested_colors <- c("#71BDFF", "#3DA4FF", "#12385A") 
sleep_restricted_colors <- c("#FFBF71", "#FF9807", "#9A5F16")  

# Assign colors based on "panel"
p_mu <- p_mu + 
  scale_fill_manual(name = "PGo", 
                    values = rep(c(well_rested_colors, sleep_restricted_colors), each = 1),
                    labels = c("20%", "50%", "80%", "20%", "50%", "80%")) +
  scale_color_manual(name = "PGo", 
                     values = rep(c(well_rested_colors, sleep_restricted_colors), each = 1),
                     labels = c("20%", "50%", "80%", "20%", "50%", "80%")) +
  scale_y_continuous(limits = c(.32, .48), breaks = seq(.32,.48,.04))

p_mu  # Display the modified plot


p_mu_rt = p_mu
p_mu_rt

# Export plot
ggsave("PLOT/RT_vkf_mu.png", plot = p_mu_rt, dpi = 600, 
       width = 8, height = 6)
