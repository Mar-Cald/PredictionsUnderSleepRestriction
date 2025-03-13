# Accuracy

set.seed(15595)

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages <- c("rstan", "dplyr", "readr", "brms", "tidyverse","ggpubr",
              "sjPlot", "effectsize", "bayesplot", "bayestestR", "HDInterval")
sapply(packages, require, character.only = T)

theme_set(theme_pubr(base_size = 12))


# 2. Load data  ---------------------------------------------------------------

go_nogo <- read_csv("DATA/OUT/go_nogo.csv")
ideal_pred <- read_csv("DATA/OUT/ideal_pred_vkf.csv")

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
df_ideal <- df_clean %>%
  left_join(ideal_pred, by = "Trial")


# ACC -------------------------------------
df_nogo <- subset(df_ideal, target == "nogo.png" &  (is.na(rt) |rt> .1))%>%
  dplyr::select(acc,id, P, Order, S, Trial, predictions)%>%
  dplyr::mutate(id = as.factor(id),
                C = as.factor(Order),
                S = as.factor(S),
                PG = predictions/3,
                TN = scale(Trial)[,1]/3)

#contrast for modeling
contrasts(df_nogo$C) <- contr.sum(2)/2;contrasts(df_nogo$C)
str(df_nogo$id)
contrasts(df_nogo$S)

# 3. Fit ---------------------------------------------------------------------------------
formula <- bf(acc | trials(1) ~ 1 + C * PG  * TN * S + (1 + PG * TN * S|gr(id, by = C)),
              family = binomial())
# 
get_prior(formula = formula, data = df_nogo)
# 

# Set weakly informative priors 
priors <- c(
  set_prior("normal(0, 1.5)", class = "Intercept"),
  set_prior("normal(0, 1.5)", class = "b"),
  set_prior("normal(0.5, 1)", class = "sd",lb = 0),
  
  set_prior("lkj(4)", class = "cor")
)

# Run model  -------------------------------------------------

mod_acc <- brm(formula = formula,
               data = df_nogo,
               prior = priors,
               sample_prior = "yes", chains = 4,
               save_pars = save_pars(all = TRUE),
               cores = 4, iter = 6000)


# save 
save(mod_acc, file = "MODEL/mod_acc_vkf.rda", 
     compress = "xz")


## 3.3 Check model --------------------------------------------------------------------
load("MODEL/mod_acc_vkf.rda")

summary(mod_acc)
# R-hat and ESS
tail(sort(rstan::summary(mod_acc$fit)$summary[,"Rhat"]))
head(sort(rstan::summary(mod_acc$fit)$summary[,"n_eff"])) 
# Traces
plot(mod_acc) 


# 3.1 Posterior predictive check ---------------------------------------------------

brms::pp_check(mod_acc, ndraws = 100, type = "dens_overlay_grouped", 
               group = "C")


# 4. Results ---------------------------------------------------------------------------

nrow(summary(mod_acc)$fixed)
# Extract the summary as a tidy dataframe
post = data.frame(mod_acc)[2:16]
tab_summary = data.frame(parameter = rownames(summary(mod_acc)$fixed)[2:16],
                         mest = rep(NA, 15), low = rep(NA,15), high = rep(NA,15),
                         MPE = rep(NA, 15), bf01 = rep(NA, 15))
for (i in 1:15){
  tab_summary$mest[i]=round(mean(post[[i]]),2)
  tab_summary$low[i]=round(hdi(post[[i]], ci = .95)[1],2)
  tab_summary$high[i]=round(hdi(post[[i]], ci = .95)[2],2)
  tab_summary$MPE[i]=ifelse(mean(post[[i]]) > 0, mean(post[[i]]>0), mean(post[[i]]<0))
  tab_summary$bf01[i]=round(hypothesis(mod_acc, paste(tab_summary$parameter[[i]], "= 0"))$hypothesis$Evid.Ratio,2)
  }


tab_summary %>%
  kable("html", digits = 2, col.names = c("Parameter", "Estimate","Lower", "Upper", "MPE","BF01")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = TRUE)


## 4.1. Visualization Interaction ---------------------------------------------------------------------------------

# mu
p_mu <- sjPlot::plot_model(mod_acc, type = "pred", terms = c("TN [-.5,0,.5]","PG[-.5,0,.5]", 
                                                            "S","C"),
                           ci.lvl = .95, line.size = 1.5,bias_correction = TRUE,
                           auto.label = FALSE, show.legend = F)+
  ylab("accuracy")+
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
  scale_y_continuous(limits = c(.9, 1), breaks = seq(.9,1,.02))

p_mu  # Display the modified plot


p_mu_acc = p_mu
p_mu_acc

# Export plot
ggsave("PLOT/ACC_vkf_mu.png", plot = p_mu_acc, dpi = 600, 
       width = 8, height = 8)
