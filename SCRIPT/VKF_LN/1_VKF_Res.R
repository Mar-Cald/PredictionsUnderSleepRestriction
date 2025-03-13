# Results form VKF-LN models

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")

packages = c("rstan", "dplyr", "readr", "tidyverse","ggpubr","emmeans",
              "sjPlot", "effectsize", "bayesplot", "bayestestR", "tidybayes",
              "kableExtra", "patchwork")
sapply(packages, require, character.only = T)

theme_set(theme_pubclean(base_size = 8))

# 2. Load Models  ---------------------------------------------------------------

FS = readRDS("MODEL/VKF_LN/fs_complete.rds")
SD = readRDS("MODEL/VKF_LN/sd_complete.rds")

## 3 Check model --------------------------------------------------------------------
# R-Hat
tail(sort(rstan::summary(FS)$summary[,"Rhat"])) # ok
tail(sort(rstan::summary(SD)$summary[,"Rhat"])) # ok
# ESS
head(sort(rstan::summary(FS)$summary[,"n_eff"])) # ok
head(sort(rstan::summary(SD)$summary[,"n_eff"])) # ok
# Traces
traceplot(FS) # good chain convergence
traceplot(SD) # good chain convergence

# Corr parameters, post, sampling
p_cor_post_FS = bayesplot::mcmc_pairs(FS, off_diag_args = list(size = .5, alpha = .5),
                                      pars = c("lambda", "omega", "beta_om_se",
                                               "intercept","beta", "beta_se" ,"beta_p_se",
                                               "beta_nogo","beta_tn","beta_tn_se","sigma"))


p_cor_post_SD = bayesplot::mcmc_pairs(SD, off_diag_args = list(size = .5, alpha = .5),
                                      pars = c("lambda", "omega", "beta_om_se",
                   "intercept","beta", "beta_se" ,"beta_p_se",
                   "beta_nogo","beta_tn","beta_tn_se","sigma"))

# Export plot
ggsave("PLOT/p_cor_post_FS.png", plot = p_cor_post_FS, dpi = 300, 
       width = 18, height = 10)
ggsave("PLOT/p_cor_post_SD.png", plot = p_cor_post_SD, dpi = 300, 
       width = 18, height = 10)

# 4. Summary of Model Output ---------------------------------------------------------------------

post_fs =as.data.frame(FS)
post_sd =as.data.frame(SD)

# Back transform omega exp to real
post_fs$omega_tran_1 = exp(post_fs$omega)
post_sd$omega_tran_1 = exp(post_sd$omega)
post_fs$omega_tran_2 = exp(post_fs$omega + post_fs$beta_om_se)
post_sd$omega_tran_2 = exp(post_sd$omega + post_sd$beta_om_se)
# Back transform lambda prob to real
post_fs$lam_tran = pnorm(post_fs$lambda)
post_sd$lam_tran = pnorm(post_sd$lambda)


# Summary Table FS
tab_summary_FS = data.frame(parameter = colnames(post_fs[1:11]))
for (i in 1:11){
  tab_summary_FS$mest[i]=round(mean(post_fs[[i]]),2)
  tab_summary_FS$low[i]=round(HDInterval::hdi(post_fs[[i]], ci = .95)[1],2)
  tab_summary_FS$high[i]=round(HDInterval::hdi(post_fs[[i]], ci = .95)[2],2)
}

tab_summary_FS %>%
  kable("html", digits = 2, col.names = c("Parameter", "Estimate","Lower", "Upper")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = FALSE)

# Summary Table SD
tab_summary_SD = data.frame(parameter = colnames(post_sd[1:11]))
for (i in 1:11){
  tab_summary_SD$mest[i]=round(mean(post_sd[[i]]),2)
  tab_summary_SD$low[i]=round(HDInterval::hdi(post_sd[[i]], ci = .95)[1],2)
  tab_summary_SD$high[i]=round(HDInterval::hdi(post_sd[[i]], ci = .95)[2],2)
}

tab_summary_SD %>%
  kable("html", digits = 2, col.names = c("Parameter", "Estimate","Lower", "Upper")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = FALSE)

# 5. HP Test ---------------------------------------------
## 5.1 Perceptual component 
### Lambda
lam_contra = post_fs$lam_tran - post_sd$lam_tran
MPE = round(ifelse(mean(lam_contra) > 0, mean(lam_contra>0), 
                   mean(lam_contra<0)),2);MPE
mest = round(mean(lam_contra),2);mest
descr = round(hdi(lam_contra, ci = .95),2);descr

#lam WR
mest = round(mean(post_fs$lam_tran),2);mest
descr = round(hdi(post_fs$lam_tran, ci = .95),2);descr

#lam SD
mest = round(mean(post_sd$lam_tran),2);mest
descr = round(hdi(post_sd$lam_tran, ci = .95),2);descr


### Omega S1
om_contra_1 = post_fs$omega_tran_1 - post_sd$omega_tran_1
MPE = round(ifelse(mean(om_contra_1) > 0, mean(om_contra_1>0), 
                   mean(om_contra_1<0)),2);MPE
mest = round(mean(om_contra_1),2);mest
descr = round(hdi(om_contra_1, ci = .95),2);descr

### Omega S2
om_contra_2 = post_fs$omega_tran_2 - post_sd$omega_tran_2
MPE = round(ifelse(mean(om_contra_2) > 0, mean(om_contra_2>0), 
                   mean(om_contra_2<0)),2);MPE
mest = round(mean(om_contra_2),2);mest
descr = round(hdi(om_contra_2, ci = .95),2);descr


## 5.2 Response component
### Intercept
int_cont = post_fs$intercept - post_sd$intercept
MPE = round(ifelse(mean(int_cont) > 0, mean(int_cont>0), 
                   mean(int_cont<0)),2);MPE
mest = round(mean(int_cont),2);mest
descr = round(HDInterval::hdi(int_cont, ci = .95),2);descr


### Beta PGo
beta_cont = post_fs$beta - post_sd$beta
MPE = round(ifelse(mean(beta_cont) > 0, mean(beta_cont>0), 
                   mean(beta_cont<0)),2);MPE
mest = round(mean(beta_cont),2);mest
descr = round(HDInterval::hdi(beta_cont, ci = .95),2);descr

### Beta Session
beta_cont = post_fs$beta_se - post_sd$beta_se
MPE = round(ifelse(mean(beta_cont) > 0, mean(beta_cont>0), 
                   mean(beta_cont<0)),2);MPE
mest = round(mean(beta_cont),2);mest
descr = round(HDInterval::hdi(beta_cont, ci = .95),2);descr

### Beta NoGo
betanogo_cont = post_fs$beta_nogo - post_sd$beta_nogo
MPE = round(ifelse(mean(betanogo_cont) > 0, mean(betanogo_cont>0), 
                   mean(betanogo_cont<0)),2);MPE
mest = round(mean(betanogo_cont),2);mest
descr = round(HDInterval::hdi(betanogo_cont, ci = .95),2);descr

### Beta TN
betatn_cont = post_fs$beta_tn - post_sd$beta_tn
MPE = round(ifelse(mean(betatn_cont) > 0, mean(betatn_cont>0), 
                   mean(betatn_cont<0)),2);MPE
mest = round(mean(betatn_cont),2);mest
descr = round(HDInterval::hdi(betatn_cont, ci = .95),2);descr

### Beta PGo Session
beta_p_se_cont = post_fs$beta_p_se - post_sd$beta_p_se
MPE = round(ifelse(mean(beta_p_se_cont) > 0, mean(beta_p_se_cont>0), 
                   mean(beta_p_se_cont<0)),2);MPE
mest = round(mean(beta_p_se_cont),2);mest
descr = round(HDInterval::hdi(beta_p_se_cont, ci = .95),2);descr

### Beta TN Session
beta_tn_se_cont = post_fs$beta_tn_se - post_sd$beta_tn_se
MPE = round(ifelse(mean(beta_tn_se_cont) > 0, mean(beta_tn_se_cont>0), 
                   mean(beta_tn_se_cont<0)),2);MPE
mest = round(mean(beta_tn_se_cont),2);mest
descr = round(HDInterval::hdi(beta_tn_se_cont, ci = .95),2);descr






# Plot Omega and Lambda ----------------------------------------------------------------------------
df_plot = data.frame(omega =c(post_fs$omega_tran_1,post_fs$omega_tran_2,
                               post_sd$omega_tran_1,post_sd$omega_tran_2),
                      lambda = c(post_fs$lam_tran,post_fs$lam_tran,
                                 post_sd$lam_tran,post_sd$lam_tran),
                      Group = c(rep("WR", length(post_fs$omega_tran_1) + 
                                      length(post_fs$omega_tran_2)), 
                                rep("SR",length(post_sd$omega_tran_1) + 
                                      length(post_sd$omega_tran_2))),
                      Session = c(rep(c("Session 1","Session 2"), 
                                    each = length(post_fs$omega_tran_1),
                                    times = 2)))

p_omega_1 = ggplot(subset(df_plot, Session == "Session 1"), aes(x = omega, fill = Group))+
  geom_density(alpha = .8, lwd = .8, adjust = 1.5)+ 
  scale_fill_manual(values = pal_okabe_ito_2[c(2,1)])+
  scale_x_continuous(breaks = seq(0,2, 0.2),
                     limits = c(0,2)) + ggtitle("Session 1")
p_omega_2 = ggplot(subset(df_plot, Session == "Session 2"), aes(x = omega, fill = Group))+
  geom_density(alpha = .8, lwd = .8, adjust = 1.5)+ 
  scale_fill_manual(values = pal_okabe_ito_2[c(2,1)])+
  scale_x_continuous(breaks = seq(0,2, 0.2),
                     limits = c(0,2)) + ggtitle("Session 2")

p_omega_1|p_omega_2






