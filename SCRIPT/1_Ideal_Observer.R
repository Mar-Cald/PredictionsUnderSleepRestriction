## Calculate trial-level Prob Go ##

set.seed(15595)

# 1. Import libraries ---------------------------------------------------------------
source("SCRIPT/utl/palette_plot.R")
source("SCRIPT/utl/functions.R")

packages = c("rstan", "readr", "patchwork", "ggpubr")
sapply(packages, require, character.only = T)
theme_set(theme_pubr(base_size = 24))

# 2.Load data -------------------------------------

go_nogo = read_csv("DATA/OUT/go_nogo.csv")

go = ifelse(go_nogo$target[1:448] == "go.png", 1,0)

dat_mod =list(N = 448, 
               u = go)

# 3. Fit ----------------------------------------
file = "
functions {
    // Sigmoid x vkf
  real sigmoid(real x) {
    return 1 / (1 + exp(-x));
  }
}

data {

  // number of observations
  int<lower=1> N;   

  //stimuli : go vs nogo
  vector[N] u;

}

parameters {

    // volatile kalman filter parameters
    real<lower = 1e-3, upper = 1-1e-3> sigma_v; //lambda
    real<lower = 1e-3, upper = 6> omega;
    real<lower = 1e-3, upper = 6> v0;

}
transformed parameters {
// volatile kalman filter (vkf)
    // vkf_bin parameters
    real mpre; // prediction n-1
    real wpre; // variance n-1
    real delta_m; // pred error
    real k; // kalman gain
    real wcov; // covariance
    real o; // input
    real delta_v; // input
    
    real m = 0;  // Initialize predictions
    real w = omega;  // Initialize posterior variances
    real v = v0;  // Initialize volatilities

    vector[N] predictions;
    vector[N] volatility;

      for (t in 1:N) {
      o = u[t]; // input
      mpre = m; // prediction
      wpre = w; // variance
      predictions[t] = m; 
      volatility[t] = v;
      delta_m = o - sigmoid(mpre); // prediction error
      k = (wpre + v) / (wpre + v + omega); // Kalman Gain/learning rate
      m = mpre + sqrt(wpre + v) * delta_m; // prediction update
      w = (1 - k) * (wpre + v);  // variance update
      wcov = (1 - k) * wpre; // covariance
      delta_v     = (m-mpre)^2 + w + wpre - 2*wcov - v;             
      v           = v + sigma_v*delta_v;
    }
}
model {
  real log_lik[N];
  real pred;

  // vkf model priors
  target += normal_lpdf(v0 | .5, 1);  
  target += normal_lpdf(omega | .5, 1);  
  target += normal_lpdf(sigma_v | .5, 1); 

  for (n in 1:N) {
      pred = sigmoid(predictions[n]);
      log_lik[n] = u[n]*log(pred) + (1-u[n])*log(1-pred);
    }
  target += sum(log_lik);
}
"

fit = stan(
  model_code = file,# Stan program
  data = dat_mod, chains = 3, cores = 3,
  iter = 4000)


# 4. Extract predictions for RTs and ACC analysis -------------
 
df_fit = rstan::get_posterior_mean(fit) 
predictions = df_fit[14:461,4]
predictions = data.frame(predictions = predictions, Trial = 1:448)

# Save!
write_csv(predictions, "DATA/OUT/ideal_pred_vkf.csv")


# 5. Plot ------------------------------------------------------------------------------------------------------------
#rm(list = ls())

go_nogo = read_csv("DATA/OUT/go_nogo.csv")

go = ifelse(go_nogo$target[1:448] == "go.png", 1,0)

dat_mod =list(N = 448, 
               u = go)

predictions = read.csv("DATA/OUT/ideal_pred_vkf.csv")

p_block = data.frame(predictions = c(rep(0.2,40),rep(0.8,40),rep(0.5,32),rep(0.8,40),rep(0.2,40),rep(0.5,32),
                      rep(0.8,40),rep(0.2,40),rep(0.5,32), rep(0.2,40),rep(0.8,40),rep(0.5,32)),
                     Trial = 1:448)

predictions = data.frame(predictions = sigmoid(predictions$predictions), Trial = predictions$Trial)

df_plot = rbind(p_block,predictions)
df_plot$GO = dat_mod$u
df_plot$NOGO = ifelse(dat_mod$u == 1, 0,1)

df_plot$type = c(rep("block_level",448),rep("trial_level",448))

pal = c("black", "green4")

pred = ggplot(df_plot,aes(x =Trial,y = predictions, color = type)) +
  geom_line(linewidth = 1.2, alpha = 0.9, show.legend = FALSE) +
  theme_classic()+xlab("Trial Number")+ylab("PGo")+
  scale_color_manual(values = pal)+
  geom_point(aes(y = GO), color = "orange", alpha= .2)


# Save Plot ---------------------------------------------
ggsave("PLOT/trialPGo_vkf_ideal.png",  pred, width = 10, height = 3)

