functions {
  // Sigmoid x vkf
  real sigmoid(real x) {
    return 1 / (1 + exp(-x));
  }
}
data {
  
  // number of observations x subj
  int<lower = 1> N_trial;
  // number of participants
  int<lower = 1> N_subj;
  int<lower = 1> N_tot;
  array[N_tot] int<lower=1> subj;
  int<lower = 1> N_rt;
  
  //stimuli : go vs nogo (i.e 1 vs 0)
  vector[N_tot] GO;
  vector[N_tot] NOGO;
  array[N_tot] int<lower=0, upper=1> SE;
  vector[N_tot] TN;

  // response
  array[N_tot] real rt;

  // number of individual adj
  int<lower = 1> N_re;
}
parameters {
  
/// volatile kalman filter parameters
  real omega;
  real beta_om_se; //effect of Session on omega
  //real v0; fixed to omega
  real lambda; 

  // regression model parameters
  // sigma
  real<lower = 0> sigma;
  // mu
  // Group
  real<lower = -3, upper = 3> intercept;
  real<lower = -2, upper = 2> beta; // effect PGo
  real<lower = -2, upper = 2> beta_nogo;
  real<lower = -2, upper = 2> beta_se; //effect of Session
  real<lower = -2, upper = 2> beta_p_se; //intercation PGo x Session
  real<lower = -2, upper = 2> beta_tn; //TN
  real<lower = -2, upper = 2> beta_tn_se; //intercation TN x Session
  

  // Individual 
  vector<lower = 0>[N_re] tau_u;
  matrix[N_subj, N_re] u;
  
}
model {
}
generated quantities {
  
  // loop
  array[N_rt] real rt_pred;
  int i = 1;
  int tn;

  // regression model
  real mu;
  
  // volatile kalman filter (vkf)
  real mpre; // prediction n-1
  real wpre; // variance n-1
  real k; // kalman gain
  real wcov; // covariance
  real o; // input
  real delta_v; 
  
  real om = exp(omega + u[subj[1], 1] + SE[1] * (beta_om_se + u[subj[1], 2]));
  real v_0 = om; //init vol
  real w = om;
  real v = v_0;  // Initialize volatilities
  real m = 0; // Initialize predictions
  
  vector[N_tot] predictions;
  vector[N_rt] PGo;
  vector[N_tot] volatility;
  vector[N_tot] delta_m; //pred_error


  for (n in 1:N_tot) {
    
        o = GO[n]; // input
        predictions[n] = sigmoid(m)-.5; 
        volatility[n] = v; 
      
        mpre = m; // p_prediction
        wpre = w; // p_variance
      
        delta_m[n] = o - sigmoid(mpre); // prediction error
        k = (wpre + v) / (wpre + v + (exp(omega + u[subj[n], 1] + SE[n] *
        (beta_om_se + u[subj[n], 8])))); // Kalman Gain/learning rate
        m = mpre + sqrt(wpre + v) * delta_m[n]; // update
        w = (1 - k) * (wpre + v);  // variance update
        wcov = (1 - k) * wpre; // covariance
        delta_v     = (m-mpre)^2 + w + wpre - 2*wcov - v;  // volatility per
        v   = v + Phi(lambda + u[subj[n],2]) * delta_v; // volatility update = sigma_v fixed to 0.1
      
    // regression model
    if (rt[n]!= 999){
    
    mu = intercept + u[subj[n], 4] + NOGO[n] * (beta_nogo + u[subj[n],5])+ 
    predictions[n] * (beta + u[subj[n],6]) + SE[n] * (beta_se + u[subj[n],7]) +
    predictions[n] * SE[n] * (beta_p_se + u[subj[n],8]) +
    TN[n] * (beta_tn + u[subj[n],9]) +
    TN[n] * SE[n] * (beta_tn_se + u[subj[n],10]);
    
    PGo[i] =  predictions[n];
        // lognormal
    rt_pred[i] = lognormal_rng(mu, sigma); 
    i = i + 1;
    }
    // Initialize if subj !=
    tn = n < (N_tot) ? n : n-1;
  
    v = subj[n] != subj[tn+1] ? exp(omega + u[subj[tn+1], 1] + SE[tn] * (beta_om_se + u[subj[tn+1], 8])) : v; // Initialize volatilities
    w = subj[n] != subj[tn+1] ? exp(omega + u[subj[tn+1], 1] + SE[tn] * (beta_om_se + u[subj[tn+1], 8])) : w;  // Initialize posterior variances
    m = subj[n] != subj[tn+1] ? 0 : m; // Initialize predictions

      }
}

