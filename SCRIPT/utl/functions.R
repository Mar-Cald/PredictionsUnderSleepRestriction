vkf_bin = function(u,omega,v0, sigma_v){
  
  w0 = omega; 
  # TN: number of trials
  TN = length(u) # Get nrows, size of outcomes
  
  m = 0 #prediction, start with 0.5
  w = w0 #posterior variance
  v = v0 #volatility
  
  predictions = matrix(NA, nrow = TN, ncol = 1)
  learning_rate = matrix(NA, nrow = TN, ncol = 1)
  volatility = matrix(NA, nrow = TN, ncol = 1)
  prediction_error = matrix(NA, nrow = TN, ncol = 1)
  volatility_error = matrix(NA, nrow = TN, ncol = 1)
  variance = matrix(NA, nrow = TN, ncol = 1)
  gain =  matrix(NA, nrow = TN, ncol = 1)
  
  sigmoid = function(x) {
    1 / (1 + exp(-x))
  }
  
  t = 1; 
  for (t in 1:TN) { 
    o = u[t];
    predictions[t] = m;    
    volatility[t] = v;  
    variance[t] = w;
    
    mpre        = m;
    wpre        = w;
    
    delta_m     = o - sigmoid(m);
    k           = (w+v)/(w+v+omega); 
    alpha       = sqrt(w+v);        
    
    m           = m + alpha*delta_m;  
    w           = (1-k)*(w+v);   
    
    wcov        = (1-k)*wpre;                                      # Eq 18
    delta_v     = (m-mpre)^2 + w + wpre - 2*wcov - v;  
    
    v           = v + sigma_v*delta_v;   
    
    learning_rate[t] = alpha;
    prediction_error[t] = delta_m;
    volatility_error[t] = delta_v;
    gain[t] = k;
  }
  
  signals <- list(
    predictions = predictions,
    volatility = volatility,
    learning_rate = learning_rate,
    prediction_error = prediction_error,
    variance = variance,
    gain = gain
    
  )
  return(signals)
}


sigmoid = function(x) {
  1 / (1 + exp(-x))
}



