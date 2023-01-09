// Model 1 - Only stimulation model
// ---------------------------------------------------------------------------------------------------------------

//Data uploaded through R script with this function. stan_data <- list(N = length(data$Q), subject = data$S, S=length(unique(data$S)),
//                   q = data$Q, std = data$Std, X = data$X, r = data$R);


data {
  int<lower=0> N;		    //Total number of trials
  int subject [N];                  //Sequence with subject IDs of the length of the number of trials (1, 1, 1, 1 (...) 28, 28, 28, 28
  int <lower=0> S;                  //Number of subjects 
  vector[N] q;                      //Mean of Presented Que
  vector[N] std;                    //Standard Deviation of Presented Que
  vector[N] X;                      //Administerd Stimulus
  vector<lower=0, upper=100>[N] r;  //Subject Rating
} 


parameters {
  
//Hyperparameter mean boundaries 
  real<lower=0> beta_u; 

//Hyperparameter standard deviation boundaries 
  real<lower=0> beta_s;

//Parameters  
  vector<lower=0>[S] beta;  
}

transformed parameters {
  
//We assign the appropriate parameters to each trial  
  vector[N] beta_n;
  vector[N] mean_two;
  vector[N] variance_two;
  for (n in 1:N){
    int s = subject [n];
    beta_n[n] = beta[s];
    mean_two [n]= X[n]; ;
    variance_two [n]= beta_n[n];
    }
}

model {
    target += gamma_lpdf(beta | beta_u, beta_s);
    
for (n in 1:N){
    target += normal_lpdf(r[n] | X[n], sqrt(beta_n[n]));
    }

}

generated quantities {
     vector [N] log_lik;
      real r_rep[N];
     for (n in 1:N){
       r_rep[n] = normal_rng(mean_two[n], sqrt(variance_two[n]));
	}
     for (n in 1:N){
       log_lik[n]= normal_lpdf(r[n] | X[n], sqrt(beta_n[n]));
	}
	
     }
