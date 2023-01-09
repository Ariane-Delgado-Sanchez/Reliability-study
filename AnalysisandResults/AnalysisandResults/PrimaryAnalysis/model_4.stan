// 
// Model 4 - Variable full Bayesian model
// ---------------------------------------------------------------------------------------------------------------


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
  
//Hyperparameter boundaries
  real<lower=0> beta_u; 
  real<lower=0> rho_u;
  real<lower=0> etatr_u;
  real mu_u;
  real<lower=0> nu_u;
  real<lower=0> beta_s;
  real<lower=0> rho_s;
  real<lower=0> etatr_s;
  real<lower=0> mu_s;
  real<lower=0> nu_s;

//Parameter boundaries  
  vector<lower=0>[S] beta; //this corresponds to beta^2 in the mathematical models
  vector<lower=0>[S] rho;  //this corresponds to rho^2 in the mathematical models 
  vector<lower=0, upper=1>[S] etatr;  //this corresponds to (eta+1)/2 in the mathematical models
  vector[S] mu;
  vector<lower=0>[S] nu; //this corresponds to nu^2 in the mathematical models	
                
}

transformed parameters {
  
//We assign the appropriate parameters to each trial
  vector[N] beta_n;
  vector[N] rho_n;
  vector<lower=0, upper=2>[N] eta_n; //this corresponds to eta+1 in the mathematical models
  vector[N] etatr_n; //this corresponds to (eta+1)/2 in the mathematical models
  vector[N] mu_n;
  vector[N] nu_n;
  vector[N] A;
  vector[N] B;
  vector[N] mean_two;
  vector[N] variance_two; 

 for (n in 1:N){
    int s = subject [n];
    beta_n[n] = beta[s];
    rho_n[n] = rho[s];
    etatr_n[n]= etatr[s];
    eta_n[n] = 2*etatr[s];  
    mu_n[n] = mu[s];
    nu_n[n] = nu[s];
    A[n]= 1/beta_n[n] + 1/rho_n[n] + 1/nu_n[n];
    B[n]= X[n]/beta_n[n] + (q[n]+ (eta_n[n]-1)*std[n])/rho_n[n] + mu_n[n]/nu_n[n];
    mean_two[n]=  B[n]/A[n]; 
    variance_two[n] = 1/A[n];
       }
}

model {

   target += gamma_lpdf(beta | beta_u, beta_s);
   target += gamma_lpdf(rho | rho_u, rho_s); 
   target += beta_lpdf(etatr | etatr_u, etatr_s);
   target += normal_lpdf(mu | mu_u, mu_s);
   target += gamma_lpdf(nu | nu_u, nu_s);
		 
    
for (n in 1:N){
    target += normal_lpdf(r[n] | X[n], sqrt(beta_n[n]));
    target += normal_lpdf(r[n] | q[n] + (eta_n[n]-1)*std[n], sqrt(rho_n[n]));
    target += normal_lpdf(r[n] |mu_n[n], sqrt(nu_n[n]));
    }
   
}

generated quantities {
     vector [N] log_lik;
     real r_rep[N];
    for (n in 1:N){
       r_rep[n] = normal_rng(mean_two[n], sqrt(variance_two[n]));
	}
     for (n in 1:N){
       log_lik[n] = normal_lpdf(r[n] | X[n], sqrt(beta_n[n])) + normal_lpdf(r[n] | q[n] + (eta_n[n]-1)*std[n], sqrt(rho_n[n])) + normal_lpdf(r[n] |mu_n[n], sqrt(nu_n[n]));
         }   
}

