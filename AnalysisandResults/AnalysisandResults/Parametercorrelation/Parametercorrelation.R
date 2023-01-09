#README: Running this script provides the reliability graphs. 
#In order to find the script sections that need changing to make the code work in your PC "CTRL"+"F" the word "CHANGE"

#Run these lines every time before running the rest of the parts of the script until mark. 
#Load library
library (rstan);
library (loo);
#Set stan options
rstan_options (auto_write = TRUE);
options (mc.cores = 4);#Use the same amount of cores as chains
set.seed (1); #Always the same one to have consistent results

directory="Y:/Reliability study/AnalysisandResults/"  #"CHANGE" to location in your PC



#Set model location (change based on where the models are stored in your PC)
models4 <- paste(directory,"Parametercorrelation/model_4.stan", sep = ""); 

#Set data locations
Session1 <-paste(directory,"Parametercorrelation/Session1Data.csv", sep=""); 
Session2 <-paste(directory,"/Parametercorrelation/Session2Data.csv", sep="");

#Format data for stan
data1<-data.frame(read.csv(Session1 , header = TRUE));
stan_data1 <- list(N = length(data1$R), subject = data1$S, S=length(unique(data1$S)),
                   q = data1$Q, std = data1$Std, X = data1$X, r = data1$R);
data2<-data.frame(read.csv(Session2 , header = TRUE));
stan_data2 <- list(N = length(data2$Q), subject = data2$S, S=length(unique(data2$S)),
                   q = data2$Q, std = data2$Std, X = data2$X, r = data2$R);



###########-------------Until here-----------------------------until here---------------------------------until here---------------#####

#Now it may be needed to run these bit by bit. Sometimes R crashes if you do them all together. So run it one by one and it will be saved. 
#Once finished all the modelling and saving, load all the saved modelling and it can be compared. 

#####Model 4, modelling and saving

model4 <-stan_model(models4);

#Model 4, session 1
fit4 <- stan(file=models4, data=stan_data1, iter=10000, chains = 4);
saveRDS(fit4, paste(directory,"Parametercorrelation/Session1results/fit4.rds", sep="")); 
log_lik_4 <- extract_log_lik(fit4, merge_chains = FALSE);
saveRDS(log_lik_4, paste(directory,"Parametercorrelation/Session1results/log_lik_4.rds", sep="")); 
r_eff_4 <- relative_eff(exp(log_lik_4));
loo_4 <- loo(log_lik_4, r_eff = r_eff_4);
saveRDS(loo_4, paste(directory,"Parametercorrelation/Session1results/loo_4.rds", sep="")); 

fit_rep <- extract(fit4,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=2024)
a<-colMeans(rep1)
sim4<-data.frame(a)
write.csv(sim4,paste(directory,"Parametercorrelation/Session1results/sim4.csv", sep=""));

#Model 4, session 2
fit4 <- stan(file=models4, data=stan_data2, iter=10000, chains = 4);
saveRDS(fit4, paste(directory,"Parametercorrelation/Session2results/fit4.rds", sep=""));
log_lik_4 <- extract_log_lik(fit4, merge_chains = FALSE);
saveRDS(log_lik_4, paste(directory,"Parametercorrelation/Session2results/log_lik_4.rds", sep=""));
r_eff_4 <- relative_eff(exp(log_lik_4));
loo_4 <- loo(log_lik_4, r_eff = r_eff_4);
saveRDS(loo_4, paste(directory,"Parametercorrelation/Session2results/loo_4.rds", sep=""));

fit_rep <- extract(fit4,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=2051)
a<-colMeans(rep1)
sim4<-data.frame(a)
write.csv(sim4,paste(directory,"Parametercorrelation/Session2results/sim4.csv", sep=""))


#####-----------------------until here the modelling and saving--------------------------

#################We extract parameters with full sample

fit4 <- readRDS(file = paste(directory,"Parametercorrelation/Session1results/fit4.rds", sep=""))

fit_beta <- extract(fit4,pars = "beta")
beta1=matrix(fit_beta[[1]],nrow=20000,ncol=26)
beta1<-colMeans(beta1)

fit_rho <- extract(fit4,pars = "rho")
rho1=matrix(fit_rho[[1]],nrow=20000,ncol=26)
rho1<-colMeans(rho1)

fit_mu <- extract(fit4,pars = "mu")
mu1=matrix(fit_mu[[1]],nrow=20000,ncol=26)
mu1<-colMeans(mu1)

fit_nu <- extract(fit4,pars = "nu")
nu1=matrix(fit_nu[[1]],nrow=20000,ncol=26)
nu1<-colMeans(nu1)

fit_eta <- extract(fit4,pars = "etatr")
eta_tra=matrix(fit_eta[[1]],nrow=20000,ncol=26)
eta_orig=2*eta_tra-1
eta1<-colMeans(eta_orig)

Fullsample1<-data.frame(beta1, rho1, eta1, mu1, nu1)
write.csv(Fullsample1,paste(directory,"Parametercorrelation/Session1results/Fullsample1.csv", sep=""))

fit42 <- readRDS(file = paste(directory,"Parametercorrelation/Session2results/fit4.rds", sep=""))

fit_beta <- extract(fit42,pars = "beta")
beta2=matrix(fit_beta[[1]],nrow=20000,ncol=26)
beta2<-colMeans(beta2)

fit_rho <- extract(fit42,pars = "rho")
rho2=matrix(fit_rho[[1]],nrow=20000,ncol=26)
rho2<-colMeans(rho2)

fit_mu <- extract(fit42,pars = "mu")
mu2=matrix(fit_mu[[1]],nrow=20000,ncol=26)
mu2<-colMeans(mu2)

fit_nu <- extract(fit42,pars = "nu")
nu2=matrix(fit_nu[[1]],nrow=20000,ncol=26)
nu2<-colMeans(nu2)

fit_eta <- extract(fit42,pars = "etatr")
eta_tra=matrix(fit_eta[[1]],nrow=20000,ncol=26)
eta_orig=2*eta_tra-1
eta2<-colMeans(eta_orig)

Fullsample2<-data.frame(beta2, rho2, eta2, mu2, nu2)
write.csv(Fullsample2,paste(directory,"Parametercorrelation/Session2results/Fullsample2.csv", sep=""))

######Now we extract the parameters with the reduced sample to compare
Reduced <- read.csv (paste(directory,"PrimaryAnalysis/Parameters.csv", sep=""))
Reduced1 <- data.frame (Reduced$beta1, Reduced$rho1, Reduced$eta1, Reduced$mu1, Reduced$nu1)
Reduced2 <- data.frame (Reduced$beta2, Reduced$rho2, Reduced$eta2, Reduced$mu2, Reduced$nu2)
