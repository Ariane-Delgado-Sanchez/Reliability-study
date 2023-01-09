#### Model comparison script ####

#Run these lines every time before running the rest of the parts of the script until mark. 
#Load library
library (rstan);
library (loo);
#Set stan options
rstan_options (auto_write = TRUE);
options (mc.cores = 4);#Use the same amount of cores as chains
set.seed (1); #Always the same one to have consistent results

directory="Y:/Reliability study/AnalysisandResults/"  #"CHANGE" to location in your PC

#Set model locations (change based on where the models are stored in your PC)
models1 <- paste(directory,"PrimaryAnalysis/model_1.stan", sep="");
models2 <- paste(directory,"PrimaryAnalysis/model_2.stan", sep="");
models3 <- paste(directory,"PrimaryAnalysis/model_3.stan", sep="");
models4 <- paste(directory,"PrimaryAnalysis/model_4.stan", sep="");


#Set data locations
Session1 <-paste(directory,"PrimaryAnalysis/Session1Data.csv", sep="");
Session2 <-paste(directory,"PrimaryAnalysis/Session2Data.csv", sep="");

#Format data for stan
data1<-data.frame(read.csv(Session1 , header = TRUE));
stan_data1 <- list(N = length(data1$R), subject = data1$S, S=length(unique(data1$S)),
                   q = data1$Q, std = data1$Std, X = data1$X, r = data1$R);
data2<-data.frame(read.csv(Session2 , header = TRUE));
stan_data2 <- list(N = length(data2$Q), subject = data2$S, S=length(unique(data2$S)),
                   q = data2$Q, std = data2$Std, X = data2$X, r = data2$R);

#######Descriptives and general tendencies. 
aggregate (data1$R, list(data1$Q), FUN=mean)
aggregate (data1$R, list(data1$Q), FUN=sd)
aggregate (data2$R, list(data2$Q), FUN=mean)           
aggregate (data2$R, list(data2$Q), FUN=sd) 

aggregate (data1$R, list(data1$X), FUN=mean)
aggregate (data1$R, list(data1$X), FUN=sd)
aggregate (data2$R, list(data2$X), FUN=mean)           
aggregate (data2$R, list(data2$X), FUN=sd) 


###########-------------Until here-----------------------------until here---------------------------------until here---------------#####

#Now it may be needed to run these bit by bit. Sometimes R crashes if you do them all together. So run it one by one and it will be saved. 
#Once finished all the modelling and saving, load all the saved modelling and it can be compared. 

#####Model 1, modelling and saving
model1 <-stan_model(models1);
#Model 1, session 1
fit1 <- stan(file=models1, data=stan_data1, iter=10000, chains = 4);

saveRDS(fit1, paste(directory,"PrimaryAnalysis/Session1results/fit1.rds", sep=""));
log_lik_1 <- extract_log_lik(fit1, merge_chains = FALSE);
saveRDS(log_lik_1, paste(directory,"PrimaryAnalysis/Session1results/log_lik_1.rds", sep=""));
r_eff_1 <- relative_eff(exp(log_lik_1));
loo_1 <- loo(log_lik_1, r_eff = r_eff_1);
saveRDS(loo_1, paste(directory,"PrimaryAnalysis/Session1results/loo_1.rds", sep=""));
fit_rep <- extract(fit1,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1012)
a<-colMeans(rep1)
sim1<-data.frame(a)
write.csv(sim1,paste(directory,"PrimaryAnalysis/Session1results/sim1.csv", sep=""))


#Model 1, session 2
fit1 <- stan(file=models1, data=stan_data2, iter=10000, chains = 4);

saveRDS(fit1, paste(directory,"PrimaryAnalysis/Session2results/fit1.rds", sep=""));
log_lik_1 <- extract_log_lik(fit1, merge_chains = FALSE);
saveRDS(log_lik_1, paste(directory,"PrimaryAnalysis/Session2results/log_lik_1.rds", sep=""));
r_eff_1 <- relative_eff(exp(log_lik_1));
loo_1 <- loo(log_lik_1, r_eff = r_eff_1);
saveRDS(loo_1, paste(directory,"PrimaryAnalysis/Session2results/loo_1.rds", sep=""));
fit_rep <- extract(fit1,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1027)
a<-colMeans(rep1)
sim1<-data.frame(a)
write.csv(sim1,paste(directory,"PrimaryAnalysis/Session2results/sim1.csv", sep=""))

#####Model 2, modelling and saving
model2 <-stan_model(models2);
#Model 2, session 1
fit2 <- stan(file=models2, data=stan_data1, iter=10000, chains = 4);

saveRDS(fit2, paste(directory,"PrimaryAnalysis/Session1results/fit2.rds", sep=""));
log_lik_2 <- extract_log_lik(fit2, merge_chains = FALSE);
saveRDS(log_lik_2, paste(directory,"PrimaryAnalysis/Session1results/log_lik_2.rds", sep=""));
r_eff_2 <- relative_eff(exp(log_lik_2));
loo_2 <- loo(log_lik_2, r_eff = r_eff_2);
saveRDS(loo_2, paste(directory,"PrimaryAnalysis/Session1results/loo_2.rds", sep=""));
fit_rep <- extract(fit2,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1012)
a<-colMeans(rep1)
sim2<-data.frame(a)
write.csv(sim2,paste(directory,"PrimaryAnalysis/Session1results/sim2.csv", sep=""))


#Model 2, session 2
fit2 <- stan(file=models2, data=stan_data2, iter=10000, chains = 4);
saveRDS(fit2, paste(directory,"PrimaryAnalysis/Session2results/fit2.rds", sep=""));
log_lik_2 <- extract_log_lik(fit2, merge_chains = FALSE);
saveRDS(log_lik_2, paste(directory,"PrimaryAnalysis/Session2results/log_lik_2.rds", sep=""));
r_eff_2 <- relative_eff(exp(log_lik_2));
loo_2 <- loo(log_lik_2, r_eff = r_eff_2);
saveRDS(loo_2, paste(directory,"PrimaryAnalysis/Session2results/loo_2.rds", sep=""));
fit_rep <- extract(fit2,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1027)
a<-colMeans(rep1)
sim2<-data.frame(a)
write.csv(sim2,paste(directory,"PrimaryAnalysis/Session2results/sim2.csv", sep=""))


#####Model 3, modelling and saving
model3 <-stan_model(models3);
#Model 3, session 1
fit3 <- stan(file=models3, data=stan_data1, iter=10000, chains = 4);

saveRDS(fit3, paste(directory,"PrimaryAnalysis/Session1results/fit3.rds", sep=""));
log_lik_3 <- extract_log_lik(fit3, merge_chains = FALSE);
saveRDS(log_lik_3, paste(directory,"PrimaryAnalysis/Session1results/log_lik_3.rds", sep=""));
r_eff_3 <- relative_eff(exp(log_lik_3));
loo_3 <- loo(log_lik_3, r_eff = r_eff_3);
saveRDS(loo_3, paste(directory,"PrimaryAnalysis/Session1results/loo_3.rds", sep=""));

fit_rep <- extract(fit3,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1012)
a<-colMeans(rep1)
sim3<-data.frame(a)
write.csv(sim3,paste(directory,"PrimaryAnalysis/Session1results/sim3.csv", sep=""))

#Model 3, session 2
fit3 <- stan(file=models3, data=stan_data2, iter=10000, chains = 4);
saveRDS(fit3, paste(directory,"PrimaryAnalysis/Session2results/fit3.rds", sep=""));
log_lik_3 <- extract_log_lik(fit3, merge_chains = FALSE);
saveRDS(log_lik_3, paste(directory,"PrimaryAnalysis/Session2results/log_lik_3.rds", sep=""));
r_eff_3 <- relative_eff(exp(log_lik_3));
loo_3 <- loo(log_lik_3, r_eff = r_eff_3);
saveRDS(loo_3, paste(directory,"PrimaryAnalysis/Session2results/loo_3.rds", sep=""));

fit_rep <- extract(fit3,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1027)
a<-colMeans(rep1)
sim3<-data.frame(a)
write.csv(sim3,paste(directory,"PrimaryAnalysis/Session2results/sim3.csv", sep=""))

#####Model 4, modelling and saving

model4 <-stan_model(models4);

#Model 4, session 1
fit4 <- stan(file=models4, data=stan_data1, iter=10000, chains = 4);
saveRDS(fit4, paste(directory,"PrimaryAnalysis/Session1results/fit4.rds", sep=""));
log_lik_4 <- extract_log_lik(fit4, merge_chains = FALSE);
saveRDS(log_lik_4, paste(directory,"PrimaryAnalysis/Session1results/log_lik_4.rds", sep=""));
r_eff_4 <- relative_eff(exp(log_lik_4));
loo_4 <- loo(log_lik_4, r_eff = r_eff_4);
saveRDS(loo_4, paste(directory,"PrimaryAnalysis/Session1results/loo_4.rds", sep=""));

fit_rep <- extract(fit4,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1012)
a<-colMeans(rep1)
sim4<-data.frame(a)
write.csv(sim4,paste(directory,"PrimaryAnalysis/Session1results/sim4.csv", sep=""))

#Model 4, session 2
fit4 <- stan(file=models4, data=stan_data2, iter=10000, chains = 4);
saveRDS(fit4, paste(directory,"PrimaryAnalysis/Session2results/fit4.rds", sep=""));
log_lik_4 <- extract_log_lik(fit4, merge_chains = FALSE);
saveRDS(log_lik_4, paste(directory,"PrimaryAnalysis/Session2results/log_lik_4.rds", sep=""));
r_eff_4 <- relative_eff(exp(log_lik_4));
loo_4 <- loo(log_lik_4, r_eff = r_eff_4);
saveRDS(loo_4, paste(directory,"PrimaryAnalysis/Session2results/loo_4.rds", sep=""));

fit_rep <- extract(fit4,pars = "r_rep")
rep1=matrix(fit_rep[[1]],nrow=20000,ncol=1027)
a<-colMeans(rep1)
sim4<-data.frame(a)
write.csv(sim4,paste(directory,"PrimaryAnalysis/Session2results/sim4.csv", sep=""))


#####-----------------------until here the modelling and saving--------------------------


##########Predictive performance test

loo_1<-readRDS(file = paste(directory,"PrimaryAnalysis/Session1results/loo_1.rds", sep=""));
loo_2<-readRDS(file = paste(directory,"PrimaryAnalysis/Session1results/loo_2.rds", sep=""));
loo_3<-readRDS(file = paste(directory,"PrimaryAnalysis/Session1results/loo_3.rds", sep=""));
loo_4<-readRDS(file = paste(directory,"PrimaryAnalysis/Session1results/loo_4.rds", sep=""));

comp1 <- loo_compare(loo_1, loo_2, loo_3, loo_4);


###########Comparison through loo for session 2
loo_1<-readRDS(file = paste(directory,"PrimaryAnalysis/Session2results/loo_1.rds", sep=""));
loo_2<-readRDS(file = paste(directory,"PrimaryAnalysis/Session2results/loo_2.rds", sep=""));
loo_3<-readRDS(file = paste(directory,"PrimaryAnalysis/Session2results/loo_3.rds", sep=""));
loo_4<-readRDS(file = paste(directory,"PrimaryAnalysis/Session2results/loo_4.rds", sep=""));

comp2 <- loo_compare(loo_1, loo_2, loo_3, loo_4);


#################Generative performance test
library (lme4)
library (lmerTest)
library(MuMIn)

sim1<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim1.csv", sep=""))
sim1<- sim1$a
sim2<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim2.csv", sep=""))
sim2<- sim2$a
sim3<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim3.csv", sep=""))
sim3<- sim3$a
sim4<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim4.csv", sep=""))
sim4<- sim4$a
real<-data1$R
S<-data1$S

GP<-data.frame (S, real, sim1, sim2, sim3, sim4)

mod1<-lmer(real~sim1 + (1 | S), data=GP)
mod2<-lmer(real~sim2  + (1 | S), data=GP)
mod3<-lmer(real~sim3  + (1 | S), data=GP)
mod4<-lmer(real~sim4  + (1 | S), data=GP)

r.squaredGLMM(mod1)
r.squaredGLMM(mod2)
r.squaredGLMM(mod3)
r.squaredGLMM(mod4)

fixef (mod1)
fixef (mod2)
fixef (mod3)
fixef (mod4)

confint (mod1)
confint (mod2)
confint (mod3)
confint (mod4)



sim12<- read.csv(paste(directory,"PrimaryAnalysis/Session2results/sim1.csv", sep=""))
sim12<- sim12$a
sim22<- read.csv(paste(directory,"PrimaryAnalysis/Session2results/sim2.csv", sep=""))
sim22<- sim22$a
sim32<- read.csv(paste(directory,"PrimaryAnalysis/Session2results/sim3.csv", sep=""))
sim32<- sim32$a
sim42<- read.csv(paste(directory,"PrimaryAnalysis/Session2results/sim4.csv", sep=""))
sim42<- sim42$a
real2<-data2$R
S2<-data2$S

GP2<-data.frame (S2, real2, sim12, sim22, sim32, sim42)

mod12<-lmer(real2~sim12  + (1 | S2), data=GP2)
mod22<-lmer(real2~sim22  + (1 | S2), data=GP2)
mod32<-lmer(real2~sim32  + (1 | S2), data=GP2)
mod42<-lmer(real2~sim42  + (1 | S2), data=GP2)

r.squaredGLMM(mod12)
r.squaredGLMM(mod22)
r.squaredGLMM(mod32)
r.squaredGLMM(mod42)


fixef (mod12)
fixef (mod22)
fixef (mod32)
fixef (mod42)

confint (mod12)
confint (mod22)
confint (mod32)
confint (mod42)
#################Reliability

fit4 <- readRDS(file = paste(directory,"PrimaryAnalysis/Session1results/fit4.rds", sep=""))

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

Parameters1<-data.frame(beta1, rho1, eta1, mu1, nu1)
write.csv(Parameters1,paste(directory,"PrimaryAnalysis/Session1results/Parameters1.csv", sep=""))


fit42 <- readRDS(file = paste(directory,"PrimaryAnalysis/Session2results/fit4.rds", sep=""))

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

Parameters2<-data.frame(beta2, rho2, eta2, mu2, nu2)
write.csv(Parameters2,paste(directory,"PrimaryAnalysis/Session2results/Parameters2.csv", sep=""))

a <- read.csv(paste(directory,"PrimaryAnalysis/Session1results/Parameters1.csv", sep=""))
b <- read.csv(paste(directory,"PrimaryAnalysis/Session2results/Parameters2.csv", sep=""))

Parameters<-data.frame(a,b)

write.csv(Parameters,paste(directory,"PrimaryAnalysis/Parameters.csv", sep=""))

P <- read.csv(paste(directory,"PrimaryAnalysis/Parameters.csv", sep=""))

beta<-data.frame(P$beta1, P$beta2)
rho<-data.frame(P$rho1, P$rho2)
eta<-data.frame(P$eta1, P$eta2)
mu<-data.frame(P$mu1, P$mu2)
nu<-data.frame(P$nu1, P$nu2)

library("irr")
ICCbeta <-icc(beta, model = "twoway",
    type = "agreement", unit = "single")

ICCrho <- icc(rho, model = "twoway",
    type = "agreement", unit = "single")

ICCeta <-icc(eta, model = "twoway",
    type = "agreement", unit = "single")

ICCmu <-icc(mu, model = "twoway",
    type = "agreement", unit = "single")

ICCnu <-icc(nu, model = "twoway",
    type = "agreement", unit = "single")



