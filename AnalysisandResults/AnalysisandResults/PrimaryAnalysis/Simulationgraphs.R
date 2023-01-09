library (ggplot2)
library (dplyr)

directory="Y:/Reliability study/AnalysisandResults/"  #"CHANGE" to location in your PC

#Load data
Session1 <-paste(directory,"PrimaryAnalysis/Session1Data.csv", sep="");
Session2 <-paste(directory,"PrimaryAnalysis/Session2Data.csv", sep="");
data1<-data.frame(read.csv(Session1 , header = TRUE));
data2<-data.frame(read.csv(Session2 , header = TRUE));
sim1<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim1.csv", sep=""))
Trial<-sim1$X
sim1<- sim1$a
sim2<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim2.csv", sep=""))
sim2<- sim2$a
sim3<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim3.csv", sep=""))
sim3<- sim3$a
sim4<- read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim4.csv", sep=""))
sim4<- sim4$a
real<-data1$R
Trial2<-Trial
Simulations<-data.frame(real, sim1, sim2, sim3, sim4)

Name1<-rep("Simulation", times = length (sim1), length.out = NA, each = 1)
Name2<-rep("Real", times = length (sim1), length.out = NA, each = 1)


df1<-data.frame (Trial, Name1, sim1)
df2<-data.frame (Trial2, Name2, real)
colnames(df1) <- c('Trial', 'Name', 'Type')
colnames(df2) <- c('Trial', 'Name', 'Type')
dfg1 <- rbind(df1, df2)

PLOT1 <- ggplot(dfg1, aes(x=Trial, y=Type, color=Name)) + 
  geom_point() + 
  scale_color_manual(values = c('#191919','#e5714f'))+
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 1 simulation vs real pain ratings",
    y = "Pain Ratings", x = "Trial")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))
 

df1<-data.frame (Trial, Name1, sim2)
df2<-data.frame (Trial2, Name2, real)
colnames(df1) <- c('Trial', 'Name', 'Type')
colnames(df2) <- c('Trial', 'Name', 'Type')
dfg2 <- rbind(df1, df2)

PLOT2 <- ggplot(dfg2, aes(x=Trial, y=Type, color=Name)) + 
  geom_point() + 
  scale_color_manual(values = c('#191919','#e5714f'))+
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 2 simulation vs real pain ratings",
       y = "Pain Ratings", x = "Trial")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))


df1<-data.frame (Trial, Name1, sim3)
df2<-data.frame (Trial2, Name2, real)
colnames(df1) <- c('Trial', 'Name', 'Type')
colnames(df2) <- c('Trial', 'Name', 'Type')
dfg3 <- rbind(df1, df2)

PLOT3 <- ggplot(dfg3, aes(x=Trial, y=Type, color=Name)) + 
  geom_point() + 
  scale_color_manual(values = c('#191919','#e5714f'))+
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 3 simulation vs real pain ratings",
       y = "Pain Ratings", x = "Trial")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))



df1<-data.frame (Trial, Name1, sim4)
df2<-data.frame (Trial2, Name2, real)
colnames(df1) <- c('Trial', 'Name', 'Type')
colnames(df2) <- c('Trial', 'Name', 'Type')
dfg4 <- rbind(df1, df2)

PLOT4 <- ggplot(dfg4, aes(x=Trial, y=Type, color=Name)) + 
  geom_point() + 
  scale_color_manual(values = c('#191919','#e5714f'))+
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 4 simulation vs real pain ratings",
       y = "Pain Ratings", x = "Trial")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))

PLOT12 <- ggplot(Simulations, aes(x=sim1, y=real)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = '#e5714f', se = FALSE)  +
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 1 simulation vs real pain ratings",
       y = "Real pain ratings", x = "Simulated pain ratings")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))

PLOT22 <- ggplot(Simulations, aes(x=sim2, y=real)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = '#e5714f', se = FALSE)  +
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 2 simulation vs real pain ratings",
       y = "Real pain ratings", x = "Simulated pain ratings")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))
PLOT32 <- ggplot(Simulations, aes(x=sim3, y=real)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = '#e5714f', se = FALSE)  +
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 3 simulation vs real pain ratings",
       y = "Real pain ratings", x = "Simulated pain ratings")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))
PLOT42 <- ggplot(Simulations, aes(x=sim4, y=real)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = '#e5714f', se = FALSE)  +
  theme_classic() +
  theme(text=element_text(size=12, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Model 4 simulation vs real pain ratings",
       y = "Real pain ratings", x = "Simulated pain ratings")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))

#Mean and CI graphs
#beta, rho, nu and mu are the matrices (with dim=iterations x n) containing the posterior samples for each parameter
library(bayestestR) #Load package to compute credible intervals (might need to install this first)
library(rstan)
fit4<-readRDS(file = paste(directory,"PrimaryAnalysis/Session1results/fit4.rds", sep=""));
beta0 <- extract(fit4,pars = "beta")
beta=matrix(beta0[[1]],nrow=20000,ncol=26)

rho0 <- extract(fit4,pars = "rho")
rho=matrix(rho0[[1]],nrow=20000,ncol=26)

etatr0 <- extract(fit4,pars = "etatr")
etatr=matrix(etatr0[[1]],nrow=20000,ncol=26)

mu0 <- extract(fit4,pars = "mu")
mu=matrix(mu0[[1]],nrow=20000,ncol=26)

nu0 <- extract(fit4,pars = "nu")
nu=matrix(nu0[[1]],nrow=20000,ncol=26)

#Initialise upper and lower bounds for credible intervals 
u=matrix(0,nrow=5,ncol=26) #n is the number of participants
l=matrix(0,nrow=5,ncol=26)
#Compute credible intervals
for (i in 1:26)
{
  ci_beta <- ci(beta[,i], method = "HDI") #High Density CI; choose ETI for equal tailed version
  ci_rho <- ci(rho[,i], method = "HDI")
  ci_nu <- ci(nu[,i], method = "HDI")
  ci_mu <- ci(mu[,i], method = "HDI")
  ci_etatr<- ci(etatr[,i], method = "HDI")
  l[,i]=c(ci_beta[[2]],ci_rho[[2]],ci_nu[[2]],ci_mu[[2]], 1-2*ci_etatr[[2]])
  u[,i]=c(ci_beta[[3]],ci_rho[[3]],ci_nu[[3]],ci_mu[[3]], 1-2*ci_etatr[[3]])
}

fit_beta <- extract(fit4,pars = "beta")
beta2=matrix(fit_beta[[1]],nrow=20000,ncol=26)
meanbeta<-colMeans(beta2)

fit_rho <- extract(fit4,pars = "rho")
rho2=matrix(fit_rho[[1]],nrow=20000,ncol=26)
meanrho<-colMeans(rho2)

fit_mu <- extract(fit4,pars = "mu")
mu2=matrix(fit_mu[[1]],nrow=20000,ncol=26)
meanmu<-colMeans(mu2)

fit_nu <- extract(fit4,pars = "nu")
nu2=matrix(fit_nu[[1]],nrow=20000,ncol=26)
meannu<-colMeans(nu2)

fit_eta <- extract(fit4,pars = "etatr")
eta_tra=matrix(fit_eta[[1]],nrow=20000,ncol=26)
eta_orig=1-2*eta_tra
meaneta<-colMeans(eta_orig)

#meanbeta,meanrho,meannu and meanmu are the vectors of posterior means for each parameter
library(ggplot2) #Load ggplot package for nicer graphics (might need installing first)
#Create data frame which includes the posterior means as well as the upper and lower bounds for the credible intervals
data=data.frame(seq(1,26,1),meanbeta,meanrho,meannu,meanmu,meaneta, t(l),t(u))
#Define names for each column in the dataframe
names(data)=c("Participant","mbeta","mrho","mnu","mmu","meta", "lbeta","lrho","lnu","lmu","leta", "ubeta","urho","unu","umu", "ueta")
#Plot posterior means and credible intervals for each parameter
MCI1<- ggplot(data,aes(x=Participant, y=mbeta)) + geom_point() + geom_errorbar(aes(ymin=lbeta, ymax=ubeta)) + labs(y = expression(beta^2))+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  #labs(title = "Parameter mean and Credible Intervals")+
  theme(axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"))+
  scale_x_continuous(breaks = round(seq(min(data$Participant), max(data$Participant), by = 1),1))



MCI2<-ggplot(data,aes(x=Participant, y=mrho)) + geom_point() + geom_errorbar(aes(ymin=lrho, ymax=urho)) + labs(y = expression(rho^2))+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  #labs(title = "Parameter mean and Confidence Intervals")+
  theme(axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"))+
  scale_x_continuous(breaks = round(seq(min(data$Participant), max(data$Participant), by = 1),1))



MCI3<-ggplot(data,aes(x=Participant, y=meta)) + geom_point() + geom_errorbar(aes(ymin=leta, ymax=ueta)) + labs(y = expression(eta))+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  #labs(title = "Parameter mean and Confidence Intervals")+
  theme(axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"))+
  scale_x_continuous(breaks = round(seq(min(data$Participant), max(data$Participant), by = 1),1))




MCI4<-ggplot(data,aes(x=Participant, y=mnu)) + geom_point() + geom_errorbar(aes(ymin=lnu, ymax=unu)) + labs(y = expression(nu^2))+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  #labs(title = "Parameter mean and Confidence Intervals")+
  theme(axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"))+
  scale_x_continuous(breaks = round(seq(min(data$Participant), max(data$Participant), by = 1),1))

MCI5<-ggplot(data,aes(x=Participant, y=mmu)) + geom_point() + geom_errorbar(aes(ymin=lmu, ymax=umu)) + labs(y = expression(mu))+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size=0.5),
        legend.box.margin = margin(0.8, 0.8, 0.8, 0.8),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  #labs(title = "Parameter mean and Confidence Intervals")+
  theme(axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"))+
  scale_x_continuous(breaks = round(seq(min(data$Participant), max(data$Participant), by = 1),1))


Session1 <-paste(directory,"PrimaryAnalysis/Session1Data.csv", sep="");
data1<-data.frame(read.csv(Session1 , header = TRUE));
sim1<-read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim1.csv", sep=""))
sim3<-read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim3.csv", sep=""))
sim4<-read.csv(paste(directory,"PrimaryAnalysis/Session1results/sim4.csv", sep=""))

Trial<-sim1$X
Trial2<-Trial
sim1<-sim1$a
sim3<-sim3$a
sim4<-sim4$a

real<-data1$R
P<-data1$S
P2<-P
Namesim1<-rep("Simulation 1", times = length (sim1), length.out = NA, each = 1)
Namesim3<-rep("Simulation 3", times = length (sim1), length.out = NA, each = 1)
Namesim4<-rep("Simulation 4", times = length (sim1), length.out = NA, each = 1)

Namereal<-rep("Real", times = length (sim1), length.out = NA, each = 1)


dfsim1<-data.frame (P, Trial, Namesim1, sim1)
dfsim3<-data.frame (P, Trial, Namesim3, sim3)
dfreal<-data.frame (P2, Trial2, Namereal, real)
colnames(dfsim1) <- c('Participant', 'Trial', 'Type', 'Rating')
colnames(dfsim3) <- c('Participant', 'Trial', 'Type', 'Rating')
colnames(dfreal) <- c('Participant', 'Trial', 'Type', 'Rating')

exgraphs1 <- rbind(dfsim1,  dfreal)
exgraphs3 <- rbind(dfsim3,  dfreal)


P3.1<-subset(exgraphs1, Participant == 3)
P3.3<-subset(exgraphs3, Participant == 3)


P6.1<-subset(exgraphs1, Participant == 6)
P6.3<-subset(exgraphs3, Participant == 6)


P3graph1<-ggplot(P3.1, aes(x=Rating, fill=Type)) +
  geom_density(alpha=0.4)+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="right",
        legend.title = element_blank(),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Participant 3", x = "Pain Ratings", y = "Density")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))+
  scale_fill_manual(values = c('#191919','#e5714f'))

P3graph3<-ggplot(P3.3, aes(x=Rating, fill=Type)) +
  geom_density(alpha=0.4)+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="right",
        legend.title = element_blank(),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Participant 3", x = "Pain Ratings", y = "Density")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))+
  scale_fill_manual(values = c('#191919','#35B55E'))

P6graph1<-ggplot(P6.1, aes(x=Rating, fill=Type)) +
  geom_density(alpha=0.4)+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="right",
        legend.title = element_blank(),
         axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Participant 6", x = "Pain Ratings", y = "Density")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))+
  scale_fill_manual(values = c('#191919','#e5714f'))

P6graph3<-ggplot(P6.3, aes(x=Rating, fill=Type)) +
  geom_density(alpha=0.4)+
  theme_classic() +
  theme(text=element_text(size=8, family="serif"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position="right",
        legend.title = element_blank(),
        axis.text = element_text(face="bold"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  labs(title = "Participant 6", x = "Pain Ratings", y = "Density")+
  theme(axis.title.x = element_text(size=10, face="bold", color = "black"),
        axis.title.y = element_text(size=10, face="bold", color = "black"))+
  scale_fill_manual(values = c('#191919','#35B55E'))





