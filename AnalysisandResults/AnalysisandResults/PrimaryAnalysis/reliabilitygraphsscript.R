
#README: Running this script provides the reliability graphs. 
#In order to find the script sections that need changing to make the code work in your PC "CTRL"+"F" the word "CHANGE"


data<-read.csv("Y:/Reliability study/AnalysisandResults/PrimaryAnalysis/Parametersgraphs.csv") #CHANGE to the directory in which you have the Parametersgraph.csv file

library (ggplot2)


#Beta points
bg<-ggplot (data = data, aes(Time, beta, color=Participant.number)) +
  geom_point()+
  geom_line(aes(group=Participant))  + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) + 
  labs(y=expression(beta^2), x = "")+
  theme(text=element_text(size=12,  family="serif", color = "black"))+
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))+
  theme (axis.title.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
  axis.text.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
  axis.title.y = element_text(size = 12, family="serif", color = "black", face = "bold"))+
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))
  

#Rho points
rg<-ggplot (data = data, aes(Time, rho, color=Participant.number)) +
  geom_point()+
  geom_line(aes(group=Participant))  + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) + 
  labs(y=expression(rho^2), x = "")+
  theme(text=element_text(size=12,  family="serif", color = "black"))+
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))+
  theme (axis.title.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.text.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.title.y = element_text(size = 12, family="serif", color = "black", face = "bold"))+
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))
#Eta points
eg<-ggplot (data = data, aes(Time, eta, color=Participant.number)) +
  geom_point()+
  geom_line(aes(group=Participant))  + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) + 
  labs(y=expression(eta), x = "")+
  theme(text=element_text(size=12,  family="serif", color = "black"))+
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))+
  theme (axis.title.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.text.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.title.y = element_text(size = 12, family="serif", color = "black", face = "bold"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))
#Mu points
mg<-ggplot (data = data, aes(Time, mu, color=Participant.number)) +
  geom_point()+
  geom_line(aes(group=Participant))  + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) + 
  labs(y=expression(mu), x = "")+
  theme(text=element_text(size=12,  family="serif", color = "black"))+
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))+
  theme (axis.title.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.text.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.title.y = element_text(size = 12, family="serif", color = "black", face = "bold"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))

#Nu points
ng<-ggplot (data = data, aes(Time, nu, color=Participant.number)) +
  geom_point()+
  geom_line(aes(group=Participant))  + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) + 
  labs(y=expression(nu^2), x = "")+
  theme(text=element_text(size=12,  family="serif", color = "black"))+
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))+
  theme (axis.title.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.text.x = element_text(size = 12, family="serif", color = "black", face = "bold"),
         axis.title.y = element_text(size = 12, family="serif", color = "black", face = "bold"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))