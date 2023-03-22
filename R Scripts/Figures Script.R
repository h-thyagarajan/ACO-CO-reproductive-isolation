####### Figures #########

#load required packages 
library(ggplot2)
library(dplyr)
library(gridExtra)
require(grid)

##################################################################################################

######mate choice composite plot- Figure 1 in manuscript#########

fdat<-read.csv(file.choose()) #choose "female choice- proportions and averages.csv"

head(fdat)
summary(fdat)
names(fdat)

#boxplot of female choice (overall A/C)

population.plot.fem<-ggplot(fdat, aes(y = proportion, x = female.pop, fill=type)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Proportion of matings", limits=c(0,1))+
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"), 
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating Type") +
  annotate(geom="text", x=1.1, y= 0.65, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=2.1, y= 0.69, label= paste("*"), 
           size= 7, colour="black")+
  ggtitle("(a)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,0,0,0),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

#boxplot of male choice (overall A/C)

mdat<-read.csv(file.choose()) #choose "male choice- proportions and averages.csv"

head(mdat)
summary(mdat)
names(mdat)

# making boxplot for A & C male population versus male choice 

population.plot.mal<- ggplot(mdat, aes(y = proportion, x = male.pop, fill=type)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Male population") +
  scale_y_continuous(name= "Proportion of matings", limits=c(0,1))+
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"), 
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating Type") +
  annotate(geom="text", x=1.05, y= 0.725, label= paste("*"), 
           size= 7, colour="black") +
  ggtitle("(b)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,10,0,10),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

#boxplot of group mate choice (overall A/C)

gdat<-read.csv(file.choose()) #choose "group choice- proportions and averages.csv"

head(gdat)
summary(gdat)
names(gdat)

# making the plot for A & C populations versus female choice- in a group setting 

population.plot.group<- ggplot(gdat) + 
  geom_boxplot(aes(x=Female.pop, y=proportion.mated, fill=type)) +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Proportion of matings", limits= c(0,1))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"), 
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating Type") +
  annotate(geom="text", x=1.0, y= 0.85, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=2., y= 0.8, label= paste("*"), 
           size= 7, colour="black") +
  ggtitle("(c)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,20,0,0),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

#extracting legend from one of the plots 

legend<-ggplot(gdat) + 
  geom_boxplot(aes(x=Female.pop, y=proportion.mated, fill=type)) +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Proportion of matings", limits= c(0,1))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"), 
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating Type") +
  annotate(geom="text", x=1.0, y= 0.85, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=2., y= 0.8, label= paste("*"), 
           size= 7, colour="black") +
  ggtitle("(c)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "top",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,0,0,0),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(legend)

###composite plot###

lay= rbind(c(1,1,1), c(2,3,4))
grid.arrange(legend, population.plot.fem, population.plot.mal, population.plot.group, 
             bottom=textGrob("Population", gp=gpar(fontsize=15, fontfamily= "Times New Roman"), hjust=0.4, vjust= 0.5), padding=unit(1, "cm"), heights= c(0.5,1.5), 
             left=textGrob("Proportion of Matings", gp=gpar(fontsize=15, fontfamily= "Times New Roman"), rot= 90, hjust=0.6, vjust= 0.1),layout_matrix=lay)

##################################################################################################

######Mating latency and duration- Figure 2 in manuscript#########

#load female and male data and filtering out NAs
fmatechoicedat <- read.csv(file.choose()) #select female choice compiled data
mmatechoicedat <- read.csv(file.choose()) #select male choice compiled data 

mmatechoicedat <- mmatechoicedat %>% filter(mating.latency!="NA")
fmatechoicedat <- fmatechoicedat %>% filter(mating.latency!="NA")


#Female latency

latency.plot.fem<-ggplot(fmatechoicedat, aes(y = log(mating.latency+1), x = female.pop, fill=mated.male.pop)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "log(Mating Latency (minutes) +1)", limits=c(0,6))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("A", "C"),
                    name="Mate Population")+
  ggtitle("(a)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(linewidth=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=12, colour="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 10, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,0,10,5),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))+
  
  annotate(geom="text", x=0.81, y= 5.5, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.19, y= 5.5, label= paste("b"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.81, y= 5.5, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=2.19, y= 5.5, label= paste("b"), 
           size= 5, colour="black", family= "Times New Roman")

#female duration

duration.plot.fem<-ggplot(fmatechoicedat, aes(y = mating.duration, x = female.pop, fill=mated.male.pop)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Mating Duration (minutes)", limits=c(0,60))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("A", "C"),
                    name="Mate Population")+
  ggtitle("(c)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black"),
    axis.title.x = element_text(size=15, colour="black",
                                margin = margin(t = 5, r = 5, b = 0, l = 0)),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 5)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,0,0,0),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))+
  
  annotate(geom="text", x=0.81, y= 54, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.19, y= 54, label= paste("b"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.81, y= 54, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=2.19, y= 54, label= paste("c"), 
           size= 5, colour="black", family= "Times New Roman")


#male latency 

latency.plot.mal<-ggplot(mmatechoicedat, aes(y = log(mating.latency+1), x = Mated.male.pop, fill=Mated.female.pop)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Male population") +
  scale_y_continuous(name= "log(Mating Latency (minutes) +1)", limits=c(0,6))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("A", "C"),
                    name="Mate Population")+
  ggtitle("(b)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,2,10,20),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))+
  
  annotate(geom="text", x=0.81, y= 5.5, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.19, y= 5.5, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.81, y= 5.5, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=2.19, y= 5.5, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")

#male duration 

duration.plot.mal<-ggplot(mmatechoicedat, aes(y = mating.duration, x = Mated.male.pop, fill=Mated.female.pop)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Male Population") +
  scale_y_continuous(name= "Mating Duration (minutes)", limits=c(0,60))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("A", "C"),
                    name="Mate Population")+
  ggtitle("(d)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size=15, colour="black",
                                margin = margin(t = 5, r = 5, b = 0, l = 0)),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "NONE",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,2,0,20),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))+
  
  annotate(geom="text", x=0.81, y= 54, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.19, y= 54, label= paste("b"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=1.81, y= 54, label= paste("a"), 
           size= 5, colour="black", family= "Times New Roman")+
  annotate(geom="text", x=2.19, y= 54, label= paste("b"), 
           size= 5, colour="black", family= "Times New Roman")


#extracting legend
legend<- ggplot(mmatechoicedat, aes(y = mating.duration, x = Mated.male.pop, fill=Mated.female.pop)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Male Population") +
  scale_y_continuous(name= "Mating Duration (minutes)", limits=c(0,55))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("A", "C"),
                    name="Mated Population")+
  ggtitle("(d)")+ theme(
    text= element_text(family= "Times New Roman"),
    axis.ticks = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black"),
    axis.title.x = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "top",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.margin=margin(0,0,0,0),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(legend)

#making the composite plot

lay= rbind(c(1,1), c(2,3), c(4,5))

grid.arrange(legend, latency.plot.fem, latency.plot.mal, duration.plot.fem, duration.plot.mal, layout_matrix=lay, heights= c(0.2,1,1))


##################################################################################################

######hatch time, viability, dev. time, body weights composite plot- Figure 3 in manuscript#######

###### Hatchability and viability #########

hybrid.dat<-read.csv(file.choose()) #choose "Hybrid viability data.csv"

head(hybrid.dat)
summary(hybrid.dat)
names(hybrid.dat)

# making plot for cross identity versus hatchability 

hatch<- ggplot(hybrid.dat, aes(y = Hatchability, x = Generation)) + 
  geom_boxplot(fill="sienna 1") +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Hatchability", limits= c(0, 1.06), breaks=seq(0,1, by=0.25)) +
  
  annotate(geom="text", x=1.0, y= 1.06, label= paste("a"), 
           size= 5, colour="sienna 1", family="Times New Roman")+
  annotate(geom="text", x=2.0, y= 1.06, label= paste("ab"), 
           size= 5, colour="sienna 1", family="Times New Roman")+
  annotate(geom="text", x=3.0, y= 1.06, label= paste("ab"), 
           size= 5, colour="sienna 1", family="Times New Roman")+
  annotate(geom="text", x=4.0, y= 1.06, label= paste("a"), 
           size= 5, colour="sienna 1", family="Times New Roman")+
  annotate(geom="text", x=5.0, y= 1.06, label= paste("ab"), 
           size= 5, colour="sienna 1", family="Times New Roman")+
  annotate(geom="text", x=6.0, y= 1.06, label= paste("b"), 
           size= 5, colour="sienna 1", family="Times New Roman") + ggtitle("(a)")+
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(linewidth=0.3, colour="black"),
    axis.ticks.x = element_line(linewidth=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= c(0.8,0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

# making plot for cross identity versus larvae to adult viability

viability<- ggplot(hybrid.dat, aes(y = LarvaetoAdultCorrected, x = Generation)) + 
  geom_boxplot(fill="#B2DFDB") +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Larvae to Adult Viability", limits=c(0, 1.06)) +
  
  annotate(geom="text", x=1.0, y= 1.06, label= paste("a"), 
           size= 5, colour="cyan4", family="Times New Roman")+
  annotate(geom="text", x=2.0, y= 1.06, label= paste("a"), 
           size= 5, colour="cyan4", family="Times New Roman")+
  annotate(geom="text", x=3.0, y= 1.06, label= paste("b"), 
           size= 5, colour="cyan4", family="Times New Roman")+
  annotate(geom="text", x=4.0, y= 1.06, label= paste("a"), 
           size= 5, colour="cyan4", family="Times New Roman")+
  annotate(geom="text", x=5.0, y= 1.06, label= paste("b"), 
           size= 5, colour="cyan4", family="Times New Roman")+
  annotate(geom="text", x=6.0, y= 1.06, label= paste("b"), 
           size= 5, colour="cyan4", family="Times New Roman") + ggtitle("(b)") +
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(linewidth=0.3, colour="black"),
    axis.ticks.x = element_line(linewidth=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= c(0.8,0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))


###### Development time and size #########

# making plot for cross identity versus development time 
## uses same data as used previously "hybrid.dat"

devtime<-ggplot(hybrid.dat, aes(y = Av.Development.Time, x = Generation)) + 
  geom_boxplot(fill="sienna 1") +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Average Development Time (hours)", limits= c(170, 230), breaks=seq(170,230, by=10)) +
  
  annotate(geom="text", x=1.0, y= 230, label= paste("a"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=2.0, y= 230, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=3.0, y= 230, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=4.0, y= 230, label= paste("c"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=5.0, y= 230, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=6.0, y= 230, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman") + ggtitle("(d)") +
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(linewidth=0.3, colour="black"),
    axis.ticks.x = element_line(linewidth=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= c(0.8,0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

# making plot for cross identity versus individual size 
## need to load size data to make this plot 

size.dat<-read.csv(file.choose()) #choose "Hybrid size data.csv"

head(size.dat)
summary(size.dat)
names(size.dat)

size<-ggplot(size.dat, aes(y = Individual.Weight, x = Generation)) + 
  geom_boxplot(fill="#B2DFDB") +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Individual Size (grams)", limits= c(0.18, 0.38), breaks=seq(0.20,0.35, by=0.05)) +
  
  annotate(geom="text", x=1.0, y= 0.38, label= paste("a"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=2.0, y= 0.38, label= paste("b"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=3.0, y= 0.38, label= paste("c"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=4.0, y= 0.38, label= paste("d"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=5.0, y= 0.38, label= paste("bc"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=6.0, y= 0.38, label= paste("bc"), 
           size= 5, colour="cyan4", family= "Times New Roman") + ggtitle("(c)") +
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(linewidth=0.3, colour="black"),
    axis.ticks.x = element_line(linewidth=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= c(0.8,0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

###composite plot###

grid.arrange(hatch, viability, size, devtime, 
             bottom=textGrob("Cross Identity", gp=gpar(fontsize=15, fontfamily= "Times New Roman"), hjust=0.3, vjust= 0.5), padding=unit(1, "cm"), ncol=2)

##################################################################################################

######Postzygotic hybrid fertility plot- Figure 4 in manuscript#########

fert.dat<-read.csv(file.choose()) #choose "PZRF data proportions.csv"

head(fert.dat)
summary(fert.dat)
names(fert.dat)

ggplot(fert.dat, aes(y = average.proportion.red, x = Cross.Identity, fill=sex)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross Identity", labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Average Proportion of Red Eyed Offspring", limits= c(0, 0.3), breaks=seq(0,0.3, by=0.05))+
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"), 
                    labels= c("female", "male"),
                    name="Sex") +
  geom_hline(yintercept=0.2, colour="dimgrey", size=0.5, linetype= "dashed")+
  
  annotate(geom="text", x=0.81, y= 0.18, label= paste("a"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=1.81, y= 0.26, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=2.81, y= 0.21, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=3.81, y= 0.281, label= paste("c"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=4.81, y= 0.26, label= paste("c"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  annotate(geom="text", x=5.81, y= 0.21, label= paste("b"), 
           size= 5, colour="sienna 1", family= "Times New Roman")+
  
  annotate(geom="text", x=1.18, y= 0.08, label= paste("a"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=2.18, y= 0.16, label= paste("ab"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=3.18, y= 0.118, label= paste("ab"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=4.18, y= 0.18, label= paste("b"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=5.18, y= 0.23, label= paste("b"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  annotate(geom="text", x=6.18, y= 0.13, label= paste("ab"), 
           size= 5, colour="cyan4", family= "Times New Roman")+
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(linewidth=0.3, colour="black"),
    axis.ticks.x = element_line(linewidth=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_text(size=15, colour="black",
                                margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size=15, colour="black",
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "top",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

###########################################################################
#####Supplementary Material 
##########################################################################

#####Figure S1-Female mate choice by replicate#######

female<-ggplot(fdat, aes(y = proportion, x = female.id, fill=type)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Proportion of Matings", limits=c(0,1)) +
  annotate(geom="text", x=3.1, y= 0.647, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=4.1, y= 0.707, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=5.1, y= 0.677, label= paste("*"), 
           size= 7, colour="black")+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating Type")+ ggtitle("(a)")+
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(size=0.3, colour="black"),
    axis.ticks.x = element_line(size=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "none",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))


#####Figure S2-Male mate choice by replicate#######

male<-ggplot(mdat, aes(y = proportion, x = male.id, fill=type)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Male population") +
  scale_y_continuous(name= "Proportion of Matings", limits=c(0,1))+
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"), 
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating Type") +
  annotate(geom="text", x=1.1, y= 0.738, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=2.1, y= 0.637, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=3.1, y= 0.67, label= paste("*"), 
           size= 7, colour="black")+ ggtitle("(b)")+
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(size=0.3, colour="black"),
    axis.ticks.x = element_line(size=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "none",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))


#####Figure S3-Group mate choice by replicate#######

group<-ggplot(gdat) + 
  geom_boxplot(aes(x=Female.ID, y=proportion.mated, fill=type)) +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Proportion of Matings", limits=c(0,1))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating type") +
  annotate(geom="text", x=2, y= 0.85, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=3, y= 0.85, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=6.1, y= 0.79, label= paste("*"), 
           size= 7, colour="black")+ ggtitle("(c)") +
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(size=0.3, colour="black"),
    axis.ticks.x = element_line(size=0.3, colour="black"),
    axis.text.x = element_text(size=12, colour="black"),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "none",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

#extracting legend
legend<- ggplot(gdat) + 
  geom_boxplot(aes(x=Female.ID, y=proportion.mated, fill=type)) +
  scale_x_discrete(name= "Female population") +
  scale_y_continuous(name= "Proportion of Matings", limits=c(0,1))+ 
  scale_fill_manual(values= c("sienna 1", "#B2DFDB"),
                    labels= c("Homotypic", "Heterotypic"),
                    name="Mating type") +
  annotate(geom="text", x=2, y= 0.85, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=3, y= 0.85, label= paste("*"), 
           size= 7, colour="black")+
  annotate(geom="text", x=6.1, y= 0.79, label= paste("*"), 
           size= 7, colour="black") +
  theme(
    text =element_text(family="Times New Roman"),
    axis.ticks.y = element_line(size=0.3, colour="black"),
    axis.ticks.x = element_line(size=0.3, colour="black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=12, colour="black", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill="white", colour="black"), 
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position= "top",
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title=element_text(size=12, colour="black", face="bold"),
    legend.text = element_text(size=12, colour="black"),
    legend.title=element_text(size=12, colour="black"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(legend)

###composite plot###

lay= rbind(c(1), c(2), c(3), c(4))

grid.arrange(legend, female, male, group,
             bottom=textGrob("Population", gp=gpar(fontsize=15, fontfamily= "Times New Roman"), hjust=0.4, vjust= 0.5), padding=unit(1, "cm"), heights=c(0.1,1,1,1), 
             left=textGrob("Proportion of Matings", gp=gpar(fontsize=15, fontfamily= "Times New Roman"), rot= 90, hjust=0.6, vjust= 0.1),layout_matrix=lay)

