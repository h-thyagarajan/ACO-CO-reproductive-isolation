######### Mixed Model Script ############

library(dplyr)
library(ggplot2)
library(ggfortify)
library(MuMIn)
library(gridExtra)
library(car)
library(agricolae)
library(lme4)
library(emmeans)

#######################################################################
#####load hybrid viability data#########

dat<-read.csv(file.choose()) #choose "Hybrid viability data.csv"
dat$Parental.Replicate<- as.factor(dat$Parental.Replicate)

head(dat)
summary(dat)
names(dat)

################### development time ######################

##step 1: visualize the data 

#by replicate
ggplot(dat, aes(y = Av.Development.Time, x = Generation, fill= Parental.Replicate)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Average development time (hours)")

#overall population
ggplot(dat, aes(y = Av.Development.Time, x = Generation)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Average development time (hours)")

##step 2: fitting a linear model with cross identity as fixed effect and parental replicate as random effect

dev.model<- lmer(Av.Development.Time~Generation+ (1|Parental.Replicate), data=dat)

library(lmerTest) #needs to be run prior to anova() to get p-values

anova(dev.model) #significant fixed effect 
ranova(dev.model) #no significant random effect

##step 3: testing model meets assumptions

plot(resid(dev.model))
hist(resid(dev.model))

###assumption are met 

##step 4: Using emmeans() to determine pairwise differences 

emmeans(dev.model, pairwise~Generation)

##step 5: determine averages and sd 

dev.dat<-dat %>% group_by(Generation) %>% summarize(dev.time.average=mean(Av.Development.Time), dev.time.sd=sd(Av.Development.Time))
dev.dat

################### hatchability ######################

##step 1: visualize the data

#by replicate
ggplot(dat, aes(y = Hatchability, x = Generation, fill= Parental.Replicate)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Hatchability")

#by population
ggplot(dat, aes(y = Hatchability, x = Generation)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Hatchability")

##step 2: fitting a generalized mixed model (binomial), with cross identity as fixed effect and parental replicate as random effect

hatch.model<- glmer(Viable.Eggs/Total.Eggs~Generation+ (1|Parental.Replicate), data=dat, weights= Total.Eggs, family=binomial)

Anova(hatch.model) #significant fixed effect

#repeate with lme to determine denominator df
library(nlme)
model<-lme(Viable.Eggs/Total.Eggs~Generation, random=~1|Parental.Replicate, data=dat)
anova(model)

#ranova() doesn't work for glmer() so fitting another model without random effect and comparing 

hatch.model.r<- glm(Viable.Eggs/Total.Eggs~Generation, data=dat, weights= Total.Eggs, family=binomial)
anova(hatch.model, hatch.model.r) #no significant random effect 


##step 3: Checking assumptions 

plot(resid(hatch.model))
hist(resid(hatch.model))
## assumptions met, model fits well  

##step 4: Using emmeans() 

emmeans(hatch.model, pairwise~Generation)

##step 5: determine averages and sd

dat.hatch<-dat %>% group_by(Generation) %>% summarize(hatch.average=mean(Hatchability), hatch.sd=sd(Hatchability))
dat.hatch

################### Larvae to adult viability ######################

##step 1: visualize the data


#by replicate 
ggplot(dat, aes(y = LarvaetoAdultCorrected, x = Generation, fill= Parental.Replicate)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Egg to Adult Viability")

#by population

ggplot(dat, aes(y = LarvaetoAdultCorrected, x = Generation)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Egg to Adult Viability")

##step 2: fitting generalized linear mixed model 

viab.model<- glmer(Total.Adults/Viable.Eggs.corrected~Generation+ (1|Parental.Replicate), data=dat, weights= Viable.Eggs.corrected, family=binomial)

Anova(viab.model) #significant fixed effect

#repeate with lme to determine denominator df
library(nlme)
model<-lme(Total.Adults/Viable.Eggs.corrected~Generation, random=~1|Parental.Replicate, data=dat)
anova(model)

#ranova() doesn't work for glmer() so fitting another model without random effect and comparing 

viab.model.r<- glm(Total.Adults/Viable.Eggs.corrected~Generation, data=dat, weights= Viable.Eggs.corrected, family=binomial)
anova(viab.model, viab.model.r) #significant random effect 

##step 3: testing model meets assumptions

plot(resid(viab.model))
hist(resid(viab.model))

##step 4: Using emmeans() to determine pairwise differences 

emmeans(viab.model, pairwise~Generation)

##step 5: determine averages and sd

dat.viab<-dat %>% group_by(Generation) %>% summarize(viab.average=mean(LarvaetoAdultCorrected), viab.sd=sd(LarvaetoAdultCorrected))
dat.viab

################### hybrid body size ######################

datw<-read.csv(file.choose()) #choose "Hybrid size data.csv"

datw$Parental.Replicate<- as.factor(datw$Parental.Replicate)

head(datw)
summary(datw)
names(datw)

##step 1: visualize the data

#by replicate
ggplot(datw, aes(y = Individual.Weight, x = Generation, fill= Parental.Replicate)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Individual weight")

#by population
ggplot(datw, aes(y = Individual.Weight, x = Generation)) + 
  geom_boxplot() +
  scale_x_discrete(name= "Cross identity", limits= c("AP", "AF1", "AF2", "CP", "CF1", "CF2"), 
                   labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2")) +
  scale_y_continuous(name= "Individual weight")

##step 2: Fitting a linear mixed model, with cross identity as fixed effect and parental replicate as random effect 

mass.model<- lmer(Individual.Weight~Generation+ (1|Parental.Replicate), data=datw)
anova(mass.model) #singificant fixed effect 
ranova(mass.model) #significant random effect 

##step 3: Checking assumptions 

plot(resid(mass.model))
hist(resid(mass.model))
## assumptions met, model fits really well  

##step 4: Using emmeans() 

emmeans(mass.model, pairwise~Generation)

##step 5: determine averages and sd

dat.mass<-datw %>% group_by(Generation) %>% summarize(weight.average=mean(Individual.Weight), weight.sd=sd(Individual.Weight))
dat.mass



############### postzygotic fertility assay ##################

fert.dat<-read.csv(file.choose()) #choose "PZRF compiled data.csv"
fert.dat<- fert.dat %>% filter(!is.na(Proportion.Red))
names(fert.dat)

### analysis performed separately for male and female fertility data ######

#############female anova###################
datf<- fert.dat %>% filter(Sex=="f") #filter data so you can just work with female values
names(datf)

##step 1: visualize the data

#by replicate 
ggplot(datf) + 
  geom_boxplot(aes(x=Cross.Identity, y=Proportion.Red, fill= Population))+
  scale_x_discrete(name= "Cross identity- female", labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2"))+ 
  scale_y_continuous(name= "Proportion of red eyed offspring")

#by population
ggplot(datf) + 
  geom_boxplot(aes(x=Cross.Identity, y=Proportion.Red))+
  scale_x_discrete(name= "Cross identity- female", labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2"))+ 
  scale_y_continuous(name= "Proportion of red eyed offspring")


##step 2: Fitting the generalized linear mixed model, with cross identity as fixed effect and parental replicate as random effect 

fem.mod<- glmer(Red/(Red+Brown)~Cross.Identity+ (1|Population), data=datf, weights=(Red+Brown), family=binomial)
Anova(fem.mod) #significant fixed effect 

fem.mod.r<- glm(Red/(Red+Brown)~Cross.Identity, data=datf, weights=(Red+Brown), family=binomial)
anova(fem.mod, fem.mod.r) #no significant random effect 

#repeate with lme to determine denominator df
model.fem<-lme(Red/(Red+Brown)~Cross.Identity, random=~1|Population, data=datf)
anova(model.fem)

##step 3: Checking assumptions 
plot(resid(fem.mod))
hist(resid(fem.mod)) #assumptions met nicely 

##step 4: Using emmeans() 

emmeans(fem.mod, pairwise~Cross.Identity)

##step 5: determine averages and sd

dat.fem<- datf %>% group_by(Cross.Identity) %>% summarize(average= mean(Proportion.Red), sd=sd(Proportion.Red))
dat.fem


#############male anova######################
datm<- fert.dat %>% filter(Sex=="m") #filter out female values so just working with male values 

##step 1: visualize the data

#by replicate
ggplot(datm) + 
  geom_boxplot(aes(x=Cross.Identity, y=Proportion.Red, fill=Population))+
  scale_x_discrete(name= "Cross identity- male", labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2"))+ 
  scale_y_continuous(name= "Proportion of red eyed offspring")

#by population
ggplot(datm) + 
  geom_boxplot(aes(x=Cross.Identity, y=Proportion.Red))+
  scale_x_discrete(name= "Cross identity- male", labels= c("AfAm", "AfCm F1", "AfCm F2", "CfCm", "CfAm F1", "CfAm F2"))+ 
  scale_y_continuous(name= "Proportion of red eyed offspring")

##step 2: Fitting the model, cross identity as fixed effect and replicate as random effect 

mal.mod<- glmer(Red/(Red+Brown)~Cross.Identity+ (1|Population), data=datm, family= binomial, weights=(Red+Brown))

Anova(mal.mod) #significant effect

mal.mod.r<- glm(Red/(Red+Brown)~Cross.Identity, data=datm, family= binomial, weights=(Red+Brown))
anova(mal.mod, mal.mod.r) #significant effect of replicate 

#repeate with lme to determine denominator df
model.mal<-lme(Red/(Red+Brown)~Cross.Identity, random=~1|Population, data=datm)
anova(model.mal)

##step 3: Checking assumptions 
plot(resid(mal.mod))
hist(resid(mal.mod))

##step 4: Using emmeans()

emmeans(mal.mod, pairwise~Cross.Identity)

##step 5: determine averages and sd

dat.mal<- datm %>% group_by(Cross.Identity) %>% summarize(average= mean(Proportion.Red), sd=sd(Proportion.Red))
dat.mal
