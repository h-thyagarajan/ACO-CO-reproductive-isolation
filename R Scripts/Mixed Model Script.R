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
library(DHARMa)
library(multcomp)
library(multcompView)



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
qqnorm(residuals(dev.model))

###assumption are met 

##step 4: Using emmeans() to determine pairwise differences 

emmeans(dev.model, pairwise~Generation)

means<-emmeans(dev.model, specs="Generation")
cld(object= means, Letters=letters)

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
names(dat)
hatch.model<- glmer(Viable.Eggs/Total.Eggs~Generation+ (1|Parental.Replicate), data=dat, weights= Total.Eggs, family=binomial)

##step 3: Checking assumptions (using DHARMa package)

#testing fit and dispersion
hatchOutput<- simulateResiduals(fittedModel=hatch.model)
plot(hatchOutput)
testDispersion(hatchOutput) #residuals look great, but significant over dispersion detected (p=0.048)

#looking at residuals by group (mentioned as important in vignette to uncover problems with model)
plotResiduals(hatchOutput, dat$Parental.Replicate, quantreg = T)
plotResiduals(hatchOutput, dat$Generation, quantreg = T) #no issues detected

#conclude no major issues associated with fit of model. Though slight over dispersion, DHARMa vignette stresses importance of interpreting 
#fit apart from just looking at p-values- "If you have a lot of data points, residual diagnostics will nearly inevitably become significant, because 
#having a perfectly fitting model is very unlikely. That, however, doesn’t necessarily mean that you need to change your model. The p-values confirm 
#that there is a deviation from your null hypothesis. It is, however, in your discretion to decide whether this deviation is worth worrying about. 
#For example, if you see a dispersion parameter of 1.01, I would not worry, even if the dispersion test is significant. A significant value of 5, 
#however, is clearly a reason to move to a model that accounts for overdispersion."- quote from vignette 

##step 4: Testing mixed and random effects

Anova(hatch.model) #significant fixed effect

#repeat with lme to determine denominator df
library(nlme)

model<-lme(Viable.Eggs/Total.Eggs~Generation, random=~1|Parental.Replicate, data=dat)
anova(model)

#ranova() doesn't work for glmer() so fitting another model without random effect and comparing 

hatch.model.r<- glm(Viable.Eggs/Total.Eggs~Generation, data=dat, weights= Total.Eggs, family=binomial)
anova(hatch.model, hatch.model.r) #no significant random effect 

##step 5: Using emmeans() 

emmeans(hatch.model, pairwise~Generation)

means<-emmeans(hatch.model, specs="Generation")
cld(object= means, Letters=letters)

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

##step 3: testing model meets assumptions

#testing overall fit and dispersion
viabilityOutput<- simulateResiduals(fittedModel=viab.model)
plot(viabilityOutput)
testDispersion(viabilityOutput) #significant overdispersion detected- reflected in residuals

#looking at residuals by group
plotResiduals(viabilityOutput, dat$Parental.Replicate, quantreg = T) #heteroscedasticity, level of overdispersion depends on Parental.Replicate- 
#which I believe is accounted for in random effect term
plotResiduals(viabilityOutput, dat$Generation, quantreg = T)

#so there is overdispersion that needs to be accounted for. Heteroscedasticity uncovered when look at residuals grouped by Parental Replicate, 
#but this is accounted for in random effect which is a good start. 

##step 4: fitting betabinomial model to account for overdispersion

#I am confident that binomial is most appropriate for our count-proportion data. 
#This is a method I have seen mentioned as a way to deal with overdispersed count-proportion data without changing main distribution (see DHARMa vignette).

library(glmmTMB)

install.packages('glmmTMB', type = 'source')

betamodel.via<-glmmTMB(Total.Adults/Viable.Eggs.corrected~Generation+ (1|Parental.Replicate), data=dat, weights= Viable.Eggs.corrected, family='betabinomial')

#testing fit and dispersion of betabinomial model

viability.B.Output<- simulateResiduals(fittedModel=betamodel.via) 
plot(viability.B.Output) 
testDispersion(viability.B.Output) #corrects overdispersion

#looking at residuals by group
plotResiduals(viability.B.Output, dat$Parental.Replicate, quantreg = T) #corrects heteroscedasticity
plotResiduals(viability.B.Output, dat$Generation, quantreg = T)

#confirm this model is a better fit
AIC(betamodel.via, viab.model) #betamodel is better fit

##step 5: Testing mixed and random effects

Anova(betamodel.via) #significant effect of generation 

#testing random effects
betamodel.via.nr<-glmmTMB(Total.Adults/Viable.Eggs.corrected~Generation, data=dat, weights= Viable.Eggs.corrected, family='betabinomial')

anova(betamodel.via, betamodel.via.nr) #no significant random effect

#repeat with lme to determine denominator df
library(nlme)
model<-lme(Total.Adults/Viable.Eggs.corrected~Generation, random=~1|Parental.Replicate, data=dat)
anova(model) #5, 154 degrees of freedom


##step 6: Using emmeans() to determine pairwise differences 

emmeans(betamodel.via, pairwise~Generation)
means.via<-emmeans(betamodel.via, specs="Generation")
cld(object= means.via, Letters=letters)

##step 7: determine averages and sd

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
anova(mass.model) #significant fixed effect 
ranova(mass.model) #significant random effect 

##step 3: Checking assumptions 

plot(resid(mass.model))
hist(resid(mass.model))
qqnorm(residuals(mass.model))
## assumptions met, model fits well  

##step 4: Using emmeans() 

emmeans(mass.model, pairwise~Generation)
means<-emmeans(mass.model, specs="Generation")
cld(object= means, Letters=letters)

##step 5: determine averages and sd

dat.mass<-datw %>% group_by(Generation) %>% summarize(weight.average=mean(Individual.Weight), weight.sd=sd(Individual.Weight))
dat.mass



############### postzygotic fertility assay ##################

fert.dat<-read.csv(file.choose()) #choose "PZRF compiled data.csv"
fert.dat<- fert.dat %>% filter(!is.na(Proportion.Red))
fert.dat <- fert.dat %>% group_by(Population, Vial.ID) %>% mutate(Total=sum(Red,Brown))
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


##step 3: Checking assumptions 

#testing fit and dispersion
femOutput<- simulateResiduals(fittedModel=fem.mod)
plot(femOutput)
testDispersion(femOutput) #significant residual patterns and overdispersion detected

#looking at residuals by group
plotResiduals(femOutput, datf$Population, quantreg = T) #heterogeneity in cross identity and population 
plotResiduals(femOutput, datf$Cross.Identity, quantreg = T)

#significant dispersion issues that need to be accounted for. 

##step 4: Fitting betabinomial model to account for overdispersion

beta.f.model<-glmmTMB(Red/Total~Cross.Identity+ (1|Population), data=datf, weights=Total, family='betabinomial')

#testing fit and dispersion
f.B.Output<- simulateResiduals(fittedModel=beta.f.model) 
plot(f.B.Output) 
testDispersion(f.B.Output) #seems to have helped, though now significant underdispersion

#looking at residuals by group
plotResiduals(f.B.Output, datf$Population, quantreg = T) #better, but still significant 

plotResiduals(f.B.Output, datf$Cross.Identity, quantreg = T) #better, but still significant

#seems to have helped a lot, but still dispersion issues. Do not want to overcomplicating model 

AIC(beta.f.model, fem.mod) #betabinomial zero inflation model offers best fit

##step 5: Testing fixed and random effects

Anova(beta.f.model) #significant effect of Cross Identity 

#testing random effects
beta.f.model.nr<-glmmTMB(Red/Total~Cross.Identity, data=datf, weights=Total, family='betabinomial')
anova(beta.f.model, beta.f.model.nr) #no significant random effect 

#repeat with lme to determine denominator df
model.fem<-lme(Red/(Red+Brown)~Cross.Identity, random=~1|Population, data=datf)
anova(model.fem) # 5, 515 df 

##step 6: Using emmeans() 

means<-emmeans(beta.f.model, specs="Cross.Identity")
cld(object= means, Letters=letters)


##step 7: determine averages and sd

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

mal.mod<- glmer(Red/(Total)~Cross.Identity+ (1|Population), data=datm, family= binomial, weights=(Total))

##step 3: Checking assumptions 

#testing fit and dispersion
malOutput<- simulateResiduals(fittedModel=mal.mod)
plot(malOutput)
testDispersion(malOutput) #extreme residual and dispersion issues

#looking at residuals by group
plotResiduals(malOutput, datm$Population, quantreg = T)
plotResiduals(malOutput, datm$Cross.Identity, quantreg = T) 

##step 4: Fitting betabinomial model to account for overdispersion

beta.m.model<-glmmTMB(Red/(Total)~Cross.Identity+ (1|Population), data=datm, weights=(Total), family='betabinomial')

m.B.Output<- simulateResiduals(fittedModel=beta.m.model) 
plot(m.B.Output) 
testDispersion(m.B.Output) #seems to have accounted for overdispersion

#looking at residuals by group
plotResiduals(m.B.Output, datm$Population, quantreg = T) #much better

plotResiduals(m.B.Output, datm$Cross.Identity, quantreg = T) #MUCH better

#solved dispersion issues, next step to make sure results given by new model still make sense. 

AIC(beta.m.model, mal.mod) #beta model offers better fit

##step 5: Testing fixed and random effects

Anova(beta.m.model) #significant effect of Cross Identity

#testing random effect
beta.m.model.nr<-glmmTMB(Red/(Total)~Cross.Identity, data=datm, weights=(Total), family='betabinomial')
anova(beta.m.model, beta.m.model.nr) #no significant random effect 

#repeat with lme to determine denominator df
model.mal<-lme(Red/(Red+Brown)~Cross.Identity, random=~1|Population, data=datm)
anova(model.mal) #5, 502 df 

##step 6: Using emmeans() 

means<-emmeans(beta.m.model, specs="Cross.Identity")
cld(object= means, Letters=letters)

##step 7: determine averages and sd

dat.mal<- datm %>% group_by(Cross.Identity) %>% summarize(average= mean(Proportion.Red), sd=sd(Proportion.Red))
dat.mal
