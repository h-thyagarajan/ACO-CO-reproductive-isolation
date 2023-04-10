library(ggplot2)
library(lme4)
library(dplyr)
library(car)
require(emmeans)

##FEMALE DATA SECTION##
#input data+data structure+filter out NAs
fmatechoicedat <- read.csv(file.choose()) #select compiled data
fmatechoicedat <- fmatechoicedat %>% filter(mating.latency!="NA")
fmatechoicedat$replicate <- as.numeric(gsub("[AC]", "", fmatechoicedat$female.id))
fmatechoicedat$replicate <- as.factor(fmatechoicedat$replicate)
fmatechoicedat$mated.male.pop <- as.factor(fmatechoicedat$mated.male.pop)
fmatechoicedat$female.pop <- as.factor(fmatechoicedat$female.pop)

str(fmatechoicedat)
#latency models - log transformed data to fix residual kurtosis
f.lat.int.mix.mod <- lmer(log(mating.latency+1) ~ mated.male.pop*female.pop + (1|trial.id/replicate), data=fmatechoicedat)
library(lmerTest)
anova(f.lat.int.mix.mod)

#testing random effects model comparison and looking at variance described by random effect

summary(f.lat.int.mix.mod)

f.lat.int.mod <- lm(log(mating.latency+1) ~ mated.male.pop*female.pop, data=fmatechoicedat)
anova(f.lat.int.mix.mod, f.lat.int.mod)

emmeans(f.lat.int.mix.mod, pairwise~female.pop*mated.male.pop)
f.lat<-fmatechoicedat %>% group_by(female.pop, mated.male.pop) %>% summarize(lat.avg=mean(mating.latency), lat.sd=sd(mating.latency), n=n());f.lat

#Assumption testing
plot(resid(f.lat.int.mix.mod))
hist(resid(f.lat.int.mix.mod))

#duration models
f.dur.int.mix.mod <- lmer(mating.duration ~ mated.male.pop*female.pop + (1|trial.id/replicate), data=fmatechoicedat)
anova(f.dur.int.mix.mod)

#testing random effects
summary(f.dur.int.mix.mod)
f.dur.int.mod <- lm(mating.duration ~ mated.male.pop*female.pop, data=fmatechoicedat)
anova(f.dur.int.mix.mod,f.dur.int.mod)

emmeans(f.dur.int.mix.mod, pairwise~female.pop*mated.male.pop)
f.dur<-fmatechoicedat %>% group_by(female.pop, mated.male.pop) %>% summarize(dur.avg=mean(mating.duration), dur.sd=sd(mating.duration), n=n());f.dur

#Assumption testing
plot(resid(f.dur.int.mix.mod))
hist(resid(f.dur.int.mix.mod))

#latency, duration visualization
ggplot(data=fmatechoicedat, aes(x=female.pop, y=log(mating.latency), fill=mated.male.pop))+
  geom_boxplot() + 
  theme_classic()

ggplot(data=fmatechoicedat, aes(x=female.pop, y=mating.duration, fill=mated.male.pop))+
  geom_boxplot() +
  theme_classic()

#data summary
f.dur<-fmatechoicedat %>% group_by(female.pop) %>% summarize(dur.avg=mean(mating.duration), dur.sd=sd(mating.duration));f.dur


##MALE DATA SECTION##
#input data+data structure+filter out NAs
mmatechoicedat <- read.csv(file.choose())
mmatechoicedat <- mmatechoicedat %>% filter(mating.latency!="NA")
mmatechoicedat$replicate <- as.numeric(gsub("[AC]", "", mmatechoicedat$Mated.male))
head(mmatechoicedat)
#latency models - log transformed data to fix residual kurtosis
m.lat.int.mix.mod <- lmer(log(mating.latency+1) ~ Mated.male.pop*Mated.female.pop + (1|Trial.ID/replicate), data=mmatechoicedat)
anova(m.lat.int.mix.mod)

#random effects
summary(m.lat.int.mix.mod)
m.lat.int.mod <- lm(log(mating.latency+1) ~ Mated.male.pop*Mated.female.pop, data=mmatechoicedat)
anova(m.lat.int.mix.mod, m.lat.int.mod)

emmeans(m.lat.int.mix.mod, pairwise~Mated.male.pop*Mated.female.pop)
m.lat<-mmatechoicedat %>% group_by(Mated.female.pop, Mated.male.pop) %>% summarize(lat.avg=mean(mating.latency), lat.sd=sd(mating.latency), n=n());m.lat

#Assumption testing
plot(resid(m.lat.int.mix.mod))
hist(resid(m.lat.int.mix.mod))

#duration models
m.dur.int.mix.mod <- lmer(mating.duration ~ Mated.male.pop*Mated.female.pop + (1|Trial.ID/replicate), data=mmatechoicedat)
anova(m.dur.int.mix.mod)

#random effects 
summary(m.dur.int.mix.mod)
m.dur.int.mod <- lm(mating.duration ~ Mated.male.pop*Mated.female.pop, data=mmatechoicedat)
anova(m.dur.int.mix.mod, m.dur.int.mod)
emmeans(m.dur.int.mix.mod, pairwise~Mated.male.pop*Mated.female.pop)
m.dur<-mmatechoicedat %>% group_by(Mated.female.pop, Mated.male.pop) %>% summarize(dur.avg=mean(mating.duration), dur.sd=sd(mating.duration), n=n());m.dur

#Assumption testing
plot(resid(m.dur.int.mix.mod))
hist(resid(m.dur.int.mix.mod))

#latency, duration visualization
ggplot(data=mmatechoicedat, aes(x=Mated.male.pop, y=log(mating.latency), fill=Mated.female.pop))+
  geom_boxplot() + 
  theme_classic()

ggplot(data=mmatechoicedat, aes(x=Mated.male.pop, y=mating.duration, fill=Mated.female.pop))+
  geom_boxplot() + 
  theme_classic()

