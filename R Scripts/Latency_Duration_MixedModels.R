library(ggplot2)
library(lme4)
library(dplyr)
library(car)

##FEMALE DATA SECTION##
#input data+data structure+filter out NAs
fmatechoicedat <- read.csv(file.choose()) #select compiled data
fmatechoicedat <- fmatechoicedat %>% filter(mating.latency!="NA")

#latency models
f.lat.int.mix.mod <- lmer(mating.latency ~ mated.male.pop*female.pop + (1|trial.id), data=fmatechoicedat)
Anova(f.lat.int.mix.mod)

#duration models
f.dur.int.mix.mod <- lmer(mating.duration ~ mated.male.pop*female.pop + (1|trial.id), data=fmatechoicedat)
Anova(f.dur.int.mix.mod)

#latency, duration visualization
ggplot(data=fmatechoicedat, aes(x=female.pop, y=mating.latency, fill=mated.male.pop))+
  geom_boxplot() +
  theme_classic()

ggplot(data=fmatechoicedat, aes(x=female.pop, y=mating.duration, fill=mated.male.pop))+
  geom_boxplot() +
  theme_classic()

##MALE DATA SECTION##
#input data+data structure+filter out NAs
mmatechoicedat <- read.csv(file.choose())
mmatechoicedat <- mmatechoicedat %>% filter(mating.latency!="NA")
mmatechoicedat$mated.combo <- as.factor(paste(mmatechoicedat$Mated.male.pop, mmatechoicedat$Mated.female.pop))

#latency models
m.lat.int.mix.mod <- lmer(mating.latency ~ Mated.male.pop*Mated.female.pop + (1|Trial.ID), data=mmatechoicedat)
Anova(m.lat.int.mix.mod)

#duration models
m.dur.int.mix.mod <- lmer(mating.duration ~ Mated.male.pop*Mated.female.pop + (1|Trial.ID), data=mmatechoicedat)
Anova(m.dur.int.mix.mod)

#latency, duration visualization
ggplot(data=mmatechoicedat, aes(x=Mated.male.pop, y=mating.latency, fill=Mated.female.pop))+
  geom_boxplot() +
  theme_classic()

ggplot(data=mmatechoicedat, aes(x=Mated.male.pop, y=mating.duration, fill=Mated.female.pop))+
  geom_boxplot() +
  theme_classic()

