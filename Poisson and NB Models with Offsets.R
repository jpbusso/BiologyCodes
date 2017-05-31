## Load packages
library(lattice)
library(rgl)
library(plot3D)
library(scatterplot3d)
library(plot3Drgl)
library(mvtnorm)
library(plyr)
library(ggplot2)
library(lme4)
library(gridExtra)
library(drc)
library(devtools)
library(FactoMineR)
library(Hmisc)
library(RColorBrewer)
library(binhf)
library(pracma)
library(mixtools)
library(MASS)
library(adehabitatLT)
library(adehabitatHR)
library(gdata)
library(drc)
library(Rmisc)
library(car)
library(RCurl)
library(GGally)
library(dplyr)
library(MuMIn) 
library(nlme)
library(ggExtra)
library(qvalue)
library(gtools)
library(scales)
library(pscl)
library(boot)
library(R2admb)
library(glmmADMB)
library(coefplot)
library(bbmle)
library(sjPlot)
library(R2admb)


## Clear Rs brain
rm(list=ls())
##########################################################################################################
## Load DataSets #####
##########################################################################################################

## Start with the 444
## Read the dataset for Density
dat <- read.csv("D:/My documents/University/Zuerich/PhD Thesis/Female Preference/4-4-4/444 Sex Sel/Data/AllColorSorted.csv")
keepList<-c("ID","FArea.1","FArea.6","HArea.1","HArea.6","TArea.1","TArea.6")
dat<-dat[keepList]
##Read the dataset for Behavior and Pop and such
dat1<-read.csv("D:/My documents/University/Zuerich/PhD Thesis/Female Preference/4-4-4/444OffDataSet.csv")
names(dat1)
## Merge DataFrames of the 444 Exp with the IDs
all444 <- merge(dat, dat1, by="ID")


##From the label extract a Binomial Var "Sex" and with this it should be READY
all444$Sex<-substring(all444$ID,1,1)

##Substract the StartTime

all444$X1st<- all444$X1st - all444$StartTime
all444$X1stWave<- all444$X1stWave - all444$StartTime
all444$X1stMount<- all444$X1stMount - all444$StartTime
all444$DefinitMount<-all444$DefinitMount - all444$StartTime
all444$CopuIni<-all444$CopuIni - all444$StartTime
all444$CopuFin<-all444$CopuFin - all444$StartTime
all444$CopLat<-all444$CopuIni

## Vars of Latencies and Durations
all444$CopDur<-all444$CopuFin-all444$CopuIni
all444$FemResistance<-all444$CopuIni - all444$DefinitMount

##Couple ID for 444
all444$PairID <- paste(all444$Trial,all444$CopRest, sep = " ")

dat<-all444
# ######## Now the ReproKurs 2014
# ##Read the dataset
# dat2 <- read.csv("D:/My documents/University/Zuerich/PhD Thesis/Morph Analysis/All Exps/Measuremnts and all possible sex differences.csv")
# names(dat2)
# ## Rename the vars that need to be renamed
# dat2<-rename.vars(dat2, c("Area.1","Area.6","MaleAttempt"),
#                   c("FArea.1","FArea.6","MountAttempt"))
# 
# keepList2<-c("ID","Experiment","Pop","Line","Density","Temp","FArea.1","FArea.6","HArea.1","HArea.6","TArea.1","TArea.6","FLFemur","FLTibia","FLLength",
#             "Sex","ID2","FemFemur","FemTibia","FemLength","CopLat","CopDur","MountAttempt","Trial")
# 
# dat2<-dat2[keepList2]
# 
# ## Convert the minutes to seconds in the Reporkurs DataSet
# dat2$CopDur<- dat2$CopDur * 60
# dat2$CopLat<- dat2$CopLat * 60
# 
# ## Merge the files
# dat <- merge(all444, dat2, by=c("ID","Trial","Pop","Line","Temp","Experiment","FArea.1","FArea.6","Sex","HArea.1",
#                                 "HArea.6","TArea.1","TArea.6","CopDur","CopLat","MountAttempt"),
#              all=TRUE)
# 
# names(dat)



######### Now the Lara and Chantal Flies
# ##Read the Color dataset
# dat3 <- read.csv("D:/My documents/University/Zuerich/PhD Thesis/Female Preference/Chantal und Lara/Data/All Colors.csv")
# ##Read the dataset for Behavior and Pop and such
# behav <- read.csv("D:/My documents/University/Zuerich/PhD Thesis/Female Preference/Chantal und Lara/data fly behaviour.csv")
# 
# ## Merge DataFrames of Lara und Chantal with the IDs
# LarCha <- merge(dat3, behav, by="ID",
#                 all=TRUE)
# 
# keepList3<-c("ID","Area.1","Area.6","Block","Trial","Density","Experiment","Temp","Frequency","Pop","Line","Date","Starting.time","Fights","Sender","ind1",
#              "Receiver","Wave","morphT","X..neg.try","X..pos.try","Sex","MorphCopu","search.time","CopDur",
#              "X..offspring","Cop","BS","yellowness","BS.F","Nactors","Nreceivers","TimeLapse")
# 
# LarCha<-LarCha[keepList3]
# 
# ##From the label extract a Binomial Var (True if "COP" is present)
# LarCha$Cop<-grepl("COP",LarCha$ID)
# MSE<-summarySE(LarCha, measurevar="Cop", groupvars=c("Trial"), na.rm = TRUE)
# 
# 
# ## Rename the vars that need to be renamed
# LarCha<-rename.vars(LarCha, c("Area.1","Area.6","Fights","Wave"),
#                     c("FArea.1","FArea.6","Fight","AbWav"))
# 
# ## Assign Individuals Sex based on name
# LarCha$Sex<-grepl("F",LarCha$ID)
# LarCha$Sex[LarCha$Sex ==TRUE]<-"F"
# LarCha$Sex[LarCha$Sex ==FALSE]<-"M"
# 
# ## Assign to all Experiment LarCha and also Temp
# LarCha$Experiment<-"LarCha"
# LarCha$Temp<-30
# 
# ## COnvert Trial to Factor in LarCha
# LarCha$Trial<-substring(LarCha$ID,1,4)
# LarCha$Trial<-factor(LarCha$Trial)
# 
# ##Couple ID for LarCha
# LarCha$PairID <- substring(LarCha$ID,1,6)
# 
# ## Remove the Fights between A-O because we don't know who started it.
# LarCha$Fights[LarCha$ind1 == "AO"]<-NA
# 
# 
# dat <- merge(dat, LarCha, by=c("ID","Trial","Pop","Line","Temp","Experiment","FArea.1","FArea.6","Sex","CopDur","Density",
#                                "Sender","Receiver","AbWav","Fight","PairID","Nactors","Nreceivers","TimeLapse"),
#              all=TRUE)

##
levels(dat$Sender)
levels(dat$Receiver)
dat$Sender<-revalue(dat$Sender, c("A"="Amber", "O"="Obsidian"))
dat$Receiver<-revalue(dat$Receiver, c("A"="Amber", "O"="Obsidian"))

##
dat<-dat[dat$Sender!="",]
dat<-dat[dat$Receiver!="",]
dat$Sender<-factor(dat$Sender)
dat$Receiver<-factor(dat$Receiver)
str(dat)
dat$TimeLapse<-dat$TimeLapse/60

##########################################################################################################
## Calculate and Correct some Variables #####
##########################################################################################################

##Calculate % Yellow FL
dat$YoverA<-as.numeric(dat$FArea.6/dat$FArea.1)

##Calculate % Yellow Thorax
dat$TYoverA<-as.numeric(dat$TArea.6/dat$TArea.1)

##Calculate % Yellow HL
dat$HYoverA<-as.numeric(dat$HArea.6/dat$HArea.1)

##There is no 100% Yellow, so all 1s converted to 0s
dat$YoverA[dat$YoverA == 1]<-0
dat$HYoverA[dat$HYoverA == 1]<-0

##From the label extract a Binomial Var (True if "COP" is present)
dat$Cop<-grepl("COP",dat$ID)


##########################################################################################################
## Set Morphs and Graphing Colorations  #####
##########################################################################################################

##Set the morphs and Sexes based on YoverA
dat$Morph <- ifelse(dat$Sex == "F",dat$Morph<-"Female",
                    ifelse(dat$YoverA <= .27, dat$Morph <-"Obsidian",
                           ifelse(dat$YoverA >= .57, dat$Morph <-"Amber",
                                  dat$Morph <- "Intermediate")))

datTest<-dat[,c("ID","Experiment","YoverA","FArea.1","Sex","Morph")]

dat<-dat[complete.cases(dat$Morph),]

dat$Morph<-factor(dat$Morph)

## Define color for MORPHS
MorphCol<-c("#FFAA00","#00CF00","#0088FF","black")
names(MorphCol)<-levels(dat$Morph)

## Reverse Coloration
dat$YoverA<-1-dat$YoverA

##########################################################################################################
## Fights GLMER With Offsets #####
##########################################################################################################

datStat<-dat[complete.cases(dat$Fight),]
# datStat<-dat[dat$Experiment!="LarCha",]
# datStat$Receiver<-factor(datStat$Receiver)
# datStat$Trial<-factor(datStat$Trial)
# datStat<-all444
# datStat<-datStat[datStat$Sender!="",]
# datStat<-datStat[datStat$Receiver!="",]
# datStat$datStat<-factor(datStat$Sender)
# datStat$Receiver<-factor(datStat$Receiver)

ggplot(data = datStat, aes(Fight, fill = Receiver))+
  geom_histogram(position = "dodge")+
  facet_grid(~Sender)

modelPoisson<-glmer(Fight~Sender+Receiver+
                      Sender:Receiver+
                      (1|Trial)+(1|Pop:Line),
                    offset = log(TimeLapse)+log(Nactors)+log(Nreceivers),
                    data = datStat,
                    family=poisson)

## Check the variance
plot(modelPoisson)
## Check normality of the residuals
qqnorm(resid(modelPoisson))
## Check goodness of fit (residual deviance and degrees of freedom)
1-pchisq(sum(residuals(modelPoisson)^2),df.residual(modelPoisson))
## Check if random effects are normally distributed
sjp.glmer(modelPoisson, type = "re.qq")
## Check correlation of fixed effects
sjp.glmer(modelPoisson, type = "fe.cor")
## plot probability curve of fixed effects
sjp.glmer(modelPoisson, type = "ri.pc",
          show.se = FALSE,
          facet.grid = FALSE)
## Check the distribution of residuals in levels of factors
plot(datStat$Sender:datStat$Receiver,residuals(modelPoisson))
## posterior predictive simulations to test whether the model is behaving like the data in other ways
sims <- simulate(modelPoisson,nsim=1000)
nOnes <- colSums(sims==1)
par(las=1,bty="l")
plot(pt <- prop.table(table(nOnes)),
     ylab="Probability")
(obsOne <- sum(datStat$Fight==1))
points(obsOne,0.02,col="red",pch=16,cex=2)

modelNB<-glmer.nb(Fight~Sender+Receiver+
                    Sender:Receiver+
                    (1|Trial)+(1|Pop:Line),
                  offset = log(TimeLapse)+log(Nactors)+log(Nreceivers),
                  data = datStat,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2000000)))

## Check normality of the residuals
qqnorm(resid(modelNB))
## Check goodness of fit (residual deviance and degrees of freedom)
1-pchisq(sum(residuals(modelNB)^2),df.residual(modelNB))
## Check if random effects are normally distributed
sjp.glmer(modelNB, type = "re.qq")
## Check correlation of fixed effects
sjp.glmer(modelNB, type = "fe.cor")
## plot probability curve of fixed effects
sjp.glmer(modelNB, type = "ri.pc",
          show.se = FALSE,
          facet.grid = FALSE)
## Check the distribution of residuals in levels of factors
plot(datStat$Sender:datStat$Receiver,residuals(modelNB))
## posterior predictive simulations to test whether the model is behaving like the data in other ways
sims <- simulate(modelNB,nsim=1000)
nOnes <- colSums(sims==1)
par(las=1,bty="l")
plot(pt <- prop.table(table(nOnes)),
     ylab="Probability")
(obsOne <- sum(datStat$Fight==1))
points(obsOne,0.02,col="red",pch=16,cex=2)

## Hence I need generate a Poisson model (already done) and compare their likelihood ratios
anova(modelNB, modelPoisson)

## OR more oldschool with a manual Loglikelihood comparison
X2 <- 2 * (logLik(modelNB) - logLik(modelPoisson))
# and then in the Chisquare Distribution look for the signifcance, with df as difference between the df of the two models
pchisq(X2, df = 1, lower.tail = FALSE)

## Compare the models
AICtab(modelPoisson, modelNB)
multiplot(modelPoisson,modelNB)

summary(modelPoisson)
plot(modelPoisson)
Anova(modelPoisson)
Anova(modelPoisson, type =3)
hist(resid(modelPoisson))

summary(modelNB)
plot(modelNB)
Anova(modelNB)
Anova(modelNB, type =3)
hist(resid(modelNB))

datStat$predY<-predict(modelPoisson, newdata = datStat, type = "response", na.action="na.exclude")

MSE<-summarySE(datStat, measurevar="predY", groupvars=c("Sender","Receiver"), na.rm = TRUE)
MSE<-MSE[complete.cases(MSE$predY),]

pd <- position_dodge(.3)
ggplot(MSE, aes(x=Sender, y=predY, color = Receiver))+ 
  geom_errorbar(aes(ymin=predY-ci, ymax=predY+ci), width=.2, size = 2, position = pd) +
  geom_point(position = pd, size = 8.5)+
  ##geom_line(position=pd)+
  ##stat_smooth(method=glm, family = binomial, size = 2)+
  ##xlim(.03,.23)+
  ##facet_grid(~Pop)+
  xlab("Attacker Morph")+
  ylab("Fights (per individual/per min)")+
  scale_color_manual(values = MorphCol, name = "Target \nMorph")+
  ##scale_fill_manual(values = MorphCol)+
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        axis.text.x=element_text(face="italic"))+
  theme(strip.text.x = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

datStat$predY<-predict(modelNB, newdata = datStat, type = "response", na.action="na.exclude")

MSE<-summarySE(datStat, measurevar="predY", groupvars=c("Sender","Receiver"), na.rm = TRUE)
MSE<-MSE[complete.cases(MSE$predY),]

pd <- position_dodge(.3)
ggplot(MSE, aes(x=Sender, y=predY, color = Receiver))+ 
  geom_errorbar(aes(ymin=predY-ci, ymax=predY+ci), width=.2, size = 2, position = pd) +
  geom_point(position = pd, size = 8.5)+
  ##geom_line(position=pd)+
  ##stat_smooth(method=glm, family = binomial, size = 2)+
  ##xlim(.03,.23)+
  ylim(0,0.07)+
  ##facet_grid(~Pop)+
  xlab("Attacker Morph")+
  ylab("Fights (per individual/per min)")+
  scale_color_manual(values = MorphCol, name = "Target \nMorph")+
  ##scale_fill_manual(values = MorphCol)+
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        axis.text.x=element_text(face="italic"))+
  theme(strip.text.x = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

MSE<-summarySE(dat, measurevar="Fight", groupvars=c("Sender","Receiver"), na.rm = TRUE)
MSE<-MSE[complete.cases(MSE$Fight),]

pd <- position_dodge(.3)
ggplot(MSE, aes(x=Sender, y=Fight, color = Receiver))+ 
  geom_errorbar(aes(ymin=Fight-ci, ymax=Fight+ci), width=.2, size = 2, position = pd) +
  geom_point(position = pd, size = 8.5)+
  ##geom_line(position=pd)+
  ##stat_smooth(method=glm, family = binomial, size = 2)+
  ##xlim(.03,.23)+
  ##facet_grid(~Pop)+
  xlab("Attacker Morph")+
  ylab("Fights (per individual/\nper trial)")+
  scale_color_manual(values = MorphCol, name = "Target \nMorph")+
  ##scale_fill_manual(values = MorphCol)+
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        legend.position = "none")+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        axis.text.x=element_text(face="italic"))+
  theme(strip.text.x = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

MSE<-summarySE(dat, measurevar="Fight", groupvars=c("Sender"), na.rm = TRUE)
MSE<-MSE[complete.cases(MSE$Fight),]

ggplot(MSE, aes(x=Sender, y=Fight, color = Sender))+ 
  geom_errorbar(aes(ymin=Fight-ci, ymax=Fight+ci), width=.2, size = 2, position = pd) +
  geom_point(position = pd, size = 8.5)+
  ##geom_line(position=pd)+
  ##stat_smooth(method=glm, family = binomial, size = 2)+
  ##xlim(.03,.23)+
  ##facet_grid(~Pop)+
  xlab("Attacker Morph")+
  ylab("Fights (per individual/\nper trial)")+
  scale_color_manual(values = MorphCol, name = "Target \nMorph")+
  ##scale_fill_manual(values = MorphCol)+
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        legend.position = "none")+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        axis.text.x=element_text(face="italic"))+
  theme(strip.text.x = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

##########################################################################################################
## Wave GLMER With Offsets #####
##########################################################################################################
datStat<-dat

modelPoisson<-glmer(AbWav~Sender+Receiver+
                      Sender:Receiver+
                      (1|Trial),
                    offset = log(TimeLapse)+log(Nactors)+log(Nreceivers),
                    data = datStat,
                    family=poisson)

modelNB<-glmer.nb(AbWav~Sender+Receiver+
                    Sender:Receiver+
                    (1|Trial),
                  offset = log(TimeLapse)+log(Nactors)+log(Nreceivers),
                  data = datStat,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2000000)))

## Hence I need generate a Poisson model (already done) and compare their likelihood ratios
anova(modelNB, modelPoisson)

## OR more oldschool with a manual Loglikelihood comparison
X2 <- 2 * (logLik(modelNB) - logLik(modelPoisson))
# and then in the Chisquare Distribution look for the signifcance, with df as difference between the df of the two models
pchisq(X2, df = 1, lower.tail = FALSE)

## Compare the models
AICtab(modelPoisson, modelNB)
multiplot(modelPoisson,modelNB)

summary(modelNB)
plot(modelNB)
Anova(modelNB)
Anova(modelNB, type =3)
hist(resid(modelNB))

Anova(modelPoisson)

dat$predY<-predict(modelNB, newdata = datStat, type = "response", na.action="na.exclude")
dat$predY<-predict(modelPoisson, newdata = datStat, type = "response", na.action="na.exclude")


MSE<-summarySE(dat, measurevar="predY", groupvars=c("Sender","Receiver"), na.rm = TRUE)
MSE<-MSE[complete.cases(MSE$predY),]

pd <- position_dodge(.3)
ggplot(MSE, aes(x=Sender, y=predY, color = Receiver))+ 
  geom_errorbar(aes(ymin=predY-ci, ymax=predY+ci), width=.2, size = 2, position = pd) +
  geom_point(position = pd, size = 8.5)+
  ##geom_line(position=pd)+
  ##stat_smooth(method=glm, family = binomial, size = 2)+
  ##xlim(.03,.23)+
  ##facet_grid(~Pop)+
  ylim(0,0.07)+
  xlab("Signaler Morph")+
  ylab("Wavings (per individual/per min)")+
  scale_color_manual(values = MorphCol, name = "Target \nMorph")+
  ##scale_fill_manual(values = MorphCol)+
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        axis.text.x=element_text(face="italic"))+
  theme(strip.text.x = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

MSE<-summarySE(dat, measurevar="AbWav", groupvars=c("Sender","Receiver"), na.rm = TRUE)
MSE<-MSE[complete.cases(MSE$Fight),]

pd <- position_dodge(.3)
ggplot(MSE, aes(x=Sender, y=AbWav, color = Receiver))+ 
  geom_errorbar(aes(ymin=AbWav-ci, ymax=AbWav+ci), width=.2, size = 2, position = pd) +
  geom_point(position = pd, size = 8.5)+
  ##geom_line(position=pd)+
  ##stat_smooth(method=glm, family = binomial, size = 2)+
  ##xlim(.03,.23)+
  ##facet_grid(~Pop)+
  xlab("Attacker Morph")+
  ylab("Fights (per individual/\nper trial)")+
  scale_color_manual(values = MorphCol, name = "Target \nMorph")+
  ##scale_fill_manual(values = MorphCol)+
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        legend.position = "none")+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        axis.text.x=element_text(face="italic"))+
  theme(strip.text.x = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

##########################################################################################################
## Fights based on Waves GLMER With Offsets  #####
##########################################################################################################
datStat<-dat[dat$Sender == "Amber" & dat$Receiver == "Obsidian",]
datStat1<-dat[dat$Sender == "Obsidian" & dat$Receiver == "Amber",]

keeps<-c("Trial","AbWav")
datStat2<-datStat[keeps]

drops <- c("AbWav")
datStat1<-datStat1[,!(names(datStat1) %in% drops)]

datStat3<-merge(datStat1,datStat2, by = "Trial", all = TRUE)

plot(datStat3$Fight~datStat3$AbWav)

modelPoisson<-glmer(Fight~AbWav+
                      (1|Trial),
                    offset = log(TimeLapse)+log(Nactors)+log(Nreceivers),
                    data = datStat3,
                    family=poisson,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2000000)))

## Check the variance
plot(modelPoisson)
## Check normality of the residuals
qqnorm(resid(modelPoisson))
## Check goodness of fit (residual deviance and degrees of freedom)
1-pchisq(sum(residuals(modelPoisson)^2),df.residual(modelPoisson))
## Check if random effects are normally distributed
sjp.glmer(modelPoisson, type = "re.qq")
## Check correlation of fixed effects
sjp.glmer(modelPoisson, type = "fe.cor")
## plot probability curve of fixed effects
sjp.glmer(modelPoisson, type = "ri.pc",
          show.se = FALSE,
          facet.grid = FALSE)
## posterior predictive simulations to test whether the model is behaving like the data in other ways
sims <- simulate(modelPoisson,nsim=1000)
nOnes <- colSums(sims==1)
par(las=1,bty="l")
plot(pt <- prop.table(table(nOnes)),
     ylab="Probability")
(obsOne <- sum(datStat$Fight==1))
points(obsOne,0.02,col="red",pch=16,cex=2)


modelNB<-glmer.nb(Fight~AbWav+
                    (1|Trial)+(1|Pop:Line),
                  offset = log(TimeLapse)+log(Nactors)+log(Nreceivers),
                  data = datStat3,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20000000000)))

## Check normality of the residuals
qqnorm(resid(modelNB))
## Check goodness of fit (residual deviance and degrees of freedom)
1-pchisq(sum(residuals(modelNB)^2),df.residual(modelNB))
## Check if random effects are normally distributed
sjp.glmer(modelNB, type = "re.qq")
## Check correlation of fixed effects
sjp.glmer(modelNB, type = "fe.cor")
## plot probability curve of fixed effects
sjp.glmer(modelNB, type = "ri.pc",
          show.se = FALSE,
          facet.grid = FALSE)
## Check the distribution of residuals in levels of factors
##plot(datStat$Sender:datStat$Receiver,residuals(modelNB))
## posterior predictive simulations to test whether the model is behaving like the data in other ways
sims <- simulate(modelNB,nsim=1000)
nOnes <- colSums(sims==1)
par(las=1,bty="l")
plot(pt <- prop.table(table(nOnes)),
     ylab="Probability")
(obsOne <- sum(datStat$Fight==1))
points(obsOne,0.02,col="red",pch=16,cex=2)

## Hence I need generate a Poisson model (already done) and compare their likelihood ratios
anova(modelNB, modelPoisson)

## OR more oldschool with a manual Loglikelihood comparison
X2 <- 2 * (logLik(modelNB) - logLik(modelPoisson))
# and then in the Chisquare Distribution look for the signifcance, with df as difference between the df of the two models
pchisq(X2, df = 1, lower.tail = FALSE)

## Compare the models
AICtab(modelPoisson, modelNB)
multiplot(modelPoisson,modelNB)

summary(modelPoisson)
plot(modelPoisson)
Anova(modelPoisson)
Anova(modelPoisson, type =3)
hist(resid(modelPoisson))

summary(modelNB)
plot(modelNB)
Anova(modelNB)
Anova(modelNB, type =3)
hist(resid(modelNB))
