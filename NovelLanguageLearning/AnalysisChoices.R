library(foreign)
library(lme4)
library(languageR)
library(relimp)
library(nlme)
#library(lmerTest)

#############
thedatachoices = read.csv("BothDaysAllRTResponses.csv",stringsAsFactors=F)
#thedatachoices = read.csv("BothDaysAllButtonResponses.csv",stringsAsFactors=F)
thedatachoices$subject <- as.factor(as.numeric(as.character(thedatachoices$subject)));
thedatachoices$item <- as.factor(as.character(thedatachoices$real_item));
thedatachoices$SP <- as.factor(as.numeric(as.character(thedatachoices$SPrep)));
thedatachoices$VP <- as.factor(as.numeric(as.character(thedatachoices$VPrep)));
#thedatachoices$position <- as.factor(as.numeric(as.character(thedatachoices$position)));
thedatachoices$structure <- as.factor(as.character(thedatachoices$structure));
thedatachoices$correct <- as.numeric(as.character(thedatachoices$correct));
thedatachoices$which_freq <- as.factor(as.character(thedatachoices$which_freq));
thedatachoices$which_freq_no <- as.factor(as.numeric(as.character(thedatachoices$which_freq_no)));
thedatachoices$which_freq_list <- as.factor(as.character(thedatachoices$which_freq_list));
thedatachoices$verb <- as.factor(as.character(thedatachoices$verb_2));
thedatachoices$verb_prime <- as.factor(as.character(thedatachoices$verb_prime));
thedatachoices$Day <- as.factor(as.numeric(as.character(thedatachoices$Day)));
#######FINAL CONDITION SPEC HELMERT CODING
contrasts(thedatachoices$SP)=cbind(".nSPvsSP"= c(-.5, .5))
contrasts(thedatachoices$VP)=cbind(".nVPvsVP"= c(-.5, .5))
contrasts(thedatachoices$Day)=cbind(".Day3vs9"= c(-.5, .5))
#contrasts(thedatachoices$which_freq)=contr.sum(3)
contrasts(thedatachoices$which_freq)<-
  cbind(".freq.vs.inf"= c(0,.5, -.5),
        ".old.vs.new"=c((2/3), -(1/3), -(1/3))
  )
contrasts(thedatachoices$which_freq_list)=cbind(".W1vsW2"= c(-.5, .5))

thedatachoicesEXNEW<-thedatachoices
levels(thedatachoicesEXNEW$which_freq)<-c("ex","new","new")
contrasts(thedatachoicesEXNEW$which_freq)=contr.sum(2)
thedatachoicesFreqInfreq <- subset(thedatachoices, which_freq=="freq" | which_freq=="infreq")
thedatachoicesFreq<- subset(thedatachoices, which_freq=="freq")
thedatachoicesInfreq <- subset(thedatachoices, which_freq=="infreq")
thedatachoicesEx <- subset(thedatachoices, which_freq=="ex")
thedatachoicesNEW <- subset(thedatachoicesEXNEW, which_freq=="new")
#thedatachoicesDay2 <- subset(thedatachoices, Day==0)
#thedatachoicesDay3 <- subset(thedatachoices, Day==1)
#######note use stargazer to make tables
###all conditions   MAX STRUCTURE THAT CONVERGES lowest variances removed first

thedataVerb<-thedatachoices
levels(thedataVerb$VP)<-c("nVP","VP")
thedataVP <- subset(thedataVerb, VP=="VP")
thedatanVP <- subset(thedataVerb, VP=="nVP")
thedata.lmer_vponly = glmer(correct~1+Day*SP*which_freq+(1+Day|subject)+(1+Day|item),data=thedataVP,family=binomial,control = glmerControl(optimizer="bobyqa"))
thedata.lmer_nvponly =glmer(correct~1+Day*SP*which_freq+(1+Day|subject)+(1+Day|item),data=thedatanVP,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(thedata.lmer_vponly)
summary(thedata.lmer_nvponly)

l1=glmer(correct~1+Day*VP*SP*which_freq+(1+Day+which_freq|subject)+(1+Day+which_freq|item),data=thedatachoices,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(l1)


###variance
lSP=glmer(correct~1+Day*VP*SP*which_freq+(1+SP|subject)+(1+SP|item),data=thedatachoices,family=binomial,control = glmerControl(optimizer="bobyqa"))
lVP=glmer(correct~1+Day*VP*SP*which_freq+(1+VP|subject)+(1+VP|item),data=thedatachoices,family=binomial,control = glmerControl(optimizer="bobyqa"))
print(VarCorr(lSP),comp="Variance")
print(VarCorr(lVP),comp="Variance")

lDay=glmer(correct~1+Day*VP*SP*which_freq+(1+Day|subject)+(1+Day|item),data=thedatachoices,family=binomial,control = glmerControl(optimizer="bobyqa"))
lwhichfreq=glmer(correct~1+Day*VP*SP*which_freq+(1+which_freq|subject)+(1+which_freq|item),data=thedatachoices,family=binomial,control = glmerControl(optimizer="bobyqa"))
print(VarCorr(lDay),comp="Variance")
print(VarCorr(lwhichfreq),comp="Variance")

####by which_freq
lfreq=glmer(correct~1+Day*VP*SP*+(1+Day|subject)+(1+Day|item),data=thedatachoicesFreq,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(lfreq)
linfreq=glmer(correct~1+Day*VP*SP*+(1+Day|subject)+(1|item),data=thedatachoicesInfreq,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(linfreq)
lex=glmer(correct~1+Day*VP*SP*+(1+Day|subject)+(1+Day|item),data=thedatachoicesEx,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(lex)
lnew=glmer(correct~1+Day*VP*SP+(1+Day|subject)+(1+Day|item),data=thedatachoicesNEW,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(lnew)

##comparisonEXNEW
#lexnew=glmer(correct~1+Day*VP*SP*which_freq+(1+SP+VP|subject)+(1|item),data=thedatachoicesEXNEW,family=binomial,control = glmerControl(optimizer="bobyqa"))
#summary(lexnew)


thedatachoicesNEWDay3 <- subset(thedatachoicesNEW, Day==0)
thedatachoicesNEWDay9 <- subset(thedatachoicesNEW, Day==1)

lnewDay3=glmer(correct~1+VP*SP+(1+SP+VP|subject)+(1|item),data=thedatachoicesNEWDay3,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(lnewDay3)

lnewDay9=glmer(correct~1+VP*SP+(1+SP+VP|subject)+(1|item),data=thedatachoicesNEWDay9,family=binomial,control = glmerControl(optimizer="bobyqa"))
summary(lnewDay9)


##################making the plots############################
##################making the plots############################
#perday

###verb by syntax

thedatachoices_day1 <- thedatachoices
thedatachoices_day1 <- subset(thedatachoices, Day==1)
library(plyr)
levels(thedatachoices_day1$which_freq)<-c("ex","freq","infreq")
levels(thedatachoices_day1$SP)<-c("noSP","SP")
levels(thedatachoices_day1$VP)<-c("noVP","VP")
thedata_persubject_proportion_day1<-aggregate(correct ~ SP +VP + subject, data=thedatachoices_day1, FUN=mean, na.rm=TRUE)
thedata_persubject_proportion_day1$f1f2<- interaction(thedata_persubject_proportion_day1$SP,thedata_persubject_proportion_day1$VP)
#thedata_persubject_proportion$which_condition_perfect_label<- factor(thedata_persubject_proportion$which_condition_perfect_label, c("100% noun", "50/50 noun verb", "100% verb"))
plot.mean <- function(x) {
  m <- mean(x)
  c(y = m, ymin = m, ymax = m)
}
library(scales) 
library(ggplot2)
#thedatachoices_day1$f1f2<- interaction(thedata_persubject_proportion_day1$which_freq, thedata_persubject_proportion_day1$SP)
datachoice_day1 <- summarySEwithin(thedata_persubject_proportion_day1, measurevar="correct", withinvars=c("f1f2"), idvar="subject")
datachoice_day1
p<-ggplot(thedata_persubject_proportion_day1, aes(x=f1f2, y=correct, group=subject)) +
  geom_point(size=3, alpha=.3, position=position_dodge(width=0.1),aes(group=f1f2,y=correct,colour=f1f2)) +
  geom_line(size=.5, alpha=.2, position=position_dodge(width=0.1) )+
  # stat_summary(aes(group=f1f2, colour=f1f2), fun.data="plot.mean", geom="bar", width=0.4, size=.8,alpha=0.1) +
  geom_bar(data=datachoice_day1,aes(group=f1f2,y=correct,colour=f1f2,fill=f1f2),
           stat = "identity",
           size=.6,    # Thinner lines
           width=.8,alpha=0.5) +  # width=.4,colour="black",fill="grey",alpha=0.1)
  geom_errorbar(data=datachoice_day1,aes(group=f1f2,ymin=correct-se, ymax=correct+se),
                size=.6,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,by=.25),labels=percent)+
  xlab('Condition') +
  ylab('Percentage Correct') +
  ###scale MPI colours
  scale_colour_manual(values=c("#000000", "#000000", "#000000", "#000000", "#E96A24", "#E96A24","#E96A24", "#E96A24", "#5D909B", "#5D909B", "#5D909B", "#5D909B")) + 
  scale_fill_manual(values=c("#000000", "white", "#000000", "white", "#000000", "#E96A24", "#5D909B", "white", "#000000", "#E96A24", "#5D909B", "white")) + 
  ###scale color blind friendly
  #  scale_colour_manual(values=c("#000000", "#000000", "#000000", "#000000", "#0072B2", "#0072B2","#0072B2", "#0072B2", "#D55E00", "#D55E00", "#D55E00", "#D55E00")) + 
  #  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#CC79A7")) + 
  
  
  #theme_bw())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black",lineend=1))

#cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p + coord_cartesian(ylim=c(.35,1))


p + scale_y_log10


###verb by syntax by structure

thedatachoices_day1 <- thedatachoices
thedatachoices_day1 <- subset(thedatachoices, Day==1)
library(plyr)
levels(thedatachoices_day1$which_freq)<-c("ex","freq","infreq")
levels(thedatachoices_day1$SP)<-c("noSP","SP")
levels(thedatachoices_day1$VP)<-c("noVP","VP")
thedata_persubject_proportion_day1<-aggregate(correct ~ SP +VP +which_freq+ subject, data=thedatachoices_day1, FUN=mean, na.rm=TRUE)
thedata_persubject_proportion_day1$f1f2<- interaction(thedata_persubject_proportion_day1$SP,thedata_persubject_proportion_day1$VP,thedata_persubject_proportion_day1$which_freq)
#thedata_persubject_proportion$which_condition_perfect_label<- factor(thedata_persubject_proportion$which_condition_perfect_label, c("100% noun", "50/50 noun verb", "100% verb"))
plot.mean <- function(x) {
  m <- mean(x)
  c(y = m, ymin = m, ymax = m)
}
library(scales) 
library(ggplot2)
#thedatachoices_day1$f1f2<- interaction(thedata_persubject_proportion_day1$which_freq, thedata_persubject_proportion_day1$SP)
datachoice_day1 <- summarySEwithin(thedata_persubject_proportion_day1, measurevar="correct", withinvars=c("f1f2"), idvar="subject")
datachoice_day1
p<-ggplot(thedata_persubject_proportion_day1, aes(x=f1f2, y=correct, group=subject)) +
 geom_point(size=3, alpha=.3, position=position_dodge(width=0.1),aes(group=f1f2,y=correct,colour=f1f2)) +
  geom_line(size=.5, alpha=.2, position=position_dodge(width=0.1) )+
 # stat_summary(aes(group=f1f2, colour=f1f2), fun.data="plot.mean", geom="bar", width=0.4, size=.8,alpha=0.1) +
  geom_bar(data=datachoice_day1,aes(group=f1f2,y=correct,colour=f1f2,fill=f1f2),
           stat = "identity",
           size=.6,    # Thinner lines
           width=.8,alpha=0.5) +  # width=.4,colour="black",fill="grey",alpha=0.1)
  geom_errorbar(data=datachoice_day1,aes(group=f1f2,ymin=correct-se, ymax=correct+se),
             size=.6,    # Thinner lines
               width=.2,
              position=position_dodge(.9)) +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,by=.25),labels=percent)+
  xlab('Condition') +
  ylab('Percentage Correct') +
 ###scale MPI colours
  scale_colour_manual(values=c("#000000", "#000000", "#000000", "#000000", "#E96A24", "#E96A24","#E96A24", "#E96A24", "#5D909B", "#5D909B", "#5D909B", "#5D909B")) + 
  scale_fill_manual(values=c("#000000", "#E96A24", "#5D909B", "white", "#000000", "#E96A24", "#5D909B", "white", "#000000", "#E96A24", "#5D909B", "white")) + 
  ###scale color blind friendly
 #  scale_colour_manual(values=c("#000000", "#000000", "#000000", "#000000", "#0072B2", "#0072B2","#0072B2", "#0072B2", "#D55E00", "#D55E00", "#D55E00", "#D55E00")) + 
  #  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#CC79A7")) + 
  
  
  #theme_bw())+
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black",lineend=1))

 #cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p + coord_cartesian(ylim=c(0,1))


p + scale_y_log10




l2=glmer(correct~1+VP*VDay9c+(1+VP|subject)+(1|item),data=thedatachoices,family=binomial)
summary(l2)

l2=glmer(correct~1+SP*PicDay9c+(1+SP|subject)+(1|item),data=thedatachoices,family=binomial)
summary(l2)

datachoice <- summarySEwithin(thedatachoices, measurevar="correct", withinvars=c("which_freq","VP","SP"), idvar="subject")
datachoice



library(plyr)
total_values= ddply(thedatachoices,
                    .(Day),
                    function(df) data.frame(
                      Correct.mean=mean(df$correct),
                      Correct.sd=sd(df$correct),
                      Correct.count=length(df$correct),
                      Correct.se=sd(df$correct)/sqrt(length(df$correct))
                    )
)
total_values
