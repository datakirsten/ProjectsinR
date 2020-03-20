#library(foreign)
library(lme4)
library(languageR)
#library(relimp)
library(lmerTest)
library(ggplot2)
#############

#############
########use as initial step data from all 3 days
thedata = read.csv("3DaysAllRTResponses_correctonly.csv",stringsAsFactors=F)
thedata$subject <- as.factor(as.numeric(as.character(thedata$subject)));
thedata$item <- as.factor(as.character(thedata$real_item));
thedata$SP <- as.factor(as.numeric(as.character(thedata$SPrep)));
thedata$VP <- as.factor(as.numeric(as.character(thedata$VPrep)));
thedata$structure <- as.factor(as.character(thedata$structure));
thedata$which_freq <- as.factor(as.character(thedata$which_freq));
thedata$which_freq_group <- as.factor(as.character(thedata$which_freq_list));
thedata$total_time <- as.numeric(as.character(thedata$offset_sec));
thedata$onset <- as.numeric(as.character(thedata$onset_sec));
thedata$duration <- as.numeric(as.character(thedata$duration));
thedata$Day <- as.factor(as.numeric(thedata$Day));
#thedata$frequent_structure<- as.factor(as.numeric(as.character(thedata$freq_condition)));
#thedata$order <- as.numeric(as.character(thedata$order));
as.data.frame(thedata)
#thedata<- subset(thedata, Day==0 | Day==1)
#thedatachoicesfreqinfreq$which_freq<-relevel(thedatachoicesfreqinfreq$which_freq,"freq")
thedata$Day<-relevel(thedata$Day,"3")
contrasts(thedata$SP)=cbind("nSPvsSP"= c(-.5, .5))
contrasts(thedata$VP)=cbind("nVPvsVP"= c(-.5, .5))
contrasts(thedata$which_freq_group)=cbind("W1vsW2"= c(-.5, .5))
contrasts(thedata$Day)<-cbind("Day2vs3"= c(0,.5, -.5),
      "Day1vs23"=c((2/3), -(1/3), -(1/3))  )
contrasts(thedata$which_freq)<-
  cbind("freq.vs.inf"= c(0,.5, -.5),
        "known.vs.new"=c((2/3), -(1/3), -(1/3))
  )


thedataVerb<-thedata
levels(thedataVerb$VP)<-c("nVP","VP")
thedataVP <- subset(thedataVerb, VP=="VP")
thedatanVP <- subset(thedataVerb, VP=="nVP")
thedata.lmer_vponly = lmer(total_time ~ (Day*SP*which_freq) + (1+Day+which_freq|subject) + (1+which_freq+Day|item), data = thedataVP)
thedata.lmer_nvponly = lmer(total_time ~ (Day*SP*which_freq) + (1+Day+which_freq|subject) + (1+which_freq+Day|item), data = thedatanVP)
summary(thedata.lmer_vponly)
summary(thedata.lmer_nvponly)



thedataEXNEW<-thedata
levels(thedataEXNEW$which_freq)<-c("ex","new","new")
#contrasts(thedataEXNEW$which_freq)=contr.sum(2)
#thedataFreqInfreq <- subset(thedata, which_freq=="freq" | which_freq=="infreq")
#thedataFreq<- subset(thedata, which_freq=="freq")
#thedataInfreq <- subset(thedata, which_freq=="infreq")
thedataEx <- subset(thedata, which_freq=="ex")
thedataNEW <- subset(thedataEXNEW, which_freq=="new")
#thedataDay2 <- subset(thedata, Day==0)
#thedataDay3 <- subset(thedata, Day==1)

#####all conditions
#thedata.lmer_total = lmer(total_time ~ (Day*SP*VP*which_freq*which_freq_group) + (1+SP+VP|which_freq_group/subject) + (1|item), data = thedata)
###aldays  
#####max model that converges
thedata.lmer_total = lmer(total_time ~ (Day*SP*VP*which_freq) + (1+Day+which_freq|subject) + (1+which_freq+Day|item), data = thedata)

summary(thedata.lmer_total)

###variance
thedata.lDay = lmer(total_time ~ (Day*SP*VP*which_freq) + (1+Day|subject) + (1+Day|item), data = thedata)
print(VarCorr(thedata.lDay),comp="Variance")
thedata.lSP = lmer(total_time ~ (Day*SP*VP*which_freq) + (1+SP|subject) + (1+SP|item), data = thedata)
print(VarCorr(thedata.lSP),comp="Variance")
thedata.lVP = lmer(total_time ~ (Day*SP*VP*which_freq) + (1+VP|subject) + (1+VP|item), data = thedata)
print(VarCorr(thedata.lVP),comp="Variance")
thedata.lwhich_freq = lmer(total_time ~ (Day*SP*VP*which_freq) + (1+which_freq|subject) + (1+which_freq|item), data = thedata)
print(VarCorr(thedata.lwhich_freq),comp="Variance")

####by which_freq
thedata.lfreq = lmer(total_time ~ (Day*SP*VP) + (1+Day+SP+VP|subject) + (1+SP|item), data = thedataFreq)
summary(thedata.lfreq)
thedata.linfreq = lmer(total_time ~ (Day*SP*VP) + (1+Day+SP+VP|subject) + (1+SP|item), data = thedataInfreq)
summary(thedata.linfreq)
thedata.lex = lmer(total_time ~ (Day*SP*VP) + (1+Day+SP+VP|subject) + (1+SP|item), data = thedataEx)
summary(thedata.lex)

##comparisonEXNEW
thedata.lexnew = lmer(total_time ~ (Day*SP*VP*which_freq) + (1+Day+SP+VP+which_freq|subject) + (1+SP|item), data = thedataEXNEW)
summary(thedata.lexnew)

thedata.lnew = lmer(total_time ~ (Day*SP*VP) + (1+Day|subject) + (1+Day|item), data = thedataNEW)
summary(thedata.lnew)

##comparionsFreqInfreq
lfreqinfreq=lmer(total_time ~ (Day*SP*VP*which_freq) + (1+Day+SP+VP+which_freq|subject) + (1+SP|item), data = thedataFreqInfreq)
summary(lfreqinfreq)


######Day 1
thedata.lmer_totalday1 = lmer(total_time ~ (which_freq*SP*VP) + (1+VP|subject) + (1+VP|item), data = thedata_day1)
summary(thedata.lmer_totalday1)

###########make plots data syntax by verb per day
thedata_day1 <- subset(thedata, Day==1)
library(plyr)
levels(thedata_day1$which_freq)<-c("ex","freq","infreq")
levels(thedata_day1$VP)<-c("noVP","VP")
levels(thedata_day1$SP)<-c("noSP","SP")
thedata_persubject_proportion_day1<-aggregate(total_time ~ SP +VP+ subject, data=thedata_day1, FUN=mean, na.rm=TRUE)
thedata_persubject_proportion_day1$f1f2<- interaction(thedata_persubject_proportion_day1$SP, thedata_persubject_proportion_day1$VP)


#thedata_persubject_proportion$which_condition_perfect_label<- factor(thedata_persubject_proportion$which_condition_perfect_label, c("100% noun", "50/50 noun verb", "100% verb"))
plot.mean <- function(x) {
  m <- mean(x)
  c(y = m, ymin=m,ymax = m)
}
library(scales) 

datasummarywithin_day1 <- summarySEwithin(thedata_persubject_proportion_day1, measurevar="total_time", withinvars=c("f1f2"), idvar="subject")

p<-ggplot(thedata_persubject_proportion_day1, aes(x=f1f2, y=total_time, group=subject)) +
  geom_point(size=3, alpha=.3, position=position_dodge(width=0.1),aes(group=f1f2,y=total_time,colour=f1f2)) +
  geom_line(size=.5, alpha=.2, position=position_dodge(width=0.1) )+
  # stat_summary(aes(group=f1f2, colour=f1f2), fun.data="plot.mean", geom="bar", width=0.4, size=.8,alpha=0.1) +
  geom_bar(data=datasummarywithin_day1,aes(group=f1f2,y=total_time,colour=f1f2,fill=f1f2),
           stat = "identity",
           size=.6,    # Thinner lines
           width=.8,alpha=0.5) +  # width=.4,colour="black",fill="grey",alpha=0.1)
  geom_errorbar(data=datasummarywithin_day1,aes(group=f1f2,ymin=total_time-se, ymax=total_time+se),
                size=.6,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(limits=c(0, 4), breaks=seq(0,4,by=.5),labels=waiver())+
  xlab('Condition') +
  ylab('Seconds') +
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

p  + coord_cartesian(ylim=c(2,3.6))



#######make plots data syntax by verb by day by structure
thedata_day1 <- subset(thedata, Day==1)
library(plyr)
levels(thedata_day1$which_freq)<-c("ex","freq","infreq")
levels(thedata_day1$VP)<-c("noVP","VP")
levels(thedata_day1$SP)<-c("noSP","SP")
thedata_persubject_proportion_day1<-aggregate(total_time ~ SP +VP+which_freq+ subject, data=thedata_day1, FUN=mean, na.rm=TRUE)
thedata_persubject_proportion_day1$f1f2<- interaction(thedata_persubject_proportion_day1$SP, thedata_persubject_proportion_day1$VP, thedata_persubject_proportion_day1$which_freq)


#thedata_persubject_proportion$which_condition_perfect_label<- factor(thedata_persubject_proportion$which_condition_perfect_label, c("100% noun", "50/50 noun verb", "100% verb"))
plot.mean <- function(x) {
  m <- mean(x)
  c(y = m, ymin=m,ymax = m)
}
library(scales) 

datasummarywithin_day1 <- summarySEwithin(thedata_persubject_proportion_day1, measurevar="total_time", withinvars=c("f1f2"), idvar="subject")

p<-ggplot(thedata_persubject_proportion_day1, aes(x=f1f2, y=total_time, group=subject)) +
  geom_point(size=3, alpha=.3, position=position_dodge(width=0.1),aes(group=f1f2,y=total_time,colour=f1f2)) +
  geom_line(size=.5, alpha=.2, position=position_dodge(width=0.1) )+
  # stat_summary(aes(group=f1f2, colour=f1f2), fun.data="plot.mean", geom="bar", width=0.4, size=.8,alpha=0.1) +
  geom_bar(data=datasummarywithin_day1,aes(group=f1f2,y=total_time,colour=f1f2,fill=f1f2),
           stat = "identity",
           size=.6,    # Thinner lines
           width=.8,alpha=0.5) +  # width=.4,colour="black",fill="grey",alpha=0.1)
  geom_errorbar(data=datasummarywithin_day1,aes(group=f1f2,ymin=total_time-se, ymax=total_time+se),
                size=.6,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(limits=c(0, 4), breaks=seq(0,4,by=1),labels=waiver())+
  xlab('Condition') +
  ylab('Seconds') +
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

p  + coord_cartesian(ylim=c(1.75,3.25))




library(plyr)
total_values= ddply(thedata,
                    .(which_freq),
                    function(df) data.frame(
                      Correct.mean=mean(df$total_time),
                      Correct.sd=sd(df$total_time),
                      Correct.count=length(df$total_time),
                      Correct.se=sd(df$total_time)/sqrt(length(df$total_time))
                    )
)
total_values



summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
tgc <- summarySE(thedata, measurevar="RT", groupvars=c("SP"))
library(ggplot2)
ggplot(tgc, aes(x=SP, y=RT)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


###summarySE within helper function
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


###sde for within variables
datac <- summarySEwithin(thedata, measurevar="duration", withinvars=c("SP","frequent_structure"), idvar="subject")
datac









#dotdata$condition<- factor(dotdata$condition, as.character(dotdata$condition))
# re-order the levels in the order of appearance in the dataframe
# otherwise it plots it in alphabetical order
thedata_totaltime<-aggregate(total_time ~ VP + subject, data=thedata, FUN=mean, na.rm=TRUE)

plot.mean <- function(x) {
  m <- mean(x)
  c(y = m, ymin = m, ymax = m)
}
ggplot(thedata_totaltime, aes(x=VP , y=total_time, group=subject)) +
  geom_point(aes(colour=VP ), size=4.5, alpha=.3, position=position_dodge(width=0.1)) +
  geom_line(size=1, alpha=.2, position=position_dodge(width=0.1)) +
  stat_summary(aes(group=VP , colour=VP ), fun.data="plot.mean", geom="bar", width=0.4, size=2,alpha=0.1) +
  stat_summary(aes(group=VP, colour=VP), geom="errorbar", width=0.1, size=1,alpha=1) +
  scale_y_continuous(limits=c(0, 4), breaks=seq(0,4,by=.5))+
  xlab('Condition') +
  ylab('Proportion PO') +
  scale_colour_manual(values=c("#E96A24", "#000000"), guide=FALSE) + 
  theme_bw()