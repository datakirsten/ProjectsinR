####Reaction Time Analysis for Day 2 of the experiment, mixed effects model with random effects for items and subjects

#import relevant packages
#library(foreign)
library(lme4)
library(languageR)
#library(relimp)
library(lmerTest)
library(ggplot2)
library(languageR)
#############

#############
###input is a CSV file with Reaction Times (Speech Onset Times) per subject and item
####the factors/dependent variables are
####subject (the participants in the experiment), item (the individual sentence items a subject sees), SP(syntactic repetition yes/no), VP(verb reptition yes/no),structure (3 word orders:VOS,OSV,SVO), which_freq (whether VOS or OSV was the frequent structure),total_time (RT offset), onset (RT onset), duraction(RT between onset and offset),order(which list order)
thedataday2 = read.csv("Day2OnsetsFinal26Sub.csv",stringsAsFactors=F)
thedataday2$subject <- as.factor(as.numeric(as.character(thedataday2$subject)));
thedataday2$item <- as.factor(as.character(thedataday2$real_item));
thedataday2$SP <- as.factor(as.numeric(as.character(thedataday2$SP)));
thedataday2$VP <- as.factor(as.numeric(as.character(thedataday2$VP)));
thedataday2$structure <- as.factor(as.character(thedataday2$structure));
thedataday2$which_freq_no <- as.factor(as.character(thedataday2$freq_condition));
thedataday2$which_freq<- as.factor(as.character(thedataday2$which_freq));
thedataday2$which_freq_group <- as.factor(as.character(thedataday2$which_freq_group));
thedataday2$total_time <- as.numeric(as.character(thedataday2$offset));
thedataday2$onset <- as.numeric(as.character(thedataday2$onset_sec));
thedataday2$duration <- as.numeric(as.character(thedataday2$duration));
#thedataday2$VP_RT <- as.factor(as.numeric(as.character(thedataday2$VP_RT)));
#thedataday2$Day <- as.factor(as.numeric(as.character(thedataday2$Day)));
#thedataday2$frequent_structure<- as.factor(as.numeric(as.character(thedataday2$freq_condition)));
thedataday2$order <- as.numeric(as.character(thedataday2$order));


as.data.frame(thedataday2)

###contrast coding for our conditions of interest, Helmert Coding is used
contrasts(thedataday2$SP)=contr.helmert(2)
contrasts(thedataday2$VP)=contr.helmert(2)
contrasts(thedataday2$which_freq_group)=contr.helmert(2)
contrasts(thedataday2$which_freq)<-
  cbind("freq.vs.inf"= c(0,.5, -.5),
        "old.vs.new"=c((2/3), -(1/3), -(1/3))
  )

###max model that reaches convergence for analysis total reaction time
###interaction between SP,VP and which_freq, with random effects for subjects and items but no random slopes
thedataday2.lmer_total = lmer(total_time ~ (SP*VP*which_freq)+ (1|subject) + (1|item), data = thedataday2)
summary(thedataday2.lmer_total)


###split data by level of verb repetition (yes/no) and do the analysis for each split dataset (interaction between SP and which_freq)
thedataVerb<-thedataday2
levels(thedataVerb$VP)<-c("nVP","VP")
thedataVP <- subset(thedataVerb, VP=="VP")
thedatanVP <- subset(thedataVerb, VP=="nVP")
thedata.lmer_vponly = lmer(total_time ~ (SP*which_freq) + (1|subject) +  (1|item), data = thedataVP)
thedata.lmer_nvponly = lmer(total_time ~ (SP*which_freq) + (1|subject) + (1|item), data = thedatanVP)
summary(thedata.lmer_vponly)
summary(thedata.lmer_nvponly)



thedataday2.lmerSP = lmer(total_time ~ (SP*VP*which_freq)+ (1+SP|subject) + (1+SP|item), data = thedataday2)
print(VarCorr(thedataday2.lmerSP),comp="Variance")
thedataday2.lmerVP = lmer(total_time ~ (SP*VP*which_freq)+ (1+VP|subject) + (1+VP|item), data = thedataday2)
print(VarCorr(thedataday2.lmerVP),comp="Variance")
thedataday2.lmerwhich_freq = lmer(total_time ~ (SP*VP*which_freq)+ (1+which_freq|subject) + (1+which_freq|item), data = thedataday2)
print(VarCorr(thedataday2.lmerwhich_freq),comp="Variance")


####provide means, sds etc for different condition combinations
library(plyr)
total_values1= ddply(thedataday2,
                    .(which_freq_group,which_freq,VP,SP),
                    function(df) data.frame(
                      Correct.mean=mean(df$total_time),
                      Correct.sd=sd(df$total_time),
                      Correct.count=length(df$total_time),
                      Correct.se=sd(df$total_time)/sqrt(length(df$total_time))
                    )
)
total_values1

total_values2= ddply(thedataday2,
                    .(VP,structure),
                    function(df) data.frame(
                      Correct.mean=mean(df$total_time),
                      Correct.sd=sd(df$total_time),
                      Correct.count=length(df$total_time),
                      Correct.se=sd(df$total_time)/sqrt(length(df$total_time))
                    )
)
total_values2


#dotdata$condition<- factor(dotdata$condition, as.character(dotdata$condition))
# re-order the levels in the order of appearance in the dataframe
# otherwise it plots it in alphabetical order

#############MAKE PLOT#########plot using ggplot

##get data into plottable form, get SE for within subject effects
levels(thedataday2$which_freq)<-c("ex","freq","infreq")
levels(thedataday2$SP)<-c("noSP","SP")
levels(thedataday2$VP)<-c("noVP","VP")
thedata_persubject_total_time<-aggregate(total_time ~ SP +VP +which_freq+ subject, data=thedataday2, FUN=mean, na.rm=TRUE)
thedata_persubject_total_time$f1f2<- interaction(thedata_persubject_total_time$SP,thedata_persubject_total_time$VP,thedata_persubject_total_time$which_freq)
datachoice_total_time <- summarySEwithin(thedata_persubject_total_time, measurevar="total_time", withinvars=c("f1f2"), idvar="subject")

###barplot with individual datapoints shown in the background
p<-ggplot(thedata_persubject_total_time, aes(x=f1f2, y=total_time, group=subject)) +
  geom_point(size=4.5, alpha=.6, position=position_dodge(width=0.1),aes(group=f1f2,y=total_time,colour=f1f2)) +
  geom_line(size=1, alpha=.3, position=position_dodge(width=0.1) )+
  # stat_summary(aes(group=f1f2, colour=f1f2), fun.data="plot.mean", geom="bar", width=0.4, size=.8,alpha=0.1) +
  geom_bar(data=datachoice_total_time,aes(group=f1f2,y=total_time,colour=f1f2,fill=f1f2),
           stat = "identity",
           size=.6,    # Thinner lines
           width=.8,alpha=0.5) +  # width=.4,colour="black",fill="grey",alpha=0.1)
  geom_errorbar(data=datachoice_total_time,aes(group=f1f2,ymin=total_time-se, ymax=total_time+se),
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

p  + coord_cartesian(ylim=c(2,4))




###############summarysewithinfunction:


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
tgc <- summarySE(thedataday2, measurevar="RT", groupvars=c("SP"))
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
datac <- summarySEwithin(thedataday2, measurevar="duration", withinvars=c("SP","frequent_structure"), idvar="subject")
datac



