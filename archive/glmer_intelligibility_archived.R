# Load packages and define some functions

library(car)
library(dplyr)
library(tidyr)

library(ggplot2)
library(ggsci)
library(ggeffects)
#library(sjPlot)
library(forcats)

library(lme4)
library(lmerTest)

library(quickpsy)


library(mgcv)
library(mgcViz)
library(itsadug)

# Data wrangling

## Load and clean the raw data:

### Exp 1:

dat_exp1<-read.csv("Exp2_intelligibility-final.csv")
dat_exp1$total_word<-5
dat_exp1$SentenceID<-as.factor(dat_exp1$SentenceID)
dat_exp1$Voice<-as.factor(dat_exp1$Voice)

dat_exp1_noise_reorder<-c('-6dB','-3dB','0dB','3dB') # reorder the levels for plotting

dat_exp1 <- dat_exp1 %>%
  drop_na(WordsCorrect) %>%
  mutate(Noise = fct_relevel(Noise, dat_exp1_noise_reorder)) # reorder the raw data per level of task

dat_exp1$Accuracy2<-dat_exp1$WordsCorrect/dat_exp1$total_word

#### Prepare spreadsheet for plot:

library(Rmisc)
intel_aggregated_exp1 <- summarySE(dat_exp1, measurevar="Accuracy2", groupvars=c("Participant","Type","Noise"))
intel_aggregated_exp1_agg <- summarySE(intel_aggregated_exp1, measurevar="Accuracy2", groupvars=c("Type","Noise"))
detach("package:Rmisc", unload=TRUE)

library(Rmisc)
intel_aggregated_exp1_nopar <- summarySE(dat_exp1, measurevar="Accuracy", groupvars=c("Type","Noise"))
detach("package:Rmisc", unload=TRUE)

#library(Rmisc)
#intel_aggregated_exp1_bypar <- summarySE(dat_exp1, measurevar="Accuracy2", groupvars=c("Participant"))
#detach("package:Rmisc", unload=TRUE)

### Exp 2:

dat_exp2<-read.csv("Exp2_intelligibility-final.csv")
dat_exp2$total_word<-5
dat_exp2$SentenceID<-as.factor(dat_exp2$SentenceID)
dat_exp2$Voice<-as.factor(dat_exp2$Voice)

dat_exp2_noise_reorder<-c('-6dB','-3dB','0dB','3dB') # reorder the levels for plotting

dat_exp2 <- dat_exp2 %>%
  drop_na(IndividualWordsCorrect) %>%
  mutate(Noise = fct_relevel(Noise, dat_exp2_noise_reorder)) # reorder the raw data per level of task

dat_exp2$Accuracy2<-dat_exp2$IndividualWordsCorrect/dat_exp2$total_word

### Prepare spreadsheet for plot:

library(Rmisc)
intel_aggregated_exp2 <- summarySE(dat_exp2, measurevar="Accuracy2", groupvars=c("Participant","Type","Noise"))
intel_aggregated_exp2_agg <- summarySE(intel_aggregated_exp2, measurevar="Accuracy2", groupvars=c("Type","Noise"))
detach("package:Rmisc", unload=TRUE)

library(Rmisc)
intel_aggregated_exp2_nopar <- summarySE(dat_exp2, measurevar="Accuracy2", groupvars=c("Type","Noise"))
detach("package:Rmisc", unload=TRUE)


# Models:

## Exp 1:
### the saturated model:

m_exp1_full <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type*Noise+(1+Type*Noise|Participant)+(1+Type*Noise|SentenceID)+(1+Type*Noise|Voice),
                       data=dat_exp1, family = binomial(link = "logit"), 
                       control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=60e5)))
summary(m_exp1_full)

### NB: No model will converge for Experiment 1 so we will have to go with experiment 2 for now and add new participants.

m_exp1_1 <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type*Noise+(1|Voice),
                data=dat_exp1, family = binomial(link = "logit"), 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=60e5)))
summary(m_exp1_1)


### Plots:

#### Model predictions:
gg_m_exp1_1<-ggpredict(m_exp1_1, terms = c("Noise","Type")) # save the prediction from the model using ggpredict()
plot(gg_m_exp1_1) # plot the model


#### Raw data:


plot_speech_accuracy_exp1 <-ggplot(intel_aggregated_exp1, aes(x=Noise, y=Accuracy2,fill=Type)) +
  geom_boxplot(outlier.shape=NA)+
  stat_summary(aes(group=Type,fill = Type),fun = "mean", geom = "point", shape = 23, size = 3, fill = "grey",position = position_dodge(0.75)) +
  geom_point(aes(fill=Type,group=Type),size=2,shape=21, position = position_jitterdodge(jitter.width = 0.25), alpha = 0.5)+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
  scale_fill_manual(name = "Voice Type",values = c("light pink","light blue"))+
  coord_cartesian(ylim = c(0, 1))+
  labs(x="Noise level", y = "Accuracy")+
  theme_minimal()

plot_speech_accuracy_exp1+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))



## Exp 2:
### the saturated model:

m_exp2_full <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type*Noise+(1+Type*Noise|Participant)+(1+Type*Noise|SentenceID)+(1+Type*Noise|Voice),
                     data=dat_exp2, family = binomial(link = "logit"), 
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=60e5)))
summary(m_exp2_full)

### the best model is m_exp2_1:

m_exp2_1 <- glmer(cbind(IndividualWordsCorrect,total_word-IndividualWordsCorrect)~1+Type*Noise+(1+Type|Voice),
                  data=dat_exp2, family = binomial(link = "logit"), 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=60e5)))
summary(m_exp2_1)


m_exp2_2 <- glmer(cbind(IndividualWordsCorrect,total_word-IndividualWordsCorrect)~1+Type*Noise+(1|Voice),
                  data=dat_exp2, family = binomial(link = "logit"), 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=60e5)))
summary(m_exp2_2)

anova(m_exp2_1,m_exp2_2)


### Plots:

#### Model predictions:

gg_m_exp2_1<-ggpredict(m_exp2_1, terms = c("Noise","Type")) # save the prediction from the model using ggpredict()
plot(gg_m_exp2_1) # plot the model


#### Raw data:

plot_speech_accuracy_exp2 <-ggplot(intel_aggregated_exp2, aes(x=Noise, y=Accuracy2,fill=Type)) +
  geom_boxplot(outlier.shape=NA)+
  stat_summary(aes(group=Type,fill = Type),fun = "mean", geom = "point", shape = 23, size = 3, fill = "grey",position = position_dodge(0.75)) +
  geom_point(aes(fill=Type,group=Type),size=2,shape=21, position = position_jitterdodge(jitter.width = 0.25), alpha = 0.5)+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
  scale_fill_manual(name = "Voice Type",values = c("light pink","light blue"))+
  coord_cartesian(ylim = c(0, 1))+
  labs(x="Noise level", y = "Accuracy")+
  theme_minimal()

plot_speech_accuracy_exp2+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))

## refitted model with different baselines to look at the effect of interaction:

m_1_refn3db <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type*relevel(as.factor(Noise), ref="-3dB")+(1|Participant)+(1|SentenceID),
             data=dat_exp1, family = binomial(link = "logit"), 
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=40e5)))
summary(m_1_refn3db)


m_1_ref0db <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type*relevel(as.factor(Noise), ref="0dB")+(1|Participant)+(1|SentenceID),
                     data=dat_exp1, family = binomial(link = "logit"), 
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=40e5)))
summary(m_1_ref0db)



m_1_refn3db <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+relevel(as.factor(Type), ref="human")*relevel(as.factor(Noise), ref="-3dB")+(1|Participant)+(1|SentenceID),
                     data=dat_exp1, family = binomial(link = "logit"), 
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=40e5)))
summary(m_1_refn3db)

## model refitted with afex for straight-forward pairwise comparisons

library(afex)
library(emmeans)
m_1_afex<-(mixed(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type*Noise+(1|Participant)+(1|SentenceID),
                    data=dat_exp1,method = "LRT",family = binomial(link = "logit"),
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=40e5))))
summary(m_1_afex)

(emm_m_1_afex<-emmeans(m_1_afex,"Type","Noise")) # estimation for task-wise performance using library(afex)
pairs(emm_m_1_afex,adjust="holm") # pair-wise comparison results for task conditions

detach("package:afex", unload=TRUE)


## model without the interaction term: BF10 = 432

m_2 <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type+Noise+(1|Participant)+(1|SentenceID),
             data=dat_exp1, family = binomial(link = "logit"), 
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=40e5)))
summary(m_2)
anova(m_1,m_2)

BF01_m1_m2<-exp((BIC(m_2) - BIC(m_1))/2)
BF10_m1_m2<-1/BF01_m1_m2

## model without the Type term: BF10 = INFINITY

m_3 <- glmer(cbind(WordsCorrect,total_word-WordsCorrect)~1+Type+(1|Participant)+(1|SentenceID),
             data=dat_exp1, family = binomial(link = "logit"), 
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=40e5)))

anova(m_2,m_3)

BF01_m2_m3<-exp((BIC(m_2) - BIC(m_3))/2)
BF10_m2_m3<-1/BF01_m2_m3
BF10_m2_m3







