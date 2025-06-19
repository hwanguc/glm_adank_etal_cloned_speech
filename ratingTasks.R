# Author: Han Wang

### Date: 19 June 2025
### This script contains models for analysing the accent and clarity ratings, as well as the ABX task.

# Load packages and define some functions

library(car)
library(dplyr)
library(tidyr)
library(tibble)

library(ggplot2)
library(ggsci)
library(ggeffects)
library(forcats)

library(lme4)
library(lmerTest)
library(ordinal)

library(quickpsy)


library(mgcv)
library(mgcViz)
library(itsadug)


# Data wrangling

## Load and clean the raw data:

### accent rating:

dat_exp2_accentRating<-read.csv("Exp2_accent-final.csv")
dat_exp2_accentRating$Voice<-as.factor(dat_exp2_accentRating$Voice)
dat_exp2_accentRating$Response<-as.factor(dat_exp2_accentRating$Response)
dat_exp2_accentRating$ResponseNum<-as.numeric(dat_exp2_accentRating$Response)

#### spreadsheet for plot:

library(Rmisc)
accent_aggregated_exp2 <- summarySE(dat_exp2_accentRating, measurevar="ResponseNum", groupvars=c("Participant","Type"))
detach("package:Rmisc", unload=TRUE)


### clarity rating:

dat_exp2_clarityRating<-read.csv("Exp2_clarity-final.csv")
dat_exp2_clarityRating$Voice<-as.factor(dat_exp2_clarityRating$Voice)
dat_exp2_clarityRating$Response<-as.factor(dat_exp2_clarityRating$Response)
dat_exp2_clarityRating$ResponseNum<-as.numeric(dat_exp2_clarityRating$Response)

#### spreadsheet for plot:

library(Rmisc)
clarity_aggregated_exp2 <- summarySE(dat_exp2_clarityRating, measurevar="ResponseNum", groupvars=c("Participant","Type"))
detach("package:Rmisc", unload=TRUE)



### ABX task

dat_exp2_abx<-read.csv("Exp2_abx-final.csv")

#### Generate the true label
dat_exp2_abx <- dat_exp2_abx %>%
  mutate(VoiceType = case_when(
    Response == "First" & Correct == 1 ~ wav1_type,
    Response == "First" & Correct == 0 ~ wav2_type,
    Response == "Second" & Correct == 1 ~ wav2_type,
    Response == "Second" & Correct == 0 ~ wav1_type
  ))

# Models and Plots:

### accent rating:

m_exp2_accent_1<-clmm2(Response ~ Type, random = Voice, data = dat_exp2_accentRating, Hess = TRUE, nAGQ = 10)
summary(m_exp2_accent_1)

#### Plot (accent rating, raw data):

plot_accentRating_exp2 <-ggplot(accent_aggregated_exp2, aes(x=Type, y=ResponseNum,fill=Type)) +
  geom_boxplot(outlier.shape=NA)+
  stat_summary(aes(group=Type,fill = Type),fun = "mean", geom = "point", shape = 23, size = 3, fill = "grey",position = position_dodge(0.75)) +
  geom_point(aes(fill=Type,group=Type),size=2,shape=21, position = position_jitterdodge(jitter.width = 0.25), alpha = 0.5)+
  scale_y_continuous(breaks = seq(0, 7, by = 0.5))+
  scale_fill_manual(name = "Voice Type",values = c("light yellow","light green"))+
  coord_cartesian(ylim = c(0, 7))+
  labs(x="Voice Type", y = "Rating (Accent)")+
  theme_minimal()

plot_accentRating_exp2+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.position = "none")


### clarity rating:

m_exp2_clarity_1<-clmm2(Response ~ Type, random = Voice, data = dat_exp2_clarityRating, Hess = TRUE, nAGQ = 10)
summary(m_exp2_clarity_1)

#### Plot (clarity rating, raw data):

plot_clarityRating_exp2 <-ggplot(clarity_aggregated_exp2, aes(x=Type, y=ResponseNum,fill=Type)) +
  geom_boxplot(outlier.shape=NA)+
  stat_summary(aes(group=Type,fill = Type),fun = "mean", geom = "point", shape = 23, size = 3, fill = "grey",position = position_dodge(0.75)) +
  geom_point(aes(fill=Type,group=Type),size=2,shape=21, position = position_jitterdodge(jitter.width = 0.25), alpha = 0.5)+
  scale_y_continuous(breaks = seq(0, 7, by = 0.5))+
  scale_fill_manual(name = "Voice Type",values = c("light yellow","light green"))+
  coord_cartesian(ylim = c(0, 7))+
  labs(x="Voice Type", y = "Rating (Clarity)")+
  theme_minimal()

plot_clarityRating_exp2+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.position = "none")

