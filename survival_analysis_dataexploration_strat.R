#SA HW1
#Blue3
#
# Meghan Weber
# Jonathan Rice
# Kevin McDowell
# Ragi Nayak
# Patrick Campbell

library(survival)
library(survminer)
library(haven)
library(ggsci)
library(FSA)
library(tidyverse)
library(ggplot2)

# Load Needed Data Sets #
setwd("C:/Users/Jonathan/Documents/Survival Analysis")
data = read_sas('hurricane.sas7bdat')

##Percentage of pumps that survived the hurricane
nrow(data %>% filter(survive == 1))/nrow(data)

##Percentage of pumps in each type of failure and average failure time for each failure type
data %>% group_by(reason) %>% summarize(n()/nrow(data))
data %>% group_by(reason) %>% summarize(mean(hour))
 
##ANOVA: if these averages for each type of failure are different, then tukey test for pairwise compairsons 
reason.lm <- lm(hour ~ factor(reason), data = data)
reason.av <- aov(reason.lm)
summary(reason.av)
tukey.test <- TukeyHSD(reason.av)
tukey.test

##Kruskal-Wallis: if these medians for each type of failure are different, then dunn test for pairwise comparisons
kruskal.test(hour ~ factor(reason), data = data)
dunnTest(hour ~ factor(reason), data=data, method="bh")

## Survival probability across time for all pumps together
pump_surv <- Surv(time = data$hour, event = data$survive == 0)

pump_km <- survfit(pump_surv ~ 1, data = data)
summary(pump_km)
plot(pump_km, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

g=ggsurvplot(pump_km, data = data, conf.int = TRUE, palette = '#1f78b4',
           xlab = "Hour", ylab = "Survival Probability", title = 'Survival Probability For All Pumps', legend = "none",
           break.y.by = 0.1)
g$plot + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=18))

##Survival probability across time for pumps broken down by failure type
# Stratified Analysis #

survdiff(pump_surv ~ reason, rho = 0, data = data)
data = data %>% filter(reason != 0)
pump_surv = Surv(time = data$hour, event = data$survive == 0)
pump_strat <- survfit(pump_surv ~ reason, data = data)
summary(pump_strat)
g = ggsurvplot(pump_strat, data = data, palette = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'),
           xlab = "Hour", ylab = "Survival Probability", break.y.by = 0.1, title='Survival Probability For Each Failure Type',
           legend.title = "Failure Type", legend.labs = c("Flood", 'Motor', 'Surge', 'Jammed'))
g$plot + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=18))

##Conditional failure probabilities across time for all pumps together
pump_km$hp <- pump_km$n.event/pump_km$n.risk
pump_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = pump_km$time, hp = pump_km$hp), by = "time", all = TRUE)
pump_haz[is.na(pump_haz) == TRUE] <- 0

plot(y = pump_haz$hp, x = pump_haz$time, main = "Hazard Probability Function", xlab = "Hour", ylab = "Hazard Probability",
     type = 'l')

g=ggplot(data=pump_haz, aes(x=time, y=hp)) + geom_line(size=1, color='#33a02c') + ggtitle("Hazard Probability Function For All Pumps") + labs(x = "Hour", y = "Hazard Probability")
g + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=18))
##Conditional failure probabilities across time for pumps broken down by failure type
# create flood hazard probability 
surv=data
flood <- surv[which(surv$reason == 1),]
flood_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = flood)
flood_km$hp <- flood_km$n.event/flood_km$n.risk
flood_haz <- merge(data.frame(time=seq(1,48,1)), data.frame(time = flood_km$time, hp = flood_km$hp), by = "time", all = TRUE)
flood_haz[is.na(flood_haz) == TRUE] <- 0

# create motor hazard probability 
motor <- surv[which(surv$reason == 2),]
motor_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = motor)
motor_km$hp <- motor_km$n.event/motor_km$n.risk
motor_haz <- merge(data.frame(time=seq(1,48,1)), data.frame(time = motor_km$time, hp = motor_km$hp), by = "time", all = TRUE)
motor_haz[is.na(motor_haz) == TRUE] <- 0

# create surge hazard probability 
surge <- surv[which(surv$reason == 3),]
surge_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = surge)
surge_km$hp <- surge_km$n.event/surge_km$n.risk
surge_haz <- merge(data.frame(time=seq(1,48,1)), data.frame(time = surge_km$time, hp = surge_km$hp), by = "time", all = TRUE)
surge_haz[is.na(surge_haz) == TRUE] <- 0

# create jam hazard probability 
jam <- surv[which(surv$reason == 4),]
jam_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = jam)
jam_km$hp <- jam_km$n.event/jam_km$n.risk
jam_haz <- merge(data.frame(time=seq(1,48,1)), data.frame(time = jam_km$time, hp = jam_km$hp), by = "time", all = TRUE)
jam_haz[is.na(jam_haz) == TRUE] <- 0


p <- ggplot(x='Hour') + 
  geom_line(data=flood_haz, aes(x=time,y=hp), color = '#a6cee3', size=1) +
  geom_line(data=motor_haz, aes(x=time, y=hp), color = '#1f78b4', size=1) + 
  geom_line(data=surge_haz, aes(x=time, y = hp), color= '#b2df8a', size=1) + 
  geom_line(data=jam_haz, aes(x=time,y=hp), color = '#33a02c', size=1) +
  ggtitle("Hazard Probability Function For Each Failure Type") + labs(x = "Hour", y = "Hazard Probability")

p + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=18))

# check pairwise comparisons of survival curves for each failure type
surv_cen <- surv[which(surv$reason != 0),]
pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 0, p.adjust.method = "bonferroni", data = surv_cen)
