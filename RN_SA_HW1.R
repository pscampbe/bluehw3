#install.packages("survival")
#install.packages("survminer")

library(haven)
library(dplyr)
library(survival)
library(survminer)

# load data
hurricane <- read_sas("Documents/MSA/FALL_3/Survival_Analysis/Homework1_SA/hurricane.sas7bdat", 
                      +     NULL)

summary(hurricane)
str(hurricane)

# adding creating factor + adding labels for ease of viewing
hurricane$reason <- factor(hurricane$reason, 
                           levels = c(0,1,2,3,4),
                           labels = c("No Failure", "Flood Failure", "Motor Failure", "Surge Failure","Jammed Failure"))

# Percentage of pumps that survived overall
prop_survive <- 100*mean(hurricane$survive)

# Proportion of pumps that survived, by reason 
# How could I change the 770 to something more robust?

hurricane %>%
  group_by(reason) %>% 
    summarize(numPumps=n(),propFail=n()/770,avgTime=sum(hour)/n())

# ANOVA for failures
model1<- aov(hurricane$hour~hurricane$reason)

summary(model1)
# Significant, but means aren't the best for us to use

# Plot 1
survObj <- Surv(time=hurricane$hour, event=hurricane$survive==0)

simple_km <- survfit(survObj ~ 1, data=hurricane)

summary(simple_km)

plot(simple_km, main="Survival Function of Pumps", 
                xlab="Hour (1-48)", 
                ylab="Survival Probability")

# Plot 2
hurricane_strat <- survfit(survObj ~ reason, data=hurricane)
summary(hurricane_strat)

stratified_plot <- ggsurvplot(hurricane_strat, data=hurricane, conf.int=FALSE,
                              palette = c("white","black","red","orange","blue"), xlab="Hour (1-48)", ylab="Surivival Probablility",
                              break.y.by=0.01, legend.title="Reason for Failure",
                              legend.labs=c("No Failure","Flood Failure","Motor Failure","Surge Failure","Jammed Failure"))

stratified_plot

# Plot 3

simple_km$hp <- simple_km$n.event/simple_km$n.risk

simple_haz <- merge(data.frame(time=seq(1,48,1)),
                    data.frame(time=simple_km$time, hp=simple_km$hp),
                    by = "time", all = TRUE) 

print(simple_haz)

plot(y = simple_haz$hp, x = simple_haz$time,
     main="Conditional Failure Probability Across 48 Hours",
     xlab="Hour (1-48)",
     ylab="Conditional Probability",
     type="l")

# Plot 4

hurricane_strat <- survfit(survObj ~ reason, data=hurricane)

hurricane_strat$hp <- hurricane_strat$n.event/hurricane_strat$n.risk

strat_haz <- merge(data.frame(time=seq(1,48,1)),
                   data.frame(time=hurricane_strat$time, hp=hurricane_strat$hp),
                   by = "time", all = TRUE) 
print(strat_haz)

plot(y = strat_haz$hp, x = strat_haz$time,
     main="Conditional Failure Probability Across 48 Hours",
     xlab="Hour (1-48)",
     ylab="Conditional Probability",
     type="l")

