# read in packages

  library(sas7bdat)
  library(AMR)
  library(ggplot2)
  library(stats)
  library(survival)
  library(survminer)

# read in data set
  surv <- read.sas7bdat('C:\\Users\\kevin\\Documents\\NC State\\Courses\\Fall 3\\dont die\\HW\\hurricane.sas7bdat')

# Provide the following summary statistics for each of the types of pump station failure:
  
  # Percentage of pumps in each type of failure:
    freq(surv$reason)

    # Percent Survived: 41.0%
    # Percent Flood: 14.9%
    # Percent Motor: 14.5%
    # Percent Surge: 14.4%
    # Percent Jammed: 15.1%

  # Average failure time for each failure type:
    avg_hr <- aggregate(surv$hour, list(surv$reason), mean)

    # Survived: 48
    # Flood: 26.44
    # Motor: 41.04
    # Surge: 38.93
    # Jammed: 21.94

  # Statistical tests if these averages for each type of failure are different.
    aov.surv <- aov(hour ~ reason, surv)
    summary(aov.surv)

  # median hour for each failure type
    med_hr <- aggregate(surv$hour, list(surv$reason), median)

    # Survived: 48
    # Flood: 26
    # Motor: 45
    # Surge: 42
    # Jammed: 25


# Provide the following graphs as well as any meaningful insights from the graphs that you see:

  # Survival probability across time for all pumps together - not broken down by failure type.

    # create survival fit from survival object
    simple_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = surv)

    # summary of survival object
    summary(simple_km)  

    # plot survival function 
    plot(simple_km, main = "Survival Function", xlab = "Tenure", ylab = "Survival Probability")


  # Survival probability across time for pumps broken down by failure type overlaid into one graph.  

    # create data set that removes pumps that did not fail
    surv_cen <- surv[which(surv$reason != 0),]
    
    freq(surv_cen$reason)

    # create surv object on pumps with failures only
    hurr_surv <- Surv(time = surv_cen$hour, event = surv_cen$survive == 0)
    
    # check global comparison of survival curves across all failure types
    survdiff(hurr_surv ~ reason, rho = 0, data = surv_cen)
    
    # check pairwise comparisons of survival curves for each failure type
    pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 0, p.adjust.method = "bonferroni", data = surv_cen)
   
    # create stratified survival fit object
    hurr_strat <- survfit(hurr_surv ~ reason, data = surv_cen)
    summary(hurr_strat)

    # create stratified survival plot   
    ggsurvplot(hurr_strat, data = surv_cen, palette = c( "black", "orange", "blue", "red"),
           xlab = "Hour", ylab = "Survival Probability", break.y.by = 0.1,
           legend.title = "failure type", legend.labs = c("flood", "motor", "surge", "jammed"))  

  # create conditional failure probabilities across time for all pumps
    summary(simple_km)
    simple_km$hp <- simple_km$n.event/simple_km$n.risk
    simple_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = simple_km$time, hp = simple_km$hp), by = "time", all = TRUE)
    simple_haz[is.na(simple_haz) == TRUE] <- 0

    print(simple_haz)

    # plot hazard function
    plot(y = simple_haz$hp, x = simple_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

  # plot cumulative hazard across time for all pumps
    ggsurvplot(simple_km, data = surv, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Hour", ylab = "Cumulative Hazard", legend = "none")

  # plot cumulative hazard across time for each failure type
    
    # create flood hazard probability 
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

    # plot all hazards
    plot(y = flood_haz$hp, x = flood_haz$time, main = "Hazard Probability Function by Failure Type", xlab = "Tenure", ylab = "Hazard Probability",
      type = 'l')
    lines(y=motor_haz$hp, x = motor_haz$time, type = 'l', col = 'red')
    lines(y=surge_haz$hp, x = surge_haz$time, type = 'l', col = 'orange')
    lines(y=jam_haz$hp, x = jam_haz$time, type = 'l', col = 'purple')

    # plot cumulative hazards  
    ggsurvplot(hurr_strat, data = surv_cen, fun = "event", palette = c("purple","blue","red","orange"),
               xlab = "Hour", ylab = "Cumulative Hazard", legend = "none")
    
    