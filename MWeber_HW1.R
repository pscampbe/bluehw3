# Installing all necessary packages
library(haven)
library(dplyr)
library(survival)
library(survminer)
library(RColorBrewer)

# Reading in the data
setwd("~/Documents/MSA/Fall3/SurvivalAnalysis")
pump <- read_sas("hurricane.sas7bdat")
print(pump)

# 1. Percentage of pumps that survived the hurricane 
  # 41%
# 2. Percentage of pumps in each type of failure and average failure time for each failure type
#group by mean and median
group_by(pump, reason) %>%
  summarise(
    count = n(),
    mean = mean(hour, na.rm = TRUE),
    median = median(hour, na.rm = TRUE),
    percentage = (n()/770)
  )


# 3. Statistical tests if these averages for each type of failure are different.
pump_surv <- Surv(time = pump$hour, event = pump$survive == 0) # different than SAS which would be 1
pump_km <- survfit(pump_surv ~ 1, data = pump) # the ~1 compares it to the intercept only equation
survdiff(pump_surv ~ reason, rho = 0, data = pump)
# significant p-value


#########################################################################################################
##### Provide the following graphs as well as any meaningful insights from the graphs that you see: #####
#########################################################################################################


# 1. Survival probability across time for all pumps together – not broken down byfailure type
ggsurvplot(pump_km, data = pump, conf.int = TRUE, palette = 'purple',
                            xlab = 'Hour', ylab = 'Survival Probability', legend = 'none',
                            break.y.by = 0.1)
# We want to test if there is a difference between the different reasons
# in order to do this we need to remove 0 because these are the pumps that did not fail
pump_without_zero  <- pump %>% filter(reason!=0)
#survival object
pump_surv_no_zero <- Surv(time = pump_without_zero$hour, event = pump_without_zero$survive == 0)
pump_strat_no_zero <- survfit(pump_surv_no_zero ~ reason, data = pump_without_zero)
summary(pump_strat_no_zero)


# 2. Survival probability across time for pumps broken down by failure type overlaid into one graph
ggsurvplot(pump_strat_no_zero, data = pump_without_zero, palette = brewer.pal(n = 4, name = 'Dark2'),
                            title = 'Survival Probability Over Time, Stratified By Failure Type', xlab = 'Hour', ylab = 'Survival Probability',
                            break.y.by = 0.1, legend = 'right',
                            legend.title = 'Failure Type', legend.labs = c('Flood Failure',
                                                                           'Motor Failure',
                                                                           'Surge Failure',
                                                                           'Jammed Failure'))



#3. Conditional failure probabilities across time for all pumps together – not broken down by failure type.
# Creating the hazard function
pump_km$hp <- pump_km$n.event/pump_km$n.risk
# merge back into original dataframe
simple_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = pump_km$time, hp = pump_km$hp), by = "time", all = TRUE)
simple_haz[is.na(simple_haz) == TRUE] <- 0
print(simple_haz)

plot(y = simple_haz$hp, x = simple_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(pump_km, data = pump, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Hour", ylab = "Cumulative Hazard", legend = "none")




#4. Conditional failure probabilities across time for pumps broken down by failure type overlaid into one graph.
# Once again we have to take out the zero because we want to subset by failure reason
# and zero indicated no failure
pump_without_zero  <- pump %>% filter(reason!=0)
#survival object
# Make 5 different datasets and then pot them all on top of eachother
# plot cumulative hazard across time for each failure type
pump_strat <- survfit(pump_surv ~ reason, data = pump)
pump_strat$hp <- pump_strat$n.event/pump_strat$n.risk
hp_data = data.frame(hour=pump_strat$time, hp = pump_strat$hp)
hp_data$reason = c(rep(0, pump_strat$strata[1]), rep(1, pump_strat$strata[2]), rep(2, pump_strat$strata[3]), rep(3, pump_strat$strata[4]), rep(4, pump_strat$strata[5]))
hp_data$reason = as.factor(hp_data$reason)
hp_data$hp[is.na(hp_data$hp) == TRUE] <- 0

ggplot(data=hp_data, aes(x=hour, y=hp, color=reason)) + geom_line()

