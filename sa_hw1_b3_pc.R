###############################
#                             #
#      Survival Analysis:     #
#         Homework #1         #
#                             #
#     Hurricane Readiness     #
#     Phase 1: HK - F3.H1     #
#                             #
#        Blue Team 3          #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("survival")
install.packages("survminer")
install.packages("haven")
install.packages("gmodels")
install.packages("dplyr")

library(survival)
library(survminer)
library(haven)
library(gmodels)
library(dplyr)

# Set Working Directory #
setwd("C:\\Users\\patri\\OneDrive\\Desktop\\_survival_analysis\\sa_data\\")

# Load Needed Data Sets #
cane <- read_sas("hurricane.sas7bdat")
summary(cane)

# Survival Function - Hurricane Pumps #

# Create Survival Object #
cane_surv <- Surv(time = cane$hour, event = cane$survive == 0)

# Create Survival Fit Object #
cane_km <- survfit(cane_surv ~ 1, data = cane)

# Summarize SurvFit Object #
summary(cane_km)

# Plot SurvFit Object #
plot(cane_km, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

# Create Fancy Plot of SurvFit Object #
ggsurvplot(cane_km, data = cane, conf.int = TRUE, palette = "purple",
           xlab = "Hour", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1)

# Compute Percentages of Pumps Survive/Fail #
prop.table(table(cane$survive))

# Compute 3-Way Frequency Table #
# Survive 0/1 -> hour -> reason code #
# Note the where survive == 1, there are no reason codes #
ftable(cane$survive, cane$hour, cane$reason)

# more simple way to view 2-way freq #
cane_table <- table(cane$reason, cane$hour)
cane_table

# Create Dataframe for frequency of Reasons #
reason_count <- as.data.frame(table(cane$reason))
View(reason_count)

# Calculate Percentage of pumps in each type of failure #
reason_proportion <- as.data.frame(table(cane$reason) / length(cane$reason))
View(reason_proportion)
reason_percentage <- reason_proportion$Freq*100
View(reason_percentage)

# ANOVA #
# need to add in ANOVA code #


# Stratified Analysis #

# rho = 0 is asking for the log rank test #
# rho = 1 will give you wilcoxon test #
survdiff(cane_surv ~ reason, rho = 0, data = cane)

cane_strat_reason <- survfit(cane_surv ~ reason, data = cane)
summary(cane_strat_reason)

plot(cane_strat_reason, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

# This plot is not working #
ggsurvplot(cane_strat_reason, data = cane, conf.int = TRUE, 
           palette = c("purple", "black", "green1", "yellow1", "blue1"),
           xlab = "Hour", 
           ylab = "Survival Probability", 
           break.y.by = 0.1,
           legend.title = "Failure Reason", 
           legend.labs = c("0", "1", "2", "3", "4"))

pairwise_survdiff(cane_surv ~ reason, rho = 0, data = cane)



# Hazard Function - Hurricane Data Set #
cane_km$hp <- cane_km$n.event/cane_km$n.risk
cane_haz <- merge(data.frame(time = seq(1,52,1)), data.frame(time = cane_km$time, hp =  cane_km$hp), by = "time", all = TRUE)
cane_haz[is.na(cane_haz) == TRUE] <- 0

plot(y = cane_haz$hp, x = cane_haz$time, main = "Hazard Probability Function", xlab = "Hour", ylab = "Hazard Probability", type = 'l')
ggsurvplot(cane_km, data = cane, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Hour", ylab = "Cumulative Hazard", legend = "none")
