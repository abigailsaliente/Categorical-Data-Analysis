#######PRACTICE DATA#######

# Practice
# Create datset
survdata <- data.frame (
  time = c(3, 5, 7, 2, 18, 16, 2, 9, 16, 5),
  event = c(0, 1, 1, 1, 0, 1, 1, 1, 1, 0))

View(survdata)


# Create Kaplan-Meier function
# Note that ~1 means indicates that we estimate the
# Kaplan-Meier w/o any grouping.
install.packages("survival")
library(survival)
km <- survfit(Surv(time, event) ~1,
              data = survdata)
km
summary(km)


# Create Kaplan-Meier plots
install.packages("ggsurvfit")
library(ggplot2)
library(dplyr)
library(ggsurvfit)

survfit(Surv(time, event) ~1, data = survdata) %>%
  ggsurvfit() +
  add_confidence_interval() +
  # Add the median survival line (p = 0.5)
  add_quantile(0.5) +
  add_risktable() +
  labs(
    x = "Time",
    y = "Overall survival probability"
  ) +
  coord_cartesian(xlim = c(0, 20)) +
  ggtitle("Overall Estimate of Kaplan-Meier Survival Function")



#######LOG-RANK TEST#######

# Log-rank test
# Create dataset
LRdata <- data.frame(
  group = c(rep("No", 6), rep("Yes", 6)),
  time = c(4.1, 7.8, 10, 10, 12.3, 17.2, 9.7,
           10, 11.1, 13.1, 19.7, 24.1),
  event = c(1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0))

View(LRdata)


# Create a Kaplan-Meier function
lr <- survdiff(Surv(time, event) ~ group, data = LRdata)
lr


# Create Kaplan-Meier plots with log-rank test
survfit2(Surv(time, event) ~ group, data = LRdata) %>%
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
  ) +
  coord_cartesian(xlim = c(0, 24.1)) +  # Set x-axis limits
  # Add the median survival line (p = 0.5)
  add_quantile(0.5) +
  # Display the log-rank test p-value
  add_pvalue(
    test = "log-rank",
    pvalue_fun = scales::pvalue_format(),  # Control decimal places
    x = 20,  # Position on x-axis
    y = 0.1  # Position on y-axis
  ) +
  ggtitle("Kaplan-Meier Curve for the Waiting Time by Group") +
  scale_color_manual(
    name = "group",
    values = c("No" = "blue", "Yes" = "red"),  # Colors for each group
    labels = c("1" = "No", "2" = "Yes")        # Labels in the legend
  )



#######SAMPLE DATA#######

attach(SURVDATA)
str(SURVDATA)


# Create Overall Kaplan-Meier function
# Note that ~1 means indicates that we estimate the
# Kaplan-Meier without any grouping
km <- survfit(
  Surv(Months, Event) ~1,
  data = SURVDATA)
km
summary(km)


# Create Kaplan-Meier plots
survfit(Surv(Months, Event) ~1, data = SURVDATA) %>%
  ggsurvfit() +
  add_confidence_interval() +
  # Add the median survival line (p = 0.5)
  add_quantile(0.5) +
  add_risktable() +
  labs(
    x = "Months",
    y = "Overall survival probability"
  ) +
  coord_cartesian(xlim = c(0, 25)) +
  ggtitle("Overall Estimate of Kaplan-Meier Survival Function")


##CIVIL STATUS##
# Ensure CivilStatus is a factor with correct labels
SURVDATA$CivilStatus <- factor(SURVDATA$CivilStatus,
                               levels = c("1", "2"),
                               labels = c("Single", "Married"))
SURVDATA$CivilStatus

CS <- survdiff(Surv(Months, Event) ~ CivilStatus, data = SURVDATA$CivilStatus)
CS

# Kaplan-Meier with grouping
# Create Kaplan-Meier plots with log-rank test
survfit2(Surv(Months, Event) ~ CivilStatus, data = SURVDATA) %>%
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
  ) +
  coord_cartesian(xlim = c(0, 25)) +  # Set x-axis limits
  # Add the median survival line (p = 0.5)
  add_quantile(0.5) +
  # Display the log-rank test p-value
  add_pvalue(
    test = "log-rank",
    pvalue_fun = scales::pvalue_format(),  # control decimal places
    x = 20,  # position on x-axis
    y = 0.1  # position on y-axis
  ) +
  ggtitle("Kaplan-Meier Curve and Log-rank Test for the Waiting Time by Civil Status") +
  scale_color_manual(
    name = "Civil Status",
    values = c("Single" = "blue", "Married" = "red"),  # Colors for each group
    labels = c("Single", "Married")                    # Labels in the legend
  )


#######SIGNIFICANT AND NOT SIGNIFICANT FACTORS#######

lr_sex <- survdiff(Surv(Months, Event) ~ Sex, data=SURVDATA)
lr_sex
# NOT SIGNI ~ 1

lr_cs <- survdiff(Surv(Months, Event) ~ CourseDesc, data=SURVDATA)
lr_cs
# SIGNI ~ 0.0000007

lr_yg <- survdiff(Surv(Months, Event) ~ YearGrad1, data=SURVDATA)
lr_yg
# SIGNI ~ 0.00001

lr_ae <- survdiff(Surv(Months, Event) ~ advancededucation, data=SURVDATA)
lr_ae
# SIGNI ~ 0.00000001

lr_pl <- survdiff(Surv(Months, Event) ~ ProLicense, data=SURVDATA)
lr_pl
# SIGNI ~ 0.001

lr_pt <- survdiff(Surv(Months, Event) ~ PassedTechSpecialSkills, data=SURVDATA)
lr_pt
# NOT SIGNI ~ 0.2

lr_cs <- survdiff(Surv(Months, Event) ~ IsCivilService, data=SURVDATA)
lr_cs
# NOT SIGNI ~ 0.4

lr_ws <- survdiff(Surv(Months, Event) ~ IsWorkingStudent, data=SURVDATA)
lr_ws
# SIGNI ~ 0.0009




#######Cox PH#######
# Assumption Checking for CoxPH

# Schoenfeld test - proportional hazard (global; overall - not individually)

# Ho: There is no significant correlation between residual (error) and time.

# Ha: There is a significant correlation between residual and time.



Cox1 <- coxph(Surv(Months, Event) ~ as.factor(CourseDesc) +
                as.factor(YearGrad1) + as.factor(advancededucation) +
                as.factor(ProLicense) + as.factor(IsWorkingStudent),
             data = SURVDATA)
Cox1

library(MASS)
stepwise_cox <- stepAIC(Cox1,               # initial model
                        direction = "both")  # Can also use "backward" or "forward"

summary(stepwise_cox)



Cox2 <- coxph(Surv(Months, Event) ~ as.factor(CourseDesc) +
                 as.factor(YearGrad1) + as.factor(advancededucation) + 
                + as.factor(IsWorkingStudent), data = SURVDATA)
Cox2



# Schoenfeld test - proportional hazard
# Assumption Checking - failed to reject;
# must be greater than 0.05; just like normality
cox.zph(Cox2)


aft_exponential <- survreg(Surv(Months, Event) ~ as.factor(CourseDesc) +
                             as.factor(YearGrad1) + as.factor(advancededucation) +
                             as.factor(IsWorkingStudent), data = SURVDATA,
                           dist = "exponential")


aft_weibull <- survreg(Surv(Months, Event) ~ as.factor(CourseDesc) +
                             as.factor(YearGrad1) + as.factor(advancededucation) +
                             as.factor(IsWorkingStudent), data = SURVDATA,
                           dist = "weibull")


aft_lognormal <- survreg(Surv(Months, Event) ~ as.factor(CourseDesc) +
                         as.factor(YearGrad1) + as.factor(advancededucation) +
                         as.factor(IsWorkingStudent), data = SURVDATA,
                       dist = "lognormal")


aft_loglogistic <- survreg(Surv(Months, Event) ~ as.factor(CourseDesc) +
                           as.factor(YearGrad1) + as.factor(advancededucation) +
                           as.factor(IsWorkingStudent), data = SURVDATA,
                         dist = "loglogistic")

summary(aft_loglogistic) # best model

AIC(aft_exponential, aft_weibull, aft_lognormal, aft_loglogistic)
BIC(aft_exponential, aft_weibull, aft_lognormal, aft_loglogistic)

logLik(aft_exponential)
logLik(aft_weibull)
logLik(aft_lognormal)
logLik(aft_loglogistic)



