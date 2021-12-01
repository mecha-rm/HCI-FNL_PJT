# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 12/01/2021
#
# Description: final project for human-computer interaction for games.
#
# References:
# - https://www.rdocumentation.org/packages/openxlsx/versions/4.2.4/topics/read.xlsx 

# INFR 4350U: Human-Computer Interaction for Games - Final Project

library(tidyverse)
library(ggpubr)
library(dplyr)
library(ggplot2)

library(rstatix) # normality
library(pastecs) # homgenity of Variances
library(ez) # ezANOVA for Mixed Anova
library(stats) # interaction plots

# Exporting Information #
auto_export <- FALSE # automatically export graphs
export_path <- "exports" # export path from working directory

# installing the required package.
if (!require(readxl)) install.packages(readxl)

vplData <- read_xlsx("imports/vpl-fnl_pjt_data.xlsx")
vplData

# NOTES
# Participants: 10
#
# Order: order of courses done (A->B, or B->A)
# - independent and between-subject (everyone is in every group).
# Course: course (A) or (B)
# - independent and within-subject (no group has everyone in it).
# Time: time it took to complete hte course.

# Two Groups (A->B, B->A)
# Counterbalanced using 2X2 Latin Square (2 Possible Orders)
# Analyzing Time

orderColours = c("RED", "BLUE")

# bar graph - ver. 1
ggbarplot(data = vplData, x = "Order", y = "Time",
          color = "Order", palette = orderColours,
          fill = "WHITE",
          title = "HCI-FNL_PJT - Bar Plot (Raw Data)",
          ylab = "Clear Time", xlab = "Order Group",
          merge = TRUE
          )

# bar graph - ver. 2
ggbarplot(data = vplData, x = "Order", y = "Time",
          color = "Order", palette = orderColours,
          fill = "WHITE",
          title = "HCI-FNL_PJT - Bar Plot (Raw Data)",
          ylab = "Clear Time", xlab = "Order Group",
          facet.by = "Course",
          merge = TRUE
)

# bar chart - ver. 3 (with error bars)
bargraph <- ggplot(vplData, aes(Order, Time))
bargraph + stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.3) + 
  labs(title = "HCI-FNL_PJT - Bar Graph (Raw Data)", x = "Order", y = "Time")

# TODO: COUNTERBALANCE

# MIXED ANOVA
# Assumptions

# Outliers
vplData %>%
  group_by(Order, Course) %>%
  identify_outliers(Time)

# boxplot(formula = vplData$Time ~ vplData$Course * vplData$Order)
# outlier check - ver. 1
boxplot(formula = vplData$Time ~ vplData$Order)

# outlier check - ver. 2
ggboxplot(data = vplData, x = "Order", y = "Time",
          color = "Order", palette = orderColours,
          order = c("A->B", "B->A"),
          title = "HCI-FNL_PJT - Time - Order-Based Box Plot (Raw Data)",
          fill = c("GREY", "GREY"),
          ylab = "Clear Time", xlab = "Order Group"
)

# TODO: use this to take out outliers
# vplData_no <- with(vplData, Time[Order == "A->B"] - Time[Order == "B->A"])

###
# Normality
if (!require(rstatix)) install.packages(rstatix)

vplData %>%
  group_by(Order) %>%
  shapiro_test(Time) # get p-value for homogeneity check

###
# Assumption of Sphereicity
res.aov <- anova_test(
  data = vplData, dv = Time, wid = Participant,
  between = Order, within = Course
)

# print values
get_anova_table(res.aov)


###
# Homgenity of Variances
if(!require(rstatix)) install.packages("rstatix")
if(!require(pastecs)) install.packages("pastecs") 

levene_test(data = vplData, formula = Time~Order*Course)

# order grouping
vplData %>%
  group_by(Order) %>%
  levene_test(Time ~ Course)

# course grouping
vplData %>%
  group_by(Course) %>%
  levene_test(Time ~ Order)

###
# Homogeneity of Covariances
if(!require(rstatix)) install.packages("rstatix")

box_m(vplData["Time"], vplData$Course)


# Mixed ANOVA Test with ezANOVA
if(!require(ez)) install.packages("ez")

# TODO: check if type should be changed.
vplData_MixedANOVA <- ezANOVA(data = vplData, dv = .(Time), 
                        wid = .(Participant), within = .(Course), between = .(Order),
                        detailed = T, type = 3)
vplData_MixedANOVA


# --- #


# OTHER RESULTS
# interaction plots
if(!require(stats)) install.packages("stats")

# pairwise tests #
# pairwise.t.test(vplData$Time, interaction(vplData$Course, vplData$Order), paired=T, p.adjust.method ="bonferroni")
# pairwise.t.test(vplData$Time, interaction(vplData$Order, vplData$Course), paired=T, p.adjust.method ="bonferroni") # recommended

