# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 12/11/2021
#
# Description: final project for human-computer interaction for games.
#
# References:
# - https://www.rdocumentation.org/packages/openxlsx/versions/4.2.4/topics/read.xlsx 

# INFR 4350U: Human-Computer Interaction for Games - Final Project

library(tidyverse)
library(ggpubr) # qqplot
library(dplyr)
library(ggplot2)

library(rstatix) # normality and wilcoxon
library(pastecs) # homogeneity of Variances
library(ez) # ezANOVA for Mixed Anova
library(stats) # interaction plots
library(reshape) # diverging stacked bargraphs
library(likert) # diverging stacked bargraphs
library(coin) # used for wilcoxon
library(stats) # friedman

# Exporting Information #
auto_export <- FALSE # automatically export graphs
export_path <- "exports" # export path from working directory

# the rank clamps for the questionnaires
qnaireClamps = c(0, 4)

# installing the required package.
if (!require(readxl)) install.packages(readxl)

# gets the results data
vplData <- read_xlsx("imports/vpl-fnl_pjt_data.xlsx", sheet = "Results")
vplData

# gets the SUS data
sus <- read_xlsx("imports/vpl-fnl_pjt_data.xlsx", sheet = "SUS")
sus

# clamping the rankings.
sus$Rank[sus$Rank < qnaireClamps[1] ] <- qnaireClamps[1] 
sus$Rank[sus$Rank > qnaireClamps[2] ] <- qnaireClamps[2]

# gets the questionnaire data
qnaire <- read_xlsx("imports/vpl-fnl_pjt_data.xlsx", sheet = "Questionnaire")
qnaire

# clamping the rankings.
qnaire$Rank[qnaire$Rank < qnaireClamps[1] ] <- qnaireClamps[1] 
qnaire$Rank[qnaire$Rank > qnaireClamps[2] ] <- qnaireClamps[2]



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

# used for qqplots
if (!require(ggpubr)) install.packages(ggpubr)

# outlier check - ver. 2
ggboxplot(data = vplData, x = "Order", y = "Time",
          color = "Order", palette = orderColours,
          order = c("A->B", "B->A"),
          title = "HCI-FNL_PJT - Time - Order-Based Box Plot (Raw Data)",
          fill = c("GREY", "GREY"),
          ylab = "Clear Time", xlab = "Order Group"
)

# TODO: remove outliers.
# vplDataRaw <- vplData
# vplData <- with(vplData, Time[Order == "A->B"] - Time[Order == "B->A"])

###
# Normality
if (!require(ggpubr)) install.packages(ggpubr)

vplData %>%
  group_by(Order) %>%
  shapiro_test(Time) # get p-value for homogeneity check

ggqqplot(vplData, x = "Time", facet.by = "Course", 
         title = "HCI-FNL_PJT - QQPlot for Normality (Course Faceted)")

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



# INTERACTION #
# post-hoc, interaction plots, and effects
if(!require(stats)) install.packages("stats")

# post-hoc tests
pairwise.t.test(vplData$Time, vplData$Course, paired=T, p.adjust.method ="bonferroni")
pairwise.t.test(vplData$Time, vplData$Order, paired=T, p.adjust.method ="bonferroni") # recommended


# Bargraphs with Error Bars (include these if the effect sizes are significant)
bargraph <- ggplot(vplData, aes(Order, Time))
bargraph + stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.3) + 
  labs(title = "HCI-FNL_PJT - Bar Graph (Raw Data)", x = "Order", y = "Time")


# Interaction Plots (include these if the interactions are significant)
# pairwise tests (for interaction plots)
pairwise.t.test(vplData$Time, interaction(vplData$Course, vplData$Order), paired=T, p.adjust.method ="bonferroni")
pairwise.t.test(vplData$Time, interaction(vplData$Order, vplData$Course), paired=T, p.adjust.method ="bonferroni") # recommended

# Interaction Plots #
# interaction plots (X = Order) (recommended)
interaction.plot(x.factor = vplData$Order, trace.factor = vplData$Course,
                 response = vplData$Time, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Order", ylab="Time", col = orderColours, trace.label ="Course")

# interaction plots (X = Course)
interaction.plot(x.factor = vplData$Course, trace.factor = vplData$Order,
                 response = vplData$Time, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Course", ylab="Time", col = orderColours, trace.label ="Order")


# QUALITIVE ANALYSES #
# TODO: provide standard questionnaire and self-developed questionnaire responses.
# - display the information in diverging stacked bar charts.
# - include data for user-defined test and standard test. Standard test chosen is SUS.

# needed for casting.
if(!require(reshape)) install.packages("reshape")
if(!require(likert)) install.packages("likert")

likOrder = c("A", "B")
likColours <- c("#ffc7c7","#cdffc7","#c7f8ff","#ffff99","#fce3ff")

# gets wide data version of the variables.


# SUS #
# wide data
# susWideData<-cast(sus, Participant + Order + Course ~ Question, value = "Rank")
susWideData<-cast(sus, Participant + Order + Course ~ Question, value = "Rank")

# amount printed is (end - start + 1)
susWideData_start = 3
susWideData_end = 12

# first six
# susWideData_start = 3
# susWideData_end = 8

# applying data
susWideData[susWideData_start:susWideData_end] <- lapply(susWideData[susWideData_start:susWideData_end], factor, levels = 0:4)

# create new likert
likSus <- likert::likert(susWideData[,c(susWideData_start:susWideData_end)], grouping = susWideData$Course)

# plot
plot(likSus, plot.percents = TRUE, colors = likColours, group.order = likOrder)

# TESTS
# used for effect size.
if (!require("coin")) install.packages("coin") # friedman
if (!require("stats")) install.packages("stats") # friedman
if (!require("rstatix")) install.packages("rstatix") #wilcoxon

# WILCOXON TEST

# Compute some summary statistics by groups
sus %>%
  group_by(Order) %>%
  get_summary_stats(Rank, type = "median_iqr")

# Compute the differences between pairs
sus <- sus %>% mutate(differences = B - A)

# Create histogram
gghistogram(sus, x = "differences", y = "..density..", fill = "steelblue", bins = 5, add_density = TRUE)

# Computation
stat.test <- sus %>%
   wilcox_test(Rank ~ Order, paired = TRUE)

stat.test

# Effect Size
sus  %>%
  wilcox_effsize(Rank ~ Order, paired = TRUE)


# FRIEDMAN TEST
head(sus, 10)

# TODO: remove
# transforms the SUS data into long data (already in long data format)
# sus_long <- sus %>%
#   gather(key = "Course", value = "Time", A, B) %>%
#  convert_as_factor(Participant, Course)
# head(sus, 10)

# Compute some summary statistics by groups (course)
sus %>%
  group_by(Course) %>%
  get_summary_stats(Rank, type = "common")

# Compute some summary statistics by groups (order)
sus %>%
  group_by(Order) %>%
  get_summary_stats(Rank, type = "common")

# Computation
friedman.test(y=sus$Rank, groups = sus$Course, blocks = sus$Participant)
friedman.test(y=sus$Rank, groups = sus$Order, blocks = sus$Participant)

friedman_test(Rank~Course | Participant, sus)
friedman_test(Rank~Order | Participant, sus)

# Effect Size
sus %>% friedman_effsize(Rank ~ Course | Participant)
sus %>% friedman_effsize(Rank ~ Order | Participant)


###
# QNAIRE #
qnaireWideData<-cast(qnaire, Participant + Course ~ Question, value = "Rank")
qnaireWideData_start = 3
qnaireWideData_end = 8

# applying data
qnaireWideData[qnaireWideData_start:qnaireWideData_end] <- lapply(qnaireWideData[qnaireWideData_start:qnaireWideData_end], factor, levels = 0:4)

# create new likert
likQnaire <- likert::likert(qnaireWideData[,c(qnaireWideData_start:qnaireWideData_end)], grouping = qnaireWideData$Course)

# plot
plot(likQnaire, plot.percents = TRUE, colors = likColours, group.order = likOrder)

# TODO: perform Wilcoxon and Friedman tests.

# regarding free-form questions 
print("See included report for free form question responses and reportings on findings.")

