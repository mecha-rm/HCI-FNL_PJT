# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 12/14/2021
#
# Description: final project for human-computer interaction for games.
#
# References:
# - https://www.rdocumentation.org/packages/openxlsx/versions/4.2.4/topics/read.xlsx 
# - https://stackoverflow.com/questions/21457505/friedman-test-unreplicated-complete-block-design-error

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
auto_export <- TRUE # automatically export graphs
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
# - independent and between-subject (no group has everyone in it).
# Course: course (A) or (B)
# - independent and within-subject (everyone is in every group).
# Time: time it took to complete the course.

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

# if the ggplot should be exported.
if(auto_export) {
  ggsave(filename = "hci-fnl_pjt-bar_plot_rd_v01.png", path = export_path)
  ggsave(filename = "hci-fnl_pjt-bar_plot_rd_v01.eps", path = export_path)
}

# bar graph - ver. 2
ggbarplot(data = vplData, x = "Order", y = "Time",
          color = "Order", palette = orderColours,
          fill = "WHITE",
          title = "HCI-FNL_PJT - Bar Plot (Raw Data)",
          ylab = "Clear Time", xlab = "Order Group",
          facet.by = "Course",
          merge = TRUE
)

# if the plot should be exported.
if(auto_export) {
  ggsave(filename = "hci-fnl_pjt-bar_plot_rd_v02.png", path = export_path)
  ggsave(filename = "hci-fnl_pjt-bar_plot_rd_v02.eps", path = export_path)
}

# bargraph - ver. 3 (with error bars)
bargraph <- ggplot(vplData, aes(Order, Time))
bargraph + stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.3) + 
  labs(title = "HCI-FNL_PJT - Bar Graph (Raw Data)", x = "Order", y = "Time")

# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-bar_plot_errors.png", sep = "/")
  dev.copy(png, f)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-bar_plot_errors.eps", sep = "/")
  postscript(f)
  bargraph <- ggplot(vplData, aes(Order, Time))
  bargraph + stat_summary(fun = mean, geom = "bar") + 
    stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.3) + 
    labs(title = "HCI-FNL_PJT - Bar Graph (Raw Data)", x = "Order", y = "Time")
  dev.off()
  
}


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

# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-box_plot_order-time_ver01.png", sep = "/")
  dev.copy(png, f)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-box_plot_order-time_ver01.eps", sep = "/")
  postscript(f)
  boxplot(formula = vplData$Time ~ vplData$Order)
  dev.off()
  
}


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

# if the ggplot should be exported.
if(auto_export) {
  ggsave(filename = "hci-fnl_pjt-box_plot_order-time_ver02.png", path = export_path)
  ggsave(filename = "hci-fnl_pjt-box_plot_order-time_ver02.eps", path = export_path)
}


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

# the ggqqplot
if(auto_export) {
  ggsave(filename = "hci-fnl_pjt-qqplot.png", path = export_path)
  ggsave(filename = "hci-fnl_pjt-qqplot.eps", path = export_path)
}

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
  labs(title = "HCI-FNL_PJT - Bar Graph (Adjusted)", x = "Order", y = "Time")


# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-bargraph_adjusted.png", sep = "/")
  dev.copy(png, f)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-bargraph_adjusted.eps", sep = "/")
  postscript(f)
  bargraph <- ggplot(vplData, aes(Order, Time))
  bargraph + stat_summary(fun = mean, geom = "bar") + 
    stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.3) + 
    labs(title = "HCI-FNL_PJT - Bar Graph (Adjusted)", x = "Order", y = "Time")
  dev.off()
  
}


# Interaction Plots (include these if the interactions are significant)
# pairwise tests (for interaction plots)
pairwise.t.test(vplData$Time, interaction(vplData$Course, vplData$Order), paired=T, p.adjust.method ="bonferroni")
pairwise.t.test(vplData$Time, interaction(vplData$Order, vplData$Course), paired=T, p.adjust.method ="bonferroni") # recommended

# Interaction Plots #
# interaction plots (X = Order) (recommended)
interaction.plot(x.factor = vplData$Order, trace.factor = vplData$Course,
                 response = vplData$Time, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Order", ylab="Time", col = orderColours, trace.label ="Course")


# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-interaction_order_ver.png", sep = "/")
  dev.copy(png, f)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-interaction_order_ver.eps", sep = "/")
  postscript(f)
  interaction.plot(x.factor = vplData$Order, trace.factor = vplData$Course,
                   response = vplData$Time, fun = mean, type = "b", legend = TRUE, 
                   xlab = "Order", ylab="Time", col = orderColours, trace.label ="Course")
  dev.off()
  
}


# interaction plots (X = Course)
interaction.plot(x.factor = vplData$Course, trace.factor = vplData$Order,
                 response = vplData$Time, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Course", ylab="Time", col = orderColours, trace.label ="Order")



# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-interaction_course_ver.png", sep = "/")
  dev.copy(png, f)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-interaction_course_ver.eps", sep = "/")
  postscript(f)
  interaction.plot(x.factor = vplData$Course, trace.factor = vplData$Order,
                   response = vplData$Time, fun = mean, type = "b", legend = TRUE, 
                   xlab = "Course", ylab="Time", col = orderColours, trace.label ="Order")
  dev.off()
  
}


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


# TODO: implement graph exports for SUS and QNAIRE
# SUS #
# wide data
# NOTE: for some reason it doesn't like it when order is included.
# susWideData<-cast(sus, Participant + Order + Course ~ Question, value = "Rank") # question ver.
susWideData<-cast(sus, Participant + Course ~ Question, value = "Rank") # question ver.
susWideDataCourses<-cast(sus, Participant + Order + Question ~ Course, value = "Rank") # course ver.

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

# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-sus-diverging_stacked_bar_graph.png", sep = "/")
  dev.copy(png, f, width = 768, height = 1792)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-sus-diverging_stacked_bar_graph.eps", sep = "/")
  postscript(f)
  plot(likSus, plot.percents = TRUE, colors = likColours, group.order = likOrder)
  dev.off()
  
}

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
susWideDataCourses <- susWideDataCourses %>% mutate(differences = B - A)

# Create histogram
# TODO: change bin count
gghistogram(susWideDataCourses, x = "differences", y = "..density..", fill = "steelblue", bins = 5, add_density = TRUE)

# if the plot should be exported.
if(auto_export) {
  ggsave(filename = "hci-fnl_pjt-sus-histrogram.png", path = export_path)
  ggsave(filename = "hci-fnl_pjt-sus-histrogram.eps", path = export_path)
}


# Computation
stat.test <- sus %>%
   wilcox_test(Rank ~ Order, paired = TRUE)

# stat.test <- susWideDataCourses %>%
#  wilcox_test(Rank, paired = TRUE)

#.TEST AND _TEST() are different.
stat.test <- wilcox.test(x = sus$Rank, paired = FALSE)
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

# Computation (doesn't work for some reason)
# uses within subject variable
friedman.test(y=sus$Rank, groups = sus$Course, blocks = sus$Participant)

# table(groups, blocks)
table(sus$Course, sus$Participant)

# friedman.test(y=sus$Rank, groups = sus$Order, blocks = sus$Participant)

# uses within subject variable
friedman_test(Rank~Course | Participant, sus)
# friedman_test(Rank~Order | Participant, sus)

# Effect Size
sus %>% friedman_effsize(Rank ~ Course | Participant)
sus %>% friedman_effsize(Rank ~ Order | Participant)


###
# QNAIRE #
# qnaireWideData<-cast(qnaire, Participant + Order + Course ~ Question, value = "Rank")
qnaireWideData<-cast(qnaire, Participant + Course ~ Question, value = "Rank")
qnaireWideDataCourses<-cast(qnaire, Participant + Order + Question ~ Course, value = "Rank") # course ver.

qnaireWideData_start = 3
qnaireWideData_end = 10

# applying data
qnaireWideData[qnaireWideData_start:qnaireWideData_end] <- lapply(qnaireWideData[qnaireWideData_start:qnaireWideData_end], factor, levels = 0:4)

# create new likert
likQnaire <- likert::likert(qnaireWideData[,c(qnaireWideData_start:qnaireWideData_end)], grouping = qnaireWideData$Course)

# plot
plot(likQnaire, plot.percents = TRUE, colors = likColours, group.order = likOrder)

# if the standard plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-fnl_pjt-qnaire-diverging_stacked_bar_graph.png", sep = "/")
  dev.copy(png, f, width = 512, height = 768)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-fnl_pjt-qnaire-diverging_stacked_bar_graph.eps", sep = "/")
  postscript(f)
  plot(likQnaire, plot.percents = TRUE, colors = likColours, group.order = likOrder)
  dev.off()
  
}

# -
# TODO: perform Wilcoxon and Friedman tests.
# WILCOXON TEST

# Compute some summary statistics by groups
qnaire %>%
  group_by(Order) %>%
  get_summary_stats(Rank, type = "median_iqr")

# Compute the differences between pairs
qnaireWideDataCourses <- qnaireWideDataCourses %>% mutate(differences = B - A)

# Create histogram
# TODO: change bin count
gghistogram(susWideDataCourses, x = "differences", y = "..density..", fill = "steelblue", bins = 5, add_density = TRUE)

# if the plot should be exported.
if(auto_export) {
  ggsave(filename = "hci-fnl_pjt-qnaire-histrogram.png", path = export_path)
  ggsave(filename = "hci-fnl_pjt-qnaire-histrogram.eps", path = export_path)
}

# Computation
#.TEST AND _TEST() are different.
stat.test <- wilcox.test(x = qnaire$Rank, paired = FALSE)
stat.test

# Effect Size
qnaire  %>%
  wilcox_effsize(Rank ~ Order, paired = TRUE)


# FRIEDMAN TEST
head(qnaire, 8)

# Compute some summary statistics by groups (course)
qnaire %>%
  group_by(Course) %>%
  get_summary_stats(Rank, type = "common")

# Compute some summary statistics by groups (order)
qnaire %>%
  group_by(Order) %>%
  get_summary_stats(Rank, type = "common")

# Computation
friedman.test(y=qnaire$Rank, groups = qnaire$Course, blocks = qnaire$Participant)
friedman.test(y=qnaire$Rank, groups = qnaire$Order, blocks = qnaire$Participant)

friedman_test(Rank~Course | Participant, qnaire)
friedman_test(Rank~Order | Participant, qnaire)

# Effect Size
qnaire %>% friedman_effsize(Rank ~ Course | Participant)
qnaire %>% friedman_effsize(Rank ~ Order | Participant)


# regarding free-form questions 
print("See included report for free form question responses and reportings on findings.")

