# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 11/21/2021
#
# Description: final project for human-computer interaction for games.
#
# References:
# - https://www.rdocumentation.org/packages/openxlsx/versions/4.2.4/topics/read.xlsx 

# INFR 4350U: Human-Computer Interaction for Games - Final Project

# Exporting Information #
auto_export <- FALSE # automatically export graphs
export_path <- "exports" # export path from working directory

# installing the required package.
if (!require(readxl)) install.packages(readxl)

data <- read_xlsx("imports/vpl-fnl_pjt_data.xlsx")
data
