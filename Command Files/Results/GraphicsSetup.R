

# Detach previous packages

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

# Load packages

require(tidyverse)
library(stringr)
require(igraph)
library(ggraph)
library(tidygraph)

# mixed modeling

library(nlme)
library(lme4)
#library(car)
library(lmerTest)
#library(lsmeans)
library(emmeans)
library(multcompView)
library(r2glmm)
library(splines)

require(lattice)
source(system.file("utils", "allFit.R", package="lme4"))

# negative binomial

require(foreign)
require(MASS)

# beta regression
require(mgcv)
require(glmmTMB)
require(lmtest)
library(betareg)
library(car)
library(gamlss)
library(gamlss.dist)
library(RODBC)
library(rcompanion)
library(broom)
#library(BSagri)
#library(gamboostLSS)
library(brms)
library(tidybayes)
library(magrittr)



# mixed effects logistic

require(GGally)
require(reshape2)
require(compiler)
require(parallel)
require(boot)

# output

#library(officer)
#library(rvg)
#library(devEMF)

# Environmental Variables

setwd("~/Box Sync/PhD-AngelaZoss/2-Experimental Studies/CombinedExperimental/Command Files/Results")

originalDataDir <- "../../Original Data"
analysisDataDir <- "../../Analysis Data"

generatedDataDir <- file.path(originalDataDir, "Generated data")

figureDir <- "../../Documents/"

# Loading analysis data files

#TO DO : in previous scripts, save data with saveRDS

orig.graphics <- read_csv(file.path(analysisDataDir, "Graphics.csv"))

layoutfac <- read_csv(file.path(analysisDataDir, "LayoutFac.csv"))


# Slight processing for analysis

graphics <- set_tidy_names(orig.graphics, syntactic=TRUE)

graphics$ClustConf <- factor(graphics$ClustConf,
                             levels = c("Very confident (76-100%)", 
                                        "Somewhat confident (51-75%)", 
                                        "Somewhat doubtful (26-50%)", 
                                        "Very doubtful (0-25%)"),
                             ordered = TRUE)

graphics$Dataset <- factor(graphics$Dataset, ordered = TRUE)

#Make control the reference group
#TO DO: decide on reference group for task??

graphics <- graphics %>% mutate(ConditionPhrasing = case_when(
  Condition == "Ctrl" ~ "1-Technical",
  Condition == "Phr" ~ "2-Informal",
  Condition == "Col" ~ "1-Technical",
  Condition == "Siz" ~ "1-Technical"
)
)

graphics <- graphics %>% mutate(ConditionGraphics = case_when(
  Condition == "Ctrl" ~ "1-Default",
  Condition == "Phr" ~ "1-Default",
  Condition == "Col" ~ "2-Color",
  Condition == "Siz" ~ "3-Size"
)
)

graphics <- graphics %>% mutate(ConditionColor = case_when(
  Condition == "Col" ~ "1-Color",
  Condition == "Phr" ~ "2-Other",
  Condition == "Ctrl" ~ "2-Other",
  Condition == "Siz" ~ "2-Other"
)
)


graphics <- graphics %>% mutate(Condition1 = factor(
  case_when(
    Condition == "Ctrl" ~ 0,
    Condition == "Phr" ~ 1,
    Condition == "Col" ~ 2,
    Condition == "Siz" ~ 3)
)
)

graphics$Condition <- factor(graphics$Condition)

graphics$Condition <- relevel(graphics$Condition, ref = "Ctrl")

graphics <- graphics %>% mutate(Ctrl_dummy = 
                                  case_when(
                                    Condition == "Ctrl" ~ 1,
                                    Condition != "Ctrl" ~ 2)
)

graphics$ClustConf <- factor(graphics$ClustConf, levels=c("Very doubtful (0-25%)","Somewhat doubtful (26-50%)","Somewhat confident (51-75%)","Very confident (76-100%)"))

graphics$Demo.lang <- factor(graphics$Demo.lang)

graphics$Demo.educ <- factor(graphics$Demo.educ, 
                             levels = c("High School diploma",
                                        "Bachelor’s degree",
                                        "Master’s degree",
                                        "Professional degree",
                                        "Doctorate degree",
                                        "Other"),
                             ordered = TRUE)

freq4 <- c("None", "A little", "Some", "A lot", "Skipped")

graphics$Demo.expdataanal <- factor(graphics$Demo.expdataanal,
                                    levels = c(freq4),
                                    ordered = TRUE)

graphics$Demo.expdatavis <- factor(graphics$Demo.expdatavis,
                                   levels = c(freq4),
                                   ordered = TRUE)

graphics$Demo.expreadnetvis <- factor(graphics$Demo.expreadnetvis,
                                      levels = c(freq4),
                                      ordered = TRUE)

graphics <- graphics %>% mutate(Demo.expreadnetvis.alot = ifelse(Demo.expreadnetvis == "A lot",1,0))

graphics$Demo.expreadnetvis.alot <- factor(graphics$Demo.expreadnetvis.alot)

graphics <- graphics %>% mutate(Demo.expreadnetvis.none = ifelse(Demo.expreadnetvis == "None",1,0))

graphics$Demo.expreadnetvis.none <- factor(graphics$Demo.expreadnetvis.none)

graphics <- graphics %>% mutate(Demo.expreadnetvis.three = case_when(
  Demo.expreadnetvis == "None" ~ "None",
  Demo.expreadnetvis == "A little" ~ "A little",
  TRUE ~ "Other"
))

graphics$Demo.expreadnetvis.three <- factor(graphics$Demo.expreadnetvis.three)

graphics$Demo.expcreatenetvis <- factor(graphics$Demo.expcreatenetvis,
                                        levels = c(freq4),
                                        ordered = TRUE)

graphics <- graphics %>% mutate(Demo.expcreatenetvis.alot = ifelse(Demo.expcreatenetvis == "A lot",1,0))

graphics$Demo.expcreatenetvis.alot <- factor(graphics$Demo.expcreatenetvis.alot)


graphics <- graphics %>% mutate(Stats.OperatingSystemCombined = case_when(
  str_detect(Stats.OperatingSystem, "Android") ~ "Android",
  str_detect(Stats.OperatingSystem, "Windows") ~ "Windows",
  str_detect(Stats.OperatingSystem, "CrOS") ~ "CrOS",
  str_detect(Stats.OperatingSystem, "Linux") | str_detect(Stats.OperatingSystem, "Ubuntu") ~ "Linux/Ubuntu",
  TRUE ~ Stats.OperatingSystem)
)

graphics <- graphics %>% mutate(Stats.OperatingSystemCombined2 = case_when(
  str_detect(Stats.OperatingSystemCombined, "Android") | str_detect(Stats.OperatingSystemCombined, "iPhone") ~ "Android/iPhone",
  TRUE ~ Stats.OperatingSystemCombined)
)

graphics <- graphics %>% mutate(Stats.OperatingSystemCombined3 = case_when(
  str_detect(Stats.OperatingSystemCombined2, "Macintosh") | str_detect(Stats.OperatingSystemCombined2, "Windows") ~ Stats.OperatingSystemCombined2,
  TRUE ~ "Other")
)

graphics <- graphics %>% mutate(Stats.OperatingSystemCombined4 = case_when(
  str_detect(Stats.OperatingSystem, "Macintosh") | str_detect(Stats.OperatingSystem, "Windows") ~ "Mac/Windows",
  TRUE ~ "Other")
)

graphics <- graphics %>% mutate(Stats.OperatingSystemCombined5 = case_when(
  str_detect(Stats.OperatingSystem, "Macintosh") | str_detect(Stats.OperatingSystem, "Windows") | str_detect(Stats.OperatingSystem, "CrOS") ~ "Mac/Windows/CrOS",
  TRUE ~ "Other")
)

graphics <- graphics %>% mutate(Stats.OperatingSystemWindows = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "Windows",
  TRUE ~ "Other")
)

graphics <- graphics %>% mutate(Stats.OperatingSystemMacintosh = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "Macintosh",
  TRUE ~ "Other")
)

graphics <- graphics %>% mutate(Stats.OperatingSystemAndroid = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "Android",
  TRUE ~ "Other")
)

graphics <- graphics %>% mutate(Stats.OperatingSystemiPhone = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "iPhone",
  TRUE ~ "Other")
)

graphics <- graphics %>%  mutate(Stats.OperatingSystemNumClust = case_when(
  str_detect(Stats.OperatingSystem, "Android 6.0.1") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "CrOS x86_64 9592.96.0") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Linux x86_64") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Ubuntu") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Windows NT 10.0") ~ "1-HighSig",
  str_detect(Stats.OperatingSystem, "Windows NT 5.1") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Windows NT 6.0") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Windows NT 6.1") ~ "1-HighSig",
  TRUE ~ "Other"))

graphics <- graphics %>% mutate(Demo.acfieldGrouped = case_when(
  Demo.acfield %in% c("Anthropology","Arts","Classics","History","Languages","Literature","Philosophy","Religion") ~ "Humanities",
  Demo.acfield %in% c("Archaeology","Communication studies","Cultural and ethnic studies","Economics","Geography","Information science","Linguistics","Political science","Psychology","Sociology") ~ "Social sciences",
  Demo.acfield %in% c("Biology","Chemistry","Earth sciences","Physics","Space sciences") ~ "Life sciences",
  Demo.acfield %in% c("Mathematics","Computer sciences","Logic","Statistics","Systems science") ~ "Formal sciences",
  Demo.acfield %in% c("Architecture and design","Business","Divinity","Education","Engineering","Human physical performance and recreation","Journalism, media studies and communication","Law","Library and museum studies","Medicine","Military sciences","Public administration") ~ "Professional",
  TRUE ~ Demo.acfield
))

graphics <- graphics %>% mutate(Demo.acfieldGrouped2 = case_when(
  Demo.acfield %in% c("Architecture and design", "Arts", "Business","Earth sciences","Information science","Languages","Library and museum studies","Other","Political science","Psychology") ~ "SignificantGroup",
  TRUE ~ "ZZ-Etc."
))

graphics <- graphics %>% mutate(Demo.acfieldGrouped3 = case_when(
  Demo.acfield %in% c("Business", "Arts", "Computer sciences", "Economics", "Information science", "Law", "Linguistics", "Medicine", "Other", "Political science", "Skipped", "Sociology") ~ "SignificantGroup",
  TRUE ~ "ZZ-Etc."
))

graphics <- graphics %>% mutate(Overestimated = case_when(
  Underestimated == "over" ~ "1-Overestimated",
  Underestimated %in% c("correct","under") ~ "2-CorrectOrUnder"
))

graphics <- graphics %>% mutate(UnderestDummy = case_when(
  Underestimated == "under" ~ "1-Underestimated",
  Underestimated %in% c("correct","over") ~ "2-CorrectOrOver"
))

# separate datasets for each Task

graphics_avgdeg <- graphics %>% filter(Task == "AvgDeg")

graphics_bc <- graphics %>% filter(Task == "BC")

graphics_clickhighdeg <- graphics %>% filter(Task == "ClickHighDeg")

graphics_lgclust <- graphics %>% filter(Task == "LargeClust1")

graphics_numclust <- graphics %>% filter(Task == "NumClust")

graphics_numhighdeg <- graphics %>% filter(Task == "NumHighDegree")

graphics_numlinks <- graphics %>% filter(Task == "NumLinks")

graphics_numnodes <- graphics %>% filter(Task == "NumNodes")


# Setting up colors, etc., for lsmeans plots

sig.level.names <- c("p < .0001","p < .001","p < .01","p < .05","NS")
sig.colors <- c("gray20", "gray35", "gray50", "gray65", "white")
names(sig.colors) <- sig.level.names
star.colors <- c("gray20","white")
names(star.colors) <- c(TRUE, FALSE)


