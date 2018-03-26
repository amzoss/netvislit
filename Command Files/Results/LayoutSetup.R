# Detach previous packages

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:layouts","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
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
library(graphics)
library(knitr)

# mixed modeling

library(nlme)
library(lme4)
#library(car)
library(lmerTest)
#library(lsmeans)
library(emmeans)
library(multcompView)

library(lattice)
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
library(r2glmm)



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

originalDataDir <- "../../Original Data"
analysisDataDir <- "../../Analysis Data"

generatedDataDir <- file.path(originalDataDir, "Generated data")

figureDir <- "../../Documents/"

# Loading analysis data files

#TO DO : in previous scripts, save data with saveRDS

orig.layoutfac <- read_csv(file.path(analysisDataDir, "LayoutFac.csv"))

# Slight processing for analysis

layouts <- set_tidy_names(orig.layoutfac, syntactic=TRUE)

layouts$ClustConf <- factor(layouts$ClustConf,
                            levels = c("Very confident (76-100%)", 
                                       "Somewhat confident (51-75%)", 
                                       "Somewhat doubtful (26-50%)", 
                                       "Very doubtful (0-25%)"),
                            ordered = TRUE)

layouts$Dataset <- factor(layouts$Dataset, ordered = TRUE)

#Make control the reference group

layouts$Condition <- factor(layouts$Condition)

layouts$Condition <- relevel(layouts$Condition, ref = "Ctrl")

layouts <- layouts %>% mutate(Ctrl_dummy = 
                                case_when(
                                  Condition == "Ctrl" ~ 1,
                                  Condition != "Ctrl" ~ 2)
)

layouts$ClustConf <- factor(layouts$ClustConf, levels=c("Very doubtful (0-25%)","Somewhat doubtful (26-50%)","Somewhat confident (51-75%)","Very confident (76-100%)"))

layouts$Demo.lang <- factor(layouts$Demo.lang)

layouts$Demo.educ <- factor(layouts$Demo.educ, 
                            levels = c("High School diploma",
                                       "Bachelor’s degree",
                                       "Master’s degree",
                                       "Professional degree",
                                       "Doctorate degree",
                                       "Other"),
                            ordered = TRUE)

freq4 <- c("None", "A little", "Some", "A lot", "Skipped")

layouts$Demo.expdataanal <- factor(layouts$Demo.expdataanal,
                                   levels = c(freq4),
                                   ordered = TRUE)

layouts$Demo.expdatavis <- factor(layouts$Demo.expdatavis,
                                  levels = c(freq4),
                                  ordered = TRUE)

layouts$Demo.expreadnetvis <- factor(layouts$Demo.expreadnetvis,
                                     levels = c(freq4),
                                     ordered = TRUE)

layouts <- layouts %>% mutate(Demo.expreadnetvis.alot = ifelse(Demo.expreadnetvis == "A lot",1,0))

layouts$Demo.expreadnetvis.alot <- factor(layouts$Demo.expreadnetvis.alot)

layouts <- layouts %>% mutate(Demo.expreadnetvis.none = ifelse(Demo.expreadnetvis == "None",1,0))

layouts$Demo.expreadnetvis.none <- factor(layouts$Demo.expreadnetvis.none)

layouts <- layouts %>% mutate(Demo.expreadnetvis.three = case_when(
  Demo.expreadnetvis == "None" ~ "None",
  Demo.expreadnetvis == "A little" ~ "A little",
  TRUE ~ "Other"
))

layouts$Demo.expreadnetvis.three <- factor(layouts$Demo.expreadnetvis.three)

layouts$Demo.expcreatenetvis <- factor(layouts$Demo.expcreatenetvis,
                                       levels = c(freq4),
                                       ordered = TRUE)

layouts <- layouts %>% mutate(Demo.expcreatenetvis.alot = ifelse(Demo.expcreatenetvis == "A lot",1,0))

layouts$Demo.expcreatenetvis.alot <- factor(layouts$Demo.expcreatenetvis.alot)


layouts <- layouts %>% mutate(Stats.OperatingSystemCombined = case_when(
  str_detect(Stats.OperatingSystem, "Android") ~ "Android",
  str_detect(Stats.OperatingSystem, "Windows") ~ "Windows",
  str_detect(Stats.OperatingSystem, "CrOS") ~ "CrOS",
  str_detect(Stats.OperatingSystem, "Linux") | str_detect(Stats.OperatingSystem, "Ubuntu") ~ "Linux/Ubuntu",
  TRUE ~ Stats.OperatingSystem)
)

layouts <- layouts %>% mutate(Stats.OperatingSystemCombined2 = case_when(
  str_detect(Stats.OperatingSystemCombined, "Android") | str_detect(Stats.OperatingSystemCombined, "iPhone") ~ "Android/iPhone",
  TRUE ~ Stats.OperatingSystemCombined)
)

layouts <- layouts %>% mutate(Stats.OperatingSystemCombined3 = case_when(
  str_detect(Stats.OperatingSystemCombined2, "Macintosh") | str_detect(Stats.OperatingSystemCombined2, "Windows") ~ Stats.OperatingSystemCombined2,
  TRUE ~ "Other")
)

layouts <- layouts %>% mutate(Stats.OperatingSystemCombined4 = case_when(
  str_detect(Stats.OperatingSystem, "Macintosh") | str_detect(Stats.OperatingSystem, "Windows") ~ "Mac/Windows",
  TRUE ~ "Other")
)

layouts <- layouts %>% mutate(Stats.OperatingSystemCombined5 = case_when(
  str_detect(Stats.OperatingSystem, "Macintosh") | str_detect(Stats.OperatingSystem, "Windows") | str_detect(Stats.OperatingSystem, "CrOS") ~ "Mac/Windows/CrOS",
  TRUE ~ "Other")
)

layouts <- layouts %>% mutate(Stats.OperatingSystemWindows = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "Windows",
  TRUE ~ "Other")
)

layouts <- layouts %>% mutate(Stats.OperatingSystemMacintosh = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "Macintosh",
  TRUE ~ "Other")
)

layouts <- layouts %>% mutate(Stats.OperatingSystemAndroid = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "Android",
  TRUE ~ "Other")
)

layouts <- layouts %>% mutate(Stats.OperatingSystemiPhone = case_when(
  str_detect(Stats.OperatingSystem, "Windows") ~ "iPhone",
  TRUE ~ "Other")
)

layouts <- layouts %>%  mutate(Stats.OperatingSystemNumClust = case_when(
  str_detect(Stats.OperatingSystem, "Android 6.0.1") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "CrOS x86_64 9592.96.0") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Linux x86_64") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Ubuntu") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Windows NT 10.0") ~ "1-HighSig",
  str_detect(Stats.OperatingSystem, "Windows NT 5.1") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Windows NT 6.0") ~ "1-HighSig", 
  str_detect(Stats.OperatingSystem, "Windows NT 6.1") ~ "1-HighSig",
  TRUE ~ "Other"))

layouts <- layouts %>% mutate(Demo.acfieldGrouped = case_when(
  Demo.acfield %in% c("Anthropology","Arts","Classics","History","Languages","Literature","Philosophy","Religion") ~ "Humanities",
  Demo.acfield %in% c("Archaeology","Communication studies","Cultural and ethnic studies","Economics","Geolayouty","Information science","Linguistics","Political science","Psychology","Sociology") ~ "Social sciences",
  Demo.acfield %in% c("Biology","Chemistry","Earth sciences","Physics","Space sciences") ~ "Life sciences",
  Demo.acfield %in% c("Mathematics","Computer sciences","Logic","Statistics","Systems science") ~ "Formal sciences",
  Demo.acfield %in% c("Architecture and design","Business","Divinity","Education","Engineering","Human physical performance and recreation","Journalism, media studies and communication","Law","Library and museum studies","Medicine","Military sciences","Public administration") ~ "Professional",
  TRUE ~ Demo.acfield
))

layouts <- layouts %>% mutate(Demo.acfieldGrouped2 = case_when(
  Demo.acfield %in% c("Architecture and design", "Arts", "Business","Earth sciences","Information science","Languages","Library and museum studies","Other","Political science","Psychology") ~ "SignificantGroup",
  TRUE ~ "ZZ-Etc."
))

layouts <- layouts %>% mutate(Demo.acfieldGrouped3 = case_when(
  Demo.acfield %in% c("Business", "Arts", "Computer sciences", "Economics", "Information science", "Law", "Linguistics", "Medicine", "Other", "Political science", "Skipped", "Sociology") ~ "SignificantGroup",
  TRUE ~ "ZZ-Etc."
))

layouts <- layouts %>% mutate(Overestimated = case_when(
  Underestimated == "over" ~ "1-Overestimated",
  Underestimated %in% c("correct","under") ~ "2-CorrectOrUnder"
))

layouts <- layouts %>% mutate(UnderestDummy = case_when(
  Underestimated == "under" ~ "1-Underestimated",
  Underestimated %in% c("correct","over") ~ "2-CorrectOrOver"
))

# separate datasets for each Task

layouts_avgdeg <- layouts %>% filter(Task == "AvgDeg")

layouts_bc <- layouts %>% filter(Task == "BC")

layouts_clickhighdeg <- layouts %>% filter(Task == "ClickHighDeg")

layouts_lgclust <- layouts %>% filter(Task == "LargeClust1")

layouts_numclust <- layouts %>% filter(Task == "NumClust")

layouts_numhighdeg <- layouts %>% filter(Task == "NumHighDegree")

layouts_numlinks <- layouts %>% filter(Task == "NumLinks")

layouts_numnodes <- layouts %>% filter(Task == "NumNodes")


# Setting up colors, etc., for lsmeans plots

sig.level.names <- c("p < .0001","p < .001","p < .01","p < .05","NS")
sig.colors <- c("gray20", "gray35", "gray50", "gray65", "white")
names(sig.colors) <- sig.level.names
star.colors <- c("gray20","white")
names(star.colors) <- c(TRUE, FALSE)


