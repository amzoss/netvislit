# Clear environment

rm(list=ls())

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

setwd("~/Box Sync/PhD-AngelaZoss/2-Experimental Studies/CombinedExperimental/Command Files/Results")

originalDataDir <- "../../Original Data"
analysisDataDir <- "../../Analysis Data"

generatedDataDir <- file.path(originalDataDir, "Generated data")

figureDir <- "../../Documents/"

# Setting up colors, etc., for lsmeans plots

sig.level.names <- c("p < .0001","p < .001","p < .01","p < .05","NS")
sig.colors <- c("gray20", "gray35", "gray50", "gray65", "white")
names(sig.colors) <- sig.level.names
star.colors <- c("gray20","white")
names(star.colors) <- c(TRUE, FALSE)

freq4 <- c("None", "A little", "Some", "A lot", "Skipped")

