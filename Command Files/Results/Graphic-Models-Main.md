Graphics Models for All Tasks
================
Angela Zoss
March 23, 2018

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

## Summary

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics <- readRDS(file.path(analysisDataDir, "Graphics.rds"))
```

``` r
graphics %>% filter(Task %in% c("AvgDeg","NumClust","NumHighDegree","NumLinks","NumNodes")) %>% 
  ggplot(aes(Task, LogError)) + geom_boxplot() + #scale_y_log10() +
  labs(title="Distribution of LogError for Numerical Response Tasks,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave(file.path(figureDir, "logerrortasksgraphics.pdf"), width=10, height=6)

graphics %>% filter(Task %in% c("AvgDeg","NumClust","NumHighDegree","NumLinks","NumNodes")) %>% 
  ggplot(aes(Dataset, Percentage)) + geom_boxplot() + #scale_y_log10() +
  labs(title="Distribution of Percentage for Numerical Response Tasks,\ngraphics conditions") +
  facet_wrap(~Task) + scale_y_log10()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
graphics %>% filter(Task %in% c("AvgDeg","NumClust","NumHighDegree","NumLinks","NumNodes")) %>% 
  ggplot(aes(Dataset, abs(Response - CorrectAnswer)/CorrectAnswer)) + geom_violin() + #scale_y_log10() +
  labs(title="Error as Percentage of Correct Answer for Numerical Response Tasks,\ngraphics conditions") +
  facet_wrap(~Task) + scale_y_log10(breaks=c(.1,10,1000),labels=c("10%","1,000%","100,000%")) +
  geom_hline(yintercept = 1)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
graphics %>% filter(Task %in% c("AvgDeg","NumClust","NumHighDegree","NumLinks","NumNodes")) %>%
ggplot() + geom_violin(aes(Dataset,Response)) + geom_point(data=graphics %>% filter(Task %in% c("AvgDeg","NumClust","NumHighDegree","NumLinks","NumNodes")) %>% group_by(Task, Dataset) %>% summarise(Correct=mean(CorrectAnswer)), aes(Dataset,Correct)) +
facet_grid(.~Task) + scale_y_log10()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
graphics %>% filter(Task %in% c("AvgDeg","NumClust","NumHighDegree","NumLinks","NumNodes")) %>%
    ggplot() + geom_boxplot(aes(Dataset,LogError)) +
    facet_grid(.~Task) + labs(title="LogError distributions by Task and Dataset, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

``` r
ggsave(file.path(figureDir, "logerrortasksdatasetsgraphics.pdf"), width=10, height=6)
```

## Mixed Models

### Average Degree

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_avgdeg <- readRDS(file.path(analysisDataDir, "GraphicsAvgDeg.rds"))
```

#### nlme

``` r
# https://iucat.iu.edu/catalog/14518998, chapters 3 and 5
# data at http://www-personal.umich.edu/~bwest/almmussp.html

# testing models, no nesting/grouping

# Repeated measures, tasks and datasets (crossed)

# starting with older package (nlme), with function lme()

# R by default treats the lowest category (alphabetically or numerically) of a
# categorical fixed factor as the reference category in a model

# We don't really have reference categories for the repeated measures factors, 
# though, so I guess it doesn't matter?  Maybe want to exclude training dataset

# Note about syntax; can use either ":" or "*" for interaction, but using "*" automatically adds
# the main effects of each factor into the model, too, instead of just the interaction

graph.avgdeg.lme <- lme(LogError ~ Dataset, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)


# Dataset is not significant for AvgDeg; try Condition

graph.avgdeg.lme <- lme(LogError ~ Condition, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Condition is not significant; try ConditionPhrasing

graph.avgdeg.lme <- lme(LogError ~ ConditionPhrasing, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# ConditionPhrasing is not significant; try ConditionGraphics

graph.avgdeg.lme <- lme(LogError ~ ConditionGraphics, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# ConditionGraphics is not significant; try TaskOrder

graph.avgdeg.lme <- lme(LogError ~ TaskOrder, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# TaskOrder is not significant; try DatasetOrder

graph.avgdeg.lme <- lme(LogError ~ DatasetOrder, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# DatasetOrder is not significant; try DatasetDuration

graph.avgdeg.lme <- lme(LogError ~ DatasetDuration, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(DatasetDuration))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# DatasetDuration is not significant; try Stats.Q_TotalDuration

graph.avgdeg.lme <- lme(LogError ~ Stats.Q_TotalDuration, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Stats.Q_TotalDuration is not significant; try Stats.OperatingSystem

graph.avgdeg.lme <- lme(LogError ~ Stats.OperatingSystem, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Stats.OperatingSystem is not significant; try StatsNumPixels

graph.avgdeg.lme <- lme(LogError ~ StatsNumPixels, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# StatsNumPixels is not significant; try Demo.age

graph.avgdeg.lme <- lme(LogError ~ Demo.age, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.age))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.age is not significant; try Demo.gender

graph.avgdeg.lme <- lme(LogError ~ Demo.gender, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.gender))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.gender is not significant; try Demo.lang

graph.avgdeg.lme <- lme(LogError ~ Demo.lang, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.lang))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.lang is not significant; try Demo.educ

graph.avgdeg.lme <- lme(LogError ~ Demo.educ, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.educ))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.educ is not significant; try Demo.acfield

graph.avgdeg.lme <- lme(LogError ~ Demo.acfield, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.acfield))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.acfield is not significant; try Demo.dailytech_Computer

graph.avgdeg.lme <- lme(LogError ~ Demo.dailytech_Computer, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_Computer))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.dailytech_Computer is not significant; try Demo.dailytech_Tablet

graph.avgdeg.lme <- lme(LogError ~ Demo.dailytech_Tablet, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_Tablet))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.dailytech_Tablet is not significant; try Demo.dailytech_SmartPhone

graph.avgdeg.lme.SP <- lme(LogError ~ Demo.dailytech_SmartPhone, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))))

# TO DO : figure out if SmartPhone should be a factor

summary(graph.avgdeg.lme.SP)

anova(graph.avgdeg.lme.SP)

## Demo.dailytech_SmartPhone *is* significant

ggplot(graphics_avgdeg) +
  geom_point(aes(Demo.dailytech_SmartPhone, LogError)) +
  geom_smooth(aes(Demo.dailytech_SmartPhone, LogError), method="lm")

# try Demo.weeklygaming

graph.avgdeg.lme <- lme(LogError ~ Demo.weeklygaming, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.weeklygaming))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.weeklygaming not significant; try Demo.expdataanal

graph.avgdeg.lme <- lme(LogError ~ Demo.expdataanal, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.expdataanal))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.expdataanal not significant; try Demo.expdatavis

graph.avgdeg.lme <- lme(LogError ~ Demo.expdatavis, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.expdatavis))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.expdatavis not significant; try Demo.expreadnetvis

graph.avgdeg.lme.RNV <- lme(LogError ~ Demo.expreadnetvis, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.expreadnetvis))))

summary(graph.avgdeg.lme.RNV)

anova(graph.avgdeg.lme.RNV)

# Demo.expreadnetvis barely significant (p=0.0494); try just the experts (only 1, actually)

graph.avgdeg.lme.RNVAL <- lme(LogError ~ Demo.expreadnetvis.alot, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.expreadnetvis.alot))))

summary(graph.avgdeg.lme.RNVAL)

anova(graph.avgdeg.lme.RNVAL)

# better, but still barely significant; p = 0.0106

# try Demo.expcreatenetvis

graph.avgdeg.lme <- lme(LogError ~ Demo.expcreatenetvis, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.expcreatenetvis))))

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Demo.expcreatenetvis not significant; try AvgDeg

graph.avgdeg.lme <- lme(LogError ~ AvgDeg, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# AvgDeg not significant; try Density

graph.avgdeg.lme <- lme(LogError ~ Density, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Density not significant; try LargeClust1

graph.avgdeg.lme <- lme(LogError ~ LargeClust1, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# LargeClust1 not significant; try Modularity

graph.avgdeg.lme <- lme(LogError ~ Modularity, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Modularity not significant; try NumClust

graph.avgdeg.lme <- lme(LogError ~ NumClust, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# NumClust not significant; try NumHighDegree

graph.avgdeg.lme <- lme(LogError ~ NumHighDegree, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# NumHighDegree not significant; try NumLinks

graph.avgdeg.lme <- lme(LogError ~ NumLinks, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# NumLinks not significant; try NumNodes

graph.avgdeg.lme <- lme(LogError ~ NumNodes, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# NumNodes not significant; try NumNodesClust1

graph.avgdeg.lme <- lme(LogError ~ NumNodesClust1, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg)

summary(graph.avgdeg.lme)

anova(graph.avgdeg.lme)

# Final; try both significant factors

graph.avgdeg.lme.SP.RNVAL <- lme(LogError ~ Demo.dailytech_SmartPhone + Demo.expreadnetvis.alot, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))) %>% filter(!(is.na(Demo.expreadnetvis.alot))))

summary(graph.avgdeg.lme.SP.RNVAL)

anova(graph.avgdeg.lme.SP.RNVAL)

# Better than just one predictor?

anova(graph.avgdeg.lme.SP.RNVAL, graph.avgdeg.lme.SP)

# no significant difference

graph.avgdeg.lme.RNVAL <- lme(LogError ~ Demo.expreadnetvis.alot, random = ~ 1 | Demo.ResponseID, method="REML", data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))) %>% filter(!(is.na(Demo.expreadnetvis.alot))))

anova(graph.avgdeg.lme.SP.RNVAL, graph.avgdeg.lme.RNVAL)

# no significant difference; guess we can use both predictors? or maybe should just use stronger
# predictor, SmartPhone?


random.effects(graph.avgdeg.lme.SP)

# do we really need random effects?  test model without random effects using gls

graph.avgdeg.gls <- gls(LogError ~ Demo.dailytech_SmartPhone, data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))))

anova(graph.avgdeg.lme.SP, graph.avgdeg.gls)

# result of anova is significant (p=0.0019), indicating that there is a significant difference when
# you remove random effects, so should leave them in
```

``` r
# NOTE: nesting is not appropriate, but syntax for tests might be useful in future

# model with nesting; takes *forever* but does finish
# can nest when I recode Condition as 0, 1, 2, 3 (Condition1), 
# but if I just list Ctrl as the ref (Condition), won't converge

model.lme.update.nest <- update(model.lme, random = ~ Condition1 | Demo.ResponseID)

summary(model.lme.update.nest)

anova(model.lme.update.nest)

# might want to check whether nesting makes a difference, but can't run anova because 
# "the test statistic has a null distribution that is a mixture of X12 and X22 distributions
# with equal weights of 0.5, so the anova() function cannot be used for the p-value." (p.220)

# Check summary of each model for -2 REML log-likelihood values 

# "logLik" value for unnested: -14415.3
# "logLik" value for nested: -14441
# -2 REML logLik is just -2 * the logLik value

# test statistics = unnested - nested
test.stat <- abs((-2*-14415.3) - (-2*-14441)) # ~51.4

# note: when test.stat was negative, p-value was not significant, but highly significant when
# positive
p.val <- 0.5*(1-pchisq(test.stat,1)) + 0.5*(1-pchisq(test.stat,2))

# p-value is < 0.001, so nesting is significantly different
```

``` r
# adding weights to have a separate residual variance for each treatment group
# TO DO: are these the only weights I need to consider???  check that these weights make sense

graph.avgdeg.weight <- lme(LogError ~ Demo.dailytech_SmartPhone, 
                 random = ~ 1 | Demo.ResponseID, 
                 method="REML", 
                 data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))),
                 weights = varIdent(form = ~1 | Condition))

summary(graph.avgdeg.weight)

# Parameter estimates are:
#    Ctrl      Siz      Phr      Col 
#1.000000 1.486356 1.409153 1.436532 

anova(graph.avgdeg.lme.SP, graph.avgdeg.weight)

# result is significant (p<.0001), which suggests that we should retain the second model with the 
# weights,  otherwise known as the "heterogeneous variances model" (p. 94)
```

``` r
graph.avgdeg.weight.pooled <- lme(LogError ~ Demo.dailytech_SmartPhone, 
                 random = ~ 1 | Demo.ResponseID, 
                 method="REML", 
                 data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))),
                 weights = varIdent(form = ~1 | Ctrl_dummy))

anova(graph.avgdeg.weight, graph.avgdeg.weight.pooled)

# test is not significant, so should not keep the weights pooled? p. 95 not clear, since in 
# example difference is not significant but pooled model is retained anyway

summary(graph.avgdeg.weight)
anova(graph.avgdeg.weight)
```

``` r
# TO DO: need to conduct Type I F-test (section 5.7)? to try to remove nonsignficant fixed effects?
# note p. 223 explains that large F-statistics can indicate significance, even when you don't compute
# p-values

# another way to test if each additional item is significant:
# create two models, one with and one without the fixed effect
# use ML estimation (REML = F for lmer()) for each
# use anova() to compare the models (see 4.4.3.2)

# On p. 238, talks about needing to be careful about interpretation because an interaction is
# also significant; can investigate "estimated marginal means" of the combinations of levels in the
# interaction (needs to be pairwise for the interaction?)

# On p. 133, "Post-hoc comparisons can also be computed in R with 
# some additional programming (Faraway, 2005)"

# From Faraway, 2015, p. 20:
# https://search.library.duke.edu/search?id=DUKE008022150

# "We can extract the regression quantities we need from the model object. Commonly used are residuals(),
# fitted(), df.residual() which gives the degrees of freedon, deviance() which gives the RSS and
# coef() which gives the ^Beta."

modsum <- summary(graph.avgdeg.weight)
names(modsum)
modsum$logLik * -2

modsum$sigma # 0.711... what is sigma, exactly?
```

``` r
# Residuals plot
plot(graph.avgdeg.weight)

# Residuals vs. SmartPhone, scatterplot
plot(graph.avgdeg.weight, Demo.dailytech_SmartPhone ~ resid(.))

# Predicted vs. SmartPhone
plot(graph.avgdeg.weight, Demo.dailytech_SmartPhone ~ fitted(.))


# Predicted vs. Actual LogError
plot(graph.avgdeg.weight, LogError ~ fitted(.), abline = c(0,1))

qqnorm(graph.avgdeg.weight, abline = c(0,1))

qqnorm(graph.avgdeg.weight, ~ resid(., type = "p") | Demo.dailytech_SmartPhone, abline = c(0,1))

qqnorm(graph.avgdeg.weight, ~ resid(., type = "p") | Condition, abline = c(0,1))

qqnorm(graph.avgdeg.weight, ~ranef(.))
```

#### lme4

``` r
# Trying dataset first

graph.avgdeg.lmer <- lmer(LogError ~ Dataset + (1|Demo.ResponseID), data = graphics_avgdeg, REML = T)

lmsum <- summary(graph.avgdeg.lmer)
lmsum
#names(lmsum)

anova(graph.avgdeg.lmer)

# Dataset not significant; trying SmortPhone

graph.avgdeg.lmer.SP <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))), REML = T)

summary(graph.avgdeg.lmer.SP)

anova(graph.avgdeg.lmer.SP)

# Smartphone is significant; trying readnetvisalot

graph.avgdeg.lmer.RNVAL <- lmer(LogError ~ Demo.expreadnetvis.alot + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expreadnetvis.alot))), REML = T)

summary(graph.avgdeg.lmer.RNVAL)

anova(graph.avgdeg.lmer.RNVAL)

# slightly significant; trying both

graph.avgdeg.lmer.SP.RNVAL <- lmer(LogError ~ Demo.dailytech_SmartPhone + Demo.expreadnetvis.alot + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))) %>% filter(!(is.na(Demo.expreadnetvis.alot))), REML = T)

summary(graph.avgdeg.lmer.SP.RNVAL)

anova(graph.avgdeg.lmer.SP.RNVAL)
```

``` r
# Condition

graph.avgdeg.lmer <- lmer(LogError ~ Condition + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Condition is not significant; trying Ctrl_dummy

graph.avgdeg.lmer <- lmer(LogError ~ Ctrl_dummy + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Ctrl_dummy is not significant; trying ConditionPhrasing

graph.avgdeg.lmer <- lmer(LogError ~ ConditionPhrasing + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# ConditionPhrasing is not significant

graph.avgdeg.lmer <- lmer(LogError ~ ConditionGraphics + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# ConditionGraphics is not significant

graph.avgdeg.lmer <- lmer(LogError ~ ConditionColor + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# ConditionColor is not significant

# trying Dataset

graph.avgdeg.lmer <- lmer(LogError ~ Dataset + (1|Demo.ResponseID), data = graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Dataset is not significant

#graph.avgdeg.lmer <- lmer(LogError ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

graphics_avgdeg.CS <- graphics_avgdeg %>% mutate(QuestionOrder=scale(QuestionOrder))

graph.avgdeg.lmer <- lmer(LogError ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_avgdeg.CS, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# QuestionOrder is not significant

graph.avgdeg.lmer <- lmer(LogError ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# DatasetOrder is not significant

graph.avgdeg.lmer <- lmer(LogError ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# DatasetDuration is not significant

graph.avgdeg.lmer <- lmer(LogError ~ DatasetStartTime + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# DatasetStartTime is not significant; trying TaskOrder

graph.avgdeg.lmer <- lmer(LogError ~ TaskOrder + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# TaskOrder is not significant; trying CorrectAnswer

graph.avgdeg.lmer <- lmer(LogError ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# CorrectAnswer is not significant

graph.avgdeg.lmer.underest <- lmer(LogError ~ Underestimated + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer.underest)

anova(graph.avgdeg.lmer.underest)

# Underestimated is highly significant (p < 2.2e-16)

graph.avgdeg.lmer <- lmer(LogError ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Stats.Q_TotalDuration is not significant; trying Stats.dataset_count

graph.avgdeg.lmer <- lmer(LogError ~ Stats.dataset_count + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Stats.dataset_count is not significant; trying Stats.OperatingSystem

graph.avgdeg.lmer <- lmer(LogError ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Stats.OperatingSystem is not significant

graph.avgdeg.lmer <- lmer(LogError ~ Stats.OperatingSystemCombined + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Stats.OperatingSystemCombined is not significant

graph.avgdeg.lmer <- lmer(LogError ~ Stats.OperatingSystemiPhone + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Stats.OperatingSystemiPhone is not significant

graphics_avgdeg.CS <- graphics_avgdeg %>% mutate(StatsNumPixels=scale(StatsNumPixels))

graph.avgdeg.lmer <- lmer(LogError ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_avgdeg.CS, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# StatsNumPixels is not significant; trying Demo.age

graph.avgdeg.lmer <- lmer(LogError ~ Demo.age + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.age))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.age is not significant; trying Demo.gender

graph.avgdeg.lmer <- lmer(LogError ~ Demo.gender + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.gender))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.gender is not signficant; trying Demo.lang

graph.avgdeg.lmer <- lmer(LogError ~ Demo.lang + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.lang))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.lang is not signficant; trying Demo.educ

graph.avgdeg.lmer <- lmer(LogError ~ Demo.educ + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.educ))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.educ is not signficant; trying Demo.acfield

graph.avgdeg.lmer <- lmer(LogError ~ Demo.acfield + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.acfield))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.acfield is not signficant overall; trying Demo.acfieldGrouped

graph.avgdeg.lmer <- lmer(LogError ~ Demo.acfieldGrouped + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.acfieldGrouped))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.acfieldGrouped is not significant

graph.avgdeg.lmer <- lmer(LogError ~ Demo.acfieldGrouped2 + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.acfieldGrouped2))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.acfieldGrouped2 is not significant

graph.avgdeg.lmer <- lmer(LogError ~ Demo.acfieldGrouped3 + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.acfieldGrouped3))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.acfieldGrouped3 is not signficant

graph.avgdeg.lmer <- lmer(LogError ~ Demo.dailytech_Computer + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_Computer))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.dailytech_Computer is not significant; trying Demo.dailytech_Tablet

graph.avgdeg.lmer <- lmer(LogError ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_Tablet))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.dailytech_Computer is not significant

graph.avgdeg.lmer.SP <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))), REML = T)

summary(graph.avgdeg.lmer.SP)

anova(graph.avgdeg.lmer.SP)

# Demo.dailytech_SmartPhone is significant

graph.avgdeg.lmer <- lmer(LogError ~ Demo.weeklygaming + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.weeklygaming))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.weeklygaming is not significant; trying Demo.expdataanal

graph.avgdeg.lmer <- lmer(LogError ~ Demo.expdataanal + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expdataanal))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.expdataanal is not significant; trying Demo.expdatavis

graph.avgdeg.lmer <- lmer(LogError ~ Demo.expdatavis + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expdatavis))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.expdatavis is not significant; trying Demo.expreadnetvis

graph.avgdeg.lmer.ERNV <- lmer(LogError ~ Demo.expreadnetvis + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expreadnetvis))), REML = T)

summary(graph.avgdeg.lmer.ERNV)

anova(graph.avgdeg.lmer.ERNV)

# Demo.expreadnetvis is somewhat significant (p=0.04943), but very small numbers in some categories

graph.avgdeg.lmer <- lmer(LogError ~ Demo.expreadnetvis.none + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expreadnetvis.none))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.expreadnetvis.none is not significant

graph.avgdeg.lmer <- lmer(LogError ~ Demo.expreadnetvis.three + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expreadnetvis.three))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.expreadnetvis.three is not significant; 

graph.avgdeg.lmer <- lmer(LogError ~ Demo.expcreatenetvis + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expcreatenetvis))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.expcreatenetvis is not significant; trying Demo.expcreatenetvis.alot

graph.avgdeg.lmer <- lmer(LogError ~ Demo.expcreatenetvis.alot + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!(is.na(Demo.expcreatenetvis.alot))), REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Demo.expcreatenetvis.alot is not significant; trying AvgDeg

graph.avgdeg.lmer <- lmer(LogError ~ AvgDeg + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# AvgDeg is not significant

graph.avgdeg.lmer <- lmer(LogError ~ Density + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Density is not significant

graph.avgdeg.lmer <- lmer(LogError ~ LargeClust1 + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# LargeClust1 is note significant

graph.avgdeg.lmer <- lmer(LogError ~ Modularity + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# Modularity is not significant

graph.avgdeg.lmer <- lmer(LogError ~ NumClust + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# NumClust is not significant

graph.avgdeg.lmer <- lmer(LogError ~ NumHighDegree + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# NumHighDegree is not significant

graph.avgdeg.lmer <- lmer(LogError ~ NumLinks + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# NumLinks is not significant

graph.avgdeg.lmer <- lmer(LogError ~ NumNodes + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# NumNodes is not significant

graph.avgdeg.lmer <- lmer(LogError ~ NumNodesClust1 + (1|Demo.ResponseID), data=graphics_avgdeg, REML = T)

summary(graph.avgdeg.lmer)

anova(graph.avgdeg.lmer)

# NumNodesClust1 is not significant
```

``` r
temp <- graphics_avgdeg %>% dplyr::select(Demo.ResponseID, LogError, Underestimated, Demo.dailytech_SmartPhone) %>% drop_na()
```

Run this
one

``` r
graph.avgdeg.lmer.SP <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=temp, REML = T)

graph.avgdeg.lmer.underest <- lmer(LogError ~ Underestimated + (1|Demo.ResponseID), data=temp, REML = T)

graph.avgdeg.lmer.full <- lmer(LogError ~ Underestimated + Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.avgdeg.lmer.full)

anova(graph.avgdeg.lmer.full)

anova(graph.avgdeg.lmer.full, graph.avgdeg.lmer.SP)
anova(graph.avgdeg.lmer.full, graph.avgdeg.lmer.underest)
# keep full

graph.avgdeg.lmer.int <- lmer(LogError ~ Underestimated + Demo.dailytech_SmartPhone + Underestimated:Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.avgdeg.lmer.int)

anova(graph.avgdeg.lmer.int)

anova(graph.avgdeg.lmer.full, graph.avgdeg.lmer.int)
# interaction not significantly better; keep full
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.avgdeg.lmer.full, file = file.path(analysisDataDir,"fits/graph_avgdeg_lmer_full.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_avgdeg_lmer_full.RData"))
```

``` r
rand(graph.avgdeg.lmer.full)
```

    ## Analysis of Random effects Table:
    ##                 Chi.sq Chi.DF p.value  
    ## Demo.ResponseID    3.4      1    0.07 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# result shows that random effects of participant are not significant (p=0.07), but going to keep anyway(?)

anova(graph.avgdeg.lmer.full)
```

    ## Analysis of Variance Table of type III  with  Satterthwaite 
    ## approximation for degrees of freedom
    ##                             Sum Sq  Mean Sq NumDF   DenDF F.value  Pr(>F)
    ## Underestimated            0.203992 0.203992     1 1086.56  90.316 < 2e-16
    ## Demo.dailytech_SmartPhone 0.012225 0.012225     1  388.13   5.413 0.02051
    ##                              
    ## Underestimated            ***
    ## Demo.dailytech_SmartPhone *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#ranef(graph.avgdeg.lmer.full)

# displays the random effects; not that useful

# unlike lme(), lmer() doesn't allow for heterogeneous error variance structures (the "weights")

ggplot(graphics_avgdeg) + geom_histogram(aes(LogError), binwidth=.005) + labs(title="Distribution of LogError values for Average Degree task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggsave(file.path(figureDir, "logerroravgdeggraphics.pdf"), width=5, height=3)


(r2nsj = r2beta(graph.avgdeg.lmer.full, method = 'nsj', partial = TRUE))
```

    ##                      Effect   Rsq upper.CL lower.CL
    ## 1                     Model 0.084    0.117    0.057
    ## 2       Underestimatedunder 0.074    0.105    0.048
    ## 3 Demo.dailytech_SmartPhone 0.005    0.017    0.000

``` r
(r2nsj = r2beta(graph.avgdeg.lmer.full, method = 'nsj', partial = TRUE))[1,'Rsq']
```

    ## [1] 0.08395571

``` r
# From Faraway (2015), p. 156 - use step() to start with a complex model and 
# systematically remove each effects

graph.avgdeg.full.lmer <- lmer(LogError ~ Dataset + Condition + 
                           QuestionOrder + (1|Demo.ResponseID), 
                         data = graphics_avgdeg, REML = T)

step(graph.avgdeg.full.lmer)

# not sure this is useful
```

``` r
# Clustered Longitudinal Data (Chapter 7)
# Data are not clustered, so can skip

#model7.1.fit <- lme(gcf ~ time + 
#                   base_gcf + cda + 
#                   age + 
#                   time:base_gcf + time:cda + 
#                   time:age, 
#                   random = list(patient = ~time, tooth = ~1), 
#                   data = veneer, 
#                   method = "REML")

clust.model.fit <- lme(LogError ~ Task + TaskOrder +
                         Dataset + DatasetOrder +
                         Task:TaskOrder + 
                         Task:Dataset + Task:DatasetOrder,
                       random = list(`Demo.ResponseID` = ~Condition),
                       data = graphics_avgdeg,
                       method = "REML")

summary(clust.model.fit)

intervals(clust.model.fit, which="fixed")

random.effects(clust.model.fit)
```

``` r
plot(graph.avgdeg.lmer.full)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
plot(graph.avgdeg.lmer.full, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

``` r
plot(graph.avgdeg.lmer.full, resid(.) ~ fitted(.) | Underestimated, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->

``` r
plot(graph.avgdeg.lmer.full, resid(., scaled=TRUE) ~ fitted(.) | Underestimated, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-24-4.png)<!-- -->

``` r
plot(graph.avgdeg.lmer.full, LogError ~ fitted(.), abline = c(0,1))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-24-5.png)<!-- -->

``` r
graph.avgdeg.lmer.full.f <- fortify(graph.avgdeg.lmer.full)

ggplot(graph.avgdeg.lmer.full.f) +
  geom_density(aes(.fitted))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
ggplot(graph.avgdeg.lmer.full.f, aes(.fitted,.resid)) + 
  geom_point() +
  #facet_grid(.~Sex) + 
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

``` r
ggplot(graph.avgdeg.lmer.full.f, aes(LogError,.fitted)) + 
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0)) +
  scale_y_continuous(limits=c(0,0.3)) +
  labs(title="Real vs. Predicted LogError values for Average Degree\ntask, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->

``` r
ggsave(file.path(figureDir, "fittedpredictavgdeggraphics.pdf"), width=5, height=3)
```

``` r
# TO DO: check out interpretation for these plots??

library(lattice)

prof <-  profile(graph.avgdeg.lmer.full, optimizer="Nelder_Mead", which="beta_")

prof.CI <- confint(prof)

#CI2 <- confint(graph.avgdeg.lmer.SP, maxpts = 8)

xyplot(prof)

xyplot(prof, absVal = TRUE)

xyplot(prof, conf = c(0.95, 0.99), main = "95% and 99% profile() intervals")

# can also apply logProf() and varianceProf() to profile object

densityplot(prof)

splom(prof)
```

#### Logistic regression for over/underestimation with mixed effects (lme4, glmer)

``` r
# https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/

#ggpairs(graphics_avgdeg %>% dplyr::select(Response, DatasetDuration, CorrectAnswer, AbsDifference, Stats.Q_TotalDuration, StatsNumPixels, Demo.age, Demo.dailytech_Computer, Demo.dailytech_Tablet, Demo.dailytech_SmartPhone, Demo.weeklygaming, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumHighDegree, NumLinks, NumNodes, NumNodes))
```

``` r
graphics_avgdeg_incorrect <- graphics_avgdeg %>% filter(Underestimated != "correct")

graphics_avgdeg_incorrect$Underestimated <- factor(graphics_avgdeg_incorrect$Underestimated)

graph.avgdeg.logit <- glmer(Underestimated ~ Dataset + Condition +
    (1 | Demo.ResponseID), data = graphics_avgdeg_incorrect, family = binomial, control = glmerControl(optimizer = "bobyqa"),
    nAGQ = 10)

print(graph.avgdeg.logit, corr = FALSE)

se <- sqrt(diag(vcov(graph.avgdeg.logit)))

# table of estimates with 95% CI using coefficients
(tab <- cbind(Est = fixef(graph.avgdeg.logit), LL = fixef(graph.avgdeg.logit) - 1.96 * se, UL = fixef(graph.avgdeg.logit) + 1.96 * se))

# odds ratios instead of coefficients
exp(tab)
```

``` r
# bootstrapping, but sampling function isn't working

sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
    cid <- unique(dat[, clustervar[1]])
    ncid <- length(cid)
    recid <- sample(cid, size = ncid * reps, replace = TRUE)
    if (replace) {
        rid <- lapply(seq_along(recid), function(i) {
            cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
        })
    } else {
        rid <- lapply(seq_along(recid), function(i) {
            cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
        })
    }
    dat <- as.data.frame(do.call(rbind, rid))
    dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
        labels = FALSE))
    dat$NewID <- factor(dat$NewID)
    return(dat)
}

set.seed(20)
tmp <- sampler(graphics_avgdeg_incorrect, "Demo.ResponseID", reps = 1000)
bigdata <- cbind(tmp, graphics_avgdeg_incorrect[tmp$RowID, ])

# sampler throws an error:
# "Error in Ops.data.frame(dat[, clustervar], recid[i]) : ‘==’ only defined for equally-sized data frames"

f <- fixef(graph.avgdeg.logit)
r <- getME(graph.avgdeg.logit, "theta")

cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))

myboot <- function(i) {
    object <- try(glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay +
        Experience + (1 | NewID), data = bigdata, subset = Replicate == i, family = binomial,
        nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
    if (class(object) == "try-error")
        return(object)
    c(fixef(object), getME(object, "theta"))
}

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

# calculate proportion of models that successfully converged
success <- sapply(res, is.numeric)
mean(success)

# combine successful results
bigres <- do.call(cbind, res[success])

# calculate 2.5th and 97.5th percentiles for 95% CI
(ci <- t(apply(bigres, 1, quantile, probs = c(0.025, 0.975))))

# All results
finaltable <- cbind(Est = c(f, r), SE = c(se, NA), BootMean = rowMeans(bigres),
    ci)
# round and print
round(finaltable, 3)
```

``` r
# temporary data
tmpdat <- graphics_avgdeg_incorrect %>% filter(!is.na(DatasetDuration))

jvalues <- with(graphics_avgdeg_incorrect, seq(from = min(DatasetDuration), to = max(DatasetDuration), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
    tmpdat$LengthofStay <- j
    predict(m, newdata = tmpdat, type = "response")
})
```

``` r
# OLD CODE, NOT MIXED EFFECTS

graphics.est <- graphics_avgdeg %>% mutate(underest = ifelse(RawDifference<0,1,0)) 

graphics.est$underest <- factor(graphics.est$underest, levels=c(1,0), labels = c("underestimated","overestimated"))

graphics.est$Task <- factor(graphics.est$Task)

xtabs(~underest + Task, data = graphics.est)

# difference between family="binomial" and family=binomial(link='logit') ?

#log.model <- glm(underest ~ Task, family=binomial(link='logit'), data=graphics.est)

log.model <- glm(underest ~ Task, family="binomial", data=graphics.est)

summary(log.model)
# Interpretation of Estimate: compared to reference task (AvgDegree), each task changes
# the log odds of underestimation by the estimate shown; negative estimates reduce
# log odds of underestimation compared to AvgDegree

## CIs using profiled log-likelihood
confint(log.model)

## CIs using standard errors
confint.default(log.model)

## Wald test for effect of Task, which is terms 2:8 in the model
wald.test(b = coef(log.model), Sigma = vcov(log.model), Terms = 2:8)

## test two specific tasks against each other (NumHighDegree and NumNodes)
l <- cbind(0,0,0,0,0,1,0,-1)
wald.test(b = coef(log.model), Sigma = vcov(log.model), L = l)
# p = 0.015, so only marginally significant

## odds ratios only
exp(coef(log.model))

## odds ratios and 95% CI
exp(cbind(OR = coef(log.model), confint(log.model)))

# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/ 

## testing fit

# likelihood ratio test
with(log.model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# p-value is 0, so model with predictor definitely fits better than the null model

# testing the quality of the variable, is this the best model we can get?

# is this a good fitting model?  check % accuracy; split data into training test, check accuracy on test

anova(log.model, test="Chisq")

pR2(log.model)

# don't know how to interpret; should be looking at McFadden as a sort of pseudo R^2?

# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
# "If comparing two models on the same data, McFadden’s would be higher for the model with the greater likelihood."
```

#### Negative binomial model for responses

Responses are basically count data, so using a negative binomial
distribution to model. Negative binomial is especially useful for
over-dispersed data - data where the conditional variances exceed
conditional
means.

``` r
# https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/ for non-mixed version

# Test for overdispersion

with(graphics_avgdeg, tapply(Response, Condition, function(x) {
    sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# variances in each condition are much larger than means in the conditions
```

``` r
graph.avgdeg.nb.null <- glmer.nb(Response ~ (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)



graph.avgdeg.nb <- glmer.nb(Response ~ Condition + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# model is not significantly different from null model (p=0.4081), so Condition isn't a
# significant predictor

graph.avgdeg.nb.dataset <- glmer.nb(Response ~ Dataset + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.dataset)

anova(graph.avgdeg.nb.dataset, graph.avgdeg.nb.null)

# model is significantly different from null model (p < 2.2e-16), so Dataset is a
# significant predictor

graph.avgdeg.nb <- glmer.nb(Response ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# model is not significantly different from null model (p = 0.2556), so DatasetOrder is not a
# significant predictor

#graph.avgdeg.nb <- glmer.nb(Response ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

#summary(graph.avgdeg.nb)

#anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# error trying to run model: 
#"Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, : pwrssUpdate did not converge in (maxit) iterations"

graph.avgdeg.nb.taskorder <- glmer.nb(Response ~ TaskOrder + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.taskorder)

anova(graph.avgdeg.nb.taskorder, graph.avgdeg.nb.null)

# model is significantly different from null model (p = 0.01659), so TaskOrder is a
# significant predictor

#graph.avgdeg.nb <- glmer.nb(Response ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

#summary(graph.avgdeg.nb)

#anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# error trying to run model:
#"Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, : (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate"

graph.avgdeg.nb.correct <- glmer.nb(Response ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.correct)

anova(graph.avgdeg.nb.correct, graph.avgdeg.nb.null)

# model is significantly different from null model (p < 2.2e-16), so CorrectAnswer is a
# significant predictor

#graph.avgdeg.nb <- glmer.nb(Response ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

#summary(graph.avgdeg.nb)

#anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# error trying to run model:
#"Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, : pwrssUpdate did not converge in (maxit) iterations"

graph.avgdeg.nb <- glmer.nb(Response ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# model is not significantly different from null model (p = 0.5602), so Stats.OperatingSystems is not a
# significant predictor

#graph.avgdeg.nb <- glmer.nb(Response ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

#summary(graph.avgdeg.nb)

#anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# error trying to run model:
#"Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, : Downdated VtV is not positive definite"

# TO DO: try to fix errors? try different negative binomial model syntax?

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.age + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.age)), verbose=TRUE)

summary(graph.avgdeg.nb)

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.age)

summary(graph.avgdeg.nb.null.2)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# model is not significantly different from null model (p = 0.07795), so Demo.age is not a
# significant predictor
# also, model summary says "Model failed to converge with max|grad| = 0.00119421 (tol = 0.001, component 1)"

# TO DO : keep going with predictors, try combinations of the significant ones? do some prediction to check how good model is?

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.gender + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.gender)), verbose=TRUE)

summary(graph.avgdeg.nb)

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.gender)

summary(graph.avgdeg.nb.null.2)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# model is not significantly different from null model (p = 0.7306), so Demo.gender is not a
# significant predictor

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.lang + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.lang)), verbose=TRUE)

summary(graph.avgdeg.nb)

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.lang)

summary(graph.avgdeg.nb.null.2)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# null model failed to converge
# "Model failed to converge with max|grad| = 0.0291884 (tol = 0.001, component 1)"

# model is not significantly different from null model (p = 0.1938), so Demo.lang is not a
# significant predictor

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.educ + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.educ)), verbose=TRUE)

summary(graph.avgdeg.nb)

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.educ)

summary(graph.avgdeg.nb.null.2)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# null model failed to converge

# model is not significantly different from null model (p = 0.7197), so Demo.educ is not a
# significant predictor

# takes forever, not significant
#graph.avgdeg.nb <- glmer.nb(Response ~ Demo.acfield + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.acfield)), verbose=TRUE)

#summary(graph.avgdeg.nb)

# no categories significant

#graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.acfield)

#summary(graph.avgdeg.nb.null.2)

# model failed to converge

#anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# not significant

# TO DO : test acfield group

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.dailytech_Computer + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.dailytech_Computer)), verbose=TRUE)

summary(graph.avgdeg.nb)

# not significant

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.dailytech_Computer)

summary(graph.avgdeg.nb.null.2)

# model failed to converge

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# not significant

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.dailytech_Tablet)), verbose=TRUE)

summary(graph.avgdeg.nb)

# not significant

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.dailytech_Tablet)

summary(graph.avgdeg.nb.null.2)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# not significant

graph.avgdeg.nb.sp <- glmer.nb(Response ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.dailytech_SmartPhone)), verbose=TRUE)

summary(graph.avgdeg.nb.sp)

# significant (p = 0.00253)

graph.avgdeg.nb.null.sp <- update(graph.avgdeg.nb.sp, . ~ . - Demo.dailytech_SmartPhone)

summary(graph.avgdeg.nb.null.sp)

# model failed to converge

anova(graph.avgdeg.nb.sp, graph.avgdeg.nb.null.sp)

# significant (p = 0.002548)

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.weeklygaming + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.weeklygaming)), verbose=TRUE)

summary(graph.avgdeg.nb)

# not significant

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.weeklygaming)

summary(graph.avgdeg.nb.null.2)

# model failed to converge

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# not significant

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.expdataanal + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.expdataanal)), verbose=TRUE)

summary(graph.avgdeg.nb)

# not significant

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.expdataanal)

summary(graph.avgdeg.nb.null.2)

# model failed to converge

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# not significant

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.expdatavis + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.expdatavis)), verbose=TRUE)

summary(graph.avgdeg.nb)

# not significant

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.expdatavis)

summary(graph.avgdeg.nb.null.2)

# model failed to converge

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# not significant

graph.avgdeg.nb <- glmer.nb(Response ~ Demo.expreadnetvis + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.expreadnetvis)), verbose=TRUE)

summary(graph.avgdeg.nb)

# some categories significant; not sure what the reference category is though!
# TO DO : set reference level to None?

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.expreadnetvis)

summary(graph.avgdeg.nb.null.2)

# model failed to converge

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# just over significance (p = 0.06055)

# TO DO : figure out which categories are significant and make some groups to increase
# significance?


graph.avgdeg.nb <- glmer.nb(Response ~ Demo.expcreatenetvis + (1|Demo.ResponseID), data=graphics_avgdeg %>% filter(!is.na(Demo.expcreatenetvis)), verbose=TRUE)

summary(graph.avgdeg.nb)

# one categories significant; not sure what the reference category is though!
# TO DO : set reference level to None?

graph.avgdeg.nb.null.2 <- update(graph.avgdeg.nb, . ~ . - Demo.expcreatenetvis)

summary(graph.avgdeg.nb.null.2)

# model failed to converge

anova(graph.avgdeg.nb, graph.avgdeg.nb.null.2)

# just over significance (p = 0.05901)

# TO DO : figure out which categories are significant and make some groups to increase
# significance?

graph.avgdeg.nb.avgdeg <- glmer.nb(Response ~ AvgDeg + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.avgdeg)

anova(graph.avgdeg.nb.avgdeg, graph.avgdeg.nb.null)

# model is significantly different from null model (p < 2.2e-16), so AvgDeg is a
# significant predictor

graph.avgdeg.nb.density <- glmer.nb(Response ~ Density + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.density)

anova(graph.avgdeg.nb.density, graph.avgdeg.nb.null)

# model is significantly different from null model (p < 2.2e-16), so Density is a
# significant predictor

graph.avgdeg.nb.largeclust <- glmer.nb(Response ~ LargeClust1 + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.largeclust)

anova(graph.avgdeg.nb.largeclust, graph.avgdeg.nb.null)

# model is significantly different from null model (p < 2.2e-16), so LargeClust1 is a
# significant predictor

graph.avgdeg.nb.mod <- glmer.nb(Response ~ Modularity + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.mod)

anova(graph.avgdeg.nb.mod, graph.avgdeg.nb.null)

# model is significantly different from null model (p < 2.2e-16), so Modularity is a
# significant predictor

graph.avgdeg.nb.numclust <- glmer.nb(Response ~ NumClust + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.numclust)

anova(graph.avgdeg.nb.numclust, graph.avgdeg.nb.null)

# model is significantly different from null model (p = 1.174e-12), so NumClust is a
# significant predictor

graph.avgdeg.nb <- glmer.nb(Response ~ NumHighDegree + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb)

anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# model is not significantly different from null model (p = 0.2065), so NumHighDegree is not
# a significant predictor

graph.avgdeg.nb.numlinks <- glmer.nb(Response ~ NumLinks + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.numlinks)

# Error:
# Model failed to converge with max|grad| = 0.0063552 (tol = 0.001, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?
# Model is nearly unidentifiable: large eigenvalue ratio
# - Rescale variables?

anova(graph.avgdeg.nb.numlinks, graph.avgdeg.nb.null)

# model is not significantly different from null model (p = 1.875e-06), so NumLinks is 
# a significant predictor

graph.avgdeg.nb.numnodes <- glmer.nb(Response ~ NumNodes + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb.numnodes)

# Error:
#  failed to converge with max|grad| = 0.00178473 (tol = 0.001, component 1)
# Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?

anova(graph.avgdeg.nb.numnodes, graph.avgdeg.nb.null)

# model is not significantly different from null model (p = 0.0004385), so NumNodes is 
# a significant predictor

graph.avgdeg.nb <- glmer.nb(Response ~ NumNodesClust1 + (1|Demo.ResponseID), data=graphics_avgdeg, verbose=TRUE)

summary(graph.avgdeg.nb)

# Error:
#Model failed to converge with max|grad| = 0.00187437 (tol = 0.001, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?

anova(graph.avgdeg.nb, graph.avgdeg.nb.null)

# model is not significantly different from null model (p = 0.4105), so NumNodesClust1 is 
# not a significant predictor

#---------------------------------
# Multiple predictors
#---------------------------------

temp <- graphics_avgdeg %>% drop_na(Response, Dataset, TaskOrder, CorrectAnswer, Demo.dailytech_SmartPhone, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumLinks, NumNodes)

graph.avgdeg.nb.full <- glmer.nb(Response ~ Dataset + TaskOrder + Demo.dailytech_SmartPhone + (1|Demo.ResponseID), 
                            data=temp, verbose=TRUE)

summary(graph.avgdeg.nb.full)
# still significant - Dataset, TaskOrder, SmartPhone

graph.avgdeg.nb.full.2 <- glmer.nb(Response ~ TaskOrder + CorrectAnswer + Demo.dailytech_SmartPhone + LargeClust1 + Modularity + NumNodes + (1|Demo.ResponseID), 
                            data=temp, verbose=TRUE)

summary(graph.avgdeg.nb.full.2)
# still significant: TaskOrder, CorrectAnswer, SmartPhone, LargeClust1, Modularity, NumNodes

# Error:
# Model failed to converge with max|grad| = 0.00841304 (tol = 0.001, component 1)
# Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?
# Model is nearly unidentifiable: large eigenvalue ratio
#  - Rescale variables?

anova(graph.avgdeg.nb.full, graph.avgdeg.nb.full.2)
# two models not significantly different

graph.avgdeg.nb.full.3 <- glmer.nb(Response ~ TaskOrder + CorrectAnswer + Demo.dailytech_SmartPhone + LargeClust1 + (1|Demo.ResponseID), 
                            data=temp, verbose=TRUE)

summary(graph.avgdeg.nb.full.3)
# still significant: TaskOrder, CorrectAnswer, SmartPhone, LargeClust1

# Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?

anova(graph.avgdeg.nb.full, graph.avgdeg.nb.full.3)

# model is not significantly different from original full model, so probably just keep graph.avgdeg.nb.full

#---------------------------------
# Interactions
#---------------------------------

graph.avgdeg.nb.full.int <- glmer.nb(Response ~ Dataset + 
                                       TaskOrder + Demo.dailytech_SmartPhone + 
                                       Dataset:TaskOrder + 
                                       TaskOrder:Demo.dailytech_SmartPhone + 
                                       Dataset:Demo.dailytech_SmartPhone +
                                       (1|Demo.ResponseID), 
                            data=temp, verbose=TRUE)

summary(graph.avgdeg.nb.full.int)

anova(graph.avgdeg.nb.full, graph.avgdeg.nb.full.int)
# yes, interaction is significantly different from just main effects

# TO DO : check lmer results again after factoring
```

``` r
graph.avgdeg.nb.full.f <- fortify(graph.avgdeg.nb.full)

ggplot(graph.avgdeg.nb.full.f, aes(.fitted,.resid)) + 
  geom_point() +
  geom_hline(yintercept=0)

ggplot(graph.avgdeg.nb.full.f, aes(.fitted,Response)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10()

ggplot(graph.avgdeg.nb.full.f, aes(.fitted,Response, color=Dataset)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10()

ggplot(graph.avgdeg.nb.full.f, aes(.fitted,Response)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10() +
  facet_wrap(~Dataset)

graph.avgdeg.nb.full.int.f <- fortify(graph.avgdeg.nb.full.int)

ggplot(graph.avgdeg.nb.full.int.f, aes(.fitted,.resid)) + 
  geom_point() +
  geom_hline(yintercept=0)

ggplot(graph.avgdeg.nb.full.int.f, aes(.fitted,Response)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10()
```

``` r
# is negative binomial different from poisson?  if so, indicates over-dispersion is true
# and negative binomial is necessary

graph.avgdeg.pois <- glmer(Response ~ Dataset + 
                                       TaskOrder + Demo.dailytech_SmartPhone + 
                                       Dataset:TaskOrder + 
                                       TaskOrder:Demo.dailytech_SmartPhone + 
                                       Dataset:Demo.dailytech_SmartPhone +
                                       (1|Demo.ResponseID), data=temp, family="poisson")

pchisq(2 * (logLik(graph.avgdeg.nb.full.int) - logLik(graph.avgdeg.pois)), df=1, lower.tail = FALSE)

# value = 0, so keep the negative binomial
```

``` r
# To fit a negative binomial model with known overdispersion parameter (e.g. as part of a model
#comparison exercise), use glmer with the negative.binomial family from the MASS package, e.g.
#glmer(...,family=MASS::negative.binomial(theta=1.75)).

graph.avgdeg.nb2 <- glmer(Response ~ Condition + (1|Demo.ResponseID), data=graphics_avgdeg, family=MASS::negative.binomial(theta=1.75))

graph.avgdeg.nb3 <- glmer(Response ~ Condition + (1|Demo.ResponseID), data=graphics_avgdeg, family=negative.binomial(2))


#graph.avgdeg.nb

summary(graph.avgdeg.nb2)
summary(graph.avgdeg.nb3)
```

``` r
# run without mixed effects to validate

m.glm <- glm.nb(Response ~ Dataset + TaskOrder + Demo.dailytech_SmartPhone + 
                                       Dataset:TaskOrder + 
                                       TaskOrder:Demo.dailytech_SmartPhone + 
                                       Dataset:Demo.dailytech_SmartPhone, data=temp, trace=TRUE)
summary(m.glm)

## The neg.binomial theta parameter:
getME(graph.avgdeg.nb.full.int, "glmer.nb.theta")

#1.567446

## mixed model has 1 additional parameter (RE variance)
stopifnot(attr(logLik(graph.avgdeg.nb.full.int),"df")==attr(logLik(m.glm),"df")+1) # not sure what this does

anova(graph.avgdeg.nb.full.int,m.glm) # can I use anova to compare mixed and fixed effects?
# p < 2.2e-16, so definitely random effects

plot(graph.avgdeg.nb.full.int, resid(.) ~ Response)# works, as long as data 'dd' is found


# TO DO : check if this is all right
```

``` r
par(mfrow=c(2,2))
qqnorm(resid(graph.avgdeg.nb.full.int), main="normal qq-plot, residuals")
qqline(resid(graph.avgdeg.nb.full.int))

qqnorm(ranef(graph.avgdeg.nb.full.int)$Demo.ResponseID[,1])
qqline(ranef(graph.avgdeg.nb.full.int)$Demo.ResponseID[,1])


plot(fitted(graph.avgdeg.nb.full.int), resid(graph.avgdeg.nb.full.int)) #residuals vs fitted
abline(h=0)

#graph.avgdeg.nb2.f <- fortify(graph.avgdeg.nb2)

#ggplot(graph.avgdeg.nb2.f, aes(.fitted,.resid)) + 
#  geom_point() +
#  geom_hline(yintercept=0)

#temp <- graphics_avgdeg

temp$fitted <- fitted(graph.avgdeg.nb.full.int) 
plot(temp$fitted, jitter(temp$Response,0.1)) #fitted vs observed
abline(0,1)

#ggplot(graph.avgdeg.nb2.f, aes(.fitted,Response)) + 
#  geom_point() +
#  geom_abline(aes(slope = 1, intercept = 0))
```

``` r
# Confidence Intervals, using coefficients

(est <- cbind(Estimate = coef(graph.avgdeg.nb.full.int), confint(graph.avgdeg.nb.full.int)))

# exponentiate model to look at incident rate ratios instead of coefficients

exp(est)
```

``` r
# predictions

# model: Response ~ Dataset + TaskOrder + Demo.dailytech_SmartPhone + Dataset:TaskOrder + TaskOrder:Demo.dailytech_SmartPhone + Dataset:Demo.dailytech_SmartPhone + (1 | Demo.ResponseID)

newdata1 <- data.frame(Demo.dailytech_SmartPhone = rep(mean(temp$Demo.dailytech_SmartPhone),54), 
                       Dataset = factor(rep(c(1,3,5,7,8,9),9), levels = levels(temp$Dataset),ordered = TRUE),
                       TaskOrder = rep(1:9,6),
                       Demo.ResponseID = sample(temp$Demo.ResponseID,54))
newdata1$phat <- predict(graph.avgdeg.nb.full.int, newdata1, type = "response")
#newdata1

newdata2 <- data.frame(
  Demo.dailytech_SmartPhone = rep(seq(from = min(temp$Demo.dailytech_SmartPhone), to = max(temp$Demo.dailytech_SmartPhone), length.out = 100), 6),
  Dataset = factor(rep(c(1,3,5,7,8,9), each = 100), levels = levels(temp$Dataset),ordered = TRUE),
  TaskOrder = rep(1:9,len=600),
  Demo.ResponseID = sample(temp$Demo.ResponseID,600)
  )

#predict(graph.avgdeg.nb.full.int, newdata2, type = "link", se.fit=TRUE)
newdata2 <- cbind(newdata2, predict(graph.avgdeg.nb.full.int, newdata2, type = "link", se.fit=TRUE))

# not sure about this; asking for "fit", but that's not recognized. should the new column be called "fit"? what about se.fit?

#newdata2 <- within(newdata2, {
#  Response <- exp(fit)
#  LL <- exp(fit - 1.96 * se.fit)
#  UL <- exp(fit + 1.96 * se.fit)
#})

#ggplot(newdata2, aes(math, DaysAbsent)) +
#  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
#  geom_line(aes(colour = prog), size = 2) +
#  labs(x = "Math Score", y = "Predicted Days Absent")

# note on negative binomial:

# TO DO : If the data generating process does not allow for any 0s (such as the number of days spent in the hospital), then a zero-truncated model may be more appropriate.

# TO DO : Count data often have an exposure variable, which indicates the number of times the event could have happened (i.e. a max). This variable should be incorporated into your negative binomial regression model with the use of the offset option. See the glm documentation for details.  (so, that would make sense for click data???)

# other info:
# Cameron, A. C. and Trivedi, P. K. 1998. Regression Analysis of Count Data. New York: Cambridge Press.
# Dupont, W. D. 2002. Statistical Modeling for Biomedical Researchers: A Simple Introduction to the Analysis of Complex Data. New York: Cambridge Press.
```

#### brms/tidybayes

``` r
# graph.avgdeg.lmer.full <- lmer(LogError ~ Underestimated + Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=temp, REML = T)

#library(rstan)

theme_set(theme_grey())

#avgd.fit <- brm(LogError~Underestimated + Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data = temp, REML=T)
avgd.fit <- brm(LogError~Underestimated + Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data = temp)

summary(avgd.fit, waic = TRUE) 

#plot(avgd.fit)

avgd.fit$formula

#avgd.fit %<>% recover_types(temp)

#tidybayes::parameters(avgd.fit)
```

``` r
#plot(fit, ask=FALSE) 
#plot(marginal_effects(fit, probs = c(0.05, 0.95)),points=TRUE, ask=FALSE)
#plot(marginal_effects(fit, probs = c(0.05, 0.95)),ask=FALSE)
plot(marginal_effects(avgd.fit, effects="Underestimated", probs = c(0.05, 0.95)),ask=FALSE)
```

``` r
# all pairs

avgd.hyp <- c("Underestimatedunder = 0")

(avgd.hyp.res <- hypothesis(avgd.fit, avgd.hyp))

#plot(avgd.hyp.res, chars = NULL, ask=FALSE)

avgd.hyp.res.df <- avgd.hyp.res$hypothesis

avgd.hyp.res.df$contrast <- c("under-over")

avgd.hyp.res.df$contrast <- factor(avgd.hyp.res.df$contrast,
                                    levels=avgd.hyp.res.df %>% arrange(desc(Estimate)) %>%
                                      dplyr::select(contrast) %>% unlist())

ggplot(avgd.hyp.res.df, aes(contrast,Estimate)) + 
  geom_errorbar(aes(ymin=`l-95% CI`,ymax=`u-95% CI`)) +
  geom_point(aes(fill=Star=="*"), size=7, shape=21) +
  #geom_hline(yintercept = 0) +
  scale_fill_manual(values=star.colors,name="Outside 95% CI") +
  coord_flip()
```

``` r
hyp.pairs.2.res.df <- hyp.pairs.2.res.df %>% separate(contrast, c("From", "To"), sep="[-]", remove=FALSE)

copy <- hyp.pairs.2.res.df %>% rename(From=To,To=From) %>% 
  mutate(Estimate=-Estimate,
         `l-95% CI`=-`l-95% CI`,
         `u-95% CI`=-`u-95% CI`)

pairs.compl <- bind_rows(hyp.pairs.2.res.df, copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

pairs.compl$From <- factor(pairs.compl$From, levels=c(9,8,7,5,3,1))
pairs.compl$To <- factor(pairs.compl$To, levels=c(1,3,5,7,8,9))

#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
pairs.compl %>% arrange(Estimate)

# can't get color to go the right direction
ggplot(pairs.compl %>% filter(Estimate >= 0)) +
  geom_tile(aes(x=To,y=From,fill=Estimate), color="black") +
  scale_fill_distiller(palette="Greys", direction=1) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Star=="*"),shape=21, color="black") +
  scale_fill_manual(values=star.colors, name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=Estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=Star=="*"), shape=21,stroke=1) +
  scale_fill_distiller(type="div", palette=4, limits=c(-max(abs(pairs.compl$Estimate)),max(abs(pairs.compl$Estimate)))) +
  scale_color_manual(values=c("grey90","black"), name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

hyp.pairs.2.res.df$From <- factor(hyp.pairs.2.res.df$From, levels=c(9,8,7,5,3,1))
hyp.pairs.2.res.df$To <- factor(hyp.pairs.2.res.df$To, levels=c(1,3,5,7,8,9))


ggplot(hyp.pairs.2.res.df) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=Star=="*"), shape=21,stroke=1) +
  scale_fill_distiller(type="div", palette=4, limits=c(-max(abs(pairs.compl$Estimate)),max(abs(pairs.compl$Estimate)))) +
  scale_color_manual(values=c("grey90","black"), name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

#### Least Squares Means-numlinks

Do for each categorical predictor. Final model: LogError ~
Underestimated + Demo.dailytech\_SmartPhone + (1 | Demo.ResponseID)

##### Underestimated

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.avgdeg.emm.under <- emmeans(graph.avgdeg.lmer.full, "Underestimated", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.avgdeg.emm.under
```

    ##  Underestimated      emmean          SE     df    lower.CL    upper.CL
    ##  over           0.033366872 0.002170908 690.62 0.029104499 0.037629244
    ##  under          0.005457973 0.002060875 646.68 0.001411159 0.009504787
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.avgdeg.emm.under.df <- dplyr::as_data_frame(graph.avgdeg.emm.under)

graph.avgdeg.emm.under.df
```

    ## # A tibble: 2 x 6
    ##   Underestimated  emmean      SE    df lower.CL upper.CL
    ## * <fct>            <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 over           0.0334  0.00217   691  0.0291   0.0376 
    ## 2 under          0.00546 0.00206   647  0.00141  0.00950

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.avgdeg.emm.under.cld <- cld(graph.avgdeg.emm.under,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.avgdeg.emm.under.cld.df <- graph.avgdeg.emm.under.cld$emmeans

graph.avgdeg.emm.under.cld.df %>% dplyr::select(Underestimated,.group)
```

    ##  Underestimated .group
    ##  under           1    
    ##  over             2   
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.avgdeg.emm.under.cld.df
```

    ##  Underestimated      emmean          SE     df    lower.CL    upper.CL
    ##  under          0.005457973 0.002060875 646.68 0.001411159 0.009504787
    ##  over           0.033366872 0.002170908 690.62 0.029104499 0.037629244
    ##  .group
    ##   1    
    ##    2   
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.avgdeg.emm.under.cld.df$Underestimated <- factor(graph.avgdeg.emm.under.cld.df$Underestimated, levels=graph.avgdeg.emm.under.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Underestimated) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.avgdeg.emm.under, ~Underestimated, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
plot(graph.avgdeg.emm.under)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-45-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.avgdeg.emm.under.cld.df %>% arrange(desc(emmean))
```

    ##   Underestimated      emmean          SE       df    lower.CL    upper.CL
    ## 1           over 0.033366872 0.002170908 690.6155 0.029104499 0.037629244
    ## 2          under 0.005457973 0.002060875 646.6752 0.001411159 0.009504787
    ##   .group
    ## 1      2
    ## 2     1

``` r
ggplot(graph.avgdeg.emm.under.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Underestimated,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Underestimated,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-45-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.avgdeg.emm.under.pairs <- dplyr::as_data_frame(pairs(graph.avgdeg.emm.under)) 
graph.avgdeg.emm.under.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast     estimate      SE    df t.ratio                  p.value
    ## * <fct>           <dbl>   <dbl> <dbl>   <dbl>                    <dbl>
    ## 1 over - under   0.0279 0.00294  1086    9.49 0.0000000000000000000147

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.avgdeg.emm.under))

graph.avgdeg.emm.under.pairs <- full_join(graph.avgdeg.emm.under.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.avgdeg.emm.under))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
plot(pairs(graph.avgdeg.emm.under), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-46-2.png)<!-- -->

``` r
graph.avgdeg.emm.under.pairs$sig.levels <- 
  case_when(graph.avgdeg.emm.under.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.avgdeg.emm.under.pairs$p.value < .001 ~ sig.level.names[2],
            graph.avgdeg.emm.under.pairs$p.value < .01 ~ sig.level.names[3],
            graph.avgdeg.emm.under.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.avgdeg.emm.under.pairs$sig.levels <- factor(graph.avgdeg.emm.under.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.avgdeg.emm.under.pairs$contrast <- factor(graph.avgdeg.emm.under.pairs$contrast, levels=graph.avgdeg.emm.under.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.avgdeg.emm.under.pairs <- graph.avgdeg.emm.under.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.avgdeg.emm.under.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast  From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##   <fct>     <chr> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 over - u… over  under   0.0279 0.00294  1086    9.49   1.47e⁻²⁰   0.0221
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.avgdeg.emm.under.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-46-3.png)<!-- -->

``` r
ggplot(graph.avgdeg.emm.under.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-46-4.png)<!-- -->

``` r
copy <- graph.avgdeg.emm.under.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.avgdeg.emm.under.pairs.compl <- bind_rows(graph.avgdeg.emm.under.pairs, copy)

#cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.avgdeg.emm.under.pairs.compl$From <- factor(graph.avgdeg.emm.under.pairs.compl$From, levels=rev(unique(graph.avgdeg.emm.under.pairs.compl$From)))
#graph.avgdeg.emm.under.pairs.compl$To <- factor(graph.avgdeg.emm.under.pairs.compl$To, levels=cond.lev)

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.avgdeg.emm.under.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast  From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##   <fct>     <fct> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 over - u… under over   -0.0279 0.00294  1086   -9.49   1.47e⁻²⁰  -0.0221
    ## 2 over - u… over  under   0.0279 0.00294  1086    9.49   1.47e⁻²⁰   0.0221
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.avgdeg.emm.under.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
ggplot(graph.avgdeg.emm.under.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-47-2.png)<!-- -->

``` r
ggplot(graph.avgdeg.emm.under.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-47-3.png)<!-- -->

``` r
ggplot(graph.avgdeg.emm.under.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-47-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.avgdeg.emm.under.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*50
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.avgdeg.emm.under.pairs %>% dplyr::select(-contrast), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Demo.dailytech\_SmartPhone - not a factor

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.avgdeg.emm.SP <- emmeans(graph.avgdeg.lmer.full, "Demo.dailytech_SmartPhone", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.avgdeg.emm.SP

graph.avgdeg.emm.SP.df <- dplyr::as_data_frame(graph.avgdeg.emm.SP)

graph.avgdeg.emm.SP.df


# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.avgdeg.emm.SP.cld <- cld(graph.avgdeg.emm.SP,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.avgdeg.emm.SP.cld.df <- graph.avgdeg.emm.SP.cld$emmeans

graph.avgdeg.emm.SP.cld.df %>% dplyr::select(Demo.dailytech_SmartPhone,.group)

graph.avgdeg.emm.SP.cld.df

graph.avgdeg.emm.SP.cld.df$Demo.dailytech_SmartPhone <- factor(graph.avgdeg.emm.SP.cld.df$Demo.dailytech_SmartPhone, levels=graph.avgdeg.emm.SP.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Demo.dailytech_SmartPhone) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.avgdeg.emm.SP, ~Demo.dailytech_SmartPhone, CIs = TRUE)
plot(graph.avgdeg.emm.SP)
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.avgdeg.emm.SP.cld.df %>% arrange(desc(emmean))

ggplot(graph.avgdeg.emm.SP.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.dailytech_SmartPhone,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Demo.dailytech_SmartPhone,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()


#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numlinks.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.numlinks.emm.data)) 
graph.numlinks.emm.data.pairs
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numlinks.emm.data))

graph.numlinks.emm.data.pairs <- full_join(graph.numlinks.emm.data.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numlinks.emm.data))
plot(pairs(graph.numlinks.emm.data), comparisons = TRUE)

graph.numlinks.emm.data.pairs$sig.levels <- 
  case_when(graph.numlinks.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numlinks.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numlinks.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numlinks.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numlinks.emm.data.pairs$sig.levels <- factor(graph.numlinks.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numlinks.emm.data.pairs$contrast <- factor(graph.numlinks.emm.data.pairs$contrast, levels=graph.numlinks.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numlinks.emm.data.pairs <- graph.numlinks.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numlinks.emm.data.pairs %>% arrange(estimate)

ggplot(graph.numlinks.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()

ggplot(graph.numlinks.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

``` r
data.copy <- graph.numlinks.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numlinks.emm.data.pairs.compl <- bind_rows(graph.numlinks.emm.data.pairs, data.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=cond.lev)
#graph.numlinks.emm.data.pairs.compl$To <- factor(graph.numlinks.emm.data.pairs.compl$To, levels=cond.lev)

graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=rev(unique(graph.numlinks.emm.data.pairs.compl$From)))

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numlinks.emm.data.pairs.compl %>% arrange(estimate)

ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

### Betweenness Centrality

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_bc <- readRDS(file.path(analysisDataDir, "GraphicsBC.rds"))
```

#### Negative binomial model for click questions Node Rank

Ranks are like count data, in that they are nonnegative integers, so
using a negative binomial distribution to model. Negative binomial is
especially useful for over-dispersed data - data where the conditional
variances exceed conditional
means.

``` r
# https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/ for non-mixed version

# Test for overdispersion

with(graphics_bc, tapply(NodeRank, Condition, function(x) {
    sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
```

    ##                   Ctrl                    Cir                    Col 
    ## "M (SD) = 3.35 (3.77)"                     NA "M (SD) = 3.89 (4.20)" 
    ##                    Fru                    Ord                    Phr 
    ##                     NA                     NA "M (SD) = 3.86 (4.70)" 
    ##                    Siz 
    ## "M (SD) = 3.25 (3.08)"

``` r
# variances in each condition (except Size) are larger than means in the conditions
```

``` r
# negative binomial models - no offset, first single fixed effects, then multiple, then interactions

graph.bc.nb.null <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# Model failed to converge with max|grad| = 0.040352 (tol = 0.001, component 1) --> dev.= -2*logLik(.) = 5486.616 

graph.bc.nb.null <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

graph.bc.nb <- glmer.nb(NodeRank ~ Condition + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# model is not significantly different from null model (p=0.232), so Condition isn't a
# significant predictor

graph.bc.nb <- glmer.nb(NodeRank ~ factor(Ctrl_dummy) + (1|Demo.ResponseID), 
                        data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Ctrl_dummy is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ ConditionPhrasing + (1|Demo.ResponseID), 
                        data=graphics_bc, verbose=TRUE)

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# ConditionPhrasing is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ ConditionGraphics + (1|Demo.ResponseID), 
                        data=graphics_bc, verbose=TRUE)

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# ConditionPhrasing is not significant


#graph.bc.nb.dataset <- glmer.nb(NodeRank ~ Dataset + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# model failed to converge

graph.bc.nb.dataset <- glmer.nb(NodeRank ~ Dataset + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

summary(graph.bc.nb.dataset)

anova(graph.bc.nb.dataset, graph.bc.nb.null)

# model is significantly different from null model (p < 2.2e-16)

#graph.bc.nb.dataOrder <- glmer.nb(NodeRank ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# model failed to converge

graph.bc.nb.dataOrder <- glmer.nb(NodeRank ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb.dataOrder)

anova(graph.bc.nb.dataOrder, graph.bc.nb.null)

# model is significantly different from null model (p = 0.005009)

#graph.bc.nb <- glmer.nb(NodeRank ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_bc %>% filter(!is.na(DatasetDuration)), verbose=TRUE)

# major error: PIRLS step-halvings failed to reduce deviance in pwrssUpdate; rescale data

graphics_bc.CS <- graphics_bc %>% mutate(DatasetDuration=scale(DatasetDuration))

graph.bc.nb <- glmer.nb(NodeRank ~ DatasetDuration + (1|Demo.ResponseID), 
                        data=graphics_bc.CS %>% filter(!is.na(DatasetDuration)), verbose=TRUE)

# get an error about model convergence; trying suggestions from lme4 documentation

# try fitting with all optimizers; if all converge to about the same values, convergence warning can be ignored

#source(system.file("utils", "allFit.R", package="lme4"))

#graph.bc.nb.all <- allFit(graph.bc.nb)
#ss <- summary(graph.bc.nb.all)
#ss$fixef
#ss$llik
#ss$sdcor
#ss$theta
#ss$which.OK

# or can restart fit from apparent optimum
#graph.bc.nb.restart <- update(graph.bc.nb, start=pars)
#summary(graph.bc.nb.restart)

# trying just manually changing optimizer

graph.bc.nb <- glmer.nb(NodeRank ~ DatasetDuration + (1|Demo.ResponseID), 
                        data=graphics_bc.CS %>% filter(!is.na(DatasetDuration)), verbose=TRUE, 
                        control=glmerControl(optimizer = "nlminbw"))

#seems okay; don't see any errors

summary(graph.bc.nb)

graph.bc.nb.null2 <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc.CS %>% filter(!is.na(DatasetDuration)), verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

anova(graph.bc.nb, graph.bc.nb.null2)

# DatasetDuration is not significant 

#graph.bc.nb.taskorder <- glmer.nb(NodeRank ~ TaskOrder + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# failed to converge

graph.bc.nb <- glmer.nb(NodeRank ~ TaskOrder + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "Nelder_Mead"))

# still have a failed to converge warning, but results seem similar with different optimizers

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# TaskOrder is not significant

#graph.bc.nb <- glmer.nb(NodeRank ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# warnings about rescaling variables

graphics_bc.CS <- graphics_bc %>% mutate(QuestionOrder=scale(QuestionOrder))

graph.bc.nb <- glmer.nb(NodeRank ~ QuestionOrder + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

summary(graph.bc.nb)

graph.bc.nb.null2 <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

anova(graph.bc.nb, graph.bc.nb.null2)

# QuestionOrder is not significant


#graph.bc.nb <- glmer.nb(NodeRank ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# error about rescaling

graphics_bc.CS <- graphics_bc %>% mutate(CorrectAnswer=scale(CorrectAnswer))

graph.bc.nb.correct <- glmer.nb(NodeRank ~ CorrectAnswer + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE)

summary(graph.bc.nb.correct)

graph.bc.nb.null2 <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

anova(graph.bc.nb.correct, graph.bc.nb.null2)

# CorrectAnswer is significant (p=9.358e-09)

#graph.bc.nb <- glmer.nb(NodeRank ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# warnings about rescaling variables

graphics_bc.CS <- graphics_bc %>% mutate(Stats.Q_TotalDuration=scale(Stats.Q_TotalDuration))

graph.bc.nb.totdur <- glmer.nb(NodeRank ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_bc.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

summary(graph.bc.nb.totdur)

graph.bc.nb.null2 <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

anova(graph.bc.nb.totdur, graph.bc.nb.null2)

# Stats.Q_TotalDuration is significant (p=0.003248)


graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))
# very slooooooow, but no errors

summary(graph.bc.nb) # nothing significant

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystems is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb) # iPhone significantly different from Android

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemCombined is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined2 + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemCombined2 is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined3 + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemCombined3 is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined4 + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemCombined4 is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined5 + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemCombined5 is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemWindows + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemWindows is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemMacintosh + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemMacintosh is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemAndroid + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemAndroid is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemiPhone + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

anova(graph.bc.nb, graph.bc.nb.null)

# Stats.OperatingSystemiPhone is not significant


#graph.bc.nb <- glmer.nb(NodeRank ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(StatsNumPixels=scale(StatsNumPixels))

graph.bc.nb <- glmer.nb(NodeRank ~ StatsNumPixels + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE, 
                        control=glmerControl(optimizer = "nlminbw"))

summary(graph.bc.nb)

graph.bc.nb.null2 <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_bc.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

anova(graph.bc.nb, graph.bc.nb.null2)

# StatsNumPixels not significant

#graph.bc.nb <- glmer.nb(NodeRank ~ Demo.age + (1|Demo.ResponseID), data=graphics_bc %>% filter(!is.na(Demo.age)), verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(Demo.age=scale(Demo.age))

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.age + (1|Demo.ResponseID), 
                        data=graphics_bc.CS %>% filter(!is.na(Demo.age)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))


summary(graph.bc.nb)

graph.bc.nb.null2 <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), 
                        data=graphics_bc.CS %>% filter(!is.na(Demo.age)), verbose=TRUE, 
                        control=glmerControl(optimizer = "nlminbw"))

anova(graph.bc.nb, graph.bc.nb.null2)

# Demo.age is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.gender + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.gender)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.gender)

summary(graph.bc.nb.null.2)

anova(graph.bc.nb, graph.bc.nb.null.2)

# Demo.gender is not significant (p=0.09937)

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.lang + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.lang)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.lang)

summary(graph.bc.nb.null.2)

anova(graph.bc.nb, graph.bc.nb.null.2)

# Demo.lang is not a significant predictor

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.educ + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.educ)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.educ)

anova(graph.bc.nb, graph.bc.nb.null.2)

# Demo.educ is not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.acfield + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.acfield)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

# several categories significantly different from Anthropology:
# Architecture and design, Arts, Business, Earth sciences, Information science, Library and museum studies, Other, Political science
# marginal: Computer sciences, History, Languages, Psychology, Skipped, Sociology

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.acfield)

anova(graph.bc.nb, graph.bc.nb.null.2)

# almost significant (p=0.07284)

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.acfieldGrouped + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.acfieldGrouped)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.acfieldGrouped)

anova(graph.bc.nb, graph.bc.nb.null.2)

# Demo.acfieldGrouped is not significant

graph.bc.nb.acfield2 <- glmer.nb(NodeRank ~ Demo.acfieldGrouped2 + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.acfieldGrouped2)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

graph.bc.nb.null.2 <- update(graph.bc.nb.acfield2, . ~ . - Demo.acfieldGrouped2)

anova(graph.bc.nb.acfield2, graph.bc.nb.null.2)

# Demo.acfieldGrouped2 (actually set up because of results of bc.lmer model) is 
# significant (p=2.861e-05)

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.dailytech_Computer + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.dailytech_Computer)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

# almost significant

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.dailytech_Computer)

anova(graph.bc.nb, graph.bc.nb.null.2)

# almost significant (p=0.06008)

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.dailytech_Tablet)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.dailytech_Tablet)

anova(graph.bc.nb, graph.bc.nb.null.2)

# not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.dailytech_SmartPhone)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.dailytech_SmartPhone)

anova(graph.bc.nb, graph.bc.nb.null.2)

# not significant (p = 0.1108)

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.weeklygaming + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.weeklygaming)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.weeklygaming)

anova(graph.bc.nb, graph.bc.nb.null.2)

# not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.expdataanal + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.expdataanal)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.expdataanal)

anova(graph.bc.nb, graph.bc.nb.null.2)

# not significant

graph.bc.nb <- glmer.nb(NodeRank ~ Demo.expdatavis + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.expdatavis)), verbose=TRUE)

summary(graph.bc.nb)

graph.bc.nb.null.2 <- update(graph.bc.nb, . ~ . - Demo.expdatavis)

anova(graph.bc.nb, graph.bc.nb.null.2)

# not significant

graph.bc.nb.readNV <- glmer.nb(NodeRank ~ Demo.expreadnetvis + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.expreadnetvis)), verbose=TRUE)

summary(graph.bc.nb.readNV)

# most categories significant; not sure what the reference category is though.  "None"?

graph.bc.nb.null.2 <- update(graph.bc.nb.readNV, . ~ . - Demo.expreadnetvis)

anova(graph.bc.nb.readNV, graph.bc.nb.null.2)

# significant (p = 0.007718)

graph.bc.nb.createNV <- glmer.nb(NodeRank ~ Demo.expcreatenetvis + (1|Demo.ResponseID), 
                        data=graphics_bc %>% filter(!is.na(Demo.expcreatenetvis)), verbose=TRUE)

summary(graph.bc.nb.createNV)

graph.bc.nb.null.2 <- update(graph.bc.nb.createNV, . ~ . - Demo.expcreatenetvis)

anova(graph.bc.nb.createNV, graph.bc.nb.null.2)

# significant (p = 0.003442)

graph.bc.nb.avgdeg <- glmer.nb(NodeRank ~ AvgDeg + (1|Demo.ResponseID), 
                               data=graphics_bc, verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb.avgdeg)

anova(graph.bc.nb.avgdeg, graph.bc.nb.null)

# AvgDeg is significant (p < 2.2e-16)

graph.bc.nb.density <- glmer.nb(NodeRank ~ Density + (1|Demo.ResponseID), 
                                data=graphics_bc, verbose=TRUE)

summary(graph.bc.nb.density)

anova(graph.bc.nb.density, graph.bc.nb.null)

# Density is a significant(p < 2.2e-16)

#graph.bc.nb.largeclust <- glmer.nb(NodeRank ~ LargeClust1 + (1|Demo.ResponseID), 
#                                   data=graphics_bc, verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(LargeClust1=scale(LargeClust1))

graph.bc.nb.largeclust <- glmer.nb(NodeRank ~ LargeClust1 + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE)

summary(graph.bc.nb.largeclust)

graph.bc.nb.null.2 <- update(graph.bc.nb.largeclust, . ~ . - LargeClust1)

anova(graph.bc.nb.largeclust, graph.bc.nb.null.2)

# LargeClust1 is significant (p < 2.2e-16)

graph.bc.nb.mod <- glmer.nb(NodeRank ~ Modularity + (1|Demo.ResponseID), 
                            data=graphics_bc, verbose=TRUE)

summary(graph.bc.nb.mod)

anova(graph.bc.nb.mod, graph.bc.nb.null)

# Modularity is significant(p < 2.2e-16)

graph.bc.nb.numclust <- glmer.nb(NodeRank ~ NumClust + (1|Demo.ResponseID), 
                                 data=graphics_bc, verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb.numclust)

anova(graph.bc.nb.numclust, graph.bc.nb.null)

# NumClust is significant (p = 1.91e-11)

#graph.bc.nb <- glmer.nb(NodeRank ~ NumHighDegree + (1|Demo.ResponseID), 
#                        data=graphics_bc, verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(NumHighDegree=scale(NumHighDegree))

graph.bc.nb.numhighdeg <- glmer.nb(NodeRank ~ NumHighDegree + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE)

summary(graph.bc.nb.numhighdeg)

graph.bc.nb.null.2 <- update(graph.bc.nb.numhighdeg, . ~ . - NumHighDegree)

anova(graph.bc.nb.numhighdeg, graph.bc.nb.null.2)

# NumHighDegree is significant (p=0.003951)

#graph.bc.nb.numlinks <- glmer.nb(NodeRank ~ NumLinks + (1|Demo.ResponseID), 
#                                 data=graphics_bc, verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(NumLinks=scale(NumLinks))

graph.bc.nb.numlinks <- glmer.nb(NodeRank ~ NumLinks + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE)

summary(graph.bc.nb.numlinks)

graph.bc.nb.null.2 <- update(graph.bc.nb.numlinks, . ~ . - NumLinks)

anova(graph.bc.nb.numlinks, graph.bc.nb.null.2)

# NumLinks is significant (p=1.533e-08)

#graph.bc.nb.numnodes <- glmer.nb(NodeRank ~ NumNodes + (1|Demo.ResponseID), 
#                                 data=graphics_bc, verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(NumNodes=scale(NumNodes))

graph.bc.nb.numnodes <- glmer.nb(NodeRank ~ NumNodes + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE)

summary(graph.bc.nb.numnodes)

graph.bc.nb.null.2 <- update(graph.bc.nb.numnodes, . ~ . - NumNodes)

anova(graph.bc.nb.numnodes, graph.bc.nb.null.2)

# NumNodes is significant (p=1.075e-14)

#graph.bc.nb <- glmer.nb(NodeRank ~ NumNodesClust1 + (1|Demo.ResponseID), 
#                        data=graphics_bc, verbose=TRUE)

# need to rescale

graphics_bc.CS <- graphics_bc %>% mutate(NumNodesClust1=scale(NumNodesClust1))

graph.bc.nb.numnodesclust1 <- glmer.nb(NodeRank ~ NumNodesClust1 + (1|Demo.ResponseID), 
                        data=graphics_bc.CS, verbose=TRUE)

summary(graph.bc.nb.numnodesclust1)

graph.bc.nb.null.2 <- update(graph.bc.nb.numnodesclust1, . ~ . - NumNodesClust1)

anova(graph.bc.nb.numnodesclust1, graph.bc.nb.null.2)

# NumNodesClust1 is significant (p=1.08e-10)
```

``` r
# Trying offset, since NodeRank does have a maximum, and that changes by dataset

graph.bc.nb.null.offset <- glmer.nb(NodeRank ~ (1|Demo.ResponseID) + offset(log(MaxNodeRank)), data=graphics_bc, verbose=TRUE)

summary(graph.bc.nb.null.offset)

anova(graph.bc.nb.null, graph.bc.nb.null.offset)

# no significant difference with offset; try once with a significant predictor 

graph.bc.nb.dataset.offset <- glmer.nb(NodeRank ~ Dataset + offset(log(MaxNodeRank)) + (1|Demo.ResponseID), data=graphics_bc, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

summary(graph.bc.nb.dataset.offset)

anova(graph.bc.nb.dataset, graph.bc.nb.dataset.offset)

# nope, offset didn't make any difference; proceeding with no-offset models
```

``` r
temp <- graphics_bc %>% dplyr::select(Demo.ResponseID, NodeRank, Dataset, DatasetOrder, CorrectAnswer, Stats.Q_TotalDuration, Demo.acfieldGrouped2, Demo.expreadnetvis, Demo.expcreatenetvis, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumHighDegree, NumLinks, NumNodes, NumNodesClust1) %>% drop_na()

temp <- temp %>% mutate(Stats.Q_TotalDuration=scale(Stats.Q_TotalDuration),
                        NormRank=NodeRank/NumNodes)
```

Run this one

``` r
#---------------------------------
# Multiple predictors
#---------------------------------

graph.bc.nb.full <- glmer.nb(NodeRank ~ Dataset + Stats.Q_TotalDuration + 
                               Demo.acfieldGrouped2 + Demo.expcreatenetvis +
                               (1|Demo.ResponseID),
                             data=temp, verbose=TRUE,
                             control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb.full)
# still significant - Dataset, Stats.Q_TotalDuration, Demo.acfieldGrouped2, Demo.expcreatenetvis
```

Run this one

``` r
#---------------------------------
# Interactions
#---------------------------------

graph.bc.nb.full.int <- glmer.nb(NodeRank ~ Dataset + Stats.Q_TotalDuration + 
                                   Demo.acfieldGrouped2 + Demo.expcreatenetvis +
                                   Stats.Q_TotalDuration:Demo.acfieldGrouped2 +
                                   (1|Demo.ResponseID),
                                 data=temp, verbose=TRUE,
                                 control=glmerControl(optimizer = "bobyqa"))

summary(graph.bc.nb.full.int)

anova(graph.bc.nb.full, graph.bc.nb.full.int)
# yes, interaction is significantly different from just main effects (p = 0.02812)
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.bc.nb.full.int, file = file.path(analysisDataDir,"fits/graph_bc_nb_full_int.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_bc_nb_full_int.RData"))
```

``` r
graph.bc.nb.full.int.f <- fortify(graph.bc.nb.full.int)

ggplot(graph.bc.nb.full.int.f, aes(.fitted,.resid)) + 
  geom_point() +
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

``` r
ggplot(graph.bc.nb.full.int.f, aes(.resid)) +
  geom_histogram()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-2.png)<!-- -->

``` r
# not *quite* normally distributed...  but not too skewed?

ggplot(graph.bc.nb.full.int.f, aes(.fitted,NodeRank)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-3.png)<!-- -->

``` r
ggplot(graph.bc.nb.full.int.f, aes(.fitted,NodeRank, color=Dataset)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-4.png)<!-- -->

``` r
ggplot(graph.bc.nb.full.int.f, aes(NodeRank,.fitted)) + 
  geom_point() +
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-5.png)<!-- -->

``` r
ggplot(graph.bc.nb.full.int.f, aes(NodeRank, .fitted, color=Dataset)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_color_brewer(palette = "Dark2")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-6.png)<!-- -->

``` r
ggplot(graph.bc.nb.full.int.f, aes(NodeRank,.fitted)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  facet_wrap(~Dataset)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-7.png)<!-- -->

``` r
(r2nsj = r2beta(graph.bc.nb.full.int, method = 'nsj', partial = TRUE))[1,'Rsq']
```

    ## [1] 0.1738031

``` r
ggplot(graphics_bc) + geom_histogram(aes(NodeRank), binwidth=1) + 
  labs(title="Distribution of NodeRank values for\nNode Betweenness Centrality task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-8.png)<!-- -->

``` r
ggplot(graph.bc.nb.full.int.f, aes(NodeRank,.fitted)) +
geom_bin2d() +
geom_abline(aes(slope = 1, intercept = 0)) +
#scale_y_continuous(limits=c(0,0.3)) +
labs(title="Real vs. Predicted NodeRank values for\nNode Betweenness Centrality task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-64-9.png)<!-- -->

``` r
# is negative binomial different from poisson?  if so, indicates over-dispersion is true
# and negative binomial is necessary

graph.bc.pois <- glmer(NodeRank ~ Dataset + Stats.Q_TotalDuration + 
                                   Demo.acfieldGrouped2 + Demo.expcreatenetvis +
                                   Stats.Q_TotalDuration:Demo.acfieldGrouped2 +
                                   (1|Demo.ResponseID), data=temp, family="poisson")


pchisq(2 * (logLik(graph.bc.nb.full.int) - logLik(graph.bc.pois)), df=1, lower.tail = FALSE)
```

    ## 'log Lik.' 6.566148e-73 (df=14)

``` r
# value = 6.566148e-73, so keep the negative binomial
```

``` r
# run without mixed effects to validate

m.glm <- glm.nb(NodeRank ~ Dataset + Stats.Q_TotalDuration + 
                                   Demo.acfieldGrouped2 + Demo.expcreatenetvis +
                                   Stats.Q_TotalDuration:Demo.acfieldGrouped2, data=temp, trace=TRUE)
summary(m.glm)
```

    ## 
    ## Call:
    ## glm.nb(formula = NodeRank ~ Dataset + Stats.Q_TotalDuration + 
    ##     Demo.acfieldGrouped2 + Demo.expcreatenetvis + Stats.Q_TotalDuration:Demo.acfieldGrouped2, 
    ##     data = temp, trace = TRUE, init.theta = 2.343582214, link = log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5107  -1.0040  -0.3177   0.2443   4.8431  
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error
    ## (Intercept)                                        1.313148   0.120134
    ## Dataset.L                                          0.610858   0.066013
    ## Dataset.Q                                         -0.430991   0.065294
    ## Dataset.C                                          0.399800   0.062233
    ## Dataset^4                                         -0.197515   0.060525
    ## Dataset^5                                          0.310676   0.060223
    ## Stats.Q_TotalDuration                              0.003274   0.039521
    ## Demo.acfieldGrouped2ZZ-Etc.                        0.303137   0.055941
    ## Demo.expcreatenetvis.L                             0.889321   0.247780
    ## Demo.expcreatenetvis.Q                             0.663185   0.229865
    ## Demo.expcreatenetvis.C                             0.335973   0.210268
    ## Stats.Q_TotalDuration:Demo.acfieldGrouped2ZZ-Etc. -0.161578   0.053468
    ##                                                   z value Pr(>|z|)    
    ## (Intercept)                                        10.931  < 2e-16 ***
    ## Dataset.L                                           9.254  < 2e-16 ***
    ## Dataset.Q                                          -6.601 4.09e-11 ***
    ## Dataset.C                                           6.424 1.33e-10 ***
    ## Dataset^4                                          -3.263 0.001101 ** 
    ## Dataset^5                                           5.159 2.49e-07 ***
    ## Stats.Q_TotalDuration                               0.083 0.933973    
    ## Demo.acfieldGrouped2ZZ-Etc.                         5.419 6.00e-08 ***
    ## Demo.expcreatenetvis.L                              3.589 0.000332 ***
    ## Demo.expcreatenetvis.Q                              2.885 0.003913 ** 
    ## Demo.expcreatenetvis.C                              1.598 0.110081    
    ## Stats.Q_TotalDuration:Demo.acfieldGrouped2ZZ-Etc.  -3.022 0.002512 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(2.3436) family taken to be 1)
    ## 
    ##     Null deviance: 1303.0  on 1155  degrees of freedom
    ## Residual deviance: 1050.3  on 1144  degrees of freedom
    ## AIC: 5247.2
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  2.344 
    ##           Std. Err.:  0.152 
    ## 
    ##  2 x log-likelihood:  -5221.236

``` r
## The neg.binomial theta parameter:
getME(graph.bc.nb.full.int, "glmer.nb.theta")
```

    ## [1] 3.835875

``` r
#3.835875

## mixed model has 1 additional parameter (RE variance)
stopifnot(attr(logLik(graph.bc.nb.full.int),"df")==attr(logLik(m.glm),"df")+1) # not sure what this does

anova(graph.bc.nb.full.int,m.glm) # can I use anova to compare mixed and fixed effects?
```

    ## Data: temp
    ## Models:
    ## m.glm: NodeRank ~ Dataset + Stats.Q_TotalDuration + Demo.acfieldGrouped2 + 
    ## m.glm:     Demo.expcreatenetvis + Stats.Q_TotalDuration:Demo.acfieldGrouped2
    ## graph.bc.nb.full.int: NodeRank ~ Dataset + Stats.Q_TotalDuration + Demo.acfieldGrouped2 + 
    ## graph.bc.nb.full.int:     Demo.expcreatenetvis + Stats.Q_TotalDuration:Demo.acfieldGrouped2 + 
    ## graph.bc.nb.full.int:     (1 | Demo.ResponseID)
    ##                      Df    AIC    BIC  logLik deviance  Chisq Chi Df
    ## m.glm                13 5247.2 5312.9 -2610.6   5221.2              
    ## graph.bc.nb.full.int 14 5177.9 5248.7 -2575.0   5149.9 71.321      1
    ##                      Pr(>Chisq)    
    ## m.glm                              
    ## graph.bc.nb.full.int  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# p < 2.2e-16, so definitely random effects

plot(graph.bc.nb.full.int, resid(.) ~ NodeRank)# works, as long as data 'dd' is found
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
# TO DO : check if this is all right
```

``` r
par(mfrow=c(2,2))
qqnorm(resid(graph.bc.nb.full.int), main="normal qq-plot, residuals")
qqline(resid(graph.bc.nb.full.int))

qqnorm(ranef(graph.bc.nb.full.int)$Demo.ResponseID[,1])
qqline(ranef(graph.bc.nb.full.int)$Demo.ResponseID[,1])


plot(fitted(graph.bc.nb.full.int), resid(graph.bc.nb.full.int)) #residuals vs fitted
abline(h=0)

#graph.avgdeg.nb2.f <- fortify(graph.avgdeg.nb2)

#ggplot(graph.avgdeg.nb2.f, aes(.fitted,.resid)) + 
#  geom_point() +
#  geom_hline(yintercept=0)

#temp <- graphics_avgdeg

temp$fitted <- fitted(graph.bc.nb.full.int) 
plot(temp$fitted, jitter(temp$NodeRank,0.1)) #fitted vs observed
abline(0,1)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
#ggplot(graph.avgdeg.nb2.f, aes(.fitted,Response)) + 
#  geom_point() +
#  geom_abline(aes(slope = 1, intercept = 0))
```

``` r
# Confidence Intervals, using coefficients

(est <- cbind(Estimate = coef(graph.avgdeg.nb.full.int), confint(graph.avgdeg.nb.full.int)))

# exponentiate model to look at incident rate ratios instead of coefficients

exp(est)
```

``` r
# predictions

# model: Response ~ Dataset + TaskOrder + Demo.dailytech_SmartPhone + Dataset:TaskOrder + TaskOrder:Demo.dailytech_SmartPhone + Dataset:Demo.dailytech_SmartPhone + (1 | Demo.ResponseID)

newdata1 <- data.frame(Demo.dailytech_SmartPhone = rep(mean(temp$Demo.dailytech_SmartPhone),54), 
                       Dataset = factor(rep(c(1,3,5,7,8,9),9), levels = levels(temp$Dataset),ordered = TRUE),
                       TaskOrder = rep(1:9,6),
                       Demo.ResponseID = sample(temp$Demo.ResponseID,54))
newdata1$phat <- predict(graph.avgdeg.nb.full.int, newdata1, type = "response")
#newdata1

newdata2 <- data.frame(
  Demo.dailytech_SmartPhone = rep(seq(from = min(temp$Demo.dailytech_SmartPhone), to = max(temp$Demo.dailytech_SmartPhone), length.out = 100), 6),
  Dataset = factor(rep(c(1,3,5,7,8,9), each = 100), levels = levels(temp$Dataset),ordered = TRUE),
  TaskOrder = rep(1:9,len=600),
  Demo.ResponseID = sample(temp$Demo.ResponseID,600)
  )

#predict(graph.avgdeg.nb.full.int, newdata2, type = "link", se.fit=TRUE)
newdata2 <- cbind(newdata2, predict(graph.avgdeg.nb.full.int, newdata2, type = "link", se.fit=TRUE))

# not sure about this; asking for "fit", but that's not recognized. should the new column be called "fit"? what about se.fit?

#newdata2 <- within(newdata2, {
#  Response <- exp(fit)
#  LL <- exp(fit - 1.96 * se.fit)
#  UL <- exp(fit + 1.96 * se.fit)
#})

#ggplot(newdata2, aes(math, DaysAbsent)) +
#  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
#  geom_line(aes(colour = prog), size = 2) +
#  labs(x = "Math Score", y = "Predicted Days Absent")

# note on negative binomial:

# TO DO : If the data generating process does not allow for any 0s (such as the number of days spent in the hospital), then a zero-truncated model may be more appropriate.

# TO DO : Count data often have an exposure variable, which indicates the number of times the event could have happened (i.e. a max). This variable should be incorporated into your negative binomial regression model with the use of the offset option. See the glm documentation for details.  (so, that would make sense for click data???)

# other info:
# Cameron, A. C. and Trivedi, P. K. 1998. Regression Analysis of Count Data. New York: Cambridge Press.
# Dupont, W. D. 2002. Statistical Modeling for Biomedical Researchers: A Simple Introduction to the Analysis of Complex Data. New York: Cambridge Press.
```

#### Least Squares

Full model (graph.bc.nb.full.int): NodeRank ~ Dataset +
Stats.Q\_TotalDuration + Demo.acfieldGrouped2 + Demo.expcreatenetvis +
Stats.Q\_TotalDuration:Demo.acfieldGrouped2 + (1|Demo.ResponseID)

##### Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.bc.emm.data <- emmeans(graph.bc.nb.full.int, "Dataset", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.bc.emm.data
```

    ##  Dataset    emmean        SE  df asymp.LCL asymp.UCL
    ##  1       0.6276297 0.1581509 Inf 0.3176595 0.9375998
    ##  3       1.6042802 0.1482513 Inf 1.3137130 1.8948474
    ##  5       1.3405016 0.1507783 Inf 1.0449816 1.6360215
    ##  7       1.6464824 0.1479834 Inf 1.3564402 1.9365246
    ##  8       1.4940898 0.1470611 Inf 1.2058553 1.7823242
    ##  9       1.6593051 0.1496461 Inf 1.3660042 1.9526061
    ## 
    ## Results are averaged over the levels of: Demo.acfieldGrouped2, Demo.expcreatenetvis 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95

``` r
graph.bc.emm.data.df <- dplyr::as_data_frame(graph.bc.emm.data)

graph.bc.emm.data.df
```

    ## # A tibble: 6 x 6
    ##   Dataset emmean    SE    df asymp.LCL asymp.UCL
    ## * <fct>    <dbl> <dbl> <dbl>     <dbl>     <dbl>
    ## 1 1        0.628 0.158   Inf     0.318     0.938
    ## 2 3        1.60  0.148   Inf     1.31      1.89 
    ## 3 5        1.34  0.151   Inf     1.04      1.64 
    ## 4 7        1.65  0.148   Inf     1.36      1.94 
    ## 5 8        1.49  0.147   Inf     1.21      1.78 
    ## 6 9        1.66  0.150   Inf     1.37      1.95

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.bc.emm.data.cld <- cld(graph.bc.emm.data,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.bc.emm.data.cld.df <- graph.bc.emm.data.cld$emmeans

graph.bc.emm.data.cld.df %>% dplyr::select(Dataset,.group)
```

    ##  Dataset .group
    ##  1        1    
    ##  5         2   
    ##  8         23  
    ##  3          3  
    ##  7          3  
    ##  9          3  
    ## 
    ## Results are averaged over the levels of: Demo.acfieldGrouped2, Demo.expcreatenetvis 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.data.cld.df
```

    ##  Dataset    emmean        SE  df asymp.LCL asymp.UCL .group
    ##  1       0.6276297 0.1581509 Inf 0.3176595 0.9375998  1    
    ##  5       1.3405016 0.1507783 Inf 1.0449816 1.6360215   2   
    ##  8       1.4940898 0.1470611 Inf 1.2058553 1.7823242   23  
    ##  3       1.6042802 0.1482513 Inf 1.3137130 1.8948474    3  
    ##  7       1.6464824 0.1479834 Inf 1.3564402 1.9365246    3  
    ##  9       1.6593051 0.1496461 Inf 1.3660042 1.9526061    3  
    ## 
    ## Results are averaged over the levels of: Demo.acfieldGrouped2, Demo.expcreatenetvis 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.data.cld.df$Dataset <- factor(graph.bc.emm.data.cld.df$Dataset, levels=graph.bc.emm.data.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Dataset) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.bc.emm.data, ~Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

``` r
plot(graph.bc.emm.data)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-70-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.bc.emm.data.cld.df %>% arrange(desc(emmean))
```

    ##   Dataset    emmean        SE  df asymp.LCL asymp.UCL .group
    ## 1       9 1.6593051 0.1496461 Inf 1.3660042 1.9526061      3
    ## 2       7 1.6464824 0.1479834 Inf 1.3564402 1.9365246      3
    ## 3       3 1.6042802 0.1482513 Inf 1.3137130 1.8948474      3
    ## 4       8 1.4940898 0.1470611 Inf 1.2058553 1.7823242     23
    ## 5       5 1.3405016 0.1507783 Inf 1.0449816 1.6360215     2 
    ## 6       1 0.6276297 0.1581509 Inf 0.3176595 0.9375998    1

``` r
ggplot(graph.bc.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-70-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Dataset for\nNode Betweenness Centrality task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-70-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.bc.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.bc.emm.data)) 
graph.bc.emm.data.pairs
```

    ## # A tibble: 15 x 6
    ##    contrast estimate     SE    df z.ratio            p.value
    ##  * <fct>       <dbl>  <dbl> <dbl>   <dbl>              <dbl>
    ##  1 1 - 3     -0.977  0.0928   Inf -10.5   0.0000000000000586
    ##  2 1 - 5     -0.713  0.0946   Inf - 7.53  0.000000000000772 
    ##  3 1 - 7     -1.02   0.0919   Inf -11.1   0.0000000000000266
    ##  4 1 - 8     -0.866  0.0934   Inf - 9.27  0.0000000000000638
    ##  5 1 - 9     -1.03   0.0921   Inf -11.2   0.0000000000000147
    ##  6 3 - 5      0.264  0.0799   Inf   3.30  0.0123            
    ##  7 3 - 7     -0.0422 0.0785   Inf - 0.537 0.995             
    ##  8 3 - 8      0.110  0.0796   Inf   1.38  0.736             
    ##  9 3 - 9     -0.0550 0.0788   Inf - 0.698 0.982             
    ## 10 5 - 7     -0.306  0.0813   Inf - 3.76  0.00232           
    ## 11 5 - 8     -0.154  0.0818   Inf - 1.88  0.417             
    ## 12 5 - 9     -0.319  0.0812   Inf - 3.93  0.00121           
    ## 13 7 - 8      0.152  0.0795   Inf   1.92  0.391             
    ## 14 7 - 9     -0.0128 0.0785   Inf - 0.163 1.000             
    ## 15 8 - 9     -0.165  0.0795   Inf - 2.08  0.299

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.bc.emm.data))

graph.bc.emm.data.pairs <- full_join(graph.bc.emm.data.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.bc.emm.data))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
plot(pairs(graph.bc.emm.data), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-71-2.png)<!-- -->

``` r
graph.bc.emm.data.pairs$sig.levels <- 
  case_when(graph.bc.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.bc.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.bc.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.bc.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.bc.emm.data.pairs$sig.levels <- factor(graph.bc.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.bc.emm.data.pairs$contrast <- factor(graph.bc.emm.data.pairs$contrast, levels=graph.bc.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.bc.emm.data.pairs <- graph.bc.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.bc.emm.data.pairs %>% arrange(estimate)
```

    ## # A tibble: 15 x 11
    ##    contrast From  To    estimate     SE    df z.ratio    p.value asymp.LCL
    ##    <fct>    <chr> <chr>    <dbl>  <dbl> <dbl>   <dbl>      <dbl>     <dbl>
    ##  1 1 - 9    1     9      -1.03   0.0921   Inf -11.2     1.47e⁻¹⁴   -1.29  
    ##  2 1 - 7    1     7      -1.02   0.0919   Inf -11.1     2.66e⁻¹⁴   -1.28  
    ##  3 1 - 3    1     3      -0.977  0.0928   Inf -10.5     5.86e⁻¹⁴   -1.24  
    ##  4 1 - 8    1     8      -0.866  0.0934   Inf - 9.27    6.38e⁻¹⁴   -1.13  
    ##  5 1 - 5    1     5      -0.713  0.0946   Inf - 7.53    7.72e⁻¹³   -0.982 
    ##  6 5 - 9    5     9      -0.319  0.0812   Inf - 3.93    1.21e⁻ ³   -0.550 
    ##  7 5 - 7    5     7      -0.306  0.0813   Inf - 3.76    2.32e⁻ ³   -0.538 
    ##  8 8 - 9    8     9      -0.165  0.0795   Inf - 2.08    2.99e⁻ ¹   -0.392 
    ##  9 5 - 8    5     8      -0.154  0.0818   Inf - 1.88    4.17e⁻ ¹   -0.387 
    ## 10 3 - 9    3     9      -0.0550 0.0788   Inf - 0.698   9.82e⁻ ¹   -0.280 
    ## 11 3 - 7    3     7      -0.0422 0.0785   Inf - 0.537   9.95e⁻ ¹   -0.266 
    ## 12 7 - 9    7     9      -0.0128 0.0785   Inf - 0.163  10.00e⁻ ¹   -0.236 
    ## 13 3 - 8    3     8       0.110  0.0796   Inf   1.38    7.36e⁻ ¹   -0.117 
    ## 14 7 - 8    7     8       0.152  0.0795   Inf   1.92    3.91e⁻ ¹   -0.0741
    ## 15 3 - 5    3     5       0.264  0.0799   Inf   3.30    1.23e⁻ ²    0.0362
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-71-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-71-4.png)<!-- -->

``` r
#cond.lev <- c("Ctrl","Phr","Col","Siz")


copy <- graph.bc.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         z.ratio=-z.ratio,
         asymp.LCL=-asymp.LCL,
         asymp.UCL=-asymp.UCL)

graph.bc.emm.data.pairs.compl <- bind_rows(graph.bc.emm.data.pairs, copy)

copy$From <- factor(copy$From, levels=rev(unique(copy$From)))
#graph.bc.emm.data.pairs$To <- factor(graph.bc.emm.data.pairs$To, levels=cond.lev)
#graph.bc.emm.data.pairs$From <- factor(graph.bc.emm.data.pairs$From, levels=rev(cond.lev))

highest<-max(abs(copy$estimate))

ggplot(copy, aes(x=To,y=From)) +
  geom_tile(aes(fill=estimate)) +
  geom_text(aes(label=paste0(format(estimate, digits=2, nsmall=2),"\n(",sig.levels,")"))) +
    scale_fill_distiller(type="div", palette=4, limits=c(-highest,highest)) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

``` r
graph.bc.emm.data.pairs.compl$From <- factor(graph.bc.emm.data.pairs.compl$From, levels=rev(unique(graph.bc.emm.data.pairs.compl$From)))
#graph.bc.emm.data.pairs.compl$To <- factor(graph.bc.emm.data.pairs.compl$To, levels=cond.lev)
#graph.bc.emm.data.pairs.compl$From <- factor(graph.bc.emm.data.pairs.compl$From, levels=rev(cond.lev))


#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.bc.emm.data.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 30 x 11
    ##    contrast From  To    estimate     SE    df z.ratio    p.value asymp.LCL
    ##    <fct>    <fct> <chr>    <dbl>  <dbl> <dbl>   <dbl>      <dbl>     <dbl>
    ##  1 1 - 9    1     9       -1.03  0.0921   Inf  -11.2    1.47e⁻¹⁴   -1.29  
    ##  2 1 - 7    1     7       -1.02  0.0919   Inf  -11.1    2.66e⁻¹⁴   -1.28  
    ##  3 1 - 3    1     3       -0.977 0.0928   Inf  -10.5    5.86e⁻¹⁴   -1.24  
    ##  4 1 - 8    1     8       -0.866 0.0934   Inf  - 9.27   6.38e⁻¹⁴   -1.13  
    ##  5 1 - 5    1     5       -0.713 0.0946   Inf  - 7.53   7.72e⁻¹³   -0.982 
    ##  6 5 - 9    5     9       -0.319 0.0812   Inf  - 3.93   1.21e⁻ ³   -0.550 
    ##  7 5 - 7    5     7       -0.306 0.0813   Inf  - 3.76   2.32e⁻ ³   -0.538 
    ##  8 3 - 5    5     3       -0.264 0.0799   Inf  - 3.30   1.23e⁻ ²   -0.0362
    ##  9 8 - 9    8     9       -0.165 0.0795   Inf  - 2.08   2.99e⁻ ¹   -0.392 
    ## 10 5 - 8    5     8       -0.154 0.0818   Inf  - 1.88   4.17e⁻ ¹   -0.387 
    ## # ... with 20 more rows, and 2 more variables: asymp.UCL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.bc.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

``` r
ggplot(graph.bc.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-73-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-73-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-73-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.bc.emm.data.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*10
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.bc.emm.data.pairs.compl %>% dplyr::select(-contrast) %>% filter(sig.levels != "NS"), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Stats.Q\_TotalDuration

``` r
ggplot(graphics_bc) + 
  geom_bin2d(aes(Stats.Q_TotalDuration,NodeRank)) +
  geom_smooth(aes(Stats.Q_TotalDuration,NodeRank), method="lm") +
  labs(title="Total Survey Duration vs. NodeRank for\nNode Betweenness Centrality task, graphics conditions",
       x="Total Duration (sec)")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->

##### Demo.acfieldGrouped2

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.bc.emm.acfield2 <- emmeans(graph.bc.nb.full.int, "Demo.acfieldGrouped2", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.bc.emm.acfield2
```

    ##  Demo.acfieldGrouped2   emmean        SE  df asymp.LCL asymp.UCL
    ##  SignificantGroup     1.255031 0.1466623 Inf 0.9675784  1.542484
    ##  ZZ-Etc.              1.535732 0.1418258 Inf 1.2577584  1.813705
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.expcreatenetvis 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95

``` r
graph.bc.emm.acfield2.df <- dplyr::as_data_frame(graph.bc.emm.acfield2)

graph.bc.emm.acfield2.df
```

    ## # A tibble: 2 x 6
    ##   Demo.acfieldGrouped2 emmean    SE    df asymp.LCL asymp.UCL
    ## * <fct>                 <dbl> <dbl> <dbl>     <dbl>     <dbl>
    ## 1 SignificantGroup       1.26 0.147   Inf     0.968      1.54
    ## 2 ZZ-Etc.                1.54 0.142   Inf     1.26       1.81

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.bc.emm.acfield2.cld <- cld(graph.bc.emm.acfield2,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.bc.emm.acfield2.cld.df <- graph.bc.emm.acfield2.cld$emmeans

graph.bc.emm.acfield2.cld.df %>% dplyr::select(Demo.acfieldGrouped2,.group)
```

    ##  Demo.acfieldGrouped2 .group
    ##  SignificantGroup      1    
    ##  ZZ-Etc.                2   
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.expcreatenetvis 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.acfield2.cld.df
```

    ##  Demo.acfieldGrouped2   emmean        SE  df asymp.LCL asymp.UCL .group
    ##  SignificantGroup     1.255031 0.1466623 Inf 0.9675784  1.542484  1    
    ##  ZZ-Etc.              1.535732 0.1418258 Inf 1.2577584  1.813705   2   
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.expcreatenetvis 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.acfield2.cld.df$Demo.acfieldGrouped2 <- factor(graph.bc.emm.acfield2.cld.df$Demo.acfieldGrouped2, levels=graph.bc.emm.acfield2.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Demo.acfieldGrouped2) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.bc.emm.acfield2, ~Demo.acfieldGrouped2, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-77-1.png)<!-- -->

``` r
plot(graph.bc.emm.acfield2)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-77-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.bc.emm.acfield2.cld.df %>% arrange(desc(emmean))
```

    ##   Demo.acfieldGrouped2   emmean        SE  df asymp.LCL asymp.UCL .group
    ## 1              ZZ-Etc. 1.535732 0.1418258 Inf 1.2577584  1.813705      2
    ## 2     SignificantGroup 1.255031 0.1466623 Inf 0.9675784  1.542484     1

``` r
ggplot(graph.bc.emm.acfield2.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.acfieldGrouped2,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Demo.acfieldGrouped2,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-77-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield2.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.acfieldGrouped2,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Demo.acfieldGrouped2,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
  scale_x_discrete(labels=c("High Error Group","Low Error Group")) +
labs(title="Estimated Marginal Means for Academic Field for\nNode Betweenness Centrality task, graphics conditions",
     x="Academic Field")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-77-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.bc.emm.acfield2.pairs <- dplyr::as_data_frame(pairs(graph.bc.emm.acfield2)) 
graph.bc.emm.acfield2.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast                   estimate     SE    df z.ratio   p.value
    ## * <fct>                         <dbl>  <dbl> <dbl>   <dbl>     <dbl>
    ## 1 SignificantGroup - ZZ-Etc.   -0.281 0.0686   Inf   -4.09 0.0000429

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.bc.emm.acfield2))

graph.bc.emm.acfield2.pairs <- full_join(graph.bc.emm.acfield2.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.bc.emm.acfield2))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

``` r
#plot(pairs(graph.bc.emm.acfield2), comparisons = TRUE)

graph.bc.emm.acfield2.pairs$sig.levels <- 
  case_when(graph.bc.emm.acfield2.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.bc.emm.acfield2.pairs$p.value < .001 ~ sig.level.names[2],
            graph.bc.emm.acfield2.pairs$p.value < .01 ~ sig.level.names[3],
            graph.bc.emm.acfield2.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.bc.emm.acfield2.pairs$sig.levels <- factor(graph.bc.emm.acfield2.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.bc.emm.acfield2.pairs$contrast <- factor(graph.bc.emm.acfield2.pairs$contrast, levels=graph.bc.emm.acfield2.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.bc.emm.acfield2.pairs <- graph.bc.emm.acfield2.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.bc.emm.acfield2.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast    From   To    estimate     SE    df z.ratio p.value asymp.LCL
    ##   <fct>       <chr>  <chr>    <dbl>  <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    ## 1 Significan… Signi… ZZ-E…   -0.281 0.0686   Inf   -4.09 4.29e⁻⁵    -0.415
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.acfield2.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-78-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield2.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-78-3.png)<!-- -->

``` r
#cond.lev <- c("Ctrl","Phr","Col","Siz")


copy <- graph.bc.emm.acfield2.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         z.ratio=-z.ratio,
         asymp.LCL=-asymp.LCL,
         asymp.UCL=-asymp.UCL)

graph.bc.emm.acfield2.pairs.compl <- bind_rows(graph.bc.emm.acfield2.pairs, copy)

copy$From <- factor(copy$From, levels=rev(unique(copy$From)))
#graph.bc.emm.data.pairs$To <- factor(graph.bc.emm.data.pairs$To, levels=cond.lev)
#graph.bc.emm.data.pairs$From <- factor(graph.bc.emm.data.pairs$From, levels=rev(cond.lev))

highest<-max(abs(copy$estimate))

ggplot(copy, aes(x=To,y=From)) +
  geom_tile(aes(fill=estimate)) +
  geom_text(aes(label=paste0(format(estimate, digits=2, nsmall=2),"\n(",sig.levels,")"))) +
    scale_fill_distiller(type="div", palette=4, limits=c(-highest,highest)) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

``` r
graph.bc.emm.acfield2.pairs.compl$From <- factor(graph.bc.emm.acfield2.pairs.compl$From, levels=rev(unique(graph.bc.emm.acfield2.pairs.compl$From)))
#graph.bc.emm.data.pairs.compl$To <- factor(graph.bc.emm.data.pairs.compl$To, levels=cond.lev)
#graph.bc.emm.data.pairs.compl$From <- factor(graph.bc.emm.data.pairs.compl$From, levels=rev(cond.lev))


#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.bc.emm.acfield2.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast   From   To     estimate     SE    df z.ratio p.value asymp.LCL
    ##   <fct>      <fct>  <chr>     <dbl>  <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    ## 1 Significa… Signi… ZZ-Et…   -0.281 0.0686   Inf   -4.09 4.29e⁻⁵    -0.415
    ## 2 Significa… ZZ-Et… Signi…    0.281 0.0686   Inf    4.09 4.29e⁻⁵     0.415
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.acfield2.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield2.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-80-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield2.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-80-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield2.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-80-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.bc.emm.acfield2.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*10
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.bc.emm.acfield2.pairs %>% dplyr::select(-contrast) %>% filter(sig.levels != "NS"), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  #scale_edge_color_manual("Significance Levels", values=sig.colors) +
  #scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Demo.expcreatenetvis

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.bc.emm.ecnv <- emmeans(graph.bc.nb.full.int, "Demo.expcreatenetvis", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.bc.emm.ecnv
```

    ##  Demo.expcreatenetvis   emmean         SE  df asymp.LCL asymp.UCL
    ##  None                 1.021001 0.03678073 Inf 0.9489118  1.093090
    ##  A little             1.044763 0.09725288 Inf 0.8541504  1.235375
    ##  Some                 1.006482 0.35280439 Inf 0.3149978  1.697966
    ##  A lot                2.509281 0.42023116 Inf 1.6856430  3.332919
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.acfieldGrouped2 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95

``` r
graph.bc.emm.ecnv.df <- dplyr::as_data_frame(graph.bc.emm.ecnv)

graph.bc.emm.ecnv.df
```

    ## # A tibble: 4 x 6
    ##   Demo.expcreatenetvis emmean     SE    df asymp.LCL asymp.UCL
    ## * <fct>                 <dbl>  <dbl> <dbl>     <dbl>     <dbl>
    ## 1 None                   1.02 0.0368   Inf     0.949      1.09
    ## 2 A little               1.04 0.0973   Inf     0.854      1.24
    ## 3 Some                   1.01 0.353    Inf     0.315      1.70
    ## 4 A lot                  2.51 0.420    Inf     1.69       3.33

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.bc.emm.ecnv.cld <- cld(graph.bc.emm.ecnv,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.bc.emm.ecnv.cld.df <- graph.bc.emm.ecnv.cld$emmeans

graph.bc.emm.ecnv.cld.df %>% dplyr::select(Demo.expcreatenetvis,.group)
```

    ##  Demo.expcreatenetvis .group
    ##  Some                  1    
    ##  None                  1    
    ##  A little              1    
    ##  A lot                  2   
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.acfieldGrouped2 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.ecnv.cld.df
```

    ##  Demo.expcreatenetvis   emmean         SE  df asymp.LCL asymp.UCL .group
    ##  Some                 1.006482 0.35280439 Inf 0.3149978  1.697966  1    
    ##  None                 1.021001 0.03678073 Inf 0.9489118  1.093090  1    
    ##  A little             1.044763 0.09725288 Inf 0.8541504  1.235375  1    
    ##  A lot                2.509281 0.42023116 Inf 1.6856430  3.332919   2   
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.acfieldGrouped2 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
#graph.bc.emm.ecnv.cld.df$Demo.expcreatenetvis <- factor(graph.bc.emm.ecnv.cld.df$Demo.expcreatenetvis, levels=graph.bc.emm.ecnv.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Demo.expcreatenetvis) %>% unlist())
graph.bc.emm.ecnv.cld.df$Demo.expcreatenetvis <- factor(graph.bc.emm.ecnv.cld.df$Demo.expcreatenetvis, levels=freq4)


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.bc.emm.ecnv, ~Demo.expcreatenetvis, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
plot(graph.bc.emm.ecnv)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-83-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.bc.emm.ecnv.cld.df %>% arrange(Demo.expcreatenetvis)
```

    ##   Demo.expcreatenetvis   emmean         SE  df asymp.LCL asymp.UCL .group
    ## 1                 None 1.021001 0.03678073 Inf 0.9489118  1.093090     1 
    ## 2             A little 1.044763 0.09725288 Inf 0.8541504  1.235375     1 
    ## 3                 Some 1.006482 0.35280439 Inf 0.3149978  1.697966     1 
    ## 4                A lot 2.509281 0.42023116 Inf 1.6856430  3.332919      2

``` r
ggplot(graph.bc.emm.ecnv.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.expcreatenetvis,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Demo.expcreatenetvis,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-83-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.ecnv.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=ordered(Demo.expcreatenetvis,levels=rev(freq4)),ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Demo.expcreatenetvis,y=emmean), size=7) +
  coord_flip() +
  labs(title="Estimated Marginal Means for Experience Creating Network Visualizations for\nNode Betweenness Centrality task, graphics conditions",
       x="Experience Creating Network Visualizations")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-83-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.bc.emm.ecnv.pairs <- dplyr::as_data_frame(pairs(graph.bc.emm.ecnv)) 
graph.bc.emm.ecnv.pairs
```

    ## # A tibble: 6 x 6
    ##   contrast         estimate    SE    df z.ratio p.value
    ## * <fct>               <dbl> <dbl> <dbl>   <dbl>   <dbl>
    ## 1 None - A little   -0.0238 0.100   Inf -0.237  0.995  
    ## 2 None - Some        0.0145 0.354   Inf  0.0410 1.000  
    ## 3 None - A lot      -1.49   0.422   Inf -3.53   0.00237
    ## 4 A little - Some    0.0383 0.365   Inf  0.105  1.000  
    ## 5 A little - A lot  -1.46   0.431   Inf -3.39   0.00384
    ## 6 Some - A lot      -1.50   0.548   Inf -2.74   0.0312

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.bc.emm.ecnv))

graph.bc.emm.ecnv.pairs <- full_join(graph.bc.emm.ecnv.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.bc.emm.ecnv))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
#plot(pairs(graph.bc.emm.acfield2), comparisons = TRUE)

graph.bc.emm.ecnv.pairs$sig.levels <- 
  case_when(graph.bc.emm.ecnv.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.bc.emm.ecnv.pairs$p.value < .001 ~ sig.level.names[2],
            graph.bc.emm.ecnv.pairs$p.value < .01 ~ sig.level.names[3],
            graph.bc.emm.ecnv.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.bc.emm.ecnv.pairs$sig.levels <- factor(graph.bc.emm.ecnv.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.bc.emm.ecnv.pairs$contrast <- factor(graph.bc.emm.ecnv.pairs$contrast, levels=graph.bc.emm.ecnv.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.bc.emm.ecnv.pairs <- graph.bc.emm.ecnv.pairs %>% separate(contrast, c("From", "To"), sep="[-]", remove=FALSE) %>% mutate(From=str_trim(From),To=str_trim(To))

graph.bc.emm.ecnv.pairs %>% arrange(estimate)
```

    ## # A tibble: 6 x 11
    ##   contrast    From   To     estimate    SE    df z.ratio p.value asymp.LCL
    ##   <fct>       <chr>  <chr>     <dbl> <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    ## 1 Some - A l… Some   A lot   -1.50   0.548   Inf -2.74   0.0312     -2.91 
    ## 2 None - A l… None   A lot   -1.49   0.422   Inf -3.53   0.00237    -2.57 
    ## 3 A little -… A lit… A lot   -1.46   0.431   Inf -3.39   0.00384    -2.57 
    ## 4 None - A l… None   A lit…  -0.0238 0.100   Inf -0.237  0.995      -0.281
    ## 5 None - Some None   Some     0.0145 0.354   Inf  0.0410 1.000      -0.895
    ## 6 A little -… A lit… Some     0.0383 0.365   Inf  0.105  1.000      -0.899
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.ecnv.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-84-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.ecnv.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-84-3.png)<!-- -->

``` r
#cond.lev <- c("Ctrl","Phr","Col","Siz")


copy <- graph.bc.emm.ecnv.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         z.ratio=-z.ratio,
         asymp.LCL=-asymp.LCL,
         asymp.UCL=-asymp.UCL)

graph.bc.emm.ecnv.pairs.compl <- bind_rows(graph.bc.emm.ecnv.pairs, copy)

#copy$From <- factor(copy$From, levels=rev(unique(copy$From)))
copy$To <- factor(copy$To, levels=freq4)
copy$From <- factor(copy$From, levels=rev(freq4))

highest<-max(abs(copy$estimate))

ggplot(copy, aes(x=To,y=From)) +
  geom_tile(aes(fill=estimate)) +
  geom_text(aes(label=paste0(format(estimate, digits=2, nsmall=2),"\n(",sig.levels,")"))) +
    scale_fill_distiller(type="div", palette=4, limits=c(-highest,highest)) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-85-1.png)<!-- -->

``` r
#graph.bc.emm.ecnv.pairs.compl$From <- factor(graph.bc.emm.ecnv.pairs.compl$From, levels=rev(unique(graph.bc.emm.ecnv.pairs.compl$From)))
graph.bc.emm.ecnv.pairs.compl$To <- factor(graph.bc.emm.ecnv.pairs.compl$To, levels=freq4)
graph.bc.emm.ecnv.pairs.compl$From <- factor(graph.bc.emm.ecnv.pairs.compl$From, levels=rev(freq4))


#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.bc.emm.ecnv.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 12 x 11
    ##    contrast    From   To    estimate    SE    df z.ratio p.value asymp.LCL
    ##    <fct>       <fct>  <fct>    <dbl> <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    ##  1 Some - A l… Some   A lot  -1.50   0.548   Inf -2.74   0.0312     -2.91 
    ##  2 None - A l… None   A lot  -1.49   0.422   Inf -3.53   0.00237    -2.57 
    ##  3 A little -… A lit… A lot  -1.46   0.431   Inf -3.39   0.00384    -2.57 
    ##  4 A little -… Some   A li…  -0.0383 0.365   Inf -0.105  1.000       0.899
    ##  5 None - A l… None   A li…  -0.0238 0.100   Inf -0.237  0.995      -0.281
    ##  6 None - Some Some   None   -0.0145 0.354   Inf -0.0410 1.000       0.895
    ##  7 None - Some None   Some    0.0145 0.354   Inf  0.0410 1.000      -0.895
    ##  8 None - A l… A lit… None    0.0238 0.100   Inf  0.237  0.995       0.281
    ##  9 A little -… A lit… Some    0.0383 0.365   Inf  0.105  1.000      -0.899
    ## 10 A little -… A lot  A li…   1.46   0.431   Inf  3.39   0.00384     2.57 
    ## 11 None - A l… A lot  None    1.49   0.422   Inf  3.53   0.00237     2.57 
    ## 12 Some - A l… A lot  Some    1.50   0.548   Inf  2.74   0.0312      2.91 
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.ecnv.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

``` r
ggplot(graph.bc.emm.ecnv.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-86-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.ecnv.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-86-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.ecnv.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-86-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.bc.emm.ecnv.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*10
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.bc.emm.ecnv.pairs %>% dplyr::select(-contrast) %>% filter(sig.levels != "NS"), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  #scale_edge_color_manual("Significance Levels", values=sig.colors) +
  #scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Demo.acfieldGrouped2:Stats.Q\_TotalDuration

``` r
ggplot(graphics_bc) + 
  geom_bin2d(aes(Stats.Q_TotalDuration,NodeRank)) +
  geom_smooth(aes(Stats.Q_TotalDuration,NodeRank), method="lm") +
  labs(title="Total Survey Duration vs. NodeRank by Academic Field for\nNode Betweenness Centrality task, graphics conditions",
       x="Total Duration (sec)") +
  facet_wrap(~factor(Demo.acfieldGrouped2, labels=c("Low Error Group","High Error Group")))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->

``` r
ggplot(graphics_bc) +
  geom_bar(aes(NodeRank)) +
  #coord_flip() +
  labs(title="Rank of Selected Node for Betweenness Centrality Task by Dataset",
       x="Rank of selected node", y="Number of times selected") +
  facet_wrap(~Dataset, scales="free_x")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Select Highest Degree Node

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_clickhighdeg <- readRDS(file.path(analysisDataDir, "GraphicsClickHD.rds"))
```

#### Negative binomial model for click questions Node Rank

Ranks are like count data, in that they are nonnegative integers, so
using a negative binomial distribution to model. Negative binomial is
especially useful for over-dispersed data - data where the conditional
variances exceed conditional
means.

``` r
# https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/ for non-mixed version

# Test for overdispersion

with(graphics_clickhighdeg, tapply(NodeRank, Condition, function(x) {
    sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
```

    ##                   Ctrl                    Cir                    Col 
    ## "M (SD) = 2.15 (2.08)"                     NA "M (SD) = 2.10 (2.29)" 
    ##                    Fru                    Ord                    Phr 
    ##                     NA                     NA "M (SD) = 1.92 (1.97)" 
    ##                    Siz 
    ## "M (SD) = 2.06 (2.23)"

``` r
# variances in each condition (except Control) are larger than means in the conditions
```

``` r
graph.clickhd.nb.null <- glmer.nb(NodeRank ~ (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

graph.clickhd.nb <- glmer.nb(NodeRank ~ Condition + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# model is not significantly different from null model (p=0.5654), so Condition isn't a
# significant predictor

graph.clickhd.nb <- glmer.nb(NodeRank ~ factor(Ctrl_dummy) + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Ctrl_dummy is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ ConditionPhrasing + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# ConditionPhrasing is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ ConditionPhrasing + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(Condition %in% c("Ctrl","Phr")), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - ConditionPhrasing)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# ConditionPhrasing is not significant


graph.clickhd.nb <- glmer.nb(NodeRank ~ ConditionGraphics + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# ConditionGraphics is not significant


graph.clickhd.nb.dataset <- glmer.nb(NodeRank ~ Dataset + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb.dataset)

anova(graph.clickhd.nb.dataset, graph.clickhd.nb.null)

# model is significantly different from null model (p < 2.2e-16)

graph.clickhd.nb <- glmer.nb(NodeRank ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# DatasetOrder is not significant

#graph.clickhd.nb <- glmer.nb(NodeRank ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_clickhighdeg %>% filter(!is.na(DatasetDuration)), verbose=TRUE)

# major error: PIRLS step-halvings failed to reduce deviance in pwrssUpdate; rescale data

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(DatasetDuration=scale(DatasetDuration))

graph.clickhd.nb <- glmer.nb(NodeRank ~ DatasetDuration + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS %>% filter(!is.na(DatasetDuration)), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null2 <- update(graph.clickhd.nb, . ~ . - DatasetDuration)

anova(graph.clickhd.nb, graph.clickhd.nb.null2)

# DatasetDuration is not significant 

graph.clickhd.nb <- glmer.nb(NodeRank ~ TaskOrder + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# TaskOrder is not significant

#graph.clickhd.nb <- glmer.nb(NodeRank ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

# warnings about rescaling variables

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(QuestionOrder=scale(QuestionOrder))

graph.clickhd.nb <- glmer.nb(NodeRank ~ QuestionOrder + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null2 <- update(graph.clickhd.nb, . ~ . - QuestionOrder)

anova(graph.clickhd.nb, graph.clickhd.nb.null2)

# QuestionOrder is not significant


#graph.clickhd.nb <- glmer.nb(NodeRank ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

# error about rescaling

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(CorrectAnswer=scale(CorrectAnswer))

graph.clickhd.nb.correct <- glmer.nb(NodeRank ~ CorrectAnswer + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb.correct)

graph.clickhd.nb.null2 <- update(graph.clickhd.nb.correct, . ~ . - CorrectAnswer)

anova(graph.clickhd.nb.correct, graph.clickhd.nb.null2)

# CorrectAnswer is significant (p=4.931e-13)

#graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

# warnings about rescaling variables

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(Stats.Q_TotalDuration=scale(Stats.Q_TotalDuration))

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_clickhighdeg.CS, verbose=TRUE, control=glmerControl(optimizer = "nlminbw"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null2 <- update(graph.clickhd.nb, . ~ . - Stats.Q_TotalDuration)

anova(graph.clickhd.nb, graph.clickhd.nb.null2)

# Stats.Q_TotalDuration is not significant


graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))
# very slooooooow, but no errors

summary(graph.clickhd.nb) # nothing significant

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystems is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb) # nothing significant

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemCombined is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined2 + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemCombined2 is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined3 + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemCombined3 is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined4 + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemCombined4 is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemCombined5 + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemCombined5 is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemWindows + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemWindows is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemMacintosh + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemMacintosh is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemAndroid + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemAndroid is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Stats.OperatingSystemiPhone + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE, control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Stats.OperatingSystemiPhone is not significant


#graph.clickhd.nb <- glmer.nb(NodeRank ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_clickhighdeg, verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(StatsNumPixels=scale(StatsNumPixels))

graph.clickhd.nb <- glmer.nb(NodeRank ~ StatsNumPixels + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE, 
                        control=glmerControl(optimizer = "nlminbw"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null2 <- update(graph.clickhd.nb, . ~ . - StatsNumPixels)

anova(graph.clickhd.nb, graph.clickhd.nb.null2)

# StatsNumPixels not significant

#graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.age + (1|Demo.ResponseID), data=graphics_clickhighdeg %>% filter(!is.na(Demo.age)), verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(Demo.age=scale(Demo.age))

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.age + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS %>% filter(!is.na(Demo.age)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))


summary(graph.clickhd.nb)

graph.clickhd.nb.null2 <- update(graph.clickhd.nb, . ~ . - Demo.age)

anova(graph.clickhd.nb, graph.clickhd.nb.null2)

# Demo.age is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.gender + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.gender)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.gender)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# Demo.gender is not significant 

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.lang + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.lang)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.lang)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# Demo.lang is not a significant predictor

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.educ + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.educ)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.educ)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# Demo.educ is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.acfield + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.acfield)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))
# tons of categories, very slow

summary(graph.clickhd.nb)

# many categories significantly different from Anthropology:
# ***: Business
# **: Arts, Computer sciences, Economics, Information science, Law, Linguistics, 
#     Medicine, Other, Political science, Skipped, Sociology
# *: Architecture and design, Biology, Education, Engineering, History, Journalism, 
#    media studies and communication, Languages, Library and museum studies, Literature, 
#    Mathematics, Philosophy, Psychology, Public administration

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.acfield)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.acfieldGrouped + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.acfieldGrouped)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.acfieldGrouped)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# Demo.acfieldGrouped is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.acfieldGrouped2 + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.acfieldGrouped2)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.acfieldGrouped2)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# Demo.acfieldGrouped2 is not significant

graph.clickhd.nb.acfield3 <- glmer.nb(NodeRank ~ Demo.acfieldGrouped3 + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.acfieldGrouped3)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb.acfield3)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb.acfield3, . ~ . - Demo.acfieldGrouped3)

anova(graph.clickhd.nb.acfield3, graph.clickhd.nb.null.2)

# Demo.acfieldGrouped3 (made up of ** and *** fields from clickhd.nb - Demo.acfield) is significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.dailytech_Computer + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.dailytech_Computer)), verbose=TRUE)

summary(graph.clickhd.nb)

# almost significant

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.dailytech_Computer)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.dailytech_Tablet)), verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.dailytech_Tablet)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# almost significant (p=0.06001)

graph.clickhd.nb.SP <- glmer.nb(NodeRank ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.dailytech_SmartPhone)), verbose=TRUE)

summary(graph.clickhd.nb.SP)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb.SP, . ~ . - Demo.dailytech_SmartPhone)

anova(graph.clickhd.nb.SP, graph.clickhd.nb.null.2)

# significant (p = 0.008337)

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.weeklygaming + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.weeklygaming)), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.weeklygaming)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.expdataanal + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.expdataanal)), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.expdataanal)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.expdatavis + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.expdatavis)), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.expdatavis)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.expreadnetvis + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.expreadnetvis)), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.expreadnetvis)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Demo.expcreatenetvis + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg %>% filter(!is.na(Demo.expcreatenetvis)), verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - Demo.expcreatenetvis)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# not significant

graph.clickhd.nb.avgdeg <- glmer.nb(NodeRank ~ AvgDeg + (1|Demo.ResponseID), 
                               data=graphics_clickhighdeg, verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb.avgdeg)

anova(graph.clickhd.nb.avgdeg, graph.clickhd.nb.null)

# AvgDeg is marginally significant (p < 0.02327)

graph.clickhd.nb <- glmer.nb(NodeRank ~ Density + (1|Demo.ResponseID), 
                                data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Density is not significant

#graph.clickhd.nb.largeclust <- glmer.nb(NodeRank ~ LargeClust1 + (1|Demo.ResponseID), 
#                                   data=graphics_clickhighdeg, verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(LargeClust1=scale(LargeClust1))

graph.clickhd.nb <- glmer.nb(NodeRank ~ LargeClust1 + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - LargeClust1)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# LargeClust1 is not significant

graph.clickhd.nb <- glmer.nb(NodeRank ~ Modularity + (1|Demo.ResponseID), 
                            data=graphics_clickhighdeg, verbose=TRUE)

summary(graph.clickhd.nb)

anova(graph.clickhd.nb, graph.clickhd.nb.null)

# Modularity is not significant

graph.clickhd.nb.numclust <- glmer.nb(NodeRank ~ NumClust + (1|Demo.ResponseID), 
                                 data=graphics_clickhighdeg, verbose=TRUE, 
                        control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb.numclust)

anova(graph.clickhd.nb.numclust, graph.clickhd.nb.null)

# NumClust is significant (p = 0.0008861)

#graph.clickhd.nb <- glmer.nb(NodeRank ~ NumHighDegree + (1|Demo.ResponseID), 
#                        data=graphics_clickhighdeg, verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(NumHighDegree=scale(NumHighDegree))

graph.clickhd.nb.numhighdeg <- glmer.nb(NodeRank ~ NumHighDegree + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb.numhighdeg)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb.numhighdeg, . ~ . - NumHighDegree)

anova(graph.clickhd.nb.numhighdeg, graph.clickhd.nb.null.2)

# NumHighDegree is significant (p=4.931e-13)

#graph.clickhd.nb.numlinks <- glmer.nb(NodeRank ~ NumLinks + (1|Demo.ResponseID), 
#                                 data=graphics_clickhighdeg, verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(NumLinks=scale(NumLinks))

graph.clickhd.nb <- glmer.nb(NodeRank ~ NumLinks + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - NumLinks)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# NumLinks is barely not significant (p=0.05679)

#graph.clickhd.nb.numnodes <- glmer.nb(NodeRank ~ NumNodes + (1|Demo.ResponseID), 
#                                 data=graphics_clickhighdeg, verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(NumNodes=scale(NumNodes))

graph.clickhd.nb <- glmer.nb(NodeRank ~ NumNodes + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb, . ~ . - NumNodes)

anova(graph.clickhd.nb, graph.clickhd.nb.null.2)

# NumNodes is barely not significant (p=0.07374)

#graph.clickhd.nb <- glmer.nb(NodeRank ~ NumNodesClust1 + (1|Demo.ResponseID), 
#                        data=graphics_clickhighdeg, verbose=TRUE)

# need to rescale

graphics_clickhighdeg.CS <- graphics_clickhighdeg %>% mutate(NumNodesClust1=scale(NumNodesClust1))

graph.clickhd.nb.numnodesclust1 <- glmer.nb(NodeRank ~ NumNodesClust1 + (1|Demo.ResponseID), 
                        data=graphics_clickhighdeg.CS, verbose=TRUE)

summary(graph.clickhd.nb.numnodesclust1)

graph.clickhd.nb.null.2 <- update(graph.clickhd.nb.numnodesclust1, . ~ . - NumNodesClust1)

anova(graph.clickhd.nb.numnodesclust1, graph.clickhd.nb.null.2)

# NumNodesClust1 is significant (p=0.008985)
```

``` r
temp <- graphics_clickhighdeg %>% dplyr::select(Demo.ResponseID, NodeRank, Dataset, CorrectAnswer, Demo.acfieldGrouped3, Demo.dailytech_SmartPhone, AvgDeg, NumClust, NumHighDegree, NumNodesClust1) %>% drop_na()

#temp <- temp %>% mutate(Stats.Q_TotalDuration=scale(Stats.Q_TotalDuration))
```

Run this one

``` r
#---------------------------------
# Multiple predictors
#---------------------------------

graph.clickhd.nb.full <- glmer.nb(NodeRank ~ Dataset +
                                    Demo.acfieldGrouped3 + Demo.dailytech_SmartPhone +
                                    (1|Demo.ResponseID),
                                  data=temp, verbose=TRUE,
                                  control=glmerControl(optimizer = "bobyqa"))

# not every dataset is significant; should i be combining? TO DO

summary(graph.clickhd.nb.full)
# still significant - Dataset, Demo.acfieldGrouped3, Demo.dailytech_SmartPhone
```

``` r
#---------------------------------
# Interactions
#---------------------------------

graph.clickhd.nb.full.int <- glmer.nb(NodeRank ~ Dataset +
                                        Demo.acfieldGrouped3 + Demo.dailytech_SmartPhone +
                                        Demo.acfieldGrouped3:Demo.dailytech_SmartPhone +
                                        (1|Demo.ResponseID),
                                      data=temp, verbose=TRUE,
                                      control=glmerControl(optimizer = "bobyqa"))

summary(graph.clickhd.nb.full.int)

anova(graph.clickhd.nb.full, graph.clickhd.nb.full.int)

# no significant interactions
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.clickhd.nb.full, file = file.path(analysisDataDir,"fits/graph_clickhd_nb_full.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_clickhd_nb_full.RData"))
```

``` r
graph.clickhd.nb.full.f <- fortify(graph.clickhd.nb.full)

ggplot(graph.clickhd.nb.full.f, aes(.fitted,.resid)) + 
  geom_point() +
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-1.png)<!-- -->

``` r
ggplot(graph.clickhd.nb.full.f, aes(.resid)) +
  geom_histogram()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-2.png)<!-- -->

``` r
# not *quite* normally distributed...  but not too skewed?

ggplot(graph.clickhd.nb.full.f, aes(.fitted,NodeRank)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-3.png)<!-- -->

``` r
ggplot(graph.clickhd.nb.full.f, aes(.fitted,NodeRank, color=Dataset)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-4.png)<!-- -->

``` r
ggplot(graph.clickhd.nb.full.f, aes(.fitted,NodeRank)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  scale_y_log10() +
  facet_wrap(~Dataset)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-5.png)<!-- -->

``` r
#(r2nsj = r2beta(graph.clickhd.nb.full, method = 'nsj', partial = TRUE))[1,'Rsq']

ggplot(graphics_clickhighdeg) + geom_histogram(aes(NodeRank), binwidth=1) +
labs(title="Distribution of NodeRank values for\nHighest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-6.png)<!-- -->

``` r
ggplot(graph.clickhd.nb.full.f, aes(NodeRank,.fitted)) +
geom_bin2d() +
geom_abline(aes(slope = 1, intercept = 0)) +
#scale_y_continuous(limits=c(0,0.3)) +
labs(title="Real vs. Predicted NodeRank values for\nHighest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-100-7.png)<!-- -->

``` r
# is negative binomial different from poisson?  if so, indicates over-dispersion is true
# and negative binomial is necessary

graph.clickhd.pois <- glmer(NodeRank ~ Dataset + Demo.acfieldGrouped3 + Demo.dailytech_SmartPhone +
                                   (1|Demo.ResponseID), data=temp, family="poisson")

pchisq(2 * (logLik(graph.clickhd.nb.full) - logLik(graph.clickhd.pois)), df=1, lower.tail = FALSE)

# value = 2.926157e-08, so keep the negative binomial
```

``` r
# run without mixed effects to validate

m.glm <- glm.nb(NodeRank ~ Dataset + Demo.acfieldGrouped3 + Demo.dailytech_SmartPhone, 
                data=temp, trace=TRUE)
summary(m.glm)

## The neg.binomial theta parameter:
getME(graph.clickhd.nb.full, "glmer.nb.theta")

#11.58727

## mixed model has 1 additional parameter (RE variance)
stopifnot(attr(logLik(graph.clickhd.nb.full),"df")==attr(logLik(m.glm),"df")+1) # not sure what this does

anova(graph.clickhd.nb.full,m.glm) # can I use anova to compare mixed and fixed effects?
# p < 5.213e-09, so definitely random effects

plot(graph.clickhd.nb.full, resid(.) ~ NodeRank)# works, as long as data 'dd' is found


# TO DO : check if this is all right
```

``` r
par(mfrow=c(2,2))
qqnorm(resid(graph.clickhd.nb.full), main="normal qq-plot, residuals")
qqline(resid(graph.clickhd.nb.full))

qqnorm(ranef(graph.clickhd.nb.full)$Demo.ResponseID[,1])
qqline(ranef(graph.clickhd.nb.full)$Demo.ResponseID[,1])


plot(fitted(graph.clickhd.nb.full), resid(graph.clickhd.nb.full)) #residuals vs fitted
abline(h=0)

#graph.avgdeg.nb2.f <- fortify(graph.avgdeg.nb2)

#ggplot(graph.avgdeg.nb2.f, aes(.fitted,.resid)) + 
#  geom_point() +
#  geom_hline(yintercept=0)

#temp <- graphics_avgdeg

temp$fitted <- fitted(graph.clickhd.nb.full) 
plot(temp$fitted, jitter(temp$NodeRank,0.1)) #fitted vs observed
abline(0,1)

#ggplot(graph.avgdeg.nb2.f, aes(.fitted,Response)) + 
#  geom_point() +
#  geom_abline(aes(slope = 1, intercept = 0))
```

``` r
# Confidence Intervals, using coefficients

(est <- cbind(Estimate = coef(graph.avgdeg.nb.full.int), confint(graph.avgdeg.nb.full.int)))

# exponentiate model to look at incident rate ratios instead of coefficients

exp(est)
```

``` r
# predictions

# model: Response ~ Dataset + TaskOrder + Demo.dailytech_SmartPhone + Dataset:TaskOrder + TaskOrder:Demo.dailytech_SmartPhone + Dataset:Demo.dailytech_SmartPhone + (1 | Demo.ResponseID)

newdata1 <- data.frame(Demo.dailytech_SmartPhone = rep(mean(temp$Demo.dailytech_SmartPhone),54), 
                       Dataset = factor(rep(c(1,3,5,7,8,9),9), levels = levels(temp$Dataset),ordered = TRUE),
                       TaskOrder = rep(1:9,6),
                       Demo.ResponseID = sample(temp$Demo.ResponseID,54))
newdata1$phat <- predict(graph.avgdeg.nb.full.int, newdata1, type = "response")
#newdata1

newdata2 <- data.frame(
  Demo.dailytech_SmartPhone = rep(seq(from = min(temp$Demo.dailytech_SmartPhone), to = max(temp$Demo.dailytech_SmartPhone), length.out = 100), 6),
  Dataset = factor(rep(c(1,3,5,7,8,9), each = 100), levels = levels(temp$Dataset),ordered = TRUE),
  TaskOrder = rep(1:9,len=600),
  Demo.ResponseID = sample(temp$Demo.ResponseID,600)
  )

#predict(graph.avgdeg.nb.full.int, newdata2, type = "link", se.fit=TRUE)
newdata2 <- cbind(newdata2, predict(graph.avgdeg.nb.full.int, newdata2, type = "link", se.fit=TRUE))

# not sure about this; asking for "fit", but that's not recognized. should the new column be called "fit"? what about se.fit?

#newdata2 <- within(newdata2, {
#  Response <- exp(fit)
#  LL <- exp(fit - 1.96 * se.fit)
#  UL <- exp(fit + 1.96 * se.fit)
#})

#ggplot(newdata2, aes(math, DaysAbsent)) +
#  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
#  geom_line(aes(colour = prog), size = 2) +
#  labs(x = "Math Score", y = "Predicted Days Absent")

# note on negative binomial:

# TO DO : If the data generating process does not allow for any 0s (such as the number of days spent in the hospital), then a zero-truncated model may be more appropriate.

# TO DO : Count data often have an exposure variable, which indicates the number of times the event could have happened (i.e. a max). This variable should be incorporated into your negative binomial regression model with the use of the offset option. See the glm documentation for details.  (so, that would make sense for click data???)

# other info:
# Cameron, A. C. and Trivedi, P. K. 1998. Regression Analysis of Count Data. New York: Cambridge Press.
# Dupont, W. D. 2002. Statistical Modeling for Biomedical Researchers: A Simple Introduction to the Analysis of Complex Data. New York: Cambridge Press.
```

#### Least Squares

Full model (graph.clickhd.nb.full): NodeRank ~ Dataset +
Demo.acfieldGrouped3 + Demo.dailytech\_SmartPhone + (1|Demo.ResponseID)

##### Dataset

###### broom

``` r
graph.clickhd.broom.tidy <- tidy(graph.clickhd.nb.full, conf.int=TRUE)
graph.clickhd.broom.aug <- augment(graph.clickhd.nb.full, temp)
#graph.clickhd.broom.ci <- confint_tidy(graph.clickhd.nb.full)
#graph.clickhd.broom.tidyci <- bind_cols(graph.clickhd.broom.tidy %>% arrange(group), graph.clickhd.broom.ci)

#graph.clickhd.broom.tidyci$term <- factor(graph.clickhd.broom.tidyci$term,
#                                          levels=graph.clickhd.broom.tidyci %>% arrange(estimate) %>%
#                                            dplyr::select(term) %>% unlist())

#ggplot(graph.clickhd.broom.tidyci, aes(term, estimate)) + 
#  geom_point() + 
#  coord_flip()

graph.clickhd.broom.tidy$term <- factor(graph.clickhd.broom.tidy$term,
                                          levels=graph.clickhd.broom.tidy %>% arrange(estimate) %>%
                                            dplyr::select(term) %>% unlist())

ggplot(graph.clickhd.broom.tidy, aes(term, estimate)) + 
  geom_point() + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-106-1.png)<!-- -->

``` r
ggplot(graph.clickhd.broom.aug %>% group_by(Dataset) %>% summarize(est=mean(.fitted))) +
  geom_point(aes(Dataset,est)) +
  scale_y_continuous(limits = c(-.06,1.2), breaks=c(0,.3,.6,.9,1.2)) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-106-2.png)<!-- -->

``` r
plot(emmeans(graph.clickhd.nb.full, "Dataset"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-106-3.png)<!-- -->

``` r
ggplot(graph.clickhd.broom.aug) +
  geom_violin(aes(Dataset,.fitted)) +
  scale_y_continuous(limits = c(-.06,1.2), breaks=c(0,.3,.6,.9,1.2)) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-106-4.png)<!-- -->

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.clickhd.emm.data <- emmeans(graph.clickhd.nb.full, "Dataset", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.clickhd.emm.data
```

    ##  Dataset    emmean         SE  df   asymp.LCL asymp.UCL
    ##  1       0.7422954 0.05866838 Inf  0.62730747 0.8572833
    ##  3       1.0216216 0.05489035 Inf  0.91403844 1.1292047
    ##  5       0.1698092 0.07395114 Inf  0.02486762 0.3147508
    ##  7       0.6631614 0.06303405 Inf  0.53961695 0.7867059
    ##  8       0.1231543 0.07674066 Inf -0.02725462 0.2735632
    ##  9       1.0592641 0.05492148 Inf  0.95162002 1.1669083
    ## 
    ## Results are averaged over the levels of: Demo.acfieldGrouped3 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95

``` r
graph.clickhd.emm.data.df <- dplyr::as_data_frame(graph.clickhd.emm.data)

graph.clickhd.emm.data.df
```

    ## # A tibble: 6 x 6
    ##   Dataset emmean     SE    df asymp.LCL asymp.UCL
    ## * <fct>    <dbl>  <dbl> <dbl>     <dbl>     <dbl>
    ## 1 1        0.742 0.0587   Inf    0.627      0.857
    ## 2 3        1.02  0.0549   Inf    0.914      1.13 
    ## 3 5        0.170 0.0740   Inf    0.0249     0.315
    ## 4 7        0.663 0.0630   Inf    0.540      0.787
    ## 5 8        0.123 0.0767   Inf   -0.0273     0.274
    ## 6 9        1.06  0.0549   Inf    0.952      1.17

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.clickhd.emm.data.cld <- cld(graph.clickhd.emm.data,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.clickhd.emm.data.cld.df <- graph.clickhd.emm.data.cld$emmeans

graph.clickhd.emm.data.cld.df %>% dplyr::select(Dataset,.group)
```

    ##  Dataset .group
    ##  8        1    
    ##  5        1    
    ##  7         2   
    ##  1         2   
    ##  3          3  
    ##  9          3  
    ## 
    ## Results are averaged over the levels of: Demo.acfieldGrouped3 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.clickhd.emm.data.cld.df
```

    ##  Dataset    emmean         SE  df   asymp.LCL asymp.UCL .group
    ##  8       0.1231543 0.07674066 Inf -0.02725462 0.2735632  1    
    ##  5       0.1698092 0.07395114 Inf  0.02486762 0.3147508  1    
    ##  7       0.6631614 0.06303405 Inf  0.53961695 0.7867059   2   
    ##  1       0.7422954 0.05866838 Inf  0.62730747 0.8572833   2   
    ##  3       1.0216216 0.05489035 Inf  0.91403844 1.1292047    3  
    ##  9       1.0592641 0.05492148 Inf  0.95162002 1.1669083    3  
    ## 
    ## Results are averaged over the levels of: Demo.acfieldGrouped3 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.clickhd.emm.data.cld.df$Dataset <- factor(graph.clickhd.emm.data.cld.df$Dataset, levels=graph.clickhd.emm.data.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Dataset) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.clickhd.emm.data, ~Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-107-1.png)<!-- -->

``` r
plot(graph.clickhd.emm.data)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-107-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.clickhd.emm.data.cld.df %>% arrange(desc(emmean))
```

    ##   Dataset    emmean         SE  df   asymp.LCL asymp.UCL .group
    ## 1       9 1.0592641 0.05492148 Inf  0.95162002 1.1669083      3
    ## 2       3 1.0216216 0.05489035 Inf  0.91403844 1.1292047      3
    ## 3       1 0.7422954 0.05866838 Inf  0.62730747 0.8572833     2 
    ## 4       7 0.6631614 0.06303405 Inf  0.53961695 0.7867059     2 
    ## 5       5 0.1698092 0.07395114 Inf  0.02486762 0.3147508    1  
    ## 6       8 0.1231543 0.07674066 Inf -0.02725462 0.2735632    1

``` r
ggplot(graph.clickhd.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-107-3.png)<!-- -->

``` r
ggplot(graph.clickhd.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Dataset for\nHighest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-107-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.clickhd.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.clickhd.emm.data)) 
graph.clickhd.emm.data.pairs
```

    ## # A tibble: 15 x 6
    ##    contrast estimate     SE    df z.ratio            p.value
    ##  * <fct>       <dbl>  <dbl> <dbl>   <dbl>              <dbl>
    ##  1 1 - 3     -0.279  0.0749   Inf - 3.73  0.00264           
    ##  2 1 - 5      0.572  0.0895   Inf   6.39  0.00000000242     
    ##  3 1 - 7      0.0791 0.0804   Inf   0.985 0.923             
    ##  4 1 - 8      0.619  0.0921   Inf   6.72  0.000000000264    
    ##  5 1 - 9     -0.317  0.0742   Inf - 4.27  0.000279          
    ##  6 3 - 5      0.852  0.0868   Inf   9.82  0.0000000000000614
    ##  7 3 - 7      0.358  0.0781   Inf   4.59  0.0000652         
    ##  8 3 - 8      0.898  0.0894   Inf  10.1   0.0000000000000682
    ##  9 3 - 9     -0.0376 0.0715   Inf - 0.527 0.995             
    ## 10 5 - 7     -0.493  0.0924   Inf - 5.34  0.00000140        
    ## 11 5 - 8      0.0467 0.102    Inf   0.457 0.998             
    ## 12 5 - 9     -0.889  0.0869   Inf -10.2   0.0000000000000505
    ## 13 7 - 8      0.540  0.0947   Inf   5.70  0.000000177       
    ## 14 7 - 9     -0.396  0.0777   Inf - 5.10  0.00000509        
    ## 15 8 - 9     -0.936  0.0893   Inf -10.5   0.0000000000000528

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.clickhd.emm.data))

graph.clickhd.emm.data.pairs <- full_join(graph.clickhd.emm.data.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.clickhd.emm.data))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-108-1.png)<!-- -->

``` r
plot(pairs(graph.clickhd.emm.data), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-108-2.png)<!-- -->

``` r
graph.clickhd.emm.data.pairs$sig.levels <- 
  case_when(graph.clickhd.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.clickhd.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.clickhd.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.clickhd.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.clickhd.emm.data.pairs$sig.levels <- factor(graph.clickhd.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.clickhd.emm.data.pairs$contrast <- factor(graph.clickhd.emm.data.pairs$contrast, levels=graph.clickhd.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.clickhd.emm.data.pairs <- graph.clickhd.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.clickhd.emm.data.pairs %>% arrange(estimate)
```

    ## # A tibble: 15 x 11
    ##    contrast From  To    estimate     SE    df z.ratio    p.value asymp.LCL
    ##    <fct>    <chr> <chr>    <dbl>  <dbl> <dbl>   <dbl>      <dbl>     <dbl>
    ##  1 8 - 9    8     9      -0.936  0.0893   Inf -10.5     5.28e⁻¹⁴    -1.19 
    ##  2 5 - 9    5     9      -0.889  0.0869   Inf -10.2     5.05e⁻¹⁴    -1.14 
    ##  3 5 - 7    5     7      -0.493  0.0924   Inf - 5.34    1.40e⁻ ⁶    -0.757
    ##  4 7 - 9    7     9      -0.396  0.0777   Inf - 5.10    5.09e⁻ ⁶    -0.618
    ##  5 1 - 9    1     9      -0.317  0.0742   Inf - 4.27    2.79e⁻ ⁴    -0.528
    ##  6 1 - 3    1     3      -0.279  0.0749   Inf - 3.73    2.64e⁻ ³    -0.493
    ##  7 3 - 9    3     9      -0.0376 0.0715   Inf - 0.527   9.95e⁻ ¹    -0.241
    ##  8 5 - 8    5     8       0.0467 0.102    Inf   0.457   9.98e⁻ ¹    -0.244
    ##  9 1 - 7    1     7       0.0791 0.0804   Inf   0.985   9.23e⁻ ¹    -0.150
    ## 10 3 - 7    3     7       0.358  0.0781   Inf   4.59    6.52e⁻ ⁵     0.136
    ## 11 7 - 8    7     8       0.540  0.0947   Inf   5.70    1.77e⁻ ⁷     0.270
    ## 12 1 - 5    1     5       0.572  0.0895   Inf   6.39    2.42e⁻ ⁹     0.317
    ## 13 1 - 8    1     8       0.619  0.0921   Inf   6.72    2.64e⁻¹⁰     0.357
    ## 14 3 - 5    3     5       0.852  0.0868   Inf   9.82    6.14e⁻¹⁴     0.605
    ## 15 3 - 8    3     8       0.898  0.0894   Inf  10.1     6.82e⁻¹⁴     0.644
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.clickhd.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-108-3.png)<!-- -->

``` r
ggplot(graph.clickhd.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-108-4.png)<!-- -->

``` r
#cond.lev <- c("Ctrl","Phr","Col","Siz")


copy <- graph.clickhd.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         z.ratio=-z.ratio,
         asymp.LCL=-asymp.LCL,
         asymp.UCL=-asymp.UCL)

graph.clickhd.emm.data.pairs.compl <- bind_rows(graph.clickhd.emm.data.pairs, copy)

copy$From <- factor(copy$From, levels=rev(unique(copy$From)))
#graph.bc.emm.data.pairs$To <- factor(graph.bc.emm.data.pairs$To, levels=cond.lev)
#graph.bc.emm.data.pairs$From <- factor(graph.bc.emm.data.pairs$From, levels=rev(cond.lev))

highest<-max(abs(copy$estimate))

ggplot(copy, aes(x=To,y=From)) +
  geom_tile(aes(fill=estimate)) +
  geom_text(aes(label=paste0(format(estimate, digits=2, nsmall=2),"\n(",sig.levels,")"))) +
    scale_fill_distiller(type="div", palette=4, limits=c(-highest,highest)) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-109-1.png)<!-- -->

``` r
graph.clickhd.emm.data.pairs.compl$From <- factor(graph.clickhd.emm.data.pairs.compl$From, levels=rev(unique(graph.clickhd.emm.data.pairs.compl$From)))
#graph.bc.emm.data.pairs.compl$To <- factor(graph.bc.emm.data.pairs.compl$To, levels=cond.lev)
#graph.bc.emm.data.pairs.compl$From <- factor(graph.bc.emm.data.pairs.compl$From, levels=rev(cond.lev))


#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.clickhd.emm.data.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 30 x 11
    ##    contrast From  To    estimate     SE    df z.ratio    p.value asymp.LCL
    ##    <fct>    <fct> <chr>    <dbl>  <dbl> <dbl>   <dbl>      <dbl>     <dbl>
    ##  1 8 - 9    8     9       -0.936 0.0893   Inf  -10.5    5.28e⁻¹⁴    -1.19 
    ##  2 3 - 8    8     3       -0.898 0.0894   Inf  -10.1    6.82e⁻¹⁴    -0.644
    ##  3 5 - 9    5     9       -0.889 0.0869   Inf  -10.2    5.05e⁻¹⁴    -1.14 
    ##  4 3 - 5    5     3       -0.852 0.0868   Inf  - 9.82   6.14e⁻¹⁴    -0.605
    ##  5 1 - 8    8     1       -0.619 0.0921   Inf  - 6.72   2.64e⁻¹⁰    -0.357
    ##  6 1 - 5    5     1       -0.572 0.0895   Inf  - 6.39   2.42e⁻ ⁹    -0.317
    ##  7 7 - 8    8     7       -0.540 0.0947   Inf  - 5.70   1.77e⁻ ⁷    -0.270
    ##  8 5 - 7    5     7       -0.493 0.0924   Inf  - 5.34   1.40e⁻ ⁶    -0.757
    ##  9 7 - 9    7     9       -0.396 0.0777   Inf  - 5.10   5.09e⁻ ⁶    -0.618
    ## 10 3 - 7    7     3       -0.358 0.0781   Inf  - 4.59   6.52e⁻ ⁵    -0.136
    ## # ... with 20 more rows, and 2 more variables: asymp.UCL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.clickhd.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-110-1.png)<!-- -->

``` r
ggplot(graph.clickhd.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-110-2.png)<!-- -->

``` r
ggplot(graph.clickhd.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-110-3.png)<!-- -->

``` r
ggplot(graph.clickhd.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-110-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.clickhd.emm.data.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*10
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.clickhd.emm.data.pairs %>% dplyr::select(-contrast) %>% filter(sig.levels != "NS"), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  #scale_edge_color_manual("Significance Levels", values=sig.colors) +
  #scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Demo.acfieldGrouped3

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.bc.emm.acfield3 <- emmeans(graph.clickhd.nb.full, "Demo.acfieldGrouped3", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.bc.emm.acfield3
```

    ##  Demo.acfieldGrouped3    emmean         SE  df asymp.LCL asymp.UCL
    ##  SignificantGroup     0.5589462 0.03462688 Inf 0.4910787 0.6268136
    ##  ZZ-Etc.              0.7008225 0.05347614 Inf 0.5960112 0.8056338
    ## 
    ## Results are averaged over the levels of: Dataset 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95

``` r
graph.bc.emm.acfield3.df <- dplyr::as_data_frame(graph.bc.emm.acfield3)

graph.bc.emm.acfield3.df
```

    ## # A tibble: 2 x 6
    ##   Demo.acfieldGrouped3 emmean     SE    df asymp.LCL asymp.UCL
    ## * <fct>                 <dbl>  <dbl> <dbl>     <dbl>     <dbl>
    ## 1 SignificantGroup      0.559 0.0346   Inf     0.491     0.627
    ## 2 ZZ-Etc.               0.701 0.0535   Inf     0.596     0.806

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.bc.emm.acfield3.cld <- cld(graph.bc.emm.acfield3,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.bc.emm.acfield3.cld.df <- graph.bc.emm.acfield3.cld$emmeans

graph.bc.emm.acfield3.cld.df %>% dplyr::select(Demo.acfieldGrouped3,.group)
```

    ##  Demo.acfieldGrouped3 .group
    ##  SignificantGroup      1    
    ##  ZZ-Etc.                2   
    ## 
    ## Results are averaged over the levels of: Dataset 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.acfield3.cld.df
```

    ##  Demo.acfieldGrouped3    emmean         SE  df asymp.LCL asymp.UCL .group
    ##  SignificantGroup     0.5589462 0.03462688 Inf 0.4910787 0.6268136  1    
    ##  ZZ-Etc.              0.7008225 0.05347614 Inf 0.5960112 0.8056338   2   
    ## 
    ## Results are averaged over the levels of: Dataset 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.bc.emm.acfield3.cld.df$Demo.acfieldGrouped3 <- factor(graph.bc.emm.acfield3.cld.df$Demo.acfieldGrouped3, levels=graph.bc.emm.acfield3.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Demo.acfieldGrouped3) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.bc.emm.acfield3, ~Demo.acfieldGrouped3, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-113-1.png)<!-- -->

``` r
plot(graph.bc.emm.acfield3)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-113-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.bc.emm.acfield3.cld.df %>% arrange(desc(emmean))
```

    ##   Demo.acfieldGrouped3    emmean         SE  df asymp.LCL asymp.UCL .group
    ## 1              ZZ-Etc. 0.7008225 0.05347614 Inf 0.5960112 0.8056338      2
    ## 2     SignificantGroup 0.5589462 0.03462688 Inf 0.4910787 0.6268136     1

``` r
ggplot(graph.bc.emm.acfield3.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.acfieldGrouped3,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Demo.acfieldGrouped3,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-113-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield3.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.acfieldGrouped3,ymax=asymp.UCL,ymin=asymp.LCL), width=.2) +
  geom_point(aes(x=Demo.acfieldGrouped3,y=emmean), size=7) +
  scale_x_discrete(labels=c("High Error Group","Low Error Group"),name="Academic Field") +
  coord_flip() +
labs(title="Estimated Marginal Means for Academic Field for\nHighest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-113-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.bc.emm.acfield3.pairs <- dplyr::as_data_frame(pairs(graph.bc.emm.acfield3)) 
graph.bc.emm.acfield3.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast                   estimate     SE    df z.ratio p.value
    ## * <fct>                         <dbl>  <dbl> <dbl>   <dbl>   <dbl>
    ## 1 SignificantGroup - ZZ-Etc.   -0.142 0.0619   Inf   -2.29  0.0220

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.bc.emm.acfield3))

graph.bc.emm.acfield3.pairs <- full_join(graph.bc.emm.acfield3.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.bc.emm.acfield3))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-114-1.png)<!-- -->

``` r
#plot(pairs(graph.bc.emm.acfield2), comparisons = TRUE)

graph.bc.emm.acfield3.pairs$sig.levels <- 
  case_when(graph.bc.emm.acfield3.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.bc.emm.acfield3.pairs$p.value < .001 ~ sig.level.names[2],
            graph.bc.emm.acfield3.pairs$p.value < .01 ~ sig.level.names[3],
            graph.bc.emm.acfield3.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.bc.emm.acfield3.pairs$sig.levels <- factor(graph.bc.emm.acfield3.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.bc.emm.acfield3.pairs$contrast <- factor(graph.bc.emm.acfield3.pairs$contrast, levels=graph.bc.emm.acfield3.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.bc.emm.acfield3.pairs <- graph.bc.emm.acfield3.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.bc.emm.acfield3.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast    From   To    estimate     SE    df z.ratio p.value asymp.LCL
    ##   <fct>       <chr>  <chr>    <dbl>  <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    ## 1 Significan… Signi… ZZ-E…   -0.142 0.0619   Inf   -2.29  0.0220    -0.263
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.acfield3.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-114-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield3.pairs) +
  geom_errorbar(aes(x=contrast,ymax=asymp.UCL,ymin=asymp.LCL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-114-3.png)<!-- -->

``` r
#cond.lev <- c("Ctrl","Phr","Col","Siz")


copy <- graph.bc.emm.acfield3.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         z.ratio=-z.ratio,
         asymp.LCL=-asymp.LCL,
         asymp.UCL=-asymp.UCL)

graph.bc.emm.acfield3.pairs.compl <- bind_rows(graph.bc.emm.acfield3.pairs, copy)

copy$From <- factor(copy$From, levels=rev(unique(copy$From)))
#graph.bc.emm.data.pairs$To <- factor(graph.bc.emm.data.pairs$To, levels=cond.lev)
#graph.bc.emm.data.pairs$From <- factor(graph.bc.emm.data.pairs$From, levels=rev(cond.lev))

highest<-max(abs(copy$estimate))

ggplot(copy, aes(x=To,y=From)) +
  geom_tile(aes(fill=estimate)) +
  geom_text(aes(label=paste0(format(estimate, digits=2, nsmall=2),"\n(",sig.levels,")"))) +
    scale_fill_distiller(type="div", palette=4, limits=c(-highest,highest)) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-115-1.png)<!-- -->

``` r
graph.bc.emm.acfield3.pairs.compl$From <- factor(graph.bc.emm.acfield3.pairs.compl$From, levels=rev(unique(graph.bc.emm.acfield3.pairs.compl$From)))
#graph.bc.emm.data.pairs.compl$To <- factor(graph.bc.emm.data.pairs.compl$To, levels=cond.lev)
#graph.bc.emm.data.pairs.compl$From <- factor(graph.bc.emm.data.pairs.compl$From, levels=rev(cond.lev))


#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.bc.emm.acfield3.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast   From   To     estimate     SE    df z.ratio p.value asymp.LCL
    ##   <fct>      <fct>  <chr>     <dbl>  <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    ## 1 Significa… Signi… ZZ-Et…   -0.142 0.0619   Inf   -2.29  0.0220    -0.263
    ## 2 Significa… ZZ-Et… Signi…    0.142 0.0619   Inf    2.29  0.0220     0.263
    ## # ... with 2 more variables: asymp.UCL <dbl>, sig.levels <ord>

``` r
ggplot(graph.bc.emm.acfield3.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-116-1.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield3.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-116-2.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield3.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-116-3.png)<!-- -->

``` r
ggplot(graph.bc.emm.acfield3.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-116-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.bc.emm.acfield3.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*10
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.bc.emm.acfield3.pairs %>% dplyr::select(-contrast) %>% filter(sig.levels != "NS"), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  #scale_edge_color_manual("Significance Levels", values=sig.colors) +
  #scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Demo.dailytech\_SmartPhone

``` r
ggplot(graphics_clickhighdeg, aes(Demo.dailytech_SmartPhone, NodeRank)) +
  geom_bin2d() +
  geom_smooth(method="lm") +
  labs(title="Smart Phone Usage vs. NodeRank for\nHighest Degree Node task, graphics conditions",
x="Average Daily Time Using Smart Phone (hours)")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-119-1.png)<!-- -->

### Percentage of Nodes in Largest Cluster

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_lgclust <- readRDS(file.path(analysisDataDir, "GraphicsLgClust.rds"))
```

#### Beta regression for percentage

See also:
<http://rcompanion.org/handbook/J_02.html>

``` r
#graphics_lgclust.2 <- graphics_lgclust %>% mutate(ResponsePct = (Response-.001)/100)
graphics_lgclust.2 <- graphics_lgclust %>% mutate(ResponseInfl = Response/100,
                                                  ResponsePct = (Response+0.00000001)/100.01)
# should give a min of .00001 and a max of .99999
```

``` r
temp <- graphics_lgclust %>% dplyr::select(Demo.ResponseID, Response, Condition, Dataset, DatasetOrder, DatasetStartTime, TaskOrder, CorrectAnswer, Underestimated, Stats.Q_TotalDuration, Stats.OperatingSystem, Demo.age, Demo.gender, Demo.lang, Demo.educ, Demo.acfield, Demo.dailytech_Computer, Demo.dailytech_Tablet, Demo.dailytech_SmartPhone, Demo.weeklygaming, Demo.expdataanal, Demo.expdatavis, Demo.expreadnetvis, Demo.expcreatenetvis, StatsNumPixels, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumHighDegree, NumLinks, NumNodes, NumNodesClust1, ConditionPhrasing, ConditionGraphics, ConditionColor, Ctrl_dummy, Demo.expreadnetvis.alot, Demo.expcreatenetvis.alot, Stats.OperatingSystemCombined, Stats.OperatingSystemCombined2, Stats.OperatingSystemCombined3, Stats.OperatingSystemCombined4, Stats.OperatingSystemCombined5, Stats.OperatingSystemWindows, Stats.OperatingSystemMacintosh, Stats.OperatingSystemAndroid, Stats.OperatingSystemiPhone, Stats.OperatingSystemNumClust, Demo.acfieldGrouped, Demo.acfieldGrouped2, Demo.acfieldGrouped3, Overestimated, UnderestDummy, DatasetDuration) %>% drop_na() %>% mutate(ResponsePct = Response/100, StatsNumPixels=scale(StatsNumPixels))
```

##### brms/tidybayes

``` r
#library(rstan)

theme_set(theme_grey())

#fit <- brm(ResponseInfl~Dataset, data = graphics_lgclust.2, family = zero_one_inflated_beta())
#fit <- brm(bf(ResponseInfl ~ Dataset + UnderestDummy + Demo.gender + 
#    Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows + 
#    Dataset:UnderestDummy + Demo.gender:Stats.OperatingSystemWindows + 
#    UnderestDummy:Demo.gender + UnderestDummy:Stats.OperatingSystemWindows, 
#    sigma ~ Dataset + UnderestDummy), data = graphics_lgclust.2, family = zero_one_inflated_beta())

#compose_data(graphics_lgclust.2)

# THIS WAS RUN
#fit <- brm(bf(ResponseInfl ~ Dataset + UnderestDummy + Demo.gender + 
#    Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows + 
#    Dataset:UnderestDummy + Demo.gender:Stats.OperatingSystemWindows + 
#    UnderestDummy:Demo.gender + UnderestDummy:Stats.OperatingSystemWindows), 
#    data = graphics_lgclust.2, family = zero_one_inflated_beta())

#fit2 <- update(fit, formula = . ~ . + (1 | Demo.ResponseID), newdata=graphics_lgclust.2)

LOO(fit, fit2)

# fit2 has lower LOOIC (1646.85 < 1620.62), which is good

fit2$formula

#fixed_form <- brms:::extract_effects(fit$formula)$fixed
#mm <- brms:::get_model_matrix(fixed_form, data = fit$data) 

coef(fit2)
fixef(fit2)
fixef(fit2, robust=TRUE)
fitted(fit2)
tidy(fit2)


fit2 %<>% recover_types(graphics_lgclust.2)

#print(fit2, pars = c("b_Intercept"))

#str(extract(fit2))

summary(fit2, waic = TRUE) 
```

``` r
#plot(fit, ask=FALSE) 
#plot(marginal_effects(fit, probs = c(0.05, 0.95)),points=TRUE, ask=FALSE)
#plot(marginal_effects(fit, probs = c(0.05, 0.95)),ask=FALSE)
plot(marginal_effects(fit, effects="Dataset", probs = c(0.05, 0.95)),ask=FALSE)
```

``` r
#fit.df <- tidy(fit2)

# diff between each dataset and dataset 1
hyp.vs.1 <- c("Dataset.L = 0", 
         "Dataset.Q = 0",
         "Dataset.C = 0",
         "DatasetE4 = 0",
         "DatasetE5 = 0")

hyp.vs.1.res <- hypothesis(fit2, hyp.vs.1)

# 1 = Intercept
# 3 = Dataset.L + Intercept
# 5 = Dataset.Q + Intercept
# 7 = Dataset.C + Intercept
# 8 = DatasetE4 + Intercept
# 9 = DatasetE5 + Intercept

# diff between each dataset and dataset 3
hyp.vs.3 <- c("Dataset.Q > Dataset.L", # 5 vs. 3
         "Dataset.C > Dataset.L",
         "DatasetE4 > Dataset.L",
         "DatasetE5 > Dataset.L")

hyp.vs.3.res <- hypothesis(fit2, hyp.vs.3)

# diff between each dataset and dataset 5
hyp.vs.5 <- c("Dataset.C > Dataset.Q", # 7 vs. 5
         "DatasetE4 > Dataset.Q",
         "DatasetE5 > Dataset.Q")

hyp.vs.5.res <- hypothesis(fit2, hyp.vs.5)

# diff between each dataset and dataset 7
hyp.vs.7 <- c("DatasetE4 > Dataset.C", # 8 vs. 7
         "DatasetE5 > Dataset.C")

hyp.vs.7.res <- hypothesis(fit2, hyp.vs.7)

# diff between each dataset and dataset 8
hyp.vs.8 <- c("DatasetE5 > DatasetE4" # 9 vs. 8
         )

hyp.vs.8.res <- hypothesis(fit2, hyp.vs.8)

# sequential differences
hyp.seq <- c("Dataset.L = 0",
             "Dataset.L + Dataset.Q > Dataset.L",
             "Dataset.Q + Dataset.C > Dataset.Q",
             "Dataset.C + DatasetE4 > Dataset.C",
             "DatasetE4 + DatasetE5 > DatasetE4")

(hyp.seq.res <- hypothesis(fit2, hyp.seq))
```

``` r
# all pairs

hyp.pairs <- c("Dataset.L = 0", # 3 vs. 1
         "Dataset.Q = 0",
         "Dataset.C = 0",
         "DatasetE4 = 0",
         "DatasetE5 = 0",
         "Dataset.Q > Dataset.L", # 5 vs. 3
         "Dataset.C > Dataset.L",
         "DatasetE4 > Dataset.L",
         "DatasetE5 > Dataset.L",
         "Dataset.C > Dataset.Q", # 7 vs. 5
         "DatasetE4 > Dataset.Q",
         "DatasetE5 > Dataset.Q",
         "DatasetE4 > Dataset.C", # 8 vs. 7
         "DatasetE5 > Dataset.C",
         "DatasetE5 > DatasetE4")

# above syntax converts to Dataset.Q - Dataset.L > 0, which then calculates evidence ratio, which often makes upper CI infinite; correct syntax is Dataset.Q - Dataset.L = 0

hyp.pairs.2 <- c("Dataset.L = 0", # 3 vs. 1
         "Dataset.Q = 0",
         "Dataset.C = 0",
         "DatasetE4 = 0",
         "DatasetE5 = 0",
         "Dataset.Q - Dataset.L = 0", # 5 vs. 3
         "Dataset.C - Dataset.L = 0",
         "DatasetE4 - Dataset.L = 0",
         "DatasetE5 - Dataset.L = 0",
         "Dataset.C - Dataset.Q = 0", # 7 vs. 5
         "DatasetE4 - Dataset.Q = 0",
         "DatasetE5 - Dataset.Q = 0",
         "DatasetE4 - Dataset.C = 0", # 8 vs. 7
         "DatasetE5 - Dataset.C = 0",
         "DatasetE5 - DatasetE4 = 0")


(hyp.pairs.2.res <- hypothesis(fit2, hyp.pairs.2))

#plot(hyp.pairs.2.res, chars = NULL, ask=FALSE)

hyp.pairs.2.res.df <- hyp.pairs.2.res$hypothesis

hyp.pairs.2.res.df$contrast <- c("3-1","5-1","7-1","8-1","9-1","5-3","7-3","8-3","9-3","7-5","8-5","9-5","8-7","9-7","9-8")

hyp.pairs.2.res.df$contrast <- factor(hyp.pairs.2.res.df$contrast,
                                    levels=hyp.pairs.2.res.df %>% arrange(desc(Estimate)) %>%
                                      dplyr::select(contrast) %>% unlist())

ggplot(hyp.pairs.2.res.df, aes(contrast,Estimate)) + 
  geom_errorbar(aes(ymin=`l-95% CI`,ymax=`u-95% CI`)) +
  geom_point(aes(fill=Star=="*"), size=7, shape=21) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=star.colors,name="Outside 95% CI") +
  coord_flip()
```

``` r
hyp.pairs.2.res.df <- hyp.pairs.2.res.df %>% separate(contrast, c("From", "To"), sep="[-]", remove=FALSE)

copy <- hyp.pairs.2.res.df %>% rename(From=To,To=From) %>% 
  mutate(Estimate=-Estimate,
         `l-95% CI`=-`l-95% CI`,
         `u-95% CI`=-`u-95% CI`)

pairs.compl <- bind_rows(hyp.pairs.2.res.df, copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

pairs.compl$From <- factor(pairs.compl$From, levels=c(9,8,7,5,3,1))
pairs.compl$To <- factor(pairs.compl$To, levels=c(1,3,5,7,8,9))

#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
pairs.compl %>% arrange(Estimate)

# can't get color to go the right direction
ggplot(pairs.compl %>% filter(Estimate >= 0)) +
  geom_tile(aes(x=To,y=From,fill=Estimate), color="black") +
  scale_fill_distiller(palette="Greys", direction=1) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Star=="*"),shape=21, color="black") +
  scale_fill_manual(values=star.colors, name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=Estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=Star=="*"), shape=21,stroke=1) +
  scale_fill_distiller(type="div", palette=4, limits=c(-max(abs(pairs.compl$Estimate)),max(abs(pairs.compl$Estimate)))) +
  scale_color_manual(values=c("grey90","black"), name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

hyp.pairs.2.res.df$From <- factor(hyp.pairs.2.res.df$From, levels=c(9,8,7,5,3,1))
hyp.pairs.2.res.df$To <- factor(hyp.pairs.2.res.df$To, levels=c(1,3,5,7,8,9))


ggplot(hyp.pairs.2.res.df) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=Star=="*"), shape=21,stroke=1) +
  scale_fill_distiller(type="div", palette=4, limits=c(-max(abs(pairs.compl$Estimate)),max(abs(pairs.compl$Estimate)))) +
  scale_color_manual(values=c("grey90","black"), name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

##### mgcv

``` r
#mgcv

library(mgcv)

graph.lgclust.beta.data <- gam(ResponsePct~Dataset,family=mgcv::betar(),data=graphics_lgclust.2)
graph.lgclust.beta.data.infl <- gam(ResponseInfl~Dataset,family=mgcv::betar(),data=graphics_lgclust.2)

gam.check(graph.lgclust.beta.data.infl)

bGAM <- coef(graph.lgclust.beta.data.infl)[2:6]
seGAM <- diag(vcov(graph.lgclust.beta.data.infl))[2:6]

    df <- data.frame(term     = nms <- names(bGAM),
                     estimate = bGAM)
    df <- transform(df,
                    upper = estimate + sqrt(seGAM),
                    lower = estimate - sqrt(seGAM))
    df
    
    p1 <- ggplot(df, aes(y = estimate, x = term, ymax = upper, ymin = lower)) +
    geom_pointrange() +
    labs(y = NULL,
         x = "Regression estimate",
         caption = "Bars are ±1 SE")

p1
```

##### glmmTMB

``` r
#package glmmTMB:

(graph.lgclust.beta.cond <- glmmTMB(ResponsePct ~ Condition + (1|Demo.ResponseID), graphics_lgclust.2, family=list(family="beta",link="logit")))

summary(graph.lgclust.beta.cond) # no pseudo R^2, unlike betareg

#(graph.lgclust.beta.cond.ZI <- glmmTMB(Response ~ Condition + (1|Demo.ResponseID), graphics_lgclust.2, family=list(family="beta",link="logit"),ziformula =~1))
#Error in nlminb(start = par, objective = fn, gradient = gr, control = control$optCtrl) : gradient function must return a numeric vector of length 7

#ranef(graph.lgclust.beta.cond)

#fixef(graph.lgclust.beta.cond)

#confint(graph.lgclust.beta.cond)

#getME(graph.lgclust.beta.cond, name=c("X"))

hist(residuals(graph.lgclust.beta.cond))

lrtest(graph.lgclust.beta.cond)
```

##### betareg

``` r
# betareg package

graph.lgclust.beta.cond <- betareg(ResponsePct~Condition, data=graphics_lgclust.2, type="ML") 
# specify random effects?
# no zero inflation?

summary(graph.lgclust.beta.cond)

lrtest(graph.lgclust.beta.cond)

# from car library

Anova(graph.lgclust.beta.cond, type="II")
```

##### gamlss

``` r
# gamlss
# also from http://rcompanion.org/handbook/J_02.html
# https://stats.stackexchange.com/questions/48028/beta-regression-of-proportion-data-including-1-and-0
# http://www.de.ufpe.br/~raydonal/Talks/talk-10-12-2008.pdf
# https://stats.stackexchange.com/questions/253738/what-is-mu-in-zero-one-beta-inflated-models-gamlss-beinf
# http://www.de.ufpe.br/~raydonal/Talks/Gamlss-Theory.pdf
# http://www.de.ufpe.br/~raydonal/Talks/Gamlss-Applications.pdf
# https://www.researchgate.net/publication/228429663_Instructions_on_how_to_use_the_gamlss_package_in_R_Second_Edition

# For subsets use data=subset(mydata, subset_of_mydata),
# for na.action use data=na.omit(mydata)
# The function refit() can be used if the converged component of the gamlss fitted object is FALSE
# Warning: The random(), ra() and rc() additive functions do not have a predict option implemented.

#temp <- graphics_lgclust %>% mutate(Response=Response/100) %>% dplyr::select(Demo.ResponseID, Response, Condition, Dataset, DatasetOrder) %>% drop_na()
```

``` r
#===== Fit model (all covariates) =====
mod.0 <- gamlss(Response~. + re(random=~1|Demo.ResponseID, opt = 'optim'), nu.formula=~., family=BEINF, data=temp)

#==== model selection for alpha using AIC ====
mod.1 <- stepGAIC(mod.0, what="nu")

#==== model selection for mu using AIC ====
mod.2 <- stepGAIC(mod.1)

#==== final fit model ====
fit <- gamlss(formula = Response ~ Dataset + (re(random = ~1 | Demo.ResponseID, opt = "optim")) , nu.formula = ~1, family = BEINF, data = temp, trace = FALSE)

summary(fit)
mean.fit <- meanBEINF(fit)

# https://stats.stackexchange.com/questions/309047/zero-inflated-beta-regression-using-gamlss-for-vegetation-cover-data

#means_m1 <- lpred(fit, type='response', what='mu', se.fit=T)
# can't do prediction with random effects?
```

``` r
graph.lgclust.beta.cond <- gamlss(Response~Condition+random(factor(Demo.ResponseID)), data=temp, family=BEINF) 

graph.lgclust.beta.cond.2 <- gamlss(Response~Condition+re(random=~1|Demo.ResponseID, opt = 'optim'), data=temp, family=BEINF) 

graph.lgclust.beta.cond.3 <- gamlss(Response~re(fixed=~Condition, random=~1|Demo.ResponseID, opt = 'optim'), data=temp, family=BEINF) 


car::Anova(graph.lgclust.beta.cond.2)
# Error: $ operator is invalid for atomic vectors

AIC(graph.lgclust.beta.cond,graph.lgclust.beta.cond.2,graph.lgclust.beta.cond.3)

lrtest(graph.lgclust.beta.cond.2)

print(graph.lgclust.beta.cond.2)

summary(graph.lgclust.beta.cond.2)

#names(graph.lgclust.beta.cond.2)

#deviance(graph.lgclust.beta.cond.2, "G")
#deviance(graph.lgclust.beta.cond.2, "P")


#plot(fitted(graph.lgclust.beta.cond), fitted(graph.lgclust.beta.cond.2))

plot(fitted(graph.lgclust.beta.cond.2), residuals(graph.lgclust.beta.cond.2))

plot(graph.lgclust.beta.cond.2)

#term.plot(graph.lgclust.beta.cond.2)
wp(graph.lgclust.beta.cond.2)
wp(graph.lgclust.beta.cond.2,ylim.all=2)

#summary results
summary(graph.lgclust.beta.cond)
# fit values
mean.fit <- meanBEINF(graph.lgclust.beta.cond)
#meanBEINF(graph.lgclust.beta.cond)[1]

#source("ExtraBIc.R")
#source("residualsBIc.R")
# can't find these files

#==== residuals =====
#a <- residuals.BIc(graph.lgclust.beta.cond) #standardized residual
#b <- residuals.BIc(graph.lgclust.beta.cond, type="experimental") #weighted residual
#c <- residuals.BIc(graph.lgclust.beta.cond, type="quantil") #quantal residual

#==== normal probability plots ====
#envelope.BIc(graph.lgclust.beta.cond, main="standardized residual" )
#envelope.BIc(graph.lgclust.beta.cond, type="experimental", main="weighted residual" )
#envelope.BIc(graph.lgclust.beta.cond, type="quantil", main="quantal residual")
```

``` r
# from https://pdfs.semanticscholar.org/ed3d/f96aa477d6454d5737403a5aab751ccd9315.pdf

#gamlss(Response~Condition+random(factor(Demo.ResponseID)), data=temp, family=BEINF) 
#gamlss(Response~Condition+re(random=~1|Demo.ResponseID, opt = 'optim'), data=temp, family=BEINF) 
#gamlss(Response~re(fixed=~Condition, random=~1|Demo.ResponseID, opt = 'optim'), data=temp, family=BEINF)

mod01.0 <- gamlss(Response ~ 1, data = temp, family = BEINF)
mod01.r <- gamlss(Response ~ 1 + random(factor(Demo.ResponseID)), data = temp, family = BEINF) # lowest deviance, highest AIC, highest df
mod01.r1 <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID), data = temp, family = BEINF)
mod01.r2 <- gamlss(Response ~ re(fixed=~1, random=~1|Demo.ResponseID), data = temp, family = BEINF)
mod01.r3 <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), data = temp, family = BEINF) # change in optimizer decreases df a small fraction, much faster 
mod01.r4 <- gamlss(Response ~ re(fixed=~1, random=~1|Demo.ResponseID, opt = 'optim'), data = temp, family = BEINF)
AIC(mod01.0,mod01.r,mod01.r1,mod01.r2,mod01.r3,mod01.r4) # keep .r3 syntax

mod01 <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), data = temp, family = BEINF)

mod02.0 <- gamlss(Response ~ Condition, data = temp, family = BEINF)
mod02.r <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), data = temp, family = BEINF)
mod02.0.s <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, data = temp, family = BEINF)
mod02.0.n <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), nu.fo = ~Condition, data = temp, family = BEINF)
mod02.0.t <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), tau.fo = ~Condition, data = temp, family = BEINF)
mod02.0.s.n <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, nu.fo = ~Condition, data = temp, family = BEINF)
mod02.0.s.t <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, tau.fo = ~Condition, data = temp, family = BEINF)
mod02.0.n.t <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), tau.fo = ~Condition, nu.fo = ~Condition, data = temp, family = BEINF)
mod02.0.s.n.t <- gamlss(Response ~ 1 + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, nu.fo = ~Condition, tau.fo = ~Condition, data = temp, family = BEINF)
mod02.r.s <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, data = temp, family = BEINF)
mod02.r.n <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), nu.fo = ~Condition, data = temp, family = BEINF)
mod02.r.t <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), tau.fo = ~Condition, data = temp, family = BEINF)
mod02.r.s.n <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, nu.fo = ~Condition, data = temp, family = BEINF)
mod02.r.s.t <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, tau.fo = ~Condition, data = temp, family = BEINF)
mod02.r.n.t <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), tau.fo = ~Condition, nu.fo = ~Condition, data = temp, family = BEINF)
mod02.r.s.n.t <- gamlss(Response ~ Condition + re(random=~1|Demo.ResponseID, opt = 'optim'), sigma.fo = ~Condition, nu.fo = ~Condition, tau.fo = ~Condition, data = temp, family = BEINF)

#summary(mod02.r.s.n.t)

AIC(mod01, mod02.0, mod02.r, mod02.0.s, mod02.0.n, mod02.0.t, mod02.0.s.n, mod02.0.s.t, mod02.0.n.t, mod02.0.s.n.t, mod02.r.s, mod02.r.n, mod02.r.t, mod02.r.s.n, mod02.r.s.t, mod02.r.n.t, mod02.r.s.n.t) 
# best is mod01

AIC(mod01, mod02.0, mod02.r, mod02.0.s, mod02.0.n, mod02.0.t, mod02.0.s.n, mod02.0.s.t, mod02.0.n.t, mod02.0.s.n.t, mod02.r.s, mod02.r.n, mod02.r.t, mod02.r.s.n, mod02.r.s.t, mod02.r.n.t, mod02.r.s.n.t, k=3.8) 
# best is mod01

AIC(mod01, mod02.0, mod02.r, mod02.0.s, mod02.0.n, mod02.0.t, mod02.0.s.n, mod02.0.s.t, mod02.0.n.t, mod02.0.s.n.t, mod02.r.s, mod02.r.n, mod02.r.t, mod02.r.s.n, mod02.r.s.t, mod02.r.n.t, mod02.r.s.n.t, k=7.96) 
# best is mod01

# adding condition does not improve model 

plot(mod01)
term.plot(mod01, se=T)

#op <- par(mfrow = c(4, 2))
#lev <- c(1, 2, 3, 4, 5, 6, 7)
#ind <- c(3, 2, 9, 12, 1, 7, 4)
#j <- 0
#for (i in ind) {
#  j = 1 + j
#  xlab <- paste("treatment = ", eval(substitute(lev[j])))
#  ylab <- paste("p(y)")
#  plotBEINF(mu = fitted(mod22)[i],
#  sigma = fitted(mod22, "sigma")[i],
#  nu = fitted(mod22, "nu")[i],
#  tau = fitted(mod22, "tau")[i],
#  from = 0, to = 1, n = 101, xlab = xlab, ylab = ylab)
#  }
#term.plot(mod22, se = T)
#par(op)

plotBEINF(mu=fitted(mod01)[1],
          sigma=fitted(mod01, "sigma")[1],
          nu=fitted(mod01, "nu")[1],
          tau=fitted(mod01, "tau")[1],
          from = 0, to = 1, n=101)
```

Run this one

``` r
#data(usair)
#mod1 <- gamlss(y ~ ., data = usair, family = GA)

#mod1 <- gamlss(ResponsePct ~ Condition +
#Dataset + DatasetOrder +
#DatasetStartTime + TaskOrder + CorrectAnswer + Underestimated +
#Stats.Q_TotalDuration + Stats.OperatingSystem + Demo.age +
#Demo.gender + Demo.lang + Demo.educ + Demo.acfield +
#Demo.dailytech_Computer + Demo.dailytech_Tablet + Demo.dailytech_SmartPhone +
#Demo.weeklygaming + Demo.expdataanal + Demo.expdatavis + Demo.expreadnetvis +
#Demo.expcreatenetvis + StatsNumPixels + AvgDeg + Density + LargeClust1 +
#Modularity + NumClust + NumHighDegree + NumLinks + NumNodes + NumNodesClust1 +
#ConditionPhrasing + ConditionGraphics + ConditionColor + Ctrl_dummy +
#Demo.expreadnetvis.alot + Demo.expcreatenetvis.alot + Stats.OperatingSystemCombined +
#Stats.OperatingSystemCombined2 + Stats.OperatingSystemCombined3 +
#Stats.OperatingSystemCombined4 + Stats.OperatingSystemCombined5 +
#Stats.OperatingSystemWindows + Stats.OperatingSystemMacintosh +
#Stats.OperatingSystemAndroid + Stats.OperatingSystemiPhone +
#Stats.OperatingSystemNumClust + Demo.acfieldGrouped + Demo.acfieldGrouped2 +
#Demo.acfieldGrouped3 + Overestimated + UnderestDummy +
#DatasetDuration,
#data = temp, family = BEINF)

mod1 <- gamlss(ResponsePct ~ Dataset + 
                 UnderestDummy + 
                 Demo.gender + Demo.lang + 
                 Demo.expreadnetvis + 
                 Stats.OperatingSystemWindows,
               data = temp, family = BEINF)

#library(MASS)
#dropterm(mod1, test = "Chisq")

dropterm(mod1, test = "Chisq")

#To demonstrate the function addterm consider adding a two way interaction term into the model mod1. Note that the scope argument has to be defined explicitly here.
#addterm(mod1, scope = ~(x1 + x2 + x3 + x4 + x5 + x6)^2, test = "Chisq")

addterm(mod1, scope = ~(Dataset + UnderestDummy + Demo.gender + Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows)^2, test = "Chisq")

#Dataset:UnderestDummy, UnderestDummy:Stats.OperatingSystemWindows, UnderestDummy:Demo.gender

mod2 <- gamlss(ResponsePct ~ Dataset + 
                 UnderestDummy + 
                 Demo.gender + Demo.lang + 
                 Demo.expreadnetvis + 
                 Stats.OperatingSystemWindows +
                 Dataset:UnderestDummy +
                 UnderestDummy:Stats.OperatingSystemWindows +
                 UnderestDummy:Demo.gender,
               data = temp, family = BEINF)

summary(mod2)

mod3 <- stepGAIC(mod1, scope = list(lower = ~1, upper = ~(Dataset + UnderestDummy + Demo.gender + Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows)^2))

# Final
# ResponsePct ~ Dataset + UnderestDummy + Demo.gender + Demo.lang + 
#    Demo.expreadnetvis + Stats.OperatingSystemWindows + Dataset:UnderestDummy + 
#    Demo.gender:Stats.OperatingSystemWindows + UnderestDummy:Demo.gender + 
#    UnderestDummy:Stats.OperatingSystemWindows

AIC(mod1, mod2, mod3)
# mod3 is best

mod3$anova
# pretty complicated model, though

#gs <- gamlss.scope(model.frame(ResponsePct ~ Dataset + factor(UnderestDummy) + factor(Demo.gender) + factor(Demo.lang) + Demo.expreadnetvis + factor(Stats.OperatingSystemWindows), data = temp))

#gs

#mod4 <- stepGAIC.CH(mod3, scope = gs, k = 2)
# no real point looking for smoothing functions; all of these are factor variables
#mod4$anova
#formula(mod4, "mu")

#op <- par(mfrow = c(3, 2))
# term.plot(mod3, se = T, partial = T)
# par(op)

mod7<-stepGAIC(mod3, what="sigma", scope=~Dataset + UnderestDummy + Demo.gender + Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows)


# final sigma: ~Dataset + UnderestDummy 
```

Run this one

``` r
#SAVE THE RESULTS
save(mod7, file = file.path(analysisDataDir,"fits/graph_lgclust_beinf_int.RData"))
```

###### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_lgclust_beinf_int.RData"))
```

##### broom

``` r
mod7$call
```

    ## gamlss(formula = ResponsePct ~ Dataset + UnderestDummy + Demo.gender + 
    ##     Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows + 
    ##     Dataset:UnderestDummy + Demo.gender:Stats.OperatingSystemWindows + 
    ##     UnderestDummy:Demo.gender + UnderestDummy:Stats.OperatingSystemWindows, 
    ##     sigma.formula = ~Dataset + UnderestDummy, family = BEINF, 
    ##     data = temp, trace = FALSE)

``` r
mod7.tidy.df <- tidy(mod7)
mod7.tidy.df.mu <- mod7.tidy.df %>% filter(parameter == "mu")

#mod7.ci.df <- confint_tidy(mod7)
#mod7.ci.df.nona <- mod7.ci.df %>% drop_na()

#mod7.tidyci.df <- bind_cols(mod7.tidy.df.mu, mod7.ci.df.nona)

mod7.aug.df <- augment(mod7,temp) # augment isn't supposed to work but seems to
```

    ## ******************************************************************
    ## Family:  c("BEINF", "Beta Inflated") 
    ## 
    ## Call:  gamlss(formula = ResponsePct ~ Dataset + UnderestDummy +  
    ##     Demo.gender + Demo.lang + Demo.expreadnetvis +  
    ##     Stats.OperatingSystemWindows + Dataset:UnderestDummy +  
    ##     Demo.gender:Stats.OperatingSystemWindows + UnderestDummy:Demo.gender +  
    ##     UnderestDummy:Stats.OperatingSystemWindows, sigma.formula = ~Dataset +  
    ##     UnderestDummy, family = BEINF, data = temp, trace = FALSE) 
    ## 
    ## Fitting method: RS() 
    ## 
    ## ------------------------------------------------------------------
    ## Mu link function:  logit
    ## Mu Coefficients:
    ##                                                                  Estimate
    ## (Intercept)                                                      -0.29360
    ## Dataset.L                                                        -0.65261
    ## Dataset.Q                                                         0.50000
    ## Dataset.C                                                        -0.56058
    ## Dataset^4                                                         0.17376
    ## Dataset^5                                                         0.05939
    ## UnderestDummy2-CorrectOrOver                                      1.92492
    ## Demo.genderMale                                                   0.34428
    ## Demo.genderNon-binary                                             0.48798
    ## Demo.langEnglish                                                 -1.08096
    ## Demo.expreadnetvis.L                                              0.93106
    ## Demo.expreadnetvis.Q                                             -0.66855
    ## Demo.expreadnetvis.C                                             -1.47123
    ## Demo.expreadnetvis^4                                             -0.85594
    ## Stats.OperatingSystemWindowsWindows                               0.15374
    ## Dataset.L:UnderestDummy2-CorrectOrOver                           -0.79741
    ## Dataset.Q:UnderestDummy2-CorrectOrOver                            0.42585
    ## Dataset.C:UnderestDummy2-CorrectOrOver                           -0.15378
    ## Dataset^4:UnderestDummy2-CorrectOrOver                            0.64210
    ## Dataset^5:UnderestDummy2-CorrectOrOver                           -0.90244
    ## Demo.genderMale:Stats.OperatingSystemWindowsWindows              -0.27711
    ## UnderestDummy2-CorrectOrOver:Demo.genderMale                     -0.16377
    ## UnderestDummy2-CorrectOrOver:Stats.OperatingSystemWindowsWindows -0.15675
    ##                                                                  Std. Error
    ## (Intercept)                                                         0.38225
    ## Dataset.L                                                           0.08652
    ## Dataset.Q                                                           0.14349
    ## Dataset.C                                                           0.11489
    ## Dataset^4                                                           0.13090
    ## Dataset^5                                                           0.18979
    ## UnderestDummy2-CorrectOrOver                                        0.11865
    ## Demo.genderMale                                                     0.10649
    ## Demo.genderNon-binary                                               0.26479
    ## Demo.langEnglish                                                    0.34753
    ## Demo.expreadnetvis.L                                                0.23130
    ## Demo.expreadnetvis.Q                                                0.20309
    ## Demo.expreadnetvis.C                                                0.29918
    ## Demo.expreadnetvis^4                                                0.22792
    ## Stats.OperatingSystemWindowsWindows                                 0.11119
    ## Dataset.L:UnderestDummy2-CorrectOrOver                              0.10615
    ## Dataset.Q:UnderestDummy2-CorrectOrOver                              0.15639
    ## Dataset.C:UnderestDummy2-CorrectOrOver                              0.12618
    ## Dataset^4:UnderestDummy2-CorrectOrOver                              0.13874
    ## Dataset^5:UnderestDummy2-CorrectOrOver                              0.19586
    ## Demo.genderMale:Stats.OperatingSystemWindowsWindows                 0.09613
    ## UnderestDummy2-CorrectOrOver:Demo.genderMale                        0.07955
    ## UnderestDummy2-CorrectOrOver:Stats.OperatingSystemWindowsWindows    0.10722
    ##                                                                  t value
    ## (Intercept)                                                       -0.768
    ## Dataset.L                                                         -7.543
    ## Dataset.Q                                                          3.484
    ## Dataset.C                                                         -4.879
    ## Dataset^4                                                          1.327
    ## Dataset^5                                                          0.313
    ## UnderestDummy2-CorrectOrOver                                      16.223
    ## Demo.genderMale                                                    3.233
    ## Demo.genderNon-binary                                              1.843
    ## Demo.langEnglish                                                  -3.110
    ## Demo.expreadnetvis.L                                               4.025
    ## Demo.expreadnetvis.Q                                              -3.292
    ## Demo.expreadnetvis.C                                              -4.918
    ## Demo.expreadnetvis^4                                              -3.755
    ## Stats.OperatingSystemWindowsWindows                                1.383
    ## Dataset.L:UnderestDummy2-CorrectOrOver                            -7.512
    ## Dataset.Q:UnderestDummy2-CorrectOrOver                             2.723
    ## Dataset.C:UnderestDummy2-CorrectOrOver                            -1.219
    ## Dataset^4:UnderestDummy2-CorrectOrOver                             4.628
    ## Dataset^5:UnderestDummy2-CorrectOrOver                            -4.608
    ## Demo.genderMale:Stats.OperatingSystemWindowsWindows               -2.883
    ## UnderestDummy2-CorrectOrOver:Demo.genderMale                      -2.059
    ## UnderestDummy2-CorrectOrOver:Stats.OperatingSystemWindowsWindows  -1.462
    ##                                                                  Pr(>|t|)
    ## (Intercept)                                                      0.442612
    ## Dataset.L                                                        9.84e-14
    ## Dataset.Q                                                        0.000513
    ## Dataset.C                                                        1.23e-06
    ## Dataset^4                                                        0.184657
    ## Dataset^5                                                        0.754382
    ## UnderestDummy2-CorrectOrOver                                      < 2e-16
    ## Demo.genderMale                                                  0.001263
    ## Demo.genderNon-binary                                            0.065623
    ## Demo.langEnglish                                                 0.001918
    ## Demo.expreadnetvis.L                                             6.09e-05
    ## Demo.expreadnetvis.Q                                             0.001028
    ## Demo.expreadnetvis.C                                             1.01e-06
    ## Demo.expreadnetvis^4                                             0.000182
    ## Stats.OperatingSystemWindowsWindows                              0.167057
    ## Dataset.L:UnderestDummy2-CorrectOrOver                           1.23e-13
    ## Dataset.Q:UnderestDummy2-CorrectOrOver                           0.006577
    ## Dataset.C:UnderestDummy2-CorrectOrOver                           0.223207
    ## Dataset^4:UnderestDummy2-CorrectOrOver                           4.14e-06
    ## Dataset^5:UnderestDummy2-CorrectOrOver                           4.57e-06
    ## Demo.genderMale:Stats.OperatingSystemWindowsWindows              0.004024
    ## UnderestDummy2-CorrectOrOver:Demo.genderMale                     0.039757
    ## UnderestDummy2-CorrectOrOver:Stats.OperatingSystemWindowsWindows 0.144032
    ##                                                                     
    ## (Intercept)                                                         
    ## Dataset.L                                                        ***
    ## Dataset.Q                                                        ***
    ## Dataset.C                                                        ***
    ## Dataset^4                                                           
    ## Dataset^5                                                           
    ## UnderestDummy2-CorrectOrOver                                     ***
    ## Demo.genderMale                                                  ** 
    ## Demo.genderNon-binary                                            .  
    ## Demo.langEnglish                                                 ** 
    ## Demo.expreadnetvis.L                                             ***
    ## Demo.expreadnetvis.Q                                             ** 
    ## Demo.expreadnetvis.C                                             ***
    ## Demo.expreadnetvis^4                                             ***
    ## Stats.OperatingSystemWindowsWindows                                 
    ## Dataset.L:UnderestDummy2-CorrectOrOver                           ***
    ## Dataset.Q:UnderestDummy2-CorrectOrOver                           ** 
    ## Dataset.C:UnderestDummy2-CorrectOrOver                              
    ## Dataset^4:UnderestDummy2-CorrectOrOver                           ***
    ## Dataset^5:UnderestDummy2-CorrectOrOver                           ***
    ## Demo.genderMale:Stats.OperatingSystemWindowsWindows              ** 
    ## UnderestDummy2-CorrectOrOver:Demo.genderMale                     *  
    ## UnderestDummy2-CorrectOrOver:Stats.OperatingSystemWindowsWindows    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Sigma link function:  logit
    ## Sigma Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -1.58028    0.06938 -22.777  < 2e-16 ***
    ## Dataset.L                    -0.26033    0.07080  -3.677 0.000248 ***
    ## Dataset.Q                     0.01649    0.06916   0.238 0.811576    
    ## Dataset.C                    -0.12726    0.06480  -1.964 0.049795 *  
    ## Dataset^4                     0.37193    0.06413   5.799 8.76e-09 ***
    ## Dataset^5                    -0.45130    0.06625  -6.812 1.60e-11 ***
    ## UnderestDummy2-CorrectOrOver  0.64494    0.07827   8.240 5.01e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Nu link function:  log 
    ## Nu Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   -20.54     847.38  -0.024    0.981
    ## 
    ## ------------------------------------------------------------------
    ## Tau link function:  log 
    ## Tau Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -6.3054     0.7078  -8.909   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## No. of observations in the fit:  1097 
    ## Degrees of Freedom for the fit:  32
    ##       Residual Deg. of Freedom:  1065 
    ##                       at cycle:  6 
    ##  
    ## Global Deviance:     -1746.723 
    ##             AIC:     -1682.723 
    ##             SBC:     -1522.712 
    ## ******************************************************************

``` r
#mod7.summ.df <- glance(mod7) #glance doesn't work

#mod7.tidyci.df$term <- factor(mod7.tidyci.df$term,
#                                          levels=mod7.tidyci.df %>% arrange(estimate) %>%
#                                            dplyr::select(term) %>% unlist())

ggplot(mod7.aug.df) + geom_point(aes(ResponsePct,.fitted))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-140-1.png)<!-- -->

``` r
#ggplot(mod7.tidyci.df, aes(term, estimate)) + 
#  geom_point() + 
#  coord_flip()

#ggplot(mod7.aug.df %>% group_by(Dataset) %>% summarize(est=mean(.fitted))) +
#  geom_point(aes(Dataset,est)) +
#  #scale_y_continuous(limits = c(-.06,1.2), breaks=c(0,.3,.6,.9,1.2)) +
#  coord_flip()

#plot(emmeans(graph.clickhd.nb.full, "Dataset"))

#ggplot(mod7.aug.df) +
#  geom_violin(aes(Dataset,.fitted)) +
#  geom_point(data=mod7.aug.df %>% group_by(Dataset) %>% summarize(est=median(ResponsePct)),aes(Dataset,est)) +
#  #scale_y_continuous(limits = c(-.06,1.2), breaks=c(0,.3,.6,.9,1.2)) +
#  coord_flip()

# could be okay, but not sure of calculation
#ggplot(data = mod7.aug.df) + 
#  geom_errorbar(data=mod7.aug.df %>% group_by(Dataset) %>% 
#               summarize(est=mean(.fitted), 
#                         ymin=mean(.fitted-1.96*.se.fit), 
#                         ymax=mean(.fitted+1.96*.se.fit)),
#             aes(Dataset,est, ymin=ymin, ymax=ymax, width=.2)) +
#  geom_point(data=mod7.aug.df %>% group_by(Dataset) %>% 
#               summarize(est=mean(.fitted), 
#                         ymin=mean(.fitted-1.96*.se.fit), 
#                         ymax=mean(.fitted+1.96*.se.fit)),
#             aes(Dataset,est))

# no good
#ggplot(data = mod7.aug.df) + 
#  geom_errorbar(data=mod7.aug.df %>% group_by(Dataset) %>% 
#               summarize(est=mean(ResponsePct), 
#                         ymin=mean(.fitted-1.96*.se.fit), 
#                         ymax=mean(.fitted+1.96*.se.fit)),
#             aes(Dataset,est, ymin=ymin, ymax=ymax, width=.2)) +
#  geom_point(data=mod7.aug.df %>% group_by(Dataset) %>% 
#               summarize(est=mean(ResponsePct), 
#                         ymin=mean(.fitted-1.96*.se.fit), 
#                         ymax=mean(.fitted+1.96*.se.fit)),
#             aes(Dataset,est))
```

``` r
set.seed(2014)

# doesn't work
bootnls <- mod7.aug.df %>% bootstrap(100) %>% do(tidy(
      gamlss(formula = ResponsePct ~ Dataset + UnderestDummy + Demo.gender + 
    Demo.lang + Demo.expreadnetvis + Stats.OperatingSystemWindows + 
    Dataset:UnderestDummy + Demo.gender:Stats.OperatingSystemWindows + 
    UnderestDummy:Demo.gender + UnderestDummy:Stats.OperatingSystemWindows, 
    sigma.formula = ~Dataset + UnderestDummy, family = BEINF, 
    ., trace = FALSE)
    ))
alpha = .05
bootnls %>% group_by(term) %>% summarize(low=quantile(estimate, alpha / 2),
                                         high=quantile(estimate, 1 - alpha / 2))
ggplot(bootnls, aes(estimate)) + geom_histogram(binwidth=2) + facet_wrap(~ term, scales="free")

bootnls_aug <- mtcars %>% bootstrap(100) %>%
    do(augment(nls(mpg ~ k / wt + b, ., start=list(k=1, b=0)), .))

ggplot(bootnls_aug, aes(wt, mpg)) + geom_point() +
    geom_line(aes(y=.fitted, group=replicate), alpha=.2)

smoothspline_aug <- mtcars %>% bootstrap(100) %>%
    do(augment(smooth.spline(.$wt, .$mpg, df=4), .))

ggplot(smoothspline_aug, aes(wt, mpg)) + geom_point() +
    geom_line(aes(y=.fitted, group=replicate), alpha=.2)
```

``` r
graph.lgclust.broom.tidy <- tidy(mod7)
graph.lgclust.broom.aug <- augment(graph.lgclust.nb.full, temp)
graph.lgclust.broom.ci <- confint_tidy(graph.lgclust.nb.full)
graph.lgclust.broom.tidyci <- bind_cols(graph.lgclust.broom.tidy %>% arrange(group), graph.lgclust.broom.ci)

graph.clickhd.broom.tidyci$term <- factor(graph.clickhd.broom.tidyci$term,
                                          levels=graph.clickhd.broom.tidyci %>% arrange(estimate) %>%
                                            select(term) %>% unlist())

ggplot(graph.clickhd.broom.tidyci, aes(term, estimate)) + 
  geom_point() + 
  coord_flip()

ggplot(graph.clickhd.broom.aug %>% group_by(Dataset) %>% summarize(est=mean(.fitted))) +
  geom_point(aes(Dataset,est)) +
  scale_y_continuous(limits = c(-.06,1.2), breaks=c(0,.3,.6,.9,1.2)) +
  coord_flip()

plot(emmeans(graph.clickhd.nb.full, "Dataset"))

ggplot(graph.clickhd.broom.aug) +
  geom_violin(aes(Dataset,.fitted)) +
  scale_y_continuous(limits = c(-.06,1.2), breaks=c(0,.3,.6,.9,1.2)) +
  coord_flip()
```

``` r
predict.current <- bind_cols(temp,as.data.frame(predict(mod7,what="mu",type="response",se.fit=TRUE)))
# can also get fitted values with fitted(mod7), just join to temp

ggplot(predict.current) + geom_point(aes(ResponsePct,fit))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-143-1.png)<!-- -->

``` r
# trying to reproduce other M/SE calculations; doesn't seem quite right
#ggplot(predict.current %>% 
#         group_by(Dataset) %>% 
#         summarise(avgFit=mean(fit),avgSE=mean(se.fit)) %>% 
#         mutate(lower.CI=avgFit-avgSE, upper.CI=avgFit+avgSE)) + 
#  geom_pointrange(aes(Dataset,avgFit,ymin=lower.CI,ymax=upper.CI))

#lsmeans(mod7, ~ Dataset)
#Anova(mod7)

ggplot(mod7.aug.df) + geom_point(aes(ResponsePct, .fitted))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-143-2.png)<!-- -->

``` r
# the fitted values from this are different from the fitted values from augment with broom --
# different scale anyway.  why?
# answer: augment is using type="link" rather than type="response"; hard to compare to ResponsePct directly, but
# matches result from term.plot better?
```

##### gamlss 2

``` r
ggplot(graphics_lgclust, aes(Response)) +
  geom_histogram(binwidth = 1) +
  labs(title="Distribution of Response values for Percentage of Nodes\nin Largest Cluster task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-144-1.png)<!-- -->

``` r
Rsq(mod7)
```

    ## [1] 0.7812866

``` r
Rsq(mod7, type="both")
```

    ## $CoxSnell
    ## [1] 0.7812866
    ## 
    ## $CraggUhler
    ## [1] -10.42331

``` r
#mod7.aug.df <- augment(mod7,temp)

ggplot(mod7.aug.df, aes(ResponsePct,.fitted)) + 
  geom_bin2d() +
  geom_smooth(method="lm") +
  labs(title="Real vs. Predicted Response values for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
       x="Response as a Percentage")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-144-2.png)<!-- -->

``` r
#a1 <- mod7$anova
#a1 <- anova(mod7)
#a1 <- aov(mod7, what="mu")

#posthoc <- TukeyHSD(x=a1, 'temp$Dataset', conf.level=0.95)
# doesn't work

summary(mod7)

#acfResid(mod7)

#dtop(mod7)

plot(mod7)

plot2way(mod7,c("Dataset","UnderestDummy"))

print(mod7)

rqres.plot(mod7, ylim.all=1)
#rqres.plot(mod7, howmany=40, plot="all")

term.plot(mod7, ask=FALSE)

ggplot(mod7.aug.df %>% group_by(Dataset) %>% summarise(median=median(.fitted))) + geom_boxplot(aes(Dataset,median)) + scale_y_continuous(limits=c(-2,3))

#term.plot(mod7, ask=FALSE, pages=1)
#term.plot(mod7, ask=FALSE, partial.resid = TRUE)
term.plot(mod7, what="mu", ask=FALSE)
term.plot(mod7, what="sigma", ask=FALSE)



temp2 <- temp
temp2$Demo.ResponseID <- factor(temp2$Demo.ResponseID)



hist(temp$ResponsePct)

pdf.plot(obj=mod7, min=0, max=1, step=.01)

# plotting the distribution
plotBEINF(mu = predict(mod7, what="mu", type="response")[1], 
          sigma = predict(mod7, what="sigma", type="response")[1], 
          nu = predict(mod7, what="nu", type="response")[1], 
          tau = predict(mod7, what="tau", type="response")[1],
          from = 0, 
          to = 1, 
          n = 101)

# trying link instead
#plotBEINF(mu = predict(mod7, what="mu", type="link"), 
#          sigma = predict(mod7, what="sigma", type="link"), 
#          nu = predict(mod7, what="nu", type="link"), 
#          tau = predict(mod7, what="tau", type="link"),
#          from = 0, 
#          to = 1, 
#          n = 101)


# plotting the cdf
plot(function(y) pBEINF(y, mu = predict(mod7, what="mu", type="response")[1], 
          sigma = predict(mod7, what="sigma", type="response")[1], 
          nu = predict(mod7, what="nu", type="response")[1], 
          tau = predict(mod7, what="tau", type="response")[1]), 0, 1)

# plotting the inverse cdf
plot(function(y) qBEINF(y, mu = predict(mod7, what="mu", type="response")[1], 
          sigma = predict(mod7, what="sigma", type="response")[1], 
          nu = predict(mod7, what="nu", type="response")[1], 
          tau = predict(mod7, what="tau", type="response")[1]), 0.01, .99)

# mean
meanBEINF(mod7)[1]



# from http://www.stat.tugraz.at/friedl/GAMLSS/Practical-Graz.pdf
# The worm plot (see Section ??) is a de-trended normal Q-Q plot of the residuals. Modelinadequacy is indicated when many points plotted lie outside the (dotted) point-wise 95% confidence bands. 

wp(mod7, ylim.all=1) # not great

#wp(mod7, resid = resid(mod7), xvar=~temp$Dataset)
# not quite right

#edfAll(mod7)

fittedPlot(mod7, x=temp$Dataset)
fittedPlot(mod7, x=temp$Demo.expreadnetvis)
fittedPlot(mod7, x=temp$Demo.lang)
fittedPlot(mod7, x=factor(temp$Stats.OperatingSystemWindows))
fittedPlot(mod7, x=factor(temp$UnderestDummy))
fittedPlot(mod7, x=factor(temp$Demo.gender))

#Q.stats(mod7,xvar=temp$Dataset,n.inter=30)
```

``` r
smod7<-summary(mod7, save=TRUE)
smod7$mu.coef.table
smod7$sigma.coef.table
smod7$coef.table
smod7$coef
smod7$object$anova
#to get a list of all the slots in the object
str(smod7)

estimate.pval.mu <- as.data.frame(cbind(term=rownames(smod7$coef.table[c(1:23),]),smod7$coef.table[c(1:23),]), row.names=FALSE, stringsAsFactors = FALSE)
```

``` r
# try https://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re

# https://stats.stackexchange.com/questions/299792/r-lm-covariance-matrix-manual-calculation-failure

#print(vcov(mod7))

#new.data <- expand.grid(
#  ResponsePct=seq(0,1,by=.001),
#  Dataset=unique(temp$Dataset),
#  UnderestDummy=unique(temp$UnderestDummy),
#  Demo.gender=unique(temp$Demo.gender),
#  Demo.lang=unique(temp$Demo.lang),
#  Demo.expreadnetvis=unique(temp$Demo.expreadnetvis),
#  Stats.OperatingSystemWindows=unique(temp$Stats.OperatingSystemWindows)
#  )

#beta.pred <-bind_cols(as.data.frame(predictAll(mod7, newdata=new.data, type="response")))

pi <- mod7.aug.df$.fitted

w <- pi * (1 - pi)

v <- diag(w, length(w), length(w))

#XtX_inv <- solve(t(as.matrix(new.data)) %*% v %*% as.matrix(new.data))
# still doesn't work

#p0 = nu /(1+nu+tau) & p1 = tau/(1+nu+tau)

vcov.mod7 <- vcov(mod7) # just mu? 23 x 23 (no, mu has 25 coefficients, but 23 df?)
vcov.mod7.2 <- smod7$covmat$vcov # all? 32 x 32
std.err <- sqrt(t(as.matrix(new.data)) %*% vcov.mod7 %*% as.matrix(new.data))

trans <- t(as.matrix(new.data))
new.data.mat <- as.matrix(new.data)

trans %*% new.data.mat

plot(mod7)

sum.pred <- beta.pred %>% group_by(Dataset) %>% summarise(mean=mean(mu),
                                                          sd=sd(mu),
                                                          sample.size=n(),
                                                          df=sample.size-1,
                                                          alpha=.025,
                                                          t.dist=1.96,
                                                          mod=t.dist*(sd/sqrt(sample.size)),
                                                          lower.ci=mean-mod,
                                                          upper.ci=mean+mod
                                                          )

ggplot(beta.pred) + geom_boxplot(aes(Dataset, mu)) + scale_y_continuous(limits=c(0,1)) + coord_flip()
ggplot(sum.pred, aes(Dataset, mean)) + geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) + geom_point() + scale_y_continuous(limits=c(0,1)) + coord_flip()
```

``` r
#mod2 <- stepGAIC.VR(mod1) 
#or
#mod2<-stepGAIC(mod1, additive=FALSE)
#mod2$anova
#mod2 <- stepGAIC(mod1, k = log(41))
#mod3 <- stepGAIC(mod1, scope = list(lower = ~1, upper = ~(x1 + x2 + x3 + x4 + x5 + x6)^2))
#mod3$anova
#mod2 <- stepGAIC.VR(mod1)
#mod2$anova
#mod2 <- stepGAIC(mod1, k = log(41))
#mod3 <- stepGAIC(mod1, scope = list(lower = ~1, upper = ~(x1 + x2 + x3 + x4 + x5 + x6)^2))
#gs <- gamlss.scope(model.frame(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = usair))
#gs
#gs1 <- gamlss.scope(model.frame(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = usair), smoother = "lo", arg = "span=.7", form = TRUE)
#gs1
#mod5 <- gamlss(y ~ 1, data = usair, family = GA)
#mod6 <- stepGAIC(mod5, gs, additive = TRUE)
#mod6$anova
#mod6
#mod7<-stepGAIC(mod6, what="sigma", scope=~x1+x2+x3+x4+x5+x6)
#mod7$anova

#We start with a model for mu including all the explanatory variables.
#We shall now use the dropterm to check if model m1 can be simplified by dropping any of the existing terms in mu and the function addterm to check whether two way interactions of the existing terms are needed.
#Based on the Chi square tests no terms can be left out and no two way interaction is needed.
#Since we established that adding or dropping terms in mu is not beneficial there is nopoint using stepGAIC.VR() for modelling the mu parameter with linear terms. Instead we will use (stepGAIC.CH()) trying to establish if smoothing terms are needed in the mu model.
#gs <- gamlss.scope(model.frame(Claims ~ factor(SD) + L_Popdensity + L_KI + L_Accidents + L_Population, data = LGAclaims))
#gs
#m2 <- stepGAIC.CH(m1, scope = gs, k = 2)
#m2$anova
#formula(m2, "mu")
#The best model includes smoothing terms for L_Popdensity, L_Accidents and L_Population but not for L_KI.
#op <- par(mfrow = c(3, 2))
#term.plot(m2, se = T, partial = T)
#par(op)
#Given that we have establish a good model for mu, we proceed to find a good model for sigma. We start first with linear terms but we exclude the factor SD since some of the levels of the factor have very few observations.
#m11 <- stepGAIC.VR(m2, scope = ~L_Popdensity + L_KI + L_Accidents + L_Population, what = "sigma", k = 2)
#m11$anova
#Note that the argument what is used here to determine which distribution parameter is to be modelled. Here variables L_Population, L_KI and L_Accidents were found important in explaining the sigma parameter. The model chosen using AIC appears over complicated. Maybe a higher penalty for GAIC would be more appropriate here.
```

``` r
# https://github.com/mjskay/when-ish-is-my-bus/blob/master/bus-model.Rmd

predictions <- expand.grid(
  ResponsePct=seq(0,1,by=.001),
  Dataset=unique(temp$Dataset),
  UnderestDummy=unique(temp$UnderestDummy),
  Demo.gender=unique(temp$Demo.gender),
  Demo.lang=unique(temp$Demo.lang),
  Demo.expreadnetvis=unique(temp$Demo.expreadnetvis),
  Stats.OperatingSystemWindows=unique(temp$Stats.OperatingSystemWindows)
  ) %>% 
  cbind(predictAll(mod7, newdata = ., data=temp)) %>% 
  mutate(
        upper95 = qBEINF(.975, mu, sigma, nu, tau),
        lower95 = qBEINF(.025, mu, sigma, nu, tau),
        upper80 = qBEINF(.9, mu, sigma, nu, tau),
        lower80 = qBEINF(.1, mu, sigma, nu, tau),
        upper50 = qBEINF(.75, mu, sigma, nu, tau),
        lower50 = qBEINF(.25, mu, sigma, nu, tau),
        upper10 = qBEINF(.55, mu, sigma, nu, tau),
        lower10 = qBEINF(.45, mu, sigma, nu, tau)
    )
  
# What is predictAll adding?  is it really predicting a new value for ResponsePct? maybe need to take a log of mu?

predictions %>%
    ggplot(aes(x = ResponsePct, y=mu)) +
  geom_bin2d() +
  facet_wrap(~Dataset)
```

``` r
mbi1 <- gamlss(ResponsePct ~ 1, data = temp, family = BEINF)
mbi2<-stepGAIC(mbi1,scope=list(upper=~(Condition + Dataset + DatasetOrder)^2))

summary(mbi2)
mbi2$anova

mBB21<-stepGAIC(mbi2,what="sigma",scope=list(upper=~(Condition + Dataset + DatasetOrder)^2))

mBB22<-stepGAIC(mBB21,what="mu")

mBB22$call

mBB23 <- stepGAIC(mBB22,scope=gamlss.scope(model.frame(Response~Dataset, data=temp)),what="mu")

mBB23$anova

term.plot(mBB23)

mBB23$call
```

#### Least Squares

mod7 gamlss(formula = ResponsePct ~ Dataset + UnderestDummy +
Demo.gender + Demo.lang + Demo.expreadnetvis +
Stats.OperatingSystemWindows + Dataset:UnderestDummy +
Demo.gender:Stats.OperatingSystemWindows + UnderestDummy:Demo.gender +
UnderestDummy:Stats.OperatingSystemWindows, sigma.formula = ~Dataset +
UnderestDummy, family = BEINF, data = temp, trace =
FALSE)

##### Dataset

###### gamlss

``` r
term.plot(mod7, ask=FALSE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-151-1.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-151-2.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-151-3.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-151-4.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-151-5.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-151-6.png)<!-- -->

``` r
#means_m1 <- lpred(mod7, type='response', what='mu', se.fit=T)

means_m1 <- lpred(mod7, type='terms', terms="Dataset", what='mu', se.fit=T)

#mod.t <- lpred(mod7, type = "terms", terms= "Dataset")
#mod.t

#mod.lp <- lp(mod7)
#mod.lp

df_fit <- data.frame(Dataset = temp$Dataset, M = means_m1$fit, SE = means_m1$se.fit)
names(df_fit) <- c("Dataset","M","SE")

#ggplot(df_fit, aes(Dataset, M)) + 
#  geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
#  scale_y_continuous(labels=scales::percent)

# not bad, might work
ggplot(df_fit, aes(Dataset, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7) +
  labs(title="Model Fit for Dataset for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
       y="Fit")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-152-1.png)<!-- -->

``` r
#df_fit.compare <- df_fit %>% tibble::rowid_to_column(., ".iteration") %>% 
#  mutate(.chain=1,
#         Dataset=as.character(Dataset)) %>% 
#  rename(estimate=M) %>%
#  dplyr::select(-SE)

df_fit.compare <- mod7.aug.df %>% 
  dplyr::select(Dataset,.fitted) %>%
  tibble::rowid_to_column(., ".iteration") %>% 
  mutate(.chain=1, Dataset=as.character(Dataset)) %>% 
  rename(estimate=.fitted) 

mod7.pairs <- df_fit.compare %>% compare_levels(estimate, by=Dataset)
```

``` r
library(modelr)

mod7.aug.df %>%
  data_grid(Dataset) %>%
  add_predicted_samples(mod7) %>%
  ggplot(aes(x = pred, y = Dataset)) +
  stat_pointintervalh(.prob = c(.66, .95))
```

###### lsmeans/emmeans

``` r
# any way to get post-hoc analysis? emmeans and lsmeans don't seem to work 
# (lsmeans only works on linear models, emmeans doesn't handle the random effects??)

lsmeansLT(graph.lgclust.beta.cond, test.effs = "Condition")
#Error in lsmeansLT(graph.lgclust.beta.cond, test.effs = "Condition") : The model is not linear mixed effects model

emmeans(graph.lgclust.beta.cond, "Condition")
#Error in `contrasts<-`(`*tmp*`, how.many, value = contr) : 
#  contrasts can be applied only to factors with 2 or more levels

graph.lgclust.beta.cond.norand <- gamlss(Response~Condition, data=temp, family=BEINF) 

emmeans(graph.lgclust.beta.cond.norand, "Condition")
```

###### brms/tidybayes

note: moved code up ; this section never got updated

``` r
# https://mvuorre.github.io/post/2017/bayes-factors-with-brms/

# http://mjskay.github.io/tidybayes/articles/tidy-brms.html#gathering-samples-from-a-fit-in-tidy-format-using-spread_samples

# https://github.com/paul-buerkner/brms/issues/95

# Check exact names of terms
#tidybayes::parameters(fit)

# b_Intercept is the global mean, and sigma is the standard deviation of observations

fit %>%
  gather_samples(b_Intercept, sigma) %>%
  head(10)

# If we want the mean and 95% quantile interval of the parameters, we can apply mean_qi
# We can specify the columns we want to get means and intervals from, as above, or if we omit the list of columns, mean_qi will use every column that is not a grouping column or a special column (one that starts with ., like .chain or .iteration).

fit %>%
  gather_samples(b_Intercept, sigma) %>%
  mean_qi()

# b_Intercept is the global mean, and the individual parameters are offsets from that mean

fit %>%
  gather_samples(b_Intercept, b_Dataset.L, b_Dataset.Q, b_Dataset.C, b_DatasetE4, b_DatasetE5) %>%
  head(10)

m %>%
  spread_samples(`condition_.*`[condition], regex = TRUE) %>%
  head(10)
```

``` r
library(modelr)

graphics_lgclust.2 %>%
  data_grid(Dataset) %>%
  add_fitted_samples(fit) %>%
  ggplot(aes(x = estimate, y = Dataset)) +
  stat_pointintervalh(.prob = c(.66, .95))
```

``` r
# https://github.com/paul-buerkner/brms/issues/95

(hyp2 <- hypothesis(fit, c("Aa2 + Bb2 + Aa2:Bb2 = 0", "Intercept = 0"),
                    alpha = 0.025))
plot(hyp2)
```

``` r
pairs <- fit %>%
  gather_samples(b_Intercept, b_Dataset.L, b_Dataset.Q, b_Dataset.C, b_DatasetE4, b_DatasetE5) %>%
  mutate(contrast=case_when(
    term=="b_Intercept" ~ 1,
    term=="b_Dataset.L" ~ 3,
    term=="b_Dataset.Q" ~ 5,
    term=="b_Dataset.C" ~ 7,
    term=="b_DatasetE4" ~ 8,
    term=="b_DatasetE5" ~ 9
  )) %>%
  compare_levels(estimate, by=contrast)
  
pairs$contrast <- factor(pairs$contrast, 
                         levels=pairs %>% 
                           group_by(contrast) %>% 
                           summarise(avgEst=mean(estimate)) %>%
                           arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% unlist())

m %>%
  spread_samples(b_Intercept, r_condition[condition,]) %>%
  mean_qi(condition_mean = b_Intercept + r_condition)

m %>%
  spread_samples(b_Intercept, r_condition[condition,]) %>%
  mean_qi(condition_mean = b_Intercept + r_condition, .prob = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  geom_pointintervalh() 

ggplot(pairs, aes(y=contrast,x=estimate)) + geom_halfeyeh()
ggplot(pairs, aes(y=contrast,x=estimate)) + geom_halfeyeh() + geom_vline(xintercept=0)
```

``` r
grouped_pairs <- pairs %>% group_by(contrast) %>% summarise(avgEst=mean(estimate)) %>% 
  separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

grouped_pairs %>% arrange(avgEst)
#graph.numlinks.emm.conddata.pairs %>% View()

copy <- grouped_pairs %>% rename(From=To,To=From) %>% 
  mutate(avgEst=-avgEst)

pairs.compl <- bind_rows(grouped_pairs, copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

pairs.compl$From <- factor(pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
pairs.compl %>% arrange(avgEst)

ggplot(pairs.compl) +
  geom_tile(aes(x=To,y=From), color="black") +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(avgEst))) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=avgEst)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(avgEst),color=avgEst)) +
  scale_color_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

###### bayesplot

``` r
# https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html

library(bayesplot)

posterior <- as.array(fit) # in example, model is from stan_glm

mcmc_scatter(x, pars = c("alpha", "sigma"),
                  transform = list(sigma = "log"))

mcmc_hex(x, pars = c("sigma", "alpha"), transform = list(sigma = "log"))

mcmc_intervals(posterior, pars = c("cyl", "drat", "am", "sigma"))

mcmc_violin(posterior, pars = c("wt", "sigma"), probs = c(0.1, 0.5, 0.9))

mcmc_pairs(posterior, pars = c("(Intercept)", "wt", "sigma"),
           off_diag_args = list(size = 1.5))

mcmc_pairs(x, pars = "alpha", regex_pars = "beta\\[[1,4]\\]",
           diag_fun = "dens", off_diag_fun = "hex")
```

###### ggmcmc

``` r
library(ggmcmc)

# http://xavier-fim.net/packages/ggmcmc/
# S is a data frame object with four variables, namely:
# - Iteration Number of iteration.
# - Chain Number of the chain.
# - Parameter Name of the parameter.
# - value value sampled.

ggmcmc(S)

ggs_histogram(S)

ggs_crosscorrelation(S)

L.radon.intercepts <- data.frame(
  Parameter=paste("alpha[", radon$counties$id.county, "]", sep=""),
  Label=radon$counties$County)
head(L.radon.intercepts)

S.full <- ggs(radon$s.radon, par_labels=L.radon.intercepts, family="^alpha")

ggs_caterpillar(S.full)
```

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.bc.emm.data.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*10
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.bc.emm.data.pairs.compl %>% dplyr::select(-contrast) %>% filter(sig.levels != "NS"), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### UnderestDummy

###### gamlss

``` r
term.plot(mod7, ask=FALSE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-165-1.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-165-2.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-165-3.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-165-4.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-165-5.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-165-6.png)<!-- -->

``` r
#means_m1 <- lpred(mod7, type='response', what='mu', se.fit=T)

means_m1 <- lpred(mod7, type='terms', terms="UnderestDummy", what='mu', se.fit=T)

#mod.t <- lpred(mod7, type = "terms", terms= "Dataset")
#mod.t

#mod.lp <- lp(mod7)
#mod.lp

df_fit <- data.frame(UnderestDummy = temp$UnderestDummy, M = means_m1$fit, SE = means_m1$se.fit)
names(df_fit) <- c("UnderestDummy","M","SE")

#ggplot(df_fit, aes(Dataset, M)) + 
#  geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
#  scale_y_continuous(labels=scales::percent)

# not bad, might work
ggplot(df_fit, aes(UnderestDummy, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-166-1.png)<!-- -->

``` r
ggplot(df_fit, aes(UnderestDummy, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7) +
  scale_x_discrete(labels=c("Underestimated", "Correct or Overestimated"), name="Underestimated") +
  labs(title="Model Fit for Underestimated for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
       y="Fit")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-166-2.png)<!-- -->

##### Demo.gender

###### gamlss

``` r
term.plot(mod7, ask=FALSE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-167-1.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-167-2.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-167-3.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-167-4.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-167-5.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-167-6.png)<!-- -->

``` r
#means_m1 <- lpred(mod7, type='response', what='mu', se.fit=T)

means_m1 <- lpred(mod7, type='terms', terms="Demo.gender", what='mu', se.fit=T)

#mod.t <- lpred(mod7, type = "terms", terms= "Dataset")
#mod.t

#mod.lp <- lp(mod7)
#mod.lp

df_fit <- data.frame(Demo.gender = temp$Demo.gender, M = means_m1$fit, SE = means_m1$se.fit)
names(df_fit) <- c("Demo.gender","M","SE")

#ggplot(df_fit, aes(Dataset, M)) + 
#  geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
#  scale_y_continuous(labels=scales::percent)

# not bad, might work
ggplot(df_fit, aes(Demo.gender, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-168-1.png)<!-- -->

``` r
ggplot(df_fit, aes(Demo.gender, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7) +
  #scale_x_discrete(labels=c("Underestimated", "Correct or Overestimated"), name="Underestimated") +
  labs(title="Model Fit for Gender for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
       y="Fit")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-168-2.png)<!-- -->

##### Demo.lang

###### gamlss

``` r
term.plot(mod7, ask=FALSE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-169-1.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-169-2.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-169-3.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-169-4.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-169-5.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-169-6.png)<!-- -->

``` r
#means_m1 <- lpred(mod7, type='response', what='mu', se.fit=T)

means_m1 <- lpred(mod7, type='terms', terms="Demo.lang", what='mu', se.fit=T)

#mod.t <- lpred(mod7, type = "terms", terms= "Dataset")
#mod.t

#mod.lp <- lp(mod7)
#mod.lp

df_fit <- data.frame(Demo.lang = temp$Demo.lang, M = means_m1$fit, SE = means_m1$se.fit)
names(df_fit) <- c("Demo.lang","M","SE")

#ggplot(df_fit, aes(Dataset, M)) + 
#  geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
#  scale_y_continuous(labels=scales::percent)

# not bad, might work
ggplot(df_fit, aes(Demo.lang, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-170-1.png)<!-- -->

``` r
ggplot(df_fit, aes(Demo.lang, M)) + 
    geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
    geom_point(size=7) +
    #scale_x_discrete(labels=c("Underestimated", "Correct or Overestimated"), name="Underestimated") +
    labs(title="Model Fit for Language for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
         y="Fit", x="Primary Language Spoken at Home")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-170-2.png)<!-- -->

##### Demo.expreadnetvis

###### gamlss

``` r
term.plot(mod7, ask=FALSE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-171-1.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-171-2.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-171-3.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-171-4.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-171-5.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-171-6.png)<!-- -->

``` r
#means_m1 <- lpred(mod7, type='response', what='mu', se.fit=T)

means_m1 <- lpred(mod7, type='terms', terms="Demo.expreadnetvis", what='mu', se.fit=T)

#mod.t <- lpred(mod7, type = "terms", terms= "Dataset")
#mod.t

#mod.lp <- lp(mod7)
#mod.lp

df_fit <- data.frame(Demo.expreadnetvis = temp$Demo.expreadnetvis, M = means_m1$fit, SE = means_m1$se.fit)
names(df_fit) <- c("Demo.expreadnetvis","M","SE")

#ggplot(df_fit, aes(Dataset, M)) + 
#  geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
#  scale_y_continuous(labels=scales::percent)

# not bad, might work
ggplot(df_fit, aes(Demo.expreadnetvis, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-172-1.png)<!-- -->

``` r
ggplot(df_fit, aes(Demo.expreadnetvis, M)) + 
    geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
    geom_point(size=7) +
    #scale_x_discrete(labels=c("Underestimated", "Correct or Overestimated"), name="Underestimated") +
    labs(title="Model Fit for Experience Reading Network Visualizations for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
         y="Fit", x="Experience Reading Network Visualizations")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-172-2.png)<!-- -->

##### Stats.OperatingSystemWindows

###### gamlss

``` r
term.plot(mod7, ask=FALSE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-173-1.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-173-2.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-173-3.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-173-4.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-173-5.png)<!-- -->![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-173-6.png)<!-- -->

``` r
#means_m1 <- lpred(mod7, type='response', what='mu', se.fit=T)

means_m1 <- lpred(mod7, type='terms', terms="Stats.OperatingSystemWindows", what='mu', se.fit=T)

#mod.t <- lpred(mod7, type = "terms", terms= "Dataset")
#mod.t

#mod.lp <- lp(mod7)
#mod.lp

df_fit <- data.frame(Stats.OperatingSystemWindows = temp$Stats.OperatingSystemWindows, M = means_m1$fit, SE = means_m1$se.fit)
names(df_fit) <- c("Stats.OperatingSystemWindows","M","SE")

#ggplot(df_fit, aes(Dataset, M)) + 
#  geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
#  scale_y_continuous(labels=scales::percent)

# not bad, might work
ggplot(df_fit, aes(Stats.OperatingSystemWindows, M)) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
  geom_point(size=7)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-174-1.png)<!-- -->

``` r
ggplot(df_fit, aes(Stats.OperatingSystemWindows, M)) + 
    geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.2) + 
    geom_point(size=7) +
    #scale_x_discrete(labels=c("Underestimated", "Correct or Overestimated"), name="Underestimated") +
    labs(title="Model Fit for Operating Systems for Percentage of Nodes\nin Largest Cluster task, graphics conditions",
         y="Fit", x="Operating Systems")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-174-2.png)<!-- -->

##### Dataset:UnderestDummy

###### gamlss

``` r
plot2way(mod7,c("Dataset","UnderestDummy"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-175-1.png)<!-- -->

``` r
plot2way(mod7,c("UnderestDummy","Dataset"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-175-2.png)<!-- -->

``` r
means_m1 <- data.frame(lpred(mod7, type='terms', terms=c("Dataset","UnderestDummy"), what='mu', se.fit=T)) %>%
  unite(data.comb, fit.Dataset, se.fit.Dataset) %>% 
  unite(under.comb, fit.UnderestDummy, se.fit.UnderestDummy) %>%
  bind_cols(temp %>% dplyr::select(Dataset, UnderestDummy),.) %>%
  gather(key=term,value=united, data.comb, under.comb) %>%
  separate(united,into=c("fit","se.fit"), sep="[_]",convert=TRUE) %>%
  mutate(term=case_when(term=="data.comb" ~ "Dataset",
                        term=="under.comb" ~ "UnderestDummy"))

#means_m1

means_m1 <- data.frame(lpred(mod7, type='terms', terms=c("Dataset","UnderestDummy"), what='mu', se.fit=T)) %>%
  unite(data.comb, fit.Dataset, se.fit.Dataset) %>% 
  unite(under.comb, fit.UnderestDummy, se.fit.UnderestDummy) %>%
  bind_cols(temp %>% dplyr::select(Dataset, UnderestDummy),.) %>%
  gather(key=term,value=united, data.comb, under.comb) %>%
  separate(united,into=c("fit","se.fit"), sep="[_]",convert=TRUE) %>%
  mutate(term=case_when(term=="data.comb" ~ "Dataset",
                        term=="under.comb" ~ "UnderestDummy")) %>%
  group_by(Dataset, UnderestDummy,term) %>%
  summarize(meanFit=mean(fit),
            meanSE=mean(se.fit))

ggplot(means_m1, aes(Dataset, meanFit, color=term)) + 
  geom_errorbar(aes(ymin=meanFit-meanSE, ymax=meanFit+meanSE), width=.2) + 
  geom_point(size=7) +
  facet_grid(.~term)
```

##### Demo.gender:Stats.OperatingSystemWindows

``` r
plot2way(mod7,c("Demo.gender","Stats.OperatingSystemWindows"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-177-1.png)<!-- -->

``` r
plot2way(mod7,c("Stats.OperatingSystemWindows","Demo.gender"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-177-2.png)<!-- -->

##### UnderestDummy:Demo.gender

``` r
plot2way(mod7,c("UnderestDummy","Demo.gender"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-178-1.png)<!-- -->

``` r
plot2way(mod7,c("Demo.gender","UnderestDummy"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-178-2.png)<!-- -->

##### UnderestDummy:Stats.OperatingSystemWindows

``` r
plot2way(mod7,c("UnderestDummy","Stats.OperatingSystemWindows"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-179-1.png)<!-- -->

``` r
plot2way(mod7,c("Stats.OperatingSystemWindows","UnderestDummy"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-179-2.png)<!-- -->

### Number of Clusters

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_numclust <- readRDS(file.path(analysisDataDir, "GraphicsNumClust.rds"))
```

#### lme4

``` r
# Individual fixed effects

graph.numclust.lmer.cond <- lmer(LogError ~ Condition + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.cond) # Color significantly different from Control

anova(graph.numclust.lmer.cond)

# Condition is significant (p=5.234e-07)

graph.numclust.lmer.phr <- lmer(LogError ~ ConditionPhrasing + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.phr)

anova(graph.numclust.lmer.phr)

# ConditionPhrasing is significant (p=0.003226)

graph.numclust.lmer.gr <- lmer(LogError ~ ConditionGraphics + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.gr)

anova(graph.numclust.lmer.gr)

# ConditionGraphics is significant (p=1.653e-07)

graph.numclust.lmer.col <- lmer(LogError ~ ConditionColor + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.col)

anova(graph.numclust.lmer.col)

# ConditionColor is significant (p=2.897e-08)

graph.numclust.lmer <- lmer(LogError ~ Ctrl_dummy + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Ctrl_dummy not significant; trying Dataset

graph.numclust.lmer.dataset <- lmer(LogError ~ Dataset + (1|Demo.ResponseID), data = graphics_numclust, REML = T)

lmsum <- summary(graph.numclust.lmer.dataset)
lmsum
#names(lmsum)

anova(graph.numclust.lmer.dataset)

# Dataset is significant (p < 2.2e-16); trying QuestionOrder

graph.numclust.lmer <- lmer(LogError ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# QuestionOrder is not significant; trying DatasetOrder

graph.numclust.lmer <- lmer(LogError ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# DatasetOrder is barely not significant (p=0.0684); trying DatasetDuration

graph.numclust.lmer <- lmer(LogError ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# DatasetDuration is not significant; trying DatasetStartTime

graph.numclust.lmer <- lmer(LogError ~ DatasetStartTime + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# DatasetStartTime is not significant; trying TaskOrder

graph.numclust.lmer <- lmer(LogError ~ TaskOrder + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# TaskOrder is not significant; trying CorrectAnswer

graph.numclust.lmer.correct <- lmer(LogError ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.correct)

anova(graph.numclust.lmer.correct)

# CorrectAnswer is highly significant (p = 5.251e-07); trying Underestimated

graph.numclust.lmer.underest <- lmer(LogError ~ Underestimated + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.underest)

anova(graph.numclust.lmer.underest)

# Underestimated is highly significant (p < 2.2e-16)

graph.numclust.lmer.overest <- lmer(LogError ~ Overestimated + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.overest)

anova(graph.numclust.lmer.overest)

# Underestimated is highly significant (p < 2.2e-16)

graph.numclust.lmer <- lmer(LogError ~ ClustConf + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# ClustConf is not significant

graph.numclust.lmer <- lmer(LogError ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.Q_TotalDuration is not significant; trying Stats.dataset_count

graph.numclust.lmer <- lmer(LogError ~ Stats.dataset_count + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.dataset_count is not significant; trying Stats.OperatingSystem

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

# Various significant categories:
# *: Android 6.0.1, CrOS x86_64 9592.96.0, Linux x86_64, Ubuntu, Windows NT 10.0
#    Windows NT 5.1, Windows NT 6.0, Windows NT 6.1

anova(graph.numclust.lmer)

# Stats.OperatingSystem is not quite significant; trying Stats.OperatingSystemCombined

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemCombined + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer) # no significant categories

anova(graph.numclust.lmer)

# Stats.OperatingSystemCombined is not significant; trying Stats.OperatingSystemCombined2

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemCombined2 + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.OperatingSystemCombined2 is not significant; trying Stats.OperatingSystemCombined3

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemCombined3 + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer) # Windows almost significantly different from Macintosh (0.0888)

anova(graph.numclust.lmer)

# Stats.OperatingSystemCombined3 is not significant; trying Stats.OperatingSystemCombined4

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemCombined4 + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.OperatingSystemCombined4 is not significant; trying Stats.OperatingSystemCombined5

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemCombined5 + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.OperatingSystemCombined5 is not significant; trying Stats.OperatingSystemWindows

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemWindows + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.OperatingSystemWindows is not significant; trying Stats.OperatingSystemMacintosh

graph.numclust.lmer <- lmer(LogError ~ Stats.OperatingSystemMacintosh + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Stats.OperatingSystemMacintosh is not significant

graph.numclust.lmer.OShighsig <- lmer(LogError ~ Stats.OperatingSystemNumClust + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.OShighsig)

anova(graph.numclust.lmer.OShighsig)

# Stats.OperatingSystemNumClust is significant (p=0.0002445); trying StatsNumPixels

graph.numclust.lmer.pixels <- lmer(LogError ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.pixels)

anova(graph.numclust.lmer.pixels)

# StatsNumPixels is significant (p=0.02508); trying Demo.age

graph.numclust.lmer <- lmer(LogError ~ Demo.age + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.age))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.age is not significant; trying Demo.gender

graph.numclust.lmer <- lmer(LogError ~ Demo.gender + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.gender))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.gender is not significant; trying Demo.lang

graph.numclust.lmer <- lmer(LogError ~ Demo.lang + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.lang))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.lang is not significant; trying Demo.educ

graph.numclust.lmer <- lmer(LogError ~ Demo.educ + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.educ))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.educ is not significant; trying Demo.acfield

graph.numclust.lmer <- lmer(LogError ~ Demo.acfield + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.acfield))), REML = T)

summary(graph.numclust.lmer) # no significant categories

anova(graph.numclust.lmer)

# Demo.acfield is not significant; trying Demo.acfieldGrouped

graph.numclust.lmer <- lmer(LogError ~ Demo.acfieldGrouped + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.acfieldGrouped))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.acfieldGrouped is not significant; trying Demo.dailytech_Computer

graph.numclust.lmer <- lmer(LogError ~ Demo.dailytech_Computer + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.dailytech_Computer))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.dailytech_Computer is not quite significant (p=0.0655); trying Demo.dailytech_Tablet

graph.numclust.lmer <- lmer(LogError ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.dailytech_Tablet))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.dailytech_Tablet is not significant; trying Demo.dailytech_SmartPhone

graph.numclust.lmer.SP <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.dailytech_SmartPhone))), REML = T)

summary(graph.numclust.lmer.SP)

anova(graph.numclust.lmer.SP)

# Demo.dailytech_SmartPhone is significant (p=0.01925); trying Demo.weeklygaming

graph.numclust.lmer <- lmer(LogError ~ Demo.weeklygaming + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.weeklygaming))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.weeklygaming is not significant; trying Demo.expdataanal

graph.numclust.lmer <- lmer(LogError ~ Demo.expdataanal + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expdataanal))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.expdataanal is not significant; trying Demo.expdatavis

graph.numclust.lmer <- lmer(LogError ~ Demo.expdatavis + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expdatavis))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.expdatavis is not significant; trying Demo.expreadnetvis

graph.numclust.lmer.ERNV <- lmer(LogError ~ Demo.expreadnetvis + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expreadnetvis))), REML = T)

summary(graph.numclust.lmer.ERNV) # several levels significant

anova(graph.numclust.lmer.ERNV)

# Demo.expreadnetvis is significant (p=0.000261); trying Demo.expreadnetvis.alot

graph.numclust.lmer.ERNVAL <- lmer(LogError ~ Demo.expreadnetvis.alot + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expreadnetvis.alot))), REML = T)

summary(graph.numclust.lmer.ERNVAL)

anova(graph.numclust.lmer.ERNVAL)

# Demo.expreadnetvis.alot is significant (p=4.177e-06)

graph.numclust.lmer <- lmer(LogError ~ Demo.expreadnetvis.none + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expreadnetvis.none))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.expreadnetvis.none is not significant

graph.numclust.lmer <- lmer(LogError ~ Demo.expreadnetvis.three + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expreadnetvis.three))), REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# Demo.expreadnetvis.three is not significant

graph.numclust.lmer.ECNV <- lmer(LogError ~ Demo.expcreatenetvis + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expcreatenetvis))), REML = T)

summary(graph.numclust.lmer.ECNV) # a couple significant differences

anova(graph.numclust.lmer.ECNV)

# Demo.expcreatenetvis is marginally significant (p=0.02167); trying Demo.expcreatenetvis.alot

graph.numclust.lmer.ECNVAL <- lmer(LogError ~ Demo.expcreatenetvis.alot + (1|Demo.ResponseID), data=graphics_numclust %>% filter(!(is.na(Demo.expcreatenetvis.alot))), REML = T)

summary(graph.numclust.lmer.ECNVAL)

anova(graph.numclust.lmer.ECNVAL)

# Demo.expcreatenetvis.alot is significant (p=0.002181); trying AvgDeg

graph.numclust.lmer.avgdeg <- lmer(LogError ~ AvgDeg + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.avgdeg)

anova(graph.numclust.lmer.avgdeg)

# AvgDeg is significant (p=2.351e-10); trying Density

graph.numclust.lmer.dens <- lmer(LogError ~ Density + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.dens)

anova(graph.numclust.lmer.dens)

# Density is significant (p=7.012e-08); trying LargeClust1

graph.numclust.lmer.lgclust <- lmer(LogError ~ LargeClust1 + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.lgclust)

anova(graph.numclust.lmer.lgclust)

# LargeClust1 is significant (p=6.648e-09); trying Modularity

graph.numclust.lmer.mod <- lmer(LogError ~ Modularity + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.mod)

anova(graph.numclust.lmer.mod)

# Modularity is significant (p=6.408e-05); trying NumClust

graph.numclust.lmer.numclust <- lmer(LogError ~ NumClust + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.numclust)

anova(graph.numclust.lmer.numclust)

# NumClust is significant (p=5.251e-07); trying NumHighDegree

graph.numclust.lmer <- lmer(LogError ~ NumHighDegree + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# NumHighDegree is not significant; trying NumLinks

graph.numclust.lmer <- lmer(LogError ~ NumLinks + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# NumLinks is not quite significant; trying NumNodes

graph.numclust.lmer.numnodes <- lmer(LogError ~ NumNodes + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer.numnodes)

anova(graph.numclust.lmer.numnodes)

# NumNodes is significant (p=0.0009575); trying NumNodesClust1

graph.numclust.lmer <- lmer(LogError ~ NumNodesClust1 + (1|Demo.ResponseID), data=graphics_numclust, REML = T)

summary(graph.numclust.lmer)

anova(graph.numclust.lmer)

# NumNodesClust1 is not significant 
```

``` r
temp <- graphics_numclust %>% dplyr::select(Demo.ResponseID, LogError, Condition, ConditionPhrasing, ConditionGraphics, ConditionColor, Dataset, CorrectAnswer, Underestimated, Overestimated, Stats.OperatingSystemNumClust, StatsNumPixels, Demo.dailytech_SmartPhone, Demo.expreadnetvis, Demo.expreadnetvis.alot, Demo.expcreatenetvis, Demo.expcreatenetvis.alot, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumNodes) %>% drop_na()

#temp <- temp%>% mutate(StatsNumPixels=scale(StatsNumPixels))
```

Run this one

``` r
# combining individual predictors

graph.numclust.lmer.full <- lmer(LogError ~ ConditionColor + Dataset + 
                                   Overestimated + 
                                   Stats.OperatingSystemNumClust + 
                                   Demo.dailytech_SmartPhone +
                                   Demo.expreadnetvis +
                                   (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full)

anova(graph.numclust.lmer.full)
```

Run this one

``` r
# adding interactions

graph.numclust.lmer.full.int <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       Demo.expreadnetvis +
                                       ConditionColor:Dataset +
                                       (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full.int)

anova(graph.numclust.lmer.full.int)




graph.numclust.lmer.full.int.2 <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       Demo.expreadnetvis +
                                       ConditionColor:Dataset +
                                       #Dataset:Overestimated + 
                                        Dataset:Demo.expreadnetvis + 
                                       (1|Demo.ResponseID), data=temp, REML = T)

# Note: Dataset5CorrectUnder is diff from Dataset1CorrectUnder?

summary(graph.numclust.lmer.full.int.2)

anova(graph.numclust.lmer.full.int.2)

anova(graph.numclust.lmer.full.int.2,graph.numclust.lmer.full.int)

# keep int.2

graph.numclust.lmer.full.int.3 <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       Demo.expreadnetvis +
                                       ConditionColor:Dataset +
                                       Dataset:Overestimated + 
                                        Dataset:Demo.expreadnetvis + 
                                       (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full.int.3)

anova(graph.numclust.lmer.full.int.3)

anova(graph.numclust.lmer.full.int.2,graph.numclust.lmer.full.int.3)

# barely significant, but can keep int.3

graph.numclust.lmer.full.int.4 <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       Demo.expreadnetvis +
                                       ConditionColor:Dataset +
                                       Overestimated:Stats.OperatingSystemNumClust + 
                                       (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full.int.4)

anova(graph.numclust.lmer.full.int.4)

anova(graph.numclust.lmer.full.int.3,graph.numclust.lmer.full.int.4)

# keep int.3

graph.numclust.lmer.full.int.5 <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       Demo.expreadnetvis +
                                       ConditionColor:Dataset +
                                       Overestimated:Stats.OperatingSystemNumClust + 
                                         Overestimated:Demo.expreadnetvis +
                                       (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full.int.5)

anova(graph.numclust.lmer.full.int.5)

anova(graph.numclust.lmer.full.int.3,graph.numclust.lmer.full.int.5)

# keep int.5

graph.numclust.lmer.full.int.6 <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       Demo.expreadnetvis +
                                       ConditionColor:Dataset +
                                       Overestimated:Stats.OperatingSystemNumClust + 
                                         Overestimated:Demo.expreadnetvis +
                                         Stats.OperatingSystemNumClust:Demo.expreadnetvis +
                                       (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full.int.6)

anova(graph.numclust.lmer.full.int.6)

anova(graph.numclust.lmer.full.int.5,graph.numclust.lmer.full.int.6)

# keep int.6

graph.numclust.lmer.full.int.7 <- lmer(LogError ~ ConditionColor + Dataset + 
                                       Overestimated + 
                                       Stats.OperatingSystemNumClust + 
                                       ConditionColor:Dataset +
                                       Overestimated:Stats.OperatingSystemNumClust + 
                                      (1|Demo.ResponseID), data=temp, REML = T)

summary(graph.numclust.lmer.full.int.7)

anova(graph.numclust.lmer.full.int.7)

anova(graph.numclust.lmer.full.int.7, graph.numclust.lmer.full.int.6)
# 6 is better, but need to avoid Demo.expreadnetvis
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.numclust.lmer.full.int.7, 
     file = file.path(analysisDataDir,"fits/graph_numclust_lmer_int.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_numclust_lmer_int.RData"))
```

``` r
rand(graph.numclust.lmer.full.int.7)
```

    ## Analysis of Random effects Table:
    ##                 Chi.sq Chi.DF p.value    
    ## Demo.ResponseID   42.2      1   8e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# result shows that random effects of participant are significant (p=8e-11)

#ranef(graph.avgdeg.lmer.SP)

# displays the random effects; not that useful

# unlike lme(), lmer() doesn't allow for heterogeneous error variance structures (the "weights")

(r2nsj = r2beta(graph.numclust.lmer.full.int.7, method = 'nsj', partial = TRUE))[1,'Rsq']
```

    ## [1] 0.2780631

``` r
ggplot(graphics_numclust) + geom_histogram(aes(LogError), binwidth=.005) + labs(title="Distribution of LogError values for Number of Clusters\ntask, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-189-1.png)<!-- -->

``` r
ggsave(file.path(figureDir, "logerrornumclustgraphics.pdf"), width=5, height=3)
```

``` r
plot(graph.numclust.lmer.full.int.7)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-190-1.png)<!-- -->

``` r
plot(graph.numclust.lmer.full.int.7, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-190-2.png)<!-- -->

``` r
plot(graph.numclust.lmer.full.int.7, resid(.) ~ fitted(.) | ConditionColor, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-190-3.png)<!-- -->

``` r
plot(graph.numclust.lmer.full.int.7, resid(., scaled=TRUE) ~ fitted(.) | Dataset, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-190-4.png)<!-- -->

``` r
plot(graph.numclust.lmer.full.int.7, LogError ~ fitted(.), abline = c(0,1))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-190-5.png)<!-- -->

``` r
graph.numclust.lmer.full.int.7.f <- fortify(graph.numclust.lmer.full.int.7)

ggplot(graph.numclust.lmer.full.int.7.f, aes(.fitted,.resid)) + 
  geom_point() +
  #facet_grid(.~Sex) + 
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-191-1.png)<!-- -->

``` r
ggplot(graph.numclust.lmer.full.int.7.f, aes(.fitted,LogError)) + 
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-191-2.png)<!-- -->

``` r
ggplot(graph.numclust.lmer.full.int.7.f, aes(LogError,.fitted)) +
geom_point() +
geom_abline(aes(slope = 1, intercept = 0)) +
scale_y_continuous(limits=c(0,0.3)) +
labs(title="Real vs. Predicted LogError values for Number of\nClusters task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-191-3.png)<!-- -->

``` r
ggsave(file.path(figureDir, "fittedpredictnumclustgraphics.pdf"), width=5, height=3)
```

``` r
# TO DO: check out interpretation for these plots??

library(lattice)

prof <-  profile(graph.numclust.lmer.full.int.6, optimizer="Nelder_Mead", which="beta_")

prof.CI <- confint(prof)

#CI2 <- confint(graph.avgdeg.lmer.SP, maxpts = 8)

xyplot(prof)

xyplot(prof, absVal = TRUE)

xyplot(prof, conf = c(0.95, 0.99), main = "95% and 99% profile() intervals")

# can also apply logProf() and varianceProf() to profile object

densityplot(prof)

splom(prof)
```

#### Least Squares Means

Do for each categorical predictor. Final model
(graph.numclust.lmer.full.int.7): LogError ~ ConditionColor + Dataset +
Overestimated + Stats.OperatingSystemNumClust + ConditionColor:Dataset +
Overestimated:Stats.OperatingSystemNumClust + (1|Demo.ResponseID)

##### ConditionColor

###### lsmeans (old)

``` r
# doesn't make sense for continuous predictor???

lsm.condition <- lsmeansLT(graph.numclust.lmer.full.int.6, test.effs = "Condition")

plot(lsm.condition)

lsm.condition.df <- as_data_frame(lsm.condition$lsmeans.table)

lsm.condition.df

lsm.task.df$Task <- factor(lsm.task.df$Task, levels=lsm.task.df %>% arrange(desc(Estimate)) %>% select(Task) %>% unlist())

lsm.task.df %>% arrange(desc(Estimate))


ggplot(lsm.task.df) +
  geom_point(aes(x=Task,y=Estimate, color=`p-value`<.01)) +
  geom_errorbar(aes(x=Task,ymax=`Upper CI`,ymin=`Lower CI`), width=.2) +
  coord_flip()

# TO DO: add a color scale so TRUE/FALSE values are always same color across all plots
```

``` r
# old code, used lmertest, but that is deprecated
# doesn't make sense for continuous predictor???

difflsm.task <- difflsmeans(graph.numclust.lmer.full.int.6, test.effs = "ConditionColor")

plot(difflsm.task)

difflsm.task.df <- as_data_frame(difflsm.task$diffs.lsmeans.table)

difflsm.task.df

difflsm.task.df <- difflsm.task.df %>% mutate(Pair=rownames(.)) %>% separate(Pair, c("del","Pair"), sep=5) %>% select(-del) %>% separate(Pair, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% select(-del)

difflsm.task.df$Pair <- factor(difflsm.task.df$Pair, levels=difflsm.task.df %>% arrange(desc(Estimate)) %>% select(Pair) %>% unlist())

difflsm.task.df %>% arrange(desc(Estimate))

ggplot(difflsm.task.df) +
  geom_point(aes(x=Pair,y=Estimate, color=`p-value`<.01)) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_hline(aes(yintercept=0)) +
  coord_flip()

ggplot(difflsm.task.df) +
  geom_tile(aes(x=To,y=From,fill=Estimate)) +
    scale_fill_distiller(type="div", palette=4)

ggplot(difflsm.task.df) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=`p-value`<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black"))
```

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numclust.emm.cond <- emmeans(graph.numclust.lmer.full.int.7, "ConditionColor", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

#ref_grid(graph.numclust.lmer.full.int.7) @ grid

graph.numclust.emm.cond
```

    ##  ConditionColor     emmean          SE     df   lower.CL   upper.CL
    ##  1-Color        0.05958217 0.003275461 396.91 0.05314274 0.06602159
    ##  2-Other        0.03931209 0.002362000 418.28 0.03466922 0.04395496
    ## 
    ## Results are averaged over the levels of: Dataset, Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numclust.emm.cond.df <- dplyr::as_data_frame(graph.numclust.emm.cond)

graph.numclust.emm.cond.df
```

    ## # A tibble: 2 x 6
    ##   ConditionColor emmean      SE    df lower.CL upper.CL
    ## * <fct>           <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1-Color        0.0596 0.00328   397   0.0531   0.0660
    ## 2 2-Other        0.0393 0.00236   418   0.0347   0.0440

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numclust.emm.cond.cld <- cld(graph.numclust.emm.cond,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numclust.emm.cond.cld.df <- graph.numclust.emm.cond.cld$emmeans

graph.numclust.emm.cond.cld.df %>% dplyr::select(ConditionColor,.group)
```

    ##  ConditionColor .group
    ##  2-Other         1    
    ##  1-Color          2   
    ## 
    ## Results are averaged over the levels of: Dataset, Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.cond.cld.df
```

    ##  ConditionColor     emmean          SE     df   lower.CL   upper.CL .group
    ##  2-Other        0.03931209 0.002362000 418.28 0.03466922 0.04395496  1    
    ##  1-Color        0.05958217 0.003275461 396.91 0.05314274 0.06602159   2   
    ## 
    ## Results are averaged over the levels of: Dataset, Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.cond.cld.df$ConditionColor <- factor(graph.numclust.emm.cond.cld.df$ConditionColor, levels=graph.numclust.emm.cond.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(ConditionColor) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numclust.emm.cond, ~ConditionColor, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-195-1.png)<!-- -->

``` r
plot(graph.numclust.emm.cond)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-195-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numclust.emm.cond.cld.df %>% arrange(desc(emmean))
```

    ##   ConditionColor     emmean          SE       df   lower.CL   upper.CL
    ## 1        1-Color 0.05958217 0.003275461 396.9094 0.05314274 0.06602159
    ## 2        2-Other 0.03931209 0.002362000 418.2837 0.03466922 0.04395496
    ##   .group
    ## 1      2
    ## 2     1

``` r
ggplot(graph.numclust.emm.cond.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=ConditionColor,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=ConditionColor,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(labels=c("Blue","Black")) +
  coord_flip() +
  labs(title="Estimated Marginal Means for Node Color for Number of Clusters task,\ngraphics conditions",
       x="Node Color")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-195-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numclust.emm.cond.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.cond)) 
graph.numclust.emm.cond.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast          estimate      SE    df t.ratio      p.value
    ## * <fct>                <dbl>   <dbl> <dbl>   <dbl>        <dbl>
    ## 1 1-Color - 2-Other   0.0203 0.00347   381    5.85 0.0000000107

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numclust.emm.cond))

graph.numclust.emm.cond.pairs <- full_join(graph.numclust.emm.cond.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numclust.emm.cond))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-196-1.png)<!-- -->

``` r
plot(pairs(graph.numclust.emm.cond), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-196-2.png)<!-- -->

``` r
graph.numclust.emm.cond.pairs$sig.levels <- 
  case_when(graph.numclust.emm.cond.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numclust.emm.cond.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numclust.emm.cond.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numclust.emm.cond.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numclust.emm.cond.pairs$sig.levels <- factor(graph.numclust.emm.cond.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numclust.emm.cond.pairs$contrast <- factor(graph.numclust.emm.cond.pairs$contrast, levels=graph.numclust.emm.cond.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numclust.emm.cond.pairs <- graph.numclust.emm.cond.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numclust.emm.cond.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast   From   To    estimate      SE    df t.ratio  p.value lower.CL
    ##   <fct>      <chr>  <chr>    <dbl>   <dbl> <dbl>   <dbl>    <dbl>    <dbl>
    ## 1 1-Color -… 1-Col… 2-Ot…   0.0203 0.00347   381    5.85  1.07e⁻⁸   0.0135
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-196-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-196-4.png)<!-- -->

``` r
copy <- graph.numclust.emm.cond.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numclust.emm.cond.pairs.compl <- bind_rows(graph.numclust.emm.cond.pairs,copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numclust.emm.cond.pairs.compl$From <- factor(graph.numclust.emm.cond.pairs.compl$From, 
                                                   levels=rev(unique(graph.numclust.emm.cond.pairs.compl$From)))
#graph.numclust.emm.cond.pairs.compl$To <- factor(graph.numclust.emm.cond.pairs.compl$To, levels=cond.lev)

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numclust.emm.cond.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast   From   To    estimate      SE    df t.ratio  p.value lower.CL
    ##   <fct>      <fct>  <chr>    <dbl>   <dbl> <dbl>   <dbl>    <dbl>    <dbl>
    ## 1 1-Color -… 2-Oth… 1-Co…  -0.0203 0.00347   381   -5.85  1.07e⁻⁸  -0.0135
    ## 2 1-Color -… 1-Col… 2-Ot…   0.0203 0.00347   381    5.85  1.07e⁻⁸   0.0135
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-197-1.png)<!-- -->

``` r
ggplot(graph.numclust.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-197-2.png)<!-- -->

``` r
ggplot(graph.numclust.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-197-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-197-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.numclust.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*50
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.numclust.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

##### Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numclust.emm.data <- emmeans(graph.numclust.lmer.full.int.7, "Dataset", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numclust.emm.data
```

    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.03170927 0.003709614 1093.85 0.02443051 0.03898803
    ##  3       0.03753965 0.003761116 1099.81 0.03015988 0.04491943
    ##  5       0.07204236 0.003797485 1093.92 0.06459118 0.07949354
    ##  7       0.03403456 0.003864820 1094.88 0.02645127 0.04161785
    ##  8       0.05761650 0.003827722 1100.37 0.05010604 0.06512695
    ##  9       0.06374042 0.003769987 1096.85 0.05634322 0.07113762
    ## 
    ## Results are averaged over the levels of: ConditionColor, Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numclust.emm.data.df <- dplyr::as_data_frame(graph.numclust.emm.data)

graph.numclust.emm.data.df
```

    ## # A tibble: 6 x 6
    ##   Dataset emmean      SE    df lower.CL upper.CL
    ## * <fct>    <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1       0.0317 0.00371  1094   0.0244   0.0390
    ## 2 3       0.0375 0.00376  1100   0.0302   0.0449
    ## 3 5       0.0720 0.00380  1094   0.0646   0.0795
    ## 4 7       0.0340 0.00386  1095   0.0265   0.0416
    ## 5 8       0.0576 0.00383  1100   0.0501   0.0651
    ## 6 9       0.0637 0.00377  1097   0.0563   0.0711

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numclust.emm.data.cld <- cld(graph.numclust.emm.data,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numclust.emm.data.cld.df <- graph.numclust.emm.data.cld$emmeans

graph.numclust.emm.data.cld.df %>% dplyr::select(Dataset,.group)
```

    ##  Dataset .group
    ##  1        1    
    ##  7        1    
    ##  3        1    
    ##  8         2   
    ##  9         23  
    ##  5          3  
    ## 
    ## Results are averaged over the levels of: ConditionColor, Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.data.cld.df
```

    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  1       0.03170927 0.003709614 1093.85 0.02443051 0.03898803  1    
    ##  7       0.03403456 0.003864820 1094.88 0.02645127 0.04161785  1    
    ##  3       0.03753965 0.003761116 1099.81 0.03015988 0.04491943  1    
    ##  8       0.05761650 0.003827722 1100.37 0.05010604 0.06512695   2   
    ##  9       0.06374042 0.003769987 1096.85 0.05634322 0.07113762   23  
    ##  5       0.07204236 0.003797485 1093.92 0.06459118 0.07949354    3  
    ## 
    ## Results are averaged over the levels of: ConditionColor, Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.data.cld.df$Dataset <- factor(graph.numclust.emm.data.cld.df$Dataset, levels=graph.numclust.emm.data.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Dataset) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numclust.emm.data, ~Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-200-1.png)<!-- -->

``` r
plot(graph.numclust.emm.data)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-200-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numclust.emm.data.cld.df %>% arrange(desc(emmean))
```

    ##   Dataset     emmean          SE       df   lower.CL   upper.CL .group
    ## 1       5 0.07204236 0.003797485 1093.922 0.06459118 0.07949354      3
    ## 2       9 0.06374042 0.003769987 1096.852 0.05634322 0.07113762     23
    ## 3       8 0.05761650 0.003827722 1100.370 0.05010604 0.06512695     2 
    ## 4       3 0.03753965 0.003761116 1099.807 0.03015988 0.04491943    1  
    ## 5       7 0.03403456 0.003864820 1094.882 0.02645127 0.04161785    1  
    ## 6       1 0.03170927 0.003709614 1093.845 0.02443051 0.03898803    1

``` r
ggplot(graph.numclust.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Dataset for Number of Clusters task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-200-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numclust.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.data)) 
graph.numclust.emm.data.pairs
```

    ## # A tibble: 15 x 6
    ##    contrast estimate      SE    df t.ratio          p.value
    ##  * <fct>       <dbl>   <dbl> <dbl>   <dbl>            <dbl>
    ##  1 1 - 3    -0.00583 0.00467   964  -1.25  0.813           
    ##  2 1 - 5    -0.0403  0.00466   946  -8.65  0               
    ##  3 1 - 7    -0.00233 0.00470   935  -0.495 0.996           
    ##  4 1 - 8    -0.0259  0.00472   956  -5.49  0.000000750     
    ##  5 1 - 9    -0.0320  0.00463   936  -6.92  0.000000000124  
    ##  6 3 - 5    -0.0345  0.00467   929  -7.39  0.00000000000492
    ##  7 3 - 7     0.00351 0.00477   969   0.736 0.977           
    ##  8 3 - 8    -0.0201  0.00472   936  -4.25  0.000338        
    ##  9 3 - 9    -0.0262  0.00470   974  -5.57  0.000000484     
    ## 10 5 - 7     0.0380  0.00470   938   8.09  0               
    ## 11 5 - 8     0.0144  0.00472   959   3.06  0.0278          
    ## 12 5 - 9     0.00830 0.00466   955   1.78  0.477           
    ## 13 7 - 8    -0.0236  0.00479   964  -4.92  0.0000148       
    ## 14 7 - 9    -0.0297  0.00472   956  -6.29  0.00000000713   
    ## 15 8 - 9    -0.00612 0.00469   930  -1.31  0.782

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numclust.emm.data))

graph.numclust.emm.data.pairs <- full_join(graph.numclust.emm.data.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numclust.emm.data))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-201-1.png)<!-- -->

``` r
plot(pairs(graph.numclust.emm.data), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-201-2.png)<!-- -->

``` r
graph.numclust.emm.data.pairs$sig.levels <- 
  case_when(graph.numclust.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numclust.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numclust.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numclust.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numclust.emm.data.pairs$sig.levels <- factor(graph.numclust.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numclust.emm.data.pairs$contrast <- factor(graph.numclust.emm.data.pairs$contrast, levels=graph.numclust.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numclust.emm.data.pairs <- graph.numclust.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numclust.emm.data.pairs %>% arrange(estimate)
```

    ## # A tibble: 15 x 11
    ##    contrast From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##    <fct>    <chr> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ##  1 1 - 5    1     5     -0.0403  0.00466   946  -8.65    0        -5.37e⁻²
    ##  2 3 - 5    3     5     -0.0345  0.00467   929  -7.39    4.92e⁻¹² -4.78e⁻²
    ##  3 1 - 9    1     9     -0.0320  0.00463   936  -6.92    1.24e⁻¹⁰ -4.52e⁻²
    ##  4 7 - 9    7     9     -0.0297  0.00472   956  -6.29    7.13e⁻ ⁹ -4.32e⁻²
    ##  5 3 - 9    3     9     -0.0262  0.00470   974  -5.57    4.84e⁻ ⁷ -3.96e⁻²
    ##  6 1 - 8    1     8     -0.0259  0.00472   956  -5.49    7.50e⁻ ⁷ -3.94e⁻²
    ##  7 7 - 8    7     8     -0.0236  0.00479   964  -4.92    1.48e⁻ ⁵ -3.73e⁻²
    ##  8 3 - 8    3     8     -0.0201  0.00472   936  -4.25    3.38e⁻ ⁴ -3.36e⁻²
    ##  9 8 - 9    8     9     -0.00612 0.00469   930  -1.31    7.82e⁻ ¹ -1.95e⁻²
    ## 10 1 - 3    1     3     -0.00583 0.00467   964  -1.25    8.13e⁻ ¹ -1.92e⁻²
    ## 11 1 - 7    1     7     -0.00233 0.00470   935  -0.495   9.96e⁻ ¹ -1.57e⁻²
    ## 12 3 - 7    3     7      0.00351 0.00477   969   0.736   9.77e⁻ ¹ -1.01e⁻²
    ## 13 5 - 9    5     9      0.00830 0.00466   955   1.78    4.77e⁻ ¹ -4.99e⁻³
    ## 14 5 - 8    5     8      0.0144  0.00472   959   3.06    2.78e⁻ ²  9.50e⁻⁴
    ## 15 5 - 7    5     7      0.0380  0.00470   938   8.09    0         2.46e⁻²
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-201-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-201-4.png)<!-- -->

``` r
data.copy <- graph.numclust.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numclust.emm.data.pairs.compl <- bind_rows(graph.numclust.emm.data.pairs, data.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=cond.lev)
#graph.numlinks.emm.data.pairs.compl$To <- factor(graph.numlinks.emm.data.pairs.compl$To, levels=cond.lev)

graph.numclust.emm.data.pairs.compl$From <- factor(graph.numclust.emm.data.pairs.compl$From, levels=rev(unique(graph.numclust.emm.data.pairs.compl$From)))

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numclust.emm.data.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 30 x 11
    ##    contrast From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##    <fct>    <fct> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ##  1 1 - 5    1     5      -0.0403 0.00466   946   -8.65   0        -5.37e⁻²
    ##  2 5 - 7    7     5      -0.0380 0.00470   938   -8.09   0        -2.46e⁻²
    ##  3 3 - 5    3     5      -0.0345 0.00467   929   -7.39   4.92e⁻¹² -4.78e⁻²
    ##  4 1 - 9    1     9      -0.0320 0.00463   936   -6.92   1.24e⁻¹⁰ -4.52e⁻²
    ##  5 7 - 9    7     9      -0.0297 0.00472   956   -6.29   7.13e⁻ ⁹ -4.32e⁻²
    ##  6 3 - 9    3     9      -0.0262 0.00470   974   -5.57   4.84e⁻ ⁷ -3.96e⁻²
    ##  7 1 - 8    1     8      -0.0259 0.00472   956   -5.49   7.50e⁻ ⁷ -3.94e⁻²
    ##  8 7 - 8    7     8      -0.0236 0.00479   964   -4.92   1.48e⁻ ⁵ -3.73e⁻²
    ##  9 3 - 8    3     8      -0.0201 0.00472   936   -4.25   3.38e⁻ ⁴ -3.36e⁻²
    ## 10 5 - 8    8     5      -0.0144 0.00472   959   -3.06   2.78e⁻ ² -9.50e⁻⁴
    ## # ... with 20 more rows, and 2 more variables: upper.CL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numclust.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-202-1.png)<!-- -->

``` r
ggplot(graph.numclust.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-202-2.png)<!-- -->

``` r
ggplot(graph.numclust.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-202-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-202-4.png)<!-- -->

##### Overestimated

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numclust.emm.over <- emmeans(graph.numclust.lmer.full.int.7, "Overestimated", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numclust.emm.over
```

    ##  Overestimated        emmean          SE     df   lower.CL   upper.CL
    ##  1-Overestimated  0.07358780 0.003428585 693.48 0.06685615 0.08031945
    ##  2-CorrectOrUnder 0.02530646 0.002483941 481.91 0.02042577 0.03018715
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numclust.emm.over.df <- dplyr::as_data_frame(graph.numclust.emm.over)

graph.numclust.emm.over.df
```

    ## # A tibble: 2 x 6
    ##   Overestimated    emmean      SE    df lower.CL upper.CL
    ## * <fct>             <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1-Overestimated  0.0736 0.00343   693   0.0669   0.0803
    ## 2 2-CorrectOrUnder 0.0253 0.00248   482   0.0204   0.0302

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numclust.emm.over.cld <- cld(graph.numclust.emm.over,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numclust.emm.over.cld.df <- graph.numclust.emm.over.cld$emmeans

graph.numclust.emm.over.cld.df %>% dplyr::select(Overestimated,.group)
```

    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.over.cld.df
```

    ##  Overestimated        emmean          SE     df   lower.CL   upper.CL
    ##  2-CorrectOrUnder 0.02530646 0.002483941 481.91 0.02042577 0.03018715
    ##  1-Overestimated  0.07358780 0.003428585 693.48 0.06685615 0.08031945
    ##  .group
    ##   1    
    ##    2   
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.over.cld.df$Overestimated <- factor(graph.numclust.emm.over.cld.df$Overestimated, levels=graph.numclust.emm.over.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Overestimated) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numclust.emm.over, ~Overestimated, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-203-1.png)<!-- -->

``` r
plot(graph.numclust.emm.over)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-203-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numclust.emm.over.cld.df %>% arrange(desc(emmean))
```

    ##      Overestimated     emmean          SE       df   lower.CL   upper.CL
    ## 1  1-Overestimated 0.07358780 0.003428585 693.4794 0.06685615 0.08031945
    ## 2 2-CorrectOrUnder 0.02530646 0.002483941 481.9145 0.02042577 0.03018715
    ##   .group
    ## 1      2
    ## 2     1

``` r
ggplot(graph.numclust.emm.over.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Overestimated,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Overestimated,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(labels=c("Overestimated","Correct or\nUnderestimated")) +
  coord_flip() +
labs(title="Estimated Marginal Means for Overestimated for Number of Clusters task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-203-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numclust.emm.over.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.over)) 
graph.numclust.emm.over.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast             estimate      SE    df t.ratio              p.value
    ## * <fct>                   <dbl>   <dbl> <dbl>   <dbl>                <dbl>
    ## 1 1-Overestimated - 2…   0.0483 0.00391   976    12.4             1.01e⁻³²

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numclust.emm.over))

graph.numclust.emm.over.pairs <- full_join(graph.numclust.emm.over.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numclust.emm.over))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-204-1.png)<!-- -->

``` r
plot(pairs(graph.numclust.emm.over), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-204-2.png)<!-- -->

``` r
graph.numclust.emm.over.pairs$sig.levels <- 
  case_when(graph.numclust.emm.over.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numclust.emm.over.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numclust.emm.over.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numclust.emm.over.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numclust.emm.over.pairs$sig.levels <- factor(graph.numclust.emm.over.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numclust.emm.over.pairs$contrast <- factor(graph.numclust.emm.over.pairs$contrast, levels=graph.numclust.emm.over.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numclust.emm.over.pairs <- graph.numclust.emm.over.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numclust.emm.over.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast   From  To    estimate      SE    df t.ratio   p.value lower.CL
    ##   <fct>      <chr> <chr>    <dbl>   <dbl> <dbl>   <dbl>     <dbl>    <dbl>
    ## 1 1-Overest… 1-Ov… 2-Co…   0.0483 0.00391   976    12.4  1.01e⁻³²   0.0406
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.over.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-204-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.over.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-204-4.png)<!-- -->

``` r
over.copy <- graph.numclust.emm.over.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numclust.emm.over.pairs.compl <- bind_rows(graph.numclust.emm.over.pairs, over.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=cond.lev)
#graph.numlinks.emm.data.pairs.compl$To <- factor(graph.numlinks.emm.data.pairs.compl$To, levels=cond.lev)

graph.numclust.emm.over.pairs.compl$From <- factor(graph.numclust.emm.over.pairs.compl$From, levels=rev(unique(graph.numclust.emm.over.pairs.compl$From)))

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numclust.emm.over.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast   From  To    estimate      SE    df t.ratio   p.value lower.CL
    ##   <fct>      <fct> <chr>    <dbl>   <dbl> <dbl>   <dbl>     <dbl>    <dbl>
    ## 1 1-Overest… 2-Co… 1-Ov…  -0.0483 0.00391   976   -12.4  1.01e⁻³²  -0.0406
    ## 2 1-Overest… 1-Ov… 2-Co…   0.0483 0.00391   976    12.4  1.01e⁻³²   0.0406
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.over.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-205-1.png)<!-- -->

``` r
ggplot(graph.numclust.emm.over.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-205-2.png)<!-- -->

``` r
ggplot(graph.numclust.emm.over.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-205-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.over.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-205-4.png)<!-- -->

##### Stats.OperatingSystemNumClust

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numclust.emm.OS <- emmeans(graph.numclust.lmer.full.int.7, "Stats.OperatingSystemNumClust", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numclust.emm.OS
```

    ##  Stats.OperatingSystemNumClust     emmean          SE     df   lower.CL
    ##  1-HighSig                     0.03978100 0.001961687 421.07 0.03592508
    ##  Other                         0.05911326 0.003958144 422.15 0.05133313
    ##    upper.CL
    ##  0.04363691
    ##  0.06689339
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset, Overestimated 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numclust.emm.OS.df <- dplyr::as_data_frame(graph.numclust.emm.OS)

graph.numclust.emm.OS.df
```

    ## # A tibble: 2 x 6
    ##   Stats.OperatingSystemNumClust emmean      SE    df lower.CL upper.CL
    ## * <fct>                          <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1-HighSig                     0.0398 0.00196   421   0.0359   0.0436
    ## 2 Other                         0.0591 0.00396   422   0.0513   0.0669

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numclust.emm.OS.cld <- cld(graph.numclust.emm.OS,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numclust.emm.OS.cld.df <- graph.numclust.emm.OS.cld$emmeans

graph.numclust.emm.OS.cld.df %>% dplyr::select(Stats.OperatingSystemNumClust,.group)
```

    ##  Stats.OperatingSystemNumClust .group
    ##  1-HighSig                      1    
    ##  Other                           2   
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset, Overestimated 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.OS.cld.df
```

    ##  Stats.OperatingSystemNumClust     emmean          SE     df   lower.CL
    ##  1-HighSig                     0.03978100 0.001961687 421.07 0.03592508
    ##  Other                         0.05911326 0.003958144 422.15 0.05133313
    ##    upper.CL .group
    ##  0.04363691  1    
    ##  0.06689339   2   
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset, Overestimated 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.OS.cld.df$Stats.OperatingSystemNumClust <- factor(graph.numclust.emm.OS.cld.df$Stats.OperatingSystemNumClust, levels=graph.numclust.emm.OS.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Stats.OperatingSystemNumClust) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numclust.emm.OS, ~Stats.OperatingSystemNumClust, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-206-1.png)<!-- -->

``` r
plot(graph.numclust.emm.OS)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-206-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numclust.emm.OS.cld.df %>% arrange(desc(emmean))
```

    ##   Stats.OperatingSystemNumClust     emmean          SE       df   lower.CL
    ## 1                         Other 0.05911326 0.003958144 422.1482 0.05133313
    ## 2                     1-HighSig 0.03978100 0.001961687 421.0650 0.03592508
    ##     upper.CL .group
    ## 1 0.06689339      2
    ## 2 0.04363691     1

``` r
ggplot(graph.numclust.emm.OS.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Stats.OperatingSystemNumClust,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Stats.OperatingSystemNumClust,y=emmean), size=7) +
  scale_x_discrete(labels=c("High Error Group","Low Error Group")) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
    labs(title="Estimated Marginal Means for Operating Systems for Number of Clusters task,\ngraphics conditions",
         x="Operating Systems")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-206-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numclust.emm.OS.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.OS)) 
graph.numclust.emm.OS.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast          estimate      SE    df t.ratio    p.value
    ## * <fct>                <dbl>   <dbl> <dbl>   <dbl>      <dbl>
    ## 1 1-HighSig - Other  -0.0193 0.00429   426   -4.50 0.00000866

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numclust.emm.OS))

graph.numclust.emm.OS.pairs <- full_join(graph.numclust.emm.OS.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numclust.emm.OS))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-207-1.png)<!-- -->

``` r
plot(pairs(graph.numclust.emm.OS), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-207-2.png)<!-- -->

``` r
graph.numclust.emm.OS.pairs$sig.levels <- 
  case_when(graph.numclust.emm.OS.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numclust.emm.OS.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numclust.emm.OS.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numclust.emm.OS.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numclust.emm.OS.pairs$sig.levels <- factor(graph.numclust.emm.OS.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numclust.emm.OS.pairs$contrast <- factor(graph.numclust.emm.OS.pairs$contrast, levels=graph.numclust.emm.OS.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numclust.emm.OS.pairs <- graph.numclust.emm.OS.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numclust.emm.OS.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast    From   To    estimate      SE    df t.ratio p.value lower.CL
    ##   <fct>       <chr>  <chr>    <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ## 1 1-HighSig … 1-Hig… Other  -0.0193 0.00429   426   -4.50 8.66e⁻⁶  -0.0278
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.OS.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-207-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.OS.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-207-4.png)<!-- -->

``` r
OS.copy <- graph.numclust.emm.OS.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numclust.emm.OS.pairs.compl <- bind_rows(graph.numclust.emm.OS.pairs, OS.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=cond.lev)
#graph.numlinks.emm.data.pairs.compl$To <- factor(graph.numlinks.emm.data.pairs.compl$To, levels=cond.lev)

graph.numclust.emm.OS.pairs.compl$From <- factor(graph.numclust.emm.OS.pairs.compl$From, levels=rev(unique(graph.numclust.emm.OS.pairs.compl$From)))

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numclust.emm.OS.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast   From   To     estimate      SE    df t.ratio p.value lower.CL
    ##   <fct>      <fct>  <chr>     <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ## 1 1-HighSig… 1-Hig… Other   -0.0193 0.00429   426   -4.50 8.66e⁻⁶  -0.0278
    ## 2 1-HighSig… Other  1-Hig…   0.0193 0.00429   426    4.50 8.66e⁻⁶   0.0278
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numclust.emm.OS.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-208-1.png)<!-- -->

``` r
ggplot(graph.numclust.emm.OS.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-208-2.png)<!-- -->

``` r
ggplot(graph.numclust.emm.OS.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-208-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.OS.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-208-4.png)<!-- -->

##### ConditionColor:Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numclust.emm.conddata <- emmeans(graph.numclust.lmer.full.int.7, ~ Dataset | ConditionColor, nesting = NULL)
graph.numclust.emm.conddata.2 <- emmeans(graph.numclust.lmer.full.int.7, ~  ConditionColor | Dataset, nesting = NULL)

graph.numclust.emm.conddata
```

    ## ConditionColor = 1-Color:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.04473869 0.006050348 1128.62 0.03286750 0.05660989
    ##  3       0.04197572 0.006179337 1131.26 0.02985147 0.05409997
    ##  5       0.06690047 0.006087497 1128.94 0.05495639 0.07884455
    ##  7       0.03362772 0.006297535 1130.42 0.02127154 0.04598389
    ##  8       0.07804996 0.006298838 1131.61 0.06569125 0.09040867
    ##  9       0.09220044 0.006047669 1127.60 0.08033449 0.10406639
    ## 
    ## ConditionColor = 2-Other:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.01867985 0.003802387 1086.03 0.01121899 0.02614071
    ##  3       0.03310359 0.003795363 1082.19 0.02565648 0.04055069
    ##  5       0.07718426 0.003935401 1083.18 0.06946239 0.08490613
    ##  7       0.03444141 0.003933251 1088.42 0.02672380 0.04215902
    ##  8       0.03718303 0.003845834 1087.83 0.02963694 0.04472912
    ##  9       0.03528041 0.003980369 1102.81 0.02747046 0.04309036
    ## 
    ## Results are averaged over the levels of: Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numclust.emm.conddata.df <- dplyr::as_data_frame(graph.numclust.emm.conddata)

graph.numclust.emm.conddata.df #%>% View()
```

    ## # A tibble: 12 x 7
    ##    Dataset ConditionColor emmean      SE    df lower.CL upper.CL
    ##  * <fct>   <fct>           <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ##  1 1       1-Color        0.0447 0.00605  1129   0.0329   0.0566
    ##  2 3       1-Color        0.0420 0.00618  1131   0.0299   0.0541
    ##  3 5       1-Color        0.0669 0.00609  1129   0.0550   0.0788
    ##  4 7       1-Color        0.0336 0.00630  1130   0.0213   0.0460
    ##  5 8       1-Color        0.0780 0.00630  1132   0.0657   0.0904
    ##  6 9       1-Color        0.0922 0.00605  1128   0.0803   0.104 
    ##  7 1       2-Other        0.0187 0.00380  1086   0.0112   0.0261
    ##  8 3       2-Other        0.0331 0.00380  1082   0.0257   0.0406
    ##  9 5       2-Other        0.0772 0.00394  1083   0.0695   0.0849
    ## 10 7       2-Other        0.0344 0.00393  1088   0.0267   0.0422
    ## 11 8       2-Other        0.0372 0.00385  1088   0.0296   0.0447
    ## 12 9       2-Other        0.0353 0.00398  1103   0.0275   0.0431

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numclust.emm.conddata.cld <- cld(graph.numclust.emm.conddata,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )

graph.numclust.emm.conddata.cld[["emmeans"]]
```

    ## ConditionColor = 1-Color:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  7       0.03362772 0.006297535 1130.42 0.02127154 0.04598389  1    
    ##  3       0.04197572 0.006179337 1131.26 0.02985147 0.05409997  1    
    ##  1       0.04473869 0.006050348 1128.62 0.03286750 0.05660989  12   
    ##  5       0.06690047 0.006087497 1128.94 0.05495639 0.07884455   23  
    ##  8       0.07804996 0.006298838 1131.61 0.06569125 0.09040867    34 
    ##  9       0.09220044 0.006047669 1127.60 0.08033449 0.10406639     4 
    ## 
    ## ConditionColor = 2-Other:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  1       0.01867985 0.003802387 1086.03 0.01121899 0.02614071  1    
    ##  3       0.03310359 0.003795363 1082.19 0.02565648 0.04055069   2   
    ##  7       0.03444141 0.003933251 1088.42 0.02672380 0.04215902   2   
    ##  9       0.03528041 0.003980369 1102.81 0.02747046 0.04309036   2   
    ##  8       0.03718303 0.003845834 1087.83 0.02963694 0.04472912   2   
    ##  5       0.07718426 0.003935401 1083.18 0.06946239 0.08490613    3  
    ## 
    ## Results are averaged over the levels of: Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.conddata.cld.df <- graph.numclust.emm.conddata.cld[["emmeans"]]

graph.numclust.emm.conddata.cld.df %>% dplyr::select(Dataset,ConditionColor,.group) %>% print()
```

    ## ConditionColor = 1-Color:
    ##  Dataset .group
    ##  7        1    
    ##  3        1    
    ##  1        12   
    ##  5         23  
    ##  8          34 
    ##  9           4 
    ## 
    ## ConditionColor = 2-Other:
    ##  Dataset .group
    ##  1        1    
    ##  3         2   
    ##  7         2   
    ##  9         2   
    ##  8         2   
    ##  5          3  
    ## 
    ## Results are averaged over the levels of: Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numclust.emm.conddata.2, details=TRUE, sort=TRUE)[["emmeans"]] %>% dplyr::select(Dataset,ConditionColor,.group) %>% print()
```

    ## Dataset = 1:
    ##  ConditionColor .group
    ##  2-Other         1    
    ##  1-Color          2   
    ## 
    ## Dataset = 3:
    ##  ConditionColor .group
    ##  2-Other         1    
    ##  1-Color         1    
    ## 
    ## Dataset = 5:
    ##  ConditionColor .group
    ##  1-Color         1    
    ##  2-Other         1    
    ## 
    ## Dataset = 7:
    ##  ConditionColor .group
    ##  1-Color         1    
    ##  2-Other         1    
    ## 
    ## Dataset = 8:
    ##  ConditionColor .group
    ##  2-Other         1    
    ##  1-Color          2   
    ## 
    ## Dataset = 9:
    ##  ConditionColor .group
    ##  2-Other         1    
    ##  1-Color          2   
    ## 
    ## Results are averaged over the levels of: Overestimated, Stats.OperatingSystemNumClust 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numclust.emm.conddata, ~ConditionColor|Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-209-1.png)<!-- -->

``` r
plot(graph.numclust.emm.conddata)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-209-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numclust.emm.conddata.cld.df %>% arrange(desc(emmean))
```

    ##    Dataset ConditionColor     emmean          SE       df   lower.CL
    ## 1        9        1-Color 0.09220044 0.006047669 1127.602 0.08033449
    ## 2        8        1-Color 0.07804996 0.006298838 1131.612 0.06569125
    ## 3        5        2-Other 0.07718426 0.003935401 1083.179 0.06946239
    ## 4        5        1-Color 0.06690047 0.006087497 1128.944 0.05495639
    ## 5        1        1-Color 0.04473869 0.006050348 1128.616 0.03286750
    ## 6        3        1-Color 0.04197572 0.006179337 1131.256 0.02985147
    ## 7        8        2-Other 0.03718303 0.003845834 1087.829 0.02963694
    ## 8        9        2-Other 0.03528041 0.003980369 1102.808 0.02747046
    ## 9        7        2-Other 0.03444141 0.003933251 1088.417 0.02672380
    ## 10       7        1-Color 0.03362772 0.006297535 1130.419 0.02127154
    ## 11       3        2-Other 0.03310359 0.003795363 1082.187 0.02565648
    ## 12       1        2-Other 0.01867985 0.003802387 1086.031 0.01121899
    ##      upper.CL .group
    ## 1  0.10406639      4
    ## 2  0.09040867     34
    ## 3  0.08490613      3
    ## 4  0.07884455    23 
    ## 5  0.05660989   12  
    ## 6  0.05409997   1   
    ## 7  0.04472912     2 
    ## 8  0.04309036     2 
    ## 9  0.04215902     2 
    ## 10 0.04598389   1   
    ## 11 0.04055069     2 
    ## 12 0.02614071    1

``` r
ggplot(graph.numclust.emm.conddata.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=ConditionColor,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=ConditionColor,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Dataset) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-209-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=Dataset,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~ConditionColor)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-209-4.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.cld.df) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL, color=ConditionColor), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=Dataset,y=emmean, color=ConditionColor), position=position_dodge(width=0.2)) +
  geom_line(aes(x=Dataset,y=emmean, color=ConditionColor, group=ConditionColor), position=position_dodge(width=0.2)) +
  scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  labs(title="Estimated Marginal Means for Node Color vs. Dataset for Number of Clusters\ntask, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-209-5.png)<!-- -->

``` r
ggsave(file.path(figureDir, "emmeanscolordatanumclustgraphics.pdf"), width=7, height=5)
```

``` r
graph.numclust.emm.conddata.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.conddata)) 
graph.numclust.emm.conddata.pairs #%>% View()
```

    ## # A tibble: 30 x 7
    ##    contrast ConditionColor estimate      SE    df t.ratio      p.value
    ##  * <fct>    <fct>             <dbl>   <dbl> <dbl>   <dbl>        <dbl>
    ##  1 1 - 3    1-Color         0.00276 0.00807   967   0.342 0.999       
    ##  2 1 - 5    1-Color        -0.0222  0.00795   938  -2.79  0.0602      
    ##  3 1 - 7    1-Color         0.0111  0.00807   928   1.38  0.741       
    ##  4 1 - 8    1-Color        -0.0333  0.00813   955  -4.10  0.000642    
    ##  5 1 - 9    1-Color        -0.0475  0.00790   925  -6.01  0.0000000406
    ##  6 3 - 5    1-Color        -0.0249  0.00798   922  -3.12  0.0227      
    ##  7 3 - 7    1-Color         0.00835 0.00822   966   1.02  0.913       
    ##  8 3 - 8    1-Color        -0.0361  0.00816   929  -4.42  0.000161    
    ##  9 3 - 9    1-Color        -0.0502  0.00808   978  -6.21  0.0000000114
    ## 10 5 - 7    1-Color         0.0333  0.00807   924   4.12  0.000581    
    ## # ... with 20 more rows

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

conddata.pairs.CI <- confint(pairs(graph.numclust.emm.conddata))

graph.numclust.emm.conddata.pairs <- full_join(graph.numclust.emm.conddata.pairs, conddata.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numclust.emm.conddata))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-210-1.png)<!-- -->

``` r
plot(pairs(graph.numclust.emm.conddata), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-210-2.png)<!-- -->

``` r
graph.numclust.emm.conddata.pairs$sig.levels <- 
  case_when(graph.numclust.emm.conddata.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numclust.emm.conddata.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numclust.emm.conddata.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numclust.emm.conddata.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numclust.emm.conddata.pairs$sig.levels <- factor(graph.numclust.emm.conddata.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numclust.emm.conddata.pairs$contrast.cond <- paste0(graph.numclust.emm.conddata.pairs$contrast,graph.numclust.emm.conddata.pairs$Condition)

graph.numclust.emm.conddata.pairs$contrast.cond <- factor(graph.numclust.emm.conddata.pairs$contrast.cond, levels=graph.numclust.emm.conddata.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.cond) %>% distinct() %>% unlist())

#graph.numnodes.emm.conddata.pairs %>% View()

graph.numclust.emm.conddata.pairs$contrast <- 
  factor(as.character(graph.numclust.emm.conddata.pairs$contrast), 
         levels=graph.numclust.emm.conddata.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numclust.emm.conddata.pairs <- graph.numclust.emm.conddata.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numclust.emm.conddata.pairs %>% arrange(estimate)
```

    ## # A tibble: 30 x 13
    ##    contrast From  To    ConditionColor estimate      SE    df t.ratio
    ##    <fct>    <chr> <chr> <fct>             <dbl>   <dbl> <dbl>   <dbl>
    ##  1 7 - 9    7     9     1-Color         -0.0586 0.00812   958  - 7.21
    ##  2 1 - 5    1     5     2-Other         -0.0585 0.00479   959  -12.2 
    ##  3 3 - 9    3     9     1-Color         -0.0502 0.00808   978  - 6.21
    ##  4 1 - 9    1     9     1-Color         -0.0475 0.00790   925  - 6.01
    ##  5 7 - 8    7     8     1-Color         -0.0444 0.00832   970  - 5.34
    ##  6 3 - 5    3     5     2-Other         -0.0441 0.00476   932  - 9.26
    ##  7 3 - 8    3     8     1-Color         -0.0361 0.00816   929  - 4.42
    ##  8 1 - 8    1     8     1-Color         -0.0333 0.00813   955  - 4.10
    ##  9 5 - 9    5     9     1-Color         -0.0253 0.00796   957  - 3.18
    ## 10 3 - 5    3     5     1-Color         -0.0249 0.00798   922  - 3.12
    ## # ... with 20 more rows, and 5 more variables: p.value <dbl>,
    ## #   lower.CL <dbl>, upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
#graph.numlinks.emm.conddata.pairs %>% View()

ggplot(graph.numclust.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~ConditionColor, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-210-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~ConditionColor, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-210-4.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~ConditionColor, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-210-5.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~ConditionColor, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-210-6.png)<!-- -->

``` r
conddata.copy <- graph.numclust.emm.conddata.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numclust.emm.conddata.pairs.compl <- bind_rows(graph.numclust.emm.conddata.pairs, conddata.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numclust.emm.conddata.pairs.compl$From <- factor(graph.numclust.emm.conddata.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numclust.emm.conddata.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 60 x 13
    ##    contrast From  To    ConditionColor estimate      SE    df t.ratio
    ##    <fct>    <fct> <chr> <fct>             <dbl>   <dbl> <dbl>   <dbl>
    ##  1 7 - 9    7     9     1-Color         -0.0586 0.00812   958  - 7.21
    ##  2 1 - 5    1     5     2-Other         -0.0585 0.00479   959  -12.2 
    ##  3 3 - 9    3     9     1-Color         -0.0502 0.00808   978  - 6.21
    ##  4 1 - 9    1     9     1-Color         -0.0475 0.00790   925  - 6.01
    ##  5 7 - 8    7     8     1-Color         -0.0444 0.00832   970  - 5.34
    ##  6 3 - 5    3     5     2-Other         -0.0441 0.00476   932  - 9.26
    ##  7 5 - 7    7     5     2-Other         -0.0427 0.00480   978  - 8.90
    ##  8 5 - 9    9     5     2-Other         -0.0419 0.00483   952  - 8.68
    ##  9 5 - 8    8     5     2-Other         -0.0400 0.00474   932  - 8.44
    ## 10 3 - 8    3     8     1-Color         -0.0361 0.00816   929  - 4.42
    ## # ... with 50 more rows, and 5 more variables: p.value <dbl>,
    ## #   lower.CL <dbl>, upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
ggplot(graph.numclust.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~ConditionColor)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-211-1.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~ConditionColor)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-211-2.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~ConditionColor)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-211-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~ConditionColor)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-211-4.png)<!-- -->

##### Overestimated:Stats.OperatingSystemNumClust

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numclust.emm.overnclust <- emmeans(graph.numclust.lmer.full.int.7, ~ Overestimated | Stats.OperatingSystemNumClust, nesting = NULL)
graph.numclust.emm.overnclust.2 <- emmeans(graph.numclust.lmer.full.int.7, ~ Stats.OperatingSystemNumClust | Overestimated, nesting = NULL)

graph.numclust.emm.overnclust
```

    ## Stats.OperatingSystemNumClust = 1-HighSig:
    ##  Overestimated        emmean          SE     df   lower.CL   upper.CL
    ##  1-Overestimated  0.05493169 0.002835152 774.99 0.04936621 0.06049718
    ##  2-CorrectOrUnder 0.02463030 0.002130635 502.56 0.02044425 0.02881635
    ## 
    ## Stats.OperatingSystemNumClust = Other:
    ##  Overestimated        emmean          SE     df   lower.CL   upper.CL
    ##  1-Overestimated  0.09224390 0.006133427 693.46 0.08020159 0.10428622
    ##  2-CorrectOrUnder 0.02598262 0.004360069 483.92 0.01741561 0.03454962
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numclust.emm.overnclust.df <- dplyr::as_data_frame(graph.numclust.emm.overnclust)

graph.numclust.emm.overnclust.df #%>% View()
```

    ## # A tibble: 4 x 7
    ##   Overestimated    Stats.Operating… emmean      SE    df lower.CL upper.CL
    ## * <fct>            <fct>             <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1-Overestimated  1-HighSig        0.0549 0.00284   775   0.0494   0.0605
    ## 2 2-CorrectOrUnder 1-HighSig        0.0246 0.00213   503   0.0204   0.0288
    ## 3 1-Overestimated  Other            0.0922 0.00613   693   0.0802   0.104 
    ## 4 2-CorrectOrUnder Other            0.0260 0.00436   484   0.0174   0.0345

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numclust.emm.overnclust.cld <- cld(graph.numclust.emm.overnclust,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )

graph.numclust.emm.overnclust.cld[["emmeans"]]
```

    ## Stats.OperatingSystemNumClust = 1-HighSig:
    ##  Overestimated        emmean          SE     df   lower.CL   upper.CL
    ##  2-CorrectOrUnder 0.02463030 0.002130635 502.56 0.02044425 0.02881635
    ##  1-Overestimated  0.05493169 0.002835152 774.99 0.04936621 0.06049718
    ##  .group
    ##   1    
    ##    2   
    ## 
    ## Stats.OperatingSystemNumClust = Other:
    ##  Overestimated        emmean          SE     df   lower.CL   upper.CL
    ##  2-CorrectOrUnder 0.02598262 0.004360069 483.92 0.01741561 0.03454962
    ##  1-Overestimated  0.09224390 0.006133427 693.46 0.08020159 0.10428622
    ##  .group
    ##   1    
    ##    2   
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numclust.emm.overnclust.cld.df <- graph.numclust.emm.overnclust.cld[["emmeans"]]

graph.numclust.emm.overnclust.cld.df %>% dplyr::select(Overestimated,Stats.OperatingSystemNumClust,.group) %>% print()
```

    ## Stats.OperatingSystemNumClust = 1-HighSig:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Stats.OperatingSystemNumClust = Other:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numclust.emm.overnclust.2, details=TRUE, sort=TRUE)[["emmeans"]] %>% dplyr::select(Overestimated,Stats.OperatingSystemNumClust,.group) %>% print()
```

    ## Overestimated = 1-Overestimated:
    ##  Stats.OperatingSystemNumClust .group
    ##  1-HighSig                      1    
    ##  Other                           2   
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  Stats.OperatingSystemNumClust .group
    ##  1-HighSig                      1    
    ##  Other                          1    
    ## 
    ## Results are averaged over the levels of: ConditionColor, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numclust.emm.overnclust, ~Overestimated|Stats.OperatingSystemNumClust, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-212-1.png)<!-- -->

``` r
plot(graph.numclust.emm.overnclust)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-212-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numclust.emm.overnclust.cld.df %>% arrange(desc(emmean))
```

    ##      Overestimated Stats.OperatingSystemNumClust     emmean          SE
    ## 1  1-Overestimated                         Other 0.09224390 0.006133427
    ## 2  1-Overestimated                     1-HighSig 0.05493169 0.002835152
    ## 3 2-CorrectOrUnder                         Other 0.02598262 0.004360069
    ## 4 2-CorrectOrUnder                     1-HighSig 0.02463030 0.002130635
    ##         df   lower.CL   upper.CL .group
    ## 1 693.4624 0.08020159 0.10428622      2
    ## 2 774.9927 0.04936621 0.06049718      2
    ## 3 483.9232 0.01741561 0.03454962     1 
    ## 4 502.5560 0.02044425 0.02881635     1

``` r
ggplot(graph.numclust.emm.overnclust.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Overestimated,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=Overestimated,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Stats.OperatingSystemNumClust) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-212-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.cld.df) +
  geom_errorbar(aes(x=Overestimated,ymax=upper.CL,ymin=lower.CL, color=Stats.OperatingSystemNumClust), width=.2) +
  geom_point(aes(x=Overestimated,y=emmean, color=Stats.OperatingSystemNumClust)) +
  geom_line(aes(x=Overestimated,y=emmean, color=Stats.OperatingSystemNumClust, group=Stats.OperatingSystemNumClust)) +
  scale_color_discrete(name="Operating\nSystems", labels=c("Low Error Group","High Error Group"))+
  scale_x_discrete(labels=c("Overestimated","Correct or\nUnderestimated")) +
labs(title="Estimated Marginal Means for Operating System vs. Overestimated for\nNumber of Clusters task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-212-4.png)<!-- -->

``` r
graph.numclust.emm.overnclust.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.overnclust)) 
graph.numclust.emm.overnclust.pairs #%>% View()
```

    ## # A tibble: 2 x 7
    ##   contrast     Stats.OperatingS… estimate      SE    df t.ratio    p.value
    ## * <fct>        <fct>                <dbl>   <dbl> <dbl>   <dbl>      <dbl>
    ## 1 1-Overestim… 1-HighSig           0.0303 0.00312  1121    9.70   2.07e⁻²¹
    ## 2 1-Overestim… Other               0.0663 0.00711   946    9.32   8.25e⁻²⁰

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

graph.numclust.emm.overnclust.2.pairs <- dplyr::as_data_frame(pairs(graph.numclust.emm.overnclust.2)) 
graph.numclust.emm.overnclust.2.pairs #%>% View()
```

    ## # A tibble: 2 x 7
    ##   contrast          Overestimated  estimate      SE    df t.ratio  p.value
    ## * <fct>             <fct>             <dbl>   <dbl> <dbl>   <dbl>    <dbl>
    ## 1 1-HighSig - Other 1-Overestimat… -0.0373  0.00666   722  -5.61   2.94e⁻⁸
    ## 2 1-HighSig - Other 2-CorrectOrUn… -0.00135 0.00473   494  -0.286  7.75e⁻¹

``` r
overnclust.pairs.CI <- confint(pairs(graph.numclust.emm.overnclust))

graph.numclust.emm.overnclust.pairs <- full_join(graph.numclust.emm.overnclust.pairs, overnclust.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numclust.emm.overnclust))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-213-1.png)<!-- -->

``` r
plot(pairs(graph.numclust.emm.overnclust), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-213-2.png)<!-- -->

``` r
graph.numclust.emm.overnclust.pairs$sig.levels <- 
  case_when(graph.numclust.emm.overnclust.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numclust.emm.overnclust.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numclust.emm.overnclust.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numclust.emm.overnclust.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numclust.emm.overnclust.pairs$sig.levels <- factor(graph.numclust.emm.overnclust.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numclust.emm.overnclust.pairs$contrast.cond <- paste0(graph.numclust.emm.overnclust.pairs$contrast,graph.numclust.emm.overnclust.pairs$Stats.OperatingSystemNumClust)

graph.numclust.emm.overnclust.pairs$contrast.cond <- factor(graph.numclust.emm.overnclust.pairs$contrast.cond, levels=graph.numclust.emm.overnclust.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.cond) %>% distinct() %>% unlist())

#graph.numnodes.emm.conddata.pairs %>% View()

graph.numclust.emm.overnclust.pairs$contrast <- 
  factor(as.character(graph.numclust.emm.overnclust.pairs$contrast), 
         levels=graph.numclust.emm.overnclust.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numclust.emm.overnclust.pairs <- graph.numclust.emm.overnclust.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numclust.emm.overnclust.pairs %>% arrange(estimate)
```

    ## # A tibble: 2 x 13
    ##   contrast   From   To     Stats.Operating… estimate      SE    df t.ratio
    ##   <fct>      <chr>  <chr>  <fct>               <dbl>   <dbl> <dbl>   <dbl>
    ## 1 1-Overest… 1-Ove… 2-Cor… 1-HighSig          0.0303 0.00312  1121    9.70
    ## 2 1-Overest… 1-Ove… 2-Cor… Other              0.0663 0.00711   946    9.32
    ## # ... with 5 more variables: p.value <dbl>, lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
#graph.numlinks.emm.conddata.pairs %>% View()

ggplot(graph.numclust.emm.overnclust.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Stats.OperatingSystemNumClust, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-213-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Stats.OperatingSystemNumClust, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-213-4.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Stats.OperatingSystemNumClust, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-213-5.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Stats.OperatingSystemNumClust, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-213-6.png)<!-- -->

``` r
overnclust.copy <- graph.numclust.emm.overnclust.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numclust.emm.overnclust.pairs.compl <- bind_rows(graph.numclust.emm.overnclust.pairs, overnclust.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numclust.emm.overnclust.pairs.compl$From <- factor(graph.numclust.emm.overnclust.pairs.compl$From, levels=rev(unique(graph.numclust.emm.overnclust.pairs.compl$From)))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numclust.emm.overnclust.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 4 x 13
    ##   contrast   From   To     Stats.Operating… estimate      SE    df t.ratio
    ##   <fct>      <fct>  <chr>  <fct>               <dbl>   <dbl> <dbl>   <dbl>
    ## 1 1-Overest… 2-Cor… 1-Ove… Other             -0.0663 0.00711   946   -9.32
    ## 2 1-Overest… 2-Cor… 1-Ove… 1-HighSig         -0.0303 0.00312  1121   -9.70
    ## 3 1-Overest… 1-Ove… 2-Cor… 1-HighSig          0.0303 0.00312  1121    9.70
    ## 4 1-Overest… 1-Ove… 2-Cor… Other              0.0663 0.00711   946    9.32
    ## # ... with 5 more variables: p.value <dbl>, lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
ggplot(graph.numclust.emm.overnclust.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Stats.OperatingSystemNumClust)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-214-1.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Stats.OperatingSystemNumClust)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-214-2.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Stats.OperatingSystemNumClust)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-214-3.png)<!-- -->

``` r
ggplot(graph.numclust.emm.overnclust.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Stats.OperatingSystemNumClust)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-214-4.png)<!-- -->

### Degree of Highest Degree Node

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_numhighdeg <- readRDS(file.path(analysisDataDir, "GraphicsNumHD.rds"))
```

#### lme4

``` r
# Condition

graph.numhd.lmer.cond <- lmer(LogError ~ Condition + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.cond) # Color and Phrasing very significant, Size not at all

anova(graph.numhd.lmer.cond)

# Condition is significant (p=2.557e-06); trying Ctrl_dummy

graph.numhd.lmer.ctrldummy <- lmer(LogError ~ Ctrl_dummy + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.ctrldummy)

anova(graph.numhd.lmer.ctrldummy)

# Ctrl_dummy is significant (p=0.000624)

graph.numhd.lmer.condphr <- lmer(LogError ~ ConditionPhrasing + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.condphr) 

anova(graph.numhd.lmer.condphr)

# ConditionPhrasing is significant (p=0.003696)

graph.numhd.lmer.condgr <- lmer(LogError ~ ConditionGraphics + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.condgr) 

anova(graph.numhd.lmer.condgr)

# ConditionGraphics is significant (p=0.001828)

graph.numhd.lmer.condcol <- lmer(LogError ~ ConditionColor + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.condcol) 

anova(graph.numhd.lmer.condcol)

# ConditionColor is significant (p=0.002186)


graph.numhd.lmer.dataset <- lmer(LogError ~ Dataset + (1|Demo.ResponseID), data = graphics_numhighdeg, REML = T)

lmsum <- summary(graph.numhd.lmer.dataset)
lmsum
#names(lmsum)

anova(graph.numhd.lmer.dataset)

# Dataset is significant (p < 1.3e-12); trying QuestionOrder

graph.numhd.lmer <- lmer(LogError ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# QuestionOrder is not significant; trying DatasetOrder

graph.numhd.lmer.dataord <- lmer(LogError ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.dataord)

anova(graph.numhd.lmer.dataord)

# DatasetOrder is significant (p=0.005725); trying DatasetDuration

graph.numhd.lmer <- lmer(LogError ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# DatasetDuration is barely not significant (p=0.05675); trying DatasetStartTime

graph.numhd.lmer <- lmer(LogError ~ DatasetStartTime + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# DatasetStartTime is not significant; trying TaskOrder

graph.numhd.lmer <- lmer(LogError ~ TaskOrder + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# TaskOrder is not significant; trying CorrectAnswer

graph.numhd.lmer <- lmer(LogError ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# CorrectAnswer is barely not significant (p = 0.05447); trying Underestimated

graph.numhd.lmer.underest <- lmer(LogError ~ Underestimated + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.underest)

anova(graph.numhd.lmer.underest)

# Underestimated is highly significant (p = 6.661e-16); trying Overestimated

graph.numhd.lmer.overest <- lmer(LogError ~ Overestimated + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.overest)

anova(graph.numhd.lmer.overest)

# Overestimated is highly significant (p = 1.165e); trying Stats.Q_TotalDuration

graph.numhd.lmer <- lmer(LogError ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Stats.Q_TotalDuration is not significant; trying Stats.dataset_count

graph.numhd.lmer <- lmer(LogError ~ Stats.dataset_count + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Stats.dataset_count is not significant; trying Stats.OperatingSystem

graph.numhd.lmer <- lmer(LogError ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer) # nothing significant

anova(graph.numhd.lmer)

# Stats.OperatingSystem is not significant (skipping combinations); trying StatsNumPixels

graph.numhd.lmer.pixels <- lmer(LogError ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.pixels)

anova(graph.numhd.lmer.pixels)

# StatsNumPixels is barely significant (p=0.02101); trying Demo.age

graph.numhd.lmer.age <- lmer(LogError ~ Demo.age + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.age))), REML = T)

summary(graph.numhd.lmer.age)

anova(graph.numhd.lmer.age)

# Demo.age is significant (p=0.0159); trying Demo.gender

graph.numhd.lmer <- lmer(LogError ~ Demo.gender + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.gender))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.gender is not significant; trying Demo.lang

graph.numhd.lmer <- lmer(LogError ~ Demo.lang + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.lang))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.lang is not significant; trying Demo.educ

graph.numhd.lmer <- lmer(LogError ~ Demo.educ + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.educ))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.educ is not significant; trying Demo.acfield

graph.numhd.lmer <- lmer(LogError ~ Demo.acfield + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.acfield))), REML = T)

summary(graph.numhd.lmer) # nothing significant

anova(graph.numhd.lmer)

# Demo.acfield is not significant (skipping groups); trying Demo.dailytech_Computer

graph.numhd.lmer <- lmer(LogError ~ Demo.dailytech_Computer + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.dailytech_Computer))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.dailytech_Computer is not significant; trying Demo.dailytech_Tablet

graph.numhd.lmer <- lmer(LogError ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.dailytech_Tablet))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.dailytech_Computer is not significant; trying Demo.dailytech_SmartPhone

graph.numhd.lmer <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.dailytech_SmartPhone))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.dailytech_SmartPhone is not significant; trying Demo.weeklygaming

graph.numhd.lmer <- lmer(LogError ~ Demo.weeklygaming + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.weeklygaming))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.weeklygaming is not significant; trying Demo.expdataanal

graph.numhd.lmer <- lmer(LogError ~ Demo.expdataanal + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.expdataanal))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.expdataanal is not significant; trying Demo.expdatavis

graph.numhd.lmer <- lmer(LogError ~ Demo.expdatavis + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.expdatavis))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.expdatavis is not significant; trying Demo.expreadnetvis

graph.numhd.lmer <- lmer(LogError ~ Demo.expreadnetvis + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.expreadnetvis))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.expreadnetvis is not significant; trying Demo.expreadnetvis.alot

graph.numhd.lmer <- lmer(LogError ~ Demo.expreadnetvis.alot + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.expreadnetvis.alot))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.expreadnetvis.alot is not significant; trying Demo.expcreatenetvis

graph.numhd.lmer <- lmer(LogError ~ Demo.expcreatenetvis + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.expcreatenetvis))), REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# Demo.expcreatenetvis is not significant; trying Demo.expcreatenetvis.alot

graph.numhd.lmer <- lmer(LogError ~ Demo.expcreatenetvis.alot + (1|Demo.ResponseID), data=graphics_numhighdeg %>% filter(!(is.na(Demo.expcreatenetvis.alot))), REML = T)

summary(graph.numhd.lmer) # doesn't reach significance

anova(graph.numhd.lmer)

# Demo.expcreatenetvis.alot is not significant; trying AvgDeg

graph.numhd.lmer.avgdeg <- lmer(LogError ~ AvgDeg + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.avgdeg)

anova(graph.numhd.lmer.avgdeg)

# AvgDeg is significant (p=6.871e-08); trying Density

graph.numhd.lmer.dens <- lmer(LogError ~ Density + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.dens)

anova(graph.numhd.lmer.dens)

# Density is significant (p=1.913e-07); trying LargeClust1

graph.numhd.lmer.lgclust <- lmer(LogError ~ LargeClust1 + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.lgclust)

anova(graph.numhd.lmer.lgclust)

# LargeClust1 is significant (p=5.644e-08); trying Modularity

graph.numhd.lmer.mod <- lmer(LogError ~ Modularity + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.mod)

anova(graph.numhd.lmer.mod)

# Modularity is significant (p=0.000103); trying NumClust

graph.numhd.lmer.numclust <- lmer(LogError ~ NumClust + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.numclust)

anova(graph.numhd.lmer.numclust)

# NumClust is significant (p=0.0008824); trying NumHighDegree

graph.numhd.lmer <- lmer(LogError ~ NumHighDegree + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# NumHighDegree is not quite significant (p=0.05447); trying NumLinks

graph.numhd.lmer <- lmer(LogError ~ NumLinks + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# NumLinks is not significant; trying NumNodes

graph.numhd.lmer.numnodes <- lmer(LogError ~ NumNodes + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer.numnodes)

anova(graph.numhd.lmer.numnodes)

# NumNodes is significant (p=0.0009184); trying NumNodesClust1

graph.numhd.lmer <- lmer(LogError ~ NumNodesClust1 + (1|Demo.ResponseID), data=graphics_numhighdeg, REML = T)

summary(graph.numhd.lmer)

anova(graph.numhd.lmer)

# NumNodesClust1 is not quite significant (p=0.05285); 
```

``` r
temp <- graphics_numhighdeg %>% dplyr::select(
  Demo.ResponseID, LogError, Condition, Ctrl_dummy, ConditionPhrasing, ConditionGraphics, ConditionColor, Dataset, DatasetOrder, Underestimated, Overestimated, StatsNumPixels, Demo.age, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumNodes) %>% drop_na()

temp <- temp %>% mutate(StatsNumPixels=scale(StatsNumPixels))

temp.numhd.gr <- temp %>% mutate(DatasetOrder=factor(DatasetOrder))
```

Run this one

``` r
# combinations of individual predictors

graph.numhd.lmer.full <- lmer(LogError ~ Condition + 
                                Dataset +
                                DatasetOrder +
                                Overestimated +
                                StatsNumPixels +
                                (1|Demo.ResponseID), 
                              data=temp, REML = T)

summary(graph.numhd.lmer.full)

anova(graph.numhd.lmer.full)
```

Run this one

``` r
# interactions

graph.numhd.lmer.full.int <- lmer(LogError ~ Condition + 
                                    Dataset +
                                    DatasetOrder +
                                    Overestimated +
                                    StatsNumPixels +
                                    Condition:Dataset +
                                    (1|Demo.ResponseID), 
                                  data=temp, REML = T)

summary(graph.numhd.lmer.full.int)

anova(graph.numhd.lmer.full.int)

anova(graph.numhd.lmer.full.int,graph.numhd.lmer.full)

# definitely keep the interaction

graph.numhd.lmer.full.int.2 <- lmer(LogError ~ Condition + 
                                    Dataset +
                                    DatasetOrder +
                                    Overestimated +
                                    StatsNumPixels +
                                    Condition:Dataset +
                                      Dataset:Overestimated +
                                    (1|Demo.ResponseID), 
                                  data=temp, REML = T)

summary(graph.numhd.lmer.full.int.2)

anova(graph.numhd.lmer.full.int.2)

anova(graph.numhd.lmer.full.int.2,graph.numhd.lmer.full.int)

graph.numhd.lmer.full.int.3 <- lmer(LogError ~ Condition + 
                                    Dataset +
                                    DatasetOrder +
                                    Overestimated +
                                    StatsNumPixels +
                                    Condition:Dataset +
                                      Dataset:Overestimated +
                                      DatasetOrder:Overestimated +
                                    (1|Demo.ResponseID), 
                                  data=temp, REML = T)

summary(graph.numhd.lmer.full.int.3)

anova(graph.numhd.lmer.full.int.3)

anova(graph.numhd.lmer.full.int.3,graph.numhd.lmer.full.int.2)

# keeping graph.numhd.lmer.full.int.3

graph.numhd.lmer.full.int.4 <- lmer(LogError ~ Condition + 
                                    Dataset +
                                    Condition:Dataset +
                                      Dataset:Overestimated +
                                      DatasetOrder:Overestimated +
                                    (1|Demo.ResponseID), 
                                  data=temp, REML = T)

summary(graph.numhd.lmer.full.int.4)

anova(graph.numhd.lmer.full.int.4)

anova(graph.numhd.lmer.full.int.4, graph.numhd.lmer.full.int.3)

# no significant difference and 4 is way simpler, so keeping 4

graph.numhd.lmer.full.int.5 <- lmer(LogError ~ Condition + 
                                    Dataset +
                                    Condition:Dataset +
                                      Dataset:Overestimated +
                                      factor(DatasetOrder):Overestimated +
                                    (1|Demo.ResponseID), 
                                  data=temp, REML = T)

summary(graph.numhd.lmer.full.int.5)

anova(graph.numhd.lmer.full.int.5)

anova(graph.numhd.lmer.full.int.4)

anova(graph.numhd.lmer.full.int.4, graph.numhd.lmer.full.int.5)

# should keep 5, where datasetorder is treated as a factor; only a slight difference between 4 and 5

# redoing 5 with factor in the dataset

graph.numhd.lmer.full.int.6 <- lmer(LogError ~ Condition + 
                                    Dataset +
                                    Condition:Dataset +
                                      Dataset:Overestimated +
                                      DatasetOrder:Overestimated +
                                    (1|Demo.ResponseID), 
                                  data=temp.numhd.gr, REML = T)

summary(graph.numhd.lmer.full.int.6)

anova(graph.numhd.lmer.full.int.6)
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.numhd.lmer.full.int.6, 
     file = file.path(analysisDataDir,"fits/graph_numhd_lmer_int.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_numhd_lmer_int.RData"))
```

``` r
rand(graph.numhd.lmer.full.int.6)
```

    ## Analysis of Random effects Table:
    ##                 Chi.sq Chi.DF p.value
    ## Demo.ResponseID   -1.9      1       1

``` r
# result shows that random effects of participant are *not* significant (p=1)

#ranef(graph.numhd.lmer.full.int.4)

# displays the random effects; not that useful

# unlike lme(), lmer() doesn't allow for heterogeneous error variance structures (the "weights")

(r2nsj = r2beta(graph.numhd.lmer.full.int.6, method = 'nsj', partial = TRUE))[1,'Rsq']
```

    ## [1] 0.3905217

``` r
ggplot(graphics_numhighdeg) + geom_histogram(aes(LogError), binwidth=.005) + labs(title="Distribution of LogError values for Degree of Highest Degree Node task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-224-1.png)<!-- -->

``` r
plot(graph.numhd.lmer.full.int.6)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-225-1.png)<!-- -->

``` r
plot(graph.numhd.lmer.full.int.6, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-225-2.png)<!-- -->

``` r
plot(graph.numhd.lmer.full.int.6, resid(.) ~ fitted(.) | Condition, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-225-3.png)<!-- -->

``` r
plot(graph.numhd.lmer.full.int.6, resid(., scaled=TRUE) ~ fitted(.) | Condition, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-225-4.png)<!-- -->

``` r
plot(graph.numhd.lmer.full.int.6, LogError ~ fitted(.), abline = c(0,1))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-225-5.png)<!-- -->

``` r
graph.numhd.lmer.full.int.6.f <- fortify(graph.numhd.lmer.full.int.6)

ggplot(graph.numhd.lmer.full.int.6.f, aes(.fitted,.resid)) + 
  geom_point() +
  #facet_grid(.~Sex) + 
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-226-1.png)<!-- -->

``` r
ggplot(graph.numhd.lmer.full.int.6.f, aes(.fitted,LogError)) + 
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-226-2.png)<!-- -->

``` r
ggplot(graph.numhd.lmer.full.int.6.f, aes(LogError,.fitted)) +
geom_point() +
geom_abline(aes(slope = 1, intercept = 0)) +
#scale_y_continuous(limits=c(0,0.3)) +
labs(title="Real vs. Predicted LogError values for Degree of Highest Degree Node task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-226-3.png)<!-- -->

``` r
# TO DO: check out interpretation for these plots??

prof <-  profile(graph.numhd.lmer.full.int.4, optimizer="Nelder_Mead", which="beta_")

prof.CI <- confint(prof)

CI2 <- confint(graph.numhd.lmer.full.int.4, maxpts = 8)

xyplot(prof)

xyplot(prof, absVal = TRUE)

xyplot(prof, conf = c(0.95, 0.99), main = "95% and 99% profile() intervals")

# can also apply logProf() and varianceProf() to profile object

densityplot(prof)

splom(prof)
```

#### Least Squares Means

Do for each categorical predictor.

Final model: LogError ~ Condition + Dataset + Condition:Dataset +
Dataset:Overestimated + DatasetOrder:Overestimated + (1|Demo.ResponseID)

##### Condition

###### lsmeans - older than, e.g., numnodes

``` r
# trying lmerTest::lsmeansLT

# note = lmerTest::lsmeans will only report lsmeans for factor variables and is deprecated

graph.numhd.lsmlt.cond.2 <- lsmeansLT(graph.numhd.lmer.full.int.4, test.effs = "Condition")

plot(graph.numhd.lsmlt.cond.2) # not a completely terrible style, but not sorted properly

graph.numhd.lsmlt.cond.2.df <- dplyr::as_data_frame(graph.numhd.lsmlt.cond.2$lsmeans.table)

graph.numhd.lsmlt.cond.2.df

graph.numhd.lsmlt.cond.2.df$Condition <- factor(graph.numhd.lsmlt.cond.2.df$Condition, levels=graph.numhd.lsmlt.cond.2.df %>% arrange(desc(Estimate)) %>% dplyr::select(Condition) %>% unlist())

graph.numhd.lsmlt.cond.2.df %>% arrange(desc(Estimate))

graph.numhd.lsmlt.cond.2.df <- graph.numhd.lsmlt.cond.2.df %>% 
  mutate(sig.levels = factor(case_when(
    `p-value` < .0001 ~ sig.level.names[1],
    `p-value` < .001 ~ sig.level.names[2],
    `p-value` < .01 ~ sig.level.names[3],
    `p-value` < .05 ~ sig.level.names[4],
    TRUE ~ sig.level.names[5]
    )
  ,levels=sig.level.names,ordered=TRUE))

graph.numhd.lsmlt.cond.2.df

ggplot(graph.numhd.lsmlt.cond.2.df) +
  geom_errorbar(aes(x=Condition,ymax=`Upper CI`,ymin=`Lower CI`), width=.2) +
  geom_point(aes(x=Condition,y=Estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

``` r
# LSMeans Difference data

graph.numhd.difflsmlt.cond <- difflsmeans(graph.numhd.lmer.full.int.4, test.effs = "Condition")

plot(graph.numhd.difflsmlt.cond)

graph.numhd.difflsmlt.cond.df <- dplyr::as_data_frame(graph.numhd.difflsmlt.cond$diffs.lsmeans.table, rownames="Pair")

graph.numhd.difflsmlt.cond.df

graph.numhd.difflsmlt.cond.df <- graph.numhd.difflsmlt.cond.df %>% mutate(Pair=sub("Condition ","",Pair)) %>% separate(Pair, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

copy <- graph.numhd.difflsmlt.cond.df %>% rename(From=To,To=From)

graph.numhd.difflsmlt.cond.df <- bind_rows(graph.numhd.difflsmlt.cond.df,copy)

graph.numhd.difflsmlt.cond.df$Pair <- factor(graph.numhd.difflsmlt.cond.df$Pair, levels=graph.numhd.difflsmlt.cond.df %>% arrange(desc(Estimate)) %>% dplyr::select(Pair) %>% distinct() %>% unlist())

cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numhd.difflsmlt.cond.df$From <- factor(graph.numhd.difflsmlt.cond.df$From, levels=cond.lev)
graph.numhd.difflsmlt.cond.df$To <- factor(graph.numhd.difflsmlt.cond.df$To, levels=cond.lev)

graph.numhd.difflsmlt.cond.df %>% arrange(desc(Estimate))

graph.numhd.difflsmlt.cond.df <- graph.numhd.difflsmlt.cond.df %>% 
  mutate(sig.levels = factor(case_when(
    `p-value` < .0001 ~ sig.level.names[1],
    `p-value` < .001 ~ sig.level.names[2],
    `p-value` < .01 ~ sig.level.names[3],
    `p-value` < .05 ~ sig.level.names[4],
    TRUE ~ sig.level.names[5]
    )
  ,levels=sig.level.names,ordered=TRUE))

ggplot(graph.numhd.difflsmlt.cond.df) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_point(aes(x=Pair,y=Estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()

ggplot(graph.numhd.difflsmlt.cond.df) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_point(aes(x=Pair,y=Estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()

ggplot(graph.numhd.difflsmlt.cond.df) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(graph.numhd.difflsmlt.cond.df) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(Estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numhd.emm.cond <- emmeans(graph.numhd.lmer.full.int.6, "Condition", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numhd.emm.cond
```

    ##  Condition     emmean          SE     df   lower.CL   upper.CL
    ##  Ctrl      0.06571059 0.002788662 466.93 0.06023071 0.07119047
    ##  Col       0.04804449 0.002660687 494.28 0.04281683 0.05327214
    ##  Phr       0.04971700 0.002638317 484.40 0.04453304 0.05490096
    ##  Siz       0.06195290 0.002664543 477.31 0.05671721 0.06718859
    ## 
    ## Results are averaged over the levels of: Dataset, Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numhd.emm.cond.df <- dplyr::as_data_frame(graph.numhd.emm.cond)

graph.numhd.emm.cond.df
```

    ## # A tibble: 4 x 6
    ##   Condition emmean      SE    df lower.CL upper.CL
    ## * <fct>      <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 Ctrl      0.0657 0.00279   467   0.0602   0.0712
    ## 2 Col       0.0480 0.00266   494   0.0428   0.0533
    ## 3 Phr       0.0497 0.00264   484   0.0445   0.0549
    ## 4 Siz       0.0620 0.00266   477   0.0567   0.0672

``` r
graph.numhd.emm.cond.cld <- cld(graph.numhd.emm.cond,
                                   details=TRUE,
                                   #alpha=0.01,
                                   #by="Dataset",
                                   #Letters="|||||||||||||||||||",
                                   sort=TRUE
)

graph.numhd.emm.cond.cld.df <- graph.numhd.emm.cond.cld$emmeans

graph.numhd.emm.cond.cld.df %>% dplyr::select(Condition,.group)
```

    ##  Condition .group
    ##  Col        1    
    ##  Phr        1    
    ##  Siz         2   
    ##  Ctrl        2   
    ## 
    ## Results are averaged over the levels of: Dataset, Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.cond.cld.df
```

    ##  Condition     emmean          SE     df   lower.CL   upper.CL .group
    ##  Col       0.04804449 0.002660687 494.28 0.04281683 0.05327214  1    
    ##  Phr       0.04971700 0.002638317 484.40 0.04453304 0.05490096  1    
    ##  Siz       0.06195290 0.002664543 477.31 0.05671721 0.06718859   2   
    ##  Ctrl      0.06571059 0.002788662 466.93 0.06023071 0.07119047   2   
    ## 
    ## Results are averaged over the levels of: Dataset, Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.cond.cld.df$Condition <- factor(graph.numhd.emm.cond.cld.df$Condition, levels=graph.numhd.emm.cond.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())


#emmip(graph.numhd.lmer.full.int.4, ~Condition, CIs = TRUE)
emmip(graph.numhd.emm.cond, ~Condition, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-230-1.png)<!-- -->

``` r
plot(graph.numhd.emm.cond)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-230-2.png)<!-- -->

``` r
#plot(graph.numhd.emm.cond, comparisons = TRUE)

graph.numhd.emm.cond.cld.df %>% arrange(desc(emmean))
```

    ##   Condition     emmean          SE       df   lower.CL   upper.CL .group
    ## 1      Ctrl 0.06571059 0.002788662 466.9329 0.06023071 0.07119047      2
    ## 2       Siz 0.06195290 0.002664543 477.3052 0.05671721 0.06718859      2
    ## 3       Phr 0.04971700 0.002638317 484.3977 0.04453304 0.05490096     1 
    ## 4       Col 0.04804449 0.002660687 494.2778 0.04281683 0.05327214     1

``` r
ggplot(graph.numhd.emm.cond.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Condition for Degree of\nHighest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-230-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numhd.lmer.full.int.4), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numhd.emm.cond.pairs <- dplyr::as_data_frame(pairs(graph.numhd.emm.cond)) 
graph.numhd.emm.cond.pairs
```

    ## # A tibble: 6 x 6
    ##   contrast   estimate      SE    df t.ratio    p.value
    ## * <fct>         <dbl>   <dbl> <dbl>   <dbl>      <dbl>
    ## 1 Ctrl - Col  0.0177  0.00350   382   5.05  0.00000416
    ## 2 Ctrl - Phr  0.0160  0.00350   381   4.57  0.0000382 
    ## 3 Ctrl - Siz  0.00376 0.00353   381   1.06  0.711     
    ## 4 Col - Phr  -0.00167 0.00338   385  -0.495 0.960     
    ## 5 Col - Siz  -0.0139  0.00340   382  -4.09  0.000312  
    ## 6 Phr - Siz  -0.0122  0.00340   383  -3.60  0.00207

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

cond.pairs.CI <- confint(pairs(graph.numhd.emm.cond))

graph.numhd.emm.cond.pairs <- full_join(graph.numhd.emm.cond.pairs, cond.pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numhd.emm.cond))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-231-1.png)<!-- -->

``` r
plot(pairs(graph.numhd.emm.cond), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-231-2.png)<!-- -->

``` r
graph.numhd.emm.cond.pairs$sig.levels <- 
  case_when(graph.numhd.emm.cond.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numhd.emm.cond.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numhd.emm.cond.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numhd.emm.cond.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numhd.emm.cond.pairs$sig.levels <- factor(graph.numhd.emm.cond.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numhd.emm.cond.pairs$contrast <- factor(graph.numhd.emm.cond.pairs$contrast, levels=graph.numhd.emm.cond.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numhd.emm.cond.pairs <- graph.numhd.emm.cond.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

ggplot(graph.numhd.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-231-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-231-4.png)<!-- -->

``` r
cond.copy <- graph.numhd.emm.cond.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numhd.emm.cond.pairs.compl <- bind_rows(graph.numhd.emm.cond.pairs, cond.copy)


cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numhd.emm.cond.pairs.compl$From <- factor(graph.numhd.emm.cond.pairs.compl$From, levels=cond.lev)
graph.numhd.emm.cond.pairs.compl$To <- factor(graph.numhd.emm.cond.pairs.compl$To, levels=cond.lev)

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numhd.emm.cond.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 12 x 11
    ##    contrast   From  To    estimate      SE    df t.ratio  p.value lower.CL
    ##    <fct>      <fct> <fct>    <dbl>   <dbl> <dbl>   <dbl>    <dbl>    <dbl>
    ##  1 Ctrl - Col Col   Ctrl  -0.0177  0.00350   382  -5.05   4.16e⁻⁶ -0.00863
    ##  2 Ctrl - Phr Phr   Ctrl  -0.0160  0.00350   381  -4.57   3.82e⁻⁵ -0.00697
    ##  3 Col - Siz  Col   Siz   -0.0139  0.00340   382  -4.09   3.12e⁻⁴ -0.0227 
    ##  4 Phr - Siz  Phr   Siz   -0.0122  0.00340   383  -3.60   2.07e⁻³ -0.0210 
    ##  5 Ctrl - Siz Siz   Ctrl  -0.00376 0.00353   381  -1.06   7.11e⁻¹  0.00535
    ##  6 Col - Phr  Col   Phr   -0.00167 0.00338   385  -0.495  9.60e⁻¹ -0.0104 
    ##  7 Col - Phr  Phr   Col    0.00167 0.00338   385   0.495  9.60e⁻¹  0.0104 
    ##  8 Ctrl - Siz Ctrl  Siz    0.00376 0.00353   381   1.06   7.11e⁻¹ -0.00535
    ##  9 Phr - Siz  Siz   Phr    0.0122  0.00340   383   3.60   2.07e⁻³  0.0210 
    ## 10 Col - Siz  Siz   Col    0.0139  0.00340   382   4.09   3.12e⁻⁴  0.0227 
    ## 11 Ctrl - Phr Ctrl  Phr    0.0160  0.00350   381   4.57   3.82e⁻⁵  0.00697
    ## 12 Ctrl - Col Ctrl  Col    0.0177  0.00350   382   5.05   4.16e⁻⁶  0.00863
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numhd.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-232-1.png)<!-- -->

``` r
ggplot(graph.numhd.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-232-2.png)<!-- -->

``` r
ggplot(graph.numhd.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-232-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-232-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.numhd.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*50
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.numhd.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-234-1.png)<!-- -->

##### Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numhd.emm.data <- emmeans(graph.numhd.lmer.full.int.6, "Dataset", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numhd.emm.data
```

    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.03668831 0.002912449 1136.60 0.03097393 0.04240269
    ##  3       0.06404720 0.003436674 1135.58 0.05730426 0.07079014
    ##  5       0.08142484 0.005783521 1130.54 0.07007720 0.09277248
    ##  7       0.04672951 0.003971105 1134.68 0.03893797 0.05452104
    ##  8       0.04483009 0.003001160 1136.39 0.03894165 0.05071852
    ##  9       0.06441751 0.003665314 1134.84 0.05722596 0.07160906
    ## 
    ## Results are averaged over the levels of: Condition, Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numhd.emm.data.df <- dplyr::as_data_frame(graph.numhd.emm.data)

graph.numhd.emm.data.df
```

    ## # A tibble: 6 x 6
    ##   Dataset emmean      SE    df lower.CL upper.CL
    ## * <fct>    <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1       0.0367 0.00291  1137   0.0310   0.0424
    ## 2 3       0.0640 0.00344  1136   0.0573   0.0708
    ## 3 5       0.0814 0.00578  1131   0.0701   0.0928
    ## 4 7       0.0467 0.00397  1135   0.0389   0.0545
    ## 5 8       0.0448 0.00300  1136   0.0389   0.0507
    ## 6 9       0.0644 0.00367  1135   0.0572   0.0716

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numhd.emm.data.cld <- cld(graph.numhd.emm.data,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numhd.emm.data.cld.df <- graph.numhd.emm.data.cld$emmeans

graph.numhd.emm.data.cld.df %>% dplyr::select(Dataset,.group)
```

    ##  Dataset .group
    ##  1        1    
    ##  8        1    
    ##  7        1    
    ##  3         2   
    ##  9         2   
    ##  5         2   
    ## 
    ## Results are averaged over the levels of: Condition, Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.data.cld.df
```

    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  1       0.03668831 0.002912449 1136.60 0.03097393 0.04240269  1    
    ##  8       0.04483009 0.003001160 1136.39 0.03894165 0.05071852  1    
    ##  7       0.04672951 0.003971105 1134.68 0.03893797 0.05452104  1    
    ##  3       0.06404720 0.003436674 1135.58 0.05730426 0.07079014   2   
    ##  9       0.06441751 0.003665314 1134.84 0.05722596 0.07160906   2   
    ##  5       0.08142484 0.005783521 1130.54 0.07007720 0.09277248   2   
    ## 
    ## Results are averaged over the levels of: Condition, Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.data.cld.df$Dataset <- factor(graph.numhd.emm.data.cld.df$Dataset, levels=graph.numhd.emm.data.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Dataset) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numhd.emm.data, ~Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-235-1.png)<!-- -->

``` r
plot(graph.numhd.emm.data)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-235-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numhd.emm.data.cld.df %>% arrange(desc(emmean))
```

    ##   Dataset     emmean          SE       df   lower.CL   upper.CL .group
    ## 1       5 0.08142484 0.005783521 1130.542 0.07007720 0.09277248      2
    ## 2       9 0.06441751 0.003665314 1134.845 0.05722596 0.07160906      2
    ## 3       3 0.06404720 0.003436674 1135.579 0.05730426 0.07079014      2
    ## 4       7 0.04672951 0.003971105 1134.678 0.03893797 0.05452104     1 
    ## 5       8 0.04483009 0.003001160 1136.387 0.03894165 0.05071852     1 
    ## 6       1 0.03668831 0.002912449 1136.596 0.03097393 0.04240269     1

``` r
ggplot(graph.numhd.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
  labs(title="Estimated Marginal Means for Dataset for Degree of\nHighest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-235-3.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numhd.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.numhd.emm.data)) 
graph.numhd.emm.data.pairs
```

    ## # A tibble: 15 x 6
    ##    contrast  estimate      SE    df t.ratio         p.value
    ##  * <fct>        <dbl>   <dbl> <dbl>   <dbl>           <dbl>
    ##  1 1 - 3    -0.0274   0.00447  1069 -6.13   0.0000000188   
    ##  2 1 - 5    -0.0447   0.00645  1101 -6.94   0.0000000000996
    ##  3 1 - 7    -0.0100   0.00488  1054 -2.06   0.311          
    ##  4 1 - 8    -0.00814  0.00413  1050 -1.97   0.360          
    ##  5 1 - 9    -0.0277   0.00462  1065 -6.00   0.0000000396   
    ##  6 3 - 5    -0.0174   0.00669  1097 -2.60   0.0988         
    ##  7 3 - 7     0.0173   0.00520  1072  3.33   0.0115         
    ##  8 3 - 8     0.0192   0.00451  1061  4.26   0.000324       
    ##  9 3 - 9    -0.000370 0.00496  1045 -0.0746 1.000          
    ## 10 5 - 7     0.0347   0.00698  1095  4.97   0.0000115      
    ## 11 5 - 8     0.0366   0.00647  1084  5.65   0.000000302    
    ## 12 5 - 9     0.0170   0.00683  1106  2.49   0.128          
    ## 13 7 - 8     0.00190  0.00491  1038  0.387  0.999          
    ## 14 7 - 9    -0.0177   0.00534  1057 -3.31   0.0123         
    ## 15 8 - 9    -0.0196   0.00467  1047 -4.19   0.000433

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numhd.emm.data))

graph.numhd.emm.data.pairs <- full_join(graph.numhd.emm.data.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numhd.emm.data))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-236-1.png)<!-- -->

``` r
plot(pairs(graph.numhd.emm.data), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-236-2.png)<!-- -->

``` r
graph.numhd.emm.data.pairs$sig.levels <- 
  case_when(graph.numhd.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numhd.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numhd.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numhd.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numhd.emm.data.pairs$sig.levels <- factor(graph.numhd.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numhd.emm.data.pairs$contrast <- factor(graph.numhd.emm.data.pairs$contrast, levels=graph.numhd.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numhd.emm.data.pairs <- graph.numhd.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numhd.emm.data.pairs %>% arrange(estimate)
```

    ## # A tibble: 15 x 11
    ##    contrast From  To     estimate      SE    df t.ratio   p.value lower.CL
    ##    <fct>    <chr> <chr>     <dbl>   <dbl> <dbl>   <dbl>     <dbl>    <dbl>
    ##  1 1 - 5    1     5     -0.0447   0.00645  1101 -6.94    9.96e⁻¹¹ -0.0631 
    ##  2 1 - 9    1     9     -0.0277   0.00462  1065 -6.00    3.96e⁻ ⁸ -0.0409 
    ##  3 1 - 3    1     3     -0.0274   0.00447  1069 -6.13    1.88e⁻ ⁸ -0.0401 
    ##  4 8 - 9    8     9     -0.0196   0.00467  1047 -4.19    4.33e⁻ ⁴ -0.0329 
    ##  5 7 - 9    7     9     -0.0177   0.00534  1057 -3.31    1.23e⁻ ² -0.0329 
    ##  6 3 - 5    3     5     -0.0174   0.00669  1097 -2.60    9.88e⁻ ² -0.0365 
    ##  7 1 - 7    1     7     -0.0100   0.00488  1054 -2.06    3.11e⁻ ¹ -0.0240 
    ##  8 1 - 8    1     8     -0.00814  0.00413  1050 -1.97    3.60e⁻ ¹ -0.0199 
    ##  9 3 - 9    3     9     -0.000370 0.00496  1045 -0.0746 10.00e⁻ ¹ -0.0145 
    ## 10 7 - 8    7     8      0.00190  0.00491  1038  0.387   9.99e⁻ ¹ -0.0121 
    ## 11 5 - 9    5     9      0.0170   0.00683  1106  2.49    1.28e⁻ ¹ -0.00250
    ## 12 3 - 7    3     7      0.0173   0.00520  1072  3.33    1.15e⁻ ²  0.00248
    ## 13 3 - 8    3     8      0.0192   0.00451  1061  4.26    3.24e⁻ ⁴  0.00633
    ## 14 5 - 7    5     7      0.0347   0.00698  1095  4.97    1.15e⁻ ⁵  0.0148 
    ## 15 5 - 8    5     8      0.0366   0.00647  1084  5.65    3.02e⁻ ⁷  0.0181 
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numhd.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-236-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-236-4.png)<!-- -->

``` r
data.copy <- graph.numhd.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numhd.emm.data.pairs.compl <- bind_rows(graph.numhd.emm.data.pairs, data.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=cond.lev)
#graph.numlinks.emm.data.pairs.compl$To <- factor(graph.numlinks.emm.data.pairs.compl$To, levels=cond.lev)

graph.numhd.emm.data.pairs.compl$From <- factor(graph.numhd.emm.data.pairs.compl$From, levels=rev(unique(graph.numhd.emm.data.pairs.compl$From)))

#graph.numhd.emm.data.pairs.compl %>% arrange(desc(estimate))
graph.numhd.emm.data.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 30 x 11
    ##    contrast From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##    <fct>    <fct> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ##  1 1 - 5    1     5      -0.0447 0.00645  1101   -6.94   9.96e⁻¹¹ -0.0631 
    ##  2 5 - 8    8     5      -0.0366 0.00647  1084   -5.65   3.02e⁻ ⁷ -0.0181 
    ##  3 5 - 7    7     5      -0.0347 0.00698  1095   -4.97   1.15e⁻ ⁵ -0.0148 
    ##  4 1 - 9    1     9      -0.0277 0.00462  1065   -6.00   3.96e⁻ ⁸ -0.0409 
    ##  5 1 - 3    1     3      -0.0274 0.00447  1069   -6.13   1.88e⁻ ⁸ -0.0401 
    ##  6 8 - 9    8     9      -0.0196 0.00467  1047   -4.19   4.33e⁻ ⁴ -0.0329 
    ##  7 3 - 8    8     3      -0.0192 0.00451  1061   -4.26   3.24e⁻ ⁴ -0.00633
    ##  8 7 - 9    7     9      -0.0177 0.00534  1057   -3.31   1.23e⁻ ² -0.0329 
    ##  9 3 - 5    3     5      -0.0174 0.00669  1097   -2.60   9.88e⁻ ² -0.0365 
    ## 10 3 - 7    7     3      -0.0173 0.00520  1072   -3.33   1.15e⁻ ² -0.00248
    ## # ... with 20 more rows, and 2 more variables: upper.CL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numhd.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-237-1.png)<!-- -->

``` r
ggplot(graph.numhd.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-237-2.png)<!-- -->

``` r
ggplot(graph.numhd.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-237-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-237-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.numhd.emm.data.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*50
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.numhd.emm.data.pairs %>% dplyr::select(-contrast), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-239-1.png)<!-- -->

##### Condition:Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numhd.emm.conddata <- emmeans(graph.numhd.lmer.full.int.6, ~ Dataset | Condition, nesting = NULL)
graph.numhd.emm.conddata.2 <- emmeans(graph.numhd.lmer.full.int.6, ~ Condition | Dataset, nesting = NULL)

graph.numhd.emm.conddata
```

    ## Condition = Ctrl:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.027173560 0.005868562 1136.80  0.015659131 0.03868799
    ##  3       0.064951523 0.005953953 1136.83  0.053269553 0.07663349
    ##  5       0.128477457 0.007768985 1134.12  0.113234259 0.14372066
    ##  7       0.071322914 0.006491973 1136.27  0.058585312 0.08406052
    ##  8       0.042504611 0.005866933 1136.83  0.030993379 0.05401584
    ##  9       0.059833456 0.006494374 1136.23  0.047091143 0.07257577
    ## 
    ## Condition = Col:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.036248157 0.005484123 1136.67  0.025488016 0.04700830
    ##  3       0.073369707 0.005974544 1136.33  0.061647329 0.08509209
    ##  5       0.023676580 0.007421705 1134.19  0.009114767 0.03823839
    ##  7       0.002797481 0.006271570 1136.18 -0.009507679 0.01510264
    ##  8       0.064500225 0.005746481 1136.45  0.053225320 0.07577513
    ##  9       0.087674762 0.005858705 1136.45  0.076179668 0.09916986
    ## 
    ## Condition = Phr:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.053352908 0.005718456 1136.57  0.042132991 0.06457282
    ##  3       0.052509739 0.005814669 1136.51  0.041101047 0.06391843
    ##  5       0.071597163 0.007303895 1134.48  0.057266502 0.08592782
    ##  7       0.015550694 0.006115565 1136.07  0.003551622 0.02754976
    ##  8       0.037268860 0.005529273 1136.76  0.026420134 0.04811759
    ##  9       0.068022621 0.006031059 1135.80  0.056189353 0.07985589
    ## 
    ## Condition = Siz:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.029978621 0.005534339 1136.91  0.019119955 0.04083729
    ##  3       0.065357831 0.005899221 1136.49  0.053783243 0.07693242
    ##  5       0.101948164 0.007581929 1134.04  0.087071978 0.11682435
    ##  7       0.097246936 0.006196729 1136.29  0.085088620 0.10940525
    ##  8       0.035046645 0.005584760 1136.83  0.024089051 0.04600424
    ##  9       0.042139201 0.006013195 1136.58  0.030340990 0.05393741
    ## 
    ## Results are averaged over the levels of: Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numhd.emm.conddata.df <- dplyr::as_data_frame(graph.numhd.emm.conddata)

graph.numhd.emm.conddata.df #%>% View()
```

    ## # A tibble: 24 x 7
    ##    Dataset Condition  emmean      SE    df lower.CL upper.CL
    ##  * <fct>   <fct>       <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ##  1 1       Ctrl      0.0272  0.00587  1137  0.0157    0.0387
    ##  2 3       Ctrl      0.0650  0.00595  1137  0.0533    0.0766
    ##  3 5       Ctrl      0.128   0.00777  1134  0.113     0.144 
    ##  4 7       Ctrl      0.0713  0.00649  1136  0.0586    0.0841
    ##  5 8       Ctrl      0.0425  0.00587  1137  0.0310    0.0540
    ##  6 9       Ctrl      0.0598  0.00649  1136  0.0471    0.0726
    ##  7 1       Col       0.0362  0.00548  1137  0.0255    0.0470
    ##  8 3       Col       0.0734  0.00597  1136  0.0616    0.0851
    ##  9 5       Col       0.0237  0.00742  1134  0.00911   0.0382
    ## 10 7       Col       0.00280 0.00627  1136 -0.00951   0.0151
    ## # ... with 14 more rows

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numhd.emm.conddata.cld <- cld(graph.numhd.emm.conddata,
                                       details=TRUE,
                                       #alpha=0.01,
                                       #by="Dataset",
                                       #Letters="|||||||||||||||||||",
                                       sort=TRUE
)

graph.numhd.emm.conddata.cld[["emmeans"]]
```

    ## Condition = Ctrl:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  1       0.027173560 0.005868562 1136.80  0.015659131 0.03868799  1    
    ##  8       0.042504611 0.005866933 1136.83  0.030993379 0.05401584  12   
    ##  9       0.059833456 0.006494374 1136.23  0.047091143 0.07257577   23  
    ##  3       0.064951523 0.005953953 1136.83  0.053269553 0.07663349   23  
    ##  7       0.071322914 0.006491973 1136.27  0.058585312 0.08406052    3  
    ##  5       0.128477457 0.007768985 1134.12  0.113234259 0.14372066     4 
    ## 
    ## Condition = Col:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  7       0.002797481 0.006271570 1136.18 -0.009507679 0.01510264  1    
    ##  5       0.023676580 0.007421705 1134.19  0.009114767 0.03823839  12   
    ##  1       0.036248157 0.005484123 1136.67  0.025488016 0.04700830   2   
    ##  8       0.064500225 0.005746481 1136.45  0.053225320 0.07577513    3  
    ##  3       0.073369707 0.005974544 1136.33  0.061647329 0.08509209    34 
    ##  9       0.087674762 0.005858705 1136.45  0.076179668 0.09916986     4 
    ## 
    ## Condition = Phr:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  7       0.015550694 0.006115565 1136.07  0.003551622 0.02754976  1    
    ##  8       0.037268860 0.005529273 1136.76  0.026420134 0.04811759  12   
    ##  3       0.052509739 0.005814669 1136.51  0.041101047 0.06391843   23  
    ##  1       0.053352908 0.005718456 1136.57  0.042132991 0.06457282   23  
    ##  9       0.068022621 0.006031059 1135.80  0.056189353 0.07985589    3  
    ##  5       0.071597163 0.007303895 1134.48  0.057266502 0.08592782    3  
    ## 
    ## Condition = Siz:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  1       0.029978621 0.005534339 1136.91  0.019119955 0.04083729  1    
    ##  8       0.035046645 0.005584760 1136.83  0.024089051 0.04600424  1    
    ##  9       0.042139201 0.006013195 1136.58  0.030340990 0.05393741  12   
    ##  3       0.065357831 0.005899221 1136.49  0.053783243 0.07693242   2   
    ##  7       0.097246936 0.006196729 1136.29  0.085088620 0.10940525    3  
    ##  5       0.101948164 0.007581929 1134.04  0.087071978 0.11682435    3  
    ## 
    ## Results are averaged over the levels of: Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.conddata.cld.df <- graph.numhd.emm.conddata.cld[["emmeans"]]

graph.numhd.emm.conddata.cld.df %>% dplyr::select(Dataset,Condition,.group) %>% print()
```

    ## Condition = Ctrl:
    ##  Dataset .group
    ##  1        1    
    ##  8        12   
    ##  9         23  
    ##  3         23  
    ##  7          3  
    ##  5           4 
    ## 
    ## Condition = Col:
    ##  Dataset .group
    ##  7        1    
    ##  5        12   
    ##  1         2   
    ##  8          3  
    ##  3          34 
    ##  9           4 
    ## 
    ## Condition = Phr:
    ##  Dataset .group
    ##  7        1    
    ##  8        12   
    ##  3         23  
    ##  1         23  
    ##  9          3  
    ##  5          3  
    ## 
    ## Condition = Siz:
    ##  Dataset .group
    ##  1        1    
    ##  8        1    
    ##  9        12   
    ##  3         2   
    ##  7          3  
    ##  5          3  
    ## 
    ## Results are averaged over the levels of: Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numhd.emm.conddata.2, details=TRUE, sort=TRUE)[["emmeans"]] %>% dplyr::select(Dataset,Condition,.group) %>% print()
```

    ## Dataset = 1:
    ##  Condition .group
    ##  Ctrl       1    
    ##  Siz        1    
    ##  Col        12   
    ##  Phr         2   
    ## 
    ## Dataset = 3:
    ##  Condition .group
    ##  Phr        1    
    ##  Ctrl       12   
    ##  Siz        12   
    ##  Col         2   
    ## 
    ## Dataset = 5:
    ##  Condition .group
    ##  Col        1    
    ##  Phr         2   
    ##  Siz          3  
    ##  Ctrl          4 
    ## 
    ## Dataset = 7:
    ##  Condition .group
    ##  Col        1    
    ##  Phr        1    
    ##  Ctrl        2   
    ##  Siz          3  
    ## 
    ## Dataset = 8:
    ##  Condition .group
    ##  Siz        1    
    ##  Phr        1    
    ##  Ctrl       1    
    ##  Col         2   
    ## 
    ## Dataset = 9:
    ##  Condition .group
    ##  Siz        1    
    ##  Ctrl       12   
    ##  Phr         23  
    ##  Col          3  
    ## 
    ## Results are averaged over the levels of: Overestimated, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numhd.emm.conddata, ~Condition|Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-240-1.png)<!-- -->

``` r
plot(graph.numhd.emm.conddata)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-240-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numhd.emm.conddata.cld.df %>% arrange(desc(emmean))
```

    ##    Dataset Condition      emmean          SE       df     lower.CL
    ## 1        5      Ctrl 0.128477457 0.007768985 1134.119  0.113234259
    ## 2        5       Siz 0.101948164 0.007581929 1134.044  0.087071978
    ## 3        7       Siz 0.097246936 0.006196729 1136.295  0.085088620
    ## 4        9       Col 0.087674762 0.005858705 1136.453  0.076179668
    ## 5        3       Col 0.073369707 0.005974544 1136.329  0.061647329
    ## 6        5       Phr 0.071597163 0.007303895 1134.481  0.057266502
    ## 7        7      Ctrl 0.071322914 0.006491973 1136.271  0.058585312
    ## 8        9       Phr 0.068022621 0.006031059 1135.801  0.056189353
    ## 9        3       Siz 0.065357831 0.005899221 1136.494  0.053783243
    ## 10       3      Ctrl 0.064951523 0.005953953 1136.826  0.053269553
    ## 11       8       Col 0.064500225 0.005746481 1136.453  0.053225320
    ## 12       9      Ctrl 0.059833456 0.006494374 1136.226  0.047091143
    ## 13       1       Phr 0.053352908 0.005718456 1136.573  0.042132991
    ## 14       3       Phr 0.052509739 0.005814669 1136.507  0.041101047
    ## 15       8      Ctrl 0.042504611 0.005866933 1136.831  0.030993379
    ## 16       9       Siz 0.042139201 0.006013195 1136.584  0.030340990
    ## 17       8       Phr 0.037268860 0.005529273 1136.760  0.026420134
    ## 18       1       Col 0.036248157 0.005484123 1136.665  0.025488016
    ## 19       8       Siz 0.035046645 0.005584760 1136.832  0.024089051
    ## 20       1       Siz 0.029978621 0.005534339 1136.909  0.019119955
    ## 21       1      Ctrl 0.027173560 0.005868562 1136.802  0.015659131
    ## 22       5       Col 0.023676580 0.007421705 1134.191  0.009114767
    ## 23       7       Phr 0.015550694 0.006115565 1136.067  0.003551622
    ## 24       7       Col 0.002797481 0.006271570 1136.182 -0.009507679
    ##      upper.CL .group
    ## 1  0.14372066      4
    ## 2  0.11682435      3
    ## 3  0.10940525      3
    ## 4  0.09916986      4
    ## 5  0.08509209     34
    ## 6  0.08592782      3
    ## 7  0.08406052     3 
    ## 8  0.07985589      3
    ## 9  0.07693242     2 
    ## 10 0.07663349    23 
    ## 11 0.07577513     3 
    ## 12 0.07257577    23 
    ## 13 0.06457282     23
    ## 14 0.06391843     23
    ## 15 0.05401584   12  
    ## 16 0.05393741    12 
    ## 17 0.04811759    12 
    ## 18 0.04700830    2  
    ## 19 0.04600424    1  
    ## 20 0.04083729    1  
    ## 21 0.03868799   1   
    ## 22 0.03823839   12  
    ## 23 0.02754976    1  
    ## 24 0.01510264   1

``` r
ggplot(graph.numhd.emm.conddata.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=Condition,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Dataset) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-240-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.cld.df) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL, color=Condition), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=Dataset,y=emmean, color=Condition), position=position_dodge(width=0.2)) +
  geom_line(aes(x=Dataset,y=emmean, color=Condition, group=Condition), position=position_dodge(width=0.2)) +
  #scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  scale_color_discrete(labels=c("Control","Color","Phrasing","Size")) +
  labs(title="Estimated Marginal Means for Condition vs. Dataset for\nDegree of Highest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-240-4.png)<!-- -->

``` r
graph.numhd.emm.conddata.pairs <- dplyr::as_data_frame(pairs(graph.numhd.emm.conddata)) 
graph.numhd.emm.conddata.pairs #%>% View()
```

    ## # A tibble: 60 x 7
    ##    contrast Condition estimate      SE    df t.ratio       p.value
    ##  * <fct>    <fct>        <dbl>   <dbl> <dbl>   <dbl>         <dbl>
    ##  1 1 - 3    Ctrl      -0.0378  0.00825  1041 - 4.58  0.0000768    
    ##  2 1 - 5    Ctrl      -0.101   0.00961  1033 -10.5   0            
    ##  3 1 - 7    Ctrl      -0.0441  0.00864  1040 - 5.11  0.00000571   
    ##  4 1 - 8    Ctrl      -0.0153  0.00817  1003 - 1.88  0.417        
    ##  5 1 - 9    Ctrl      -0.0327  0.00865  1056 - 3.77  0.00234      
    ##  6 3 - 5    Ctrl      -0.0635  0.00968  1048 - 6.56  0.00000000123
    ##  7 3 - 7    Ctrl      -0.00637 0.00869  1043 - 0.733 0.978        
    ##  8 3 - 8    Ctrl       0.0224  0.00827  1054   2.71  0.0732       
    ##  9 3 - 9    Ctrl       0.00512 0.00865   980   0.592 0.992        
    ## 10 5 - 7    Ctrl       0.0572  0.0100   1065   5.70  0.000000230  
    ## # ... with 50 more rows

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

conddata.pairs.CI <- confint(pairs(graph.numhd.emm.conddata))

graph.numhd.emm.conddata.pairs <- full_join(graph.numhd.emm.conddata.pairs, conddata.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numhd.emm.conddata))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-241-1.png)<!-- -->

``` r
plot(pairs(graph.numhd.emm.conddata), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-241-2.png)<!-- -->

``` r
graph.numhd.emm.conddata.pairs$sig.levels <- 
  case_when(graph.numhd.emm.conddata.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numhd.emm.conddata.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numhd.emm.conddata.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numhd.emm.conddata.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numhd.emm.conddata.pairs$sig.levels <- factor(graph.numhd.emm.conddata.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numhd.emm.conddata.pairs$contrast.cond <- paste0(graph.numhd.emm.conddata.pairs$contrast,graph.numhd.emm.conddata.pairs$Condition)

graph.numhd.emm.conddata.pairs$contrast.cond <- factor(graph.numhd.emm.conddata.pairs$contrast.cond, levels=graph.numhd.emm.conddata.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.cond) %>% distinct() %>% unlist())

#graph.numhd.emm.conddata.pairs %>% View()

graph.numhd.emm.conddata.pairs$contrast <- 
  factor(as.character(graph.numhd.emm.conddata.pairs$contrast), 
         levels=graph.numhd.emm.conddata.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numhd.emm.conddata.pairs <- graph.numhd.emm.conddata.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numhd.emm.conddata.pairs %>% arrange(estimate)
```

    ## # A tibble: 60 x 13
    ##    contrast From  To    Condition estimate      SE    df t.ratio   p.value
    ##    <fct>    <chr> <chr> <fct>        <dbl>   <dbl> <dbl>   <dbl>     <dbl>
    ##  1 1 - 5    1     5     Ctrl       -0.101  0.00961  1033  -10.5   0       
    ##  2 7 - 9    7     9     Col        -0.0849 0.00848  1038  -10.0   1.67e⁻¹⁴
    ##  3 1 - 5    1     5     Siz        -0.0720 0.00930  1071  - 7.74  4.60e⁻¹³
    ##  4 1 - 7    1     7     Siz        -0.0673 0.00819  1009  - 8.21  5.10e⁻¹³
    ##  5 5 - 9    5     9     Col        -0.0640 0.00938  1072  - 6.82  2.22e⁻¹⁰
    ##  6 3 - 5    3     5     Ctrl       -0.0635 0.00968  1048  - 6.56  1.23e⁻ ⁹
    ##  7 7 - 8    7     8     Col        -0.0617 0.00840  1044  - 7.34  6.64e⁻¹²
    ##  8 7 - 9    7     9     Phr        -0.0525 0.00849  1043  - 6.18  1.37e⁻ ⁸
    ##  9 1 - 9    1     9     Col        -0.0514 0.00788  1015  - 6.53  1.55e⁻ ⁹
    ## 10 1 - 7    1     7     Ctrl       -0.0441 0.00864  1040  - 5.11  5.71e⁻ ⁶
    ## # ... with 50 more rows, and 4 more variables: lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
#graph.numlinks.emm.conddata.pairs %>% View()

ggplot(graph.numhd.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-241-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-241-4.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-241-5.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-241-6.png)<!-- -->

``` r
conddata.copy <- graph.numhd.emm.conddata.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numhd.emm.conddata.pairs.compl <- bind_rows(graph.numhd.emm.conddata.pairs, conddata.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numhd.emm.conddata.pairs.compl$From <- factor(graph.numhd.emm.conddata.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numhd.emm.conddata.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 120 x 13
    ##    contrast From  To    Condition estimate      SE    df t.ratio   p.value
    ##    <fct>    <fct> <chr> <fct>        <dbl>   <dbl> <dbl>   <dbl>     <dbl>
    ##  1 1 - 5    1     5     Ctrl       -0.101  0.00961  1033  -10.5   0       
    ##  2 5 - 8    8     5     Ctrl       -0.0860 0.00963  1054  - 8.92  0       
    ##  3 7 - 9    7     9     Col        -0.0849 0.00848  1038  -10.0   1.67e⁻¹⁴
    ##  4 1 - 5    1     5     Siz        -0.0720 0.00930  1071  - 7.74  4.60e⁻¹³
    ##  5 3 - 7    7     3     Col        -0.0706 0.00855  1048  - 8.26  1.15e⁻¹³
    ##  6 5 - 9    9     5     Ctrl       -0.0686 0.0101   1078  - 6.83  2.18e⁻¹⁰
    ##  7 1 - 7    1     7     Siz        -0.0673 0.00819  1009  - 8.21  5.10e⁻¹³
    ##  8 5 - 8    8     5     Siz        -0.0669 0.00928  1013  - 7.21  1.63e⁻¹¹
    ##  9 5 - 9    5     9     Col        -0.0640 0.00938  1072  - 6.82  2.22e⁻¹⁰
    ## 10 3 - 5    3     5     Ctrl       -0.0635 0.00968  1048  - 6.56  1.23e⁻ ⁹
    ## # ... with 110 more rows, and 4 more variables: lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
ggplot(graph.numhd.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-242-1.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-242-2.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-242-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-242-4.png)<!-- -->

##### Dataset:Overestimated

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numhd.emm.dataover <- emmeans(graph.numhd.lmer.full.int.6, ~ Dataset | Overestimated, nesting = NULL)
graph.numhd.emm.dataover.2 <- emmeans(graph.numhd.lmer.full.int.6, ~ Overestimated | Dataset, nesting = NULL)

graph.numhd.emm.dataover
```

    ## Overestimated = 1-Overestimated:
    ##  Dataset     emmean          SE      df    lower.CL   upper.CL
    ##  1       0.05920319 0.004782244 1131.98 0.049820128 0.06858624
    ##  3       0.09860347 0.006136235 1132.05 0.086563802 0.11064315
    ##  5       0.09670685 0.011204108 1127.66 0.074723608 0.11869009
    ##  7       0.04332975 0.007344016 1130.91 0.028920326 0.05773918
    ##  8       0.05239859 0.004988002 1132.11 0.042611821 0.06218535
    ##  9       0.08104829 0.006645036 1130.42 0.068010302 0.09408628
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  Dataset     emmean          SE      df    lower.CL   upper.CL
    ##  1       0.01417344 0.003473412 1135.18 0.007358409 0.02098846
    ##  3       0.02949093 0.003106193 1136.00 0.023396407 0.03558545
    ##  5       0.06614283 0.002879568 1136.57 0.060492966 0.07179270
    ##  7       0.05012926 0.003026320 1136.20 0.044191455 0.05606706
    ##  8       0.03726158 0.003358879 1135.36 0.030671275 0.04385189
    ##  9       0.04778673 0.003110994 1135.94 0.041682787 0.05389067
    ## 
    ## Results are averaged over the levels of: Condition, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numhd.emm.dataover.df <- dplyr::as_data_frame(graph.numhd.emm.dataover)

graph.numhd.emm.dataover.df #%>% View()
```

    ## # A tibble: 12 x 7
    ##    Dataset Overestimated    emmean      SE    df lower.CL upper.CL
    ##  * <fct>   <fct>             <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ##  1 1       1-Overestimated  0.0592 0.00478  1132  0.0498    0.0686
    ##  2 3       1-Overestimated  0.0986 0.00614  1132  0.0866    0.111 
    ##  3 5       1-Overestimated  0.0967 0.0112   1128  0.0747    0.119 
    ##  4 7       1-Overestimated  0.0433 0.00734  1131  0.0289    0.0577
    ##  5 8       1-Overestimated  0.0524 0.00499  1132  0.0426    0.0622
    ##  6 9       1-Overestimated  0.0810 0.00665  1130  0.0680    0.0941
    ##  7 1       2-CorrectOrUnder 0.0142 0.00347  1135  0.00736   0.0210
    ##  8 3       2-CorrectOrUnder 0.0295 0.00311  1136  0.0234    0.0356
    ##  9 5       2-CorrectOrUnder 0.0661 0.00288  1137  0.0605    0.0718
    ## 10 7       2-CorrectOrUnder 0.0501 0.00303  1136  0.0442    0.0561
    ## 11 8       2-CorrectOrUnder 0.0373 0.00336  1135  0.0307    0.0439
    ## 12 9       2-CorrectOrUnder 0.0478 0.00311  1136  0.0417    0.0539

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numhd.emm.dataover.cld <- cld(graph.numhd.emm.dataover,
                                       details=TRUE,
                                       #alpha=0.01,
                                       #by="Dataset",
                                       #Letters="|||||||||||||||||||",
                                       sort=TRUE
)

graph.numhd.emm.dataover.cld[["emmeans"]]
```

    ## Overestimated = 1-Overestimated:
    ##  Dataset     emmean          SE      df    lower.CL   upper.CL .group
    ##  7       0.04332975 0.007344016 1130.91 0.028920326 0.05773918  1    
    ##  8       0.05239859 0.004988002 1132.11 0.042611821 0.06218535  1    
    ##  1       0.05920319 0.004782244 1131.98 0.049820128 0.06858624  12   
    ##  9       0.08104829 0.006645036 1130.42 0.068010302 0.09408628   23  
    ##  5       0.09670685 0.011204108 1127.66 0.074723608 0.11869009    3  
    ##  3       0.09860347 0.006136235 1132.05 0.086563802 0.11064315    3  
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  Dataset     emmean          SE      df    lower.CL   upper.CL .group
    ##  1       0.01417344 0.003473412 1135.18 0.007358409 0.02098846  1    
    ##  3       0.02949093 0.003106193 1136.00 0.023396407 0.03558545   2   
    ##  8       0.03726158 0.003358879 1135.36 0.030671275 0.04385189   23  
    ##  9       0.04778673 0.003110994 1135.94 0.041682787 0.05389067    34 
    ##  7       0.05012926 0.003026320 1136.20 0.044191455 0.05606706     4 
    ##  5       0.06614283 0.002879568 1136.57 0.060492966 0.07179270      5
    ## 
    ## Results are averaged over the levels of: Condition, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.dataover.cld.df <- graph.numhd.emm.dataover.cld[["emmeans"]]

graph.numhd.emm.dataover.cld.df %>% dplyr::select(Dataset,Overestimated,.group) %>% print()
```

    ## Overestimated = 1-Overestimated:
    ##  Dataset .group
    ##  7        1    
    ##  8        1    
    ##  1        12   
    ##  9         23  
    ##  5          3  
    ##  3          3  
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  Dataset .group
    ##  1        1    
    ##  3         2   
    ##  8         23  
    ##  9          34 
    ##  7           4 
    ##  5            5
    ## 
    ## Results are averaged over the levels of: Condition, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numhd.emm.dataover.2, details=TRUE, sort=TRUE)[["emmeans"]] %>% dplyr::select(Dataset,Overestimated,.group) %>% print()
```

    ## Dataset = 1:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Dataset = 3:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Dataset = 5:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Dataset = 7:
    ##  Overestimated    .group
    ##  1-Overestimated   1    
    ##  2-CorrectOrUnder  1    
    ## 
    ## Dataset = 8:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Dataset = 9:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Results are averaged over the levels of: Condition, DatasetOrder 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numhd.emm.dataover, ~Dataset|Overestimated, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-243-1.png)<!-- -->

``` r
plot(graph.numhd.emm.dataover)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-243-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numhd.emm.dataover.cld.df %>% arrange(desc(emmean))
```

    ##    Dataset    Overestimated     emmean          SE       df    lower.CL
    ## 1        3  1-Overestimated 0.09860347 0.006136235 1132.049 0.086563802
    ## 2        5  1-Overestimated 0.09670685 0.011204108 1127.663 0.074723608
    ## 3        9  1-Overestimated 0.08104829 0.006645036 1130.421 0.068010302
    ## 4        5 2-CorrectOrUnder 0.06614283 0.002879568 1136.567 0.060492966
    ## 5        1  1-Overestimated 0.05920319 0.004782244 1131.976 0.049820128
    ## 6        8  1-Overestimated 0.05239859 0.004988002 1132.110 0.042611821
    ## 7        7 2-CorrectOrUnder 0.05012926 0.003026320 1136.196 0.044191455
    ## 8        9 2-CorrectOrUnder 0.04778673 0.003110994 1135.938 0.041682787
    ## 9        7  1-Overestimated 0.04332975 0.007344016 1130.911 0.028920326
    ## 10       8 2-CorrectOrUnder 0.03726158 0.003358879 1135.355 0.030671275
    ## 11       3 2-CorrectOrUnder 0.02949093 0.003106193 1135.998 0.023396407
    ## 12       1 2-CorrectOrUnder 0.01417344 0.003473412 1135.177 0.007358409
    ##      upper.CL .group
    ## 1  0.11064315      3
    ## 2  0.11869009      3
    ## 3  0.09408628     23
    ## 4  0.07179270      5
    ## 5  0.06858624    12 
    ## 6  0.06218535    1  
    ## 7  0.05606706     4 
    ## 8  0.05389067    34 
    ## 9  0.05773918    1  
    ## 10 0.04385189   23  
    ## 11 0.03558545   2   
    ## 12 0.02098846  1

``` r
ggplot(graph.numhd.emm.dataover.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=Dataset,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-243-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.cld.df) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL, color=Overestimated), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=Dataset,y=emmean, color=Overestimated), position=position_dodge(width=0.2)) +
  geom_line(aes(x=Dataset,y=emmean, color=Overestimated, group=Overestimated), position=position_dodge(width=0.2)) +
  #scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  scale_color_discrete(labels=c("Overestimated","Correct or\nUnderestimated")) +
  labs(title="Estimated Marginal Means for Dataset vs. Overestimated for\nDegree of Highest Degree Node task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-243-4.png)<!-- -->

``` r
graph.numhd.emm.dataover.pairs <- dplyr::as_data_frame(pairs(graph.numhd.emm.dataover)) 
graph.numhd.emm.dataover.pairs #%>% View()
```

    ## # A tibble: 30 x 7
    ##    contrast Overestimated   estimate      SE    df t.ratio      p.value
    ##  * <fct>    <fct>              <dbl>   <dbl> <dbl>   <dbl>        <dbl>
    ##  1 1 - 3    1-Overestimated -0.0394  0.00776  1112  -5.08  0.00000664  
    ##  2 1 - 5    1-Overestimated -0.0375  0.0122   1119  -3.08  0.0256      
    ##  3 1 - 7    1-Overestimated  0.0159  0.00875  1105   1.81  0.457       
    ##  4 1 - 8    1-Overestimated  0.00680 0.00688  1111   0.989 0.922       
    ##  5 1 - 9    1-Overestimated -0.0218  0.00812  1108  -2.69  0.0779      
    ##  6 3 - 5    1-Overestimated  0.00190 0.0128   1121   0.149 1.000       
    ##  7 3 - 7    1-Overestimated  0.0553  0.00951  1106   5.81  0.000000122 
    ##  8 3 - 8    1-Overestimated  0.0462  0.00787  1111   5.87  0.0000000871
    ##  9 3 - 9    1-Overestimated  0.0176  0.00897  1083   1.96  0.368       
    ## 10 5 - 7    1-Overestimated  0.0534  0.0134   1110   3.99  0.000974    
    ## # ... with 20 more rows

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

dataover.pairs.CI <- confint(pairs(graph.numhd.emm.dataover))

graph.numhd.emm.dataover.pairs <- full_join(graph.numhd.emm.dataover.pairs, dataover.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numhd.emm.dataover))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-244-1.png)<!-- -->

``` r
plot(pairs(graph.numhd.emm.dataover), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-244-2.png)<!-- -->

``` r
graph.numhd.emm.dataover.pairs$sig.levels <- 
  case_when(graph.numhd.emm.dataover.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numhd.emm.dataover.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numhd.emm.dataover.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numhd.emm.dataover.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numhd.emm.dataover.pairs$sig.levels <- factor(graph.numhd.emm.dataover.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numhd.emm.dataover.pairs$contrast.all <- paste0(graph.numhd.emm.dataover.pairs$contrast,graph.numhd.emm.dataover.pairs$Overestimated)

graph.numhd.emm.dataover.pairs$contrast.all <- factor(graph.numhd.emm.dataover.pairs$contrast.all, levels=graph.numhd.emm.dataover.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.all) %>% distinct() %>% unlist())

#graph.numhd.emm.conddata.pairs %>% View()

graph.numhd.emm.dataover.pairs$contrast <- 
  factor(as.character(graph.numhd.emm.dataover.pairs$contrast), 
         levels=graph.numhd.emm.dataover.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numhd.emm.dataover.pairs <- graph.numhd.emm.dataover.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numhd.emm.dataover.pairs %>% arrange(estimate)
```

    ## # A tibble: 30 x 13
    ##    contrast From  To    Overestimated    estimate      SE    df t.ratio
    ##    <fct>    <chr> <chr> <fct>               <dbl>   <dbl> <dbl>   <dbl>
    ##  1 1 - 5    1     5     2-CorrectOrUnder  -0.0520 0.00446  1034  -11.7 
    ##  2 1 - 3    1     3     1-Overestimated   -0.0394 0.00776  1112  - 5.08
    ##  3 7 - 9    7     9     1-Overestimated   -0.0377 0.00983  1093  - 3.84
    ##  4 1 - 5    1     5     1-Overestimated   -0.0375 0.0122   1119  - 3.08
    ##  5 3 - 5    3     5     2-CorrectOrUnder  -0.0367 0.00417  1008  - 8.79
    ##  6 1 - 7    1     7     2-CorrectOrUnder  -0.0360 0.00455  1046  - 7.90
    ##  7 1 - 9    1     9     2-CorrectOrUnder  -0.0336 0.00460  1030  - 7.31
    ##  8 8 - 9    8     9     1-Overestimated   -0.0286 0.00825  1100  - 3.47
    ##  9 1 - 8    1     8     2-CorrectOrUnder  -0.0231 0.00478  1057  - 4.83
    ## 10 1 - 9    1     9     1-Overestimated   -0.0218 0.00812  1108  - 2.69
    ## # ... with 20 more rows, and 5 more variables: p.value <dbl>,
    ## #   lower.CL <dbl>, upper.CL <dbl>, sig.levels <ord>, contrast.all <fct>

``` r
#graph.numlinks.emm.conddata.pairs %>% View()

ggplot(graph.numhd.emm.dataover.pairs) +
  geom_errorbar(aes(x=contrast.all,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast.all,y=estimate, fill=sig.levels), shape=21) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-244-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.pairs) +
  geom_errorbar(aes(x=contrast.all,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast.all,y=estimate, fill=sig.levels), shape=21) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-244-4.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-244-5.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-244-6.png)<!-- -->

``` r
dataover.copy <- graph.numhd.emm.dataover.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numhd.emm.dataover.pairs.compl <- bind_rows(graph.numhd.emm.dataover.pairs, dataover.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numhd.emm.dataover.pairs.compl$From <- factor(graph.numhd.emm.dataover.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numhd.emm.dataover.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 60 x 13
    ##    contrast From  To    Overestimated    estimate      SE    df t.ratio
    ##    <fct>    <fct> <chr> <fct>               <dbl>   <dbl> <dbl>   <dbl>
    ##  1 3 - 7    7     3     1-Overestimated   -0.0553 0.00951  1106  - 5.81
    ##  2 5 - 7    7     5     1-Overestimated   -0.0534 0.0134   1110  - 3.99
    ##  3 1 - 5    1     5     2-CorrectOrUnder  -0.0520 0.00446  1034  -11.7 
    ##  4 3 - 8    8     3     1-Overestimated   -0.0462 0.00787  1111  - 5.87
    ##  5 5 - 8    8     5     1-Overestimated   -0.0443 0.0122   1100  - 3.63
    ##  6 1 - 3    1     3     1-Overestimated   -0.0394 0.00776  1112  - 5.08
    ##  7 7 - 9    7     9     1-Overestimated   -0.0377 0.00983  1093  - 3.84
    ##  8 1 - 5    1     5     1-Overestimated   -0.0375 0.0122   1119  - 3.08
    ##  9 3 - 5    3     5     2-CorrectOrUnder  -0.0367 0.00417  1008  - 8.79
    ## 10 1 - 7    1     7     2-CorrectOrUnder  -0.0360 0.00455  1046  - 7.90
    ## # ... with 50 more rows, and 5 more variables: p.value <dbl>,
    ## #   lower.CL <dbl>, upper.CL <dbl>, sig.levels <ord>, contrast.all <fct>

``` r
ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-245-1.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-245-2.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-245-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-245-4.png)<!-- -->

##### DatasetOrder:Overestimated

TO DO : add factor to DatasetOrder at beginning

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numhd.emm.dorderover <- emmeans(graph.numhd.lmer.full.int.6, ~ DatasetOrder | Overestimated, nesting = NULL)
graph.numhd.emm.dorderover.2 <- emmeans(graph.numhd.lmer.full.int.6, ~ Overestimated | DatasetOrder, nesting = NULL)

graph.numhd.emm.dorderover
```

    ## Overestimated = 1-Overestimated:
    ##  DatasetOrder     emmean          SE      df   lower.CL   upper.CL
    ##  1            0.05896111 0.004465195 1135.26 0.05020015 0.06772208
    ##  2            0.07399685 0.004868567 1136.45 0.06444446 0.08354924
    ##  3            0.08268711 0.004539326 1136.62 0.07378071 0.09159351
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  DatasetOrder     emmean          SE      df   lower.CL   upper.CL
    ##  1            0.04019809 0.002239120 1133.90 0.03580481 0.04459138
    ##  2            0.04263764 0.002196301 1132.78 0.03832837 0.04694692
    ##  3            0.03965664 0.002275377 1133.50 0.03519222 0.04412107
    ## 
    ## Results are averaged over the levels of: Condition, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numhd.emm.dorderover.df <- dplyr::as_data_frame(graph.numhd.emm.dorderover)

graph.numhd.emm.dorderover.df #%>% View()
```

    ## # A tibble: 6 x 7
    ##   DatasetOrder Overestimated    emmean      SE    df lower.CL upper.CL
    ## * <fct>        <fct>             <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1            1-Overestimated  0.0590 0.00447  1135   0.0502   0.0677
    ## 2 2            1-Overestimated  0.0740 0.00487  1136   0.0644   0.0835
    ## 3 3            1-Overestimated  0.0827 0.00454  1137   0.0738   0.0916
    ## 4 1            2-CorrectOrUnder 0.0402 0.00224  1134   0.0358   0.0446
    ## 5 2            2-CorrectOrUnder 0.0426 0.00220  1133   0.0383   0.0469
    ## 6 3            2-CorrectOrUnder 0.0397 0.00228  1133   0.0352   0.0441

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numhd.emm.dorderover.cld <- cld(graph.numhd.emm.dorderover,
                                       details=TRUE,
                                       #alpha=0.01,
                                       #by="Dataset",
                                       #Letters="|||||||||||||||||||",
                                       sort=TRUE
)

graph.numhd.emm.dorderover.cld[["emmeans"]]
```

    ## Overestimated = 1-Overestimated:
    ##  DatasetOrder     emmean          SE      df   lower.CL   upper.CL .group
    ##  1            0.05896111 0.004465195 1135.26 0.05020015 0.06772208  1    
    ##  2            0.07399685 0.004868567 1136.45 0.06444446 0.08354924   2   
    ##  3            0.08268711 0.004539326 1136.62 0.07378071 0.09159351   2   
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  DatasetOrder     emmean          SE      df   lower.CL   upper.CL .group
    ##  3            0.03965664 0.002275377 1133.50 0.03519222 0.04412107  1    
    ##  1            0.04019809 0.002239120 1133.90 0.03580481 0.04459138  1    
    ##  2            0.04263764 0.002196301 1132.78 0.03832837 0.04694692  1    
    ## 
    ## Results are averaged over the levels of: Condition, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 3 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numhd.emm.dorderover.cld.df <- graph.numhd.emm.dorderover.cld[["emmeans"]]

graph.numhd.emm.dorderover.cld.df %>% dplyr::select(DatasetOrder,Overestimated,.group) %>% print()
```

    ## Overestimated = 1-Overestimated:
    ##  DatasetOrder .group
    ##  1             1    
    ##  2              2   
    ##  3              2   
    ## 
    ## Overestimated = 2-CorrectOrUnder:
    ##  DatasetOrder .group
    ##  3             1    
    ##  1             1    
    ##  2             1    
    ## 
    ## Results are averaged over the levels of: Condition, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 3 estimates 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numhd.emm.dorderover.2, details=TRUE, sort=TRUE)[["emmeans"]] %>% dplyr::select(DatasetOrder,Overestimated,.group) %>% print()
```

    ## DatasetOrder = 1:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## DatasetOrder = 2:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## DatasetOrder = 3:
    ##  Overestimated    .group
    ##  2-CorrectOrUnder  1    
    ##  1-Overestimated    2   
    ## 
    ## Results are averaged over the levels of: Condition, Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numhd.emm.dorderover, ~DatasetOrder|Overestimated, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-246-1.png)<!-- -->

``` r
plot(graph.numhd.emm.dorderover)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-246-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numhd.emm.dorderover.cld.df %>% arrange(desc(emmean))
```

    ##   DatasetOrder    Overestimated     emmean          SE       df   lower.CL
    ## 1            3  1-Overestimated 0.08268711 0.004539326 1136.622 0.07378071
    ## 2            2  1-Overestimated 0.07399685 0.004868567 1136.448 0.06444446
    ## 3            1  1-Overestimated 0.05896111 0.004465195 1135.265 0.05020015
    ## 4            2 2-CorrectOrUnder 0.04263764 0.002196301 1132.778 0.03832837
    ## 5            1 2-CorrectOrUnder 0.04019809 0.002239120 1133.898 0.03580481
    ## 6            3 2-CorrectOrUnder 0.03965664 0.002275377 1133.496 0.03519222
    ##     upper.CL .group
    ## 1 0.09159351      2
    ## 2 0.08354924      2
    ## 3 0.06772208     1 
    ## 4 0.04694692      1
    ## 5 0.04459138      1
    ## 6 0.04412107      1

``` r
ggplot(graph.numhd.emm.dorderover.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=DatasetOrder,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=DatasetOrder,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-246-3.png)<!-- -->

``` r
ggplot(graph.numhd.emm.dorderover.cld.df) +
  geom_errorbar(aes(x=DatasetOrder,ymax=upper.CL,ymin=lower.CL, color=Overestimated), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=DatasetOrder,y=emmean, color=Overestimated), position=position_dodge(width=0.2)) +
  geom_line(aes(x=DatasetOrder,y=emmean, color=Overestimated, group=Overestimated), position=position_dodge(width=0.2)) +
  #scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  scale_color_discrete(labels=c("Overestimated","Correct or\nUnderestimated")) +
  labs(title="Estimated Marginal Means for Dataset Order vs. Overestimated for\nDegree of Highest Degree Node task, graphics conditions", x="Dataset Order")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-246-4.png)<!-- -->

``` r
graph.numhd.emm.dorderover.pairs <- dplyr::as_data_frame(pairs(graph.numhd.emm.dorderover)) 
graph.numhd.emm.dorderover.pairs #%>% View()
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

dorderover.pairs.CI <- confint(pairs(graph.numhd.emm.dorderover))

graph.numhd.emm.dorderover.pairs <- full_join(graph.numhd.emm.dorderover.pairs, dorderover.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numhd.emm.dorderover))
plot(pairs(graph.numhd.emm.dorderover), comparisons = TRUE)

graph.numhd.emm.dorderover.pairs$sig.levels <- 
  case_when(graph.numhd.emm.dorderover.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numhd.emm.dorderover.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numhd.emm.dorderover.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numhd.emm.dorderover.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numhd.emm.dorderover.pairs$sig.levels <- factor(graph.numhd.emm.dorderover.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numhd.emm.dorderover.pairs$contrast.all <- paste0(graph.numhd.emm.dorderover.pairs$contrast,graph.numhd.emm.dorderover.pairs$Overestimated)

graph.numhd.emm.dorderover.pairs$contrast.all <- factor(graph.numhd.emm.dorderover.pairs$contrast.all, levels=graph.numhd.emm.dorderover.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.all) %>% distinct() %>% unlist())

#graph.numhd.emm.conddata.pairs %>% View()

graph.numhd.emm.dorderover.pairs$contrast <- 
  factor(as.character(graph.numhd.emm.dorderover.pairs$contrast), 
         levels=graph.numhd.emm.dorderover.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numhd.emm.dorderover.pairs <- graph.numhd.emm.dorderover.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numhd.emm.dorderover.pairs %>% arrange(estimate)
#graph.numlinks.emm.conddata.pairs %>% View()

ggplot(graph.numhd.emm.dorderover.pairs) +
  geom_errorbar(aes(x=contrast.all,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast.all,y=estimate, fill=sig.levels), shape=21) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()

ggplot(graph.numhd.emm.dorderover.pairs) +
  geom_errorbar(aes(x=contrast.all,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast.all,y=estimate, fill=sig.levels), shape=21) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()


ggplot(graph.numhd.emm.dorderover.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()

ggplot(graph.numhd.emm.dorderover.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  #geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Overestimated, scales="free_y") +
  coord_flip()
```

``` r
dataover.copy <- graph.numhd.emm.dataover.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numhd.emm.dataover.pairs.compl <- bind_rows(graph.numhd.emm.dataover.pairs, dataover.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numhd.emm.dataover.pairs.compl$From <- factor(graph.numhd.emm.dataover.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numhd.emm.dataover.pairs.compl %>% arrange(estimate)

ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)

ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)

ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)

ggplot(graph.numhd.emm.dataover.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Overestimated)
```

### Number of Links

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_numlinks <- readRDS(file.path(analysisDataDir, "GraphicsNumLinks.rds"))
```

#### lme4

``` r
# Condition

graph.numlinks.lmer.cond <- lmer(LogError ~ Condition + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.cond) # Size extremely significant

anova(graph.numlinks.lmer.cond)

# Condition is significant (p=1.665e-15); trying Ctrl_dummy

graph.numlinks.lmer.ctrldum <- lmer(LogError ~ Ctrl_dummy + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.ctrldum)

anova(graph.numlinks.lmer.ctrldum)

# Ctrl_dummy is significant (p=0.02258)

graph.numlinks.lmer <- lmer(LogError ~ ConditionPhrasing + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# ConditionPhrasing is not significanT

graph.numlinks.lmer.condgr <- lmer(LogError ~ ConditionGraphics + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.condgr)

anova(graph.numlinks.lmer.condgr)

# ConditionGraphics is significant (p=5.218e-15)

graph.numlinks.lmer.condcol <- lmer(LogError ~ ConditionColor + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.condcol)

anova(graph.numlinks.lmer.condcol)

# ConditionColor is significant (p=4.49e-10)

graph.numlinks.lmer.dataset <- lmer(LogError ~ Dataset + (1|Demo.ResponseID), data = graphics_numlinks, REML = T)

lmsum <- summary(graph.numlinks.lmer.dataset)
lmsum # all significantly different from 1 except 7
#names(lmsum)

anova(graph.numlinks.lmer.dataset)

# Dataset is significant (p < 2.2e-16); trying QuestionOrder

graph.numlinks.lmer <- lmer(LogError ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# QuestionOrder is not significant; trying DatasetOrder

graph.numlinks.lmer <- lmer(LogError ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# DatasetOrder is not significant; trying DatasetDuration

graph.numlinks.lmer.datadur <- lmer(LogError ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.datadur)

anova(graph.numlinks.lmer.datadur)

# DatasetDuration is slightly significant (p=0.0222); trying DatasetStartTime

graph.numlinks.lmer <- lmer(LogError ~ DatasetStartTime + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# DatasetStartTime is not significant; trying TaskOrder

graph.numlinks.lmer <- lmer(LogError ~ TaskOrder + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# TaskOrder is not significant; trying CorrectAnswer

graph.numlinks.lmer.correct <- lmer(LogError ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.correct)

anova(graph.numlinks.lmer.correct)

# CorrectAnswer is highly significant (p = 2.2e-16); trying Underestimated

graph.numlinks.lmer.underest <- lmer(LogError ~ Underestimated + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.underest)

anova(graph.numlinks.lmer.underest)

# Underestimated is significant (p = 9.057e-06); trying Overestimated

graph.numlinks.lmer.overest <- lmer(LogError ~ Overestimated + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.overest)

anova(graph.numlinks.lmer.overest)

# Overestimated is significant (p = 3.747e-06); trying Stats.Q_TotalDuration

graph.numlinks.lmer <- lmer(LogError ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Stats.Q_TotalDuration is not significant; trying Stats.dataset_count

graph.numlinks.lmer <- lmer(LogError ~ Stats.dataset_count + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Stats.dataset_count is not signficant; trying Stats.OperatingSystem

graph.numlinks.lmer <- lmer(LogError ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer) # no categories significant

anova(graph.numlinks.lmer)

# Stats.OperatingSystem is not signficant; skipping combinations

#graph.numlinks.lmer <- lmer(LogError ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

graphics_numlinks.CS <- graphics_numlinks %>% mutate(StatsNumPixels=scale(StatsNumPixels))

graph.numlinks.lmer <- lmer(LogError ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_numlinks.CS, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# StatsNumPixels is not signficant; trying Demo.age

graph.numlinks.lmer <- lmer(LogError ~ Demo.age + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.age))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.age is not signficant; trying Demo.gender

graph.numlinks.lmer <- lmer(LogError ~ Demo.gender + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.gender))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.gender is barely signficant - nonbinary different from female (not interested in pursuing this); trying Demo.lang

graph.numlinks.lmer <- lmer(LogError ~ Demo.lang + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.lang))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.lang is not signficant; trying Demo.educ

graph.numlinks.lmer <- lmer(LogError ~ Demo.educ + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.educ))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.educ is not signficant; trying Demo.acfield

graph.numlinks.lmer <- lmer(LogError ~ Demo.acfield + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.acfield))), REML = T)

summary(graph.numlinks.lmer) # only a couple of categories significant

anova(graph.numlinks.lmer)

# Demo.acfield is barely not signficant; trying Demo.acfieldGrouped

graph.numlinks.lmer <- lmer(LogError ~ Demo.acfieldGrouped + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.acfieldGrouped))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.acfieldGrouped is less signficant

graph.numlinks.lmer <- lmer(LogError ~ Demo.acfieldGrouped2 + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.acfieldGrouped2))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.acfieldGrouped2 is absolutely not signficant

graph.numlinks.lmer <- lmer(LogError ~ Demo.acfieldGrouped3 + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.acfieldGrouped3))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.acfieldGrouped3 is just barely not signficant

graph.numlinks.lmer <- lmer(LogError ~ Demo.dailytech_Computer + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.dailytech_Computer))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.dailytech_Computer is not significant; trying Demo.dailytech_Tablet

graph.numlinks.lmer <- lmer(LogError ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.dailytech_Tablet))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.dailytech_Computer is not significant; trying Demo.dailytech_SmartPhone

graph.numlinks.lmer <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.dailytech_SmartPhone))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.dailytech_SmartPhone is barely significant; trying Demo.weeklygaming

graph.numlinks.lmer <- lmer(LogError ~ Demo.weeklygaming + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.weeklygaming))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.weeklygaming is not significant; trying Demo.expdataanal

graph.numlinks.lmer <- lmer(LogError ~ Demo.expdataanal + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.expdataanal))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.expdataanal is not significant; trying Demo.expdatavis

graph.numlinks.lmer <- lmer(LogError ~ Demo.expdatavis + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.expdatavis))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.expdatavis is not significant; trying Demo.expreadnetvis

graph.numlinks.lmer <- lmer(LogError ~ Demo.expreadnetvis + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.expreadnetvis))), REML = T)

summary(graph.numlinks.lmer) 

anova(graph.numlinks.lmer)

# Demo.expreadnetvis is not significant; trying Demo.expreadnetvis.alot

graph.numlinks.lmer <- lmer(LogError ~ Demo.expreadnetvis.alot + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.expreadnetvis.alot))), REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# Demo.expreadnetvis.alot is not significant; trying Demo.expcreatenetvis

graph.numlinks.lmer <- lmer(LogError ~ Demo.expcreatenetvis + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.expcreatenetvis))), REML = T)

summary(graph.numlinks.lmer) 

anova(graph.numlinks.lmer)

# Demo.expcreatenetvis is not significant; trying Demo.expcreatenetvis.alot

graph.numlinks.lmer <- lmer(LogError ~ Demo.expcreatenetvis.alot + (1|Demo.ResponseID), data=graphics_numlinks %>% filter(!(is.na(Demo.expcreatenetvis.alot))), REML = T)

summary(graph.numlinks.lmer) 

anova(graph.numlinks.lmer)

# Demo.expcreatenetvis.alot is not significant; trying AvgDeg

graph.numlinks.lmer.avgdeg <- lmer(LogError ~ AvgDeg + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.avgdeg)

anova(graph.numlinks.lmer.avgdeg)

# AvgDeg is significant (p=2.871e-09); trying Density

graph.numlinks.lmer.dens <- lmer(LogError ~ Density + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.dens)

anova(graph.numlinks.lmer.dens)

# Density is significant (p=2.629e-13); trying LargeClust1

graph.numlinks.lmer.lgclust <- lmer(LogError ~ LargeClust1 + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.lgclust)

anova(graph.numlinks.lmer.lgclust)

# LargeClust1 is significant (p=3.531e-14); trying Modularity

graph.numlinks.lmer.mod <- lmer(LogError ~ Modularity + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.mod)

anova(graph.numlinks.lmer.mod)

# Modularity is significant (p<2.2e-16); trying NumClust

graph.numlinks.lmer.numclust <- lmer(LogError ~ NumClust + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.numclust)

anova(graph.numlinks.lmer.numclust)

# NumClust is significant (p<2.2e-16); trying NumHighDegree

graph.numlinks.lmer <- lmer(LogError ~ NumHighDegree + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer)

anova(graph.numlinks.lmer)

# NumHighDegree is not significant; trying NumLinks

graph.numlinks.lmer.numlinks <- lmer(LogError ~ NumLinks + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.numlinks)

anova(graph.numlinks.lmer.numlinks)

# NumLinks is significant (p<2.2e-16); trying NumNodes

graph.numlinks.lmer.numnodes <- lmer(LogError ~ NumNodes + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.numnodes)

anova(graph.numlinks.lmer.numnodes)

# NumNodes is significant (p<2.2e-16); trying NumNodesClust1

graph.numlinks.lmer.sizeclust1 <- lmer(LogError ~ NumNodesClust1 + (1|Demo.ResponseID), data=graphics_numlinks, REML = T)

summary(graph.numlinks.lmer.sizeclust1)

anova(graph.numlinks.lmer.sizeclust1)

# NumNodesClust1 is significant (p<2.2e-16); 
```

``` r
temp <- graphics_numlinks %>% 
  dplyr::select(Demo.ResponseID,LogError,Condition,Ctrl_dummy,ConditionGraphics,
                ConditionColor,Dataset,DatasetDuration,CorrectAnswer,Underestimated,
                Overestimated,AvgDeg,Density,LargeClust1,Modularity,NumClust,NumLinks,
                NumNodes,NumNodesClust1) %>% drop_na()
```

Run this one

``` r
graph.numlinks.lmer.full <- lmer(LogError ~ Condition + 
                                   Dataset +
                                   DatasetDuration +
                                   (1|Demo.ResponseID), 
                                 data=temp, REML = T)

summary(graph.numlinks.lmer.full)

anova(graph.numlinks.lmer.full)
```

Run this one

``` r
graph.numlinks.lmer.full.int <- lmer(LogError ~ Condition + 
                                   Dataset +
                                     Condition:Dataset +
                                   (1|Demo.ResponseID), 
                                 data=temp, REML = T)

summary(graph.numlinks.lmer.full.int)

anova(graph.numlinks.lmer.full.int)

anova(graph.numlinks.lmer.full,graph.numlinks.lmer.full.int)
# keep interaction
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.numlinks.lmer.full.int, 
     file = file.path(analysisDataDir,"fits/graph_numlinks_lmer_int.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_numlinks_lmer_int.RData"))
```

``` r
rand(graph.numlinks.lmer.full.int)
```

    ## Analysis of Random effects Table:
    ##                 Chi.sq Chi.DF p.value   
    ## Demo.ResponseID    8.4      1   0.004 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# result shows that random effects of participant are significant (p=0.004)

anova(graph.numlinks.lmer.full.int)
```

    ## Analysis of Variance Table of type III  with  Satterthwaite 
    ## approximation for degrees of freedom
    ##                    Sum Sq  Mean Sq NumDF  DenDF F.value    Pr(>F)    
    ## Condition         0.27585 0.091951     3  394.6  50.428 < 2.2e-16 ***
    ## Dataset           1.43500 0.287000     5 1014.0 157.399 < 2.2e-16 ***
    ## Condition:Dataset 1.62065 0.108043    15 1012.9  59.254 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# SmartPhone significant, < 0.01

#ranef(graph.avgdeg.lmer.SP)

# displays the random effects; not that useful

# unlike lme(), lmer() doesn't allow for heterogeneous error variance structures (the "weights")

(r2nsj = r2beta(graph.numlinks.lmer.full.int, method = 'nsj', partial = TRUE))[1,'Rsq']
```

    ## [1] 0.5958765

``` r
ggplot(graphics_numlinks) + geom_histogram(aes(LogError), binwidth=.005) + labs(title="Distribution of LogError values for Number of Links task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-258-1.png)<!-- -->

``` r
plot(graph.numlinks.lmer.full.int)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-259-1.png)<!-- -->

``` r
plot(graph.numlinks.lmer.full.int, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-259-2.png)<!-- -->

``` r
plot(graph.numlinks.lmer.full.int, resid(.) ~ fitted(.) | Condition, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-259-3.png)<!-- -->

``` r
plot(graph.numlinks.lmer.full.int, resid(., scaled=TRUE) ~ fitted(.) | Condition, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-259-4.png)<!-- -->

``` r
plot(graph.numlinks.lmer.full.int, LogError ~ fitted(.), abline = c(0,1))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-259-5.png)<!-- -->

``` r
graph.numlinks.lmer.full.int.f <- fortify(graph.numlinks.lmer.full.int)

ggplot(graph.numlinks.lmer.full.int.f, aes(.fitted,.resid)) + 
  geom_point() +
  #facet_grid(.~Sex) + 
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-260-1.png)<!-- -->

``` r
ggplot(graph.numlinks.lmer.full.int.f, aes(.fitted,LogError)) + 
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-260-2.png)<!-- -->

``` r
ggplot(graph.numlinks.lmer.full.int.f, aes(LogError,.fitted)) +
geom_point() +
geom_abline(aes(slope = 1, intercept = 0)) +
#scale_y_continuous(limits=c(0,0.3)) +
labs(title="Real vs. Predicted LogError values for Number of Links task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-260-3.png)<!-- -->

``` r
# TO DO: check out interpretation for these plots??

library(lattice)

prof <-  profile(graph.avgdeg.lmer.SP, optimizer="Nelder_Mead", which="beta_")

prof.CI <- confint(prof)

#CI2 <- confint(graph.avgdeg.lmer.SP, maxpts = 8)

xyplot(prof)

xyplot(prof, absVal = TRUE)

xyplot(prof, conf = c(0.95, 0.99), main = "95% and 99% profile() intervals")

# can also apply logProf() and varianceProf() to profile object

densityplot(prof)

splom(prof)
```

#### brms/tidybayes

``` r
# graph.numlinks.lmer.full.int <- lmer(LogError ~ Condition + Dataset + Condition:Dataset + (1|Demo.ResponseID), data=temp, REML = T)

#library(rstan)

theme_set(theme_grey())

#avgd.fit <- brm(LogError~Underestimated + Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data = temp, REML=T)
numlinks.fit <- brm(LogError ~ Condition + Dataset + Condition:Dataset + (1|Demo.ResponseID), data = temp)

summary(numlinks.fit, waic = TRUE) 

#plot(numlinks.fit)

numlinks.fit$formula

#numlinks.fit %<>% recover_types(temp)

#tidybayes::parameters(numlinks.fit)
```

``` r
#plot(fit, ask=FALSE) 
#plot(marginal_effects(fit, probs = c(0.05, 0.95)),points=TRUE, ask=FALSE)
#plot(marginal_effects(fit, probs = c(0.05, 0.95)),ask=FALSE)
plot(marginal_effects(numlinks.fit, effects="Dataset", probs = c(0.05, 0.95)),ask=FALSE)
```

``` r
# all pairs

numlinks.pairs.2 <- c("Dataset.L = 0", # 3 vs. 1
         "Dataset.Q = 0",
         "Dataset.C = 0",
         "DatasetE4 = 0",
         "DatasetE5 = 0",
         "Dataset.Q - Dataset.L = 0", # 5 vs. 3
         "Dataset.C - Dataset.L = 0",
         "DatasetE4 - Dataset.L = 0",
         "DatasetE5 - Dataset.L = 0",
         "Dataset.C - Dataset.Q = 0", # 7 vs. 5
         "DatasetE4 - Dataset.Q = 0",
         "DatasetE5 - Dataset.Q = 0",
         "DatasetE4 - Dataset.C = 0", # 8 vs. 7
         "DatasetE5 - Dataset.C = 0",
         "DatasetE5 - DatasetE4 = 0")


(numlinks.pairs.2.res <- hypothesis(numlinks.fit, numlinks.pairs.2))

#plot(numlinks.pairs.2.res, chars = NULL, ask=FALSE)

numlinks.pairs.2.res.df <- numlinks.pairs.2.res$hypothesis

numlinks.pairs.2.res.df$contrast <- c("3-1","5-1","7-1","8-1","9-1","5-3","7-3","8-3","9-3","7-5","8-5","9-5","8-7","9-7","9-8")

numlinks.pairs.2.res.df$contrast <- factor(numlinks.pairs.2.res.df$contrast,
                                    levels=numlinks.pairs.2.res.df %>% arrange(desc(Estimate)) %>%
                                      dplyr::select(contrast) %>% unlist())

ggplot(numlinks.pairs.2.res.df, aes(contrast,Estimate)) + 
  geom_errorbar(aes(ymin=`l-95% CI`,ymax=`u-95% CI`)) +
  geom_point(aes(fill=Star=="*"), size=7, shape=21) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=star.colors,name="Outside 95% CI") +
  coord_flip()
```

``` r
numlinks.pairs.2.res.df <- numlinks.pairs.2.res.df %>% separate(contrast, c("From", "To"), sep="[-]", remove=FALSE)

copy <- numlinks.pairs.2.res.df %>% rename(From=To,To=From) %>% 
  mutate(Estimate=-Estimate,
         `l-95% CI`=-`l-95% CI`,
         `u-95% CI`=-`u-95% CI`)

pairs.compl <- bind_rows(numlinks.pairs.2.res.df, copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

pairs.compl$From <- factor(pairs.compl$From, levels=c(9,8,7,5,3,1))
pairs.compl$To <- factor(pairs.compl$To, levels=c(1,3,5,7,8,9))

#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
pairs.compl %>% arrange(Estimate)

# can't get color to go the right direction
ggplot(pairs.compl %>% filter(Estimate >= 0)) +
  geom_tile(aes(x=To,y=From,fill=Estimate), color="black") +
  scale_fill_distiller(palette="Greys", direction=1) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Star=="*"),shape=21, color="black") +
  scale_fill_manual(values=star.colors, name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=Estimate)) +
  scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=Star=="*"), shape=21,stroke=1) +
  scale_fill_distiller(type="div", palette=4, limits=c(-max(abs(pairs.compl$Estimate)),max(abs(pairs.compl$Estimate)))) +
  scale_color_manual(values=c("grey90","black"), name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

numlinks.pairs.2.res.df$From <- factor(numlinks.pairs.2.res.df$From, levels=c(9,8,7,5,3,1))
numlinks.pairs.2.res.df$To <- factor(numlinks.pairs.2.res.df$To, levels=c(1,3,5,7,8,9))


ggplot(numlinks.pairs.2.res.df) +
  geom_count(aes(x=To,y=From,size=abs(Estimate),fill=Estimate, color=Star=="*"), shape=21,stroke=1) +
  scale_fill_distiller(type="div", palette=4, limits=c(-max(abs(pairs.compl$Estimate)),max(abs(pairs.compl$Estimate)))) +
  scale_color_manual(values=c("grey90","black"), name="Outside 95% CI") +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

#### Least Squares Means

Do for each categorical predictor. Final model: LogError ~ Condition +
Dataset + Condition:Dataset + (1 | Demo.ResponseID)

##### Condition

###### lsmeans

``` r
# trying lmerTest::lsmeansLT

# note = lmerTest::lsmeans will only report lsmeans for factor variables and is deprecated

graph.numlinks.lsmlt.cond <- lsmeansLT(graph.numlinks.lmer.full.int, test.effs = "Condition")

plot(graph.numlinks.lsmlt.cond) # not a completely terrible style, but not sorted properly
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-266-1.png)<!-- -->

``` r
graph.numlinks.lsmlt.cond.df <- dplyr::as_data_frame(graph.numlinks.lsmlt.cond$lsmeans.table)

graph.numlinks.lsmlt.cond.df
```

    ## # A tibble: 4 x 8
    ##   Condition Estimate `Standard Error`    DF `t-value` `Lower CI`
    ## * <fct>        <dbl>            <dbl> <dbl>     <dbl>      <dbl>
    ## 1 Ctrl        0.0822          0.00310   398      26.9     0.0762
    ## 2 Col         0.0698          0.00290   393      24.1     0.0641
    ## 3 Phr         0.0975          0.00290   393      33.9     0.0919
    ## 4 Siz         0.118           0.00290   395      40.1     0.112 
    ## # ... with 2 more variables: `Upper CI` <dbl>, `p-value` <dbl>

``` r
graph.numlinks.lsmlt.cond.df$Condition <- factor(graph.numlinks.lsmlt.cond.df$Condition, levels=graph.numlinks.lsmlt.cond.df %>% arrange(desc(Estimate)) %>% dplyr::select(Condition) %>% unlist())

graph.numlinks.lsmlt.cond.df %>% arrange(desc(Estimate))
```

    ## # A tibble: 4 x 8
    ##   Condition Estimate `Standard Error`    DF `t-value` `Lower CI`
    ##   <fct>        <dbl>            <dbl> <dbl>     <dbl>      <dbl>
    ## 1 Siz         0.118           0.00290   395      40.1     0.112 
    ## 2 Phr         0.0975          0.00290   393      33.9     0.0919
    ## 3 Ctrl        0.0822          0.00310   398      26.9     0.0762
    ## 4 Col         0.0698          0.00290   393      24.1     0.0641
    ## # ... with 2 more variables: `Upper CI` <dbl>, `p-value` <dbl>

``` r
graph.numlinks.lsmlt.cond.df <- graph.numlinks.lsmlt.cond.df %>% 
  mutate(sig.levels = factor(case_when(
    `p-value` < .0001 ~ sig.level.names[1],
    `p-value` < .001 ~ sig.level.names[2],
    `p-value` < .01 ~ sig.level.names[3],
    `p-value` < .05 ~ sig.level.names[4],
    TRUE ~ sig.level.names[5]
    )
  ,levels=sig.level.names,ordered=TRUE))

graph.numlinks.lsmlt.cond.df
```

    ## # A tibble: 4 x 9
    ##   Condition Estimate `Standard Error`    DF `t-value` `Lower CI`
    ##   <fct>        <dbl>            <dbl> <dbl>     <dbl>      <dbl>
    ## 1 Ctrl        0.0822          0.00310   398      26.9     0.0762
    ## 2 Col         0.0698          0.00290   393      24.1     0.0641
    ## 3 Phr         0.0975          0.00290   393      33.9     0.0919
    ## 4 Siz         0.118           0.00290   395      40.1     0.112 
    ## # ... with 3 more variables: `Upper CI` <dbl>, `p-value` <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numlinks.lsmlt.cond.df) +
  geom_errorbar(aes(x=Condition,ymax=`Upper CI`,ymin=`Lower CI`), width=.2) +
  geom_point(aes(x=Condition,y=Estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-266-2.png)<!-- -->

``` r
# LSMeans Difference data

graph.numlinks.difflsmlt.cond <- difflsmeans(graph.numlinks.lmer.full.int, test.effs = "Condition")

plot(graph.numlinks.difflsmlt.cond)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-267-1.png)<!-- -->

``` r
graph.numlinks.difflsmlt.cond.df <- dplyr::as_data_frame(graph.numlinks.difflsmlt.cond$diffs.lsmeans.table, rownames="Pair")

graph.numlinks.difflsmlt.cond.df
```

    ## # A tibble: 6 x 8
    ##   Pair     Estimate `Standard Error`    DF `t-value` `Lower CI` `Upper CI`
    ##   <chr>       <dbl>            <dbl> <dbl>     <dbl>      <dbl>      <dbl>
    ## 1 Conditi…   0.0125          0.00420   396      2.96    0.00420    0.0207 
    ## 2 Conditi…  -0.0153          0.00420   396    - 3.65   -0.0236    -0.00710
    ## 3 Conditi…  -0.0358          0.00420   397    - 8.44   -0.0441    -0.0274 
    ## 4 Conditi…  -0.0278          0.00410   393    - 6.81   -0.0358    -0.0197 
    ## 5 Conditi…  -0.0482          0.00410   394    -11.7    -0.0563    -0.0401 
    ## 6 Conditi…  -0.0204          0.00410   394    - 4.97   -0.0285    -0.0124 
    ## # ... with 1 more variable: `p-value` <dbl>

``` r
graph.numlinks.difflsmlt.cond.df <- graph.numlinks.difflsmlt.cond.df %>% mutate(Pair=sub("Condition ","",Pair)) %>% separate(Pair, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numlinks.difflsmlt.cond.df$sig.levels <- 
  case_when(graph.numlinks.difflsmlt.cond.df$`p-value` < .0001 ~ sig.level.names[1],
            graph.numlinks.difflsmlt.cond.df$`p-value` < .001 ~ sig.level.names[2],
            graph.numlinks.difflsmlt.cond.df$`p-value` < .01 ~ sig.level.names[3],
            graph.numlinks.difflsmlt.cond.df$`p-value` < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numlinks.difflsmlt.cond.df$sig.levels <- factor(graph.numlinks.difflsmlt.cond.df$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numlinks.difflsmlt.cond.df$Pair <- factor(graph.numlinks.difflsmlt.cond.df$Pair, levels=graph.numlinks.difflsmlt.cond.df %>% arrange(desc(Estimate)) %>% dplyr::select(Pair) %>% distinct() %>% unlist())

#graph.numlinks.difflsmlt.cond.df %>% arrange(desc(Estimate))
graph.numlinks.difflsmlt.cond.df %>% arrange(Estimate)
```

    ## # A tibble: 6 x 11
    ##   Pair    From  To    Estimate `Standard Error`    DF `t-value` `Lower CI`
    ##   <fct>   <chr> <chr>    <dbl>            <dbl> <dbl>     <dbl>      <dbl>
    ## 1 Col - … Col   Siz    -0.0482          0.00410   394    -11.7    -0.0563 
    ## 2 Ctrl -… Ctrl  Siz    -0.0358          0.00420   397    - 8.44   -0.0441 
    ## 3 Col - … Col   Phr    -0.0278          0.00410   393    - 6.81   -0.0358 
    ## 4 Phr - … Phr   Siz    -0.0204          0.00410   394    - 4.97   -0.0285 
    ## 5 Ctrl -… Ctrl  Phr    -0.0153          0.00420   396    - 3.65   -0.0236 
    ## 6 Ctrl -… Ctrl  Col     0.0125          0.00420   396      2.96    0.00420
    ## # ... with 3 more variables: `Upper CI` <dbl>, `p-value` <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numlinks.difflsmlt.cond.df) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_point(aes(x=Pair,y=Estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-267-2.png)<!-- -->

``` r
ggplot(graph.numlinks.difflsmlt.cond.df) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_point(aes(x=Pair,y=Estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-267-3.png)<!-- -->

``` r
cond.copy <- graph.numlinks.difflsmlt.cond.df %>% rename(From=To,To=From) %>%
    mutate(Estimate=-Estimate,
         `t-value`=-`t-value`,
         `Lower CI`=-`Lower CI`,
         `Upper CI`=-`Upper CI`)

graph.numlinks.difflsmlt.cond.df.full <- bind_rows(graph.numlinks.difflsmlt.cond.df, cond.copy)

cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numlinks.difflsmlt.cond.df.full$From <- factor(graph.numlinks.difflsmlt.cond.df.full$From, levels=cond.lev)
graph.numlinks.difflsmlt.cond.df.full$To <- factor(graph.numlinks.difflsmlt.cond.df.full$To, levels=cond.lev)


ggplot(graph.numlinks.difflsmlt.cond.df.full) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-267-4.png)<!-- -->

``` r
ggplot(graph.numlinks.difflsmlt.cond.df.full) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(Estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-267-5.png)<!-- -->

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numlinks.emm.cond <- emmeans(graph.numlinks.lmer.full.int, "Condition", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numlinks.emm.cond
```

    ##  Condition     emmean          SE     df   lower.CL   upper.CL
    ##  Ctrl      0.08220255 0.003052032 395.65 0.07620233 0.08820278
    ##  Col       0.06975104 0.002891223 390.05 0.06406671 0.07543537
    ##  Phr       0.09751960 0.002876970 389.88 0.09186328 0.10317591
    ##  Siz       0.11796944 0.002939035 391.98 0.11219120 0.12374769
    ## 
    ## Results are averaged over the levels of: Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numlinks.emm.cond.df <- dplyr::as_data_frame(graph.numlinks.emm.cond)

graph.numlinks.emm.cond.df
```

    ## # A tibble: 4 x 6
    ##   Condition emmean      SE    df lower.CL upper.CL
    ## * <fct>      <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 Ctrl      0.0822 0.00305   396   0.0762   0.0882
    ## 2 Col       0.0698 0.00289   390   0.0641   0.0754
    ## 3 Phr       0.0975 0.00288   390   0.0919   0.103 
    ## 4 Siz       0.118  0.00294   392   0.112    0.124

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numlinks.emm.cond.cld <- cld(graph.numlinks.emm.cond,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numlinks.emm.cond.cld.df <- graph.numlinks.emm.cond.cld$emmeans

graph.numlinks.emm.cond.cld.df %>% dplyr::select(Condition,.group)
```

    ##  Condition .group
    ##  Col        1    
    ##  Ctrl        2   
    ##  Phr          3  
    ##  Siz           4 
    ## 
    ## Results are averaged over the levels of: Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numlinks.emm.cond.cld.df
```

    ##  Condition     emmean          SE     df   lower.CL   upper.CL .group
    ##  Col       0.06975104 0.002891223 390.05 0.06406671 0.07543537  1    
    ##  Ctrl      0.08220255 0.003052032 395.65 0.07620233 0.08820278   2   
    ##  Phr       0.09751960 0.002876970 389.88 0.09186328 0.10317591    3  
    ##  Siz       0.11796944 0.002939035 391.98 0.11219120 0.12374769     4 
    ## 
    ## Results are averaged over the levels of: Dataset 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numlinks.emm.cond.cld.df$Condition <- factor(graph.numlinks.emm.cond.cld.df$Condition, levels=graph.numlinks.emm.cond.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numlinks.emm.cond, ~Condition, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-268-1.png)<!-- -->

``` r
plot(graph.numlinks.emm.cond)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-268-2.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numlinks.emm.cond.cld.df %>% arrange(desc(emmean))
```

    ##   Condition     emmean          SE       df   lower.CL   upper.CL .group
    ## 1       Siz 0.11796944 0.002939035 391.9753 0.11219120 0.12374769      4
    ## 2       Phr 0.09751960 0.002876970 389.8795 0.09186328 0.10317591     3 
    ## 3      Ctrl 0.08220255 0.003052032 395.6472 0.07620233 0.08820278    2  
    ## 4       Col 0.06975104 0.002891223 390.0513 0.06406671 0.07543537   1

``` r
ggplot(graph.numlinks.emm.cond.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-268-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.cond.cld.df) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  coord_flip() +
labs(title="Estimated Marginal Means for Condition for Number of Links task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-268-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numlinks.emm.cond.pairs <- dplyr::as_data_frame(pairs(graph.numlinks.emm.cond)) 
graph.numlinks.emm.cond.pairs
```

    ## # A tibble: 6 x 6
    ##   contrast   estimate      SE    df t.ratio        p.value
    ## * <fct>         <dbl>   <dbl> <dbl>   <dbl>          <dbl>
    ## 1 Ctrl - Col   0.0125 0.00420   393    2.96 0.0170        
    ## 2 Ctrl - Phr  -0.0153 0.00419   393  - 3.65 0.00168       
    ## 3 Ctrl - Siz  -0.0358 0.00424   394  - 8.44 0             
    ## 4 Col - Phr   -0.0278 0.00408   390  - 6.81 0.000000000209
    ## 5 Col - Siz   -0.0482 0.00412   391  -11.7  0             
    ## 6 Phr - Siz   -0.0204 0.00411   391  - 4.97 0.00000591

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numlinks.emm.cond))

graph.numlinks.emm.cond.pairs <- full_join(graph.numlinks.emm.cond.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numlinks.emm.cond))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-269-1.png)<!-- -->

``` r
plot(pairs(graph.numlinks.emm.cond), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-269-2.png)<!-- -->

``` r
graph.numlinks.emm.cond.pairs$sig.levels <- 
  case_when(graph.numlinks.emm.cond.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numlinks.emm.cond.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numlinks.emm.cond.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numlinks.emm.cond.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numlinks.emm.cond.pairs$sig.levels <- factor(graph.numlinks.emm.cond.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numlinks.emm.cond.pairs$contrast <- factor(graph.numlinks.emm.cond.pairs$contrast, levels=graph.numlinks.emm.cond.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numlinks.emm.cond.pairs <- graph.numlinks.emm.cond.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numlinks.emm.cond.pairs %>% arrange(estimate)
```

    ## # A tibble: 6 x 11
    ##   contrast  From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##   <fct>     <chr> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 Col - Siz Col   Siz    -0.0482 0.00412   391  -11.7    0        -0.0589 
    ## 2 Ctrl - S… Ctrl  Siz    -0.0358 0.00424   394  - 8.44   0        -0.0467 
    ## 3 Col - Phr Col   Phr    -0.0278 0.00408   390  - 6.81   2.09e⁻¹⁰ -0.0383 
    ## 4 Phr - Siz Phr   Siz    -0.0204 0.00411   391  - 4.97   5.91e⁻ ⁶ -0.0311 
    ## 5 Ctrl - P… Ctrl  Phr    -0.0153 0.00419   393  - 3.65   1.68e⁻ ³ -0.0261 
    ## 6 Ctrl - C… Ctrl  Col     0.0125 0.00420   393    2.96   1.70e⁻ ²  0.00160
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numlinks.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-269-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-269-4.png)<!-- -->

``` r
copy <- graph.numlinks.emm.cond.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numlinks.emm.cond.pairs.compl <- bind_rows(graph.numlinks.emm.cond.pairs,copy)


cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numlinks.emm.cond.pairs.compl$From <- factor(graph.numlinks.emm.cond.pairs.compl$From, levels=cond.lev)
graph.numlinks.emm.cond.pairs.compl$To <- factor(graph.numlinks.emm.cond.pairs.compl$To, levels=cond.lev)

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numlinks.emm.cond.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 12 x 11
    ##    contrast  From  To    estimate      SE    df t.ratio   p.value lower.CL
    ##    <fct>     <fct> <fct>    <dbl>   <dbl> <dbl>   <dbl>     <dbl>    <dbl>
    ##  1 Col - Siz Col   Siz    -0.0482 0.00412   391  -11.7   0        -0.0589 
    ##  2 Ctrl - S… Ctrl  Siz    -0.0358 0.00424   394  - 8.44  0        -0.0467 
    ##  3 Col - Phr Col   Phr    -0.0278 0.00408   390  - 6.81  2.09e⁻¹⁰ -0.0383 
    ##  4 Phr - Siz Phr   Siz    -0.0204 0.00411   391  - 4.97  5.91e⁻ ⁶ -0.0311 
    ##  5 Ctrl - P… Ctrl  Phr    -0.0153 0.00419   393  - 3.65  1.68e⁻ ³ -0.0261 
    ##  6 Ctrl - C… Col   Ctrl   -0.0125 0.00420   393  - 2.96  1.70e⁻ ² -0.00160
    ##  7 Ctrl - C… Ctrl  Col     0.0125 0.00420   393    2.96  1.70e⁻ ²  0.00160
    ##  8 Ctrl - P… Phr   Ctrl    0.0153 0.00419   393    3.65  1.68e⁻ ³  0.0261 
    ##  9 Phr - Siz Siz   Phr     0.0204 0.00411   391    4.97  5.91e⁻ ⁶  0.0311 
    ## 10 Col - Phr Phr   Col     0.0278 0.00408   390    6.81  2.09e⁻¹⁰  0.0383 
    ## 11 Ctrl - S… Siz   Ctrl    0.0358 0.00424   394    8.44  0         0.0467 
    ## 12 Col - Siz Siz   Col     0.0482 0.00412   391   11.7   0         0.0589 
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numlinks.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-270-1.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-270-2.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-270-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-270-4.png)<!-- -->

###### networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.numlinks.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*50
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.numlinks.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-272-1.png)<!-- -->

##### Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numhighdeg, table(Dataset,Overestimated))
# ref_grid(graph.numhd.lmer.full.int.4) @ grid; .wgt. is number of observations

graph.numlinks.emm.data <- emmeans(graph.numlinks.lmer.full.int, "Dataset", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numlinks.emm.data
```

    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.05845206 0.003186206 1164.91 0.05220072 0.06470340
    ##  3       0.07900801 0.003215622 1164.87 0.07269896 0.08531707
    ##  5       0.04891687 0.003213827 1164.86 0.04261133 0.05522240
    ##  7       0.09347335 0.003229726 1164.80 0.08713662 0.09981008
    ##  8       0.11524497 0.003214446 1164.85 0.10893822 0.12155172
    ##  9       0.15606869 0.003248874 1164.74 0.14969439 0.16244299
    ## 
    ## Results are averaged over the levels of: Condition 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numlinks.emm.data.df <- dplyr::as_data_frame(graph.numlinks.emm.data)

graph.numlinks.emm.data.df
```

    ## # A tibble: 6 x 6
    ##   Dataset emmean      SE    df lower.CL upper.CL
    ## * <fct>    <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1       0.0585 0.00319  1165   0.0522   0.0647
    ## 2 3       0.0790 0.00322  1165   0.0727   0.0853
    ## 3 5       0.0489 0.00321  1165   0.0426   0.0552
    ## 4 7       0.0935 0.00323  1165   0.0871   0.0998
    ## 5 8       0.115  0.00321  1165   0.109    0.122 
    ## 6 9       0.156  0.00325  1165   0.150    0.162

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numlinks.emm.data.cld <- cld(graph.numlinks.emm.data,
                details=TRUE,
                #alpha=0.01,
                #by="Dataset",
                #Letters="|||||||||||||||||||",
                sort=TRUE
)

graph.numlinks.emm.data.cld.df <- graph.numlinks.emm.data.cld$emmeans

graph.numlinks.emm.data.cld.df %>% dplyr::select(Dataset,.group)
```

    ##  Dataset .group
    ##  5        1    
    ##  1        1    
    ##  3         2   
    ##  7          3  
    ##  8           4 
    ##  9            5
    ## 
    ## Results are averaged over the levels of: Condition 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numlinks.emm.data.cld.df
```

    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  5       0.04891687 0.003213827 1164.86 0.04261133 0.05522240  1    
    ##  1       0.05845206 0.003186206 1164.91 0.05220072 0.06470340  1    
    ##  3       0.07900801 0.003215622 1164.87 0.07269896 0.08531707   2   
    ##  7       0.09347335 0.003229726 1164.80 0.08713662 0.09981008    3  
    ##  8       0.11524497 0.003214446 1164.85 0.10893822 0.12155172     4 
    ##  9       0.15606869 0.003248874 1164.74 0.14969439 0.16244299      5
    ## 
    ## Results are averaged over the levels of: Condition 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
ggplot(graph.numlinks.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) 
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-273-1.png)<!-- -->

``` r
graph.numlinks.emm.data.cld.df$Dataset <- factor(graph.numlinks.emm.data.cld.df$Dataset, levels=graph.numlinks.emm.data.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Dataset) %>% unlist())


#emmip(graph.numlinks.lmer.full.int, ~Condition, CIs = TRUE)
emmip(graph.numlinks.emm.data, ~Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-273-2.png)<!-- -->

``` r
plot(graph.numlinks.emm.data)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-273-3.png)<!-- -->

``` r
#plot(graph.numlinks.emm.cond, comparisons = TRUE)

graph.numlinks.emm.data.cld.df %>% arrange(desc(emmean))
```

    ##   Dataset     emmean          SE       df   lower.CL   upper.CL .group
    ## 1       9 0.15606869 0.003248874 1164.743 0.14969439 0.16244299      5
    ## 2       8 0.11524497 0.003214446 1164.846 0.10893822 0.12155172     4 
    ## 3       7 0.09347335 0.003229726 1164.803 0.08713662 0.09981008    3  
    ## 4       3 0.07900801 0.003215622 1164.874 0.07269896 0.08531707   2   
    ## 5       1 0.05845206 0.003186206 1164.912 0.05220072 0.06470340  1    
    ## 6       5 0.04891687 0.003213827 1164.863 0.04261133 0.05522240  1

``` r
ggplot(graph.numlinks.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-273-4.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.data.cld.df) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  coord_flip() +
labs(title="Estimated Marginal Means for Dataset for Number of Links task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-273-5.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numlinks.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.numlinks.emm.data)) 
graph.numlinks.emm.data.pairs
```

    ## # A tibble: 15 x 6
    ##    contrast estimate      SE    df t.ratio             p.value
    ##  * <fct>       <dbl>   <dbl> <dbl>   <dbl>               <dbl>
    ##  1 1 - 3    -0.0206  0.00442  1030  - 4.65 0.0000549          
    ##  2 1 - 5     0.00954 0.00441  1011    2.16 0.256              
    ##  3 1 - 7    -0.0350  0.00441   989  - 7.95 0.00000000000102   
    ##  4 1 - 8    -0.0568  0.00442  1024  -12.9  0                  
    ##  5 1 - 9    -0.0976  0.00443  1010  -22.0  0.000000000000109  
    ##  6 3 - 5     0.0301  0.00441   987    6.82 0.000000000242     
    ##  7 3 - 7    -0.0145  0.00444  1019  - 3.26 0.0148             
    ##  8 3 - 8    -0.0362  0.00443  1015  - 8.18 0.000000000000464  
    ##  9 3 - 9    -0.0771  0.00446  1025  -17.3  0.0000000000000283 
    ## 10 5 - 7    -0.0446  0.00445  1038  -10.0  0                  
    ## 11 5 - 8    -0.0663  0.00442  1001  -15.0  0.000000000000545  
    ## 12 5 - 9    -0.107   0.00446  1025  -24.0  0.00000000000000500
    ## 13 7 - 8    -0.0218  0.00444  1012  - 4.90 0.0000161          
    ## 14 7 - 9    -0.0626  0.00446  1011  -14.0  0.000000000000373  
    ## 15 8 - 9    -0.0408  0.00445  1010  - 9.17 0.000000000000465

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numlinks.emm.data))

graph.numlinks.emm.data.pairs <- full_join(graph.numlinks.emm.data.pairs, pairs.CI)

#graph.numhd.diffemm.condition <- emmeans(graph.numhd.lmer.full.int.4, pairwise ~ Condition)
#graph.numhd.diffemm.condition$contrasts
#contrast(graph.numhd.emm.condition)
#confint(graph.numhd.emm.condition)
#pairs(graph.numhd.emm.condition, details=TRUE)
#confint(contrast(graph.numhd.emm.condition))
#confint(pairs(graph.numhd.emm.condition))
#coef(pairs(graph.numhd.emm.condition))


plot(pairs(graph.numlinks.emm.data))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-274-1.png)<!-- -->

``` r
plot(pairs(graph.numlinks.emm.data), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-274-2.png)<!-- -->

``` r
graph.numlinks.emm.data.pairs$sig.levels <- 
  case_when(graph.numlinks.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numlinks.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numlinks.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numlinks.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numlinks.emm.data.pairs$sig.levels <- factor(graph.numlinks.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numlinks.emm.data.pairs$contrast <- factor(graph.numlinks.emm.data.pairs$contrast, levels=graph.numlinks.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numlinks.emm.data.pairs <- graph.numlinks.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numlinks.emm.data.pairs %>% arrange(estimate)
```

    ## # A tibble: 15 x 11
    ##    contrast From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##    <fct>    <chr> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ##  1 5 - 9    5     9     -0.107   0.00446  1025  -24.0    5.00e⁻¹⁵ -0.120  
    ##  2 1 - 9    1     9     -0.0976  0.00443  1010  -22.0    1.09e⁻¹³ -0.110  
    ##  3 3 - 9    3     9     -0.0771  0.00446  1025  -17.3    2.83e⁻¹⁴ -0.0898 
    ##  4 5 - 8    5     8     -0.0663  0.00442  1001  -15.0    5.45e⁻¹³ -0.0790 
    ##  5 7 - 9    7     9     -0.0626  0.00446  1011  -14.0    3.73e⁻¹³ -0.0753 
    ##  6 1 - 8    1     8     -0.0568  0.00442  1024  -12.9    0        -0.0694 
    ##  7 5 - 7    5     7     -0.0446  0.00445  1038  -10.0    0        -0.0573 
    ##  8 8 - 9    8     9     -0.0408  0.00445  1010  - 9.17   4.65e⁻¹³ -0.0535 
    ##  9 3 - 8    3     8     -0.0362  0.00443  1015  - 8.18   4.64e⁻¹³ -0.0489 
    ## 10 1 - 7    1     7     -0.0350  0.00441   989  - 7.95   1.02e⁻¹² -0.0476 
    ## 11 7 - 8    7     8     -0.0218  0.00444  1012  - 4.90   1.61e⁻ ⁵ -0.0344 
    ## 12 1 - 3    1     3     -0.0206  0.00442  1030  - 4.65   5.49e⁻ ⁵ -0.0332 
    ## 13 3 - 7    3     7     -0.0145  0.00444  1019  - 3.26   1.48e⁻ ² -0.0272 
    ## 14 1 - 5    1     5      0.00954 0.00441  1011    2.16   2.56e⁻ ¹ -0.00305
    ## 15 3 - 5    3     5      0.0301  0.00441   987    6.82   2.42e⁻¹⁰  0.0175 
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numlinks.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-274-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-274-4.png)<!-- -->

``` r
data.copy <- graph.numlinks.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numlinks.emm.data.pairs.compl <- bind_rows(graph.numlinks.emm.data.pairs, data.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=cond.lev)
#graph.numlinks.emm.data.pairs.compl$To <- factor(graph.numlinks.emm.data.pairs.compl$To, levels=cond.lev)

graph.numlinks.emm.data.pairs.compl$From <- factor(graph.numlinks.emm.data.pairs.compl$From, levels=rev(unique(graph.numlinks.emm.data.pairs.compl$From)))

#graph.numlinks.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numlinks.emm.data.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 30 x 11
    ##    contrast From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##    <fct>    <fct> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ##  1 5 - 9    5     9      -0.107  0.00446  1025  -24.0    5.00e⁻¹⁵  -0.120 
    ##  2 1 - 9    1     9      -0.0976 0.00443  1010  -22.0    1.09e⁻¹³  -0.110 
    ##  3 3 - 9    3     9      -0.0771 0.00446  1025  -17.3    2.83e⁻¹⁴  -0.0898
    ##  4 5 - 8    5     8      -0.0663 0.00442  1001  -15.0    5.45e⁻¹³  -0.0790
    ##  5 7 - 9    7     9      -0.0626 0.00446  1011  -14.0    3.73e⁻¹³  -0.0753
    ##  6 1 - 8    1     8      -0.0568 0.00442  1024  -12.9    0         -0.0694
    ##  7 5 - 7    5     7      -0.0446 0.00445  1038  -10.0    0         -0.0573
    ##  8 8 - 9    8     9      -0.0408 0.00445  1010  - 9.17   4.65e⁻¹³  -0.0535
    ##  9 3 - 8    3     8      -0.0362 0.00443  1015  - 8.18   4.64e⁻¹³  -0.0489
    ## 10 1 - 7    1     7      -0.0350 0.00441   989  - 7.95   1.02e⁻¹²  -0.0476
    ## # ... with 20 more rows, and 2 more variables: upper.CL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-275-1.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-275-2.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-275-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-275-4.png)<!-- -->

##### Condition:Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numlinks.emm.conddata <- emmeans(graph.numlinks.lmer.full.int, ~ Dataset | Condition, nesting = NULL)
graph.numlinks.emm.conddata.2 <- emmeans(graph.numlinks.lmer.full.int, ~ Condition | Dataset, nesting = NULL)

graph.numlinks.emm.conddata
```

    ## Condition = Ctrl:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.03166701 0.006597512 1164.92 0.01872268 0.04461135
    ##  3       0.06702058 0.006597864 1164.95 0.05407556 0.07996561
    ##  5       0.04435960 0.006816744 1164.68 0.03098513 0.05773407
    ##  7       0.08027455 0.006816304 1164.62 0.06690094 0.09364816
    ##  8       0.15875974 0.006463136 1165.00 0.14607906 0.17144043
    ##  9       0.11113382 0.006816370 1164.63 0.09776008 0.12450756
    ## 
    ## Condition = Col:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.06585971 0.006332062 1164.78 0.05343618 0.07828323
    ##  3       0.07810960 0.006395255 1164.78 0.06556209 0.09065711
    ##  5       0.01845077 0.006271925 1164.90 0.00614524 0.03075631
    ##  7       0.02949194 0.006332178 1164.79 0.01706819 0.04191569
    ##  8       0.10163501 0.006458953 1164.59 0.08896252 0.11430749
    ##  9       0.12495921 0.006156409 1165.00 0.11288032 0.13703810
    ## 
    ## Condition = Phr:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.09660893 0.006156364 1164.99 0.08453012 0.10868773
    ##  3       0.06174548 0.006395021 1164.74 0.04919843 0.07429253
    ##  5       0.09039176 0.006332349 1164.80 0.07796767 0.10281585
    ##  7       0.03275502 0.006213545 1164.97 0.02056403 0.04494602
    ##  8       0.09989676 0.006332231 1164.79 0.08747290 0.11232061
    ##  9       0.20371962 0.006332501 1164.82 0.19129524 0.21614401
    ## 
    ## Condition = Siz:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL
    ##  1       0.03967260 0.006395899 1164.87 0.02712383 0.05222137
    ##  3       0.10915639 0.006333733 1164.95 0.09672959 0.12158319
    ##  5       0.04246533 0.006273380 1164.99 0.03015695 0.05477372
    ##  7       0.23137188 0.006459989 1164.77 0.21869737 0.24404640
    ##  8       0.10068837 0.006460283 1164.80 0.08801328 0.11336346
    ##  9       0.18446210 0.006664689 1164.27 0.17138595 0.19753824
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numlinks.emm.conddata.df <- dplyr::as_data_frame(graph.numlinks.emm.conddata)

graph.numlinks.emm.conddata.df #%>% View()
```

    ## # A tibble: 24 x 7
    ##    Dataset Condition emmean      SE    df lower.CL upper.CL
    ##  * <fct>   <fct>      <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ##  1 1       Ctrl      0.0317 0.00660  1165  0.0187    0.0446
    ##  2 3       Ctrl      0.0670 0.00660  1165  0.0541    0.0800
    ##  3 5       Ctrl      0.0444 0.00682  1165  0.0310    0.0577
    ##  4 7       Ctrl      0.0803 0.00682  1165  0.0669    0.0936
    ##  5 8       Ctrl      0.159  0.00646  1165  0.146     0.171 
    ##  6 9       Ctrl      0.111  0.00682  1165  0.0978    0.125 
    ##  7 1       Col       0.0659 0.00633  1165  0.0534    0.0783
    ##  8 3       Col       0.0781 0.00640  1165  0.0656    0.0907
    ##  9 5       Col       0.0185 0.00627  1165  0.00615   0.0308
    ## 10 7       Col       0.0295 0.00633  1165  0.0171    0.0419
    ## # ... with 14 more rows

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numlinks.emm.conddata.cld <- cld(graph.numlinks.emm.conddata,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )

graph.numlinks.emm.conddata.cld[["emmeans"]]
```

    ## Condition = Ctrl:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  1       0.03166701 0.006597512 1164.92 0.01872268 0.04461135  1    
    ##  5       0.04435960 0.006816744 1164.68 0.03098513 0.05773407  12   
    ##  3       0.06702058 0.006597864 1164.95 0.05407556 0.07996561   23  
    ##  7       0.08027455 0.006816304 1164.62 0.06690094 0.09364816    3  
    ##  9       0.11113382 0.006816370 1164.63 0.09776008 0.12450756     4 
    ##  8       0.15875974 0.006463136 1165.00 0.14607906 0.17144043      5
    ## 
    ## Condition = Col:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  5       0.01845077 0.006271925 1164.90 0.00614524 0.03075631  1    
    ##  7       0.02949194 0.006332178 1164.79 0.01706819 0.04191569  1    
    ##  1       0.06585971 0.006332062 1164.78 0.05343618 0.07828323   2   
    ##  3       0.07810960 0.006395255 1164.78 0.06556209 0.09065711   23  
    ##  8       0.10163501 0.006458953 1164.59 0.08896252 0.11430749    34 
    ##  9       0.12495921 0.006156409 1165.00 0.11288032 0.13703810     4 
    ## 
    ## Condition = Phr:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  7       0.03275502 0.006213545 1164.97 0.02056403 0.04494602  1    
    ##  3       0.06174548 0.006395021 1164.74 0.04919843 0.07429253   2   
    ##  5       0.09039176 0.006332349 1164.80 0.07796767 0.10281585    3  
    ##  1       0.09660893 0.006156364 1164.99 0.08453012 0.10868773    3  
    ##  8       0.09989676 0.006332231 1164.79 0.08747290 0.11232061    3  
    ##  9       0.20371962 0.006332501 1164.82 0.19129524 0.21614401     4 
    ## 
    ## Condition = Siz:
    ##  Dataset     emmean          SE      df   lower.CL   upper.CL .group
    ##  1       0.03967260 0.006395899 1164.87 0.02712383 0.05222137  1    
    ##  5       0.04246533 0.006273380 1164.99 0.03015695 0.05477372  1    
    ##  8       0.10068837 0.006460283 1164.80 0.08801328 0.11336346   2   
    ##  3       0.10915639 0.006333733 1164.95 0.09672959 0.12158319   2   
    ##  9       0.18446210 0.006664689 1164.27 0.17138595 0.19753824    3  
    ##  7       0.23137188 0.006459989 1164.77 0.21869737 0.24404640     4 
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numlinks.emm.conddata.cld.df <- graph.numlinks.emm.conddata.cld[["emmeans"]]

graph.numlinks.emm.conddata.cld.df %>% dplyr::select(Dataset,Condition,.group) %>% print()
```

    ## Condition = Ctrl:
    ##  Dataset .group
    ##  1        1    
    ##  5        12   
    ##  3         23  
    ##  7          3  
    ##  9           4 
    ##  8            5
    ## 
    ## Condition = Col:
    ##  Dataset .group
    ##  5        1    
    ##  7        1    
    ##  1         2   
    ##  3         23  
    ##  8          34 
    ##  9           4 
    ## 
    ## Condition = Phr:
    ##  Dataset .group
    ##  7        1    
    ##  3         2   
    ##  5          3  
    ##  1          3  
    ##  8          3  
    ##  9           4 
    ## 
    ## Condition = Siz:
    ##  Dataset .group
    ##  1        1    
    ##  5        1    
    ##  8         2   
    ##  3         2   
    ##  9          3  
    ##  7           4 
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numlinks.emm.conddata.2,details=TRUE,sort=TRUE)[["emmeans"]] %>% dplyr::select(Dataset,Condition,.group) %>% print()
```

    ## Dataset = 1:
    ##  Condition .group
    ##  Ctrl       1    
    ##  Siz        1    
    ##  Col         2   
    ##  Phr          3  
    ## 
    ## Dataset = 3:
    ##  Condition .group
    ##  Phr        1    
    ##  Ctrl       1    
    ##  Col        1    
    ##  Siz         2   
    ## 
    ## Dataset = 5:
    ##  Condition .group
    ##  Col        1    
    ##  Siz         2   
    ##  Ctrl        2   
    ##  Phr          3  
    ## 
    ## Dataset = 7:
    ##  Condition .group
    ##  Col        1    
    ##  Phr        1    
    ##  Ctrl        2   
    ##  Siz          3  
    ## 
    ## Dataset = 8:
    ##  Condition .group
    ##  Phr        1    
    ##  Siz        1    
    ##  Col        1    
    ##  Ctrl        2   
    ## 
    ## Dataset = 9:
    ##  Condition .group
    ##  Ctrl       1    
    ##  Col        1    
    ##  Siz         2   
    ##  Phr         2   
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numlinks.emm.conddata, ~Condition|Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-276-1.png)<!-- -->

``` r
plot(graph.numlinks.emm.conddata)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-276-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numlinks.emm.conddata.cld.df %>% arrange(desc(emmean))
```

    ##    Dataset Condition     emmean          SE       df   lower.CL   upper.CL
    ## 1        7       Siz 0.23137188 0.006459989 1164.766 0.21869737 0.24404640
    ## 2        9       Phr 0.20371962 0.006332501 1164.821 0.19129524 0.21614401
    ## 3        9       Siz 0.18446210 0.006664689 1164.268 0.17138595 0.19753824
    ## 4        8      Ctrl 0.15875974 0.006463136 1164.999 0.14607906 0.17144043
    ## 5        9       Col 0.12495921 0.006156409 1164.997 0.11288032 0.13703810
    ## 6        9      Ctrl 0.11113382 0.006816370 1164.625 0.09776008 0.12450756
    ## 7        3       Siz 0.10915639 0.006333733 1164.948 0.09672959 0.12158319
    ## 8        8       Col 0.10163501 0.006458953 1164.589 0.08896252 0.11430749
    ## 9        8       Siz 0.10068837 0.006460283 1164.800 0.08801328 0.11336346
    ## 10       8       Phr 0.09989676 0.006332231 1164.789 0.08747290 0.11232061
    ## 11       1       Phr 0.09660893 0.006156364 1164.994 0.08453012 0.10868773
    ## 12       5       Phr 0.09039176 0.006332349 1164.801 0.07796767 0.10281585
    ## 13       7      Ctrl 0.08027455 0.006816304 1164.616 0.06690094 0.09364816
    ## 14       3       Col 0.07810960 0.006395255 1164.785 0.06556209 0.09065711
    ## 15       3      Ctrl 0.06702058 0.006597864 1164.948 0.05407556 0.07996561
    ## 16       1       Col 0.06585971 0.006332062 1164.778 0.05343618 0.07828323
    ## 17       3       Phr 0.06174548 0.006395021 1164.744 0.04919843 0.07429253
    ## 18       5      Ctrl 0.04435960 0.006816744 1164.675 0.03098513 0.05773407
    ## 19       5       Siz 0.04246533 0.006273380 1164.989 0.03015695 0.05477372
    ## 20       1       Siz 0.03967260 0.006395899 1164.874 0.02712383 0.05222137
    ## 21       7       Phr 0.03275502 0.006213545 1164.968 0.02056403 0.04494602
    ## 22       1      Ctrl 0.03166701 0.006597512 1164.925 0.01872268 0.04461135
    ## 23       7       Col 0.02949194 0.006332178 1164.792 0.01706819 0.04191569
    ## 24       5       Col 0.01845077 0.006271925 1164.897 0.00614524 0.03075631
    ##    .group
    ## 1       4
    ## 2       4
    ## 3      3 
    ## 4       5
    ## 5       4
    ## 6      4 
    ## 7     2  
    ## 8      34
    ## 9     2  
    ## 10     3 
    ## 11     3 
    ## 12     3 
    ## 13    3  
    ## 14    23 
    ## 15   23  
    ## 16    2  
    ## 17    2  
    ## 18  12   
    ## 19   1   
    ## 20   1   
    ## 21   1   
    ## 22  1    
    ## 23   1   
    ## 24   1

``` r
ggplot(graph.numlinks.emm.conddata.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  #geom_point(aes(x=Condition,y=emmean), size=7) +
  geom_point(aes(x=Condition,y=emmean)) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Dataset) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-276-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.cld.df) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL, color=Condition), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=Dataset,y=emmean, color=Condition), position=position_dodge(width=0.2)) +
  geom_line(aes(x=Dataset,y=emmean, color=Condition, group=Condition), position=position_dodge(width=0.2)) +
  #scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  scale_color_discrete(labels=c("Control","Color","Phrasing","Size")) +
  labs(title="Estimated Marginal Means for Condition vs. Dataset for\nNumber of Links task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-276-4.png)<!-- -->

``` r
ggsave(file.path(figureDir, "emmeanscolordatanumlinksgraphics.pdf"), width=7, height=5)
```

``` r
graph.numlinks.emm.conddata.pairs <- dplyr::as_data_frame(pairs(graph.numlinks.emm.conddata)) 
graph.numlinks.emm.conddata.pairs #%>% View()
```

    ## # A tibble: 60 x 7
    ##    contrast Condition estimate      SE    df t.ratio           p.value
    ##  * <fct>    <fct>        <dbl>   <dbl> <dbl>   <dbl>             <dbl>
    ##  1 1 - 3    Ctrl       -0.0354 0.00911  1029  - 3.88 0.00154          
    ##  2 1 - 5    Ctrl       -0.0127 0.00921   989  - 1.38 0.740            
    ##  3 1 - 7    Ctrl       -0.0486 0.00925  1021  - 5.25 0.00000271       
    ##  4 1 - 8    Ctrl       -0.127  0.00898   997  -14.2  0                
    ##  5 1 - 9    Ctrl       -0.0795 0.00928  1038  - 8.57 0.000000000000493
    ##  6 3 - 5    Ctrl        0.0227 0.00921   990    2.46 0.137            
    ##  7 3 - 7    Ctrl       -0.0133 0.00925  1023  - 1.43 0.707            
    ##  8 3 - 8    Ctrl       -0.0917 0.00904  1047  -10.2  0                
    ##  9 3 - 9    Ctrl       -0.0441 0.00920   979  - 4.79 0.0000278        
    ## 10 5 - 7    Ctrl       -0.0359 0.00946  1060  - 3.80 0.00213          
    ## # ... with 50 more rows

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

conddata.pairs.CI <- confint(pairs(graph.numlinks.emm.conddata))

graph.numlinks.emm.conddata.pairs <- full_join(graph.numlinks.emm.conddata.pairs, conddata.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numlinks.emm.conddata))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-277-1.png)<!-- -->

``` r
plot(pairs(graph.numlinks.emm.conddata), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-277-2.png)<!-- -->

``` r
graph.numlinks.emm.conddata.pairs$sig.levels <- 
  case_when(graph.numlinks.emm.conddata.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numlinks.emm.conddata.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numlinks.emm.conddata.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numlinks.emm.conddata.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numlinks.emm.conddata.pairs$sig.levels <- factor(graph.numlinks.emm.conddata.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numlinks.emm.conddata.pairs$contrast.cond <- paste0(graph.numlinks.emm.conddata.pairs$contrast,graph.numlinks.emm.conddata.pairs$Condition)

graph.numlinks.emm.conddata.pairs$contrast.cond <- factor(graph.numlinks.emm.conddata.pairs$contrast.cond, levels=graph.numlinks.emm.conddata.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.cond) %>% distinct() %>% unlist())

#graph.numnodes.emm.conddata.pairs %>% View()

graph.numlinks.emm.conddata.pairs$contrast <- 
  factor(as.character(graph.numlinks.emm.conddata.pairs$contrast), 
         levels=graph.numlinks.emm.conddata.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numlinks.emm.conddata.pairs <- graph.numlinks.emm.conddata.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numlinks.emm.conddata.pairs %>% arrange(estimate)
```

    ## # A tibble: 60 x 13
    ##    contrast From  To    Condition estimate      SE    df t.ratio   p.value
    ##    <fct>    <chr> <chr> <fct>        <dbl>   <dbl> <dbl>   <dbl>     <dbl>
    ##  1 1 - 7    1     7     Siz         -0.192 0.00882   985   -21.7  0       
    ##  2 5 - 7    5     7     Siz         -0.189 0.00880  1035   -21.5  5.63e⁻¹³
    ##  3 7 - 9    7     9     Phr         -0.171 0.00863  1003   -19.8  0       
    ##  4 1 - 9    1     9     Siz         -0.145 0.00899  1004   -16.1  5.15e⁻¹³
    ##  5 5 - 9    5     9     Siz         -0.142 0.00892  1014   -15.9  4.94e⁻¹³
    ##  6 3 - 9    3     9     Phr         -0.142 0.00883  1064   -16.1  0       
    ##  7 1 - 8    1     8     Ctrl        -0.127 0.00898   997   -14.2  0       
    ##  8 3 - 7    3     7     Siz         -0.122 0.00880  1004   -13.9  0       
    ##  9 5 - 8    5     8     Ctrl        -0.114 0.00915  1008   -12.5  4.49e⁻¹³
    ## 10 5 - 9    5     9     Phr         -0.113 0.00871  1001   -13.0  0       
    ## # ... with 50 more rows, and 4 more variables: lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
#graph.numlinks.emm.conddata.pairs %>% View()

ggplot(graph.numlinks.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-277-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-277-4.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-277-5.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-277-6.png)<!-- -->

``` r
conddata.copy <- graph.numlinks.emm.conddata.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numlinks.emm.conddata.pairs.compl <- bind_rows(graph.numlinks.emm.conddata.pairs, conddata.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numlinks.emm.conddata.pairs.compl$From <- factor(graph.numlinks.emm.conddata.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numlinks.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numlinks.emm.conddata.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 120 x 13
    ##    contrast From  To    Condition estimate      SE    df t.ratio   p.value
    ##    <fct>    <fct> <chr> <fct>        <dbl>   <dbl> <dbl>   <dbl>     <dbl>
    ##  1 1 - 7    1     7     Siz         -0.192 0.00882   985   -21.7  0       
    ##  2 5 - 7    5     7     Siz         -0.189 0.00880  1035   -21.5  5.63e⁻¹³
    ##  3 7 - 9    7     9     Phr         -0.171 0.00863  1003   -19.8  0       
    ##  4 1 - 9    1     9     Siz         -0.145 0.00899  1004   -16.1  5.15e⁻¹³
    ##  5 5 - 9    5     9     Siz         -0.142 0.00892  1014   -15.9  4.94e⁻¹³
    ##  6 3 - 9    3     9     Phr         -0.142 0.00883  1064   -16.1  0       
    ##  7 7 - 8    8     7     Siz         -0.131 0.00890  1008   -14.7  2.43e⁻¹³
    ##  8 1 - 8    1     8     Ctrl        -0.127 0.00898   997   -14.2  0       
    ##  9 3 - 7    3     7     Siz         -0.122 0.00880  1004   -13.9  0       
    ## 10 5 - 8    5     8     Ctrl        -0.114 0.00915  1008   -12.5  4.49e⁻¹³
    ## # ... with 110 more rows, and 4 more variables: lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
ggplot(graph.numlinks.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-278-1.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-278-2.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-278-3.png)<!-- -->

``` r
ggplot(graph.numlinks.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-278-4.png)<!-- -->

### Number of Nodes

``` r
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```

``` r
source('GlobalSetup.R')
```

``` r
# load full graphics dataset for summary graphics

graphics_numnodes <- readRDS(file.path(analysisDataDir, "GraphicsNumNodes.rds"))
```

Extra
figures

``` r
graphics_numnodes %>% filter(Dataset==3, Condition=="Ctrl") %>% dplyr::select(Response,CorrectAnswer) %>% ggplot() + geom_histogram(aes(Response),binwidth = 5) + geom_vline(aes(xintercept=CorrectAnswer))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-282-1.png)<!-- -->

``` r
ggsave(file.path(figureDir, "resp3hist.pdf"), width=6, height=3)

graphics_numnodes %>% filter(Dataset==3, Condition=="Ctrl") %>% dplyr::select(NormResponse,NormCorrectAnswer) %>% ggplot() + geom_histogram(aes(NormResponse),binwidth = .05) + geom_vline(aes(xintercept=NormCorrectAnswer))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-282-2.png)<!-- -->

``` r
ggsave(file.path(figureDir, "normresp3hist.pdf"), width=6, height=3)

graphics_numnodes %>% filter(Dataset==3, Condition=="Ctrl") %>% dplyr::select(LogError) %>% ggplot() + geom_histogram(aes(LogError),binwidth = .01) + scale_x_continuous(limits = c(0,.3)) + scale_y_continuous(limits=c(0,10))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-282-3.png)<!-- -->

``` r
ggsave(file.path(figureDir, "logerror3hist.pdf"), width=6, height=3)


graphics_numnodes %>% filter(Dataset==9, Condition=="Ctrl") %>% dplyr::select(Response,CorrectAnswer) %>% ggplot() + geom_histogram(aes(Response),binwidth = 50) + geom_vline(aes(xintercept=CorrectAnswer))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-282-4.png)<!-- -->

``` r
ggsave(file.path(figureDir, "resp9hist.pdf"), width=6, height=3)

graphics_numnodes %>% filter(Dataset==9, Condition=="Ctrl") %>% dplyr::select(NormResponse,NormCorrectAnswer) %>% ggplot() + geom_histogram(aes(NormResponse),binwidth = .05) + geom_vline(aes(xintercept=NormCorrectAnswer))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-282-5.png)<!-- -->

``` r
ggsave(file.path(figureDir, "normresp9hist.pdf"), width=6, height=3)

graphics_numnodes %>% filter(Dataset==9, Condition=="Ctrl") %>% dplyr::select(LogError) %>% ggplot() + geom_histogram(aes(LogError),binwidth = .01) + scale_x_continuous(limits = c(0,.3)) + scale_y_continuous(limits=c(0,10))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-282-6.png)<!-- -->

``` r
ggsave(file.path(figureDir, "logerror9hist.pdf"), width=6, height=3)
```

#### lme4

``` r
# Condition

graph.numnodes.lmer.cond <- lmer(LogError ~ Condition + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.cond)

anova(graph.numnodes.lmer.cond)

# Condition is significant (p<2.2e-16); trying Ctrl_dummy

graph.numnodes.lmer.ctrldum <- lmer(LogError ~ Ctrl_dummy + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.ctrldum)

anova(graph.numnodes.lmer.ctrldum)

# Ctrl_dummy is significant (p=6.466e-08); trying ConditionPhrasing

graph.numnodes.lmer.condphr <- lmer(LogError ~ ConditionPhrasing + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.condphr)

anova(graph.numnodes.lmer.condphr)

# ConditionPhrasing is significant (p=0.000114); 

graph.numnodes.lmer.condgr <- lmer(LogError ~ ConditionGraphics + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.condgr)

anova(graph.numnodes.lmer.condgr)

# ConditionGraphics is significant (p=7.639e-10)

graph.numnodes.lmer.condcol <- lmer(LogError ~ ConditionColor + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.condcol)

anova(graph.numnodes.lmer.condcol)

# ConditionColor is significant (p=5.3e-09)

# trying Dataset

graph.numnodes.lmer.dataset <- lmer(LogError ~ Dataset + (1|Demo.ResponseID), data = graphics_numnodes, REML = T)

lmsum <- summary(graph.numnodes.lmer.dataset)
lmsum
#names(lmsum)

anova(graph.numnodes.lmer.dataset)

# Dataset is significant (p < 2.2e-16); trying QuestionOrder

graph.numnodes.lmer.qorder <- lmer(LogError ~ QuestionOrder + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.qorder)

anova(graph.numnodes.lmer.qorder)

# QuestionOrder is significant (p=0.0225); trying DatasetOrder

graph.numnodes.lmer.dorder <- lmer(LogError ~ DatasetOrder + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.dorder)

anova(graph.numnodes.lmer.dorder)

# DatasetOrder is significant (p=0.03128); trying DatasetDuration

graph.numnodes.lmer <- lmer(LogError ~ DatasetDuration + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# DatasetDuration is not significant; trying DatasetStartTime

graph.numnodes.lmer <- lmer(LogError ~ DatasetStartTime + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# DatasetStartTime is not significant; trying TaskOrder

graph.numnodes.lmer <- lmer(LogError ~ TaskOrder + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# TaskOrder is not significant; trying CorrectAnswer

graph.numnodes.lmer.correct <- lmer(LogError ~ CorrectAnswer + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.correct)

anova(graph.numnodes.lmer.correct)

# CorrectAnswer is highly significant (p < 2.2e-16); trying Underestimated

graph.numnodes.lmer.underest <- lmer(LogError ~ Underestimated + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.underest)

anova(graph.numnodes.lmer.underest)

# Underestimated is highly significant (p < 2.2e-16); trying Overestimated

graph.numnodes.lmer.overest <- lmer(LogError ~ Overestimated + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.overest)

anova(graph.numnodes.lmer.overest)

# Overestimated is somewhat significant (p = 0.01932); 

graph.numnodes.lmer.underdum <- lmer(LogError ~ UnderestDummy + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.underdum)

anova(graph.numnodes.lmer.underdum)

# UnderestDummy is highly significant (p < 2.2e-16); trying Stats.Q_TotalDuration

graph.numnodes.lmer <- lmer(LogError ~ Stats.Q_TotalDuration + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Stats.Q_TotalDuration is not significant; trying Stats.dataset_count

graph.numnodes.lmer <- lmer(LogError ~ Stats.dataset_count + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Stats.dataset_count is not signficant; trying Stats.OperatingSystem

graph.numnodes.lmer <- lmer(LogError ~ Stats.OperatingSystem + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Stats.OperatingSystem is not signficant; skipping combined

graphics_numnodes.CS <- graphics_numnodes %>% mutate(StatsNumPixels=scale(StatsNumPixels))

graph.numnodes.lmer <- lmer(LogError ~ StatsNumPixels + (1|Demo.ResponseID), data=graphics_numnodes.CS, REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# StatsNumPixels is not signficant; trying Demo.age

graph.numnodes.lmer <- lmer(LogError ~ Demo.age + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.age))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.age is not signficant; trying Demo.gender

graph.numnodes.lmer <- lmer(LogError ~ Demo.gender + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.gender))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.gender is not signficant; trying Demo.lang

graph.numnodes.lmer <- lmer(LogError ~ Demo.lang + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.lang))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.lang is not signficant; trying Demo.educ

graph.numnodes.lmer <- lmer(LogError ~ Demo.educ + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.educ))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.educ is not signficant; trying Demo.acfield

graph.numnodes.lmer <- lmer(LogError ~ Demo.acfield + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.acfield))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.acfield is not signficant overall; trying Demo.acfieldGrouped

graph.numnodes.lmer.acfieldgr <- lmer(LogError ~ Demo.acfieldGrouped + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.acfieldGrouped))), REML = T)

summary(graph.numnodes.lmer.acfieldgr)

anova(graph.numnodes.lmer.acfieldgr)

# Demo.acfieldGrouped is slightly signficant (p=0.02863)

graph.numnodes.lmer <- lmer(LogError ~ Demo.acfieldGrouped2 + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.acfieldGrouped2))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.acfieldGrouped2 is not signficant

graph.numnodes.lmer <- lmer(LogError ~ Demo.acfieldGrouped3 + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.acfieldGrouped3))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.acfieldGrouped3 is not signficant

graph.numnodes.lmer <- lmer(LogError ~ Demo.dailytech_Computer + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.dailytech_Computer))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.dailytech_Computer is not significant; trying Demo.dailytech_Tablet

graph.numnodes.lmer <- lmer(LogError ~ Demo.dailytech_Tablet + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.dailytech_Tablet))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.dailytech_Computer is not significant (p=0.08993)

graph.numnodes.lmer <- lmer(LogError ~ Demo.dailytech_SmartPhone + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.dailytech_SmartPhone))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.dailytech_SmartPhone is not significant; trying Demo.weeklygaming

graph.numnodes.lmer <- lmer(LogError ~ Demo.weeklygaming + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.weeklygaming))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.weeklygaming is not significant; trying Demo.expdataanal

graph.numnodes.lmer <- lmer(LogError ~ Demo.expdataanal + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.expdataanal))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.expdataanal is not significant; trying Demo.expdatavis

graph.numnodes.lmer <- lmer(LogError ~ Demo.expdatavis + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.expdatavis))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.expdatavis is not significant; trying Demo.expreadnetvis

graph.numnodes.lmer <- lmer(LogError ~ Demo.expreadnetvis + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.expreadnetvis))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.expreadnetvis is not significant; trying Demo.expreadnetvis.alot

graph.numnodes.lmer <- lmer(LogError ~ Demo.expreadnetvis.alot + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.expreadnetvis.alot))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.expreadnetvis.alot is not significant; trying Demo.expcreatenetvis

graph.numnodes.lmer <- lmer(LogError ~ Demo.expcreatenetvis + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.expcreatenetvis))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.expcreatenetvis is not significant; trying Demo.expcreatenetvis.alot

graph.numnodes.lmer <- lmer(LogError ~ Demo.expcreatenetvis.alot + (1|Demo.ResponseID), data=graphics_numnodes %>% filter(!(is.na(Demo.expcreatenetvis.alot))), REML = T)

summary(graph.numnodes.lmer)

anova(graph.numnodes.lmer)

# Demo.expcreatenetvis.alot is not significant; trying AvgDeg

graph.numnodes.lmer.avgdeg <- lmer(LogError ~ AvgDeg + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.avgdeg)

anova(graph.numnodes.lmer.avgdeg)

# AvgDeg is significant (p < 2.2e-16); trying Density

graph.numnodes.lmer.dens <- lmer(LogError ~ Density + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.dens)

anova(graph.numnodes.lmer.dens)

# Density is significant (p < 2.2e-16); trying LargeClust1

graph.numnodes.lmer.lgclust <- lmer(LogError ~ LargeClust1 + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.lgclust)

anova(graph.numnodes.lmer.lgclust)

# LargeClust1 is significant (p < 2.2e-16); trying Modularity

graph.numnodes.lmer.mod <- lmer(LogError ~ Modularity + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.mod)

anova(graph.numnodes.lmer.mod)

# Modularity is significant (p < 2.2e-16); trying NumClust

graph.numnodes.lmer.numclust <- lmer(LogError ~ NumClust + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.numclust)

anova(graph.numnodes.lmer.numclust)

# NumClust is significant (p < 2.2e-16); trying NumHighDegree

graph.numnodes.lmer.numhighdeg <- lmer(LogError ~ NumHighDegree + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.numhighdeg)

anova(graph.numnodes.lmer.numhighdeg)

# NumHighDegree is significant (p=1.098e-07); trying NumLinks

graph.numnodes.lmer.numlinks <- lmer(LogError ~ NumLinks + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.numlinks)

anova(graph.numnodes.lmer.numlinks)

# NumLinks is significant (p=0.0136); trying NumNodes

graph.numnodes.lmer.numnodes <- lmer(LogError ~ NumNodes + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.numnodes)

anova(graph.numnodes.lmer.numnodes)

# NumNodes is significant (p < 2.2e-16); trying NumNodesClust1

graph.numnodes.lmer.sizeclust1 <- lmer(LogError ~ NumNodesClust1 + (1|Demo.ResponseID), data=graphics_numnodes, REML = T)

summary(graph.numnodes.lmer.sizeclust1)

anova(graph.numnodes.lmer.sizeclust1)

# NumNodesClust1 is significant (p < 2.2e-16); 
```

``` r
temp.numnodes <- graphics_numnodes %>% dplyr::select(Demo.ResponseID, LogError, Condition, Ctrl_dummy, ConditionPhrasing, ConditionGraphics, ConditionColor, Dataset, QuestionOrder, DatasetOrder, CorrectAnswer, Underestimated, Overestimated, UnderestDummy, Demo.acfieldGrouped, AvgDeg, Density, LargeClust1, Modularity, NumClust, NumHighDegree, NumLinks, NumNodes, NumNodesClust1) %>% drop_na() %>% mutate(QuestionOrderSc=scale(QuestionOrder))
```

Run this one

``` r
graph.numnodes.lmer.full <- lmer(LogError ~ Condition + Dataset +
                                   QuestionOrderSc +
                                   DatasetOrder +
                                   UnderestDummy +
                                   Demo.acfieldGrouped +
                                   (1|Demo.ResponseID), data=temp.numnodes, REML = T)

summary(graph.numnodes.lmer.full)

anova(graph.numnodes.lmer.full)
```

Run this one

``` r
graph.numnodes.lmer.int <- lmer(LogError ~ Condition + Dataset +
                                  Condition:Dataset +
                                   QuestionOrderSc +
                                   DatasetOrder +
                                   UnderestDummy +
                                   Demo.acfieldGrouped +
                                   (1|Demo.ResponseID), data=temp.numnodes, REML = T)

summary(graph.numnodes.lmer.int)

anova(graph.numnodes.lmer.int)

anova(graph.numnodes.lmer.full, graph.numnodes.lmer.int)

graph.numnodes.lmer.int.2 <- lmer(LogError ~ Condition + Dataset +
                                  Condition:Dataset +
                                   QuestionOrderSc +
                                   UnderestDummy +
                                   Demo.acfieldGrouped +
                                   (1|Demo.ResponseID), data=temp.numnodes, REML = T)

summary(graph.numnodes.lmer.int.2)

anova(graph.numnodes.lmer.int.2)

anova(graph.numnodes.lmer.int, graph.numnodes.lmer.int.2) 
# not a significant difference; maybe keep the simpler one (2)

graph.numnodes.lmer.int.3 <- lmer(LogError ~ Condition + Dataset +
                                   QuestionOrderSc +
                                   UnderestDummy +
                                   Demo.acfieldGrouped +
                                  Condition:Dataset +
                                    Condition:UnderestDummy + 
                                   (1|Demo.ResponseID), data=temp.numnodes, REML = T)

summary(graph.numnodes.lmer.int.3)

anova(graph.numnodes.lmer.int.3)

anova(graph.numnodes.lmer.int.2, graph.numnodes.lmer.int.3) 
# keep 3

graph.numnodes.lmer.int.4 <- lmer(LogError ~ Condition + Dataset +
                                   QuestionOrderSc +
                                   UnderestDummy +
                                   Demo.acfieldGrouped +
                                  Condition:Dataset +
                                    Condition:UnderestDummy + 
                                    Dataset:UnderestDummy +
                                   (1|Demo.ResponseID), data=temp.numnodes, REML = T)

summary(graph.numnodes.lmer.int.4)

anova(graph.numnodes.lmer.int.4)

anova(graph.numnodes.lmer.int.3, graph.numnodes.lmer.int.4) 

# keep 3

graph.numnodes.lmer.int.5 <- lmer(LogError ~ Condition + Dataset +
                                   UnderestDummy +
                                   Demo.acfieldGrouped +
                                  Condition:Dataset +
                                    Condition:UnderestDummy + 
                                   (1|Demo.ResponseID), data=temp.numnodes, REML = T)

summary(graph.numnodes.lmer.int.5)

anova(graph.numnodes.lmer.int.5)

anova(graph.numnodes.lmer.int.3,graph.numnodes.lmer.int.5)

# keep 3
```

Run this one

``` r
#SAVE THE RESULTS
save(graph.numnodes.lmer.int.3, 
     file = file.path(analysisDataDir,"fits/graph_numnodes_lmer_int.RData"))
```

##### Load pre-built model

``` r
load(file.path(analysisDataDir,"fits/graph_numnodes_lmer_int.RData"))
```

``` r
rand(graph.numnodes.lmer.int.3)
```

    ## Analysis of Random effects Table:
    ##                 Chi.sq Chi.DF p.value    
    ## Demo.ResponseID   77.9      1  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# result shows that random effects of participant are significant (p<2e-16)

anova(graph.numnodes.lmer.int.3)
```

    ## Analysis of Variance Table of type III  with  Satterthwaite 
    ## approximation for degrees of freedom
    ##                          Sum Sq  Mean Sq NumDF   DenDF F.value    Pr(>F)
    ## Condition               0.07279 0.024264     3  463.36  19.843 4.073e-12
    ## Dataset                 0.42882 0.085763     5  927.93  70.135 < 2.2e-16
    ## QuestionOrderSc         0.00550 0.005499     1  373.84   4.497  0.034611
    ## UnderestDummy           0.01259 0.012590     1 1129.77  10.296  0.001371
    ## Demo.acfieldGrouped     0.02284 0.003806     6  376.78   3.113  0.005473
    ## Condition:Dataset       0.34846 0.023231    15  926.90  18.997 < 2.2e-16
    ## Condition:UnderestDummy 0.03969 0.013230     3 1128.40  10.819 5.199e-07
    ##                            
    ## Condition               ***
    ## Dataset                 ***
    ## QuestionOrderSc         *  
    ## UnderestDummy           ** 
    ## Demo.acfieldGrouped     ** 
    ## Condition:Dataset       ***
    ## Condition:UnderestDummy ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# SmartPhone significant, < 0.01

#ranef(graph.avgdeg.lmer.SP)

# displays the random effects; not that useful

# unlike lme(), lmer() doesn't allow for heterogeneous error variance structures (the "weights")

(r2nsj = r2beta(graph.numnodes.lmer.int.3, method = 'nsj', partial = TRUE))[1,'Rsq']
```

    ## [1] 0.4285135

``` r
ggplot(graphics_numnodes) + geom_histogram(aes(LogError), binwidth=.005) + labs(title="Distribution of LogError values for Number of Nodes task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-289-1.png)<!-- -->

``` r
plot(graph.numnodes.lmer.int.3)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-290-1.png)<!-- -->

``` r
plot(graph.numnodes.lmer.int.3, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-290-2.png)<!-- -->

``` r
plot(graph.numnodes.lmer.int.3, resid(.) ~ fitted(.) | Condition, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-290-3.png)<!-- -->

``` r
plot(graph.numnodes.lmer.int.3, resid(., scaled=TRUE) ~ fitted(.) | Condition, abline = 0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-290-4.png)<!-- -->

``` r
plot(graph.numnodes.lmer.int.3, LogError ~ fitted(.), abline = c(0,1))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-290-5.png)<!-- -->

``` r
graph.numnodes.lmer.int.3.f <- fortify(graph.numnodes.lmer.int.3)

ggplot(graph.numnodes.lmer.int.3.f, aes(.fitted,.resid)) + 
  geom_point() +
  #facet_grid(.~Sex) + 
  geom_hline(yintercept=0)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-291-1.png)<!-- -->

``` r
ggplot(graph.numnodes.lmer.int.3.f, aes(.fitted,LogError)) + 
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-291-2.png)<!-- -->

``` r
ggplot(graph.numnodes.lmer.int.3.f, aes(LogError,.fitted)) +
geom_point() +
geom_abline(aes(slope = 1, intercept = 0)) +
#scale_y_continuous(limits=c(0,0.3)) +
labs(title="Real vs. Predicted LogError values for Number of Nodes task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-291-3.png)<!-- -->

``` r
# TO DO: check out interpretation for these plots??

library(lattice)

prof <-  profile(graph.numnodes.lmer.int.3, optimizer="Nelder_Mead", which="beta_")

prof.CI <- confint(prof)

#CI2 <- confint(graph.numnodes.lmer.int.3, maxpts = 8)

xyplot(prof)

xyplot(prof, absVal = TRUE)

xyplot(prof, conf = c(0.95, 0.99), main = "95% and 99% profile() intervals")

# can also apply logProf() and varianceProf() to profile object

densityplot(prof)

splom(prof)
```

#### Least Squares Means

Do for each categorical predictor and interaction. Final model: LogError
~ Condition + Dataset + QuestionOrderSc + UnderestDummy +  
Demo.acfieldGrouped + Condition:Dataset + Condition:UnderestDummy + (1 |
Demo.ResponseID)

##### Condition

###### lsmeans

``` r
# trying lmerTest::lsmeansLT

# note = lmerTest::lsmeans will only report lsmeans for factor variables and is deprecated

#graph.numnodes.lsmlt.cond <- lsmeansLT(graph.numnodes.lmer.int.3, test.effs = "Condition")
#Error in eval(predvars, data, env) : object 'QuestionOrderSc' not found

graph.numnodes.lsmlt.cond <- lsmeansLT(graph.numnodes.lmer.int.5, test.effs = "Condition")

# model 3 not working, some problem with QuestionOrderSc

plot(graph.numnodes.lmer.int.5) 

graph.numnodes.lsmlt.cond.df <- dplyr::as_data_frame(graph.numnodes.lsmlt.cond$lsmeans.table)

graph.numnodes.lsmlt.cond.df

graph.numnodes.lsmlt.cond.df$Condition <- factor(graph.numnodes.lsmlt.cond.df$Condition, levels=graph.numnodes.lsmlt.cond.df %>% arrange(desc(Estimate)) %>% dplyr::select(Condition) %>% unlist())

graph.numnodes.lsmlt.cond.df %>% arrange(desc(Estimate))

graph.numnodes.lsmlt.cond.df <- graph.numnodes.lsmlt.cond.df %>% 
  mutate(sig.levels = factor(case_when(
    `p-value` < .0001 ~ sig.level.names[1],
    `p-value` < .001 ~ sig.level.names[2],
    `p-value` < .01 ~ sig.level.names[3],
    `p-value` < .05 ~ sig.level.names[4],
    TRUE ~ sig.level.names[5]
    )
  ,levels=sig.level.names,ordered=TRUE))

graph.numnodes.lsmlt.cond.df

ggplot(graph.numnodes.lsmlt.cond.df) +
  geom_errorbar(aes(x=Condition,ymax=`Upper CI`,ymin=`Lower CI`), width=.2) +
  geom_point(aes(x=Condition,y=Estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

``` r
# LSMeans Difference data

#graph.numnodes.difflsmlt.cond <- difflsmeans(graph.numnodes.lmer.int.3, test.effs = "Condition")
#Error in eval(predvars, data, env) : object 'QuestionOrderSc' not found
graph.numnodes.difflsmlt.cond <- difflsmeans(graph.numnodes.lmer.int.5, test.effs = "Condition")

plot(graph.numnodes.difflsmlt.cond)

graph.numnodes.difflsmlt.cond.df <- dplyr::as_data_frame(graph.numnodes.difflsmlt.cond$diffs.lsmeans.table, rownames="Pair")

graph.numnodes.difflsmlt.cond.df <- graph.numnodes.difflsmlt.cond.df %>% mutate(Pair=sub("Condition ","",Pair)) %>% separate(Pair, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numnodes.difflsmlt.cond.df$sig.levels <- 
  case_when(graph.numnodes.difflsmlt.cond.df$`p-value` < .0001 ~ sig.level.names[1],
            graph.numnodes.difflsmlt.cond.df$`p-value` < .001 ~ sig.level.names[2],
            graph.numnodes.difflsmlt.cond.df$`p-value` < .01 ~ sig.level.names[3],
            graph.numnodes.difflsmlt.cond.df$`p-value` < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.difflsmlt.cond.df$sig.levels <- factor(graph.numnodes.difflsmlt.cond.df$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numnodes.difflsmlt.cond.df$Pair <- factor(graph.numnodes.difflsmlt.cond.df$Pair, levels=graph.numnodes.difflsmlt.cond.df %>% arrange(desc(Estimate)) %>% dplyr::select(Pair) %>% distinct() %>% unlist())

#graph.numnodes.difflsmlt.cond.df %>% arrange(desc(Estimate))
graph.numnodes.difflsmlt.cond.df %>% arrange(Estimate)

ggplot(graph.numnodes.difflsmlt.cond.df) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_point(aes(x=Pair,y=Estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()

ggplot(graph.numnodes.difflsmlt.cond.df) +
  geom_errorbar(aes(x=Pair,ymax=`Upper CI`,ymin=`Lower CI`), width=.5) +
  geom_point(aes(x=Pair,y=Estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()




copy <- graph.numnodes.difflsmlt.cond.df %>% rename(From=To,To=From) %>% 
  mutate(Estimate=-Estimate,
         `t-value`=-`t-value`,
         `Lower CI`=-`Lower CI`,
         `Upper CI`=-`Upper CI`)

graph.numnodes.difflsmlt.cond.df.full <- bind_rows(graph.numnodes.difflsmlt.cond.df,copy)

cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numnodes.difflsmlt.cond.df.full$From <- factor(graph.numnodes.difflsmlt.cond.df.full$From, levels=cond.lev)
graph.numnodes.difflsmlt.cond.df.full$To <- factor(graph.numnodes.difflsmlt.cond.df.full$To, levels=cond.lev)


ggplot(graph.numnodes.difflsmlt.cond.df.full) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")

ggplot(graph.numnodes.difflsmlt.cond.df.full) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(Estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numnodes.emm.cond
```

    ##  Condition     emmean          SE     df   lower.CL   upper.CL
    ##  Ctrl      0.06788937 0.003705680 469.98 0.06060762 0.07517112
    ##  Col       0.03579620 0.003422664 461.25 0.02907026 0.04252215
    ##  Phr       0.04515076 0.003410755 453.89 0.03844793 0.05185359
    ##  Siz       0.06278993 0.003504850 450.52 0.05590205 0.06967782
    ## 
    ## Results are averaged over the levels of: Dataset, UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numnodes.emm.cond.df <- dplyr::as_data_frame(graph.numnodes.emm.cond)

graph.numnodes.emm.cond.df
```

    ## # A tibble: 4 x 6
    ##   Condition emmean      SE    df lower.CL upper.CL
    ## * <fct>      <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 Ctrl      0.0679 0.00371   470   0.0606   0.0752
    ## 2 Col       0.0358 0.00342   461   0.0291   0.0425
    ## 3 Phr       0.0452 0.00341   454   0.0384   0.0519
    ## 4 Siz       0.0628 0.00350   451   0.0559   0.0697

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

full.cld <- cld(graph.numnodes.emm.cond,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )
graph.numnodes.emm.cond.cld <- full.cld$emmeans

graph.numnodes.emm.cond.cld %>% dplyr::select(Condition,.group)
```

    ##  Condition .group
    ##  Col        1    
    ##  Phr        1    
    ##  Siz         2   
    ##  Ctrl        2   
    ## 
    ## Results are averaged over the levels of: Dataset, UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.cond.cld
```

    ##  Condition     emmean          SE     df   lower.CL   upper.CL .group
    ##  Col       0.03579620 0.003422664 461.25 0.02907026 0.04252215  1    
    ##  Phr       0.04515076 0.003410755 453.89 0.03844793 0.05185359  1    
    ##  Siz       0.06278993 0.003504850 450.52 0.05590205 0.06967782   2   
    ##  Ctrl      0.06788937 0.003705680 469.98 0.06060762 0.07517112   2   
    ## 
    ## Results are averaged over the levels of: Dataset, UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.cond.cld$Condition <- factor(graph.numnodes.emm.cond.cld$Condition, levels=graph.numnodes.emm.cond.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numnodes.emm.cond, ~Condition, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-295-1.png)<!-- -->

``` r
plot(graph.numnodes.emm.cond)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-295-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.cond, comparisons = TRUE)

graph.numnodes.emm.cond.cld %>% arrange(desc(emmean))
```

    ##   Condition     emmean          SE       df   lower.CL   upper.CL .group
    ## 1      Ctrl 0.06788937 0.003705680 469.9845 0.06060762 0.07517112      2
    ## 2       Siz 0.06278993 0.003504850 450.5169 0.05590205 0.06967782      2
    ## 3       Phr 0.04515076 0.003410755 453.8920 0.03844793 0.05185359     1 
    ## 4       Col 0.03579620 0.003422664 461.2505 0.02907026 0.04252215     1

``` r
ggplot(graph.numnodes.emm.cond.cld) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-295-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.cond.cld) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Condition for Number of Nodes task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-295-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Condition") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numnodes.emm.cond.pairs <- dplyr::as_data_frame(pairs(graph.numnodes.emm.cond)) 
graph.numnodes.emm.cond.pairs
```

    ## # A tibble: 6 x 6
    ##   contrast   estimate      SE    df t.ratio        p.value
    ## * <fct>         <dbl>   <dbl> <dbl>   <dbl>          <dbl>
    ## 1 Ctrl - Col  0.0321  0.00483   480    6.65 0.000000000535
    ## 2 Ctrl - Phr  0.0227  0.00481   467    4.73 0.0000180     
    ## 3 Ctrl - Siz  0.00510 0.00486   469    1.05 0.720         
    ## 4 Col - Phr  -0.00935 0.00465   468   -2.01 0.185         
    ## 5 Col - Siz  -0.0270  0.00467   472   -5.77 0.0000000839  
    ## 6 Phr - Siz  -0.0176  0.00468   461   -3.77 0.00106

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

pairs.CI <- confint(pairs(graph.numnodes.emm.cond))

graph.numnodes.emm.cond.pairs <- full_join(graph.numnodes.emm.cond.pairs, pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numnodes.emm.cond))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-296-1.png)<!-- -->

``` r
plot(pairs(graph.numnodes.emm.cond), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-296-2.png)<!-- -->

``` r
graph.numnodes.emm.cond.pairs$sig.levels <- 
  case_when(graph.numnodes.emm.cond.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numnodes.emm.cond.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numnodes.emm.cond.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numnodes.emm.cond.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.emm.cond.pairs$sig.levels <- factor(graph.numnodes.emm.cond.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numnodes.emm.cond.pairs$contrast <- factor(graph.numnodes.emm.cond.pairs$contrast, levels=graph.numnodes.emm.cond.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numnodes.emm.cond.pairs <- graph.numnodes.emm.cond.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numnodes.emm.cond.pairs %>% arrange(estimate)
```

    ## # A tibble: 6 x 11
    ##   contrast  From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##   <fct>     <chr> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 Col - Siz Col   Siz   -0.0270  0.00467   472   -5.77   8.39e⁻ ⁸ -0.0390 
    ## 2 Phr - Siz Phr   Siz   -0.0176  0.00468   461   -3.77   1.06e⁻ ³ -0.0297 
    ## 3 Col - Phr Col   Phr   -0.00935 0.00465   468   -2.01   1.85e⁻ ¹ -0.0213 
    ## 4 Ctrl - S… Ctrl  Siz    0.00510 0.00486   469    1.05   7.20e⁻ ¹ -0.00742
    ## 5 Ctrl - P… Ctrl  Phr    0.0227  0.00481   467    4.73   1.80e⁻ ⁵  0.0103 
    ## 6 Ctrl - C… Ctrl  Col    0.0321  0.00483   480    6.65   5.35e⁻¹⁰  0.0196 
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-296-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.cond.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-296-4.png)<!-- -->

``` r
copy <- graph.numnodes.emm.cond.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numnodes.emm.cond.pairs.compl <- bind_rows(graph.numnodes.emm.cond.pairs,copy)


cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numnodes.emm.cond.pairs.compl$From <- factor(graph.numnodes.emm.cond.pairs.compl$From, levels=cond.lev)
graph.numnodes.emm.cond.pairs.compl$To <- factor(graph.numnodes.emm.cond.pairs.compl$To, levels=cond.lev)

#graph.numnodes.emm.cond.pairs.compl %>% arrange(desc(estimate))
graph.numnodes.emm.cond.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 12 x 11
    ##    contrast  From  To    estimate      SE    df t.ratio   p.value lower.CL
    ##    <fct>     <fct> <fct>    <dbl>   <dbl> <dbl>   <dbl>     <dbl>    <dbl>
    ##  1 Ctrl - C… Col   Ctrl  -0.0321  0.00483   480   -6.65  5.35e⁻¹⁰ -0.0196 
    ##  2 Col - Siz Col   Siz   -0.0270  0.00467   472   -5.77  8.39e⁻ ⁸ -0.0390 
    ##  3 Ctrl - P… Phr   Ctrl  -0.0227  0.00481   467   -4.73  1.80e⁻ ⁵ -0.0103 
    ##  4 Phr - Siz Phr   Siz   -0.0176  0.00468   461   -3.77  1.06e⁻ ³ -0.0297 
    ##  5 Col - Phr Col   Phr   -0.00935 0.00465   468   -2.01  1.85e⁻ ¹ -0.0213 
    ##  6 Ctrl - S… Siz   Ctrl  -0.00510 0.00486   469   -1.05  7.20e⁻ ¹  0.00742
    ##  7 Ctrl - S… Ctrl  Siz    0.00510 0.00486   469    1.05  7.20e⁻ ¹ -0.00742
    ##  8 Col - Phr Phr   Col    0.00935 0.00465   468    2.01  1.85e⁻ ¹  0.0213 
    ##  9 Phr - Siz Siz   Phr    0.0176  0.00468   461    3.77  1.06e⁻ ³  0.0297 
    ## 10 Ctrl - P… Ctrl  Phr    0.0227  0.00481   467    4.73  1.80e⁻ ⁵  0.0103 
    ## 11 Col - Siz Siz   Col    0.0270  0.00467   472    5.77  8.39e⁻ ⁸  0.0390 
    ## 12 Ctrl - C… Ctrl  Col    0.0321  0.00483   480    6.65  5.35e⁻¹⁰  0.0196 
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-297-1.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-297-2.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.cond.pairs.compl) +
  geom_tile(aes(x=To,y=ordered(From,levels=rev(cond.lev)),fill=estimate)) +
    scale_fill_distiller(type="div", palette=4)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-297-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.cond.pairs.compl) +
  geom_count(aes(x=To,y=ordered(From,levels=rev(cond.lev)),size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-297-4.png)<!-- -->

###### Networks

``` r
# try a network of the pairwise significance values?  bleh...
# using igraph

graph <- graph_from_data_frame(graph.numnodes.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

E(graph)$width <- abs(E(graph)$estimate)*50
E(graph)$edge.color <- sig.colors[E(graph)$sig.levels]

graph

plot(graph, 
     edge.arrow.size=.5,
     vertex.frame.color="#555555")
```

``` r
# using ggraph and tidygraph

graph <- graph_from_data_frame(graph.numnodes.emm.cond.pairs %>% dplyr::select(-contrast), directed=TRUE)

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                     color=factor(sig.level.names[sig.levels]),
                     edge_width=.5/sig.levels
                     ), 
                   arrow = arrow(length = unit(4, 'mm')))+
  geom_node_text(label = vertex_attr(graph,"name")) +
  scale_edge_color_manual("Significance Levels", values=sig.colors) +
  scale_edge_width_continuous(labels=rev(sig.level.names)) +
  theme_graph()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-299-1.png)<!-- -->

##### Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

graph.numnodes.emm.data <- emmeans(graph.numnodes.lmer.int.3, "Dataset", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numnodes.emm.data
```

    ##  Dataset     emmean          SE      df    lower.CL   upper.CL
    ##  1       0.01431742 0.003059841 1079.27 0.008313507 0.02032133
    ##  3       0.04164320 0.003061386 1088.14 0.035636310 0.04765009
    ##  5       0.07818013 0.003273515 1102.46 0.071757108 0.08460315
    ##  7       0.04660252 0.003231103 1096.92 0.040262680 0.05294236
    ##  8       0.07751862 0.003205573 1097.43 0.071228875 0.08380837
    ##  9       0.05917752 0.003302084 1104.88 0.052698457 0.06565658
    ## 
    ## Results are averaged over the levels of: Condition, UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numnodes.emm.data.df <- dplyr::as_data_frame(graph.numnodes.emm.data)

graph.numnodes.emm.data.df
```

    ## # A tibble: 6 x 6
    ##   Dataset emmean      SE    df lower.CL upper.CL
    ## * <fct>    <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1       0.0143 0.00306  1079  0.00831   0.0203
    ## 2 3       0.0416 0.00306  1088  0.0356    0.0477
    ## 3 5       0.0782 0.00327  1102  0.0718    0.0846
    ## 4 7       0.0466 0.00323  1097  0.0403    0.0529
    ## 5 8       0.0775 0.00321  1097  0.0712    0.0838
    ## 6 9       0.0592 0.00330  1105  0.0527    0.0657

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numnodes.emm.data)

graph.numnodes.emm.data.cld <- cld(graph.numnodes.emm.data,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )
graph.numnodes.emm.data.cld.df <- graph.numnodes.emm.data.cld$emmeans

graph.numnodes.emm.data.cld.df %>% dplyr::select(Dataset,.group)
```

    ##  Dataset .group
    ##  1        1    
    ##  3         2   
    ##  7         2   
    ##  9          3  
    ##  8           4 
    ##  5           4 
    ## 
    ## Results are averaged over the levels of: Condition, UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.data.cld.df
```

    ##  Dataset     emmean          SE      df    lower.CL   upper.CL .group
    ##  1       0.01431742 0.003059841 1079.27 0.008313507 0.02032133  1    
    ##  3       0.04164320 0.003061386 1088.14 0.035636310 0.04765009   2   
    ##  7       0.04660252 0.003231103 1096.92 0.040262680 0.05294236   2   
    ##  9       0.05917752 0.003302084 1104.88 0.052698457 0.06565658    3  
    ##  8       0.07751862 0.003205573 1097.43 0.071228875 0.08380837     4 
    ##  5       0.07818013 0.003273515 1102.46 0.071757108 0.08460315     4 
    ## 
    ## Results are averaged over the levels of: Condition, UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.data.cld.df$Dataset <- factor(graph.numnodes.emm.data.cld.df$Dataset, levels=graph.numnodes.emm.data.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Dataset) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Dataset, CIs = TRUE)
emmip(graph.numnodes.emm.data, ~Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-300-1.png)<!-- -->

``` r
plot(graph.numnodes.emm.data)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-300-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.data, comparisons = TRUE)

graph.numnodes.emm.data.cld.df %>% arrange(desc(emmean))
```

    ##   Dataset     emmean          SE       df    lower.CL   upper.CL .group
    ## 1       5 0.07818013 0.003273515 1102.456 0.071757108 0.08460315      4
    ## 2       8 0.07751862 0.003205573 1097.426 0.071228875 0.08380837      4
    ## 3       9 0.05917752 0.003302084 1104.881 0.052698457 0.06565658     3 
    ## 4       7 0.04660252 0.003231103 1096.923 0.040262680 0.05294236    2  
    ## 5       3 0.04164320 0.003061386 1088.144 0.035636310 0.04765009    2  
    ## 6       1 0.01431742 0.003059841 1079.270 0.008313507 0.02032133   1

``` r
ggplot(graph.numnodes.emm.data.cld.df) +
  #geom_point(aes(x=Dataset,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-300-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.data.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Dataset,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Dataset for Number of Nodes task,\ngraphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-300-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Dataset") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numnodes.emm.data.pairs <- dplyr::as_data_frame(pairs(graph.numnodes.emm.data)) 
graph.numnodes.emm.data.pairs
```

    ## # A tibble: 15 x 6
    ##    contrast  estimate      SE    df t.ratio           p.value
    ##  * <fct>        <dbl>   <dbl> <dbl>   <dbl>             <dbl>
    ##  1 1 - 3    -0.0273   0.00381   935 - 7.17  0.0000000000229  
    ##  2 1 - 5    -0.0639   0.00404   957 -15.8   0.000000000000235
    ##  3 1 - 7    -0.0323   0.00404   950 - 7.99  0.000000000000707
    ##  4 1 - 8    -0.0632   0.00399   958 -15.8   0                
    ##  5 1 - 9    -0.0449   0.00410   963 -10.9   0.000000000000391
    ##  6 3 - 5    -0.0365   0.00386   919 - 9.47  0                
    ##  7 3 - 7    -0.00496  0.00390   938 - 1.27  0.800            
    ##  8 3 - 8    -0.0359   0.00384   929 - 9.33  0                
    ##  9 3 - 9    -0.0175   0.00393   945 - 4.46  0.000135         
    ## 10 5 - 7     0.0316   0.00382   933   8.27  0.000000000000108
    ## 11 5 - 8     0.000662 0.00376   910   0.176 1.000            
    ## 12 5 - 9     0.0190   0.00380   927   5.00  0.0000104        
    ## 13 7 - 8    -0.0309   0.00379   912 - 8.15  0                
    ## 14 7 - 9    -0.0126   0.00381   915 - 3.30  0.0126           
    ## 15 8 - 9     0.0183   0.00380   914   4.83  0.0000236

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

data.pairs.CI <- confint(pairs(graph.numnodes.emm.data))

graph.numnodes.emm.data.pairs <- full_join(graph.numnodes.emm.data.pairs, data.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numnodes.emm.data))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-301-1.png)<!-- -->

``` r
plot(pairs(graph.numnodes.emm.data), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-301-2.png)<!-- -->

``` r
graph.numnodes.emm.data.pairs$sig.levels <- 
  case_when(graph.numnodes.emm.data.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numnodes.emm.data.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numnodes.emm.data.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numnodes.emm.data.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.emm.data.pairs$sig.levels <- factor(graph.numnodes.emm.data.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numnodes.emm.data.pairs$contrast <- factor(graph.numnodes.emm.data.pairs$contrast, levels=graph.numnodes.emm.data.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numnodes.emm.data.pairs <- graph.numnodes.emm.data.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numnodes.emm.data.pairs %>% arrange(estimate)
```

    ## # A tibble: 15 x 11
    ##    contrast From  To     estimate      SE    df t.ratio   p.value lower.CL
    ##    <fct>    <chr> <chr>     <dbl>   <dbl> <dbl>   <dbl>     <dbl>    <dbl>
    ##  1 1 - 5    1     5     -0.0639   0.00404   957 -15.8    2.35e⁻¹³ -0.0754 
    ##  2 1 - 8    1     8     -0.0632   0.00399   958 -15.8    0        -0.0746 
    ##  3 1 - 9    1     9     -0.0449   0.00410   963 -10.9    3.91e⁻¹³ -0.0566 
    ##  4 3 - 5    3     5     -0.0365   0.00386   919 - 9.47   0        -0.0476 
    ##  5 3 - 8    3     8     -0.0359   0.00384   929 - 9.33   0        -0.0469 
    ##  6 1 - 7    1     7     -0.0323   0.00404   950 - 7.99   7.07e⁻¹³ -0.0438 
    ##  7 7 - 8    7     8     -0.0309   0.00379   912 - 8.15   0        -0.0417 
    ##  8 1 - 3    1     3     -0.0273   0.00381   935 - 7.17   2.29e⁻¹¹ -0.0382 
    ##  9 3 - 9    3     9     -0.0175   0.00393   945 - 4.46   1.35e⁻ ⁴ -0.0288 
    ## 10 7 - 9    7     9     -0.0126   0.00381   915 - 3.30   1.26e⁻ ² -0.0234 
    ## 11 3 - 7    3     7     -0.00496  0.00390   938 - 1.27   8.00e⁻ ¹ -0.0161 
    ## 12 5 - 8    5     8      0.000662 0.00376   910   0.176 10.00e⁻ ¹ -0.0101 
    ## 13 8 - 9    8     9      0.0183   0.00380   914   4.83   2.36e⁻ ⁵  0.00750
    ## 14 5 - 9    5     9      0.0190   0.00380   927   5.00   1.04e⁻ ⁵  0.00814
    ## 15 5 - 7    5     7      0.0316   0.00382   933   8.27   1.08e⁻¹³  0.0207 
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-301-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.data.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-301-4.png)<!-- -->

``` r
data.copy <- graph.numnodes.emm.data.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numnodes.emm.data.pairs.compl <- bind_rows(graph.numnodes.emm.data.pairs,data.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.cond.pairs.compl$From <- factor(graph.numnodes.emm.cond.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.cond.pairs.compl$To <- factor(graph.numnodes.emm.cond.pairs.compl$To, levels=cond.lev)

graph.numnodes.emm.data.pairs.compl$From <- factor(graph.numnodes.emm.data.pairs.compl$From, levels=c(9,8,7,5,3,1))

#graph.numnodes.emm.data.pairs.compl %>% arrange(desc(estimate))
graph.numnodes.emm.data.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 30 x 11
    ##    contrast From  To    estimate      SE    df t.ratio    p.value lower.CL
    ##    <fct>    <fct> <chr>    <dbl>   <dbl> <dbl>   <dbl>      <dbl>    <dbl>
    ##  1 1 - 5    1     5      -0.0639 0.00404   957  -15.8    2.35e⁻¹³ -0.0754 
    ##  2 1 - 8    1     8      -0.0632 0.00399   958  -15.8    0        -0.0746 
    ##  3 1 - 9    1     9      -0.0449 0.00410   963  -10.9    3.91e⁻¹³ -0.0566 
    ##  4 3 - 5    3     5      -0.0365 0.00386   919  - 9.47   0        -0.0476 
    ##  5 3 - 8    3     8      -0.0359 0.00384   929  - 9.33   0        -0.0469 
    ##  6 1 - 7    1     7      -0.0323 0.00404   950  - 7.99   7.07e⁻¹³ -0.0438 
    ##  7 5 - 7    7     5      -0.0316 0.00382   933  - 8.27   1.08e⁻¹³ -0.0207 
    ##  8 7 - 8    7     8      -0.0309 0.00379   912  - 8.15   0        -0.0417 
    ##  9 1 - 3    1     3      -0.0273 0.00381   935  - 7.17   2.29e⁻¹¹ -0.0382 
    ## 10 5 - 9    9     5      -0.0190 0.00380   927  - 5.00   1.04e⁻ ⁵ -0.00814
    ## # ... with 20 more rows, and 2 more variables: upper.CL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-302-1.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-302-2.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.data.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-302-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.data.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-302-4.png)<!-- -->

##### QuestionOrderSc

This is continuous; just plot against the estimate?

``` r
#graph.numnodes.lmer.int.3.f <- fortify(graph.numnodes.lmer.int.3)

#graph.numnodes.lmer.int.3.f %>% View()

ggplot(graph.numnodes.lmer.int.3.f, aes(.fitted,QuestionOrder)) + 
  geom_bin2d()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-303-1.png)<!-- -->

``` r
ggplot(temp.numnodes) + geom_point(aes(QuestionOrderSc, LogError))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-303-2.png)<!-- -->

``` r
ggplot(temp.numnodes) + geom_point(aes(QuestionOrderSc, LogError)) + geom_smooth(aes(QuestionOrderSc, LogError),method="lm") + labs(title="Overall Question Order (scaled) vs. LogError for\nNumber of Nodes task, graphics conditions", x="Overall Question Order (scaled)")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-303-3.png)<!-- -->

##### UnderestDummy

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

graph.numnodes.emm.underest <- emmeans(graph.numnodes.lmer.int.3, "UnderestDummy", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numnodes.emm.underest
```

    ##  UnderestDummy        emmean          SE     df   lower.CL   upper.CL
    ##  1-Underestimated 0.05768946 0.002059685 491.70 0.05364259 0.06173633
    ##  2-CorrectOrOver  0.04812368 0.002824056 853.27 0.04258077 0.05366659
    ## 
    ## Results are averaged over the levels of: Condition, Dataset, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numnodes.emm.underest.df <- dplyr::as_data_frame(graph.numnodes.emm.underest)

graph.numnodes.emm.underest.df
```

    ## # A tibble: 2 x 6
    ##   UnderestDummy    emmean      SE    df lower.CL upper.CL
    ## * <fct>             <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 1-Underestimated 0.0577 0.00206   492   0.0536   0.0617
    ## 2 2-CorrectOrOver  0.0481 0.00282   853   0.0426   0.0537

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numnodes.emm.data)

graph.numnodes.emm.underest.cld <- cld(graph.numnodes.emm.underest,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )
graph.numnodes.emm.underest.cld.df <- graph.numnodes.emm.underest.cld$emmeans

graph.numnodes.emm.underest.cld.df %>% dplyr::select(UnderestDummy,.group)
```

    ##  UnderestDummy    .group
    ##  2-CorrectOrOver   1    
    ##  1-Underestimated   2   
    ## 
    ## Results are averaged over the levels of: Condition, Dataset, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.underest.cld.df
```

    ##  UnderestDummy        emmean          SE     df   lower.CL   upper.CL
    ##  2-CorrectOrOver  0.04812368 0.002824056 853.27 0.04258077 0.05366659
    ##  1-Underestimated 0.05768946 0.002059685 491.70 0.05364259 0.06173633
    ##  .group
    ##   1    
    ##    2   
    ## 
    ## Results are averaged over the levels of: Condition, Dataset, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.underest.cld.df$UnderestDummy <- factor(graph.numnodes.emm.underest.cld.df$UnderestDummy, levels=graph.numnodes.emm.underest.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(UnderestDummy) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~UnderestDummy, CIs = TRUE)
emmip(graph.numnodes.emm.underest, ~UnderestDummy, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-304-1.png)<!-- -->

``` r
plot(graph.numnodes.emm.underest)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-304-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.underest, comparisons = TRUE)

graph.numnodes.emm.underest.cld.df %>% arrange(desc(emmean))
```

    ##      UnderestDummy     emmean          SE       df   lower.CL   upper.CL
    ## 1 1-Underestimated 0.05768946 0.002059685 491.7010 0.05364259 0.06173633
    ## 2  2-CorrectOrOver 0.04812368 0.002824056 853.2735 0.04258077 0.05366659
    ##   .group
    ## 1      2
    ## 2     1

``` r
ggplot(graph.numnodes.emm.underest.cld.df) +
  #geom_point(aes(x=UnderestDummy,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=UnderestDummy,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=UnderestDummy,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-304-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.underest.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=UnderestDummy,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=UnderestDummy,y=emmean), size=7) +
  scale_x_discrete(labels=c("Underestimated","Correct or\nOverestimated")) + 
labs(title="Estimated Marginal Means for Underestimated for Number of Nodes task,\ngraphics conditions",
     x="Underestimated") +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-304-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Dataset") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numnodes.emm.underest.pairs <- dplyr::as_data_frame(pairs(graph.numnodes.emm.underest)) 
graph.numnodes.emm.underest.pairs
```

    ## # A tibble: 1 x 6
    ##   contrast                          estimate      SE    df t.ratio p.value
    ## * <fct>                                <dbl>   <dbl> <dbl>   <dbl>   <dbl>
    ## 1 1-Underestimated - 2-CorrectOrOv…  0.00957 0.00299  1130    3.20 0.00140

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

underest.pairs.CI <- confint(pairs(graph.numnodes.emm.underest))

graph.numnodes.emm.underest.pairs <- full_join(graph.numnodes.emm.underest.pairs, underest.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numnodes.emm.underest))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-305-1.png)<!-- -->

``` r
plot(pairs(graph.numnodes.emm.underest), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-305-2.png)<!-- -->

``` r
graph.numnodes.emm.underest.pairs$sig.levels <- 
  case_when(graph.numnodes.emm.underest.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numnodes.emm.underest.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numnodes.emm.underest.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numnodes.emm.underest.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.emm.underest.pairs$sig.levels <- factor(graph.numnodes.emm.underest.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numnodes.emm.underest.pairs$contrast <- factor(graph.numnodes.emm.underest.pairs$contrast, levels=graph.numnodes.emm.underest.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

graph.numnodes.emm.underest.pairs <- graph.numnodes.emm.underest.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numnodes.emm.underest.pairs %>% arrange(estimate)
```

    ## # A tibble: 1 x 11
    ##   contrast    From   To    estimate      SE    df t.ratio p.value lower.CL
    ##   <fct>       <chr>  <chr>    <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ## 1 1-Underest… 1-Und… 2-Co…  0.00957 0.00299  1130    3.20 0.00140  0.00371
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.underest.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-305-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.underest.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-305-4.png)<!-- -->

``` r
underest.copy <- graph.numnodes.emm.underest.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numnodes.emm.underest.pairs.compl <- bind_rows(graph.numnodes.emm.underest.pairs, underest.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.cond.pairs.compl$From <- factor(graph.numnodes.emm.cond.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.cond.pairs.compl$To <- factor(graph.numnodes.emm.cond.pairs.compl$To, levels=cond.lev)

#graph.numnodes.emm.data.pairs.compl$From <- factor(graph.numnodes.emm.data.pairs.compl$From, levels=c(9,8,7,5,3,1))

#graph.numnodes.emm.underest.pairs.compl %>% arrange(desc(estimate))
graph.numnodes.emm.underest.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 2 x 11
    ##   contrast    From   To    estimate      SE    df t.ratio p.value lower.CL
    ##   <fct>       <chr>  <chr>    <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ## 1 1-Underest… 2-Cor… 1-Un… -0.00957 0.00299  1130   -3.20 0.00140 -0.00371
    ## 2 1-Underest… 1-Und… 2-Co…  0.00957 0.00299  1130    3.20 0.00140  0.00371
    ## # ... with 2 more variables: upper.CL <dbl>, sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.underest.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-306-1.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.underest.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-306-2.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.underest.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-306-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.underest.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black"))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-306-4.png)<!-- -->

##### Demo.acfieldGrouped

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

graph.numnodes.emm.acfieldgr <- emmeans(graph.numnodes.lmer.int.3, "Demo.acfieldGrouped", nesting = NULL)
#graph.numhd.emm.condition <- emmeans(graph.numhd.lmer.full.int.4, "Condition", lmer.df = "satterthwaite")

graph.numnodes.emm.acfieldgr
```

    ##  Demo.acfieldGrouped     emmean          SE     df   lower.CL   upper.CL
    ##  Formal sciences     0.05689999 0.004378037 390.34 0.04829251 0.06550748
    ##  Humanities          0.07052515 0.005057269 386.83 0.06058198 0.08046833
    ##  Life sciences       0.04212771 0.007321932 394.13 0.02773278 0.05652264
    ##  Other               0.04561205 0.005885088 381.08 0.03404074 0.05718336
    ##  Professional        0.05033165 0.002930579 413.04 0.04457094 0.05609236
    ##  Skipped             0.05526059 0.003068182 406.01 0.04922909 0.06129210
    ##  Social sciences     0.04958883 0.004838771 394.00 0.04007579 0.05910187
    ## 
    ## Results are averaged over the levels of: Condition, Dataset, UnderestDummy 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
graph.numnodes.emm.acfieldgr.df <- dplyr::as_data_frame(graph.numnodes.emm.acfieldgr)

graph.numnodes.emm.acfieldgr.df
```

    ## # A tibble: 7 x 6
    ##   Demo.acfieldGrouped emmean      SE    df lower.CL upper.CL
    ## * <fct>                <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 Formal sciences     0.0569 0.00438   390   0.0483   0.0655
    ## 2 Humanities          0.0705 0.00506   387   0.0606   0.0805
    ## 3 Life sciences       0.0421 0.00732   394   0.0277   0.0565
    ## 4 Other               0.0456 0.00589   381   0.0340   0.0572
    ## 5 Professional        0.0503 0.00293   413   0.0446   0.0561
    ## 6 Skipped             0.0553 0.00307   406   0.0492   0.0613
    ## 7 Social sciences     0.0496 0.00484   394   0.0401   0.0591

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numnodes.emm.data)

graph.numnodes.emm.acfieldgr.cld <- cld(graph.numnodes.emm.acfieldgr,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )
graph.numnodes.emm.acfieldgr.cld.df <- graph.numnodes.emm.acfieldgr.cld$emmeans

graph.numnodes.emm.acfieldgr.cld.df %>% dplyr::select(Demo.acfieldGrouped,.group)
```

    ##  Demo.acfieldGrouped .group
    ##  Life sciences        1    
    ##  Other                1    
    ##  Social sciences      1    
    ##  Professional         1    
    ##  Skipped              12   
    ##  Formal sciences      12   
    ##  Humanities            2   
    ## 
    ## Results are averaged over the levels of: Condition, Dataset, UnderestDummy 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 7 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.acfieldgr.cld.df
```

    ##  Demo.acfieldGrouped     emmean          SE     df   lower.CL   upper.CL
    ##  Life sciences       0.04212771 0.007321932 394.13 0.02773278 0.05652264
    ##  Other               0.04561205 0.005885088 381.08 0.03404074 0.05718336
    ##  Social sciences     0.04958883 0.004838771 394.00 0.04007579 0.05910187
    ##  Professional        0.05033165 0.002930579 413.04 0.04457094 0.05609236
    ##  Skipped             0.05526059 0.003068182 406.01 0.04922909 0.06129210
    ##  Formal sciences     0.05689999 0.004378037 390.34 0.04829251 0.06550748
    ##  Humanities          0.07052515 0.005057269 386.83 0.06058198 0.08046833
    ##  .group
    ##   1    
    ##   1    
    ##   1    
    ##   1    
    ##   12   
    ##   12   
    ##    2   
    ## 
    ## Results are averaged over the levels of: Condition, Dataset, UnderestDummy 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 7 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.acfieldgr.cld.df$Demo.acfieldGrouped <- 
  factor(graph.numnodes.emm.acfieldgr.cld.df$Demo.acfieldGrouped, 
         levels=graph.numnodes.emm.acfieldgr.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Demo.acfieldGrouped) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Demo.acfieldGrouped, CIs = TRUE)
emmip(graph.numnodes.emm.acfieldgr, ~Demo.acfieldGrouped, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-307-1.png)<!-- -->

``` r
plot(graph.numnodes.emm.acfieldgr)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-307-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.acfieldgr, comparisons = TRUE)

graph.numnodes.emm.acfieldgr.cld.df %>% arrange(desc(emmean))
```

    ##   Demo.acfieldGrouped     emmean          SE       df   lower.CL
    ## 1          Humanities 0.07052515 0.005057269 386.8282 0.06058198
    ## 2     Formal sciences 0.05689999 0.004378037 390.3359 0.04829251
    ## 3             Skipped 0.05526059 0.003068182 406.0121 0.04922909
    ## 4        Professional 0.05033165 0.002930579 413.0414 0.04457094
    ## 5     Social sciences 0.04958883 0.004838771 394.0034 0.04007579
    ## 6               Other 0.04561205 0.005885088 381.0827 0.03404074
    ## 7       Life sciences 0.04212771 0.007321932 394.1348 0.02773278
    ##     upper.CL .group
    ## 1 0.08046833      2
    ## 2 0.06550748     12
    ## 3 0.06129210     12
    ## 4 0.05609236     1 
    ## 5 0.05910187     1 
    ## 6 0.05718336     1 
    ## 7 0.05652264     1

``` r
ggplot(graph.numnodes.emm.acfieldgr.cld.df) +
  #geom_point(aes(x=Demo.acfieldGrouped,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.acfieldGrouped,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Demo.acfieldGrouped,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-307-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.acfieldgr.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Demo.acfieldGrouped,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Demo.acfieldGrouped,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip() +
labs(title="Estimated Marginal Means for Academic Field for Number of Nodes task,\ngraphics conditions",x="Academic Field")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-307-4.png)<!-- -->

``` r
#plot(ref_grid(graph.numnodes.lmer.int.3), by="Dataset") 
# try to figure this out? maybe only works on the interaction?
```

``` r
graph.numnodes.emm.acfieldgr.pairs <- dplyr::as_data_frame(pairs(graph.numnodes.emm.acfieldgr)) 
graph.numnodes.emm.acfieldgr.pairs
```

    ## # A tibble: 21 x 6
    ##    contrast                         estimate      SE    df t.ratio p.value
    ##  * <fct>                               <dbl>   <dbl> <dbl>   <dbl>   <dbl>
    ##  1 Formal sciences - Humanities     -0.0136  0.00663   380  -2.05  0.382  
    ##  2 Formal sciences - Life sciences   0.0148  0.00848   390   1.74  0.588  
    ##  3 Formal sciences - Other           0.0113  0.00730   379   1.55  0.717  
    ##  4 Formal sciences - Professional    0.00657 0.00520   380   1.26  0.868  
    ##  5 Formal sciences - Skipped         0.00164 0.00529   383   0.310 1.000  
    ##  6 Formal sciences - Social scienc…  0.00731 0.00647   383   1.13  0.919  
    ##  7 Humanities - Life sciences        0.0284  0.00885   386   3.21  0.0241 
    ##  8 Humanities - Other                0.0249  0.00772   379   3.23  0.0227 
    ##  9 Humanities - Professional         0.0202  0.00577   379   3.50  0.00936
    ## 10 Humanities - Skipped              0.0153  0.00584   380   2.62  0.124  
    ## # ... with 11 more rows

``` r
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

underest.pairs.CI <- confint(pairs(graph.numnodes.emm.acfieldgr))

graph.numnodes.emm.acfieldgr.pairs <- full_join(graph.numnodes.emm.acfieldgr.pairs, underest.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numnodes.emm.acfieldgr))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-308-1.png)<!-- -->

``` r
plot(pairs(graph.numnodes.emm.acfieldgr), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-308-2.png)<!-- -->

``` r
graph.numnodes.emm.acfieldgr.pairs$sig.levels <- 
  case_when(graph.numnodes.emm.acfieldgr.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numnodes.emm.acfieldgr.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numnodes.emm.acfieldgr.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numnodes.emm.acfieldgr.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.emm.acfieldgr.pairs$sig.levels <- factor(graph.numnodes.emm.acfieldgr.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)


graph.numnodes.emm.acfieldgr.pairs$contrast <- 
  factor(graph.numnodes.emm.acfieldgr.pairs$contrast, 
         levels=graph.numnodes.emm.acfieldgr.pairs %>% 
           arrange(desc(estimate)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())

#graph.numnodes.emm.acfieldgr.pairs <- graph.numnodes.emm.acfieldgr.pairs %>% separate(contrast, c("From", "del", "To"), sep="[-]", remove=FALSE) %>% dplyr::select(-del)
graph.numnodes.emm.acfieldgr.pairs <- graph.numnodes.emm.acfieldgr.pairs %>% separate(contrast, c("From", "To"), sep="[-]", remove=FALSE) %>% mutate(From=str_trim(From),To=str_trim(To))

graph.numnodes.emm.acfieldgr.pairs %>% arrange(estimate)
```

    ## # A tibble: 21 x 11
    ##    contrast   From   To    estimate      SE    df t.ratio p.value lower.CL
    ##    <fct>      <chr>  <chr>    <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ##  1 Formal sc… Forma… Huma… -1.36e⁻² 0.00663   380  -2.05    0.382  -0.0333
    ##  2 Life scie… Life … Skip… -1.31e⁻² 0.00791   391  -1.66    0.643  -0.0366
    ##  3 Other - S… Other  Skip… -9.65e⁻³ 0.00658   379  -1.47    0.764  -0.0291
    ##  4 Life scie… Life … Prof… -8.20e⁻³ 0.00785   391  -1.04    0.943  -0.0315
    ##  5 Life scie… Life … Soci… -7.46e⁻³ 0.00873   388  -0.855   0.979  -0.0333
    ##  6 Professio… Profe… Skip… -4.93e⁻³ 0.00414   381  -1.19    0.898  -0.0172
    ##  7 Other - P… Other  Prof… -4.72e⁻³ 0.00653   379  -0.723   0.991  -0.0241
    ##  8 Other - S… Other  Soci… -3.98e⁻³ 0.00757   379  -0.525   0.998  -0.0264
    ##  9 Life scie… Life … Other -3.48e⁻³ 0.00938   387  -0.371   1.000  -0.0313
    ## 10 Professio… Profe… Soci…  7.43e⁻⁴ 0.00556   380   0.134   1.000  -0.0157
    ## # ... with 11 more rows, and 2 more variables: upper.CL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.acfieldgr.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-308-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.acfieldgr.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-308-4.png)<!-- -->

``` r
underest.copy <- graph.numnodes.emm.acfieldgr.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numnodes.emm.acfieldgr.pairs.compl <- bind_rows(graph.numnodes.emm.acfieldgr.pairs,underest.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.cond.pairs.compl$From <- factor(graph.numnodes.emm.cond.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.cond.pairs.compl$To <- factor(graph.numnodes.emm.cond.pairs.compl$To, levels=cond.lev)

graph.numnodes.emm.acfieldgr.pairs.compl$From <- factor(graph.numnodes.emm.acfieldgr.pairs.compl$From, levels=rev(unique(graph.numnodes.emm.acfieldgr.pairs.compl$From)))

#graph.numnodes.emm.acfieldgr.pairs.compl %>% arrange(desc(estimate))
graph.numnodes.emm.acfieldgr.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 42 x 11
    ##    contrast   From   To    estimate      SE    df t.ratio p.value lower.CL
    ##    <fct>      <fct>  <chr>    <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ##  1 Humanitie… Life … Huma… -0.0284  0.00885   386   -3.21 0.0241  -2.17e⁻³
    ##  2 Humanitie… Other  Huma… -0.0249  0.00772   379   -3.23 0.0227  -2.04e⁻³
    ##  3 Humanitie… Socia… Huma… -0.0209  0.00692   379   -3.03 0.0418  -4.31e⁻⁴
    ##  4 Humanitie… Profe… Huma… -0.0202  0.00577   379   -3.50 0.00936 -3.09e⁻³
    ##  5 Humanitie… Skipp… Huma… -0.0153  0.00584   380   -2.62 0.124    2.03e⁻³
    ##  6 Formal sc… Life … Form… -0.0148  0.00848   390   -1.74 0.588    1.04e⁻²
    ##  7 Formal sc… Forma… Huma… -0.0136  0.00663   380   -2.05 0.382   -3.33e⁻²
    ##  8 Life scie… Life … Skip… -0.0131  0.00791   391   -1.66 0.643   -3.66e⁻²
    ##  9 Formal sc… Other  Form… -0.0113  0.00730   379   -1.55 0.717    1.04e⁻²
    ## 10 Other - S… Other  Skip… -0.00965 0.00658   379   -1.47 0.764   -2.91e⁻²
    ## # ... with 32 more rows, and 2 more variables: upper.CL <dbl>,
    ## #   sig.levels <ord>

``` r
ggplot(graph.numnodes.emm.acfieldgr.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-309-1.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.acfieldgr.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-309-2.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.acfieldgr.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-309-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.acfieldgr.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-309-4.png)<!-- -->

##### Condition:Dataset

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numnodes.emm.conddata <- emmeans(graph.numnodes.lmer.int.3, ~ Dataset | Condition, nesting = NULL)
graph.numnodes.emm.conddata.2 <- emmeans(graph.numnodes.lmer.int.3, ~ Condition | Dataset, nesting = NULL)

graph.numnodes.emm.conddata
```

    ## Condition = Ctrl:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.003930397 0.006145591 1125.63 -0.008127705 0.01598850
    ##  3       0.040123539 0.005976066 1120.14  0.028397995 0.05184908
    ##  5       0.078162535 0.006759697 1131.02  0.064899579 0.09142549
    ##  7       0.103339338 0.006453496 1133.69  0.090677201 0.11600148
    ##  8       0.112554726 0.006405502 1126.16  0.099986665 0.12512279
    ##  9       0.069225705 0.006670930 1134.41  0.056136959 0.08231445
    ## 
    ## Condition = Col:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.006256906 0.005781645 1126.36 -0.005087099 0.01760091
    ##  3       0.021556560 0.005791585 1127.28  0.010193060 0.03292006
    ##  5       0.055563135 0.006187592 1130.60  0.043422681 0.06770359
    ##  7       0.006619767 0.006067673 1127.41 -0.005285434 0.01852497
    ##  8       0.068109006 0.006112645 1132.57  0.056115624 0.08010239
    ##  9       0.056671840 0.006008557 1133.50  0.044882696 0.06846098
    ## 
    ## Condition = Phr:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.006744047 0.005749918 1119.62 -0.004537782 0.01802588
    ##  3       0.044673291 0.005763428 1127.43  0.033365041 0.05598154
    ##  5       0.097979701 0.006040222 1127.79  0.086128365 0.10983104
    ##  7       0.014410015 0.006223047 1128.40  0.002199971 0.02662006
    ##  8       0.081879671 0.005928545 1127.70  0.070247451 0.09351189
    ##  9       0.025217844 0.006348572 1129.79  0.012761528 0.03767416
    ## 
    ## Condition = Siz:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL
    ##  1       0.040338320 0.005819616 1119.53  0.028919736 0.05175690
    ##  3       0.060219401 0.005878188 1125.93  0.048685966 0.07175284
    ##  5       0.081015150 0.006016919 1126.11  0.069209517 0.09282078
    ##  7       0.062040962 0.006291669 1128.96  0.049696283 0.07438564
    ##  8       0.047531079 0.006066519 1124.42  0.035628107 0.05943405
    ##  9       0.085594688 0.006402995 1128.91  0.073031579 0.09815780
    ## 
    ## Results are averaged over the levels of: UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.conddata <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Condition, nesting = NULL)

#graph.numnodes.diffemm.conddata

graph.numnodes.diffemm.conddata.df <- dplyr::as_data_frame(graph.numnodes.emm.conddata)

#graph.numnodes.diffemm.conddata.df %>% View()

# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numnodes.emm.conddata.cld <- cld(graph.numnodes.emm.conddata,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )

graph.numnodes.emm.conddata.cld[["emmeans"]]
```

    ## Condition = Ctrl:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  1       0.003930397 0.006145591 1125.63 -0.008127705 0.01598850  1    
    ##  3       0.040123539 0.005976066 1120.14  0.028397995 0.05184908   2   
    ##  9       0.069225705 0.006670930 1134.41  0.056136959 0.08231445    3  
    ##  5       0.078162535 0.006759697 1131.02  0.064899579 0.09142549    3  
    ##  7       0.103339338 0.006453496 1133.69  0.090677201 0.11600148     4 
    ##  8       0.112554726 0.006405502 1126.16  0.099986665 0.12512279     4 
    ## 
    ## Condition = Col:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  1       0.006256906 0.005781645 1126.36 -0.005087099 0.01760091  1    
    ##  7       0.006619767 0.006067673 1127.41 -0.005285434 0.01852497  1    
    ##  3       0.021556560 0.005791585 1127.28  0.010193060 0.03292006  1    
    ##  5       0.055563135 0.006187592 1130.60  0.043422681 0.06770359   2   
    ##  9       0.056671840 0.006008557 1133.50  0.044882696 0.06846098   2   
    ##  8       0.068109006 0.006112645 1132.57  0.056115624 0.08010239   2   
    ## 
    ## Condition = Phr:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  1       0.006744047 0.005749918 1119.62 -0.004537782 0.01802588  1    
    ##  7       0.014410015 0.006223047 1128.40  0.002199971 0.02662006  1    
    ##  9       0.025217844 0.006348572 1129.79  0.012761528 0.03767416  12   
    ##  3       0.044673291 0.005763428 1127.43  0.033365041 0.05598154   2   
    ##  8       0.081879671 0.005928545 1127.70  0.070247451 0.09351189    3  
    ##  5       0.097979701 0.006040222 1127.79  0.086128365 0.10983104    3  
    ## 
    ## Condition = Siz:
    ##  Dataset      emmean          SE      df     lower.CL   upper.CL .group
    ##  1       0.040338320 0.005819616 1119.53  0.028919736 0.05175690  1    
    ##  8       0.047531079 0.006066519 1124.42  0.035628107 0.05943405  1    
    ##  3       0.060219401 0.005878188 1125.93  0.048685966 0.07175284  12   
    ##  7       0.062040962 0.006291669 1128.96  0.049696283 0.07438564  12   
    ##  5       0.081015150 0.006016919 1126.11  0.069209517 0.09282078   23  
    ##  9       0.085594688 0.006402995 1128.91  0.073031579 0.09815780    3  
    ## 
    ## Results are averaged over the levels of: UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.conddata.cld.df <- graph.numnodes.emm.conddata.cld[["emmeans"]]

graph.numnodes.emm.conddata.cld.df %>% dplyr::select(Dataset,Condition,.group) %>% print()
```

    ## Condition = Ctrl:
    ##  Dataset .group
    ##  1        1    
    ##  3         2   
    ##  9          3  
    ##  5          3  
    ##  7           4 
    ##  8           4 
    ## 
    ## Condition = Col:
    ##  Dataset .group
    ##  1        1    
    ##  7        1    
    ##  3        1    
    ##  5         2   
    ##  9         2   
    ##  8         2   
    ## 
    ## Condition = Phr:
    ##  Dataset .group
    ##  1        1    
    ##  7        1    
    ##  9        12   
    ##  3         2   
    ##  8          3  
    ##  5          3  
    ## 
    ## Condition = Siz:
    ##  Dataset .group
    ##  1        1    
    ##  8        1    
    ##  3        12   
    ##  7        12   
    ##  5         23  
    ##  9          3  
    ## 
    ## Results are averaged over the levels of: UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 6 estimates 
    ## significance level used: alpha = 0.05

``` r
cld(graph.numnodes.emm.conddata.2, details=TRUE, sort=TRUE)[["emmeans"]] %>% dplyr::select(Dataset,Condition,.group) %>% print()
```

    ## Dataset = 1:
    ##  Condition .group
    ##  Ctrl       1    
    ##  Col        1    
    ##  Phr        1    
    ##  Siz         2   
    ## 
    ## Dataset = 3:
    ##  Condition .group
    ##  Col        1    
    ##  Ctrl       12   
    ##  Phr         2   
    ##  Siz         2   
    ## 
    ## Dataset = 5:
    ##  Condition .group
    ##  Col        1    
    ##  Ctrl       12   
    ##  Siz         2   
    ##  Phr         2   
    ## 
    ## Dataset = 7:
    ##  Condition .group
    ##  Col        1    
    ##  Phr        1    
    ##  Siz         2   
    ##  Ctrl         3  
    ## 
    ## Dataset = 8:
    ##  Condition .group
    ##  Siz        1    
    ##  Col        12   
    ##  Phr         2   
    ##  Ctrl         3  
    ## 
    ## Dataset = 9:
    ##  Condition .group
    ##  Phr        1    
    ##  Col         2   
    ##  Ctrl        23  
    ##  Siz          3  
    ## 
    ## Results are averaged over the levels of: UnderestDummy, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.conddata.cld.df$Condition <- factor(graph.numnodes.emm.conddata.cld.df$Condition, levels=graph.numnodes.emm.conddata.cld %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Condition, CIs = TRUE)
emmip(graph.numnodes.emm.conddata, ~Condition|Dataset, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-310-1.png)<!-- -->

``` r
plot(graph.numnodes.emm.conddata)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-310-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.conddata, comparisons = TRUE)

graph.numnodes.emm.conddata.cld.df %>% arrange(desc(emmean))
```

    ##    Dataset Condition      emmean          SE       df     lower.CL
    ## 1        8      Ctrl 0.112554726 0.006405502 1126.158  0.099986665
    ## 2        7      Ctrl 0.103339338 0.006453496 1133.693  0.090677201
    ## 3        5       Phr 0.097979701 0.006040222 1127.794  0.086128365
    ## 4        9       Siz 0.085594688 0.006402995 1128.912  0.073031579
    ## 5        8       Phr 0.081879671 0.005928545 1127.699  0.070247451
    ## 6        5       Siz 0.081015150 0.006016919 1126.108  0.069209517
    ## 7        5      Ctrl 0.078162535 0.006759697 1131.015  0.064899579
    ## 8        9      Ctrl 0.069225705 0.006670930 1134.413  0.056136959
    ## 9        8       Col 0.068109006 0.006112645 1132.567  0.056115624
    ## 10       7       Siz 0.062040962 0.006291669 1128.962  0.049696283
    ## 11       3       Siz 0.060219401 0.005878188 1125.930  0.048685966
    ## 12       9       Col 0.056671840 0.006008557 1133.503  0.044882696
    ## 13       5       Col 0.055563135 0.006187592 1130.597  0.043422681
    ## 14       8       Siz 0.047531079 0.006066519 1124.421  0.035628107
    ## 15       3       Phr 0.044673291 0.005763428 1127.428  0.033365041
    ## 16       1       Siz 0.040338320 0.005819616 1119.534  0.028919736
    ## 17       3      Ctrl 0.040123539 0.005976066 1120.139  0.028397995
    ## 18       9       Phr 0.025217844 0.006348572 1129.791  0.012761528
    ## 19       3       Col 0.021556560 0.005791585 1127.280  0.010193060
    ## 20       7       Phr 0.014410015 0.006223047 1128.400  0.002199971
    ## 21       1       Phr 0.006744047 0.005749918 1119.624 -0.004537782
    ## 22       7       Col 0.006619767 0.006067673 1127.410 -0.005285434
    ## 23       1       Col 0.006256906 0.005781645 1126.361 -0.005087099
    ## 24       1      Ctrl 0.003930397 0.006145591 1125.630 -0.008127705
    ##      upper.CL .group
    ## 1  0.12512279      4
    ## 2  0.11600148      4
    ## 3  0.10983104      3
    ## 4  0.09815780      3
    ## 5  0.09351189      3
    ## 6  0.09282078     23
    ## 7  0.09142549     3 
    ## 8  0.08231445     3 
    ## 9  0.08010239      2
    ## 10 0.07438564    12 
    ## 11 0.07175284    12 
    ## 12 0.06846098      2
    ## 13 0.06770359      2
    ## 14 0.05943405    1  
    ## 15 0.05598154     2 
    ## 16 0.05175690    1  
    ## 17 0.05184908    2  
    ## 18 0.03767416    12 
    ## 19 0.03292006     1 
    ## 20 0.02662006    1  
    ## 21 0.01802588    1  
    ## 22 0.01852497     1 
    ## 23 0.01760091     1 
    ## 24 0.01598850   1

``` r
ggplot(graph.numnodes.emm.conddata.cld.df) +
  #geom_point(aes(x=Condition,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Dataset) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-310-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.cld.df) +
  geom_errorbar(aes(x=Dataset,ymax=upper.CL,ymin=lower.CL, color=Condition), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=Dataset,y=emmean, color=Condition), position=position_dodge(width=0.2)) +
  geom_line(aes(x=Dataset,y=emmean, color=Condition, group=Condition), position=position_dodge(width=0.2)) +
  #scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  scale_color_discrete(labels=c("Control","Color","Phrasing","Size")) +
  labs(title="Estimated Marginal Means for Condition vs. Dataset for\nNumber of Nodes task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-310-4.png)<!-- -->

``` r
graph.numnodes.emm.conddata.pairs <- dplyr::as_data_frame(pairs(graph.numnodes.emm.conddata)) 
#graph.numnodes.emm.conddata.pairs %>% View()
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

conddata.pairs.CI <- confint(pairs(graph.numnodes.emm.conddata))

graph.numnodes.emm.conddata.pairs <- full_join(graph.numnodes.emm.conddata.pairs, conddata.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numnodes.emm.conddata))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-311-1.png)<!-- -->

``` r
plot(pairs(graph.numnodes.emm.conddata), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-311-2.png)<!-- -->

``` r
graph.numnodes.emm.conddata.pairs$sig.levels <- 
  case_when(graph.numnodes.emm.conddata.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numnodes.emm.conddata.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numnodes.emm.conddata.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numnodes.emm.conddata.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.emm.conddata.pairs$sig.levels <- factor(graph.numnodes.emm.conddata.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numnodes.emm.conddata.pairs$contrast.cond <- paste0(graph.numnodes.emm.conddata.pairs$contrast,graph.numnodes.emm.conddata.pairs$Condition)

graph.numnodes.emm.conddata.pairs$contrast.cond <- factor(graph.numnodes.emm.conddata.pairs$contrast.cond, levels=graph.numnodes.emm.conddata.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.cond) %>% distinct() %>% unlist())

#graph.numnodes.emm.conddata.pairs %>% View()

graph.numnodes.emm.conddata.pairs$contrast <- 
  factor(as.character(graph.numnodes.emm.conddata.pairs$contrast), 
         levels=graph.numnodes.emm.conddata.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numnodes.emm.conddata.pairs <- graph.numnodes.emm.conddata.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numnodes.emm.conddata.pairs %>% arrange(estimate)
```

    ## # A tibble: 60 x 13
    ##    contrast From  To    Condition estimate      SE    df t.ratio   p.value
    ##    <fct>    <chr> <chr> <fct>        <dbl>   <dbl> <dbl>   <dbl>     <dbl>
    ##  1 1 - 8    1     8     Ctrl       -0.109  0.00813   921  -13.4   0       
    ##  2 1 - 7    1     7     Ctrl       -0.0994 0.00825   922  -12.1   2.47e⁻¹³
    ##  3 1 - 5    1     5     Phr        -0.0912 0.00797   993  -11.5   0       
    ##  4 1 - 8    1     8     Phr        -0.0751 0.00788   995  - 9.53  0       
    ##  5 1 - 5    1     5     Ctrl       -0.0742 0.00838   920  - 8.86  0       
    ##  6 3 - 8    3     8     Ctrl       -0.0724 0.00804   960  - 9.01  0       
    ##  7 7 - 8    7     8     Phr        -0.0675 0.00744   929  - 9.07  0       
    ##  8 1 - 9    1     9     Ctrl       -0.0653 0.00844   947  - 7.74  9.89e⁻¹³
    ##  9 3 - 7    3     7     Ctrl       -0.0632 0.00807   927  - 7.84  2.29e⁻¹³
    ## 10 1 - 8    1     8     Col        -0.0619 0.00799   953  - 7.75  6.86e⁻¹³
    ## # ... with 50 more rows, and 4 more variables: lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
#graph.numnodes.emm.conddata.pairs %>% View()

ggplot(graph.numnodes.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-311-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast.cond,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.cond,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-311-4.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-311-5.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~Condition, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-311-6.png)<!-- -->

``` r
copy <- graph.numnodes.emm.conddata.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numnodes.emm.conddata.pairs.compl <- bind_rows(graph.numnodes.emm.conddata.pairs,copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=cond.lev)
#graph.numnodes.emm.conddata.pairs.compl$To <- factor(graph.numnodes.emm.conddata.pairs.compl$To, levels=cond.lev)

graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numnodes.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numnodes.emm.conddata.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 120 x 13
    ##    contrast From  To    Condition estimate      SE    df t.ratio   p.value
    ##    <fct>    <fct> <chr> <fct>        <dbl>   <dbl> <dbl>   <dbl>     <dbl>
    ##  1 1 - 8    1     8     Ctrl       -0.109  0.00813   921  -13.4   0       
    ##  2 1 - 7    1     7     Ctrl       -0.0994 0.00825   922  -12.1   2.47e⁻¹³
    ##  3 1 - 5    1     5     Phr        -0.0912 0.00797   993  -11.5   0       
    ##  4 5 - 7    7     5     Phr        -0.0836 0.00748   946  -11.2   2.93e⁻¹³
    ##  5 1 - 8    1     8     Phr        -0.0751 0.00788   995  - 9.53  0       
    ##  6 1 - 5    1     5     Ctrl       -0.0742 0.00838   920  - 8.86  0       
    ##  7 5 - 9    9     5     Phr        -0.0728 0.00743   907  - 9.80  9.00e⁻¹⁴
    ##  8 3 - 8    3     8     Ctrl       -0.0724 0.00804   960  - 9.01  0       
    ##  9 7 - 8    7     8     Phr        -0.0675 0.00744   929  - 9.07  0       
    ## 10 1 - 9    1     9     Ctrl       -0.0653 0.00844   947  - 7.74  9.89e⁻¹³
    ## # ... with 110 more rows, and 4 more variables: lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.cond <fct>

``` r
ggplot(graph.numnodes.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-312-1.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-312-2.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-312-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.conddata.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~Condition)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-312-4.png)<!-- -->

##### Condition:UnderestDummy

###### emmeans

``` r
# trying to use emmeans package for lsmeans table

# can use ref_grid to see if emmeans is finding nestings within variables, which can cause problems(?)
# ref_grid(graph.numhd.lmer.full.int.4)
# unfortunately, considers Overestimated nested in Dataset because I didn't include 
# Overestimated as a fixed effect; can use nesting = NULL to ignore this auto-detection of nesting

# also want to check if any combination of two factors has zeroes in the cells
# with(graphics_numnodes, table(Dataset,Underestimated))
# ref_grid(graph.numnodes.lmer.int.5) @ grid; .wgt. is number of observations

# See also: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#graph.numnodes.emm.cond <- emmeans(graph.numnodes.lmer.int.3, "Condition", nesting = NULL)
graph.numnodes.emm.condunder <- emmeans(graph.numnodes.lmer.int.3, ~ Condition | UnderestDummy, nesting = NULL)

graph.numnodes.emm.condunder
```

    ## UnderestDummy = 1-Underestimated:
    ##  Condition     emmean          SE     df   lower.CL   upper.CL
    ##  Ctrl      0.07648513 0.003789370 503.61 0.06904021 0.08393005
    ##  Col       0.03512098 0.003619202 527.67 0.02801117 0.04223080
    ##  Phr       0.03976695 0.003620592 521.43 0.03265421 0.04687969
    ##  Siz       0.07938476 0.003684270 505.66 0.07214640 0.08662312
    ## 
    ## UnderestDummy = 2-CorrectOrOver:
    ##  Condition     emmean          SE     df   lower.CL   upper.CL
    ##  Ctrl      0.05929362 0.005610855 956.19 0.04828261 0.07030463
    ##  Col       0.03647142 0.005281542 956.96 0.02610668 0.04683616
    ##  Phr       0.05053457 0.005329262 909.15 0.04007548 0.06099366
    ##  Siz       0.04619511 0.005319088 898.48 0.03575582 0.05663439
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#graph.numnodes.diffemm.dataunder <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Dataset | Underestimated, nesting = NULL)

#graph.numnodes.diffemm.dataunder

graph.numnodes.emm.condunder.df <- dplyr::as_data_frame(graph.numnodes.emm.condunder)

graph.numnodes.emm.condunder.df
```

    ## # A tibble: 8 x 7
    ##   Condition UnderestDummy    emmean      SE    df lower.CL upper.CL
    ## * <fct>     <fct>             <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ## 1 Ctrl      1-Underestimated 0.0765 0.00379   504   0.0690   0.0839
    ## 2 Col       1-Underestimated 0.0351 0.00362   528   0.0280   0.0422
    ## 3 Phr       1-Underestimated 0.0398 0.00362   521   0.0327   0.0469
    ## 4 Siz       1-Underestimated 0.0794 0.00368   506   0.0721   0.0866
    ## 5 Ctrl      2-CorrectOrOver  0.0593 0.00561   956   0.0483   0.0703
    ## 6 Col       2-CorrectOrOver  0.0365 0.00528   957   0.0261   0.0468
    ## 7 Phr       2-CorrectOrOver  0.0505 0.00533   909   0.0401   0.0610
    ## 8 Siz       2-CorrectOrOver  0.0462 0.00532   898   0.0358   0.0566

``` r
# From: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant, based on the adjust setting (which defaults to "tukey"). (Note: Don’t ever use confidence intervals for EMMs to perform comparisons; they can be very misleading.)

# TO DO : can't figure out how to extract data about the arrows in order to reproduce the graph

#xtable::xtable(graph.numhd.emm.condition)

graph.numnodes.emm.condunder.cld <- cld(graph.numnodes.emm.condunder,
           details=TRUE,
           #alpha=0.01,
           #by="Dataset",
           #Letters="|||||||||||||||||||",
           sort=TRUE
           )

graph.numnodes.emm.condunder.cld[["emmeans"]]
```

    ## UnderestDummy = 1-Underestimated:
    ##  Condition     emmean          SE     df   lower.CL   upper.CL .group
    ##  Col       0.03512098 0.003619202 527.67 0.02801117 0.04223080  1    
    ##  Phr       0.03976695 0.003620592 521.43 0.03265421 0.04687969  1    
    ##  Ctrl      0.07648513 0.003789370 503.61 0.06904021 0.08393005   2   
    ##  Siz       0.07938476 0.003684270 505.66 0.07214640 0.08662312   2   
    ## 
    ## UnderestDummy = 2-CorrectOrOver:
    ##  Condition     emmean          SE     df   lower.CL   upper.CL .group
    ##  Col       0.03647142 0.005281542 956.96 0.02610668 0.04683616  1    
    ##  Siz       0.04619511 0.005319088 898.48 0.03575582 0.05663439  12   
    ##  Phr       0.05053457 0.005329262 909.15 0.04007548 0.06099366  12   
    ##  Ctrl      0.05929362 0.005610855 956.19 0.04828261 0.07030463   2   
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
graph.numnodes.emm.condunder.cld.df <- graph.numnodes.emm.condunder.cld[["emmeans"]]

graph.numnodes.emm.condunder.cld.df %>% dplyr::select(Condition,UnderestDummy,.group) %>% print()
```

    ## UnderestDummy = 1-Underestimated:
    ##  Condition .group
    ##  Col        1    
    ##  Phr        1    
    ##  Ctrl        2   
    ##  Siz         2   
    ## 
    ## UnderestDummy = 2-CorrectOrOver:
    ##  Condition .group
    ##  Col        1    
    ##  Siz        12   
    ##  Phr        12   
    ##  Ctrl        2   
    ## 
    ## Results are averaged over the levels of: Dataset, Demo.acfieldGrouped 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## significance level used: alpha = 0.05

``` r
# TO DO : get this to print as single table

#graph.numnodes.emm.dataunder.cld.df$Underestimated <- factor(graph.numnodes.emm.dataunder.cld.df$Underestimated, levels=graph.numnodes.emm.dataunder.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Underestimated) %>% unlist())

#graph.numnodes.emm.condunder.cld.df$Condition <- factor(graph.numnodes.emm.condunder.cld.df$Condition, levels=graph.numnodes.emm.condunder.cld.df %>% arrange(desc(emmean)) %>% dplyr::select(Condition) %>% unlist())

#emmip(graph.numnodes.lmer.int.3, ~Dataset, CIs = TRUE)
emmip(graph.numnodes.emm.condunder, ~Condition|UnderestDummy, CIs = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-313-1.png)<!-- -->

``` r
plot(graph.numnodes.emm.condunder)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-313-2.png)<!-- -->

``` r
#plot(graph.numnodes.emm.condunder, comparisons = TRUE)

graph.numnodes.emm.condunder.cld.df %>% arrange(desc(emmean))
```

    ##   Condition    UnderestDummy     emmean          SE       df   lower.CL
    ## 1       Siz 1-Underestimated 0.07938476 0.003684270 505.6590 0.07214640
    ## 2      Ctrl 1-Underestimated 0.07648513 0.003789370 503.6088 0.06904021
    ## 3      Ctrl  2-CorrectOrOver 0.05929362 0.005610855 956.1883 0.04828261
    ## 4       Phr  2-CorrectOrOver 0.05053457 0.005329262 909.1492 0.04007548
    ## 5       Siz  2-CorrectOrOver 0.04619511 0.005319088 898.4827 0.03575582
    ## 6       Phr 1-Underestimated 0.03976695 0.003620592 521.4261 0.03265421
    ## 7       Col  2-CorrectOrOver 0.03647142 0.005281542 956.9604 0.02610668
    ## 8       Col 1-Underestimated 0.03512098 0.003619202 527.6680 0.02801117
    ##     upper.CL .group
    ## 1 0.08662312      2
    ## 2 0.08393005      2
    ## 3 0.07030463      2
    ## 4 0.06099366     12
    ## 5 0.05663439     12
    ## 6 0.04687969     1 
    ## 7 0.04683616     1 
    ## 8 0.04223080     1

``` r
ggplot(graph.numnodes.emm.condunder.cld.df) +
  #geom_point(aes(x=Underestimated,y=emmean), shape=21, size=7) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL), width=.2) +
  geom_point(aes(x=Condition,y=emmean), size=7) +
  #scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~UnderestDummy) + 
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-313-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.cld.df) +
  geom_errorbar(aes(x=Condition,ymax=upper.CL,ymin=lower.CL, color=UnderestDummy), width=.2, position=position_dodge(width=0.2)) +
  geom_point(aes(x=Condition,y=emmean, color=UnderestDummy), position=position_dodge(width=0.2)) +
  #geom_line(aes(x=Condition,y=emmean, color=UnderestDummy, group=UnderestDummy), position=position_dodge(width=0.2)) +
  #scale_color_manual(labels=c("Blue","Black"),values=c("steelblue","black"), name="Node Color") +
  scale_color_discrete(labels=c("Underestimated","Correct or\nOverestimated"), name="Underestimated") +
  labs(title="Estimated Marginal Means for Condition vs. Underestimated for\nNumber of Nodes task, graphics conditions")
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-313-4.png)<!-- -->

``` r
graph.numnodes.emm.condunder.pairs <- dplyr::as_data_frame(pairs(graph.numnodes.emm.condunder)) 
#graph.numnodes.emm.condunder.pairs %>% View()
# for some reason, full.cld$comparisons returns estimates that are all positive; 
# pairs(graph.numhd.emm.condition) has both negative and positive estimates
# and joins better to confint()

condunder.pairs.CI <- confint(pairs(graph.numnodes.emm.condunder))

graph.numnodes.emm.condunder.pairs <- full_join(graph.numnodes.emm.condunder.pairs, condunder.pairs.CI)

#graph.numnodes.diffemm.cond <- emmeans(graph.numnodes.lmer.int.3, pairwise ~ Condition)
#graph.numnodes.diffemm.cond$contrasts
#contrast(graph.numnodes.emm.cond)
#confint(graph.numnodes.emm.cond)
#pairs(graph.numnodes.emm.cond, details=TRUE)
#confint(contrast(graph.numnodes.emm.cond))
#confint(pairs(graph.numnodes.emm.cond))
#coef(pairs(graph.numnodes.emm.cond))

plot(pairs(graph.numnodes.emm.condunder))
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-314-1.png)<!-- -->

``` r
plot(pairs(graph.numnodes.emm.condunder), comparisons = TRUE)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-314-2.png)<!-- -->

``` r
graph.numnodes.emm.condunder.pairs$sig.levels <- 
  case_when(graph.numnodes.emm.condunder.pairs$p.value < .0001 ~ sig.level.names[1],
            graph.numnodes.emm.condunder.pairs$p.value < .001 ~ sig.level.names[2],
            graph.numnodes.emm.condunder.pairs$p.value < .01 ~ sig.level.names[3],
            graph.numnodes.emm.condunder.pairs$p.value < .05 ~ sig.level.names[4],
            TRUE ~ sig.level.names[5])

graph.numnodes.emm.condunder.pairs$sig.levels <- factor(graph.numnodes.emm.condunder.pairs$sig.levels, levels=sig.level.names,ordered=TRUE)

graph.numnodes.emm.condunder.pairs$contrast.all <- paste0(graph.numnodes.emm.condunder.pairs$contrast,graph.numnodes.emm.condunder.pairs$UnderestDummy)

graph.numnodes.emm.condunder.pairs$contrast.all <- factor(graph.numnodes.emm.condunder.pairs$contrast.all, levels=graph.numnodes.emm.condunder.pairs %>% arrange(desc(estimate)) %>% dplyr::select(contrast.all) %>% distinct() %>% unlist())

#graph.numnodes.emm.conddata.pairs %>% View()

graph.numnodes.emm.condunder.pairs$contrast <- 
  factor(as.character(graph.numnodes.emm.condunder.pairs$contrast), 
         levels=graph.numnodes.emm.condunder.pairs %>% group_by(contrast) %>% summarise(avgEst = mean(estimate)) %>% arrange(desc(avgEst)) %>% dplyr::select(contrast) %>% distinct() %>% unlist())


graph.numnodes.emm.condunder.pairs <- graph.numnodes.emm.condunder.pairs %>% separate(contrast, c("From", "del", "To"), sep="[ ]", remove=FALSE) %>% dplyr::select(-del)

graph.numnodes.emm.condunder.pairs %>% arrange(estimate)
```

    ## # A tibble: 12 x 13
    ##    contrast   From  To    UnderestDummy    estimate      SE    df t.ratio
    ##    <fct>      <chr> <chr> <fct>               <dbl>   <dbl> <dbl>   <dbl>
    ##  1 Col - Siz  Col   Siz   1-Underestimated -0.0443  0.00489   524  -9.05 
    ##  2 Phr - Siz  Phr   Siz   1-Underestimated -0.0396  0.00496   526  -7.99 
    ##  3 Col - Phr  Col   Phr   2-CorrectOrOver  -0.0141  0.00740   958  -1.90 
    ##  4 Col - Siz  Col   Siz   2-CorrectOrOver  -0.00972 0.00739   964  -1.32 
    ##  5 Col - Phr  Col   Phr   1-Underestimated -0.00465 0.00492   534  -0.943
    ##  6 Ctrl - Siz Ctrl  Siz   1-Underestimated -0.00290 0.00505   515  -0.575
    ##  7 Phr - Siz  Phr   Siz   2-CorrectOrOver   0.00434 0.00740   931   0.586
    ##  8 Ctrl - Phr Ctrl  Phr   2-CorrectOrOver   0.00876 0.00758   957   1.16 
    ##  9 Ctrl - Siz Ctrl  Siz   2-CorrectOrOver   0.0131  0.00757   955   1.73 
    ## 10 Ctrl - Col Ctrl  Col   2-CorrectOrOver   0.0228  0.00760   991   3.00 
    ## 11 Ctrl - Phr Ctrl  Phr   1-Underestimated  0.0367  0.00505   526   7.27 
    ## 12 Ctrl - Col Ctrl  Col   1-Underestimated  0.0414  0.00499   523   8.29 
    ## # ... with 5 more variables: p.value <dbl>, lower.CL <dbl>,
    ## #   upper.CL <dbl>, sig.levels <ord>, contrast.all <fct>

``` r
#graph.numnodes.emm.condunder.pairs %>% View()

ggplot(graph.numnodes.emm.condunder.pairs) +
  geom_errorbar(aes(x=contrast.all,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.all,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~UnderestDummy, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-314-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.pairs) +
  geom_errorbar(aes(x=contrast.all,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast.all,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~UnderestDummy, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-314-4.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~UnderestDummy, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-314-5.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.pairs) +
  geom_errorbar(aes(x=contrast,ymax=upper.CL,ymin=lower.CL), width=.5) +
  geom_point(aes(x=contrast,y=estimate, fill=sig.levels), shape=21, size=7) +
  geom_hline(aes(yintercept=0)) +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  facet_wrap(~UnderestDummy, scales="free_y") +
  coord_flip()
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-314-6.png)<!-- -->

``` r
condunder.copy <- graph.numnodes.emm.condunder.pairs %>% rename(From=To,To=From) %>% 
  mutate(estimate=-estimate,
         t.ratio=-t.ratio,
         lower.CL=-lower.CL,
         upper.CL=-upper.CL)

graph.numnodes.emm.condunder.pairs.compl <- bind_rows(graph.numnodes.emm.condunder.pairs,condunder.copy)


#cond.lev <- c("Ctrl","Phr","Col","Siz")

graph.numnodes.emm.condunder.pairs.compl$From <- factor(graph.numnodes.emm.condunder.pairs.compl$From, levels=rev(cond.lev))
graph.numnodes.emm.condunder.pairs.compl$To <- factor(graph.numnodes.emm.condunder.pairs.compl$To, levels=cond.lev)

#graph.numnodes.emm.conddata.pairs.compl$From <- factor(graph.numnodes.emm.conddata.pairs.compl$From, levels=c(9,8,7,5,3,1))


#graph.numnodes.emm.conddata.pairs.compl %>% arrange(desc(estimate))
graph.numnodes.emm.condunder.pairs.compl %>% arrange(estimate)
```

    ## # A tibble: 24 x 13
    ##    contrast   From  To    UnderestDummy    estimate      SE    df t.ratio
    ##    <fct>      <fct> <fct> <fct>               <dbl>   <dbl> <dbl>   <dbl>
    ##  1 Col - Siz  Col   Siz   1-Underestimated -0.0443  0.00489   524  -9.05 
    ##  2 Ctrl - Col Col   Ctrl  1-Underestimated -0.0414  0.00499   523  -8.29 
    ##  3 Phr - Siz  Phr   Siz   1-Underestimated -0.0396  0.00496   526  -7.99 
    ##  4 Ctrl - Phr Phr   Ctrl  1-Underestimated -0.0367  0.00505   526  -7.27 
    ##  5 Ctrl - Col Col   Ctrl  2-CorrectOrOver  -0.0228  0.00760   991  -3.00 
    ##  6 Col - Phr  Col   Phr   2-CorrectOrOver  -0.0141  0.00740   958  -1.90 
    ##  7 Ctrl - Siz Siz   Ctrl  2-CorrectOrOver  -0.0131  0.00757   955  -1.73 
    ##  8 Col - Siz  Col   Siz   2-CorrectOrOver  -0.00972 0.00739   964  -1.32 
    ##  9 Ctrl - Phr Phr   Ctrl  2-CorrectOrOver  -0.00876 0.00758   957  -1.16 
    ## 10 Col - Phr  Col   Phr   1-Underestimated -0.00465 0.00492   534  -0.943
    ## # ... with 14 more rows, and 5 more variables: p.value <dbl>,
    ## #   lower.CL <dbl>, upper.CL <dbl>, sig.levels <ord>, contrast.all <fct>

``` r
ggplot(graph.numnodes.emm.condunder.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=sig.levels), color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~UnderestDummy)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-315-1.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=sig.levels), shape=21, color="black") +
  scale_fill_manual("Significance Levels", values=sig.colors) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~UnderestDummy)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-315-2.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.pairs.compl) +
  geom_tile(aes(x=To,y=From,fill=estimate)) +
    scale_fill_distiller(type="div", palette=4) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~UnderestDummy)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-315-3.png)<!-- -->

``` r
ggplot(graph.numnodes.emm.condunder.pairs.compl) +
  geom_count(aes(x=To,y=From,size=abs(estimate),fill=estimate, color=p.value<.01), shape=21) +
  scale_fill_distiller(type="div", palette=4) +
  scale_color_manual(values=c("grey90","black")) +
  scale_x_discrete(drop=FALSE, position = "top") +
  scale_y_discrete(drop=FALSE, name="From") +
  facet_wrap(~UnderestDummy)
```

![](Graphic-Models-Main_files/figure-gfm/unnamed-chunk-315-4.png)<!-- -->
