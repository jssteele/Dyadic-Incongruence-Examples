#'---
#'title: "Dyadic Incongruence in R using OLS, HLM, and SEM"
#'author: "Joel S Steele"
#'date: ""
#'---
#'
#+echo=F
library(knitr)
opts_chunk$set(comment=NA, echo=T)

#'## Data preparation
#' In order to demonstrate the outcome of a dyadic incongruence approach we begin by simulating some data. It is always best to check your work when you know the answer ahead of time.
#' Here we simulate $50$ observations from two groups, g1 and g2, that will later serve as our dyadic partners.

# set the same seed to get the same results. A good step in any simulation.
set.seed(42) # why not
# create a data frame with columns 'grp' and 'out' for group and outcome respectively.
ex.df = data.frame(
    'grp'=factor(
        c(rep('g1',25),
          rep('g2',25))),
    'out'=c(
        rnorm(25,mean=10,sd=15),
        rnorm(25,mean=25,sd=7)))
# quick peek
head(ex.df)
#'## Packages needed
#' For the following demonstration we will be using a number of packages in R. Namely, __psych__, __nlme__, __lavaan__, and __emmeans__. Let's load them into our session.
#' If these are not installed already run the following,
#'```
#'install.packages(c("psych","lavaan","emmeans"))
#'```
#' Note that __nlme__ should be installed as part of R by default.
# load the libraries
library(psych)
library(nlme)
library(lavaan)
library(emmeans)
#' #### Quick description to check our simulation
# entire sample
describe(ex.df$out,skew=F)
# per group statistics
describeBy(ex.df$out,group=ex.df$grp,skew=F)
# balanced data here
mean(ex.df$out) # grand mean on outcome
with(ex.df, aggregate(out~grp,FUN=mean)) # group means on outcome
# group means differences from grand mean on outcome
with(ex.df, aggregate(out~grp,FUN=mean))[,"out"]-mean(ex.df$out)
# difference in group means on outcome
diff(with(ex.df, aggregate(out~grp,FUN=mean))[,"out"])

#' ### OLS
#' Let's start by examining the estimates under Treatment coding (default). 
#' This equates to difference between group1 (intercept) and group 2 means.
summary(lm(out~grp,ex.df))

#' Next we can see the difference using effect coding. Here the (intercept) is now grand mean (see above)
#' and the group coefficient is group difference from grand mean
summary(lm(out~grp,ex.df,contrasts=list('grp'=contr.sum))->lm1)

#' In order to understand a bit more let's have a look at the coding
contr.sum(2) # scaled as 1,-1
#' Below we creat a new contrast, rescaled for dyadic incongruence
newcontr = contr.sum(2)*.5
newcontr # see the difference
#' This new contrast will scale our effects by a factor of 2!
summary(lm(out~grp,ex.df,contrasts=list('grp'=newcontr))->lm2)
#' ## Mixed-Effects or HLM
#' Next we will extend this idea to include dyadic nesting through HLM. In order to do so we need to add in a dyadic indicator. 
# copy the data to a new data frame
ml.df = ex.df
# add in dyad id for nesting
ml.df$did = rep(1:25,times=2)
# quick peek
head(ml.df[order(ml.df$did),])
# dyad indicator set to 1,-1 via contr.sum
lme1 = lme(out~grp,data=ml.df,
           random=~grp|did,
           contrasts=list('grp'=contr.sum))
summary(lme1)
# dyadic incongruence contrast
lme2 = lme(out~grp,data=ml.df,
    random=~grp|did,
    contrasts=list('grp'=newcontr))
summary(lme2)
#' ### Examining estimated marginal means
#' ##### OLS models
emmeans(lm1,list(pairwise~grp))# same P(>|z|)
emmeans(lm2,list(pairwise~grp))# same
#' ##### LME models
emmeans(lme1,list(pairwise~grp))# same
emmeans(lme2,list(pairwise~grp))# yawn...same



#' ## Structural Equation Modeling
#' To begin we include some hard coded contrast coefficients in order to estimate the incongruence between partners.

# group coding
lv.df = ex.df
# hard code the contrast!
lv.df$gi = ifelse(lv.df$grp=='g1',.5,-.5)
# quick peek
head(lv.df)

eff_code_mod = '
out ~ a*gi
out ~ 1
out~~out
incong := .5*a #effect code estimate
'
fit = lavaan(eff_code_mod,data=lv.df,effect.coding=c('intercepts'))
summary(fit)

# just to prove it we will hard code an effect coding contrast
lv.df = ex.df
lv.df$gi2 = ifelse(lv.df$grp=='g1',1,-1)
# quick peek
head(lv.df)

eff_code_mod2 = '
out ~ a*gi2
out ~ 1
out~~out
'
fit2 = lavaan(eff_code_mod2,data=lv.df,effect.coding=c('intercepts'))
summary(fit2)

#' Below we code the loadings from our dyadic outcomes onto a latent variable that represents the incongruence between the partners. For the following examples in SEM, we will need wide data.
g1dat = ml.df[ml.df$grp=='g1',] # all group one data
g2dat = ml.df[ml.df$grp=='g2',] # all group two data
lv.wide= merge(g1dat,g2dat,by=c('did'),all=T) # stick them together based on dyad id.
# quick peek
head(lv.wide) 
#' Our first model estimates the dyadic incongruence by creating a latent variable with constrained loadings from the outcomes or each partner. The loadings represent the incongruence coding seen above in the OLS and LME models.
dlcm_mod.a = '
d =~ -.5*out.x + .5*out.y
d ~ 1
out.x ~ 0
out.y ~ 0
out.x ~~ 1*out.x
out.y ~~ 1*out.y
'
fit.dlcm.a = sem(dlcm_mod.a,data=lv.wide)
summary(fit.dlcm.a)
#' Next we add in the dyadic average to this model above. This way we estimate both the level of the dyadic outcome as well as the incongruence between partners.
dlcm_mod.b = '
l =~ 1*out.x + 1*out.y
d =~ -.5*out.x + .5*out.y
d ~ 1
l ~ 1
d ~~ l
out.x ~ 0
out.y ~ 0
out.x ~~ 1*out.x
out.y ~~ 1*out.y
'
fit.dlcm.b = sem(dlcm_mod.b,data=lv.wide)
summary(fit.dlcm.b)


