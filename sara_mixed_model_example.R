
# setup -------------------------------------------------------------------


library(tidyverse)
# lme4 is a common package for linear mixed effects models
library(lme4)
# lmerTest gives you the p-values, load it after 
# lme4 b/c it overwrites some lme4 functions (working together w/ them)
library(lmerTest)
library(glue)


# make some data ----------------------------------------------------------


c_sites <- glue("C", "{1:4}")
d_sites <- glue("D", "{1:4}")
sites <- c(c_sites, d_sites)

veg <- rep(c(rep("C", 4), rep("D", 4)), 3)
worm <- rep(c(rep("high", 2), rep("low", 2), rep("high", 2), rep("low", 2)), 3)
reps <- c(rep(c("R1", "R2", "R3"), 8))

set.seed(123)
degree_days <-  runif(24)

df <- data.frame(site_id = sites,
                 veg = veg,
                 worm = worm,
                 dd = degree_days) %>% 
  # creating an "effect" of worm invasion
  # by adding a random positive number to "high" sites
  # so we see something in the model results below
  mutate(dd = case_when(
    worm == "high" ~ dd + runif(1, min = 0.1, max = 2), 
    TRUE ~ dd
  ))


# model examples ----------------------------------------------------------


# long way to write it
# random effect is (1|site_id), this specifies a random intercept for each site
mod1 <- lmer(data = df, formula = dd ~ veg + worm + veg*worm + (1|site_id))

# view the model object
mod1

# summary is more helpful, shows p values
summary(mod1)

# short way to write it 
# lmer assumes you also want the main effects of veg & worm, 
# when you specify the interaction here, so this model is equivalent
# to the one above.
mod2 <- lmer(data = df, formula = dd ~ veg*worm + (1|site_id))
summary(mod2)


# estimated marginal means and contrasts ----------------------------------

# OK, above we found a significant effect of "earthworm invasion" (high/low) so
# you'd likely want to report the mean response of each group and the estimated
# difference between groups. 

# emmeans = estimated marginal means
library(emmeans)

# returns a NOTE mentioning interations
# in this example the interaction term isn't significant
# so not worried about it 
emm1 <- emmeans(mod1, specs = pairwise ~ worm)

# note this is a list with two objects in it: 1) "emmeans": estimated 
# marginal means and 95% CIs for the "high" and "low" 
# conditions (averaged across veg types). 2) contrasts: estimate and standard
# error for the difference between the high and low conditions. 
# helpful for reporting because when you find a difference you'll want 
# to specify the size, direction, error of the difference
emm1




