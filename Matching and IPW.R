### BIOL 597: Matching and Inverse probability weighting:

## New packages alert! Install these first, please:
library(MatchIt) #  For matching
library(marginaleffects) # For visualizing model fits easily

## And some familiar ones:
library(tidyverse)  
library(ggdag)  
library(dagitty) 
library(broom)  



## 1) The "Naive" model: ########

# first, let's fit a 'naive' model, just containing the thinning variable, which will assume that the thinning treatment has been effectively randomized. Since you all are DAG experts by now, I know that this model seems like a potentially silly thing to fit, but we'll use it as a point of comparison for the other approaches. 
# Remember, the "true" effect of thinning should be 3.

naivemod <- lm(pondos~thinning, data=df)
summary(naivemod)

plot_cap(naivemod, type="response",
         conf_level=0.95,
         condition=c("thinning")) +
  ylim(61, 74) + 
  ylab("Pondo basal area surviving following fire") +
  xlab("Thinning treatment") + theme_bw()


## Discuss:
## How well does the naive model estimate the effect of thinning? 
## Why is the estimate biased? Why is it biased in a particular direction (positive or negative) from the true value?



## 2) Matching ########

# Next, let's try to improve balance in our covariates for treated v. untreated groups by using matching.
# We'll use a new package ("MatchIt"), which contains lots of other matching approaches. For now, we'll just use nearest neighbor matching on the basis of Mahalanobis distances.

# The matchit command will find matched pairs for our control and thinned observations
matching <- matchit(thinning~elevation+ roaddist + slope,
                        data = df,
                        distance = "mahalanobis", # using mahalanobis distances
                        method = "nearest", # find the nearest neighbors
                        replace = TRUE, # match with replacement, so control units are allowed to 
                                        # be matched to several treated units. If FALSE, each obs. can only
                                        # be matched to one other
                        caliper=c(elevation=0.4, # The caliper determines how similar the obs. have to be
                                  roaddist=0.4,  # to be matched. Lower = obs. have to be more similar.
                                  slope=0.4))  ## The caliper = "how many std dev" apart (rel. to sd
                                                # of the original data) obs. are allowed to be.

# Now, let's look at a summary of what the matching did:
summary(matching)

# Save the matched data as it's own new dataset. How many observations did you end up with?
matched.df <- match.data(matching)

## We can also visualize how the "balance" of our treated and untreated groups differs now, in relationship to variables like "slope"
ggplot(df, aes(x =slope,   ## (change to "df" to see pre-matching balance).
                       color = as.factor(thinning), 
                       fill=as.factor(thinning))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Slope (degrees)") + theme_bw()


## Discuss:
# a) Try to decipher the summary table from the matching with your group. What is the table communicating to you? What has the matching achieved?
# b) Tinker with the "caliper" argument above, shifting it a bit lower (try something like 0.2 or 0.1). How do the matched outputs change? How does the estimation of the treatment effect change?




## 3) Analysis of matched data ##########

# Reset the caliper to 0.4 and match the data again.

## Now, let's fit a model on the matched data. It is common to use additional "control" variables in your model after matching too, but for now, let's see what happens if we just include "thinning" as our only predictor.

matchmod <- lm(pondos~thinning, data=matched.df)
summary(matchmod)

plot_cap(matchmod, type="response",
         conf_level=0.95,
         condition=c("thinning")) +
  ylim(61, 74) + 
  ylab("Pondo basal area surviving following fire") +
  xlab("Thinning treatment") + theme_bw()


## Discuss:
## a) How well does the model fit to the matched data capture the true "treatment" effect? How does it compare to our naive model?
## b) Change the caliper above back to 0.2. How does the lm estimate of "thinning" change as the caliper decreases? Why?


# A quick visual of what the matched data looks like relative to the full data:
df <- df %>% 
  mutate(inmatched = ifelse(X%in% unique(matched.df$X), "matched", "notmatched"))

ggplot(df,
       aes(x=elevation, y=slope,
           color = as.factor(thinning))) +
  geom_point(aes(alpha = inmatched)) + 
  scale_alpha_discrete(range = c(0.9, 0.2))+
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  xlab("Elevation") + ylab("Slope") +
  labs(color = "Treatment group") + theme_bw()


## 4) Inverse probability weighting #############

# Now let's try IPW, which weights data points differently based on how "atypical" they are -- the "atypicalness" of a point is determined by the propensity score of a given observation. Propensity scores are generated using logistic/binomial glms and describe how probable it is for a given observation to have received treatment, based on variables you think might be biasing treatment assignment.


# First, let's create the propensity scores by fitting our binomial glm.
# We include covariates thought to be biasing treatment assignment from our DAG
propensitymod <- glm(thinning~elevation+
              roaddist + slope, data=df, family="binomial")

summary(propensitymod) #Note that our coefficients are on that tricky log-odds scale.

# In thinking about results from glms with link functions, I find it easier to generate "marginal effects" plots, which basically show the predictions that come out of a glm. That way, you can see how much the predicted probability of your response changes across the values of a given predictor variable.

# For instance, this is the prediction from the estimated relationship between elevation and treatment assignment. The "plot_cap" function comes from the marginal effects package
plot_cap(propensitymod, type="response",
         conf_level=0.95,
         condition=c("elevation")) +
  ylab("Probability of receiving thinning treatment") +
  xlab("Elevation (m)") + theme_bw()

# For road distance
plot_cap(propensitymod, type="response",
         conf_level=0.95,
         condition=c("roaddist")) +
  ylab("Probability of receiving thinning treatment") +
  xlab("Distance from road (km)") + theme_bw()

# For slope
plot_cap(propensitymod, type="response",
         conf_level=0.95,
         condition=c("slope")) +
  ylab("Probability of receiving thinning treatment") +
  xlab("Slope (degrees)") + theme_bw()

## Though, remember! We haven't developed our binomial glm using causal logic, so don't get carried away with interpreting the effects of these variables on treatment assignment. We're just using them to predict treatment assignment, not infer effects of particular variables on it. Though I picked these specifically to be variables that might, theoretically, be identifiable given our DAG.


## Now, let's use the model to generate predicted probabilities of "receiving treatment" for each observation, AKA our "propensity scores"

ipw.df <- augment_columns(propensitymod, # augment columns just adds the predictions onto our dataframe
                            df,
                            type.predict = "response") %>%
              rename(propensity = .fitted) # We are going to rename the column that it added "propensity"
                                            # because the default is to call it '.fitted'

## Take a peek at what it created. Note the structure of the propensity scores
head(ipw.df$propensity)


## Now, we'll calculate the inverse probability weights, which describe how "atypical" an observation's treatment assignment is for a given propensity score.
ipw.df <- ipw.df %>%
  mutate(ipw = (thinning / propensity) + 
           ((1 - thinning) / (1 - propensity))) 

## Take a peek at what it created. 
head(ipw.df$ipw)


## here's how those weights look in relationship to the observed data, using elevation & slope on the axes:

ggplot(ipw.df,
       aes(y=slope, x=elevation,
           color = as.factor(thinning), size=ipw)) +
  geom_point() + 
  scale_color_manual(values=c( "skyblue3", "orangered3"),
                     labels=c("No thinning", "Thinning"))+
  ylab("Slope") + xlab("Elevation") +
  labs(color = "Treatment group") + theme_bw()


## Lastly, let's fit a model using the inverse probability weights. The lm function has a "weights" option, which makes this very easy to do.
ipwmod <- lm(pondos~thinning, 
             data=ipw.df,
             weights = ipw)
summary(ipwmod)


plot_cap(ipwmod, type="response",
         conf_level=0.95,
         condition=c("thinning")) +
  ylim(61, 74) +
  ylab("Probability of receiving thinning treatment") +
  xlab("Thinning treatment") + theme_bw() 


## Discuss:
## a) How well does the IPW model fit capture the true "treatment" effect? How does this compare to the naive model?



 
## 5) A DAGgy model: ######

## lastly, let's try the DAG approach. Remember, we're in a unique situation here in that, we know exactly what generated the data, so our DAG is theoretically perfect.
daggymod <- lm(pondos~thinning + elevation+  roaddist + slope,
               data=df)
summary(daggymod)


plot_cap(daggymod, type="response",
         conf_level=0.95,
         condition=c("thinning")) +
  ylim(61, 74) +
  ylab("Probability of receiving thinning treatment") +
  xlab("Thinning treatment") + theme_bw() 


## Discuss:
## a) How well does the DAG-driven model fit capture the true "treatment" effect? 



## Bonus: IPW bootstrapped CIs: #######
## Even though the IPW-based estimate above can be trusted, the IPW approach above results in confidence intervals that are too narrow/precise. This is because there's uncertainty in the estimation of that propensity score that we don't allow to propagate into our final model (we are just using the mean propensity score predicted by our logistic regression). There are a few different ways that people handle this in IPW, but probably the most common is to "bootstrap"...

# Bootstrapping basically takes random subsets (with replacement) of our data lots of different times and then fits lots of different models to those subsets to communicate the variation in each subset's results.

## We won't do this today, but this is an excellent tutorial explaining how to do this in R:
# https://causalinferencebookr.netlify.app/chapter-12-ip-weighting-and-marginal-structural-models.html



