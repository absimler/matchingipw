## BIOL 597: Visualizing data and checking for "balance" in treatment assignment in observational studies:

## New packages alert! Install these first, please:
library(ggdist) # add on to ggplot for visualizing uncertainty
library(corrplot) ## Make correlation plots!

# plus, some familiar friends:
library(tidyverse)  
library(ggdag)  
library(dagitty) 
library(broom)  


set.seed(597)

df <- read.csv("pondos.csv") # Load the data


### 1) Visualizing the DAG:
## First, let's visualize the dag we described in lecture, using the "ggdag" package

fire_dag <- dagify(
  # Assign relationships
  surv.pondos ~ fireint,
  fireint ~ slope + temperature + roaddist + thinning,
  temperature ~ elevation,
  roaddist ~ slope,
  thinning ~ elevation + roaddist + slope,
  # Declare exposure and outcome of interest
  exposure = "thinning",
  outcome = "surv.pondos",
  # This is just to place everything on the plot in an organized fashion:
  coords = list(x = c(surv.pondos= 5, 
                      fireint = 2,
                      thinning = 0,
                      elevation = 0.5,
                      temperature = 1,
                      roaddist = 3.5,
                      slope = 2.5),
                y = c(surv.pondos = 0.5,
                      fireint = 0,
                      thinning = 0,
                      elevation = 5,
                      temperature = 4,
                      slope = 5,
                      roaddist =5)),
  # More descriptive labels for nodes
  labels = c(surv.pondos = "Surviving\nPonderosas",
             fireint = "Fire intensity",
             temperature = "Mean Temp.",
             thinning = "Pre-fire thinning",
             elevation = "Elevation", 
             slope = "Slope",
             roaddist = "Distance from nearest road"))

# Plots our DAG object
ggdag_status(fire_dag, 
             use_labels = "label", 
             text = FALSE) +
  theme_dag(legend.position = "none")

## Discuss:
## a) Given the DAG above, which variables would you include in a model to estimate the effect of thinning on ponderosa pine survival? Which would be important to account for in the matching process or propensity score estimation for inverse probability weighting?




## 2) Visualizing our data #############
## At the beginning of any observational study, it's a good idea to start with a little visualization to see how "well-balanced" our treatment and control groups are, in terms of their (possibly confounding characteristics). This will help us understand whether "treatment" has truly been randomized according to these variables.

## note that this process is informed by our DAG.

## Create a density plot for treatment groups, by their elevation values:
ggplot(df, aes(x = elevation, 
               color = as.factor(thinning), fill=as.factor(thinning))) +
  geom_density(alpha=0.4) +
  # Aesthetic stuff:
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Elevation") + theme_bw()

## Create a density plot for treatment groups, by their slope values:
ggplot(df, aes(x = slope, 
               color = as.factor(thinning), 
               fill=as.factor(thinning))) +
  geom_density(alpha=0.4) +
  # Aesthetic stuff:
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Slope (degrees)") + theme_bw()


## Create a density plot for treatment groups, by their "distance from the nearest road" values:
ggplot(df, aes(x = roaddist, 
               color = as.factor(thinning), 
               fill=as.factor(thinning))) +
  geom_density(alpha=0.4) +
  # Aesthetic stuff
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Distance from the nearest road (km)") + theme_bw()


# I like the density plots because they show the full distribution of values, but boxplots or interval plots work well here too!
ggplot(df, aes(y = elevation, x=as.factor(thinning),
               fill = as.factor(thinning))) +
  geom_boxplot() +
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  labs(x="Thinning treatment")+
  labs(y = "Elevation") + theme_bw()


# Lastly, a quick correlation plot is often a helpful tool too:
corrplot(cor(df[c("thinning", "elevation", "slope", "roaddist", "temp")]))



# DISCUSS:
# a) What are these figures illustrating? Do the treatment and "control" groups appear to be balanced? If not, by which characteristics do the treatment and control groups differ?
# b) What other visualizations would be useful to use here? What do you use in your own workflow?




