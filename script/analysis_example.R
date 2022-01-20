library(dplyr)

#Load clean data (see data_cleaning.R) ####
clean_data <- read.csv(here::here("data/clean_data.csv"))

#Prepare data for analysis ####

##Create age categories (<=35 & >35)
clean_data <- clean_data %>% 
  dplyr::mutate(age = 2021 - .data$BirthYear,
                age_category = dplyr::case_when(age <= 35 ~ "<=35",
                                                age > 35 ~ ">35"))

##Remove unknown gender (sample size too small to use for current analysis)
clean_data <- clean_data %>% 
  dplyr::filter(Gender != "Other")

##Consider only BER data for now
## Need to decide how to deal with Kiel...include and add ID as random effect?
## Use only one sample per individual?
## Include Location as a predictor.
clean_data <- clean_data %>% 
  dplyr::filter(Location == "BERLIN")

# EXAMPLE OF LOGISTIC REGRESSION ####

## Use first question (Importance of microplastics)
## Split into 'strongly agree' v. other
microplastic_data <- clean_data %>% 
  dplyr::mutate(micro_category = dplyr::case_when(Microplastic_importance_LK == 7 ~ 1,
                                                  Microplastic_importance_LK != 7 ~ 0))

##Create model
example_logmodel <- glm(micro_category ~ Gender + age_category + Treatment,
                        data = microplastic_data, family = binomial(link = "logit"))

summary(example_logmodel)

##Check model using DHARMa
library(DHARMa)
library(binom)
library(ggplot2)

simulationOutput <- simulateResiduals(example_logmodel, seed = 123, n = 5000)
plot(simulationOutput)

#Check there are no unusual residual patterns for each predictor
plotResiduals(simulationOutput, form = as.factor(microplastic_data$Gender))
plotResiduals(simulationOutput, form = as.factor(microplastic_data$age_category))
plotResiduals(simulationOutput, form = as.factor(microplastic_data$Treatment))
plotResiduals(simulationOutput, form = as.factor(microplastic_data$language))

## Plot results (gender and treatment)
plot_data <- microplastic_data %>% 
  dplyr::group_by(Gender, Treatment) %>% 
  dplyr::summarise(binom::binom.wilson(x = sum(micro_category), n = n()), .groups = "drop")

ggplot() +
  geom_errorbar(data = plot_data, aes(x = Treatment, ymin = lower, ymax = upper, colour = Gender),
                size = 1.5, width = 0.25, position = position_dodge(width = 0.25)) +
  geom_point(data = plot_data, aes(x = Treatment, y = mean, fill = Gender),
             shape = 21, size = 4, stroke = 1.5, position = position_dodge(width = 0.25)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Before exhibition", "After exhibition")) +
  labs(y = "Proportion of 'strongly agree'", x = "") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

## Seems that it would be more appropriate to have an interaction b/w treatment and gender
example_logmodel_int <- glm(micro_category ~ Gender*Treatment + age_category,
                        data = microplastic_data, family = binomial(link = "logit"))

summary(example_logmodel_int)

simulationOutput_int <- simulateResiduals(example_logmodel_int, seed = 123, n = 5000)
plot(simulationOutput_int)

#Check there are no unusual residual patterns for each predictor
plotResiduals(simulationOutput_int, form = as.factor(microplastic_data$Gender))
plotResiduals(simulationOutput_int, form = as.factor(microplastic_data$age_category))
plotResiduals(simulationOutput_int, form = as.factor(microplastic_data$Treatment))

## NOT SIGNIFICANT BUT PROBABLY MORE REASONABLE

## TRY WITH NEXT VARIABLE
microplastic_data <- clean_data %>% 
  dplyr::mutate(micro_category = dplyr::case_when(Viable_action_LK == 7 ~ 1,
                                                  Viable_action_LK != 7 ~ 0))

##Create model
example_logmodel <- glm(micro_category ~ Gender*Treatment + age_category,
                        data = microplastic_data, family = binomial(link = "logit"))

summary(example_logmodel)

##Check for outliers using DHARMa
library(DHARMa)
library(binom)
library(ggplot2)

plot(simulateResiduals(example_logmodel))

## Plot results (gender and treatment)
plot_data <- microplastic_data %>% 
  dplyr::group_by(Gender, Treatment) %>% 
  dplyr::summarise(binom::binom.wilson(x = sum(micro_category), n = n()), .groups = "drop")

ggplot() +
  geom_errorbar(data = plot_data, aes(x = Treatment, ymin = lower, ymax = upper, colour = Gender),
                size = 1.5, width = 0.25, position = position_dodge(width = 0.25)) +
  geom_point(data = plot_data, aes(x = Treatment, y = mean, fill = Gender),
             shape = 21, size = 4, stroke = 1.5, position = position_dodge(width = 0.25)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Before exhibition", "After exhibition")) +
  labs(y = "Proportion of 'strongly agree'", x = "") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))


## EXAMPLE OF COMBINED QUESTION WITH RANDOM EFFECT
microplastic_data <- clean_data %>% 
  dplyr::mutate(personal_category = dplyr::case_when(Personal_responsibility_LK == 7 ~ 1,
                                                     Personal_responsibility_LK != 7 ~ 0),
                political_category = dplyr::case_when(Political_responsibility_LK == 7 ~ 1,
                                                      Political_responsibility_LK != 7 ~ 0)) %>% 
  dplyr::select(Treatment, Gender, age_category, personal_category, political_category) %>% 
  dplyr::mutate(ID = 1:n()) %>% 
  tidyr::pivot_longer(cols = c(personal_category, political_category))

##Create model
library(lme4)
example_logmodel <- glmer(value ~ Gender + Treatment + age_category + name + Gender:Treatment + name:Treatment + (1|ID),
                        data = microplastic_data, family = binomial(link = "logit"))

summary(example_logmodel)

##Check for outliers using DHARMa
library(DHARMa)
library(binom)
library(ggplot2)

plot(simulateResiduals(example_logmodel))

## Plot results (gender and treatment)
plot_data <- microplastic_data %>% 
  dplyr::group_by(Gender, Treatment, name) %>% 
  dplyr::summarise(binom::binom.wilson(x = sum(value), n = n()), .groups = "drop")

ggplot() +
  geom_errorbar(data = plot_data, aes(x = Treatment, ymin = lower, ymax = upper, colour = Gender),
                size = 1.5, width = 0.25, position = position_dodge(width = 0.25)) +
  geom_point(data = plot_data, aes(x = Treatment, y = mean, fill = Gender),
             shape = 21, size = 4, stroke = 1.5, position = position_dodge(width = 0.25)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Before exhibition", "After exhibition")) +
  labs(y = "Proportion of 'strongly agree'", x = "") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  facet_wrap(facets = ~name)
  

# EXAMPLE OF ORDINAL LOGISTIC REGRESSION ####

library(ordinal)

## Use 'participate in collective action'
## Split into 1-2, 3-5, 6-7
collective_data <- clean_data %>% 
  dplyr::mutate(action_category = dplyr::case_when(Willingness_community_LK < 3 ~ "Disagree",
                                                   Willingness_community_LK >= 3 & Willingness_community_LK < 6 ~ "Neutral",
                                                   Willingness_community_LK >= 6 ~ "Agree")) %>% 
  #Make the new category ordered
  dplyr::mutate(action_category = ordered(action_category, levels = c("Disagree", "Neutral", "Agree")))

##Create model
example_OLRmodel <- clm(action_category ~ Treatment + Gender + age_category,
                        data = collective_data, link = "logit")

summary(example_OLRmodel)

##Check assumption of proportional odds using Brant's test
##Model needs to be refitted using polr
library(brant)
example_OLRmodel_polr <- MASS::polr(action_category ~ Treatment + Gender + age_category,
            data = collective_data, Hess = TRUE)
#Some issue with code...
brant(example_OLRmodel_polr)

#Check assumptions using inbuilt ordinal package functions
#Also no suggestion of non-proportional odds
ordinal::nominal_test(example_OLRmodel)

#Alternative method, refit separate models and see if coefs differ
#Suggests there may be some issues with treatment effect
#### NEED TO STUDY THIS IN MORE DETAIL!!
library(Hmisc)
sf <- function(y) {
  c('Disagree' = qlogis(mean(y >= 1)),
    'Neutral' = qlogis(mean(y >= 2)),
    'Agree' = qlogis(mean(y >= 3)))
}

(s <- with(collective_data, summary(as.numeric(action_category) ~ Treatment + Gender + age_category, fun=sf)))

plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))

#Fit multiple logistic models as an example
glm(I(as.numeric(action_category) >= 2) ~ Treatment + Gender + age_category, family="binomial", data = collective_data)
glm(I(as.numeric(action_category) >= 3) ~ Treatment + Gender + age_category, family="binomial", data = collective_data)
