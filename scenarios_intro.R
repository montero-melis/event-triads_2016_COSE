# Script to generate 8 different hypothetical scenarios for current study.
# Cross the following:
# - True effect: present or absent
# - Language variability: high or low
# - participant variability: high or low

library(ggplot2)
library(lme4)
library(dplyr)
library(boot)  # for inv.logit()
library(mvtnorm)


# Set plotting parameters -------------------------------------------------

# defined globally in separate script
source("global_parameters.R")



# Import actual data and set simulation parameters ------------------------

# import data
et <- read.csv("data_triads.csv", encoding = "UTF-8")

## We'll partly use some of the actual estimates

# use contrast coding for LanguageType
contrasts(et$LanguageType) <- contr.sum(2)
colnames(contrasts(et$LanguageType)) <- "Satellite_vs_Verb"
contrasts(et$LanguageType)


# Fit model with real data and extract coefficients
# The model is the same one as reported in the paper
m <- glmer(SameMannerResponse ~ 
             LanguageType +
             (1 | Language) +
             (1 | Participant) +
             (1 | ItemScene) +
             (1 | ItemWithinScene),
           data = et, family = "binomial"
           )
summary(m)


## Effect of Language type (S/V)

# actual estimated effect of Language Type
summary(m)$coef

# For simulations take one that is null and another that is greater than
# estimate but still largely consistent with the data (+/- 1.25 SE)
# Intercept always remains the same
effects <- summary(m)$coef

# true effect
true_eff <- effects[, "Estimate"]
true_eff[2] <- true_eff[2] + 1.25 * effects[2,2]
true_eff
# to probabilities
true_eff_S <- inv.logit(true_eff[1] + true_eff[2])
true_eff_V <- inv.logit(true_eff[1] - true_eff[2])

# null effect
null_eff <- effects[, "Estimate"]
null_eff[2] <- 0
null_eff
# as probability
null_eff_prob <- inv.logit(null_eff[1])


## Random effects

# Actual random effects in the data
# (extract random effects variance-covariance matrices):
summary(m)$varcor
as.data.frame(VarCorr(m)) 
# based on these we'll choose high and low variability for simulations
hi_var <- 4.25
lo_var <- 0.3

# total number of simulations to run
nb.sims <- 1
# number of languages
n.lang <- 20
# number of subjects per Language
n.subj <- 12



# Function to simulate data from the different scenarios ------------------

# This function samples languages in a balanced way, i.e. same amount of V and
# S-languages (it's taken and slightly adapted/simplified from power analysis)
make.balanced.data.generator <- function(d, n.lang, n.subj, effects,
                                         lang.ranef.covar, subj.ranef.covar) {
  
  generate.data <- function() {  # generates simulated data
    
    # first, grouping information
    simdata <- data.frame(
      SubjID = rep(1 : (n.subj * n.lang)),
      # Assign each subject to a language type and a language
      # number of S- and V-languages is balanced
      LanguageIndex = rep(1:n.lang, each = n.subj),  # Arbitrary lang index; 12 subjects/lang
      LanguageType = c(
        rep(levels(d$LanguageType)[1], each = n.subj * n.lang / 2),  # S-languages
        rep(levels(d$LanguageType)[2], each = n.subj * n.lang / 2))  # V-languages
    )
    
    # Use same contrast coding as in original data (S = 1, V = -1):
    contrasts(simdata$LanguageType) <- contrasts(d$LanguageType)
    
    # simulate random effects
    subj.adjustments <- sapply(1 : (n.subj * n.lang), function(x) {
      rmvnorm(n = 1, sigma = matrix(subj.ranef.covar)) })
    lang.adjustments <- sapply(1:n.lang, function(x) {
      rmvnorm(n = 1, sigma = matrix(lang.ranef.covar)) })
    
    # add random effects to simulated data frame
    simdata$ranef_subj <- subj.adjustments
    simdata$ranef_lang <- rep(lang.adjustments, each = n.subj)
    
    # compute actual effects
    model.mat <- model.matrix(~ LanguageType, simdata)
    simdata$fixef <- model.mat %*% effects
    simdata$LogOdds <- with(
      simdata, fixef + ranef_subj + ranef_lang)
    # to probabilitites
    simdata$prob <- inv.logit(simdata$LogOdds)
    
    return(simdata)
  }
}



# Run simulations ---------------------------------------------------------

# make simulations, and hence plots, reproducible
set.seed(1)  
# simulate
sim <- data.frame()
# Effect is true
for (effect in list(true_eff, null_eff)) {
  print(effect)
  for (subj_var in c(hi_var, lo_var)) {
    print(subj_var)
    for (lang_var in c(hi_var, lo_var)) {
      print(lang_var)
      d.simulator <- make.balanced.data.generator(
        et, n.lang, n.subj, effects = effect, lang_var, subj_var)
      current <- plyr::rdply(.n = nb.sims, d.simulator())
      current$Effect <- effect[2]
      current$LangVar <- lang_var
      current$SubjVar <- subj_var
      sim <- rbind(sim, current)
    }
  }
}



# Define and adjust columns for plotting ----------------------------------

# grouping factors
sim$LanguageVar <- factor(sim$LangVar, labels = c("low", "high"))
sim$SubjectVar <- factor(sim$SubjVar, labels = c("low", "high"))
sim$TrueEffect <- factor(sim$Effect, labels = c("null", "true"))
# give each language a unique index
sim$LangUnique <- rep(1 : (nrow(sim) / n.subj), each = n.subj)
# this is necessary to free the x-scales when plotting
sim$Variability <- with(
  sim, paste(SubjectVar, " within-language participant variability", "\n",
             LanguageVar, " within-type language variability", sep = ""))
sim$Variability <- factor(
  sim$Variability, levels = levels(factor(sim$Variability))[c(4, 3, 2, 1)])
levels(sim$Variability) <- paste(LETTERS[1:4], "-", levels(sim$Variability))

head(sim)
tail(sim)



# Plot simulations --------------------------------------------------------

# order languages by average probability of manner choice
lang_ord <- sim %>%
  group_by(LangUnique) %>%
  summarise(M = mean(prob))
myorder <- lang_ord$LangUnique[order(lang_ord$M)]
sim$Language <- factor(sim$LangUnique, levels = myorder)
rm(lang_ord, myorder)

# Two data frames, one for true and one for null
data_true <- sim[sim$TrueEffect == "true", ]
data_null <- sim[sim$TrueEffect == "null", ]

## True effect
# plot with jitter
trueplot <- ggplot(
  data_true, aes(x = Language, y = prob, color = LanguageType,
                 shape = LanguageType, linetype = LanguageType)) +
  facet_wrap(~Variability, scales = "free_x") +
  geom_jitter(position = position_jitter(width = .75, height = 0),
              alpha = .15) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .7) +
  xlab("Language") +
  ylab("Probability of manner-based categorization")
trueplot

# add empirical by-type means
type_mean_true_emp <- data_true %>%
  group_by(LanguageType, Variability) %>%
  summarise(M = mean(inv.logit(LogOdds)))

# add to plot
trueplot_hline <- trueplot +
  geom_hline(aes(yintercept=M, colour=LanguageType, linetype=LanguageType),
             data = type_mean_true_emp)
trueplot_final <- trueplot_hline + ggtheme +
  theme(axis.title.y = element_text(vjust = 1),
        legend.position = "top", axis.text.x = element_blank(),
        text = element_text(size = 11))
trueplot_final

# save
ggsave("figures/simulation_true.pdf", width = 6, height = 3.8)
ggsave("figures/simulation_true.tiff", width = 6, height = 3.8, dpi = mydpi)


## Null effect

# plot with jitter (replace data frame from nullplot)
nullplot <- trueplot %+% data_null
nullplot
# compute empirical by-type means
type_mean_null_emp <- data_null %>%
  group_by(LanguageType, Variability) %>%
  summarise(M = mean(inv.logit(LogOdds)))
# add to plot
nullplot_hline <- nullplot +
  geom_hline(aes(yintercept=M, colour=LanguageType, linetype=LanguageType),
             data = type_mean_null_emp,  size = .75)
nullplot_hline + ggtheme +
  theme(axis.title.y = element_text(vjust = 1),
        legend.position = "top", axis.text.x = element_blank())

# # save
# ggsave("figures/simulation_null.pdf", width = 7, height = 5)
# ggsave("figures/simulation_null.tiff", width = 7, height = 5, dpi = mydpi)
