## Simulate data from forced choice triads task (path- vs manner-choice)

library(ggplot2)
library(lme4)
library(dplyr)
library(mvtnorm)
library(purrr)  # for the quietly() function
library(boot)  # for inv.logit()


# Set global parameters ---------------------------------------------------

# some plotting parameters are defined globally in separate script
source("global_parameters.R")

# Stop RStudio from going into browser mode when encountering a degenerate
# Hessian during the simulations.
options(error = NULL)


# Simulation functions ----------------------------------------------------

# Data generator function; it returns a function to be called by rdply
make.data.generator <- function(d, n.obs, n.lang, effects, resid.var,
                                subj.ranef.covar, item.ranef.covar, 
                                lang.ranef.covar) {
  
  generate.data <- function() {  # generates simulated data
    
    n.subj <- n.lang * n.obs  # total number of subjects
    # first, grouping information
    simdata <- data.frame(
      SubjID = rep(1:n.subj, each = n.obs, length.out = n.subj * n.obs),
      Item = 1:n.obs,
      # Assign each subject to a language type and a language, keeping same 
      # proportions as in study (7/12 for S/V language).
      # Arbitrary language index. For each language, 12 subjects with 12 items:
      LanguageIndex = rep(1:n.lang, each = n.obs * 12), 
      LanguageType = c(
        rep(levels(d$LanguageType)[1], each = n.obs * 12 * 7),  # 7 S-framed languages
        rep(levels(d$LanguageType)[2], each = n.obs * 12 * 12))  # 12 V-framed languages
    )
    
    # Use same contrast coding as in original data (S = 1, V = -1):
    contrasts(simdata$LanguageType) <- contrasts(d$LanguageType)
    
    # simulate random effects
    subj.adjustments <- sapply(1:n.subj, function(x) rmvnorm(n = 1, sigma = subj.ranef.covar))
    # Note on item adjustments: there were 72 different items; these were divided
    # into 6 subsets of 12 items; each subject saw one such subset so that for
    # each language, each subset was seen twice (latin square)
    item.adjustments <- sapply(1:(n.obs * 6), function(x) rmvnorm(n = 1, sigma = item.ranef.covar))
    lang.adjustments <- sapply(1:n.lang, function(x) rmvnorm(n = 1, sigma = lang.ranef.covar))
    
    # add random effects to simulated data frame
    simdata$ranef_subj <- rep(subj.adjustments, each = n.obs)
    simdata$ranef_item <- item.adjustments
    simdata$ranef_lang <- rep(lang.adjustments, each = 12 * n.obs)
    simdata$error <- rnorm(n = nrow(simdata), mean = 0, sd = resid.var)
    
    # compute actual effects
    model.mat <- model.matrix(~ LanguageType, simdata)
    simdata$fixef <- model.mat %*% effects
    simdata$LogOdds <- with(
      simdata, fixef + ranef_subj + ranef_item + ranef_lang + error)
    
    # And finally for each trial, is it a Manner choice? (Bernoulli trials)
    simdata$MannerChoice <- sapply(
      X = plogis(simdata$LogOdds),
      FUN = function(x) { rbinom(1, 1, x) } )
    
    return(simdata)
  }
}

# Following function samples languages in a balanced way, i.e. same amount of
# V and S-languages, otherwise it's the same as make.data.generator()
# (it's tremendously redundant, but works)
make.balanced.data.generator <- function(d, n.obs, n.lang, effects, resid.var,
                                         subj.ranef.covar, item.ranef.covar, 
                                         lang.ranef.covar) {
  
  generate.data <- function() {  # generates simulated data
    
    n.subj <- n.lang * n.obs  # total number of subjects
    # first, grouping information
    simdata <- data.frame(
      SubjID = rep(1:n.subj, each = n.obs, length.out = n.subj * n.obs),
      Item = 1:n.obs,
      # Assign each subject to a language type and a language
      # number of S- and V-languages is balanced
      LanguageIndex = rep(1:n.lang, each = n.obs * 12),  # Arbitrary language index. For each language, 12 subjects with 12 items
      LanguageType = c(
        rep(levels(d$LanguageType)[1], each = n.obs * 12 * n.lang/2),  # 7 S-framed languages
        rep(levels(d$LanguageType)[2], each = n.obs * 12 * n.lang/2))  # 12 V-framed languages
    )
    
    # Use same contrast coding as in original data (S = 1, V = -1):
    contrasts(simdata$LanguageType) <- contrasts(d$LanguageType)
    
    # simulate random effects
    subj.adjustments <- sapply(1:n.subj, function(x) rmvnorm(n = 1, sigma = subj.ranef.covar))
    # Note on item adjustments: there were 72 different items; these were divided
    # into 6 subsets of 12 items; each subject saw one such subset so that for
    # each language, each subset was seen twice (latin square)
    item.adjustments <- sapply(1:(n.obs * 6), function(x) rmvnorm(n = 1, sigma = item.ranef.covar))
    lang.adjustments <- sapply(1:n.lang, function(x) rmvnorm(n = 1, sigma = lang.ranef.covar))
    
    # add random effects to simulated data frame
    simdata$ranef_subj <- rep(subj.adjustments, each = n.obs)
    simdata$ranef_item <- item.adjustments
    simdata$ranef_lang <- rep(lang.adjustments, each = 12 * n.obs)
    simdata$error <- rnorm(n = nrow(simdata), mean = 0, sd = resid.var)
    
    # compute actual effects
    model.mat <- model.matrix(~ LanguageType, simdata)
    simdata$fixef <- model.mat %*% effects
    simdata$LogOdds <- with(
      simdata, fixef + ranef_subj + ranef_item + ranef_lang + error)
    
    # And finally for each trial, is it a Manner choice? (Bernoulli trials)
    simdata$MannerChoice <- sapply(
      X = plogis(simdata$LogOdds),
      FUN = function(x) { rbinom(1, 1, x) } )
    
    return(simdata)
  }
}


## Function to fit model on simulated data and store output;
# it fits the full model, like in our main analysis
fit.models <- function(d.sim) {
  # use purrr:quietly to easily extract warnings and store in df
  quietly_output <- purrr::quietly(glmer)(
    formula = MannerChoice ~ LanguageType +
      (1 | SubjID) +
      (1 | Item) +
      (1 | LanguageIndex),
    data = d.sim,
    family = "binomial")
  
  # fitted model extracted from quietly's output
  m.tmp <- quietly_output$result
  
  # print any warnings
  print(quietly_output$warnings)
  
  # Store only language difference coeff (estimate of intercept irrelevant)
  simulation <- data.frame(summary(m.tmp)$coef)[2, ]
  # Store warnings to identify convergence problems
  simulation$Warnings <- paste(unlist(quietly_output$warnings), 
                               collapse = "; ")
  
  rownames(simulation) <- NULL
  return(simulation)
}

## Function to fit four models on simulated data and store output
# It's like the above fit.models() but this one fits different models to
# assess Type I errors as a function of the random effects structure
fit.models2 <- function(d.sim) {
  sim_df <- data.frame()
  # use purrr:quietly to easily extract warnings and store in df
  quiet_glmer <- purrr::quietly(glmer)
  # models to be compared:
  myformulae <- c(
    "MannerChoice ~ LanguageType + (1 | Item) + (1 | SubjID) + (1 | LanguageIndex)",
    "MannerChoice ~ LanguageType + (1 | Item) + (1 | SubjID) ",
    "MannerChoice ~ LanguageType + (1 | Item) + (1 | LanguageIndex)",
    "MannerChoice ~ LanguageType + (1 | Item)")
  
  for (myf in myformulae) {
    print(myf)
    quietly_output <- quiet_glmer(formula = myf, data = d.sim, family = "binomial")
    # print any warnings
    print(quietly_output$warnings)
    
    # fitted model extracted from quietly's output
    m.tmp <- quietly_output$result
    # Only language difference coeff is needed (estimate of intercept irrelevant)
    curr <- data.frame(summary(m.tmp)$coef)[2, ]
    curr$Formula <- myf
    
    # keep track of warnings to identify convergence problems
    curr$Warnings <- paste(unlist(quietly_output$warnings), collapse = "; ")
    
    # bind together
    sim_df <- rbind(sim_df, curr)
  }
  rownames(sim_df) <- NULL
  return(sim_df)
}



# Set up simulation parameters --------------------------------------------

## We need to use the estimates from the model fitted to our actual data

# import data
et <- read.csv("data_triads.csv", encoding = "UTF-8")
# set contrasts for language type as in original model,
# use contrast coding
contrasts(et$LanguageType) <- contr.sum(2)
colnames(contrasts(et$LanguageType)) <- "Satellite_vs_Verb"
contrasts(et$LanguageType)

# Fit model with real data and extract coefficients.
# Note the model is slightly simplified with respect to the one reported
# in the paper (removed random intercepts for ItemScene, as this effect is
# not simulated)
m <- glmer(SameMannerResponse ~ 
             LanguageType +
             (1 | Language) +
             (1 | Participant) +
             (1 | ItemWithinScene),
           et, family = "binomial"
)
summary(m)

## save 4 vectors of effects: mean and bounds for 95% C.I., and null
effects <- summary(m)$coef
# mean estimated language effects
effects_mean <- effects[, "Estimate"]
# lower bound for estimated language effects
effects_low <- effects_mean
effects_low[2] <- effects_low[2] - 1.96 * effects[2,2]
# upper bound for estimated language effects
effects_upper <- effects_mean
effects_upper[2] <- effects_upper[2] + 1.96 * effects[2,2]
# Finally, the case of a null effect
# Note: the intercept stays the same, only the language difference is 0!
effects_null <- effects_mean
effects_null[2] <- 0  # null effect

## Extract random effects variance-covariance matrices
subj.ranef.covar <- summary(m)$varcor$Participant
item.ranef.covar <- summary(m)$varcor$ItemWithinScene
lang.ranef.covar <- summary(m)$varcor$Language
resid.var <- attributes(summary(m)$varcor)$sc # sd of residuals

# Desired number of observations per subject (doesn't change across simulations);
# the number of languages will vary for the two power simulation types, however.
n.obs <- 12

# total number of simulations to run
nb.sims <- 5  # 10k simulations were run for the paper, took several days



# Express effects as probabilities ----------------------------------------

# When presenting the results of the power analysis it is clearer to express
# the true effects in the simulations as probabilities, so convert them:

# Lower bound
s_low <- inv.logit(effects_low[1] + effects_low[2])  # S-Languages
v_low <-inv.logit(effects_low[1] - effects_low[2])  # V-Languages
paste("S: prob=", round(s_low,2),"; ", "V: prob=", round(v_low,2), sep = "")
# null
s_null <- inv.logit(effects_null[1] + effects_null[2])  # S-Languages
v_null <- inv.logit(effects_null[1] - effects_null[2])  # V-Languages
# estimate
s_mean <- inv.logit(effects_mean[1] + effects_mean[2])  # S-Languages
v_mean <- inv.logit(effects_mean[1] - effects_mean[2])  # V-Languages
# upper bound
s_upper <- inv.logit(effects_upper[1] + effects_upper[2])  # S-Languages
v_upper <- inv.logit(effects_upper[1] - effects_upper[2])  # V-Languages

# convenience function to put the values into a readable string
express_prob <- function(x) {
  out <- paste("S: prob=", round(x[1], 2),";\n",
               "V: prob=", round(x [2], 2), sep = "")
  out
}
prob_low <- express_prob(c(s_low, v_low))
prob_null <- express_prob(c(s_null, v_null))
prob_mean <- express_prob(c(s_mean, v_mean))
prob_upper <- express_prob(c(s_upper, v_upper))

rm(express_prob)



# Simulations to assess type I error rate ---------------------------------

# We want to know if our analyses were anticonservative, and also how Type I
# errors would increase with different analyses (i.e. different models)

# set necessary parameters
n.lang <- 19  # number of languages

# reset data frame
simulations_null <- data.frame()

# run!
ptm <- proc.time()  # keep track of running time
# set up random data simulator as a function to be called by rdply
d.simulator <- make.data.generator(
  et, n.obs, n.lang, effects = effects_null, resid.var, subj.ranef.covar,
  item.ranef.covar, lang.ranef.covar)
# simulate data and fit models
simulations_null <- plyr::rdply(.n = nb.sims, fit.models2(d.simulator()))

# how long did the simulations take?
t.simulations_type1 <- proc.time() - ptm
print(t.simulations_type1)

head(simulations_null)



# Processing of Type I error simulations ----------------------------------

# give dataframe analogous shape to the simulations for power analysis above
simulations_null$TrueEffect <- effects_null[2]
names(simulations_null)[3:5] <- c("StdError", "z", "p")

# column that shows the model fitted in simplified form
simulations_null$Model <- factor(simulations_null$Formula,
                                 labels = c(
                                   "item only",
                                   "item & language",
                                   "item & participant",
                                   "item, participant\n& language"
                                   )
                                 )
# reorder factor levels
simulations_null$Model <- factor(simulations_null$Model,
                                 levels = c(
                                   "item, participant\n& language",
                                   "item & language",
                                   "item & participant",
                                   "item only"
                                   )
                                 )
# show
head(simulations_null)

# # store null simulations together in a single file (might require some tweaking)
# load("simulations_null_stored.RData")
# simulations_null_stored <- rbind(simulations_null_stored, simulations_null)
# simulations_null <- simulations_null_stored
# save(simulations_null_stored, file = "simulations_null_stored.RData")



# Type I error assessment -- plot & tables --------------------------------

# Overall convergence failures
simulations_null %>%
  summarise(conv_failure = sum(Warnings != ""),
            total = n(),
            conv_failure_perc = 100 * conv_failure / total)

# convergence failures by model
simulations_null %>%
  group_by(Model) %>%
  summarise(conv_failure = sum(Warnings != ""),
            total = n(),
            conv_failure_perc = 100 * conv_failure / total)

# data frame of type I error rates
type1 <- simulations_null %>%
  filter(Warnings == "") %>%  # remove convergence failures
  group_by(Model) %>%
  summarise(prop.sig = mean(p <= .05), N = n())
type1

# plot (not included in the paper)
plot_type1 <- ggplot(type1, aes(x = Model, y = prop.sig)) +
  geom_bar(stat = "identity") +
  ylab("Proportion of significant\ndifferences between types") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(vjust = 1)) +
  ggtheme 
# add horizontal line for alpha level al .05
plot_type1 + geom_hline(aes(yintercept = .05), linetype = "dashed")

# # save figures to disk (uncomment and create necessary path)
# ggsave("figures/type1-errors.pdf", width = 5, height = 4)
# ggsave("figures/type1-errors.tiff", width = 5, height = 4,
#        dpi = mydpi)



# Power simulations keeping original sample (19 languages) ----------------

# number of languages
n.lang <- 19

ptm <- proc.time()  # keep track of running time
# Simulate!
simulations <- data.frame()
for (myeffects in list(effects_low, effects_mean, effects_upper)) {
  print(myeffects)
  d.simulator <- make.data.generator(
    et, n.obs, n.lang, myeffects, resid.var,subj.ranef.covar, 
    item.ranef.covar, lang.ranef.covar)
  current <- plyr::rdply(.n = nb.sims, fit.models(d.simulator()))
  current$TrueEffect <- myeffects[2]
  simulations <- rbind(simulations, current)
}

# how long did the simulations take?
t.simulations_unbal <- proc.time() - ptm
print(t.simulations_unbal)

simulations$NbLanguages <- 19
simulations$type <- "unbalanced language sample"

head(simulations)



# Power simulations from balanced design of 20, 40 & 80 languages ---------

# number of languages
n.lang <- c(20, 40, 80)

ptm <- proc.time()  # keep track of running time
# Simulate!
simulations_bal <- data.frame()
for (mynblangs in n.lang) {
  print(paste("number of languages:", mynblangs))
  current_langs <- data.frame()
  
  for (myeffects in list(effects_low, effects_mean, effects_upper)) {
    print(myeffects)
    d.simulator <- make.balanced.data.generator (
      et, n.obs, mynblangs, myeffects, resid.var,subj.ranef.covar, 
      item.ranef.covar, lang.ranef.covar)
    current <- plyr::rdply(.n = nb.sims, fit.models(d.simulator()))
    current$TrueEffect <- myeffects[2]
    current$NbLanguages <- mynblangs
    current_langs <- rbind(current_langs, current)
  }
  simulations_bal <- rbind(simulations_bal, current_langs)
}

# how long did the simulations take?
t.simulations_bal <- proc.time() - ptm
print(t.simulations_bal)

simulations_bal$type <- "balanced language sample"

head(simulations_bal)



# Processing power simulations and saving to disk -------------------------

# put both simulations together into same dataframe
simulations_all <- rbind(simulations, simulations_bal)

# improve column names for clarity
names(simulations_all)[3:5] <- c("StdError", "z", "p")

# High-level labels for the effects 
simulations_all$Effect <- factor(simulations_all$TrueEffect, labels = c(
  "lower esimate", "mean estimate", "upper estimate"))
mylookup <- data.frame(
  Effect = factor(levels(simulations_all$Effect),levels = levels(simulations_all$Effect)),
  Probabilities = c(prob_low, prob_mean, prob_upper)
  )
simulations_all <- left_join(simulations_all, mylookup)

head(simulations_all)

## saving to disk requires tweaking if simulations are not all run at once:

# # load object cotaining previous simulations if it exists
# load("simulations_stored.RData")
# # combine with the new simulations
# simulations_stored <- rbind(simulations_stored, simulations_all)
# simulations_all <- simulations_stored
# save(simulations_stored, file = "simulations_stored.RData")



# Plot simulation results -------------------------------------------------

# proportion of convergence failures, plotted
p.conv <- simulations_all %>%
  group_by(Probabilities, Effect, NbLanguages) %>%
  summarise(runs = n(),
            fail_count = sum(Warnings != ""),
            fail_perc = mean(Warnings != "")) %>%
  ggplot(aes(x = factor(NbLanguages), y = fail_perc, fill = Effect)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Probabilities) +
  xlab("Number of languages sampled") +
  ylab("proportion of convergence failures")
p.conv + ggtheme
# # save plot to file
# ggsave("figures/power_analysis_convergence-failures.pdf", width = 6, height = 4)

# as table rather than plot; this is how it's reported in Appendix
simulations_all %>%
  group_by(NbLanguages, Effect, Probabilities) %>%
  summarise(runs = n(),
            fail_count = sum(Warnings != ""),
            fail_perc = round(100 * mean(Warnings != ""), 1)
            ) %>%
  mutate(Probabilities = gsub("\\n", " ", Probabilities)) #%>%
#   write.csv(file = "figures/convergence_failures.csv", fileEncoding = "UTF-8",
#             row.names = FALSE)


## Type II errors -- actual power analyses

# We will plot balanced design and unbalanced design separately.
# Remove convergence failures
sim_valid <- simulations_all[simulations_all$Warnings == "", ]
# unbalanced
sim_unb <- sim_valid[sim_valid$NbLanguages == 19, ] %>%
  group_by(NbLanguages, Effect, Probabilities) %>%
  summarise(prop.sig = mean(p <= .05))
sim_unb
# balanced
sim_bal <- sim_valid[sim_valid$NbLanguages != 19, ] %>%
  group_by(NbLanguages, Effect, Probabilities) %>%
  summarise(prop.sig = mean(p <= .05))
sim_bal

# plot unbalanced
p.pow_unb <- 
  ggplot(sim_unb, aes(x = factor(NbLanguages), y = prop.sig, fill = Effect)) +
  geom_bar(stat = "identity", width = .5) +
  facet_grid(~ Probabilities) +
  xlab("") +
  ylab("Proportion of significant\ndifferences between types") +
  ylim(0,1) +
  theme_bw() +
  ggtheme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 11))
p.pow_unb
# add horizontal line to show minimum desired power
p.pow_unb + geom_hline(aes(yintercept = .8), linetype = "dashed")

# # save plot to file
# ggsave("figures/power_analysis_unbalanced.pdf", width = 5, height = 2)
# ggsave("figures/power_analysis_unbalanced.tiff", width = 5, height = 2,
#        dpi = mydpi)


# plot balanced
p.pow_bal <- ggplot(sim_bal, aes(x = factor(NbLanguages), y = prop.sig,
                                 fill = Effect)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Probabilities) +
  xlab("Number of languages sampled") +
  ylab("Proportion of significant\ndifferences between types") +
  ylim(0,1) +
  theme_bw() +
  ggtheme
p.pow_bal
# add horizontal lines
p.pow_bal <- p.pow_bal + geom_hline(aes(yintercept = .8), linetype = "dashed")
# smaller font
p.pow_bal <- p.pow_bal + theme(text = element_text(size = 11))
p.pow_bal

# # save plot to file
# ggsave("figures/power_analysis_balanced.pdf", width = 5, height = 2)
# ggsave("figures/power_analysis_balanced.tiff", width = 5, height = 2,
#        dpi = mydpi)
