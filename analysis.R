## This script replicates the main analyses reported in the article:
## Montero-Melis, G., Eisenbeiss, S., Narasimhan, B., Ibarretxe-Antuñano, I.,
## Kita, S., Kopecka, A., ... Bohnemeyer, J. (accepted). Talmy’s framing 
## typology underpredicts nonverbal motion categorization: Insights from a 
## large language sample and simulations. Cognitive Semantics.

library(lme4)
library(dplyr)
library(ggplot2)

# Import data -------------------------------------------------------------

# data for event triads
et <- read.csv("data_triads.csv", encoding = "UTF-8")

# use contrast coding for LanguageType
contrasts(et$LanguageType) <- contr.sum(2)
colnames(contrasts(et$LanguageType)) <- "Satellite_vs_Verb"
contrasts(et$LanguageType)


# Global plotting parameters ----------------------------------------------

# defined globally in separate script
source("global_parameters.R")


#  ------------------------------------------------------------------------
#  S- versus V-framed languages
#  ------------------------------------------------------------------------

# Prediction: Recall that our hypothesis was that speakers of the seven 
# S-framed languages would be relatively more likely to go for same-manner
# variants compared to the speakers of the V-framed languages.
# --> positive coef for Satellite vs. Verb-framed languages.


# random effects for language, participants (crossing language), item scene and
# specific item (ItemWithinScene)
# This is the model reported in the paper.
fm_main <- glmer(SameMannerResponse ~ 
              LanguageType +
              (1 | Language) +
              (1 | Participant) +
              (1 | ItemScene) +
              (1 | ItemWithinScene),
            data = et, family = "binomial"
            )
summary(fm_main)



# Plot results analysis 1 -------------------------------------------------

# We will plot by-participant averages and by-language confidence intervals
et_bs <- et %>%
  group_by(Participant, Language, LanguageType) %>%
  summarise(SameManner = mean(SameMannerResponse))
  
# Order languages by average SameMannerResponse
manner_language <- et %>%
  group_by(Language) %>%
  summarise(M = mean(SameMannerResponse))
ordered_langs <- manner_language[order(manner_language$M), ][["Language"]]
# reorder factor levels
et_bs$Language <- factor(et_bs$Language, levels = ordered_langs)
rm(manner_language, ordered_langs)

# plot with jitter
plot_main <- ggplot(et_bs, aes(x = Language, y = SameManner, color = LanguageType,
                               shape = LanguageType, linetype = LanguageType)) +
  geom_jitter(position = position_jitter(width = .75, height = 0),
              alpha = .4, size = 1.5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = .75, width = .8) +
  ylab("Proportion of\nsame-manner responses")
plot_main
# compute by-type means
type_mean <- et %>%
  group_by(LanguageType) %>%
  summarise(M = mean(SameMannerResponse))
# add them to plot
plot_main <- plot_main + 
  geom_hline(data = type_mean, aes(yintercept = M, colour = LanguageType,
                                   linetype = LanguageType), size = .5)
# add themes
plot_main <- plot_main + ggtheme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(vjust = 1), legend.position = "top")
set.seed(562)  # make plot reproducible
plot_main

# # save figures to disk (uncomment and create necessary path)
# set.seed(562)
# ggsave("figures/manner-by-language.pdf", width = 6, height = 3)
# set.seed(562)
# ggsave("figures/manner-by-language.tiff", width = 6, height = 3, dpi = mydpi)



# Analysis 2: Effect of first choice on subsequent trials -----------------

# We can investigate the effect of response on the first trial (Manner vs Path)
# on the remainder of the experiment.That the first response is strongly 
# predictive of later responses is expected if participants are overall rather
# consistent (low within-participant variability)

# It is of theoretical interest to know whether response on the first
# trial interacted with language type. We need to centre the predictors
# to do that (this is done in the model formula).

# But first remove all responses to first trials from the data, because
# otherwise we are adding undesired redundancy to the data set:
et_tr1 <- et[et$TargetTrial != 1, ]

# now fit model
fm_trial1 <- glmer(SameMannerResponse ~
                         LanguageType *
                         scale(FirstTrial.SameMannerResponse, scale = F) +
                         (1 | Language) +
                         (1 | Participant) +
                         (1 | ItemScene) +
                         (1 | ItemWithinScene),
                       data = et_tr1, family = "binomial"
                       )
summary(fm_trial1)
# No, there is no interaction between LanguageType and effect of first choice.

# better and shorter variable name for plotting and summarising results
et_tr1$FirstTrial <- ifelse(et_tr1$FirstTrial.SameMannerResponse == 1,
                            "Same-manner", "Same-path")

# Note that for participant ru.4 no info is available about first trial:
et_tr1[et_tr1$Participant == "ru.4", ]
# remove participant for this analysis
et_tr1 <- et_tr1[et_tr1$Participant != "ru.4", ]

# In the paper we offer some descriptives of the effect of first trial choice:
et_tr1 %>%
  group_by(FirstTrial) %>%
  summarise(MannerChoices = round(mean(SameMannerResponse, na.rm = TRUE), 2))


# Plot effect of trial 1 (not included in paper) --------------------------

# compute by-subject averages
et_tr1_bs <- et_tr1 %>%
  group_by(LanguageType, Participant, FirstTrial) %>%
  summarise(MannerChoice = mean(SameMannerResponse))

# Plot participants as points and 95% bootstrapped C.I.
plot_trial1 <- ggplot(et_tr1_bs, aes(x = FirstTrial, y = MannerChoice)) +
  geom_jitter(position = position_jitter(width = .2, height = .025),
              size = 1, alpha = .3, ) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1,
               width = .5) +
  xlab("Choice on first trial") + ylab("Proportion of\nsame-manner responses") +
  ggtheme
set.seed(297)  # make plot reproducible
plot_trial1

# # save to disk
# ggsave("figures/first-trial-effect.pdf", width = 3, height = 3.5)
# ggsave("figures/first-trial-effect.tiff", width = 3, height = 3.5, dpi = mydpi)
