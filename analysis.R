## Main analyses for COSE paper

library(lme4)
library(dplyr)
library(ggplot2)

# Import data -------------------------------------------------------------

# data for event triads
et <- read.csv("data_triads.csv", encoding = "UTF-8")



# Global plotting parameters ----------------------------------------------

# defined globally in separate script
source("global_parameters.R")


# Prepare data for model fitting ------------------------------------------

contrasts(et$LanguageType) <- matrix(
  c(1,-1), nrow=2, ncol=1, byrow=F, 
  dimnames = list(c("sattelite-framed", "verb-framed"), c("SatteliteVsVerb"))
  )


# Analysis 1: S- vs V-framed languages ------------------------------------

# Prediction: Recall that our hypothesis was that speakers of the seven 
# S-framed languages would be relatively more likely to go for same-manner
# variants compared to the speakers of the V-framed languages.
# --> positive coef for Sattelite vs. Verb-framed languages.


# random effects for language, participants (crossing language), item scene and
# specific item (ItemWithinScene)
# This is the model reported in the paper.
fm_an1 <- glmer(SameMannerResponse ~ 
              LanguageType +
              (1 | language) +
              (1 | Participant) +
              (1 | ItemScene) +
              (1 | ItemWithinScene),
            data = et, family = "binomial"
            )
summary(fm_an1)


# Note that a nested (hierarchical) model yields identical results:
# Random effects for language, participants *nested* within language, item
# scene and specific scene (within item scene)
fm_an1_nested <- glmer(SameMannerResponse ~ 
                         LanguageType +
                         (1 | language/Participant) +
                         (1 | ItemScene) +
                         (1 | ItemWithinScene),
                       data = et, family = "binomial"
)
summary(fm_an1_nested)



# Plot results analysis 1 -------------------------------------------------

# We will plot by-participant averages and by-language confidence intervals
et_bs <- et %>%
  group_by(Participant, language, LanguageType) %>%
  summarise(SameManner = mean(SameMannerResponse))
  
# Order languages by average SameMannerResponse
manner_language <- et %>%
  group_by(language) %>%
  summarise(M = mean(SameMannerResponse))
ordered_langs <- manner_language[order(manner_language$M), ][["language"]]
# reorder factor levels
et_bs$language <- factor(et_bs$language, levels = ordered_langs)
rm(manner_language, ordered_langs)

# plot with jitter
plot_main <- ggplot(
  et_bs, aes(x = language, y = SameManner, color = LanguageType,
             shape = LanguageType, linetype = LanguageType)) +
  geom_jitter(position = position_jitter(width = .75, height = 0),
              alpha = .5, size = 1.5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = .75, width = .75) +
  xlab("Language") +
  ylab("Proportion of same-manner responses") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(vjust = 1), legend.position = "top") 
plot_main
# compute by-type means
type_mean <- et %>%
  group_by(LanguageType) %>%
  summarise(M = mean(SameMannerResponse))
# add them to plot
plot_main <- plot_main + 
  geom_hline(data = type_mean, aes(yintercept = M, colour = LanguageType,
                                   linetype = LanguageType), size = 1)
set.seed(564)  # make plot reproducible
plot_main + ggtheme

# save to disk
ggsave("figures/manner-by-language.pdf", width = 7, height = 5)
ggsave("figures/manner-by-language.tiff", width = 7, height = 5, dpi = mydpi)



# Analysis 2: Effect of first choice on subsequent trials -----------------

# We can investigate the effect of response on the first trial (Manner vs Path)
# on the remainder of the experiment.That the first response is strongly 
# predictive of later responses is expected if participants are overall rather
# consistent (low within-participant variability)

# It is of theoretical interest to know whether response on the first
# trial interacted with language type. We need to centre the predictors
# to do that (this is done in the model formula).
# Also, remove all responses to first trials from the data, because otherwise
# we are adding undesired redundancy to the data set:
et_tr1 <- et[et$TargetTrial != 1, ]

# now fit model
fm_an2_trial1 <- glmer(SameMannerResponse ~
                         LanguageType *
                         scale(FirstTrial.SameMannerResponse, scale = F) +
                         (1 | language) +
                         (1 | Participant) +
                         (1 | ItemScene) +
                         (1 | ItemWithinScene),
                       data = et_tr1, family = "binomial"
                       )
summary(fm_an2_trial1)
# No, there is no such interaction.



# Plot results analysis 2 -------------------------------------------------


# better and shorter variable name for plotting
et_tr1$FirstTrial <- ifelse(et_tr1$FirstTrial.SameMannerResponse == 1,
                            "Same-manner", "Same-path")

# compute by-subject averages
et_tr1_bs <- et_tr1 %>%
  group_by(LanguageType, Participant, FirstTrial) %>%
  summarise(MannerChoice = mean(SameMannerResponse))

# There is one row with missing values, remove it
sum(!complete.cases(et_tr1_bs))
et_tr1_bs <- et_tr1_bs[complete.cases(et_tr1_bs), ]

# Plot participants as points and 95% bootstrapped C.I.
plot_trial1 <- ggplot(et_tr1_bs, aes(x = FirstTrial, y = MannerChoice)) +
  geom_jitter(position = position_jitter(width = .2, height = .025),
              size = 1, alpha = .3, ) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1,
               width = .5) +
  xlab("Choice on first trial") + ylab("Proportion of\nsame-manner responses") +
  theme_bw() +
  theme(axis.title.y=element_text(vjust = 1)) +
  ggtheme
set.seed(297)  # make plot reproducible
plot_trial1

# save to disk
ggsave("figures/first-trial-effect.pdf", width = 3, height = 3.5)
ggsave("figures/first-trial-effect.tiff", width = 3, height = 3.5, dpi = mydpi)
