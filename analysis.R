## Main analyses for COSE paper

library(lme4)
library(dplyr)
library(ggplot2)

# Import data -------------------------------------------------------------

# data for event triads
et <- read.csv("data_triads.csv", encoding = "UTF-8")


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



# Plot analysis 1 ---------------------------------------------------------

# We will plot by-participant averages and language confidence intervals
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
langplot <- ggplot(
  et_bs, aes(x = language, y = SameManner, color = LanguageType,
             shape = LanguageType, linetype = LanguageType)) +
  geom_jitter(alpha = .4, size = 2, position = position_jitter(width = .75,
                                                               height = 0)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(vjust = 1), legend.position = "top") 
langplot
# one might want to add by-type means
type_mean <- et %>%
  group_by(LanguageType) %>%
  summarise(M = mean(SameMannerResponse))
langplot <- langplot + geom_hline(
  data = type_mean, aes(yintercept = M, colour = LanguageType,
                        linetype = LanguageType), size = 1)
langplot
# and suitable axis labels
langplot <- langplot + 
  xlab("Language") +
  ylab("Proportion of same-manner responses")
langplot

ggsave("figures/manner-by-language.png", width = 7, height = 5)



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











### Recycle code below as needed, delete the rest


################## beginning GMM edits

## Let's plot the effect of response to the first trial
# on the remainder of the experiment.
library(ggplot2)

theme_set(theme_bw(base_size=14))

# simplify the data frame
trial1 <- d.sub[, c("language", "SameMannerResponse", "LanguageType", "Participant",
                    "TrialOrder", "LanguageCommunity", "FirstTrial.SameMannerResponse")]
# better and shorter variable name for plotting
trial1$FirstTrial <- ifelse(trial1$FirstTrial.SameMannerResponse == 1, "same-manner", "same-path")
trial1$FirstTrial.SameMannerResponse <- NULL
# now remove all first trial observations from data (TrialOrder == 5)
trial1 <- trial1[trial1$TrialOrder != 5, ]
# compute by-subject averages, keeping info about language, language type, etc
tr1_bs <- ddply(trial1, .(language, LanguageType, Participant, LanguageCommunity,
                          FirstTrial), summarise,
                MannerChoice = mean(SameMannerResponse))

# first check if Language Type seems to play a role here, i.e. whether it
# interacts with effect of choice on first trial
ggplot(tr1_bs, aes(x = FirstTrial, y = MannerChoice, colour = LanguageType)) +
  geom_boxplot()
# really doesn't seem like it does, so let's just plot individual points and C.I.
ggplot(tr1_bs, aes(x = FirstTrial, y = MannerChoice)) +
  geom_jitter(size = 2, alpha = .4, position = position_jitter(width = .2, height = .01)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1.5, width = .5) +
  xlab("choice on first trial") + ylab("proportion\nof same-manner responses") +
  theme(axis.title.y=element_text(vjust = 1)) 

# save to disk
ggsave("figures/first-trial-effect.png", width = 3.25, height = 3.5)

################## end GMM edits



# a simplified model to investigate the variance by participants within verb- and 
# satellite-framed languages.
g4 = glmer(SameMannerResponse ~ 
            LanguageType +
            (1 | Participant),
          d.sub, family = "binomial"
)
summary(g4)

g4a = glmer(SameMannerResponse ~ 
             1 +
             (1 | Participant),
           d.sub, family = "binomial", subset = LanguageType == "verb-framed"
)
g4b = glmer(SameMannerResponse ~ 
             1 +
             (1 | Participant),
           d.sub, family = "binomial", subset = LanguageType != "verb-framed"
)
summary(g4a)
summary(g4b)

library(plyr)
library(ggplot2)

contrasts(d.sub$LanguageType) = matrix(c(1,-1), nrow=2, ncol=1, byrow=F, 
                                       dimnames= list(c("sattelite-framed", "verb-framed"), c("SatteliteVsVerb"))
)

dd = ddply(d.sub, 
           .(Participant, language, LanguageType, LanguageCommunity),
           summarise,
           SameMannerResponse = mean(SameMannerResponse))
# (GMM) at least for plotting we want the language names to be transparent
dd$language <-
  revalue(dd$language, 
          c("ba" = "Basque", "ca" = "Catalan", "du" = "Dutch", "en" = "English",
            "es" = "Estonian", "Fr" = "French", "ge" = "German", "hi" = "Hindi",
            "it" = "Italian", "ja" = "Japanese", "jo" = "Jalonke", "la" = "Lao",
            "po" = "Polish", "ru" = "Russian", "sp" = "Spanish", "ta" = "Tamil",
            "ti" = "Tidore", "tu" = "Turkish", "ty" = "Tiriyo", "yu" = "Yukatek"))
theme_set(theme_bw(base_size=18))
ggplot(dd, aes(x = language, y = SameMannerResponse, color = LanguageType)) +
  geom_hline(yintercept = mean(dd$SameMannerResponse), color = "gray") +
  geom_point(alpha = .4, size = 3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################## GMM edits starts
# (GMM) order languages by average SameMannerResponse
lang_ord <- ddply(dd, .(language), summarise, M = mean(SameMannerResponse))
myorder <- lang_ord[order(lang_ord$M), "language"]
dd$language <- factor(dd$language, levels = myorder)
rm(lang_ord, myorder)

theme_set(theme_bw(base_size=14))
# plot with jitter
langplot <- ggplot(dd, aes(x = language, y = SameMannerResponse, 
                           color = LanguageType, shape = LanguageType,
                           linetype = LanguageType)) +
  geom_jitter(alpha = .4, size = 2, 
              position = position_jitter(width = .75, height = 0)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(vjust = 1), legend.position = "top") 
#   legend.justification=c(0,1), legend.position=c(0,1),
#         legend.background = element_rect(fill="gray95"))
langplot
# one might want to add by-type means
type_mean <- ddply(d.sub, .(LanguageType), summarise, M = mean(SameMannerResponse))
langplot <- langplot + geom_hline(data = type_mean, 
                                  aes(yintercept = M, colour = LanguageType,
                                      linetype = LanguageType), size = 1)
langplot
# and suitable y-axis label
langplot <- langplot + ylab("Proportion of same-manner responses")
langplot

ggsave("figures/manner-by-language.png", width = 7, height = 5)



## Language community: small/non-standardized vs. large/standardized
# see also analyses below towards end of script

# plot with jitter
langsizeplot <- ggplot(dd, aes(x = language, y = SameMannerResponse,
                               color = LanguageCommunity, shape = LanguageCommunity,
                               linetype = LanguageCommunity)) +
  geom_jitter(alpha = .4, size = 2, 
              position = position_jitter(width = .7, height = .01)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(vjust = 1), legend.position = "top") +
#         legend.justification=c(0,1), legend.position=c(0,1),
#         legend.background = element_rect(fill="gray95")) +
  scale_color_brewer(palette="Dark2")
langsizeplot
# one might want to add by-type means
commun_mean <- ddply(d.sub, .(LanguageCommunity), summarise, M = mean(SameMannerResponse))
langsizeplot <- langsizeplot + geom_hline(data = commun_mean, 
                                  aes(yintercept = M, colour = LanguageCommunity),
                                  linetype = "dashed", size = 1)
langsizeplot
# and suitable y-axis label
langsizeplot <- langsizeplot + ylab("Proportion of same-manner responses")
langsizeplot

ggsave("figures/manner-by-language_size.png", width = 6.5, height = 5)

################## end GMM edits

# simple random effect structure plus covariates
contrasts(d.sub$tar_path) = contr.sum(2)
contrasts(d.sub$tar_man) = contr.sum(4)
contrasts(d.sub$path_ch) = contr.sum(2)
contrasts(d.sub$man_ch) = contr.sum(2)

g1a = glmer(SameMannerResponse ~ 
	LanguageType +
	tar_path *
	tar_man +
	(1 | ItemScene) +
	(1 | ItemWithinScene),
	d.sub, family = "binomial"
)

g2a = glmer(SameMannerResponse ~ 
	LanguageType +
	tar_path *
	tar_man +
	(1 | language) +
	(1 | ItemScene) +
	(1 | ItemWithinScene),
	d.sub, family = "binomial"
)

g2a2 = glmer(SameMannerResponse ~ 
              LanguageType +
              tar_path *
              tar_man * 
              man_ch +
              (1 | language) +
              (1 | ItemScene) +
              (1 | ItemWithinScene),
            d.sub, family = "binomial"
)
summary(g1a)
summary(g2a)
summary(g2a2)
## ------------------------------------------------------------------------------
#
# Community size / Standardization
#
# Most lgs in our sample come from large communities with standardized language. 
# Standardization might make participants' responses less variable. At the same time
# larger lg communities (where individuals are not in constant contact with each other)
# might have more variability between participants, compared to data from participants
# from small lg communities (especially if they were collected under the typical field
# work setting, all from one village).
# 
## ------------------------------------------------------------------------------

g5 = glmer(SameMannerResponse ~ 
             LanguageCommunity +
             (1 | Participant),
           d.sub, family = "binomial"
)
summary(g5)

# Add same random effects structure as above
g5a = glmer(SameMannerResponse ~ 
              scale(as.numeric(LanguageCommunity), scale = F) +
              (1 | language) +
              (1 | Participant) +
              (1 | ItemScene) +
              (1 | ItemWithinScene),
            d.sub, family = "binomial"
)
summary(g5a)

# add LanguageType and interaction as fixed effects
g5b = glmer(SameMannerResponse ~ 
              scale(as.numeric(LanguageCommunity), scale = F) *
              LanguageType +
              (1 | language) +
              (1 | Participant) +
              (1 | ItemScene) +
              (1 | ItemWithinScene),
            d.sub, family = "binomial"
)
summary(g5b)


g5_large_standard = glmer(SameMannerResponse ~ 
              1 +
              (1 | Participant),
            d.sub, family = "binomial", subset = LanguageCommunity == "large/standardized"
)
g5_small_nonstandard = glmer(SameMannerResponse ~ 
              1 +
              (1 | Participant),
            d.sub, family = "binomial", subset = LanguageCommunity != "large/standardized"
)
summary(g5_large_standard)
summary(g5_small_nonstandard)

# effects of language type? 
g6_large_standard = glmer(SameMannerResponse ~ 
                            LanguageType +
                            (1 | Participant),
                          d.sub, family = "binomial", subset = LanguageCommunity == "large/standardized"
)
g6_small_nonstandard = glmer(SameMannerResponse ~ 
                               LanguageType +
                               (1 | Participant),
                             d.sub, family = "binomial", subset = LanguageCommunity != "large/standardized"
)
summary(g6_large_standard)
summary(g6_small_nonstandard)
