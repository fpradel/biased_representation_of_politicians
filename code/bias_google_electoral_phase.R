################################
# PART 3: Bias around elections
#
################################

###################
# Packages
#
###################

library("readr")
library("stringr")
library("dplyr")
library("broom")
library("ggplot2")
library("interactions")
library("dplyr")
library("broom")
set.seed(100) ## setting seed, for replication

###################
# Data
#
###################
load("../data/PrePost_election_dataset.RData")

####################
#
# Models and tables
#
# ##################

m1 = lm(personal_role ~ party + election + gender, data=autocomplete_google_electoral_phase)
m2 = lm(personal_role ~ party + election + gender + bundestag, data=autocomplete_google_electoral_phase)
m3 = lm(personal_role ~ party + gender + election + party:election, data=autocomplete_google_electoral_phase)
m4 = lm(personal_role ~ party + gender + election + party:election + bundestag, data=autocomplete_google_electoral_phase)
m5 = lm(personal_role ~  party + gender + election + party:election + bundestag + gender:election, data=autocomplete_google_electoral_phase)

library(stargazer) #hier noch bearbeiten

#results, SE
stargazer(m1,m2,m3,m4, m5, title="Examining the average representation of politicians in search engines around elections", type = "text", star.cutoffs = c(0.05, 0.01, 0.001), order = c(5, 3, 4, 2, 1, 13, 6, 15, 14, 11, 9, 10, 8, 7, 12))
#results, SE, Latex
stargazer(m1,m2,m3,m4, m5, title="Examining the average representation of politicians in search engines around elections", type = "latex", star.cutoffs = c(0.05, 0.01, 0.001), order = c(5, 3, 4, 2, 1, 13, 6, 15, 14, 11, 9, 10, 8, 7, 12))



####################
#
# Graph
#
# ##################


###https://cran.r-project.org/web/packages/dotwhisker/vignettes/dotwhisker-vignette.html
#load library
#Package preload
library(ggplot2)
library(dotwhisker)
library(broom)
library(dplyr)
library(tidyverse)
library(scales)
library(gridExtra)
library(grid)
library(quanteda)
library(utf8)
library(lme4)
library(rstudioapi)
library(dotwhisker)
library(broom)
library(dplyr)
library("effects")

autocomplete_google_electoral_phase$party=autocomplete_google_electoral_phase$party%>%recode_factor(
  'cdu/csu'="CDU/CSU",
  'afd' = "AfD",
  'fdp' = "FDP",
  'spd'="SPD",
  "grüne" = "The Greens",
  'linke' = "The Left")
#subsets
autocomplete_google_preelection=subset(autocomplete_google_electoral_phase, autocomplete_google_electoral_phase$election=="preelection")
autocomplete_google_postelection=subset(autocomplete_google_electoral_phase, autocomplete_google_electoral_phase$election=="postelection")

pred_effects_pre = lm(personal_role ~ gender + party + gender:party, data=autocomplete_google_preelection)
summary(pred_effects_pre)
black_text=element_text(size=17,  color ="gray30", family="Times")
g_pre=cat_plot(pred_effects_pre, pred = party, modx = gender,  colors =c("#14B03D", "#EB811B"),
           x.label="", y.label="") +
  geom_hline(yintercept=0, linetype="dashed", color="gray60") +
  scale_y_continuous(name="\nRole relative to personal representation", limits=c(-6, 2))+
  theme(text = black_text,  axis.text.y  = black_text, axis.text.x= black_text, legend.title=element_blank(), axis.title.x = element_text(face="plain", size=14)) + 
  coord_flip()  
g_pre


pred_effects_post = lm(personal_role ~ gender + party + gender:party, data=autocomplete_google_postelection)
summary(pred_effects_post)
black_text=element_text(size=17,  color ="gray30",family="Times")

g_pos=cat_plot(pred_effects_post, pred = party, modx = gender,  colors =c("#14B03D", "#EB811B"),
           x.label="", y.label="") +
  geom_hline(yintercept=0, linetype="dashed", color="gray30") +
  scale_y_continuous(name="\nRole relative to personal representation", limits=c(-6, 2))+
  theme(text = black_text,  axis.text.y  = black_text, axis.text.x= black_text, legend.title=element_blank(), axis.title.x = element_text(face="plain", size=14)) + coord_flip()  
g_pos