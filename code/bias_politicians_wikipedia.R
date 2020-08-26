################################
# Bias in Wikipedia
#  
#
################################

########################
##   Packages         ##    
########################
library("readr")
library("stringr")
library("dplyr")
library("broom")
library("utf8")
library("quanteda")
library("stargazer")
library("ggplot2")
library("interactions")
set.seed(100) ## setting seed, for replication


########################
##   Data             ##          
########################

load("../data/autocomplete_google_wiki.RData")

########################
#
#  Models and tables
# 
#
# #######################

##### Examining the average representation of politicians in Wikipedia
m1 = lm(personal_role ~ party, data=autocomplete_google_wiki)
m2 = lm(personal_role ~ gender, data=autocomplete_google_wiki)
m3 = lm(personal_role ~ gender + party + popular + age, data=autocomplete_google_wiki)
m4 = lm(personal_role ~ gender + party + popular + age + Liste_Platz + gender:party, data=autocomplete_google_wiki) 
m5 = lm(personal_role ~ gender + party +  popular + age + Liste_Platz + gender:party + gender:popular  + party:popular , data=autocomplete_google_wiki)

# tables, SE
stargazer(m1, m2, m3, m4, m5, title="Examining the average representation of politicians in Wikipedia", type = "text", star.cutoffs = c(0.05, 0.01, 0.001),order = c(5, 3, 4, 2, 1, 20, 8, 7, 6, 19, 17, 18, 16, 15, 14, 13, 11, 12, 10, 9))

########################
#
#  Graph
# 
#
# #######################

autocomplete_google_wiki$party=autocomplete_google_wiki$party%>%recode_factor(
  'cdu/csu'="CDU/CSU",
  'afd' = "AfD",
  'fdp' = "FDP",
  'spd'="SPD",
  "grüne" = "The Greens",
  'linke' = "The Left")

pred_effects = lm(personal_role ~ gender + party +  popular + age + Liste_Platz + gender:party, data=autocomplete_google_wiki)
black_text=element_text(size=17,  color ="gray30", family="Times")
p=cat_plot(pred_effects, pred = party, modx = gender,  colors =c("#14B03D", "#EB811B"),
           
           x.label="", y.label="") +
  geom_hline(yintercept=0, linetype="dashed", color="gray60") +
  scale_y_continuous(name="\nRole relative to personal representation", limits=c(-2, 1)) +
  theme(text = black_text,  axis.text.y  = black_text, axis.text.x= black_text, legend.title=element_blank(), axis.title.x = element_text(face="plain", size=14)) +
  coord_flip()  
p