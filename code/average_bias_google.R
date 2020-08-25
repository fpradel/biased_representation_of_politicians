
################################
# PART 1: Average bias politicans Bundestag 19
# FROM one day after the election TO most recnt time (march 2019)
# 25.09.17 - 16.03.19 
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

load("../data/autocomplete_google_average.Rdata")


########################
#
#  Models and tables
# 
#
# #######################

##### Examining the average representation of politicians in search engines

m1 = lm(personal_role ~ party, data=autocomplete_google_average)
m2 = lm(personal_role ~ gender, data=autocomplete_google_average)
m3 = lm(personal_role ~ gender + party  + age, data=autocomplete_google_average)
m4 = lm(personal_role ~ gender + party + age + gender:party, data=autocomplete_google_average) 
m5 = lm(personal_role ~ gender + party + age + gender:party + Liste_Platz, data=autocomplete_google_average) 

# table with SE
stargazer(m1,m2,m3,m4,m5, title="Examining the average representation of politicians in search engines",  star.cutoffs = c(0.05, 0.01, 0.001), order = c(5, 3, 4, 2, 1, 13, 6, 7, 12, 10, 11, 9, 8), type = "text", report=('vc*s'))
# table with SE, Latex
stargazer(m1,m2,m3,m4,m5, title="Examining the average representation of politicians in search engines",  star.cutoffs = c(0.05, 0.01, 0.001), order = c(5, 3, 4, 2, 1, 13, 6, 7, 12, 10, 11, 9, 8), type = "latex", report=('vc*s'))


########################
#  ROBUSTNESS CHECK
#  Models and tables
# 
#
# #######################

m1_r = lm(personal_role ~ gender + party + popular + age, data=autocomplete_google_average)
m2_r = lm(personal_role ~ gender + party + popular + age + gender:party, data=autocomplete_google_average) 
m3_r = lm(personal_role ~ gender + party + popular + age + gender:party + gender:popular, data=autocomplete_google_average)
m4_r = lm(personal_role ~ gender + party +  popular + age + gender:party + gender:popular  + party:popular , data=autocomplete_google_average)
m5_r = lm(personal_role ~ gender + party +  popular + age + Liste_Platz + gender:party + gender:popular  + party:popular , data=autocomplete_google_average)
m6_r = lm(personal_role ~ gender + party +  popular + age + party_leader + Liste_Platz + gender:party + gender:popular  + party:popular , data=autocomplete_google_average)

# table with SE
stargazer(m1_r, m2_r, m3_r, m4_r, m5_r, m6_r, title="Average representation of personal relative to role information: Additionally controlling for popularity and position on the party list", star.cutoffs = c(0.05, 0.01, 0.001), order = c(6, 4, 5, 3, 2, 1, 8, 10, 7, 9, 15, 13, 14, 12, 11, 16, 21, 19, 20, 18, 17, 16, 21) , type = "text")

# table with SE, Latex
stargazer(m1_r, m2_r, m3_r, m4_r, m5_r, m6_r, title="Average representation of personal relative to role information: Additionally controlling for popularity and position on the party list", star.cutoffs = c(0.05, 0.01, 0.001), order = c(6, 4, 5, 3, 2, 1, 8, 10, 7, 9, 15, 13, 14, 12, 11, 16, 21, 19, 20, 18, 17, 16, 21) , type = "latex")


########################
#  Graphs
#  Model
# 
#
# #######################
# Labels
autocomplete_google_average$party=autocomplete_google_average$party%>%recode_factor(
                                                                                      'cdu/csu'="CDU/CSU",
                                                                                      'afd' = "AfD",
                                                                                      'fdp' = "FDP",
                                                                                      'spd'="SPD",
                                                                                      "grüne" = "The Greens",
                                                                                      'linke' = "The Left")

pred_effects = lm(personal_role ~ gender + party + age + gender:party + Liste_Platz, data=autocomplete_google_average)

black_text=element_text(size=17,  color ="gray30", family="Times")
p=cat_plot(pred_effects, pred = party, modx = gender,  colors =c("#14B03D", "#EB811B"),
           
           x.label="", y.label="") +
  geom_hline(yintercept=0, linetype="dashed", color="gray30") +
  scale_y_continuous(name="\nRole relative to personal representation", limits=c(-6, 2))+ geom_vline(xintercept = 0) +
  theme(text = black_text,  axis.text.y  = black_text, axis.text.x= black_text, legend.title=element_blank(), axis.title.x = element_text(face="plain", size=14)) + 
  coord_flip()  
p



black_text=element_text(size=17,  color ="gray30", family="Times")

p=cat_plot(pred_effects, pred = party, modx = gender,  colors =c("#14B03D", "#EB811B"),
           
           x.label="", y.label="") +
  geom_hline(yintercept=0, linetype="dashed", color="gray60") +
  scale_y_continuous(name="\nRole relative to personal representation", limits=c(-6, 2))+ geom_vline(xintercept = 0) +
  theme(text = black_text,  axis.text.y  = black_text, axis.text.x= black_text, legend.title=element_blank(), axis.title.x = element_text(face="plain", size=14)) + 
  coord_flip()  
p
