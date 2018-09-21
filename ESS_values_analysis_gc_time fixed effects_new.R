###############################################################################################################
### Analysis of ESS 2002-2016 round for the relationship of Self-enhancement and political beliefs
### ESS all rounds
### Analysis for data centered around a grand mean [vars with _gc suffix are grand mean centered]

### Updates:
### 20-06-2018
### - new version of ESS2016 added (new countries)
### - new weights applied: pspweight * population size (as recommended by ESS committee)
### - random slopes for Self-enhancement added
### - dummy variable of ESS round added as an interaction term with Self-enhancement variable
###
### 4-07-2018
### - analysis for self-transcendence instead of Self-enhancement [important for economic dimension especially]
### - IMPORTANT: the weights for self-tran needs to bechecked (and mayb again needs to be calculated)
### becasue it was calculated on the original ESS responses (1=like me, 6=not like me) and then weighted by
### Weights of Verkasolo et al (2009) so it represents SELF-ENHANCEMENT on positive extreme (not self-trans as
### plots suggests)
###
### 9-07-2018
### - new analysis for the Eastern countries: typology of Fenger (2007)
###
### 10-7-2018
### - data is prepared for another time: self-enh and self-trans are now correctly calculated
### - the analysis results are the same as before but re-calculate to make sure it's correct
###
### 10-08-2018
### - the analysis is run for BOTH openness and self-enhancement in one model!
###############################################################################################################

rm(list = ls())
cat("\014")
library(dplyr)
library(scales)
library(effects)
library(lme4)
library(lmerTest)
library(multcomp)
library(emmeans)
library(sjPlot)
library(sjstats)
library(Hmisc)
library(lsmeans)
library(multcompView)
options(scipen=999)#non-scientific notation

library(doSNOW)
set.seed(666)
c1 <- makeCluster(4,type="SOCK")
registerDoSNOW(c1)

setwd("C:/Users/gczar_000/Documents/_ESS")
#setwd("C:/Users/Gabriela/Dropbox/_PROJECTS/SURVEYS/European Social Survey/ESS all")

###########################################################################################################
### Read data & grand-mean center variables in the dataset (with indexes calculated) ###
###############################################################################################################
### Read data
data = read.table("ESS_2002-2016.csv", header=TRUE, sep=";", dec=".")

# Change wd to self-trans directory
setwd("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh")

# !!!remove cases with NA on Self-enhancement variable
data = subset(data, !is.na(data$openess))
data = subset(data, !is.na(data$self_enh))
# and demographic variables
#data = subset(data, !is.na(data$gndr_01))
#data = subset(data, !is.na(data$agea_s))
#data = subset(data, !is.na(data$eduyrs_s))
# summary stats
#x = as.data.frame.matrix(table(data$cntry, data$essround))
#write.csv(x, "ESS country_wave cohorts.csv", row.names = TRUE, sep = ";")

### Center vars with grand mean 
# Beliefs
# left-right self-placement
data$lrscale_s_gc = scale(data$lrscale_s, center = TRUE, scale = FALSE)
hist(data$lrscale_s_gc)
# cultural beliefs
data$polit_cult_gc = scale(data$polit_cult, center = TRUE, scale = FALSE)
hist(data$polit_cult_gc)
# economic beliefs
data$polit_econ_gc = scale(data$polit_econ, center = TRUE, scale = FALSE)
hist(data$polit_econ_gc)
### Values
# self-enhancement value
data$self_enh_s_gc = scale(data$self_enh_s, center = TRUE, scale = FALSE)
hist(data$self_enh_s_gc)
colMeans(data["self_enh_s_gc"],na.rm=TRUE)
# conservation value
data$conservation_s_gc = scale(data$conservation_s, center = TRUE, scale = FALSE)
hist(data$conservation_s_gc)
# self-transcendence value
data$self_trans_s_gc = scale(data$self_trans_s, center = TRUE, scale = FALSE)
hist(data$self_trans_s_gc)
colMeans(data["self_trans_s_gc"],na.rm=TRUE)
# openess value
data$openess_s_gc = scale(data$openess_s, center = TRUE, scale = FALSE)
hist(data$openess_s_gc)

### ???Do i need to center those variables? (I don't think so)
# age
hist(data$agea_s)
# educ
hist(data$eduyrs_s)
# interest in politics
hist(data$polintr_r_s)

### Change class of vars
data$essround = as.factor(data$essround)
data$west_east = as.factor(data$west_east)# 0-West, 1-East Europe
data$cntry = as.factor(data$cntry)
data$polintr_r_s = as.factor(data$polintr_r_s)# 0-no interest, 4-very much interest
data$age_cat = as.factor(data$age_cat)# 0-young (born after 1977), 1-older (born before 1977)

# Check NA
sum(is.na(data$west_east))
sum(is.na(data$cntry))

# Check frequencies
summary(data$essround)
summary(data$cntry)
(table(data$cntry, data$essround))
#write.table(table, "ESS_countries.csv", row.names = TRUE, col.names = TRUE)

#################################################################################################
### Instead of East vs. West: Welfare state classification based on Ferrera  ###

data$welstate <- recode(data$cntry,
                        "SE" = 1, "NO" = 1, "DK" = 1, "FI" = 1, "IS" = 1, # nordic (IS added - it was not in the classification)
                        "IE" = 2, "GB" = 2, # liberal 
                        "NL" = 3,"LU" = 3, "DE" = 3, # contitental:  # poprawione: teraz zgadza si? z tabel?
                        # komentarz wcze?niejszy: do sprawdzenia!!! Kod z spss nie zgadza si? z tabelk?: http://essedunet.nsd.uib.no/cms/topics/multilevel/ch5/2.html
                        "CH" = 3, "BE" = 3,"AT" = 3, "FR"=3, # continental
                        "ES" = 4,"IL" = 4,"IT" = 4, "GR" = 4, "TR" = 4,"PT" = 4, "CY"=4, # southern
                        "RU" = 5, "EE" = 5, "BG" = 5,"CZ" = 5,"PL" = 5,"HR" = 5, "HU" = 5,"LV" = 5,"RO" = 5,"SI" = 5,"SK" = 5,"UA" = 5, "LT" = 5, "AL" = 5, "XK" = 5)# eastern (LR, AL, and XK - added, it wasn't in the classification) 

# make a copy of the variable
data$welstate2 <- factor(data$welstate)

# make second category a one for comparison 
# edit: make 3rd category a one for comparisons
data$welstate <- factor(data$welstate,
                        levels = c(3,2,1,4,5),
                        labels = c("Continental","Liberal","Nordic","Southern","Eastern"))
summary(data$welstate)
table(data$welstate2,data$cntry)

### Instead of East vs. West: Welfare state classification based on Fenger: 3 types of Eastern countries ###

data$welstateEast3 <- recode(data$cntry,
                             "SE" = 1, "NO" = 1, "DK" = 1, "FI" = 1, "IS" = 1, # nordic (IS added - it was not in the classification)
                             "IE" = 2, "GB" = 2, # liberal 
                             "NL" = 3,"LU" = 3, "DE" = 3, # contitental:  # poprawione: teraz zgadza si? z tabel?
                             # komentarz wcze?niejszy: do sprawdzenia!!! Kod z spss nie zgadza si? z tabelk?: http://essedunet.nsd.uib.no/cms/topics/multilevel/ch5/2.html
                             "CH" = 3, "BE" = 3,"AT" = 3, "FR"=3, # continental
                             "ES" = 4,"IL" = 4,"IT" = 4, "GR" = 4, "TR" = 4,"PT" = 4, "CY"=4, # southern
                             # new classification of welfare stare for Eastern countries
                             "BG" = 6,"CZ" = 6,"PL" = 6,"HR" = 6, "HU" = 6, "SI" = 6,"SK" = 6,# Post-communist European 
                             "RU" = 7, "LV" = 7, "UA" = 7, "LT" = 7, "EE" = 7, # Former USSR countries
                             "RO" = 8, "AL" = 8, "XK" = 8)# Developing welfare 

data$welstateEast3 <- factor(data$welstateEast3,
                             levels = c(3,2,1,4,6,7,8),
                             labels = c("Continental","Liberal","Nordic","Southern",
                                        "Post-com European","Former USSR","Developing"))
summary(data$welstateEast)
table(data$welstate2,data$cntry)


###############################################################################################################
### Y: left-right self-placement ###
###############################################################################################################

setwd("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh")

### X: 
# openness + self_enh + essround, no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_r0 = lmer(data = data, lrscale_s ~ 
                 openess_s_gc + 
                 self_enh_s_gc +
                 essround +
                 gndr_01 + agea_s + eduyrs_s +
                 (1 + openess_s_gc + self_enh_s_gc|cntry/essround), 
               REML = FALSE, 
               weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_r0, "m_vl_r0.rds") # model
#saveRDS(summary(m_vl_r0), "m_vl_r0.rds") # model summary
readRDS("m_vl_r0.rds")


### X: 
# openess *  essround
# self_enh *  essround 
# no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_r0_1 = lmer(data = data, lrscale_s ~ 
                   openess_s_gc * essround +
                   self_enh_s_gc * essround +
                   gndr_01 + agea_s + eduyrs_s +
                   (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                 REML = FALSE, 
                 weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_r0_1, "m_vl_r0_1.rds") # model
#saveRDS(summary(m_vl_r0_1), "m_vl_r0_1.rds") # model summary
readRDS("m_vl_r0_1.rds")


### X:
# openess * west_est 
# self_enh * west_est 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_r1 = lmer(data = data, lrscale_s ~ 
                 openess_s_gc * west_east +
                 self_enh_s_gc * west_east +
                 essround +
                 gndr_01 + agea_s + eduyrs_s +
                 (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
               REML = FALSE, 
               weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_r1, "m_vl_r1.rds") # model
#saveRDS(summary(m_vl_r1), "m_vl_r1.rds") # model summary
readRDS("m_vl_r1.rds")

### X: 
# openess * west_est * essround
# self_enh * west_est * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_r1_1 = lmer(data = data, lrscale_s ~ 
                   openess_s_gc * west_east * essround +
                   self_enh_s_gc * west_east * essround +
                   gndr_01 + agea_s + eduyrs_s +
                   (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                 REML = FALSE, 
                 weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_r1_1, "m_vl_r1_1.rds") # model
#saveRDS(summary(m_vl_r1_1), "m_vl_r1_1.rds") # model summary
readRDS("m_vl_r1_1.rds")


###############################
### X: 
# openess * welfare state 
# self_enh * welfare state 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_r2 = lmer(data = data, lrscale_s ~ 
                 openess_s_gc * welstate +
                 self_enh_s_gc * welstate +
                 essround +
                 gndr_01 + agea_s + eduyrs_s +
                 (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
               REML = FALSE, 
               weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_r2, "m_vl_r2.rds") # model
#saveRDS(summary(m_vl_r2), "m_vl_r2.rds") # model summary
readRDS("m_vl_r2.rds")

### X: 
# openess * welfare state  * essround
# self_enh * welfare state  * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_r2_1 = lmer(data = data, lrscale_s ~ 
                   openess_s_gc * welstate * essround +
                   self_enh_s_gc * welstate * essround +
                   gndr_01 + agea_s + eduyrs_s +
                   (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                 REML = FALSE, 
                 weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_r2_1, "m_vl_r2_1.rds") # model
#saveRDS(summary(m_vl_r2_1), "m_vl_r2_1.rds") # model summary
readRDS("m_vl_r2_1.rds")

##############################################################################
### Compare models' fit for the cultural beliefs ###

# Compare m_vl_r0 with m_vl_r0_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_r0, m_vl_r0_1), "m_vl_r0_anova.rds")
readRDS("m_vl_r0_anova.rds")

# Compare m_vl_r1 with m_vl_r1_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_r1, m_vl_r1_1), "m_vl_r1_anova.rds")
readRDS("m_vl_r1_anova.rds")

### Compare models with west_east vs. welstate 
# with essround main effect
saveRDS(anova(m_vl_r1, m_vl_r2), "m_vl_r1_vs_r2_anova.rds")
readRDS("m_vl_r1_vs_r2_anova.rds")
# with essround interaction effect
saveRDS(anova(m_vl_r1_1, m_vl_r2_1), "m_vl_r1_1_vs_r2_1_anova.rds")
readRDS("m_vl_r1_1_vs_r2_1_anova")
# Compare m_vl_r2 with m_vl_r2_1 (ess round main effect or interaction) -> THIS ONE IS SAVED
saveRDS(anova(m_vl_r2, m_vl_r1_1), "m_vl_r2_vs_1_1_anova.rds")
readRDS("m_vl_r2_vs_1_1_anova.rds")
# Compare m_vl_r2 with m_vl_r2_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_r2, m_vl_r2_1), "m_vl_r2_anova.rds")
readRDS("m_vl_r2_anova.rds")

### Compare all models for cultural beliefs

saveRDS(anova(m_vl_r0,m_vl_r1,m_vl_r2,
              m_vl_r0_1,m_vl_r1_1,m_vl_r2_1), 
        "m_vl_r_all.rds")
readRDS("m_vl_r_all.rds")


write.table(anova(m_vl_r0,m_vl_r1,m_vl_r2,
                  m_vl_r0_1,m_vl_r1_1,m_vl_r2_1), 
            "anova_vl_r.csv", sep=";", dec=",")

rm(m_vl_r0)
rm(m_vl_r0_1)
rm(m_vl_r1)
rm(m_vl_r1_1)
rm(m_vl_r2)
rm(m_vl_r2_1)

##############################################################################
### Charts with slopes of fixed effects

### m1_1
# openness -> ZMIENI? WYKRES ?EBY MIA? OSOBNE KAFELKI NA OSOBNE ESSROUND
png("m_vl_r1_1_open.png")
plot(effect(term = "openess_s_gc:west_east:essround", 
            mod = m_vl_r1_1, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x West vs. East ",
     xlab = "Openness",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

sjt.lmer(m_vl_r1_1)

# self-enh -> TO NIE DZIA?A!!! SPRAWDI? DLACZEGO, edit 31-08-2018: dzia?a - chodzi o kolejno?? zmiennych
png("m_vl_r1_1_enh.png")
plot(effect(term = "west_east:essround:self_enh_s_gc", # taka kolejno?c zmiennych konieczna
            mod = m_vl_r1_1, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x West vs. East ",
     xlab = "Self-enhancement",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### m2
# openness
png("m_vl_r2_open.png")
plot(effect(term = "openess_s_gc:welstate", 
            mod = m_vl_r2, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x Welfare state ",
     xlab = "Openness",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

# self-enh -> TO NIE DZIA?A!!! SPRAWDI? DLACZEGO edit 31-08-2018: dzia?a - chodzi o kolejno?? zmiennych
png("m_vl_r2_s_enh.png")
plot(effect(term = "welstate:self_enh_s_gc", 
            mod = m_vl_r2, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x Welfare state ",
     xlab = "Self-enhancement",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

###############################################################################################################
### Models for cultural beliefs ###
###############################################################################################################

setwd("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh")

### X: 
# openness + self_enh + essround, no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_cult0 = lmer(data = data, polit_cult ~ 
                    openess_s_gc + 
                    self_enh_s_gc +
                    essround +
                    gndr_01 + agea_s + eduyrs_s +
                    (1 + openess_s_gc + self_enh_s_gc|cntry/essround), 
                  REML = FALSE, 
                  weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_cult0, "m_vl_cult0.rds") # model
#saveRDS(summary(m_vl_cult0), "m_vl_cult0.rds") # model summary
readRDS("m_vl_cult0.rds")


### X: 
# openess *  essround
# self_enh *  essround 
# no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_cult0_1 = lmer(data = data, polit_cult ~ 
                      openess_s_gc * essround +
                      self_enh_s_gc * essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                    REML = FALSE, 
                    weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_cult0_1, "m_vl_cult0_1.rds") # model
#saveRDS(summary(m_vl_cult0_1), "m_vl_cult0_1.rds") # model summary
readRDS("m_vl_cult0_1.rds")


### X:
# openess * west_est 
# self_enh * west_est 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_cult1 = lmer(data = data, polit_cult ~ 
                    openess_s_gc * west_east +
                    self_enh_s_gc * west_east +
                    essround +
                    gndr_01 + agea_s + eduyrs_s +
                    (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                  REML = FALSE, 
                  weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_cult1, "m_vl_cult1.rds") # model
#saveRDS(summary(m_vl_cult1), "m_vl_cult1.rds") # model summary
readRDS("m_vl_cult1.rds")

### X: 
# openess * west_est * essround
# self_enh * west_est * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_cult1_1 = lmer(data = data, polit_cult ~ 
                      openess_s_gc * west_east * essround +
                      self_enh_s_gc * west_east * essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                    REML = FALSE, 
                    weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_cult1_1, "m_vl_cult1_1.rds") # model
#saveRDS(summary(m_vl_cult1_1), "m_vl_cult1_1.rds") # model summary
readRDS("m_vl_cult1_1.rds")

###############################
### X: 
# openess * welfare state 
# self_enh * welfare state 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_cult2 = lmer(data = data, polit_cult ~ 
                    openess_s_gc * welstate +
                    self_enh_s_gc * welstate +
                    essround +
                    gndr_01 + agea_s + eduyrs_s +
                    (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                  REML = FALSE, 
                  weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_cult2, "m_vl_cult2.rds") # model
#saveRDS(summary(m_vl_cult2), "m_vl_cult2.rds") # model summary
readRDS("m_vl_cult2.rds")

### X: 
# openess * welfare state  * essround
# self_enh * welfare state  * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_cult2_1 = lmer(data = data, polit_cult ~
                      openess_s_gc * welstate * essround +
                      self_enh_s_gc * welstate * essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                    REML = FALSE, 
                    weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_cult2_1, "m_vl_cult2_1.rds") # model
#saveRDS(summary(m_vl_cult2_1), "m_vl_cult2_1.rds") # model summary
readRDS("m_vl_cult2_1.rds")


##############################################################################
### Compare models' fit for the cultural beliefs ###

# Compare m_vl_cult0 with m_vl_cult0_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_cult0, m_vl_cult0_1), "m_vl_cult0_anova.rds")
readRDS("m_vl_cult0_anova.rds")

# Compare m_vl_cult1 with m_vl_cult1_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_cult1, m_vl_cult1_1), "m_vl_cult1_anova.rds")
readRDS("m_vl_cult1_anova.rds")

# Compare m_vl_cult2 with m_vl_cult2_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_cult2, m_vl_cult2_1), "m_vl_cult2_anova.rds")
readRDS("m_vl_cult2_anova.rds")

### Compare models with west_east vs. welstate 
# with essround main effect
saveRDS(anova(m_vl_cult1, m_vl_cult2), "m_vl_cult1_vs_cult2_anova.rds")
readRDS("m_vl_cult1_vs_cult2_anova.rds")
# with essround interaction effect
saveRDS(anova(m_vl_cult1_1, m_vl_cult2_1), "m_vl_cult1_1_vs_cult2_1_anova.rds")
readRDS("m_vl_cult1_1_vs_cult2_1_anova")


### Compare all models for cultural beliefs

saveRDS(anova(m_vl_cult0,m_vl_cult1,m_vl_cult2,
              m_vl_cult0_1,m_vl_cult1_1,m_vl_cult2_1), 
        "m_vl_cult_all.rds")
readRDS("m_vl_cult_all.rds")


write.table(anova(m_vl_cult0,m_vl_cult1,m_vl_cult2,
                  m_vl_cult0_1,m_vl_cult1_1,m_vl_cult2_1), 
            "anova_vl_cult.csv", sep=";", dec=",")

rm(m_vl_cult0)
rm(m_vl_cult0_1)
rm(m_vl_cult1)
rm(m_vl_cult1_1)
rm(m_vl_cult2)
rm(m_vl_cult2_1)

###############################################################################################################
### Models for economic beliefs ###
###############################################################################################################

setwd("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh")

### X: 
# openness + self_enh + essround, no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_econ0 = lmer(data = data, polit_econ ~ 
                 openess_s_gc + 
                 self_enh_s_gc +
                 essround +
                 gndr_01 + agea_s + eduyrs_s +
                 (1 + openess_s_gc + self_enh_s_gc|cntry/essround), 
               REML = FALSE, 
               weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_econ0, "m_vl_econ0.rds") # model
#saveRDS(summary(m_vl_econ0), "m_vl_econ0.rds") # model summary
readRDS("m_vl_econ0.rds")


### X: 
# openess *  essround
# self_enh *  essround 
# no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_econ0_1 = lmer(data = data, polit_econ ~ 
                      openess_s_gc * essround +
                      self_enh_s_gc * essround +
                        gndr_01 + agea_s + eduyrs_s +
                        (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                      REML = FALSE, 
                      weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_econ0_1, "m_vl_econ0_1.rds") # model
#saveRDS(summary(m_vl_econ0_1), "m_vl_econ0_1.rds") # model summary
readRDS("m_vl_econ0_1.rds")


### X:
# openess * west_est 
# self_enh * west_est 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_econ1 = lmer(data = data, polit_econ ~ 
                    openess_s_gc * west_east +
                    self_enh_s_gc * west_east +
                    essround +
                    gndr_01 + agea_s + eduyrs_s +
                    (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                  REML = FALSE, 
                  weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_econ1, "m_vl_econ1.rds") # model
#saveRDS(summary(m_vl_econ1), "m_vl_econ1.rds") # model summary
readRDS("m_vl_econ1.rds")

### X: 
# openess * west_est * essround
# self_enh * west_est * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_econ1_1 = lmer(data = data, polit_econ ~ 
                      openess_s_gc * west_east * essround +
                      self_enh_s_gc * west_east * essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                    REML = FALSE, 
                    weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_econ1_1, "m_vl_econ1_1.rds") # model
#saveRDS(summary(m_vl_econ1_1), "m_vl_econ1_1.rds") # model summary
readRDS("m_vl_econ1_1.rds")

###############################
### X: 
# openess * welfare state 
# self_enh * welfare state 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_econ2 = lmer(data = data, polit_econ ~ 
                    openess_s_gc * welstate +
                    self_enh_s_gc * welstate +
                    essround +
                    gndr_01 + agea_s + eduyrs_s +
                    (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                  REML = FALSE, 
                  weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_econ2, "m_vl_econ2.rds") # model
#saveRDS(summary(m_vl_econ2), "m_vl_econ2.rds") # model summary
readRDS("m_vl_econ2.rds")

### X: 
# openess * welfare state  * essround
# self_enh * welfare state  * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_econ2_1 = lmer(data = data, polit_econ ~ 
                      openess_s_gc * welstate * essround +
                      self_enh_s_gc * welstate * essround +
                   gndr_01 + agea_s + eduyrs_s +
                   (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                 REML = FALSE, 
                 weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_econ2_1, "m_vl_econ2_1.rds") # model
#saveRDS(summary(m_vl_econ2_1), "m_vl_econ2_1.rds") # model summary
readRDS("m_vl_econ2_1.rds")

# Compare m_vl_econ2 with m_vl_econ2_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_econ2, m_vl_econ2_1), "m_vl_econ2_anova.rds")
readRDS("m_vl_econ2_anova.rds")

##############################################################################
### Compare models' fit for the economic beliefs ###

# Compare m_vl_econ0 with m_vl_econ0_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_econ0, m_vl_econ0_1), "m_vl_econ0_anova.rds")
readRDS("m_vl_econ0_anova.rds")

# Compare m_vl_econ1 with m_vl_econ1_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_econ1, m_vl_econ1_1), "m_vl_econ1_anova.rds")
readRDS("m_vl_econ1_anova.rds")

### Compare models with west_east vs. welstate 
# with essround main effect
saveRDS(anova(m_vl_econ1, m_vl_econ2), "m_vl_econ1_vs_econ2_anova.rds")
readRDS("m_vl_econ1_vs_econ2_anova.rds")
# with essround interaction effect
saveRDS(anova(m_vl_econ1_1, m_vl_econ2_1), "m_vl_econ1_1_vs_econ2_1_anova.rds")
readRDS("m_vl_econ1_1_vs_econ2_1_anova")

### Compare all models for economic beliefs

saveRDS(anova(m_vl_econ0,m_vl_econ1,m_vl_econ2,
              m_vl_econ0_1,m_vl_econ1_1,m_vl_econ2_1), 
        "m_vl_econ_all.rds")
readRDS("m_vl_econ_all.rds")


write.table(anova(m_vl_econ0,m_vl_econ1,m_vl_econ2,
                 m_vl_econ0_1,m_vl_econ1_1,m_vl_econ2_1), 
            "anova_vl_econ.csv", sep=";", dec=",")

rm(m_vl_econ0)
rm(m_vl_econ0_1)
rm(m_vl_econ1)
rm(m_vl_econ1_1)
rm(m_vl_econ2)
rm(m_vl_econ2_1)

###############################################################################################################
### Models for preference of redistribution ###
###############################################################################################################
# Economic beliefs but with only 1-item that is measured across all ESS rounds

setwd("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh")

### X: 
# openness + self_enh + essround, no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_incdif0 = lmer(data = data, gincdif_s ~ 
                      openess_s_gc + 
                      self_enh_s_gc +
                      essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround), 
                    REML = FALSE, 
                    weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_incdif0, "m_vl_incdif0.rds") # model
#saveRDS(summary(m_vl_incdif0), "m_vl_incdif0.rds") # model summary
readRDS("m_vl_incdif0.rds")


### X: 
# openess *  essround
# self_enh *  essround 
# no country location
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_incdif0_1 = lmer(data = data, gincdif_s ~ 
                        openess_s_gc * essround +
                        self_enh_s_gc * essround +
                        gndr_01 + agea_s + eduyrs_s +
                        (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                      REML = FALSE, 
                      weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_incdif0_1, "m_vl_incdif0_1.rds") # model
#saveRDS(summary(m_vl_incdif0_1), "m_vl_incdif0_1.rds") # model summary
readRDS("m_vl_incdif0_1.rds")


### X:
# openess * west_est 
# self_enh * west_est 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_incdif1 = lmer(data = data, gincdif_s ~ 
                      openess_s_gc * west_east +
                      self_enh_s_gc * west_east +
                      essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                    REML = FALSE, 
                    weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_incdif1, "m_vl_incdif1.rds") # model
#saveRDS(summary(m_vl_incdif1), "m_vl_incdif1.rds") # model summary
readRDS("m_vl_incdif1.rds")

### X: 
# openess * west_est * essround
# self_enh * west_est * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_incdif1_1 = lmer(data = data, gincdif_s ~ 
                        openess_s_gc * west_east * essround +
                        self_enh_s_gc * west_east * essround +
                        gndr_01 + agea_s + eduyrs_s +
                        (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                      REML = FALSE, 
                      weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_incdif1_1, "m_vl_incdif1_1.rds") # model
#saveRDS(summary(m_vl_incdif1_1), "m_vl_incdif1_1.rds") # model summary
readRDS("m_vl_incdif1_1.rds")

###############################
### X: 
# openess * welfare state 
# self_enh * welfare state 
# + essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh
m_vl_incdif2 = lmer(data = data, gincdif_s ~ 
                      openess_s_gc * welstate +
                      self_enh_s_gc * welstate +
                      essround +
                      gndr_01 + agea_s + eduyrs_s +
                      (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                    REML = FALSE, 
                    weights = pspwght_popsize)

# Save model and model summary
saveRDS(m_vl_incdif2, "m_vl_incdif2.rds") # model
#saveRDS(summary(m_vl_incdif2), "m_vl_incdif2.rds") # model summary
readRDS("m_vl_incdif2.rds")

### X: 
# openess * welfare state  * essround
# self_enh * welfare state  * essround
# random intercept for country and wave (wave nested within a country)
# random slopes for openess and self_enh

m_vl_incdif2_1 = lmer(data = data, gincdif_s ~ 
                        openess_s_gc * welstate * essround +
                        self_enh_s_gc * welstate * essround +
                        gndr_01 + agea_s + eduyrs_s +
                        (1 + openess_s_gc + self_enh_s_gc|cntry/essround),  
                      REML = FALSE, 
                      weights = pspwght_popsize)
# Save model and model summary
saveRDS(m_vl_incdif2_1, "m_vl_incdif2_1.rds") # model
#saveRDS(summary(m_vl_incdif2_1), "m_vl_incdif2_1.rds") # model summary
readRDS("m_vl_incdif2_1.rds")

### Update for the model 2_1 -> to achieve convergence
m_vl_incdif2_1 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_incdif2_1.rds")
m_vl_incdif2_1

# check singularity 
tt <- getME(m_vl_incdif2_1 ,"theta")
ll <- getME(m_vl_incdif2_1 ,"lower")
min(tt[ll==0])
# that might be an issue, 0.0008 (quite close to 0) but not really


# alternative method of assessing https://stats.stackexchange.com/questions/97929/lmer-model-fails-to-converge
# if a values is small, we should be happy
relgrad <- with(m_vl_incdif2_1@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

# Ben Bolker advice: https://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4
control=lmerControl(check.conv.singular="warning")
control

# restart model
ss <- getME(m_vl_incdif2_1 ,c("theta","fixef"))
m_vl_incdif2_1_update <- update(m_vl_incdif2_1 ,start=ss,control = lmerControl(optCtrl=list(maxfun=2e4)))

# different optimizer
m_vl_incdif2_1_update2 <- update(m_vl_incdif2_1,start=ss,control=lmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e4)))
# more optimizers
install.packages("optimx")
install.packages("nloptr")
#install.packages("nlminb") # not available vor R version 3.4.3
library("optimx")
library("nloptr")

# update 3 did converged!
m_vl_incdif2_1_update3 <- update(m_vl_incdif2_1,start=ss,control=lmerControl(optimizer="Nelder_Mead",
                                                                             optCtrl=list(maxfun=2e4)))

m_vl_incdif2_1 <- m_vl_incdif2_1_update3
# Save model and model summary
saveRDS(m_vl_incdif2_1, "m_vl_incdif2_1.rds") # model
#saveRDS(summary(m_vl_incdif2_1), "m_vl_incdif2_1.rds") # model summary
readRDS("m_vl_incdif2_1.rds")


##############################################################################
### Compare models' fit for the economic beliefs ###

# Compare m_vl_incdif0 with m_vl_incdif0_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_incdif0, m_vl_incdif0_1), "m_vl_incdif0_anova.rds")
readRDS("m_vl_incdif0_anova.rds")

# Compare m_vl_incdif1 with m_vl_incdif1_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_incdif1, m_vl_incdif1_1), "m_vl_incdif1_anova.rds")
readRDS("m_vl_incdif1_anova.rds")

# Compare m_vl_incdif1_1 with m_vl_incdif2 (ess round main effect or interaction) -> THIS MODEL IS SAVED
saveRDS(anova(m_vl_incdif1_1, m_vl_incdif2), "m_vl_incdif1_1_vs_2_anova.rds")
readRDS("m_vl_incdif1_1_vs_2_anova.rds")

# Compare m_vl_incdif2 with m_vl_incdif2_1 (ess round main effect or interaction)
saveRDS(anova(m_vl_incdif2, m_vl_incdif2_1), "m_vl_incdif2_anova.rds")

### Compare models with west_east vs. welstate 
# with essround main effect
saveRDS(anova(m_vl_incdif1, m_vl_incdif2), "m_vl_incdif1_vs_incdif2_anova.rds")
readRDS("m_vl_incdif1_vs_incdif2_anova.rds")
# with essround interaction effect
saveRDS(anova(m_vl_incdif1_1, m_vl_incdif2_1), "m_vl_incdif1_1_vs_incdif2_1_anova.rds")
readRDS("m_vl_incdif1_1_vs_incdif2_1_anova")

### Compare all models for economic beliefs

saveRDS(anova(m_vl_incdif0,m_vl_incdif1,m_vl_incdif2,
              m_vl_incdif0_1,m_vl_incdif1_1,m_vl_incdif2_1), 
        "m_vl_incdif_all.rds")
readRDS("m_vl_incdif_all.rds")


write.table(anova(m_vl_incdif0,m_vl_incdif1,m_vl_incdif2,
                  m_vl_incdif0_1,m_vl_incdif1_1,m_vl_incdif2_1), 
            "anova_vl_incdif.csv", sep=";", dec=",")

rm(m_vl_incdif0)
rm(m_vl_incdif0_1)
rm(m_vl_incdif1)
rm(m_vl_incdif1_1)
rm(m_vl_incdif2)
rm(m_vl_incdif2_1)

