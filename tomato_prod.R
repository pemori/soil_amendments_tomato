setwd("D:\\Users\\pedro\\Documents\\tomato_soil_amendments")

{
library(readxl)
library(emmeans)
library(multcomp)
library(multcompView)
library(lmerTest)
library(car)
library(ggplot2)
library(report)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(dplyr)
library(ggplot2)
library(gclus)
library(PerformanceAnalytics)
library(corrplot)
library(xlsx)
library(userfriendlyscience)
}
#data preparation and descriptive statistic####
data<- read_excel("data_tomato2.xlsx", sheet='prod')

names <- c('farmer','producer','treatment')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

mean_prod <- data %>%
  select(-2,-4,-5) %>%
  aggregate(. ~ treatment + producer, FUN = mean) %>%
  arrange(treatment,producer)

str(mean_prod) #perfect
#View(mean_prod)

data<- read_excel("data_tomato2.xlsx", sheet='phytoc')

names <- c('farmer','producer','treatment')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

mean_phytoc <- data %>%
  select(-2,-5,-6) %>%
  aggregate(. ~ treatment + producer, FUN = mean) %>%
  arrange(treatment,producer)

str(mean_phytoc) #perfect
#View(mean_phytoc)

# #soil_var
# data<- read_excel("data_tomato2.xlsx", sheet='soil_var')
# 
# names <- c('farmer','producer','treatment','management')
# data[,names] <- lapply(data[,names] , as.factor)
# 
# str(data)
# data
# mean_soil_var <- data %>%
#   select(-5,-12,-24) %>%
#   arrange(treatment,producer)
# 
# str(mean_soil_var) #perfect
# soil_var0<-mean_soil_var %>% dplyr::select(6:16)
# soil_mean_var0<-mean_soil_var %>% 
#   #dplyr::filter(treatment=='0_Control') %>%
#   aggregate(. ~ producer, FUN = mean)
# 
# soil_mean_var0
# soil_var0
# str(soil_var0)
# soil_var6<-mean_soil_var %>% dplyr::select(1,5,17:24)
# soil_mean_var6<-soil_var6 %>%
#   aggregate(. ~  producer + treatment, FUN = mean)
# #View(soil_var6)
# #View(mean_soil_var)
# 
# 
#soil_bact
data<- read_excel("data_tomato2.xlsx", sheet='soil_bact')

names <- c('farmer','producer','treatment')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

mean_soil_bact <- data %>%
  select(-3) %>%
  aggregate(. ~ treatment + producer, FUN = mean) %>%
  arrange(treatment,producer)

SE_soil_bact <- data %>%
  select(-3) %>%
  aggregate(. ~ treatment + producer, FUN = mean) %>%
  arrange(treatment,producer)
str(mean_soil_bact) #perfect
#View(mean_soil_bact)

soil_bact0_sci <- format(mean_soil_bact, scientific=TRUE)
write.xlsx2(soil_bact0_sci, file='bact_sci_summary.xlsx')


soil_bact0<-mean_soil_bact %>% dplyr::select(4:10)
str(soil_bact0)
soil_bact3<-mean_soil_bact %>% dplyr::select(11:17)
str(soil_bact3)
# soil_bact6<-mean_soil_bact %>% dplyr::select(18:24)
# str(soil_bact6)
# 
# soil_bact0

all <- cbind (mean_prod,mean_phytoc,mean_soil_var,mean_soil_bact)
tomato<- cbind(mean_prod,mean_phytoc)
#tomato <-tomato %>% select(-3,-7,-8,-9)
tomato
soil<- cbind(mean_soil_var,mean_soil_bact)

tomato_num <- tomato %>%select(-1,-2) %>%
  relocate(c('fruit_weight_g', 'hardness_kg_per_cm2'), .after = 'diameter')
tomato_num
soil_num <- soil %>%select(-1,-2,-4,-5)
soil_6 <- cbind(soil_var6, soil_bact6)
soil_6
datapca <- cbind(tomato_num,soil_6)

fct<- soil[,c(1:5)]
fct

# 0 - Report mean and SE of soil and plant/fruit results####

# 1- What soil variables modulate tomato productivity & quality?
#Let's see what is the relation among soil and plant/fruits variables considering the initial condition of soils
soil_0 <-cbind(fct,soil_bact0,soil_var0)
soil_0
soil_6_f<-cbind(soil_6, fct)
soil_6_f
soil_data<-soil_6_f[,1:17]
soil_data

# dim(soil_6_f)
# soil_6_ctrl <- soil_6_f %>%
#   filter(treatment== '0_Control') %>%
#   select(-c(18:19,21:22)) %>%
#   relocate(14,.after = 'Nitrogen_fixers_6') %>%
#   relocate(9:11, .before = 'MI')
# str(soil_6_ctrl)
# 
# soil_0_ctrl <- soil_0 %>%
#   filter(treatment== '0_Control') %>%
#   select(-c(1:2,4:5,21:22)) %>%
#   relocate(c(1:8), .after = 'clay') %>%
#   relocate('Phosphate _solubilizer_0',.after = 'Nitrogen_fixers_0') %>%
#   relocate(11:13, .after = 'Phosphate _solubilizer_0') %>%
#   relocate('MI', .after = 'Pseudomonas_0') %>%
#   select(-17)
# soil_0
# soil_0_ctrl
# tomato_ctrl<- tomato %>%
#   filter(treatment== '0_control') %>%
#   select(-c(1:2,4)) %>%
#   relocate(c(9:10), .after = 'diameter') %>%
#   relocate(7,6,.before = 'anthraq')
# tomato_ctrl
# soil_6_ctrl
# soil_0_ctrl
# tomato

# 1.1 Tomato productivity ####
data<- read_excel("data_tomato2.xlsx", sheet='prod')

names <- c('farmer','producer','treatment','management','SOM_cat')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

#N° of fruits in two clusters####
### Plot
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$n_fruits,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.n_fruits<-lm(n_fruits ~ treatment, data=data)
Anova(lm.n_fruits)

lmer.n_fruits<-lmer(n_fruits ~ treatment +(1|producer), data=data)
Anova(lmer.n_fruits)
ranova(lmer.n_fruits)

lm.n_fruits_m<-lm(n_fruits ~ treatment * management, data=data)
Anova(lm.n_fruits_m)

lm.n_fruits_s<-lm(n_fruits ~ treatment * SOM_cat, data=data)
Anova(lm.n_fruits_s)

#anova for the best model
report(lmer.n_fruits)
plot(resid(lmer.n_fruits))> abline(0, 0) #ok
bartlett.test(size ~ treatment, data = data) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.n_fruits)
print(anova_result)

#posthoc
difflsmeans(lmer.n_fruits, test.effs = "treatment", ddf="Kenward-Roger")
#a-a-a-a

#diameter####
### Plot
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$diameter,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.diameter<-lm(diameter ~ treatment, data=data)
Anova(lm.diameter)

lmer.diameter<-lmer(diameter ~ treatment +(1|producer), data=data)
Anova(lmer.diameter)
ranova(lmer.diameter)

lm.diameter_m<-lm(diameter ~ treatment * management, data=data)
Anova(lm.diameter_m)

lm.diameter_s<-lm(diameter ~ treatment * SOM_cat, data=data)
Anova(lm.diameter_s)

#anova for the best model
report(lmer.diameter)
plot(resid(lmer.diameter))> abline(0, 0) #ok
bartlett.test(size ~ treatment, data = data) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.diameter)
print(anova_result)

#posthoc
difflsmeans(lmer.diameter, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: a-ab-ab-b

#fruit_weight_g####
data<- read_excel("data_tomato2.xlsx", sheet='phytoc')

names <- c('farmer','producer','treatment','management','SOM_cat')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

### Plot
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$fruit_weight_g,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.fruit_weight_g<-lm(fruit_weight_g ~ treatment, data=data)
Anova(lm.fruit_weight_g)

lmer.fruit_weight_g<-lmer(fruit_weight_g ~ treatment +(1|producer), data=data)
Anova(lmer.fruit_weight_g)
ranova(lmer.fruit_weight_g)

lm.fruit_weight_g_m<-lm(fruit_weight_g ~ treatment * management, data=data)
Anova(lm.fruit_weight_g_m)

lm.fruit_weight_g_s<-lm(fruit_weight_g ~ treatment * SOM_cat, data=data)
Anova(lm.fruit_weight_g_s)

#anova for the best model
report(lmer.fruit_weight_g)
plot(resid(lmer.fruit_weight_g))> abline(0, 0) #ok
anova_result <- anova(lmer.fruit_weight_g)
print(anova_result)

#posthoc
difflsmeans(lmer.fruit_weight_g, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: a-a-a-a

#hardness####
### Plot
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$hardness_kg_per_cm2,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.hardness_kg_per_cm2<-lm(hardness_kg_per_cm2 ~ treatment, data=data)
Anova(lm.hardness_kg_per_cm2)

lmer.hardness_kg_per_cm2<-lmer(hardness_kg_per_cm2 ~ treatment +(1|producer), data=data)
Anova(lmer.hardness_kg_per_cm2)
ranova(lmer.hardness_kg_per_cm2)

lm.hardness_kg_per_cm2_m<-lm(hardness_kg_per_cm2 ~ treatment * management, data=data)
Anova(lm.hardness_kg_per_cm2_m)

lm.hardness_kg_per_cm2_s<-lm(hardness_kg_per_cm2 ~ treatment * SOM_cat, data=data)
Anova(lm.hardness_kg_per_cm2_s)

#anova for the best model
report(lmer.hardness_kg_per_cm2)
plot(resid(lmer.hardness_kg_per_cm2))> abline(0, 0) #ok
anova_result <- anova(lmer.hardness_kg_per_cm2)
print(anova_result)

#posthoc
difflsmeans(lmer.hardness_kg_per_cm2, test.effs = "treatment", ddf="Kenward-Roger")
#Tukey results: a-a-a-a

# 1.2 Tomato quality####

data<- read_excel("data_tomato2.xlsx", sheet='phytoc')

names <- c('farmer','producer','treatment','management','SOM_cat')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

#total phenols####
### Plot
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$phenol,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.phenol<-lm(phenol ~ treatment, data=data)
Anova(lm.phenol)

lmer.phenol<-lmer(phenol ~ treatment +(1|producer), data=data)
Anova(lmer.phenol)
ranova(lmer.phenol)

lm.phenol_m<-lm(phenol ~ treatment * management, data=data)
Anova(lm.phenol_m)

lm.phenol_s<-lm(phenol ~ treatment * SOM_cat, data=data)
Anova(lm.phenol_s)

#anova for the best model
report(lmer.phenol)
plot(resid(lmer.phenol))> abline(0, 0) #ok
anova_result <- anova(lmer.phenol)
print(anova_result)

#posthoc
difflsmeans(lmer.phenol, test.effs = "treatment", ddf="Kenward-Roger")
#Tukey results: b-ab-ab-a

#flavonoids####
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$flav,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.flav<-lm(flav ~ treatment, data=data)
Anova(lm.flav)

lmer.flav<-lmer(flav ~ treatment +(1|producer), data=data)
Anova(lmer.flav)
ranova(lmer.flav)

lm.flav_m<-lm(flav ~ treatment * management, data=data)
Anova(lm.flav_m)

lm.flav_s<-lm(flav ~ treatment * SOM_cat, data=data)
Anova(lm.flav_s)

#anova for the best model
report(lmer.flav)
plot(resid(lmer.flav))> abline(0, 0) #ok
anova_result <- anova(lmer.flav)
print(anova_result)

#posthoc
difflsmeans(lmer.flav, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: a-a-a-a

#anthraq####
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$anthraq,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.anthraq<-lm(anthraq ~ treatment, data=data)
Anova(lm.anthraq)

lmer.anthraq<-lmer(anthraq ~ treatment +(1|producer), data=data)
Anova(lmer.anthraq)
ranova(lmer.anthraq)

lm.anthraq_m<-lm(anthraq ~ treatment * management, data=data)
Anova(lm.anthraq_m)

lm.anthraq_s<-lm(anthraq ~ treatment * SOM_cat, data=data)
Anova(lm.anthraq_s)

#anova for the best model
report(lmer.anthraq)
plot(resid(lmer.anthraq))> abline(0, 0) #ok
anova_result <- anova(lmer.anthraq)
print(anova_result)

#posthoc
difflsmeans(lmer.anthraq, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: b-a-a-a

#lycop####
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$lycop,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.lycop<-lm(lycop ~ treatment, data=data)
Anova(lm.lycop)

lmer.lycop<-lmer(lycop ~ treatment +(1|producer), data=data)
Anova(lmer.lycop)
ranova(lmer.lycop)

lm.lycop_m<-lm(lycop ~ treatment * management, data=data)
Anova(lm.lycop_m)

lm.lycop_s<-lm(lycop ~ treatment * SOM_cat, data=data)
Anova(lm.lycop_s)

#anova for the best model
report(lmer.lycop)
plot(resid(lmer.lycop))> abline(0, 0) #ok
anova_result <- anova(lmer.lycop)
print(anova_result)

#posthoc
difflsmeans(lmer.lycop, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: b-a-a-a


#DPPH####
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$DPPH,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.DPPH<-lm(DPPH ~ treatment, data=data)
Anova(lm.DPPH)

lmer.DPPH<-lmer(DPPH ~ treatment +(1|producer), data=data)
Anova(lmer.DPPH)
ranova(lmer.DPPH)

lm.DPPH_m<-lm(DPPH ~ treatment * management, data=data)
Anova(lm.DPPH_m)

lm.DPPH_s<-lm(DPPH ~ treatment * SOM_cat, data=data)
Anova(lm.DPPH_s)

#anova for the best model
report(lmer.DPPH)
plot(resid(lmer.DPPH))> abline(0, 0) #ok
anova_result <- anova(lmer.DPPH)
print(anova_result)

#posthoc
difflsmeans(lmer.DPPH, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: a-a-a-a

#brix####
interaction.plot(x.factor     = data$producer,
                 trace.factor = data$treatment,
                 response     = data$brix,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.brix<-lm(brix ~ treatment, data=data)
Anova(lm.brix)

lmer.brix<-lmer(brix ~ treatment +(1|producer), data=data)
Anova(lmer.brix)
ranova(lmer.brix)

lm.brix_m<-lm(brix ~ treatment * management, data=data)
Anova(lm.brix_m)

lm.brix_s<-lm(brix ~ treatment * SOM_cat, data=data)
Anova(lm.brix_s)

#anova for the best model
report(lmer.brix)
plot(resid(lmer.brix))> abline(0, 0) #ok
anova_result <- anova(lmer.brix)
print(anova_result)

#posthoc
difflsmeans(lmer.brix, test.effs = "treatment", ddf="Kenward-Roger")
#printed in 500*520. Tukey results: a-a-a-a



#####


#2. What is the effect of treatments on soil responses? #####

soil<- read_excel("data_soil.xlsx", sheet='soil')
#conversion to ppm
soil$N_tot<- soil$N_tot/10000 
soil$P_av<- soil$P_av/10
soil$K_av<- soil$K_av * 390/10 
names <- c('farmer','producer','treatment','management','condition','N_cat','P_cat','K_cat','SOM_cat','clay_cat')
soil[,names] <- lapply(soil[,names] , as.factor)
str(soil)

soil_0<- soil %>% 
  filter(condition== 'pre-treatment')
str(soil_0)

soil_6<- soil %>% 
  filter(condition== '6m')
str(soil_6)

soil_bal<- read_excel("data_soil.xlsx", sheet='soil_bal')
str(soil)
names <- c('farmer','producer','treatment','management','condition')
soil_bal[,names] <- lapply(soil_bal[,names] , as.factor)
str(soil_bal)

soil_data<-cbind(soil,soil_bal)
str(soil_data)
#View(soil_data)
soil_data2<-soil_data[,-c(5,6,13,14,18:19,27:33,39,43:47)]
str(soil_data2)
#View(soil_data2)


#Para lineplots trabajaré con soil_6

#pH####   
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$pH,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=TRUE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.pH<-lm(pH ~ treatment, data=soil_6)
Anova(lm.pH)

lmer.pH<-lmer(pH ~ treatment +(1|producer), data=soil_6)
Anova(lmer.pH)
ranova(lmer.pH)

lm.pH_m<-lm(pH ~ treatment * management, data=soil_6)
Anova(lm.pH_m)

lm.pH_s<-lm(pH ~ treatment * SOM_cat, data=soil_6)
Anova(lm.pH_s)

#anova for the best model
report(lmer.pH)
plot(resid(lmer.pH))> abline(0, 0) #ok
anova_result <- anova(lmer.pH)
print(anova_result)

#posthoc
difflsmeans(lmer.pH, test.effs = "treatment", ddf="Kenward-Roger")
#no differences

#CE####  
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$CE,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.CE<-lm(CE ~ treatment, data=soil_6)
Anova(lm.CE)

lmer.CE<-lmer(CE ~ treatment +(1|producer), data=soil_6)
Anova(lmer.CE)
ranova(lmer.CE)

lm.CE_m<-lm(CE ~ treatment * management, data=soil_6)
Anova(lm.CE_m)

lm.CE_s<-lm(CE ~ treatment * SOM_cat, data=soil_6)
Anova(lm.CE_s)

#anova for the best model
report(lmer.CE)
plot(resid(lmer.CE))> abline(0, 0) #ok
anova_result <- anova(lmer.CE)
print(anova_result)

#posthoc
difflsmeans(lmer.CE, test.effs = "treatment", ddf="Kenward-Roger")
#no differences


#N_t####
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$N_tot,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.N<-lm(N_tot ~ treatment, data=soil_6)
Anova(lm.N)

lmer.N_t<-lmer(N_tot ~ treatment +(1|producer), data=soil_6)
Anova(lmer.N_t)
ranova(lmer.N_t)

lm.N_tm<-lm(N_tot ~ treatment * management, data=soil_6)
Anova(lm.N_tm)

lm.N_ts<-lm(N_tot ~ treatment * SOM_cat, data=soil_6)
Anova(lm.N_ts)

#anova for the best model
report(lmer.N_t)
plot(resid(lmer.N_t))> abline(0, 0) #ok
anova_result <- anova(lmer.N_t)
print(anova_result)

#posthoc
posthoc_result <- glht(lmer.N_t, linfct = mcp(treatment = "Tukey")) #trt se refiere
print(summary(posthoc_result))
#library(emmeans)
#emmeans(lmer.PHEN_t_p, list(pairwise ~ treatment), adjust = "tukey")
#difflsmeans(lmer.PHEN_t_p, test.effs = "treatment", ddf="Satterthwaite")
#summary(glht(lmer.PHEN_t_p, linfct = mcp(treatment = "Tukey")), test = adjusted("holm"))
difflsmeans(lmer.N_t, test.effs = "treatment", ddf="Kenward-Roger")

#P_av####
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$P_av,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.P_av<-lm(P_av ~ treatment, data=soil_6)
Anova(lm.P_av)

lmer.P_av<-lmer(P_av ~ treatment +(1|producer), data=soil_6)
Anova(lmer.P_av)
ranova(lmer.P_av)

lm.P_av_m<-lm(P_av ~ treatment *management, data=soil_6)
Anova(lm.P_av_m)

lm.P_av_s<-lm(P_av ~ treatment *SOM_cat, data=soil_6)
Anova(lm.P_av_s)

#anova for the best model
report(lmer.P_av)
plot(resid(lmer.P_av))> abline(0, 0) #ok
anova_result <- anova(lmer.P_av)
print(anova_result)

#posthoc
difflsmeans(lmer.P_av, test.effs = "treatment", ddf="Kenward-Roger")

#K_av####
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$K_av,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.K_av<-lm(K_av ~ treatment, data=soil_6)
Anova(lm.K_av)

lmer.K_av<-lmer(K_av ~ treatment +(1|producer), data=soil_6)
Anova(lmer.K_av)
ranova(lmer.K_av)

lm.K_av_m<-lm(K_av ~ treatment * management, data=soil_6)
Anova(lm.K_av_m)

lm.K_av_s<-lm(K_av ~ treatment * SOM_cat, data=soil_6)
Anova(lm.K_av_s)

#anova for the best model
report(lmer.K_av)
plot(resid(lmer.K_av))> abline(0, 0) #ok
anova_result <- anova(lmer.K_av)
print(anova_result)

#posthoc
difflsmeans(lmer.K_av, test.effs = "treatment", ddf="Kenward-Roger")

#SOM####
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$SOM,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.SOM<-lm(SOM ~ treatment, data=soil_6)
Anova(lm.SOM)

lmer.SOM<-lmer(SOM ~ treatment +(1|producer), data=soil_6)
Anova(lmer.SOM)
ranova(lmer.SOM)

lm.SOM_m<-lm(SOM ~ treatment * management, data=soil_6)
Anova(lm.SOM_m)

lm.SOM_s<-lm(SOM ~ treatment * SOM_cat, data=soil_6)
Anova(lm.SOM_s)

#anova for the best model
report(lmer.SOM)
plot(resid(lmer.SOM))> abline(0, 0) #ok
anova_result <- anova(lmer.SOM)
print(anova_result)

#posthoc
difflsmeans(lmer.SOM, test.effs = "treatment", ddf="Kenward-Roger")

#CN####
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$CN,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.CN<-lm(CN ~ treatment, data=soil_6)
Anova(lm.CN)

lmer.CN<-lmer(CN ~ treatment +(1|producer), data=soil_6)
Anova(lmer.CN)
ranova(lmer.CN)

lm.CN_m<-lm(CN ~ treatment * management, data=soil_6)
Anova(lm.CN_m)

lm.CN_s<-lm(CN ~ treatment * SOM_cat, data=soil_6)
Anova(lm.CN_s)

#anova for the best model
report(lmer.CN)
plot(resid(lmer.CN))> abline(0, 0) #ok
anova_result <- anova(lmer.CN)
print(anova_result)

#posthoc
difflsmeans(lmer.CN, test.effs = "treatment", ddf="Kenward-Roger")

#CIC####
### Plot
interaction.plot(x.factor     = soil_6$producer,
                 trace.factor = soil_6$treatment,
                 response     = soil_6$CIC,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF"),
                 pch=c(15, 17,16,18, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520
lm.CIC<-lm(CIC ~ treatment, data=soil_6)
Anova(lm.CIC)

lmer.CIC<-lmer(CIC ~ treatment +(1|producer), data=soil_6)
Anova(lmer.CIC)
ranova(lmer.CIC)

lm.CIC_m<-lm(CIC ~ treatment * management, data=soil_6)
Anova(lm.CIC_m)

lm.CIC_s<-lm(CIC ~ treatment * SOM_cat, data=soil_6)
Anova(lm.CIC_s)

#anova for the best model
report(lmer.CIC)
plot(resid(lmer.CIC))> abline(0, 0) #ok
anova_result <- anova(lmer.CIC)
print(anova_result)

#posthoc
difflsmeans(lmer.CIC, test.effs = "treatment", ddf="Kenward-Roger")


# 1- What is the effect of the treatments on the crop responses?



# 2- relation between soil and tomato variables
#####


#3 What was the effect of amendments on soil_bact?######
data<- read_excel("data_soil_pre.xlsx", sheet='bact')
str(data)
names <- c('farmer','time','trt','management','SOM_cat')
data[,names] <- lapply(data[,names] , as.factor)
str(data)

df<- read_excel("data_soil.xlsx", sheet='bact')
str(df)
names <- c('farmer','time','trt','management','SOM_cat')
df[,names] <- lapply(df[,names] , as.factor)
str(df)

#primero filtrar para gráfico
#filtro
filter_outliers_by_group <- function(data) {
  data %>%
    group_by(time) %>%
    filter_all(all_vars(!is.na(.))) %>%
    filter_if(is.numeric, ~. < quantile(., 0.95))
}
# Aplicar el filtro a todas las columnas numéricas
f_data <- filter_outliers_by_group(data)

#filtro para análisis stat
df_3m <- df%>%
    filter(time== "3 m") 

df_6m <- df%>%
  filter(time=="6 m")

df_c <- df %>%
  filter(trt == "0_Control")

df_co <- df %>%
  filter(trt == "1_Compost")

df_b <- df %>%
  filter(trt == "2_Bokashi")

df_t <- df %>%
  filter(trt == "3_Trichoderma")

colors<-c("black", "orange","#696969","#1E90FF","white")
order<-c("Pre-treatment","3 m","6 m")

nitro_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Nitrogen_fixers), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("N-fixing bacteria, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  #guides(fill = "TRUE") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
nitro_box #449*422

#3m
lmer.nitro<-lmer(log10(Nitrogen_fixers) ~ trt +(1|producer), df_3m)
Anova(lmer.nitro)
ranova(lmer.nitro)
#anova for the best model
plot(resid(lmer.nitro))> abline(0, 0) #ok
anova_result <- anova(lmer.nitro)
print(anova_result)
#posthoc
difflsmeans(lmer.nitro, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.nitro<-lmer(log10(Nitrogen_fixers) ~ trt +(1|producer), df_6m)
Anova(lmer.nitro)
ranova(lmer.nitro)
#anova for the best model
plot(resid(lmer.nitro))> abline(0, 0) #ok
anova_result <- anova(lmer.nitro)
print(anova_result)
#posthoc
difflsmeans(lmer.nitro, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.nitro<-lmer(log10(Nitrogen_fixers) ~ time +(1|producer), df_c)
Anova(lmer.nitro)
ranova(lmer.nitro)
#anova for the best model
plot(resid(lmer.nitro))> abline(0, 0) #ok
anova_result <- anova(lmer.nitro)
print(anova_result)
#posthoc
difflsmeans(lmer.nitro, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.nitro<-lmer(log10(Nitrogen_fixers) ~ time +(1|producer), df_co)
Anova(lmer.nitro)
ranova(lmer.nitro)
#anova for the best model
plot(resid(lmer.nitro))> abline(0, 0) #ok
anova_result <- anova(lmer.nitro)
print(anova_result)
#posthoc
difflsmeans(lmer.nitro, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.nitro<-lmer(log10(Nitrogen_fixers) ~ time +(1|producer), df_b)
Anova(lmer.nitro)
ranova(lmer.nitro)
#anova for the best model
plot(resid(lmer.nitro))> abline(0, 0) #ok
anova_result <- anova(lmer.nitro)
print(anova_result)
#posthoc
difflsmeans(lmer.nitro, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.nitro<-lmer(log10(Nitrogen_fixers) ~ time +(1|producer), df_t)
Anova(lmer.nitro)
ranova(lmer.nitro)
#anova for the best model
plot(resid(lmer.nitro))> abline(0, 0) #ok
anova_result <- anova(lmer.nitro)
print(anova_result)
#posthoc
difflsmeans(lmer.nitro, test.effs = "trt", ddf="Kenward-Roger")

# #3m
# # ANOVA-like test (Welch ANOVA)
# oneway.test(Nitrogen_fixers ~ trt, data = df_3m, var.equal = FALSE)
# # Games-Howell post hoc test
# games_howell_result <- posthocTGH(y=df_3m$Nitrogen_fixers, x=df_3m$trt, method="games-howell")
# print(games_howell_result)
# 
# #6m
# # ANOVA-like test (Welch ANOVA)
# oneway.test(Nitrogen_fixers ~ trt, data = df_6m, var.equal = FALSE)
# # Games-Howell post hoc test
# games_howell_result <- posthocTGH(y=df_6m$Nitrogen_fixers, x=df_6m$trt, method="games-howell")
# print(games_howell_result)
# 
# #C
# # ANOVA-like test (Welch ANOVA)
# oneway.test(Nitrogen_fixers ~ time, data = df_c, var.equal = FALSE)
# # Games-Howell post hoc test
# games_howell_result <- posthocTGH(y=df_c$Nitrogen_fixers, x=df_c$time, method="games-howell")
# print(games_howell_result)
# 
# #Co
# # ANOVA-like test (Welch ANOVA)
# oneway.test(Nitrogen_fixers ~ time, data = df_co, var.equal = FALSE)
# # Games-Howell post hoc test
# games_howell_result <- posthocTGH(y=df_co$Nitrogen_fixers, x=df_co$time, method="games-howell")
# print(games_howell_result)
# 
# #B
# # ANOVA-like test (Welch ANOVA)
# oneway.test(Nitrogen_fixers ~ time, data = df_b, var.equal = FALSE)
# # Games-Howell post hoc test
# games_howell_result <- posthocTGH(y=df_b$Nitrogen_fixers, x=df_b$time, method="games-howell")
# print(games_howell_result)
# 
# #T
# # ANOVA-like test (Welch ANOVA)
# oneway.test(Nitrogen_fixers ~ time, data = df_t, var.equal = FALSE)
# # Games-Howell post hoc test
# games_howell_result <- posthocTGH(y=df_t$Nitrogen_fixers, x=df_t$time, method="games-howell")
# print(games_howell_result)

#phosp
phos_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Phosphate_solubilizers), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("Phosphate solubilizers, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
phos_box

#3m
lmer.phos<-lmer(log10(Phosphate_solubilizers) ~ trt +(1|producer), df_3m)
Anova(lmer.phos)
ranova(lmer.phos)
#anova for the best model
plot(resid(lmer.phos))> abline(0, 0) #ok
anova_result <- anova(lmer.phos)
print(anova_result)
#posthoc
difflsmeans(lmer.phos, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.phos<-lmer(log10(Phosphate_solubilizers) ~ trt +(1|producer), df_6m)
Anova(lmer.phos)
ranova(lmer.phos)
#anova for the best model
plot(resid(lmer.phos))> abline(0, 0) #ok
anova_result <- anova(lmer.phos)
print(anova_result)
#posthoc
difflsmeans(lmer.phos, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.phos<-lmer(log10(Phosphate_solubilizers) ~ time +(1|producer), df_c)
Anova(lmer.phos)
ranova(lmer.phos)
#anova for the best model
plot(resid(lmer.phos))> abline(0, 0) #ok
anova_result <- anova(lmer.phos)
print(anova_result)
#posthoc
difflsmeans(lmer.phos, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.phos<-lmer(log10(Phosphate_solubilizers) ~ time +(1|producer), df_co)
Anova(lmer.phos)
ranova(lmer.phos)
#anova for the best model
plot(resid(lmer.phos))> abline(0, 0) #ok
anova_result <- anova(lmer.phos)
print(anova_result)
#posthoc
difflsmeans(lmer.phos, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.phos<-lmer(log10(Phosphate_solubilizers) ~ time +(1|producer), df_b)
Anova(lmer.phos)
ranova(lmer.phos)
#anova for the best model
plot(resid(lmer.phos))> abline(0, 0) #ok
anova_result <- anova(lmer.phos)
print(anova_result)
#posthoc
difflsmeans(lmer.phos, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.phos<-lmer(log10(Phosphate_solubilizers) ~ time +(1|producer), df_t)
Anova(lmer.phos)
ranova(lmer.phos)
#anova for the best model
plot(resid(lmer.phos))> abline(0, 0) #ok
anova_result <- anova(lmer.phos)
print(anova_result)
#posthoc
difflsmeans(lmer.phos, test.effs = "trt", ddf="Kenward-Roger")

#proteolytic
prot_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Proteolytic), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("Proteolytic, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
prot_box

#3m
lmer.prot<-lmer(log10(Proteolytic) ~ trt +(1|producer), df_3m)
Anova(lmer.prot)
ranova(lmer.prot)
#anova for the best model
plot(resid(lmer.prot))> abline(0, 0) #ok
anova_result <- anova(lmer.prot)
print(anova_result)
#posthoc
difflsmeans(lmer.prot, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.prot<-lmer(log10(Proteolytic) ~ trt +(1|producer), df_6m)
Anova(lmer.prot)
ranova(lmer.prot)
#anova for the best model
plot(resid(lmer.prot))> abline(0, 0) #ok
anova_result <- anova(lmer.prot)
print(anova_result)
#posthoc
difflsmeans(lmer.prot, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.prot<-lmer(log10(Proteolytic) ~ time +(1|producer), df_c)
Anova(lmer.prot)
ranova(lmer.prot)
#anova for the best model
plot(resid(lmer.prot))> abline(0, 0) #ok
anova_result <- anova(lmer.prot)
print(anova_result)
#posthoc
difflsmeans(lmer.prot, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.prot<-lmer(log10(Proteolytic) ~ time +(1|producer), df_co)
Anova(lmer.prot)
ranova(lmer.prot)
#anova for the best model
plot(resid(lmer.prot))> abline(0, 0) #ok
anova_result <- anova(lmer.prot)
print(anova_result)
#posthoc
difflsmeans(lmer.prot, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.prot<-lmer(log10(Proteolytic) ~ time +(1|producer), df_b)
Anova(lmer.prot)
ranova(lmer.prot)
#anova for the best model
plot(resid(lmer.prot))> abline(0, 0) #ok
anova_result <- anova(lmer.prot)
print(anova_result)
#posthoc
difflsmeans(lmer.prot, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.prot<-lmer(log10(Proteolytic) ~ time +(1|producer), df_t)
Anova(lmer.prot)
ranova(lmer.prot)
#anova for the best model
plot(resid(lmer.prot))> abline(0, 0) #ok
anova_result <- anova(lmer.prot)
print(anova_result)
#posthoc
difflsmeans(lmer.prot, test.effs = "trt", ddf="Kenward-Roger")
#

#Cellulolytic
cell_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Cellulolytic), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("Cellulolytic, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
cell_box

#3m
lmer.cell<-lmer(log10(Cellulolytic) ~ trt +(1|producer), df_3m)
Anova(lmer.cell)
ranova(lmer.cell)
#anova for the best model
plot(resid(lmer.cell))> abline(0, 0) #ok
anova_result <- anova(lmer.cell)
print(anova_result)
#posthoc
difflsmeans(lmer.cell, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.cell<-lmer(log10(Cellulolytic) ~ trt +(1|producer), df_6m)
Anova(lmer.cell)
ranova(lmer.cell)
#anova for the best model
plot(resid(lmer.cell))> abline(0, 0) #ok
anova_result <- anova(lmer.cell)
print(anova_result)
#posthoc
difflsmeans(lmer.cell, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.cell<-lmer(log10(Cellulolytic) ~ time +(1|producer), df_c)
Anova(lmer.cell)
ranova(lmer.cell)
#anova for the best model
plot(resid(lmer.cell))> abline(0, 0) #ok
anova_result <- anova(lmer.cell)
print(anova_result)
#posthoc
difflsmeans(lmer.cell, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.cell<-lmer(log10(Cellulolytic) ~ time +(1|producer), df_co)
Anova(lmer.cell)
ranova(lmer.cell)
#anova for the best model
plot(resid(lmer.cell))> abline(0, 0) #ok
anova_result <- anova(lmer.cell)
print(anova_result)
#posthoc
difflsmeans(lmer.cell, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.cell<-lmer(log10(Cellulolytic) ~ time +(1|producer), df_b)
Anova(lmer.cell)
ranova(lmer.cell)
#anova for the best model
plot(resid(lmer.cell))> abline(0, 0) #ok
anova_result <- anova(lmer.cell)
print(anova_result)
#posthoc
difflsmeans(lmer.cell, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.cell<-lmer(log10(Cellulolytic) ~ time +(1|producer), df_t)
Anova(lmer.cell)
ranova(lmer.cell)
#anova for the best model
plot(resid(lmer.cell))> abline(0, 0) #ok
anova_result <- anova(lmer.cell)
print(anova_result)
#posthoc
difflsmeans(lmer.cell, test.effs = "trt", ddf="Kenward-Roger")
#


#Pseudomonas
pseudo_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Pseudomonas), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("Pseudomonas, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
pseudo_box

#3m
lmer.pseu<-lmer(log10(Pseudomonas) ~ trt +(1|producer), df_3m)
Anova(lmer.pseu)
ranova(lmer.pseu)
#anova for the best model
plot(resid(lmer.pseu))> abline(0, 0) #ok
anova_result <- anova(lmer.pseu)
print(anova_result)
#posthoc
difflsmeans(lmer.pseu, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.pseu<-lmer(log10(Pseudomonas) ~ trt +(1|producer), df_6m)
Anova(lmer.pseu)
ranova(lmer.pseu)
#anova for the best model
plot(resid(lmer.pseu))> abline(0, 0) #ok
anova_result <- anova(lmer.pseu)
print(anova_result)
#posthoc
difflsmeans(lmer.pseu, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.pseu<-lmer(log10(Pseudomonas) ~ time +(1|producer), df_c)
Anova(lmer.pseu)
ranova(lmer.pseu)
#anova for the best model
plot(resid(lmer.pseu))> abline(0, 0) #ok
anova_result <- anova(lmer.pseu)
print(anova_result)
#posthoc
difflsmeans(lmer.pseu, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.pseu<-lmer(log10(Pseudomonas) ~ time +(1|producer), df_co)
Anova(lmer.pseu)
ranova(lmer.pseu)
#anova for the best model
plot(resid(lmer.pseu))> abline(0, 0) #ok
anova_result <- anova(lmer.pseu)
print(anova_result)
#posthoc
difflsmeans(lmer.pseu, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.pseu<-lmer(log10(Pseudomonas) ~ time +(1|producer), df_b)
Anova(lmer.pseu)
ranova(lmer.pseu)
#anova for the best model
plot(resid(lmer.pseu))> abline(0, 0) #ok
anova_result <- anova(lmer.pseu)
print(anova_result)
#posthoc
difflsmeans(lmer.pseu, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.pseu<-lmer(log10(Pseudomonas) ~ time +(1|producer), df_t)
Anova(lmer.pseu)
ranova(lmer.pseu)
#anova for the best model
plot(resid(lmer.pseu))> abline(0, 0) #ok
anova_result <- anova(lmer.pseu)
print(anova_result)
#posthoc
difflsmeans(lmer.pseu, test.effs = "trt", ddf="Kenward-Roger")
#

#Bacillus
baci_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Bacillus), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("Bacillus, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
baci_box

#3m
lmer.bac<-lmer(log10(Bacillus) ~ trt +(1|producer), df_3m)
Anova(lmer.bac)
ranova(lmer.bac)
#anova for the best model
plot(resid(lmer.bac))> abline(0, 0) #ok
anova_result <- anova(lmer.bac)
print(anova_result)
#posthoc
difflsmeans(lmer.bac, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.bac<-lmer(log10(Bacillus) ~ trt +(1|producer), df_6m)
Anova(lmer.bac)
ranova(lmer.bac)
#anova for the best model
plot(resid(lmer.bac))> abline(0, 0) #ok
anova_result <- anova(lmer.bac)
print(anova_result)
#posthoc
difflsmeans(lmer.bac, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.bac<-lmer(log10(Bacillus) ~ time +(1|producer), df_c)
Anova(lmer.bac)
ranova(lmer.bac)
#anova for the best model
plot(resid(lmer.bac))> abline(0, 0) #ok
anova_result <- anova(lmer.bac)
print(anova_result)
#posthoc
difflsmeans(lmer.bac, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.bac<-lmer(log10(Bacillus) ~ time +(1|producer), df_co)
Anova(lmer.bac)
ranova(lmer.bac)
#anova for the best model
plot(resid(lmer.bac))> abline(0, 0) #ok
anova_result <- anova(lmer.bac)
print(anova_result)
#posthoc
difflsmeans(lmer.bac, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.bac<-lmer(log10(Bacillus) ~ time +(1|producer), df_b)
Anova(lmer.bac)
ranova(lmer.bac)
#anova for the best model
plot(resid(lmer.bac))> abline(0, 0) #ok
anova_result <- anova(lmer.bac)
print(anova_result)
#posthoc
difflsmeans(lmer.bac, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.bac<-lmer(log10(Bacillus) ~ time +(1|producer), df_t)
Anova(lmer.bac)
ranova(lmer.bac)
#anova for the best model
plot(resid(lmer.bac))> abline(0, 0) #ok
anova_result <- anova(lmer.bac)
print(anova_result)
#posthoc
difflsmeans(lmer.bac, test.effs = "trt", ddf="Kenward-Roger")
#

#Actinobacteria
act_box<-ggplot(f_data, aes(x = factor(time, levels=order), y = log10(Actinomycetes), fill = trt)) +
  geom_boxplot() +
  labs(x = " ", y = expression("Actinobacteria, log"[10]*"UFC g"^-1)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(5.2, 7.2)) +
  theme_bw() + 
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", family = "monospace"),
    axis.text.y = element_text(size = 11, color = "black", family = "monospace"),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed")
  )
act_box


#3m
lmer.act<-lmer(log10(Actinomycetes) ~ trt +(1|producer), df_3m)
Anova(lmer.act)
ranova(lmer.act)
#anova for the best model
plot(resid(lmer.act))> abline(0, 0) #ok
anova_result <- anova(lmer.act)
print(anova_result)
#posthoc
difflsmeans(lmer.act, test.effs = "trt", ddf="Kenward-Roger")

#6m
lmer.act<-lmer(log10(Actinomycetes) ~ trt +(1|producer), df_6m)
Anova(lmer.act)
ranova(lmer.act)
#anova for the best model
plot(resid(lmer.act))> abline(0, 0) #ok
anova_result <- anova(lmer.act)
print(anova_result)
#posthoc
difflsmeans(lmer.act, test.effs = "trt", ddf="Kenward-Roger")

#C
lmer.act<-lmer(log10(Actinomycetes) ~ time +(1|producer), df_c)
Anova(lmer.act)
ranova(lmer.act)
#anova for the best model
plot(resid(lmer.act))> abline(0, 0) #ok
anova_result <- anova(lmer.act)
print(anova_result)
#posthoc
difflsmeans(lmer.act, test.effs = "trt", ddf="Kenward-Roger")

#Co
lmer.act<-lmer(log10(Actinomycetes) ~ time +(1|producer), df_co)
Anova(lmer.act)
ranova(lmer.act)
#anova for the best model
plot(resid(lmer.act))> abline(0, 0) #ok
anova_result <- anova(lmer.act)
print(anova_result)
#posthoc
difflsmeans(lmer.act, test.effs = "trt", ddf="Kenward-Roger")


#B
lmer.act<-lmer(log10(Actinomycetes) ~ time +(1|producer), df_b)
Anova(lmer.act)
ranova(lmer.act)
#anova for the best model
plot(resid(lmer.act))> abline(0, 0) #ok
anova_result <- anova(lmer.act)
print(anova_result)
#posthoc
difflsmeans(lmer.act, test.effs = "trt", ddf="Kenward-Roger")

#T
lmer.act<-lmer(log10(Actinomycetes) ~ time +(1|producer), df_t)
Anova(lmer.act)
ranova(lmer.act)
#anova for the best model
plot(resid(lmer.act))> abline(0, 0) #ok
anova_result <- anova(lmer.act)
print(anova_result)
#posthoc
difflsmeans(lmer.act, test.effs = "trt", ddf="Kenward-Roger")
#


#Actinomycetes####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Actinomycetes,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520


hist(bact_6$Actinomycetes)
ggplot(bact_6, aes(x=Actinomycetes)) + 
  geom_histogram(bins = 30)

glm.Actinomycetes<-glm(log(Actinomycetes) ~ trt, family= gaussian(link="log"), data = bact_6)
Anova(lm.Actinomycetes)


lmer.Actinomycetes<-lmer(log(Actinomycetes) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Actinomycetes)
ranova(lmer.Actinomycetes)

lm.Actinomycetes_m<-lm(log(Actinomycetes) ~ trt * management, data = bact_6)
Anova(lm.Actinomycetes_m)

lm.Actinomycetes_s<-lm(log(Actinomycetes) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Actinomycetes_s)
#no interactions

#anova for the best model
report(lmer.Actinomycetes)
plot(resid(lmer.Actinomycetes))> abline(0, 0) #ok
bartlett.test(size ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedastica.
anova_result <- anova(lmer.Actinomycetes)
print(anova_result)

#posthoc
difflsmeans(lmer.Actinomycetes, test.effs = "trt", ddf="Kenward-Roger")
#b-b-ab-a

#Bacillus####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Bacillus,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520

lm.Bacillus<-lm(log(Bacillus) ~ trt, data = bact_6)
Anova(lm.Bacillus)

lmer.Bacillus<-lmer(log(Bacillus) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Bacillus)
ranova(lmer.Bacillus)

lm.Bacillus_m<-lm(log(Bacillus) ~ trt * management, data = bact_6)
Anova(lm.Bacillus_m)

lm.Bacillus_s<-lm(log(Bacillus) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Bacillus_s)
#no interactions

#anova for the best model
report(lmer.Bacillus)
plot(resid(lmer.Bacillus))> abline(0, 0) #ok
bartlett.test(Bacillus ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.Bacillus)
print(anova_result)

#posthoc
difflsmeans(lmer.Bacillus, test.effs = "trt", ddf="Kenward-Roger")
#b-a-a-b

#Cellulolytic####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Cellulolytic,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520

lm.Cellulolytic<-lm(log(Cellulolytic) ~ trt, data = bact_6)
Anova(lm.Cellulolytic)

lmer.Cellulolytic<-lmer(log(Cellulolytic) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Cellulolytic)
ranova(lmer.Cellulolytic)

lm.Cellulolytic_m<-lm(log(Cellulolytic) ~ trt * management, data = bact_6)
Anova(lm.Cellulolytic_m)
summary(lm.Cellulolytic_m)
Boxplot(bact_6$Cellulolytic, g=bact_6$management)

lm.Cellulolytic_s<-lm(log(Cellulolytic) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Cellulolytic_s)
#interaction with management

#anova for the best model
report(lmer.Cellulolytic)
plot(resid(lmer.Cellulolytic))> abline(0, 0) #ok
bartlett.test(Cellulolytic ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.Cellulolytic)
print(anova_result)

#posthoc
difflsmeans(lmer.Cellulolytic, test.effs = "trt", ddf="Kenward-Roger")
#a-b-b-a

#Proteolytic####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Proteolytic,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520

lm.Proteolytic<-lm(log(Proteolytic) ~ trt, data = bact_6)
Anova(lm.Proteolytic)

lmer.Proteolytic<-lmer(log(Proteolytic) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Proteolytic)
ranova(lmer.Proteolytic)

lm.Proteolytic_m<-lm(log(Proteolytic) ~ trt * management, data = bact_6)
Anova(lm.Proteolytic_m)

lm.Proteolytic_s<-lm(log(Proteolytic) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Proteolytic_s)
#no interactions

#anova for the best model
report(lmer.Proteolytic)
plot(resid(lmer.Proteolytic))> abline(0, 0) #ok
bartlett.test(Proteolytic ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.Proteolytic)
print(anova_result)

#posthoc
difflsmeans(lmer.Proteolytic, test.effs = "trt", ddf="Kenward-Roger")
#a-b-b-a

#Pseudomonas####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Pseudomonas,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520

lm.Pseudomonas<-lm(log(Pseudomonas) ~ trt, data = bact_6)
Anova(lm.Pseudomonas)

lmer.Pseudomonas<-lmer(log(Pseudomonas) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Pseudomonas)
ranova(lmer.Pseudomonas)

lm.Pseudomonas_m<-lm(log(Pseudomonas) ~ trt * management, data = bact_6)
Anova(lm.Pseudomonas_m)

lm.Pseudomonas_s<-lm(log(Pseudomonas) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Pseudomonas_s)
#no interactions

#anova for the best model
report(lmer.Pseudomonas)
plot(resid(lmer.Pseudomonas))> abline(0, 0) #ok
bartlett.test(Pseudomonas ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.Pseudomonas)
print(anova_result)

#posthoc
difflsmeans(lmer.Pseudomonas, test.effs = "trt", ddf="Kenward-Roger")
#b-ab-a-ab

#Phosphate_solubilizers####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Phosphate_solubilizers,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520

lm.Phosphate_solubilizers<-lm(log(Phosphate_solubilizers) ~ trt, data = bact_6)
Anova(lm.Phosphate_solubilizers)

lmer.Phosphate_solubilizers<-lmer(log(Phosphate_solubilizers) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Phosphate_solubilizers)
ranova(lmer.Phosphate_solubilizers)

lm.Phosphate_solubilizers_m<-lm(log(Phosphate_solubilizers) ~ trt * management, data = bact_6)
Anova(lm.Phosphate_solubilizers_m)

lm.Phosphate_solubilizers_s<-lm(log(Phosphate_solubilizers) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Phosphate_solubilizers_s)
#no interactions

#anova for the best model
report(lmer.Phosphate_solubilizers)
plot(resid(lmer.Phosphate_solubilizers))> abline(0, 0) #ok
bartlett.test(Phosphate_solubilizers ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.Phosphate_solubilizers)
print(anova_result)

#posthoc
difflsmeans(lmer.Phosphate_solubilizers, test.effs = "trt", ddf="Kenward-Roger")
#c-b-a-a

#Nitrogen_fixers####
### Plot
interaction.plot(x.factor     = bact_6$producer,
                 trace.factor = bact_6$trt,
                 response     = bact_6$Nitrogen_fixers,
                 fun = mean,
                 type="b",
                 #col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 col=c("black", "orange","#696969","#1E90FF","orange"),
                 pch=c(15, 17,16,18),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in bact_6
                 legend=FALSE, #True para ppt
                 lwd=3,
                 leg.bty = "o")
#printed 500 x 520

lm.Nitrogen_fixers<-lm(log(Nitrogen_fixers) ~ trt, data = bact_6)
Anova(lm.Nitrogen_fixers)

lmer.Nitrogen_fixers<-lmer(log(Nitrogen_fixers) ~ trt +(1|producer), data = bact_6)
Anova(lmer.Nitrogen_fixers)
ranova(lmer.Nitrogen_fixers)

lm.Nitrogen_fixers_m<-lm(log(Nitrogen_fixers) ~ trt * management, data = bact_6)
Anova(lm.Nitrogen_fixers_m)

lm.Nitrogen_fixers_s<-lm(log(Nitrogen_fixers) ~ trt * SOM_cat, data = bact_6)
Anova(lm.Nitrogen_fixers_s)
#no interactions

#anova for the best model
report(lmer.Nitrogen_fixers)
plot(resid(lmer.Nitrogen_fixers))> abline(0, 0) #ok
bartlett.test(Nitrogen_fixers ~ trt, data = bact_6) # Si el valor p de esta prueba es menor que 0,05, entonces hay evidencia de varianza no homocedasticidad.
anova_result <- anova(lmer.Nitrogen_fixers)
print(anova_result)


#####

#4 Now, I will see how physicochemical and biological soil variables correlate?#####
# I decided not to use PLS-PM bc sampling was not performed accordingly

#Run Step 0, and data preparation..
str(bact_6)

mean_bact_6 <- bact_6 %>%
  aggregate(. ~ trt + producer, FUN = mean) %>%
  arrange(trt,producer)

mean_soil_6 <- soil_6 %>%
  arrange(treatment,producer)

head(mean_prod)
head(mean_phytoc)
head(mean_soil_6)
head(mean_bact_6)


str(mean_prod)
mean_prod_n <- mean_prod[,-c(1:2,4:5)]
str(mean_prod_n)
str(mean_phytoc)
mean_phytoc_n <- mean_phytoc[,-c(1:3)]
str(mean_phytoc_n)
str(mean_soil_6)
mean_soil_6_n<-mean_soil_6[,-c(1:2,4:7,13,14,16,18:19,21:25)]
mean_soil_6_n <- mean_soil_6_n %>%
  relocate(7,.before = 'CE') %>%
  relocate(5:10,.after= 'MI')

str(mean_soil_6_n)
str(mean_bact_6)
mean_bact_6_n<-mean_bact_6[,-c(1:6)]
str(mean_bact_6_n)
mean_bact_log<-(mean_bact_6_n) %>%
  lapply(function(x) log(x)) %>%
  as.data.frame() %>%
  relocate(1:3, .after = 'Cellulolytic')%>%
  relocate(7, .after = 'Pseudomonas') %>%
  relocate(7, .after = 'Nitrogen_fixers') %>%
  relocate(1:4, .after = 'Proteolytic')
  

str(mean_bact_log)

mean_tomato<-cbind(mean_prod_n,mean_phytoc_n)
mean_tomato_n<-mean_tomato %>%
      relocate(7, .after = 'diameter') %>%
      relocate(6, .after = 'phenol') %>%
      relocate(11:12,.after = 'diameter') 
str(mean_tomato_n)      


# soil_0_ctrl_pc<-soil_0_ctrl[,c(1:9)]
# soil_0_ctrl_bio<-soil_0_ctrl[,-c(1:9)]
# soil_6_ctrl_pc<-soil_6_ctrl[,c(1:8,16)]
# soil_6_ctrl_bio<-soil_6_ctrl[,c(9:15)]

# Compute the covariance matrix
cor_soil <- cor(mean_bact_log,mean_soil_6_n)
cor_soil
# #if significance can be seen, data can be exported 
sign_cor_soil<-cbind(mean_bact_6_n,mean_soil_6_n)
sign_cor_soil
write.xlsx(cor_soil, "cor_soil.xlsx")
write.xlsx(sign_cor_soil, "sign_cor_soil.xlsx")
# Plot it with corrplot
corrplot(cor_soil, method = "circle", order = 'original') 
#700x500

#Now soil vs plant/fruit
soil_all<-cbind(mean_soil_6_n,mean_bact_6_n)
soil_all
plant_all<-cbind(mean_prod_n,mean_phytoc_n)
cor_soil_plant <- cor(soil_all,plant_all)
write.xlsx(cor_soil_plant, "corr.xlsx")
# #if significance can be seen, data can be exported 
sign_all<-cbind(soil_all,plant_all)
write.xlsx(sign_all, "sign_all.xlsx")
# Plot it with corrplot
corrplot(cor_soil_plant, method = "circle", order = 'original') 
#700*1500

#Several variables correlated well among soil and plant/fruit.

#####
#Variance partitioning analysis
{
  mean_soil_6_n #24
  mean_bact_6_n #24
  soil_bact3
  mean_prod_n #24
  mean_phytoc_n #24
  
  # Partition the variation in fish community composition
  library(vegan)
  #spe.part.all <- varpart(mean_prod_n, mean_soil_6_n,  soil_bact3)
  spe.part.all <- varpart(mean_prod_n, mean_soil_6_n,  mean_bact_6_n)
  spe.part.all$part  # access results!
  
  # plot the variation partitioning Venn diagram
  plot(spe.part.all,
       Xnames = c("Soil", "Bact"), # name the partitions
       bg = c("seagreen3", "mediumpurple"), alpha = 80, # colour the circles
       digits = 2, # only show 2 digits
       cex = 1.5)
  
  # [a+c] Phys-chem without controlling for Biol
  anova.cca(rda(mean_prod_n, mean_soil_6_n))
  anova.cca(rda(mean_prod_n, mean_bact_6_n))
  
  #ahora phytoc
  spe.part.all <- varpart(mean_phytoc_n, mean_soil_6_n, mean_bact_6_n)
  spe.part.all$part  # access results!
  
  # plot the variation partitioning Venn diagram
  plot(spe.part.all,
       Xnames = c("Soil", "Bact"), # name the partitions
       bg = c("seagreen3", "mediumpurple"), alpha = 80, # colour the circles
       digits = 2, # only show 2 digits
       cex = 1.5)

  # [a+c] Phys-chem without controlling for Biol
  anova.cca(rda(mean_phytoc_n, mean_soil_6_n))
  anova.cca(rda(mean_phytoc_n, mean_bact_6_n))
   
}



# 2.1- Variables correlation######
#Debo achicar el n de variables

tomato_soil<-cbind(mean_tomato_n,mean_soil_6_n)
tomato_bact<-cbind(mean_tomato_n, mean_bact_log)

#tomato vs soil physicochemical####
datapca<-tomato_soil
#correlation
data.cor <- cor(datapca)
# assign color
data.color <- dmat.color(data.cor)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr");
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r = ", txt, sep = "")
  
  # try to set background here, but it doesn't affect the output
  if (r > 0.5)
    par(bg = "white")
  
  text(0.5, 0.6, txt)
  
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p = ", txt2, sep = "")
  if (p < 0.01) txt2 <- "p < 0.01"
  text(0.5, 0.4, txt2)
}

cpairs(datapca,border.color="black",upper.panel=panel.cor)


chart.Correlation(datapca, histogram=TRUE, pch=19)

#PCA tomato_soil#####
data.pca<-PCA(datapca, scale.unit = TRUE) #PCA from FactoMineR
#PCA from prod vs soil physicochemical variables
# printed at 600 x 550

str(datapca)
str(data.pca) 

eigenvalues <- data.pca$eig
head(eigenvalues[, 1:2])

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="black")

lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

head(data.pca$var$coord)
head(data.pca$var$cos2) 
head(data.pca$var$contrib) 
plot(data.pca, choix = "var", )

fviz_pca_var(data.pca, col.var="black")+ theme(
  panel.background = element_rect(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  axis.text = element_blank(), text = element_text(size=15)
)

pc<-princomp(na.omit(datapca), cor = TRUE,scores=TRUE) 
summary(pc)
pc$loadings[,1:6]
pc$loadings
loadings(pc) 
data.pca$var$coord  
# pending analysis....

# Compute the covariance matrix
heat <- cor(mean_soil_6_n,mean_tomato_n)
heat
# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#662 x 581

#tomato vs soil biology#####
datapca<-tomato_bact

str(tomato_bact)
#correlation
data.cor <- cor(datapca)
# assign color
data.color <- dmat.color(data.cor)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr");
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r = ", txt, sep = "")
  
  # try to set background here, but it doesn't affect the output
  if (r > 0.5)
    par(bg = "white")
  
  text(0.5, 0.6, txt)
  
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p = ", txt2, sep = "")
  if (p < 0.01) txt2 <- "p < 0.01"
  text(0.5, 0.4, txt2)
}

cpairs(datapca,border.color="black",upper.panel=panel.cor)


chart.Correlation(datapca, histogram=TRUE, pch=19)

#PCA tomato_bact#####
data.pca<-PCA(datapca, scale.unit = TRUE) #PCA from FactoMineR
#PCA from prod vs soil physicochemical variables
# printed at 600 x 550

str(datapca)
str(data.pca) 

eigenvalues <- data.pca$eig
head(eigenvalues[, 1:2])

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="black")

lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

head(data.pca$var$coord)
head(data.pca$var$cos2) 
head(data.pca$var$contrib) 
plot(data.pca, choix = "var", )

fviz_pca_var(data.pca, col.var="black")+ theme(
  panel.background = element_rect(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  axis.text = element_blank(), text = element_text(size=15)
)

pc<-princomp(na.omit(datapca), cor = TRUE,scores=TRUE) 
summary(pc)
pc$loadings[,1:6]
pc$loadings
loadings(pc) 
data.pca$var$coord  
# pending analysis....

# Compute the covariance matrix
heat <- cor(mean_bact_log,mean_tomato_n)
heat
# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#662 x 581


#Agronomic assessment
library(writexl)
head(mean_prod)
head(mean_phytoc)
View(mean_soil_6)
View(mean_bact_6)

write_xlsx(mean_bact_6, "mean_bact_6.xlsx")
write_xlsx(mean_prod, "mean_prod.xlsx")
write_xlsx(mean_phytoc, "mean_phytoc.xlsx")

agr <- read_excel("agronomic_assessment.xlsx", sheet = "Sheet3")
agr<-agr[,c(1:14)]
str(agr)

# Fit linear model
model <- lm(PI ~ SQ, data = agr)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Extract R-square and p-value
rsq <- signif(summary(model)$r.squared, digits = 2)
p_value <- signif(summary(model)$coefficients[2, 4], digits = 2)

# Define colors
colors <- c("black", "orange", "#696969", "#1E90FF")

# Create scatterplot with circle size
ggplot(agr, aes(SQ, PI, size = 2, color = trt, shape= Producer)) +
  geom_point() +
 # geom_smooth(data = filter(agr, Group == 1), method = "lm", se = FALSE, color = "blue") +  # Add single linear trend line
  scale_size_continuous(range = c(1, 4)) +
  scale_shape_manual(values = c(0, 1, 2, 15, 16, 17)) +
  scale_color_manual(values = colors) +
  labs(x = "Soil Quality",
       y = "Yield (kg)") +
  theme_bw() +
  coord_cartesian(xlim = c(0.5, 0.92), ylim = c(15, 18.2)) +
  geom_text(x = 0.65, y = 17.8, 
            label = paste("R-square =", rsq, "\nP-value =", p_value),
            hjust = 1, vjust = 0, size = 3.5, color = "black") +
  theme(text = element_text(family = "Helvetica"))

#550*442

ggplot(agr, aes(x = SQ, y = PI, size=2)) +
  geom_smooth(method = lm) + theme_bw() +
  coord_cartesian(xlim = c(0.5, 0.92), ylim = c(15, 18.2)) 

#Ahora para fitoconstituyentes
#phenol---------------
# Fit linear model
model <- lm(phenol ~ SQ, data = agr)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Extract R-square and p-value
rsq <- signif(summary(model)$r.squared, digits = 2)
p_value <- signif(summary(model)$coefficients[2, 4], digits = 2)

# Create scatterplot with circle size
ggplot(agr, aes(SQ, phenol, size = 2, color = trt, shape= Producer)) +
  geom_point() +
  #geom_smooth(data = filter(agr, Group == 1), method = "lm", se = FALSE, color = "blue") +  # Add single linear trend line
  scale_size_continuous(range = c(1, 4)) +
  scale_shape_manual(values = c(0, 1, 2, 15, 16, 17)) +
  scale_color_manual(values = colors) +
  labs(x = "Soil Quality",
       y = "Phenol concentration ") +
  theme_bw() +
  coord_cartesian(xlim = c(0.45, 0.92), ylim = c(30, 50)) +
  geom_text(x = 0.90, y = 48, 
            label = paste("R-square =", rsq, "\nP-value =", p_value),
            hjust = 1, vjust = 0, size = 3.5, color = "black") +
  theme(text = element_text(family = "Helvetica"))

#550*442

ggplot(agr, aes(x = SQ, y = phenol, size=2)) +
  geom_smooth(method = lm) + theme_bw() +
coord_cartesian(xlim = c(0.45, 0.92), ylim = c(30, 50))

#flav---------------------

# Fit linear model
model <- lm(flav ~ SQ, data = agr)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Extract R-square and p-value
rsq <- signif(summary(model)$r.squared, digits = 2)
p_value <- signif(summary(model)$coefficients[2, 4], digits = 2)

# Create scatterplot with circle size
ggplot(agr, aes(SQ, flav, size = 2, color = trt, shape= Producer)) +
  geom_point() +
  #geom_smooth(data = filter(agr, Group == 1), method = "lm", se = FALSE, color = "blue") +  # Add single linear trend line
  scale_size_continuous(range = c(1, 4)) +
  scale_shape_manual(values = c(0, 1, 2, 15, 16, 17)) +
  scale_color_manual(values = colors) +
  labs(x = "Soil Quality",
       y = "Flavonoid concentration") +
  theme_bw() +
  coord_cartesian(xlim = c(0.45, 0.92), ylim = c(12, 22.5)) +
  geom_text(x = 0.65, y = 21, 
            label = paste("R-square =", rsq, "\nP-value =", p_value),
            hjust = 1, vjust = 0, size = 3.5, color = "black") +
  theme(text = element_text(family = "Helvetica"))

#550*442

ggplot(agr, aes(x = SQ, y = flav, size=2)) +
  geom_smooth(method = lm) + theme_bw() +
  coord_cartesian(xlim = c(0.45, 0.92), ylim = c(12, 22.5))  

#DPPH
# Fit linear model
model <- lm(anthraq ~ SQ, data = agr)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Extract R-square and p-value
rsq <- signif(summary(model)$r.squared, digits = 2)
p_value <- signif(summary(model)$coefficients[2, 4], digits = 2)

# Create scatterplot with circle size
ggplot(agr, aes(SQ, anthraq, size = 2, color = trt, shape= Producer)) +
  geom_point() +
  #geom_smooth(data = filter(agr, Group == 1), method = "lm", se = FALSE, color = "blue") +  # Add single linear trend line
  scale_size_continuous(range = c(1, 4)) +
  scale_shape_manual(values = c(0, 1, 2, 15, 16, 17)) +
  scale_color_manual(values = colors) +
  labs(x = "Soil Quality",
       y = "anthraqonoid concentration") +
  theme_bw() +
  #coord_cartesian(xlim = c(0.45, 0.92), ylim = c(12, 22.5)) +
  geom_text(x = 0.65, y = 21, 
            label = paste("R-square =", rsq, "\nP-value =", p_value),
            hjust = 1, vjust = 0, size = 3.5, color = "black") +
  theme(text = element_text(family = "Helvetica"))
p_value
#550*442

ggplot(agr, aes(x = SQ, y = anthraq, size=2)) +
  geom_smooth(method = lm) + theme_bw() +
  coord_cartesian(xlim = c(0.45, 0.92), ylim = c(12, 22.5))  

#lycopene, brix, 
