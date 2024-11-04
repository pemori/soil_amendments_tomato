setwd("D:\\Users\\pedro\\Downloads\\Estudio FIC")

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

#data preparation####
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

#soil_var
data<- read_excel("data_tomato2.xlsx", sheet='soil_var')

names <- c('farmer','producer','treatment','management')
data[,names] <- lapply(data[,names] , as.factor)

str(data)
data
mean_soil_var <- data %>%
  select(-5,-12,-24) %>%
  arrange(treatment,producer)

str(mean_soil_var) #perfect
soil_var0<-mean_soil_var %>% dplyr::select(6:16)
soil_mean_var0<-mean_soil_var %>% 
  #dplyr::filter(treatment=='0_Control') %>%
  aggregate(. ~ producer, FUN = mean)

soil_mean_var0
soil_var0
str(soil_var0)
soil_var6<-mean_soil_var %>% dplyr::select(1,5,17:24)
soil_mean_var6<-soil_var6 %>%
  aggregate(. ~  producer + treatment, FUN = mean)
View(soil_var6)
#View(mean_soil_var)
KK<-soil_var6$K_av_6*390 / 10
KK

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
soil_bact6<-mean_soil_bact %>% dplyr::select(18:24)
str(soil_bact6)

soil_bact0

all <- cbind (mean_prod,mean_phytoc,mean_soil_var,mean_soil_bact)
tomato<- cbind(mean_prod,mean_phytoc)
tomato <-tomato %>% select(-3,-7,-8,-9)
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

#index
#average_values
#mean_phytoc: tomato phytoconstituents
#mean_prod: tomato productivity or condition
#mean_soil_bact: 
#mean_soil_var:
#tomato_num:
#soil_num:
#soil_var:

# 0 - Report mean and SE of soil and plant/fruit results

# 1- What soil variables modulate tomato productivity & quality?
#Let's see what is the relation among soil and plant/fruits variables considering the initial condition of soils
soil_0 <-cbind(fct,soil_bact0,soil_var0)
soil_0
soil_6_f<-cbind(soil_6, fct)
soil_6_f
soil_6_ctrl <- soil_6_f %>%
  filter(treatment== '0_Control') %>%
  select(-c(16:17,19:20)) %>%
  relocate(14,.after = 'Nitrogen_fixers_6') %>%
  relocate(9:11, .before = 'MI')
str(soil_6_ctrl)

soil_0_ctrl <- soil_0 %>%
  filter(treatment== '0_Control') %>%
  select(-c(1:2,4:5,21:22)) %>%
  relocate(c(1:8), .after = 'clay') %>%
  relocate('Phosphate _solubilizer_0',.after = 'Nitrogen_fixers_0') %>%
  relocate(11:13, .after = 'Phosphate _solubilizer_0') %>%
  relocate('MI', .after = 'Pseudomonas_0') %>%
  select(-17)
soil_0
soil_0_ctrl
tomato_ctrl<- tomato %>%
  filter(treatment== '0_control') %>%
  select(-c(1:2,4)) %>%
  relocate(c(9:10), .after = 'diameter') %>%
  relocate(7,6,.before = 'anthraq')
tomato_ctrl
soil_6_ctrl
soil_0_ctrl
tomato

# 1.1 Tomato productivity ####
data<- read_excel("data_tomato2.xlsx", sheet='prod')

names <- c('farmer','producer','treatment','management','SOM_cat')
data[,names] <- lapply(data[,names] , as.factor)

str(data)

#N° of fruits in two clusters####
### Plot
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$n_fruits,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=TRUE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$diameter,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$fruit_weight_g,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=TRUE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$hardness_kg_per_cm2,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$phenol,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$flav,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$anthraq,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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

#DPPH####
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$DPPH,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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
interaction.plot(x.factor     = data$treatment,
                 trace.factor = data$producer,
                 response     = data$brix,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),#SOM_cat
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of management
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
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



###########################################



#Now, I will see how physochemical and biological soil variables correlate?#####
soil_0_ctrl_pc<-soil_0_ctrl[,c(1:9)]
soil_0_ctrl_bio<-soil_0_ctrl[,-c(1:9)]
soil_6_ctrl_pc<-soil_6_ctrl[,c(1:8,16)]
soil_6_ctrl_bio<-soil_6_ctrl[,c(9:15)]

# Compute the covariance matrix
heat <- cor(soil_0_ctrl_pc,soil_0_ctrl_bio)
heat
ctrl<-cbind(soil_0_ctrl_pc,soil_0_ctrl_bio)
write.xlsx(ctrl, "soil_0_ctrl.xlsx")

# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#700x500

# Compute the covariance matrix
heat <- cor(soil_6_ctrl_pc,soil_6_ctrl_bio)
heat
ctrl<-cbind(soil_6_ctrl_pc,soil_6_ctrl_bio)
write.xlsx(ctrl, "soil_6_ctrl.xlsx")

# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#700*500

#Now soil vs plant/fruit
# Compute the covariance matrix for month 0
heat <- cor(tomato_ctrl,soil_0_ctrl)
heat
# ctrl<-cbind(tomato_ctrl,soil_0_ctrl)
# write.xlsx(ctrl, "tom_soil_0.xlsx")
# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#1200*900

# What soil variables at the harvest time of the control group are significant on plant/fruit responses? 

# Compute the covariance matrix for month 6
heat <- cor(tomato_ctrl,soil_6_ctrl)
heat
# ctrl<-cbind(tomato_ctrl,soil_6_ctrl)
# write.xlsx(ctrl, "tom_soil_6.xlsx")
# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#1170*900

#Several variables correlated well among soil and plant/fruit.


#Now, what is the effect of treatments on soil responses? #####

soil<- read_excel("data_soil.xlsx", sheet='soil')

names <- c('farmer','producer','treatment','management','condition','N_cat','P_cat','K_cat','SOM_cat','clay_cat')
soil[,names] <- lapply(soil[,names] , as.factor)
soil_0<- soil %>% 
  filter(condition== 'pre-treatment')
str(soil_0)

soil_bal<- read_excel("data_soil.xlsx", sheet='soil_bal')
str(soil)
names <- c('farmer','producer','treatment','management','condition')
soil_bal[,names] <- lapply(soil_bal[,names] , as.factor)
str(soil_bal)

soil_data<-cbind(soil,soil_bal)
(soil_data)
#View(soil_data)
soil_data2<-soil_data[,-c(5,6,13,14,18:19,27:33,39,43:47)]
View(soil_data2)

#pH####
### Plot
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$pH,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.pH<-lm(pH ~ treatment, data=soil_data2)
Anova(lm.pH)

lmer.pH<-lmer(pH ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.pH)
ranova(lmer.pH)

lm.pH_m<-lm(pH ~ treatment * management, data=soil_data2)
Anova(lm.pH_m)

lm.pH_s<-lm(pH ~ treatment * SOM_cat, data=soil_data2)
Anova(lm.pH_s)

#anova for the best model
report(lmer.pH)
plot(resid(lmer.pH))> abline(0, 0) #ok
anova_result <- anova(lmer.pH)
print(anova_result)

#posthoc
difflsmeans(lmer.pH, test.effs = "treatment", ddf="Kenward-Roger")

#N_t####
### Plot
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$N_tot,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.N<-lm(N_tot ~ treatment, data=soil_data2)
Anova(lm.N)

lmer.N_t<-lmer(N_tot ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.N_t)
ranova(lmer.N_t)

lm.N_tm<-lm(N_tot ~ treatment * management, data=soil_data2)
Anova(lm.N_tm)

lm.N_ts<-lm(N_tot ~ treatment * SOM_cat, data=soil_data2)
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
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$P_av,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.P_av<-lm(P_av ~ treatment, data=soil_data2)
Anova(lm.P_av)

lmer.P_av<-lmer(P_av ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.P_av)
ranova(lmer.P_av)

lm.P_av_m<-lm(P_av ~ treatment *management, data=soil_data2)
Anova(lm.P_av_m)

lm.P_av_s<-lm(P_av ~ treatment *SOM_cat, data=soil_data2)
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
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$K_av,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.K_av<-lm(K_av ~ treatment, data=soil_data2)
Anova(lm.K_av)

lmer.K_av<-lmer(K_av ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.K_av)
ranova(lmer.K_av)

lm.K_av_m<-lm(K_av ~ treatment * management, data=soil_data2)
Anova(lm.K_av_m)

lm.K_av_s<-lm(K_av ~ treatment * SOM_cat, data=soil_data2)
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
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$SOM,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.SOM<-lm(SOM ~ treatment, data=soil_data2)
Anova(lm.SOM)

lmer.SOM<-lmer(SOM ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.SOM)
ranova(lmer.SOM)

lm.SOM_m<-lm(SOM ~ treatment * management, data=soil_data2)
Anova(lm.SOM_m)

lm.SOM_s<-lm(SOM ~ treatment * SOM_cat, data=soil_data2)
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
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$CN,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.CN<-lm(CN ~ treatment, data=soil_data2)
Anova(lm.CN)

lmer.CN<-lmer(CN ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.CN)
ranova(lmer.CN)

lm.CN_m<-lm(CN ~ treatment * management, data=soil_data2)
Anova(lm.CN_m)

lm.CN_s<-lm(CN ~ treatment * SOM_cat, data=soil_data2)
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
interaction.plot(x.factor     = soil_data2$treatment,
                 trace.factor = soil_data2$producer,
                 response     = soil_data2$CIC,
                 fun = mean,
                 type="b",
                 col=c("#696969","#1E90FF","#696969","#1E90FF","#1E90FF","#1E90FF"),
                 #col=c("#1E90FF", "#696969","gold","#696969","#1E90FF","gold"),
                 pch=c(15, 17,17,17, 15,17),     ### Symbols for levels of P_av
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=TRUE, #True para ppt
                 lwd=2,
                 leg.bty = "o")
#printed 500 x 520
lm.CIC<-lm(CIC ~ treatment, data=soil_data2)
Anova(lm.CIC)

lmer.CIC<-lmer(CIC ~ treatment +(1|producer), data=soil_data2)
Anova(lmer.CIC)
ranova(lmer.CIC)

lm.CIC_m<-lm(CIC ~ treatment * management, data=soil_data2)
Anova(lm.CIC_m)

lm.CIC_s<-lm(CIC ~ treatment * SOM_cat, data=soil_data2)
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
# 2.1- Variables correlation######
#


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

#PCA#####
data.pca<-PCA(datapca, scale.unit = TRUE) #PCA from FactoMineR
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
pc$loadings[,1:3]
pc$loadings
loadings(pc) 
data.pca$var$coord  

# Compute the covariance matrix
heat <- cor(tomato_num,soil_6)
heat
# Plot it with corrplot
corrplot(heat, method = "circle", order = 'original') 
#662 x 581