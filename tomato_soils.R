setwd("D:\\Users\\pedro\\Downloads\\Estudio FIC")
library(readxl)


#OPCION 2 GLMER DE TODO CON SITE COMO RANDOM
library(emmeans)
library(multcomp)
library(multcompView)
library(lmerTest)
library(car)

# if(!require(psych)){install.packages("psych")}
# if(!require(FSA)){install.packages("FSA")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(car)){install.packages("car")}
# if(!require(multcompView)){install.packages("multcompView")}
# if(!require(lsmeans)){install.packages("lsmeans")}
# if(!require(rcompanion)){install.packages("rcompanion")}

data<- read_excel("data_soil.xlsx", sheet="soil_bal")
names <- c('farmer','management','producer','trt','SOM_level')
data[,names] <- lapply(data[,names] , as.factor)
names <- c('N_tot','P_av','K_av','CE','SOM','pH','CIC','CN')
data[,names] <- lapply(data[,names] , as.numeric)
control<-"Control"
library(dplyr)
dat <- data %>%
  arrange(trt != control, trt)
str(dat)
head(data)
head(dat)

#N_tot
lmer.N<-lmer(N_tot ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.N)
library(report)
report(lmer.N)
lmer.N2<-lm(N_tot ~ trt,data=dat) #family=Gamma(link="log"), data=data)#
anova(lmer.N,lmer.N2, test='F')
ranova(lmer.N)
plot(resid(lmer.N))> abline(0, 0)
plot(lmer.N) #Ok
anova_result <- anova(lmer.N,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.N, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$CN,
                 fun = mean,
                 type="b",
                 col=c("blue","red","yellow","red","blue","yellow"),
                       ###"#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of trace var.
                 pch=c(15, 1, 15,1,1,1),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=TRUE, #True para ppt
                 leg.bty = "o")

#printed in 500*520. Tukey results: a-a-a-a

#P_av
lmer.P<-lmer(P_av ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.P)
library(report)
report(lmer.P)
ranova(lmer.P)
plot(resid(lmer.P))> abline(0, 0)
plot(lmer.P) #Ok
anova_result <- anova(lmer.P,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.P, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$P_av,
                 fun = mean,
                 type="b",
                 col=c("#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of trace var.
                 pch=c(0, 1, 2,5,15,16,17,18),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 leg.bty = "o")
#printed in 500*520. Tukey results: ab-ab-b-a

#K_av
lmer.K<-lmer(K_av ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.K)
report(lmer.K)
ranova(lmer.K)
plot(resid(lmer.K))> abline(0, 0)
plot(lmer.K) #Ok
anova_result <- anova(lmer.K,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.K, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$K_av,
                 fun = mean,
                 type="b",
                 col=c("#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of trace var.
                 pch=c(0, 1, 2,5,15,16,17,18),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 leg.bty = "o")
#printed in 500*520. Tukey results: a-a-a-a


#SOM
lmer.SOM<-lmer(SOM ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.SOM)
report(lmer.SOM)
ranova(lmer.SOM)
plot(resid(lmer.SOM))> abline(0, 0)
plot(lmer.SOM) #Ok
anova_result <- anova(lmer.SOM,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.SOM, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$SOM,
                 fun = mean,
                 type="b",
                 col=c("#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of traSOM var.
                 pch=c(0, 1, 2,5,15,16,17,18),             ### Symbols for levels of traSOM var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 leg.bty = "o")
#printed in 500*520. Tukey results: a-a-a-a


#pH
lmer.pH<-lmer(pH ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.pH)
report(lmer.pH)
ranova(lmer.pH)
plot(resid(lmer.pH))> abline(0, 0)
plot(lmer.pH) #Ok
anova_result <- anova(lmer.pH,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.pH, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$pH,
                 fun = mean,
                 type="b",
                 col=c("#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of trapH var.
                 pch=c(0, 1, 2,5,15,16,17,18),             ### Symbols for levels of trapH var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 leg.bty = "o")
#printed in 500*520. Tukey results: a-a-a-a


#CIC
lmer.CIC<-lmer(CIC ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.CIC)
report(lmer.CIC)
ranova(lmer.CIC)
plot(resid(lmer.CIC))> abline(0, 0)
plot(lmer.CIC) #Ok
anova_result <- anova(lmer.CIC,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.CIC, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$CIC,
                 fun = mean,
                 type="b",
                 col=c("#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of traCIC var.
                 pch=c(0, 1, 2,5,15,16,17,18),             ### Symbols for levels of traCIC var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 leg.bty = "o")
#printed in 500*520. Tukey results: a-a-a-a

#CN
lmer.CN<-lmer(CN ~ trt + (1|producer),data=dat) #family=Gamma(link="log"), data=data)#
summary(lmer.CN)
report(lmer.CN)
ranova(lmer.CN)
plot(resid(lmer.CN))> abline(0, 0)
plot(lmer.CN) #Ok
anova_result <- anova(lmer.CN,type=2)
# Realizar el test de comparaciones múltiples (post hoc)
posthoc_result <- glht(lmer.CN, linfct = mcp(trt = "Tukey")) #trt se refiere a mi trt... no es una fx

# Imprimir los resultados del ANOVA
print(anova_result)

# Imprimir los resultados del post hoc
print(summary(posthoc_result))

### Plot

library(ggplot2)

interaction.plot(x.factor     = dat$trt,
                 trace.factor = dat$producer,
                 response     = dat$CN,
                 fun = mean,
                 type="b",
                 col=c("#1f78b4", "#a6cee3", "#b2b2b2", "#33a02c", "#b2df8a", "#fdbf6f", "#ff7f00", "#cab2d6"),  ### Colors for levels of traCN var.
                 pch=c(0, 1, 2,5,15,16,17,18),             ### Symbols for levels of traCN var.
                 fixed=TRUE,                    ### Order by factor order in data
                 legend=FALSE, #True para ppt
                 leg.bty = "o")
#printed in 500*520. Tukey results: a-a-a-a

#PCA usando 8 datos

data<- read_excel("data_soil.xlsx", sheet="Sheet1")
head(data)
names <- c('farmer')
data[,names] <- lapply(data[,names] , as.factor)
names <- c('P_av','N_tot','SOM','CN','Azotobacter','Actinomycetes','Cellulolytic','Proteolytic')
data[,names] <- lapply(data[,names] , as.numeric)
data <- as.data.frame(data)
head(data)

library(factoextra)
library(FactoMineR)
library(dplyr)
datapca<-data %>%
  dplyr::select(P_av,N_tot,SOM,CN,Azotobacter,Actinomycetes,Cellulolytic,Proteolytic) 
datapca
datapca <- datapca[-8,]

library(ggplot2)
library(gclus)

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

library("PerformanceAnalytics")
chart.Correlation(datapca, histogram=TRUE, pch=19)

#PCA
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


#How many components to keep? Using Kaiser criteria, I only study those variables with eigenvalue > 1.
pc$sdev^2 #eigenvalue is equal to the square of standard deviation. Thus, I keep 3 PCs.
screeplot(pc, type="lines")

# Contributions of variables to PC1 
fviz_contrib(pc, choice = "var", axes = 1, top = 10) #P,K, Ca, Zn
# Contributions of variables to PC2
fviz_contrib(pc, choice = "var", axes = 2, top = 10) #Mg, Cu, Zn
# Contributions of variables to PC3
fviz_contrib(pc, choice = "var", axes = 3, top = 10) #Mg, N, As

data.pca<-PCA(datapca) 
eig.val <- get_eigenvalue(data.pca)
eig.val
#Results for variable
var <-get_pca_var(data.pca)

# Coordinates #coordinates of variables to create a scatter plot
var$coord

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

data.frame(df, stringsAsFactors = TRUE)
Elem<-c('N','P','K','Ca','Mg','Cu','Zn','As')
Dim1<-c(0.5385263,0.7332571,0.7975871,-0.7014699,-0.1988976,-0.5836610,-0.6925797,0.3531289)
Dim2<-c(0.37022438,0.15489577,0.39481479,-0.29678822,-0.51964494,0.77411466,0.67771488,-0.05155387)
Dim3<-c(0.599339074,0.086895166,0.342502603,0.394504306,0.680746266,0.096715849,-0.004349719,-0.549610918)
df<-data.frame(Elem,Dim1,Dim2,Dim3)

#loadings
library(tidyverse)
ggplot(df, aes(
  x = fct_relevel(Elem,'N','P','K','Ca','Mg','Cu','Zn','As'),
  y = Dim1)) +
  geom_col()+
  xlab("Plant ionome") +
  ylab("Loadings PC-1") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 0, colour = "black"),
        axis.text.y = element_text(hjust = 0, colour = "black"))

ggplot(df, aes(
  x = fct_relevel(Elem,'N','P','K','Ca','Mg','Cu','Zn','As'),
  y = Dim2)) +
  geom_col()+
  xlab("Plant ionome") +
  ylab("Loadings PC-2") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 0, colour = "black"),
        axis.text.y = element_text(hjust = 0, colour = "black"))

ggplot(df, aes(
  x = fct_relevel(Elem,'N','P','K','Ca','Mg','Cu','Zn','As'),
  y = Dim3)) +
  geom_col()+
  xlab("Plant ionome") +
  ylab("Loadings PC-3") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 0, colour = "black"),
        axis.text.y = element_text(hjust = 0, colour = "black"))