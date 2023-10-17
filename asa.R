

library(readxl)
library(tidyverse)
library(ade4)
library(vegan)
library(ggplot2)
library(factoextra)
library(corrplot)
library(RVAideMemoire)
library("PerformanceAnalytics")

dflo <- read_excel("C:/Users/33763/Desktop/MODE/ASA - Advanced statictics/Projet/data_vegetation_french.xls",  
                                     sheet = "flo")


dmil <- read_excel("C:/Users/33763/Desktop/MODE/ASA - Advanced statictics/Projet/data_vegetation_french.xls", col_types = "numeric",

                                     sheet = "mil")

dcoord <- read_excel("C:/Users/33763/Desktop/MODE/ASA - Advanced statictics/Projet/data_vegetation_french.xls", col_types = "numeric",
                                     sheet = "coord")


dasso <- read_excel("C:/Users/33763/Desktop/MODE/ASA - Advanced statictics/Projet/data_vegetation_french.xls", 
                                     sheet = "association")


## DATA DESCRIPTION AND OBJECTIVES

# releve fleuristique

str(dflo)
summary(dflo)
colSums(is.na(dflo))
rowSums(is.na(dflo))


dflo2 = dflo!=0 #nombre de présence 

par(mfrow=c(2,2))
hist(colSums(dflo),col='blue')
hist(rowSums(dflo),col='blue')
hist(colSums(dflo2),col='blue')
hist(rowSums(dflo2),col='blue')



#dflo <- decostand(doubs$dflo[-8,], "hellinger")

## DATA EXPLORATION

# infos sur les milieux


str(dmil)
summary(dmil)
colSums(is.na(dmil))
rowSums(is.na(dmil))
# There is no missing value.

par(mfrow=c(2,4))
dotchart(dmil$Argile,pch=16,col='blue',xlab='Argile')
dotchart(dmil$Limon, pch=16,col='blue',xlab='Limon')
dotchart(dmil$K2O,pch=16,col='blue',xlab='K2O')
dotchart(dmil$`Mg++`,pch=16,col='blue',xlab='Mg++')
dotchart(dmil$`Na+/100g`,pch=16,col='blue',xlab='Na+/100g')
dotchart(dmil$`K+`, pch=16,col='blue',xlab='K+')
dotchart(dmil$Conduc,pch=16,col='blue',xlab='Conduc')
dotchart(dmil$Capa_Reten , pch=16,col='blue',xlab='Capa_Reten')


par(mfrow=c(2,4))
hist(dmil$Argile,pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$Limon, pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$K2O,pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$`Mg++`,pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$`Na+/100g`,pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$`K+`, pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$Conduc,pch=16,col='blue',xlab='CO2 Exchange')
hist(dmil$Capa_Reten , pch=16,col='blue',xlab='CO2 Exchange')


par(mfrow=c(1,1))
M <- cor(dmil)
corrplot(M, method = "number")

chart.Correlation(dmil,histogram=TRUE, pch=19)#usefull if few variables

str(dcoord)
plot(dcoord)


dmil_selected = dmil %>% 
  dplyr::select(-c("Argile", "Na+/100g", "Na+/l"))

chart.Correlation(dmil_selected,histogram=TRUE, pch=19)#usefull if few variables


# Standardisation ---------------------------------------------------------

dflo <- decostand(dflo, "hellinger")

dmil_selected = scale(dmil_selected)

selected_va_xy = cbind(dmil_selected, dcoord)

formule_pcca <- formula(paste("dflo ~", 
                              paste(names(selected_va_xy)[1:(length(selected_va_xy)-(length(dcoord)))], collapse = " + "), "+ Condition (", 
                              paste(names(selected_va_xy)[(length(selected_va_xy)-(length(dcoord)-1)):length(selected_va_xy)], collapse ="+"),")"))

pcca_model <- cca(formule_pcca, selected_va_xy)

#perform a partial CCA with space removing
cca=cca(dflo,dmil_selected, scan=F)
summary(cca)
vif.cca(cca) 

anova(cca) 
plot(cca)

#perform the CCA with selection of explanatory variables 
ccadoubs=cca(dflo~.,dmil_selected)
ordistep(ccadoubs,perm.max=500)

dmil_selected = dmil_selected[, c("Limon","Sable", "Mg++",
                                       "K+", "Capa_Reten", "Altitude")]


cca2=cca(dflo,dmil_selected_ordistep, scan=F)
summary(cca2)
anova(cca) 
plot(cca2)



# pCCA --------------------------------------------------------------------

#Partition of the Variation of Community with dmil and space (dcoord)
mod <- varpart(dflo,dmil_selected,dcoord)
mod
plot(mod, bg=2:5)



pcca=cca(dflo,dmil_selected, dcoord, scan=F)
pcca


# -------------------------------------------------------------------------
pcca
summary(pcca)

#test the pCCA:
anova.cca(pcca)
pcca$CCA$v
pcca$CCA$biplot

MVA.synt(pcca)
plot(pcca,scaling=1)
plot(pcca,scaling=2)
goodness(pcca)

#plot the pCCA
plot(pcca, type="n")
text(pcca, dis="cn",col="black",cex = 1.2)
text(pcca, "species", col="blue", cex=0.8)
#interprete the resul

res = summary(pcca)




# categories --------------------------------------------------------------


dkmeans = res$sites[, 1:2]

res.km <- kmeans(dkmeans, 7, nstart = 25)


res.km$cluster %>% str()
dasso$association %>% str()

dcat = tibble(kmeans = res.km$cluster,
              asso = dasso$association)

table(dcat)
