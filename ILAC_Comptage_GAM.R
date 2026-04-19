# 1) INSTALLATION DES PACKAGES ----

rm(list = ls())

if(!require("CASdatasets")) {install.packages("CASdatasets")}
if(!require("tidyr")) {install.packages("tidyr")}
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("ggplot2")) {install.packages("ggplot2")}
if(!require("readr")) {install.packages("readr")}
if(!require("mgcv")) {install.packages("mgcv")}
if(!require("AER")) {install.packages("AER")}
if(!require("MASS")) {install.packages("MASS")}
if(!require("performance")) {install.packages("performance")}



library("CASdatasets") 
library("tidyr")
library("dplyr")
library("ggplot2")
library("readr")
library("mgcv")
library("AER")
library("MASS")
library("performance")

# 1) CHARGEMENT DES DONNEES ----

## 1.1) Base de donnée Fréquence ----

data(freMTPL2freq) 
#freMTPL2freq <- read_csv("freMTPL2freq.csv")
summary(freMTPL2freq)
str(freMTPL2freq)

data1 <- freMTPL2freq %>% dplyr::select(IDpol,DrivAge,VehGas,VehPower,VehAge,DrivAge, Area,Exposure,ClaimNb)
data2 <- data1 %>% dplyr::filter(ClaimNb >=0 & ClaimNb <= 6 & !is.na(ClaimNb))
data3 <- data2 %>% dplyr::filter(Exposure >=0 & Exposure <= 1)
data_freq <- data3
remove(freMTPL2freq,data1,data2,data3)


## 1.2) Base de donnée Sévérité ----

data(freMTPL2sev)
#freMTPL2sev <- read_csv("freMTPL2sev.csv") 
summary(freMTPL2sev)
str(freMTPL2sev)
#freMTPL2sev$IDpol <- as.numeric(as.character(freMTPL2sev$IDpol))

data_sev <- freMTPL2sev %>% inner_join(data_freq)
data_sev$ClaimAmountavg <- data_sev$ClaimAmount/data_sev$ClaimNb
remove(freMTPL2sev) 

## 1.3) Elimine les colonnes non relevantes (attention, garder IDpol si on veut tenir compte de la nonindépendance----

data_freq <- data_freq %>% select(-IDpol)
data_sev <- data_sev %>% select(-IDpol,-Exposure)



# 2) SUMMARY ET VERIFICATION DES TYPES  DE DONNEES ----

str(data_freq)
# data_freq$ClaimNb <- as.integer(data_freq$ClaimNb)
# data_freq$VehGas <- factor(data_freq$VehGas)
# data_freq$Area <- factor(data_freq$Area)
summary(data_freq)


str(data_sev)
# data_freq$ClaimNb <- as.integer(data_freq$ClaimNb)
# data_freq$VehGas <- factor(data_freq$VehGas)
# data_freq$Area <- factor(data_freq$Area)
summary(data_sev)


# 3) ANALYSE EXPLORATOIRE ----
str(data_freq)
 
# Variable Age
 
summary(data_freq$DrivAge)
data_freq$DrivAgeCat = cut(data_freq$DrivAge, c(seq(from = 18, to = 78,by =6),100),include.lowest = TRUE)
summary(data_freq$DrivAgeCat)


# Variable Vehpower

summary(data_freq$VehPower)
data_freq$VehPowerCat = cut(data_freq$VehPower, c(4,5,6,7,8,9,15),include.lowest = TRUE)
summary(data_freq$VehPowerCat)



# 4) DONNEES INDIVIDUELLES VS DONNEES GROUPEES ----

## 4.1) Model with individual data ----
m_indiv = glm(ClaimNb ~ offset(log(Exposure)) + Area + VehGas, 
              data = data_freq,
              family=poisson(link = log))
summary(m_indiv)

# Prédiction
data_freq$Prediction_indiv   <- predict(m_indiv,newdata=data_freq,type="response")

## 4.2) Model with grouped data ----
data_freq_grouped <- data_freq %>% group_by(VehGas,Area) %>% summarise (Exposure=sum(Exposure), ClaimNb=sum(ClaimNb))


m_grouped = glm(ClaimNb ~ offset(log(Exposure)) + Area + VehGas, 
                data = data_freq_grouped,
                family=poisson(link = log))
summary(m_grouped)

# Prédiction
data_freq$Prediction_grouped <- predict(m_grouped,newdata=data_freq,type="response")


# Conclusions: coefficients, standard errors and predictions are identical if we work on individual 
# data or grouped data


## 4.3) Prediction build with coefficients ----

# Prédiction

design_matrix <- model.matrix(m_indiv)
betas_vector <- coef(m_indiv)
Score_vector <- design_matrix %*%  betas_vector

data_freq$Prediction_coeffs=exp(Score_vector + log(data_freq$Expo))

 

# 5) MARGINAL TOTALS PROPERTY ----

# Global balance on the full portfolio
sum(data_freq$ClaimNb)
sum(data_freq$Prediction_indiv)



# on a category of a categorical variable (depth 1 ok)
test_1 <- data_freq %>% group_by(VehGas) %>% summarise(Tot_ClaimNb=sum(ClaimNb),Tot_Pred=sum(Prediction_indiv))
test_2 <- data_freq %>% group_by(Area) %>% summarise(Tot_ClaimNb=sum(ClaimNb),Tot_Pred=sum(Prediction_indiv))

# but not for two categories of two categorical variable (depth 2 not ok)
test_3 <- data_freq %>% group_by(VehGas,Area) %>% summarise(Tot_ClaimNb=sum(ClaimNb),Tot_Pred=sum(Prediction_indiv))

 


# 6) DIAGNOSTIC DE DISPERSION ----


## Modèle Poisson
mod <- glm(ClaimNb ~ offset(log(Exposure))  + VehGas + Area, 
           data = data_freq,
           family=poisson(link = log))

summary(mod)

## 6.1) Calcul de phi ----

# A partir du Chi^2 de Pearson
phi_pearson <- sum(residuals(mod, type = "pearson")^2) / mod$df.residual
phi_pearson

# A partir de la déviance
phi_dev <- deviance(mod) / mod$df.residual
phi_dev

# Comparaison des deux 
c(phi_pearson = phi_pearson, phi_deviance = phi_dev)


## 6.2) Tests statistiques ----
 
# Test fondé sur X^2
## Statistique de Pearson
X2 <- sum(residuals(mod, type = "pearson")^2)
df_res <- mod$df.residual

## p-value unilatérale (surdispersion => queue droite)
p_pearson <- pchisq(X2, df = df_res, lower.tail = FALSE)
p_pearson

# Test fondé sur D
## Statistique de déviance
D <- deviance(mod)

## p-value unilatérale
p_deviance <- pchisq(D, df = df_res, lower.tail = FALSE)
p_deviance

# Test de Cameron & Trivedi
dispersiontest(mod, alternative = "greater") # Hypothèse implicite NB-like

## 6.3) Résidus ----
# Résidus vs valeurs ajustées
plot(fitted(mod), residuals(mod, type = "pearson"),
     xlab = "Valeurs ajustées (mu_hat)",
     ylab = "Résidus de Pearson",
     main = "Résidus de Pearson vs valeurs ajustées")
abline(h = 0, col = "red")

# Histogramme des résidus
hist(residuals(mod, type = "pearson"),
     breaks = 30,
     main = "Histogramme des résidus de Pearson")

# QQ-plot des résidus
qqnorm(residuals(mod, type = "pearson"))
qqline(residuals(mod, type = "pearson"), col = "red")


## 6.4) Comparaison avec modèle binomial négatif ----

# Modèle Binomial négatif
library(MASS)
mod_bn <- glm.nb(ClaimNb ~ offset(log(Exposure))  + VehGas + Area, 
                 data = data_freq)

# Comparaison
c(mod$aic,mod_bn$aic)

# 7) DIAGNOSTIC EXCES DE ZEROS ----

library(performance)
check_zeroinflation(mod)
check_zeroinflation(mod_bn)

# 8) RECHERCHE DU MODELE OPTIMAL ----

MODEL <- rep(0,10)
AIC <- rep(0,10)
PVALUE <- rep(0,10)
DESCRIPTION <- rep(0,10)

### Modele null 

m1 = glm(ClaimNb ~ offset(log(Exposure)), 
         data = data_freq,
         family=poisson(link = log))
summary(m1)
#We can find the average claim frequency of the portfolio.`.
mu_null_model <- exp(m1$coefficients)
MODEL[1] <-"model1"
AIC[1] <- m1$aic
PVALUE[1] <- "/"
DESCRIPTION[1] <- "/"

### Modele full avec variable continues discrétisées

m2 = glm(ClaimNb ~ offset(log(Exposure))+ VehPowerCat + DrivAgeCat + VehGas + Area, 
         data = data_freq,
         family=poisson(link = log))
summary(m2)
MODEL[2] <-"model2"
AIC[2] <- m2$aic
PVALUE[2] <- paste("model1/model2: ",anova(m1,m2,test="Chisq")[2,"Pr(>Chi)"])
DESCRIPTION[2] <- "PowerCat + AgeCat + Gas + Area"
 
 

### Modele full avec gam

m3 = gam(ClaimNb ~ offset(log(Exposure)) + s(VehPower,k = 10) +  s(DrivAge,k = 10) + VehGas + Area, 
         data = data_freq,
         family=poisson(link = log))
summary(m3)
MODEL[3] <-"model3"
AIC[3] <- m3$aic
PVALUE[3] <- paste("model1/model3: ",anova(m1,m3,test="Chisq")[2,"Pr(>Chi)"])
DESCRIPTION[3] <- "Power + Age + Gas + Area"

plot(m3)
plot(m3, pages = 1, shade = TRUE, shade.col = "lightblue")

#Vérification du centrage empirique des fonnctions lissées
mean(predict(m3, type = "terms")[, "s(DrivAge)"])
mean(predict(m3, type = "terms")[, "s(VehPower)"])
#m3$smooth
gam.check(m3) # k index proche de 1 → OK vs k index  << 1  et p-value faible → problème et il faut augmenter k


