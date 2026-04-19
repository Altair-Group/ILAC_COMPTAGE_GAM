library(mgcv)

set.seed(1)
n <- 100
x <- sort(runif(n, 0, 10))
y <- sin(x) + rnorm(n, 0, 0.2)

# Ajustement GAM avec TPRS
mod <- gam(y ~ s(x, k = 1), method = "REML")


# prédictions via le modèle
f_pred <- predict(mod)

# reconstruction manuelle
X <- model.matrix(mod) # Extraire la matrice de design
beta <- coef(mod) # Extraire les coefficients

f_manual <- X %*% beta


# Garder seulement la partie spline
# (la première colonne est l'intercept)
Xs <- X[, -1]
# coefficients correspondants
beta_s <- beta[-1]

# Visualisation des fonctions de base
matplot(x, Xs, type = "l", lty = 1,
        main = "Fonctions de base spline (TPRS)",
        xlab = "x", ylab = "valeur")

contrib <- sweep(Xs, 2, beta_s, "*")
matplot(x, contrib, type="l", lty=1,
        main="Contributions des fonctions de base",
        xlab="x", ylab="contribution")


f_manual <- rowSums(contrib) + coef(mod)[1]


plot(x, f_pred, type="l", col="blue", lwd=2,
     main="Comparaison reconstruction")
lines(x, f_manual, col="red", lty=2, lwd=2)

legend("topright",legend=c("predict()", "reconstruction"),
       col=c("blue", "red"), lty=c(1,2))



summary(mod)
gam.check(mod)


S <- mod$smooth[[1]]$S[[1]]
eig <- eigen(S)

eig$values   # intensité de pénalisation

# Les valeurs propres associées à la matrice de pénalité mesurent l’intensité de la contrainte appliquée aux différentes directions fonctionnelles. 
# Les grandes valeurs propres correspondent à des composantes fortement pénalisées, associées à des fonctions plus oscillantes, tandis que les petites 
# valeurs propres correspondent à des composantes peu pénalisées, généralement plus lisses.
# Les splines pénalisées peuvent être vues comme une base de fonctions allant de très lisses à très oscillantes, où la pénalisation agit comme un filtre qui 
# favorise les basses fréquences et élimine progressivement les composantes à haute fréquence.