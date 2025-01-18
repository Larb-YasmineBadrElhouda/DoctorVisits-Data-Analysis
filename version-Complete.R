# Installer et charger les packages nécessaires
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("missMDA")
library(FactoMineR)
library(factoextra)
library(missMDA) 

# Chargement des données 
DoctorVisits <- read.csv("C:/Users/Loubna/Downloads/DoctorVisits.csv")

# Affichage des premières lignes des données
head(DoctorVisits)

# Structure des données ( types des variables )
str(DoctorVisits)

# Résumé statistique (  un résumé pour identifier les variables peu informatives )
summary(DoctorVisits)

#Vérifiez la variance
apply(DoctorVisits, 2, var)

#Suppression des variables inutiles
	DoctorVisits <- DoctorVisits[, !(names(DoctorVisits) %in% c("rownames"))]


# Vérification des valeurs manquantes ( aucune )
sapply(DoctorVisits, function(x) sum(is.na(x)))


#Remplacement des valeurs manquantes par la moyenne
DoctorVisits_imputed <- imputePCA(DoctorVisits, ncp = 2)$completeObs

# Vérification des valeurs infinies ( aucune )
sapply(DoctorVisits, function(x) sum(is.infinite(x)))

# Identifier les variables numériques
sapply(DoctorVisits, is.numeric)

# Identifier les variables qualitatives
sapply(DoctorVisits, is.character)

# Numérisation des variables qualitatives
DoctorVisits$gender <- ifelse(DoctorVisits$gender == "female", 1, 0)
DoctorVisits$private <- ifelse(DoctorVisits$private == "yes", 1, 0)
DoctorVisits$freepoor <- ifelse(DoctorVisits$freepoor == "yes", 1, 0)
DoctorVisits$freerepat <- ifelse(DoctorVisits$freerepat == "yes", 1, 0)
DoctorVisits$nchronic <- ifelse(DoctorVisits$nchronic == "yes", 1, 0)
DoctorVisits$lchronic <- ifelse(DoctorVisits$lchronic == "yes", 1, 0)

#Standardisation des données ( centrer et réduire )
	DoctorVisits_scaled <- scale(DoctorVisits[, sapply(DoctorVisits, is.numeric)])

#Vérification de la standardisation
apply(DoctorVisits_scaled, 2, mean) # Moyenne proche de 0
apply(DoctorVisits_scaled, 2, sd)   # Écart-type proche de 1

#Effectuer l'ACP
	res.pca <- PCA(DoctorVisits_scaled, graph = TRUE)


#Coordonnées des individus
ind.coord <- res.pca$ind$coord
head(ind.coord)

#Contributions des individus
ind.contrib <- res.pca$ind$contrib
head(ind.contrib)

#cos2 des individus
ind.cos2 <- res.pca$ind$cos2
head(ind.cos2)

#Coordonnées des variables
var.coord <- res.pca$var$coord
head(var.coord)

#Contributions des variables
var.contrib <- res.pca$var$contrib
head(var.contrib)

#cos2 des variables
var.cos2 <- res.pca$var$cos2
head(var.cos2)

#Graphique des valeurs propres
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Graphique des individus ( cos2)
fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#Graphique des contributions des individus
fviz_contrib(res.pca, choice = "ind", axes = 1)

#Graphique des variables ( contrib )
fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#Graphique des cos2 des variables
fviz_cos2(res.pca, choice = "var", axes = 1:2)

#Biplot des individus et variables
fviz_pca_biplot(res.pca, repel = TRUE, col.var = "blue", col.ind = "red")










