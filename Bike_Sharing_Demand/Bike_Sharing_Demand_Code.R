
# Ce script permet l'analyse de données correspondant au nombre de locations de 
# vélos partagés par heure. Il utilise le fichier "data.csv", qui doit être 
# présent dans le même répertoire. Il affiche différentes valeurs d'intérêt
# pour l'analyse, et crée 5 graphiques dans ce même répertoire.

########################################
#  Lecture et préparation des données  #
########################################

# Chargement des librairies utilisées
library(lubridate)
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))
library(e1071)

# Lecture du fichier de données
data = read.csv("data.csv")

# Conversion de datetime en format date
data$datetime <- ymd_hms(data$datetime)

# Conversion de certaines variables en facteurs
data$season <- as.factor(data$season)
data$holiday <- as.factor(data$holiday)
data$workingday <- as.factor(data$workingday)
data$weather <- as.factor(data$weather)

# Ajout des nouvelles variables year et time
data$year <- as.factor(year(data$datetime))
data$time <- as.factor(hour(data$datetime))

# Séparation en échantillons d'apprentissage, de validation et de test
set.seed(4848)

in_training <- createDataPartition(data$count, 
                                   p = 0.6, 
                                   list = FALSE)
train <- data[in_training,]
test_data <- data[-in_training,]
in_test <- createDataPartition(test_data$count, 
                               p = 0.5, 
                               list = FALSE)
test <- test_data[in_test,]
validation <- test_data[-in_test,]


##########################################
#  Partie I - Statistiques descriptives  #
##########################################

writeLines(c("\n",
             "##########################################",
             "#  Partie I - Statistiques descriptives  #",
             "##########################################",
             ""))

########## Données manquantes

writeLines("\n########## Données manquantes")

# Calcul du nombres de données s'il n'y avait pas de valeurs manquantes
nb_complete <- 24*19*12*2
# Calcul du nombre de lignes manquantes
nb_missing <- nb_complete - dim(data)[1]
# Calcul du pourcentage de données manquantes
percent_missing <- (nb_missing/nb_complete)*100

writeLines(c("Nombre de données s'il n'y avait pas de valeurs manquantes : ", 
             nb_complete,
             "\n"),
           sep = "")
writeLines(c("Nombre de valeurs manquantes : ", 
             nb_missing,
             "\n"),
           sep = "")
writeLines(c("Pourcentage de valeurs manquantes : ", 
             round(percent_missing, 2),
             " %\n"),
           sep = "")

########## Variables temporelles

writeLines("\n########## Variables temporelles")
writeLines(".....Création du graphique du nombre moyen de locations par jour au cours du temps")

# Calcul du nombre de locations moyen par jour
count_per_day <- by(train$count, as.Date(train$datetime), sum)

# Création du graphique représentant le nombre de locations moyen
# par jour au cours du temps
png("locations_par_jour_au_cours_du_temps.png", width = 960, height = 480)
plot(count_per_day, 
     type = "l",
     xaxt = "n",
     col = "slateblue4",
     xlab = "Mois",
     ylab = "Nombre de locations par jour",
     main = "Nombre de locations de vélos par jour au cours du temps")
abline(v = 228, lty = "dashed", lwd = 2, col = "orangered")
text(114, 1000, "2011", col = "orangered", cex = 1.2)
text(342, 1000, "2012", col = "orangered", cex = 1.2)
axis(1, at = seq(0,437, by = 19), labels = c(seq(1,12), seq(1,12)))
invisible(dev.off())

writeLines(c("\nTests statistiques : ",
             "--- Différence entre 2011 et 2012 : "))

# Tests statistiques pour confirmer l'influence de l'année et des saisons
year_test <- wilcox.test(train$count[train$year == 2011], 
                         train$count[train$year == 2012],
                         alternative = "less")
print(year_test)

season_test <- kruskal.test(list(train$count[train$season == 1],
                                 train$count[train$season == 2],
                                 train$count[train$season == 3],
                                 train$count[train$season == 4]))
writeLines("--- Différence entre les saisons : ")
print(season_test)

writeLines(".....Création du graphique du nombre moyen de location par heure")

# Calcul du nombre de locations moyen en fonction de l'heure pour
# les jours travaillés et non travaillés
mean_per_time_nwd <- by(train$count[train$workingday == 0], 
                        train$time[train$workingday == 0], 
                        mean)
mean_per_time_wd <- by(train$count[train$workingday == 1], 
                       train$time[train$workingday == 1], 
                       mean)

# Création du graphique représentant le nombre moyen de location par heure
png("nombre_moyen_de_locations_par_heure_jours_travaillés_ou_non.png",
    width = 960, height = 480)
plot(x = as.numeric(levels(train$time)), 
     y = mean_per_time_nwd, 
     type = "l", 
     main = "Nombre moyen de locations de vélos en fonction de l'heure", 
     xlab = "Heure", 
     ylab = "Nombre moyen de locations", 
     col="slateblue4",
     lwd = 2,
     ylim = c(0,550),
     xaxt = "n")
points(x = levels(train$time), y = mean_per_time_wd, 
       type = "l", col = "orangered", lwd = 2)
abline(v = c(8, 17), col = "orangered", lty = "dashed")
legend("topleft",
       legend = c("Jours travaillés", "Jours non travaillés"),
       col = c("orangered", "slateblue4"),
       lty = c(1,1),
       lwd = 2)
axis(1, at = seq(0, 23, by = 1))
invisible(dev.off())


########## Variables météorologiques

writeLines("\n########## Variables météorologiques")
writeLines(".....Création du graphique du nombre de locations en fonction de la température et de l'humidité")

# Calcul du nombre moyen de locations pour chaque température observée
count_per_temp <- by(train$count, 
                     train$temp, 
                     mean)

# Calcul du nomre moyen de locations pour chaque taux d'humidité observé
count_per_hum <- by(train$count, 
                    train$hum, 
                    mean)

# Création du graphique du nombre de locations en fonction de la
# température et de l'humidité
png("locations_selon_temperature_et_humidite.png", width = 960, height = 480)
par(mfrow = c(1,2),
    mar = c(4, 5, 3, 1) + 0.1)
# Nombre de locations en fonction de la température
plot(as.numeric(names(count_per_temp)),
     count_per_temp, 
     pch = 20,
     xlab = "Température (Degrés Celsius)",
     ylab = "Nombre moyen de locations",
     main = "Nombre moyen de locations de vélos\nen fonction de la température",
     col = "slateblue4")
abline(v = 36, lty = "dashed", col = "slateblue4", lwd = 2)
axis(1, at = 36)
# Nombre de locations en fonction de l'humidité
plot(as.numeric(names(count_per_hum)),
     count_per_hum, 
     pch = 20,
     xlab = "Humidité relative (%)",
     ylab = "Nombre moyen de locations",
     main = "Nombre moyen de locations de vélos\nen fonction du taux d'humidité",
     col = "slateblue4")
abline(v = 20, lty = "dashed", col = "slateblue4", lwd = 2)
invisible(dev.off())


##################################
#  Partie II - Machine Learning  #
##################################

writeLines(c("\n",
             "##################################",
             "#  Partie II - Machine Learning  #",
             "##################################",
             ""))

# Création des formules correspondant aux différents modèles
# Formule n'utilisant que les variables originales
form_start <- count ~ season + workingday + temp + humidity + 
    atemp + weather + windspeed + holiday
# Formule utilisant les variables originales + les variables time et year
form_all <- count ~ year + time + 
    season + workingday + temp + humidity + 
    atemp + weather + windspeed + holiday
# Formule n'utilisant que les variables supposées les plus prédictives
form_select <- count ~ year + time + 
    season + workingday + temp + humidity


########## Random Forests

writeLines("\n########## Random Forests")

writeLines(".....Construction du modèle Random Forest n'ayant que les variables originales")
# Construction du modèle Random Forest n'ayant que les variables originales et 
# calcul du RMSE de l'échantillon de validation
fit_rf_start <- randomForest(form_start, data = train)
pred_rf_start <- predict(fit_rf_start, validation)
RMSE_rf_start <- sqrt(mean((validation$count - pred_rf_start)^2))
writeLines(c("RMSE de l'échantillon de validation : ",
             round(RMSE_rf_start, 2),
             "\n"),
           sep = "")

writeLines(".....Construction du modèle Random Forest ayant toutes les variables")
# Construction du modèle Random Forest ayant toutes les variables et calcul du 
# RMSE de l'échantillon de validation
fit_rf_all <- randomForest(form_all, data = train)
pred_rf_all <- predict(fit_rf_all, validation)
RMSE_rf_all <- sqrt(mean((validation$count - pred_rf_all)^2))
writeLines(c("RMSE de l'échantillon de validation : ",
             round(RMSE_rf_all, 2),
             "\n"),
           sep = "")

writeLines(".....Construction du modèle Random Forest avec variables sélectionnées")
# Construction du modèle Random Forest avec variables sélectionnées et calcul 
# du RMSE de l'échantillon de validation
fit_rf_select <- randomForest(form_select, data = train)
pred_rf_select <- predict(fit_rf_select, validation)
RMSE_rf_select <- sqrt(mean((validation$count - pred_rf_select)^2))
writeLines(c("RMSE de l'échantillon de validation : ",
             round(RMSE_rf_select, 2),
             "\n"),
           sep = "")


########## SVM

writeLines("\n########## SVM")

writeLines(".....Construction du modèle SVM de type régression avec variables sélectionnées et paramètres par défaut")
# Construction du modèle SVM de type régression avec variables sélectionnées 
# et paramètres par défaut, puis calcul du RMSE de l'échantillon de validation
fit_svm_default <- svm(form_select, data = train, type = "eps-regression")
pred_svm_default <- predict(fit_svm_default, validation)
RMSE_svm_default <- sqrt(mean((validation$count - pred_svm_default)^2))
writeLines(c("RMSE de l'échantillon de validation : ",
             round(RMSE_svm_default, 2),
             "\n"),
           sep = "")

writeLines(c(".....Ajustement des paramètres", 
             "\t/!\\ Peut prendre du temps /!\\ "))
# Ajustement des paramètres afin de trouver les paramètres optimaux, c'est à
# dire minimisant le RMSE de l'échantillon de validation.
# Test de 88 couples de valeurs pour epsilon et cost : attention, cette fonction
# peut prendre du temps !
tune_param_svm <- tune(svm, 
                       form_select,  
                       data = train,
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(0:7)),
                       validation.x = validation,
                       tunecontrol = tune.control(sampling = "fix",
                                                  error.fun = function(true, pred){sqrt(mean((true-pred)^2))}))

print(tune_param_svm)

writeLines(".....Création du graphique d'illustration de l'ajustement des paramètres")
# Création du graphique d'illustration de l'ajustement des paramètres
png("ajustement_parametres_svr.png")
plot(tune_param_svm)
invisible(dev.off())

writeLines(".....Création du modèle SVM avec les paramètre optimaux")
# Création du modèle SVM avec les paramètre optimaux et calcul des RMSE des
# échantillons d'apprentissage, de validation, et de test
fit_svm_opti <- svm(form_select, 
                    data = train, 
                    type = "eps-regression",
                    cost = tune_param_svm$best.parameters["cost"], 
                    epsilon = tune_param_svm$best.parameters["epsilon"])

pred_train_opti <- predict(fit_svm_opti, train)
RMSE_train_opti <- sqrt(mean((train$count - pred_train_opti)^2))
writeLines(c("RMSE de l'échantillon d'apprentissage : ",
             round(RMSE_train_opti, 2),
             "\n"),
           sep = "")

pred_val_opti <- predict(fit_svm_opti, validation)
RMSE_val_opti <- sqrt(mean((validation$count - pred_val_opti)^2))
writeLines(c("RMSE de l'échantillon de validation : ",
             round(RMSE_val_opti, 2),
             "\n"),
           sep = "")

pred_test_opti <- predict(fit_svm_opti, test)
RMSE_test_opti <- sqrt(mean((test$count - pred_test_opti)^2))
writeLines(c("RMSE de l'échantillon de test : ",
             round(RMSE_test_opti, 2),
             "\n"),
           sep = "")


writeLines(".....Création du graphique d'illustration de la performance du modèle")
# Création du graphique d'illustration de la performance du modèle sur
# l'échantillon de test
png("performance_test_modele_optimal.png")
plot(test$count, 
     pred_test_opti,
     xlim = c(0,900),
     ylim = c(0,900),
     xlab = "Valeur réelle",
     ylab = "Valeur prédite",
     main = "Valeurs prédites en fonction des valeurs réelles\nde l'échantillon de test avec le modèle optimal",
     col = "slateblue4",
     pch = "°")
abline(a = 0, b = 1, col = "orangered", lwd = 2)
invisible(dev.off())











