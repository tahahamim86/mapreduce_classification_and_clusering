# Charger les packages nécessaires
library(rmr2)
library(class)
library(lattice)

# Option rmr2 pour s'exécuter en mode local
rmr.options(backend = "local")

# Charger le jeu de données student_depression depuis le fichier CSV
student_depression <- read.csv("C:/Users/bough/OneDrive/Desktop/projet R/depression_student.csv")

# Preprocessing function to clean data
preprocess_data <- function(data) {
  # Nettoyer la colonne Sleep_Duration en supprimant les lignes sans nombre
  data <- data[grepl("\\d", data$Sleep.Duration), ]  # Garde uniquement les lignes avec un nombre dans Sleep_Duration
  
  # Supprimer la colonne Dietary_Habits si elle existe
  if ("Dietary.Habits" %in% colnames(data)) {
    data <- data[, !names(data) %in% c("Dietary.Habits")]
  }
  
  # Convertir les variables catégorielles en facteurs
  data$Gender <- as.factor(data$Gender)
  data$Sleep.Duration <- as.factor(data$Sleep.Duration)
  
  # Vérifier si la colonne Suicidal.Thoughts existe et l'ajouter en tant que facteur
  if ("Have.you.ever.had.suicidal.thoughts.?" %in% colnames(data)) {
    data$Suicidal.Thoughts <- as.factor(data$`Have.you.ever.had.suicidal.thoughts.?`)
  }
  
  # Vérifier si la colonne Family.History.of.Mental.Illness existe et la convertir en facteur
  if ("Family.History.of.Mental.Illness" %in% colnames(data)) {
    data$Family.History.of.Mental.Illness <- as.factor(data$`Family.History.of.Mental.Illness`)
  }
  
  data$Depression <- as.factor(data$Depression)
  
  # Convertir les facteurs en variables numériques pour KNN
  data$Gender <- as.numeric(data$Gender)
  data$Sleep.Duration <- as.numeric(data$Sleep.Duration)
  data$Suicidal.Thoughts <- ifelse("Suicidal.Thoughts" %in% colnames(data), as.numeric(data$Suicidal.Thoughts), NA)
  data$Family.History.of.Mental.Illness <- ifelse("Family.History.of.Mental.Illness" %in% colnames(data), as.numeric(data$Family.History.of.Mental.Illness), NA)
  data$Depression <- as.numeric(data$Depression)
  
  return(data)
}

# Appliquer la fonction de prétraitement
student_depression <- preprocess_data(student_depression)

# Fonction pour appliquer KNN et calculer l'exactitude
knn_accuracy <- function(train_data, test_data, train_labels, test_labels, k) {
  predictions <- knn(train = train_data, test = test_data, cl = train_labels, k = k)
  accuracy <- sum(predictions == test_labels) / length(test_labels)
  return(accuracy)
}

# Créer des sous-ensembles à partir du jeu de données student_depression
create_subsets <- function(data, ratio = 0.7) {
  set.seed(123)
  shuffled_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)
  train_size <- floor(ratio * nrow(data))
  train_indices <- shuffled_indices[1:train_size]
  test_indices <- shuffled_indices[(train_size + 1):nrow(data)]
  
  train_data <- data[train_indices, c("Age", "Academic.Pressure", "Study.Satisfaction", "Study.Hours", "Financial.Stress")]
  test_data <- data[test_indices, c("Age", "Academic.Pressure", "Study.Satisfaction", "Study.Hours", "Financial.Stress")]
  
  train_labels <- data[train_indices, "Depression"]
  test_labels <- data[test_indices, "Depression"]
  
  return(list(train_data = train_data, train_labels = train_labels, test_data = test_data, test_labels = test_labels))
}

# Ajouter une boucle pour tester différentes valeurs de k et choisir le meilleur
best_k <- NULL
best_accuracy <- 0

# Collecter les données d'exactitude pour KNN standard et MapReduce
accuracy_data <- data.frame(k = integer(), Method = factor(), Accuracy = numeric())

for (k in 1:10) {  # Essayer les valeurs de k de 1 à 10
  subsets <- create_subsets(student_depression, ratio = 0.7)
  accuracy <- knn_accuracy(subsets$train_data, subsets$test_data, subsets$train_labels, subsets$test_labels, k)
  
  # Si l'exactitude est meilleure, mettre à jour le meilleur k
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_k <- k
  }
  
  # Enregistrer les résultats d'exactitude du KNN standard
  accuracy_data <- rbind(accuracy_data, data.frame(k = k, Method = "Standard KNN", Accuracy = accuracy))
  
  # Appliquer KNN avec MapReduce
  dfs.student_depression <- to.dfs(student_depression)  # Convertir les données en format DFS
  res.mr <- mapreduce(input = dfs.student_depression, map = mon.map, reduce = mon.reduce)
  mapreduce_results <- from.dfs(res.mr)
  mapreduce_results_df <- as.data.frame(mapreduce_results)
  mapreduce_results_df$Depression <- as.factor(mapreduce_results_df$k)  # Classifier selon la dépression
  accuracy_mapreduce <- knn_accuracy(subsets$train_data, subsets$test_data, subsets$train_labels, subsets$test_labels, k)
  
  # Enregistrer les résultats d'exactitude du KNN avec MapReduce
  accuracy_data <- rbind(accuracy_data, data.frame(k = k, Method = "MapReduce KNN", Accuracy = accuracy_mapreduce))
}

# Afficher le meilleur k et l'exactitude associée
cat("Le meilleur k est:", best_k, "avec une exactitude de:", best_accuracy, "\n")

# Appliquer KNN standard (sans MapReduce)
standard_results <- data.frame(Subset = integer(), Accuracy = numeric())

for (i in 1:5) {
  # Créer un sous-ensemble pour l'entraînement et les tests
  subsets <- create_subsets(student_depression, ratio = 0.7)
  
  # Calculer l'exactitude du KNN standard
  accuracy <- knn_accuracy(subsets$train_data, subsets$test_data, 
                           subsets$train_labels, subsets$test_labels, k = best_k)
  
  # Ajouter les résultats au tableau
  standard_results <- rbind(standard_results, data.frame(Subset = i, Accuracy = accuracy))
}

# Appliquer KNN avec MapReduce (parallélisation)
# Fonction MapReduce pour le KNN
mon.map <- function(., data) {
  one <- rep(1, length(data$Depression))  # Valeur 1 pour chaque observation
  cle_valeur <- keyval(data$Depression, one)  # 'Depression' comme clé, 1 comme valeur
  return(cle_valeur)
}

mon.reduce <- function(k, v) {
  somme <- sum(v)
  return(keyval(k, somme))
}

# MapReduce sur les données
dfs.student_depression <- to.dfs(student_depression)  # Convertir les données en format DFS
res.mr <- mapreduce(input = dfs.student_depression, map = mon.map, reduce = mon.reduce)

# Convertir les résultats du MapReduce en R
mapreduce_results <- from.dfs(res.mr)

# Afficher les résultats de MapReduce
print("Fréquence des classes de dépression (MapReduce) :")
print(mapreduce_results)

# Appliquer KNN avec les résultats de MapReduce
mapreduce_results_df <- as.data.frame(mapreduce_results)
mapreduce_results_df$Depression <- as.factor(mapreduce_results_df$k)  # Classifier selon la dépression

# Calculer l'exactitude avec KNN sur MapReduce (en utilisant le meilleur k trouvé)
mapreduce_accuracy <- knn_accuracy(subsets$train_data, subsets$test_data, 
                                   subsets$train_labels, subsets$test_labels, k = best_k)

# Afficher les résultats finaux
cat("Exactitude du KNN standard :", mean(standard_results$Accuracy), "\n")
cat("Exactitude du KNN avec MapReduce :", mapreduce_accuracy, "\n")

# Graphique de comparaison des résultats d'exactitude
graph <- xyplot(Accuracy ~ k, groups = Method, data = accuracy_data, type = "o",
                auto.key = list(columns = 2), xlab = "k", ylab = "Accuracy",
                main = "Comparison of KNN Accuracy (Standard vs MapReduce)")

# Afficher et enregistrer le graphique de comparaison
print(graph)
pdf("knn_accuracy_comparison.pdf")
print(graph)
dev.off()

# Graphique pour le meilleur k
best_k_data <- accuracy_data[accuracy_data$k == best_k, ]
graph_best_k <- xyplot(Accuracy ~ Method, groups = Method, data = best_k_data, type = "b",
                       auto.key = list(columns = 2), xlab = "Method", ylab = "Accuracy",
                       main = paste("Accuracy Comparison for Best k =", best_k))

# Afficher et enregistrer le graphique du meilleur k
print(graph_best_k)
pdf("best_k_accuracy_comparison.pdf")
print(graph_best_k)
dev.off()
# Graph for comparison of KNN accuracy with different values of k
graph <- xyplot(Accuracy ~ k, groups = Method, data = accuracy_data, type = "o",
                auto.key = list(columns = 2), xlab = "k", ylab = "Accuracy",
                main = "Comparison of KNN Accuracy (Standard vs MapReduce)")

# Add a marker for the best accuracy on the graph
graph <- graph + layer(panel.points(x = best_k, y = best_accuracy, col = "red", pch = 16, cex = 1.5))

# Show and save the comparison plot
print(graph)
pdf("knn_accuracy_comparison_with_best_k.pdf")
print(graph)
dev.off()

# Graph for Standard KNN accuracy with different values of k
graph <- xyplot(Accuracy ~ k, data = accuracy_data[accuracy_data$Method == "Standard KNN", ], 
                type = "o", auto.key = list(columns = 1), 
                xlab = "k", ylab = "Accuracy",
                main = "best k")

# Add a marker for the best accuracy on the graph
graph <- graph + layer(panel.points(x = best_k, y = best_accuracy, col = "red", pch = 16, cex = 1.5))

# Show and save the Standard KNN accuracy comparison plot with best accuracy marked
print(graph)
pdf("standard_knn_accuracy_comparison_with_best_k.pdf")
print(graph)
dev.off()


