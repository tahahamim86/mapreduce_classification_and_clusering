# Charger les packages nécessaires
library(class)
library(datasets)

# Charger le jeu de données Iris
data(iris)

# 1. Définir la fonction Map qui va mapper les données (simuler le MapReduce)
map_function <- function(data) {
  # Diviser les données en caractéristiques (x1, x2, ...) et labels (classe)
  keyval_list <- lapply(1:nrow(data), function(i) {
    keyval(data[i, 5], data[i, 1:4])  # Utiliser la classe comme clé et les caractéristiques comme valeur
  })
  
  # Convertir la liste en une structure de données clé-valeur
  keyval_list
}

# 2. Définir la fonction Reduce qui va réduire les résultats
reduce_function <- function(keyval_list) {
  # Ici, on "agglomère" les données par classe (en réduisant sur les clés)
  reduced_data <- lapply(keyval_list, function(kv) {
    # Extraire la classe (clé) et les valeurs associées
    list(class = kv[[1]], values = kv[[2]])
  })
  
  return(reduced_data)
}

# 3. Appliquer la fonction Map et Reduce localement sur le jeu de données Iris
map_result <- map_function(iris)
reduced_result <- reduce_function(map_result)

# Affichage des résultats de MapReduce simulé
print("Résultats de MapReduce simulé :")
print(reduced_result)

# 4. Fonction pour appliquer KMeans et calculer l'exactitude avec un ajustement des labels des clusters
kmeans_accuracy <- function(train_data, test_data, train_labels, test_labels, k = 3) {
  # Appliquer KMeans
  kmeans_model <- kmeans(train_data, centers = k)
  
  # Ajuster les labels des clusters en fonction de la classe la plus fréquente dans chaque cluster
  cluster_labels <- sapply(1:k, function(cluster_idx) {
    # Trouver la classe la plus fréquente dans le cluster
    cluster_data <- train_labels[kmeans_model$cluster == cluster_idx]
    most_frequent_class <- names(sort(table(cluster_data), decreasing = TRUE))[1]
    return(most_frequent_class)
  })
  
  # Prédire les clusters pour les données de test
  predicted_clusters <- predict_kmeans(kmeans_model, test_data)
  
  # Mappez les clusters prédits aux classes correspondantes
  predicted_labels <- sapply(predicted_clusters, function(cluster_idx) {
    cluster_labels[cluster_idx]
  })
  
  # Calculer l'exactitude en comparant les labels prédits aux labels réels
  accuracy <- sum(predicted_labels == as.factor(test_labels)) / length(test_labels)
  return(accuracy)
}

# Fonction de prédiction pour KMeans
predict_kmeans <- function(kmeans_model, test_data) {
  # Trouver le cluster le plus proche pour chaque point de test
  distances <- sapply(1:nrow(test_data), function(i) {
    apply(kmeans_model$centers, 1, function(center) sum((test_data[i, ] - center)^2))
  })
  
  # Retourner les indices des clusters les plus proches
  predicted_clusters <- apply(distances, 2, which.min)
  return(predicted_clusters)
}

# 5. Créer des sous-ensembles à partir du jeu de données pour KMeans
create_subsets <- function(data, ratio = 0.7) {
  set.seed(123)  # Pour la reproductibilité
  shuffled_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)
  train_size <- floor(ratio * nrow(data))
  train_indices <- shuffled_indices[1:train_size]
  test_indices <- shuffled_indices[(train_size + 1):nrow(data)]
  
  train_data <- data[train_indices, -5]
  train_labels <- data[train_indices, 5]
  test_data <- data[test_indices, -5]
  test_labels <- data[test_indices, 5]
  
  return(list(train_data = train_data, train_labels = train_labels, test_data = test_data, test_labels = test_labels))
}

# 6. Appliquer KMeans pour chaque sous-ensemble généré avec l'ajustement des labels
mapreduce_results <- data.frame(Subset = integer(), Accuracy = numeric())

for (i in 1:5) {
  # Créer un sous-ensemble pour l'entraînement et les tests
  subsets <- create_subsets(iris, ratio = 0.7)  # Ratio de 70% pour l'entraînement
  accuracy <- kmeans_accuracy(subsets$train_data, subsets$test_data, 
                              subsets$train_labels, subsets$test_labels, k = 3)
  
  # Ajouter les résultats au tableau
  mapreduce_results <- rbind(mapreduce_results, data.frame(Subset = i, Accuracy = accuracy))
}

# Afficher le tableau des résultats de MapReduce simulé
print("Tableau des exactitudes pour chaque sous-ensemble MapReduce simulé :")
print(mapreduce_results)

# 7. Appliquer KMeans standard (sans MapReduce) pour l'ensemble entier
subsets_standard <- create_subsets(iris, ratio = 0.7)
standard_accuracy <- kmeans_accuracy(subsets_standard$train_data, subsets_standard$test_data, 
                                     subsets_standard$train_labels, subsets_standard$test_labels, k = 3)

# Afficher l'exactitude du KMeans standard
print(paste("Exactitude du KMeans standard : ", standard_accuracy))

# Comparaison des résultats
comparison_results <- data.frame(Method = c("MapReduce KMeans", "Standard KMeans"), 
                                 Accuracy = c(mean(mapreduce_results$Accuracy), standard_accuracy))

print("Comparaison des exactitudes :")
print(comparison_results)
