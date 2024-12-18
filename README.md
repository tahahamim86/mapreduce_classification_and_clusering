#KNN with MapReduce - Student Depression Datase
This repository contains an implementation of K-Nearest Neighbors (KNN) for predicting depression based on a student dataset. The project includes two approaches for applying KNN:

    Standard KNN: KNN algorithm applied directly to the data.
    KNN with MapReduce: Parallelized KNN using the MapReduce approach for distributed computing, leveraging the rmr2 library in R.

Purpose

The goal of this project is to explore how KNN can be applied for predicting depression among students using various features such as:

    Age
    Academic Pressure
    Study Satisfaction
    Study Hours
    Financial Stress

The dataset also includes columns related to gender, sleep duration, suicidal thoughts, and family history of mental illness. The project compares the performance of standard KNN and KNN implemented with MapReduce.
Features

    Preprocessing: Data cleaning, handling missing values, converting categorical data to factors, and encoding features numerically for KNN.
    MapReduce Implementation: Distribution of data processing tasks using MapReduce.
    KNN: KNN is used to classify students into categories (depressed vs non-depressed).
    Accuracy Calculation: The accuracy of both the standard KNN and MapReduce KNN is calculated for comparison.
KMeans with MapReduce - Iris Dataset

This repository demonstrates an implementation of KMeans clustering on the Iris dataset, using two approaches:

    Standard KMeans: Applying KMeans directly to the data for clustering.
    KMeans with MapReduce: Implementing the KMeans algorithm in a simulated MapReduce environment to distribute the computation.

Purpose

The goal of this project is to apply KMeans clustering on the Iris dataset, comparing the results between the standard KMeans algorithm and a simulated version using MapReduce. The Iris dataset consists of 150 data points with 4 features (sepal length, sepal width, petal length, and petal width) and 3 classes of Iris species.
Features

    Data Preprocessing: Includes splitting the Iris dataset into training and testing sets, with data shuffled for variability.
    KMeans Algorithm: KMeans is applied for unsupervised learning, where the goal is to classify the data into 3 clusters.
    MapReduce Simulation for KMeans: A simulated MapReduce approach is used to distribute the calculation of cluster centroids across different tasks.
    Accuracy Calculation: The accuracy of clustering is evaluated by adjusting cluster labels to match the true classes of the dataset.
    Comparison of Methods: A comparison is made between the standard KMeans and the MapReduce version of KMeans based on accuracy.

Datasets

    Iris Dataset: This famous dataset contains measurements of flowers, such as sepal length, sepal width, petal length, and petal width, along with the corresponding species of the flowers. It is used for classification and clustering tasks.
