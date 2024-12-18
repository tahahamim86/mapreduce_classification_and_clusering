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
