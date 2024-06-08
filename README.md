Overview of ImpLiMet
Optimal imputation method depends on the cause of missingness and the characteristics of the data. ImpLiMet enables users to impute missing data using 8 different methods and proposes the optimal imputation approach for the user’s data set if users have at least three features and six samples without any missing values in their dataset. Although focused on metabolomics and lipidomics, ImpLiMet can be used for any dataset.
Users can either select an imputation method among the eight provided options (indicated below) or opt for the automated selection of the optimal method. The optimized method is determined as follows:
ImpLiMet selects the largest subset of data with no missing values from the user’s dataset. There must be at least three features and six samples to optimize the imputation method. If this criteria is not met, the user chooses their own implementation method.
Three missing mechanisms are then simulated from the user’s data: missing at random (MAR), missing not at random (MNAR), and missing completely at random (MCAR).
Eight imputation methods are used to impute missing data in these three simulations. These imputation methods include five univariate and three multivariate methods:
univariate methods: mean, minimum, 1/5 minimum, median, and maximum
multi-variate methods: k-nearest neighbour (KNN), Random Forest (RF), and Multivariate Imputation by Chained Equations (MICE) . For the multivariate methods, the required parameters (number of neighbours for KNN, trees for RF, and iterations for MICE) are also optimized.
The performance of each imputation method is determined by the mean absolute percentage error (MAPE).
A comparison table is displayed showing the MAPE for each method. The method with the lowest MAPE across the missingness simulations is suggested as the optimal method.
The effect of the chosen imputation method on the data structure is visualized by principal component analysis (PCA), comparing the impact of removing all features and all samples with missing data to the chosen imputation method. Data are autoscaled prior to PCA.
visit ImpLiMet at: https://complimet.ca/implimet/


To run ImpLiMet locally just change source drives and run app.R in RStudio. 
