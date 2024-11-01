Overview of ImpLiMet - Imputation for Lipidomics and Metabolomics

Imputation, the process of replacing missing values in a dataset with estimated values, is required for multivariate and machine learning analyses. Data element omissions happen for a variety of experimental or analytical reasons, often unavoidable in high throughput measurements. Three missingness patterns have been conceptualized: missing completely at random (MCAR), missing at random (MAR), and missing not at random (MNAR).  Each presents unique dependencies between the missing and observed data. As a result, the optimal imputation method for each dataset depends on the type of data, the cause of the missing data, and the nature of relationships between the missing and observed data.  The challenge is to identify the optimal imputation solution for a given dataset.

ImpLiMet enables users to impute missing data using 8 different methods and proposes the optimal imputation approach for the user’s data set if users have at least three features and six samples without any missing values in their dataset. Five univariate and three multivariate methods are available: mean, minimum, 1/5 minimum, median, and maximum or KNN, RF, and MICE.  Users can also opt for an automated selection of the optimal imputation method. The optimized method is determined using the largest subset of data with no missing values from the user’s dataset. There must be at least three features and six samples for this optimization. MAR, MNAR, and MCAR missing mechanisms are simulated from the user’s data. All eight imputation methods are used to impute missing data across the three simulations. The performance of each imputation method is determined by the mean absolute percentage error (MAPE). A comparison table is displayed showing the MAPE for each method. The method with the lowest MAPE across the missingness simulations is suggested as the optimal method.  If the criteria required for the optimization is not met, the user can still select a single implementation method. The effect of the chosen imputation method on the data structure is visualized by histogram representation of the dataset, kurtosis and skewness feature characteristics as well as principal component analysis (PCA), comparing the impact of removing all features and all samples with missing data to the chosen imputation method. Data are autoscaled prior to PCA. Visit ImpLiMet with UI at: https://complimet.ca/shiny/implimet/.  


ImpLiMet is actively maintained, and new features will be added. User suggestions are appreciated and will be considered. 

Authors

See doc/CREDITS for the list of contributors to ImpLiMet

Availability

The latest code and offical ImpLiMet release is published online at https://github.com/complimet/implimet and made available as UI online at: https://complimet.ca/shiny/implimet/ ImpLiMet is open-source and open-code software under the LGPLv3 or later. See doc/COPYING and doc/LICENSE for licensing details.

Contact

For additional information and contacting the authors, please consult the ImpLiMet web page at ((https://complimet.ca/shiny/implimet/)). To report an error or file a request, please send an email to: ldomic@uottawa.ca.

Documentation

For installation instructions see the install.md file; User guide is provided at: https://complimet.ca/shiny/implimet/
