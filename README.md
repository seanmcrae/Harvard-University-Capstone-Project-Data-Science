# Harvard-University-Final-Capstone-Data-Science
In this final course in the Harvard University Data Science Professional Certificate, I show what I’ve learned in the 9 courses of the Professional certificate program by creating a long project of my own and having it assessed by the Harvard University Professor of BioStatistics: Rafael Irizarry

Includes 2 Capstone projects:
1. The submission for the MovieLens project will be three files: a report in the form of an Rmd file, a report in the form of a PDF    document knit from your Rmd file, and an R script or Rmd file that generates your predicted movie ratings and calculates RMSE. Your grade for the project will be based on two factors: 1. Your report and script (75%)
                                              2. The RMSE returned by testing your algorithm on the validation set (25%)

2. I have submitted my own project using a dataset of my choosing. My project has been reviewed both by my peers and the professor.
   I chose to work with Credit Card Fraud Detection, It is important that credit card companies are able to recognize fraudulent              credit card transactions so that customers are not charged for items that they did not purchase. The datasets contains 
   transactions made by credit cards in September 2013 by european cardholders.
   Due to imbalancing nature of the data, many observations could be predicted as False Negative, in this case Legal Transactions instead 
   of Fraudolent Transaction. For example, a model that predict always 0 (Legal)can archieve an Accuracy of 99.8.
   For that reason, the metric used for measuring the score is the Area. Under The Precision-Recall Curve (AUCPR) instead of the 
   traditional AUC curve. A desiderable result is an AUCPR at least greater than 0.85. For archieving the task of classifying credit card 
   fraud detection, they are trained several algorithms such as Naive Bayes Classifier, KNN, SVM, Random Forest, GBM, XGBoost and LightGBM.
   In this analysis, a XGBoost Model is capable of an AUCPR of 0.8623.

