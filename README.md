# IMDB-Score-Prediction

Midterm project for MGSC 661 as part of MMA program at McGill University

For this project, I was tasked with creating a regression model to predict the IMDB score of several movies released in December. The dataset consists of detail on 1930 movies on IMDB.

First, exploratory data analysis was conducted to understand the distribution of variables and missing values. I then performed data cleaning to remove irrelevant values and combine several columns. With the selected predictors, I then ran multiple parameter for polynomial and add different knots to the regression model to find the prediction model with the lowest Mean Standard Error.

The final model was then run to predict the score of the upcoming movie.

For confidentiality purpose, the dataset would not be uploaded. However, a data dictionary of the dataset could be found at data_dictionary_IMDB.csv.

Below are a list of the files in the repository:

  * **final_mode.R**: Contains the code to perform data cleaning, feature engineering, implementing the model and generating plots.
  * **Final_report.pdf**: The final report outlining the significance of the problem, the methodology for the analysis and insights gained from the results.
  * **data_dictionary_IMDB.csv**: Data description of the original dataset.
