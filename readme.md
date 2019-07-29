# Exploration of Harambee data! Pink-5 
## Details
* All scripts are in _scripts/_
* The model that predicts whether someone is likely to be working at baseline is *model_1.R*, and uses Naive Bayes.
* The model that predicts whether someone is likely to be working after 6 months is *model_2.R*, and uses KNN.
* *model_2.R* also runs a hierachical clustering model to explore the data.
* All cleaning and model exploration scripts are in *scripts/exploration+cleaning/*
* The cleaning script, *scripts/exploration+cleaning/clean_data.R*, outputs a RDS file used for the supervised models (*clean_data.RDS*) and a csv file which  is used for the clustering model (*dataframe2.csv*).
* The script *scripts/exploration+cleaning/part_2_exploration.R*, was used to investigate which models were best for model_2. Have a look if you'd like to see what other models were tried!

## Usage
Open the folder in RStudio and run the models!