# Coding Example

Example code for data cleaning, reshaping, analysis and visualisation for data collected in a pilot replication study.

This Repo contains two R scripts as example code:

"pilot_analysis.R" contains code to import a dataset, install or import required R packages, call on a separate custom R function for cleaning and reformatting the dataset ("pilot_cleaning.R"), perform data analysis and some inferential testing, and create data visualisations.

"pilot_cleaning.R" contains a function which cleans and reformats the dataset, by reverse scoring key items in some trait characteristic inventories, creates composite variables for trait characteristic and emotion rating data, and largely reshapes the data from wide to long format, for easier analysis and data visualisation.

This Repo also contains a small anonymised pilot dataset that can be analysed with the accompanying R code.

You can download these scripts and the anonymised dataset. If the two scripts and dataset are all within the same folder and directory level, all you need to do is update line 15 of the code in "pilot_analysis.R" to reflect the directory path for the files on your system. From there, you can run the script and function to generate data analysis outputs and some visualisations.

The scripts are developed to analyse data from a music psychology experiment, with details fully documented in the following pre-registration: https://osf.io/bqt8w?mode=&revisionId=&view_only=.
