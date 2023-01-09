# Reliability-study
Code and data for reliability study

The AnalysisandResults folder contains the code and data necessary to replicate the findings in 
"Test-retest reliability of Bayesian estimations of the effects of stimulation, prior information and individual traits on pain perception".


In the folder you can find two subfolders: Parametercorrelation and PrimaryAnalysis. 


In the folder PrimaryAnalysis you can find the following:
-Four stan files with the four different models
-MainAnalysisScript: In order to use this you will have to change the directory to point to the location of the AnalysisandResults folder. 
   In order to find the script section that needs changing "CTRL"+"F" the word "CHANGE"
   Running all the script at once sometimes leads to R crashing. In order for this to not be a problem to obtain the results the script saves each output in a folder as it goes along. 
   This does mean though that the time needed to run it is longer. If R crashes just run it by sections. Always run the first lines of the script until the mark. Then run by sections and go saving.
-Session1Data.csv and Session2Data.csv: These are the datasets with the reduced number of trials (without the ones with low SD). They are the datasets used for the analysis.
-A folder with the session 1 results and another folder with the session 2 results. 
Inside each one of the folders the outputs of the R script MainAnalysisScript will be stored when run. Currently, we have kept the csv files created by the script but we have not included the rds files. 
This is because the size of the files is very big. Nevertheless, these files can be obtained by running the script.
-A csv file named Parameters in which the parameters for both session 1 and 2 are stored. This file is created and saved by the MainAnalysisScript
-A csv file named Parametergraphs. This is the same as the parameters.csv file but in a different format that makes it easier to run the graphs. 
-A R script named reliabilitygraphscript that is used to create the reliability graphs.
-A R script named Simulationgraphs. This script is used to create the scripts of the simulated data. 



In the Parametercorrelation folder you can find the following:
-Two csv files with the data (Session1Data and Session2Data): These files contain the full dataset with all the different types of trials, including the ones a low sd.
-A stan file containing the model used for the analysis (model_4.stan)
-A R script for the analysis: In order to use this you will have to change the directory to point to the location of the AnalysisandResults folder. 
   In order to find the script section that needs changing "CTRL"+"F" the word "CHANGE"
-A folder with the session 1 results and another folder with the session 2 results. 
Inside each one of the folders the outputs of the R script Parametercorrelation will be stored when run. Currently, we have kept the csv files created by the script but we have not included the rds files. 
This is because the size of the files is very big. Nevertheless, these files can be obtained by running the script.


