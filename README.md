# Fuzzy_cpt_eval
This repository holds the notebooks and associated processing code for executing the Fuzzy changepoint method.

## Running for a single site

The method can be executed for a single site by running the **Fuzzy_cpt_single_site** notebook in either **.Rmd** or **.ipynb** format. The user can set the chosen site and the number of bootstrap samples to generate at the top of the notebook. Outputs will be written to an outputs directory. The code will create this in the home directory of the repo if it does not already exist. The pre-processed data (containing the observed and corresponding model time series aggregated to daily means) is contained in the data directory in a .csv file. Station location information is also located in the data directory. To ensure that all functions execute when the notebook is run the *curr_skipcells* variable must be set to **FALSE**.  

## Running for all sites.

The method can be run for all sites in one run by running the **Fuzzy_cpt_all_sites** notebook. This notebook is run in the same way as the singel site notebook. Note as this processes for all sites, the execution time will be longer.

## Running the Shiny app. 

If the user wishes to explore and visualise the results of the analysis, they can utilise the R Shiny Dashboard. In order to do first change into the Shiny_app directory and launch R. 

```
cd Shiny_app/
R
```

Then at the R command line run the following commands.

```
library(shiny)
runApp()
```

## Raw R code.

The raw R code for the functions to run the analysis is also available in the **R_code** directory. 