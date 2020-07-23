# R Implementation of the Adair and Lopez (2018) method for estimating death registration completeness

### Implementation first uploaded: 17 July 2020

### Implementation last updated: 17 July 2020


# File structure of implementation
* **fn\_Adair\_Lopez\.R** is an R script that contains the `AdairLopezPrediction()` function that predicts death registration completeness using the six models in Adair & Lopez (2018) -- Model 1 for females/males/both and Model 2 for females/males/both

* **Adair_Lopez_Coefficient_Estimates.Rdata** is an R-specific file that contains the following two data frames:
   + `AL_model_coefficients`, which shows the coefficient estimates from Table 2 of Adair & Lopez (2018) for the six models
   +  `AL_RE_coefficients`, which shows estimated random effects terms from Supporting Information Tables 2-7 in Adair & Lopez (2018) for the six models
   
* **Supporting_information_data.csv** is the example dataset from the supplemental materials of Adair & Lopez (2018) 

* **Implementation_of_Adair_Lopez.Rmd** is an R Markdown file that shows how to use the three files above to predict completeness based on each of the six models from Adair & Lopez (2018)

* **example_of_implementation.html** is the output of `Implementation_of_Adair_Lopez.Rmd` and can just be viewed in a web browser


## References
Tim Adair and Alan D. Lopez. "Estimating the completeness of death registration: an empirical method." PloS one 13, no. 5 (2018): e0197047.
