<center>
# Epicalculator
</center>


&nbsp; &nbsp;
------------------------------------------------------------------------------

### Outline of the package:

The package "Epicalculator" is designed for epidemiological data analysis.
The functions are based on count data provided in traditional two-by-two tabular data format. They have the capability of analyzing both crude and stratified data for common caclulations done in epidemiology.

&nbsp;



### Table of Contents:

The package contains the following functions:

| No.  |  Function |  Description    
|---|-----------|--------------------------------------------------------------------------------
| 1  | my.table  | creates upto 5 contingency tables of disease and exposure    
| 2  | risk  | shows estimates, CI and hypothesis test results for risk ratio and risk difference   
| 3 | hp.test  | hypothesis testing for crude and stratified data  
| 4  | crude.table  | creates contingency tables for crude disease and person time data 
| 5  | crude.rate  | calculates crude rate ratio and rate difference with CI   
| 6  | stratified.table  | creates contingency tables for stratified disease and person time data 
| 7  | summary.rate  | calculates summary rate ratio and rate difference with CI 
| 8  | tablex  | Creates three-by-three table for further analysis   
| 9  | OR  | Calculates the crude odds ratio with CI  
| 10  | ORmh  | Calculates the odds ratio with Mantel-Haenszel weighting and CI | 11  | AR  | Calculates the attributable risk with CI  
| 12  | ARpercent  | Calculates the percent attributable risk with CI  
| 13  | PAR  | Calculates the population attributable risk with CI  
| 14  | PARpercent  | Calculates the population attributable risk percentk with CI  
&nbsp;

### Installation:

Epicalculator is currently only available from github. To install the package, run the following code:


```
install.packages("devtools")
devtools::install_github("PHP2560-Statistical-Programming-R/r-package-episquad-2-0")

```

### Authors: 

*Sadia Sharmin 
*Ze Zhang 
*Catrina Mueller-Leonhard


### Contributions:

* Sadia Sharmin - Functions 1 to 3 and documentation of the package
* Ze Zhang - Functions 4 to 7 and code review
* Catrina Mueller-Leonhard - Functions 8 to 14 and documentation of the package


#### License:
GPL-2

#### Version:
1.0

#### Release date:
2017-12-16


#### References:

* Rothman KJ, Greenland S (1998). Modern Epidemiology. Lippincott Williams, & Wilkins, Philadelphia, pp. 271.
* Szklo M, Nieto J (2006). Epidemiology: Beyond the Basics 2nd Edition. Jones & Bartlett Learning, Burlington, MA, Appendix A.


#### Acknowledgments:

* Inspired from Dr. Gregory Wellenius's spreadsheet-EPI202 Calculator 
