# Chrnoic-Pain-CRT

In this project as part of a CRT group I looked at the develompment of and societal effects of chronic pain in European Countries, the datasets are available on request from http://www.share-project.org/fileadmin/pdf_documentation/SHARE_Data_Statement.pdf and I was analysing the datasets in wave 4 and 5 from the verison 7.1.0
The scripts must be run in the following order
1. PainDevData.R    Loads in and isolates the variables of interest.
2. PainDev.R        Logistic regression analysis on the development of chronic pain
3. Bivariateplot.R  EDA: Creates bivariate plots of the variables vs chronic pain development 
4. Healthcare.R     Zero-inflated regression of heatlcare utilisation
5. Retirement.R     Looks at the effects of chronic pain on early retirement
