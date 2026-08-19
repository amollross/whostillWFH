# whostillWFH
This repository contains the R code and related non-HILDA data to replicate analysis in _Who still WFH? The changing traits of remote workers_ by Aaron Mollross.


The code can be run directly from the **00_MASTER_RUN_FILE.R**, after editing the two "location/" lines in the file to the location of, respectively, the folder containing the combined data from HILDA wave 24 (restricted release) and the desired output folder. The 00_MASTER_RUN_FILE.R does not require downloading the non-HILDA data and other code snippets to run, as it will draw them directly from this repository.


**Data availability:** The code in this repository relies on access to HILDA Survey Restricted Release 24, but this repository **does not** contain the HILDA Survey microdata used in the analysis. Access to HILDA data is subject to approval by the Australian Government Department of Social Services. Researchers can apply for access through the Australian Data Archive Dataverse. The code in this repository is designed to reproduce the analysis once the required HILDA files from the wave 24 restricted release have been obtained.


The **nonHILDA_data_sources** folder contains two datasets needed for the analysis:
- The 'teleworkability' score of each 4-digit ANZSCO occupation in Australia. These scores are derived from Dingel & Neiman (2020), cross-walked to Australian occupation codes using a correspondence from Jobs and Skills Australia, with equivalent 3-, 2- and 1-digit scores for nfd occupations derived from the average of 4-digit scores, weighted by employment counts at the 2016 Census. The original Dingel-Neiman scores are available at https://github.com/jdingel/DingelNeiman-workathome/.
- The geographic coordinates (longitude and latitude) for the centre of each state and territory capital city in Australia, to derive distance between HILDA survey participants and the nearest major city. Locations of these coordinates (listed in the file) were selected on the basis of my personal judgement about the geographic centre of each city. 
