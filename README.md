Accessing, tyding and cleaning JNA 2024 data. Sectorial indicators calculation and PiN (IPM methodology) calculation. 
https://rstudio.unhcr.org/Catalogo_JNA_R4V/

The project contains three files. The first file reads the dataset directly from Kobo. If you are working offline, you can insert your CSV databases instead of applying this first step (1_prepare_JNA_data.R).
 
The second R file (2_indicators_calculation.R) assigns a score of 1 to each person suffering from deprivation and 0 if not, utilizing the thresholds established for calculating the JNA indicators. For indicators where the question is specific to a group (children, women, etc.), a deprivation score of 1 should be applied to all other members (this applies for all sectors, except nutrition).
 
The df_weights file consists of the weights for each indicator calculated in step 2. The third file (3_pins_calculation.R) calculates the sectorial deprivation score (ds) by summing weighted indicators per sector. The intersectoral PiN is calculated using the scores from each sector and a 33.3% cutoff to identify those to the right (>33.35) of the distribution. The intersectoral PiN is calculated with 9 dimensions as reflected in the codes.
 
Once we have the intersectoral PiN, we can calculate the PiN of each sector. To be part of the sectoral PiN, an individual needs to have a deprivation in a respective sectoral dimension as well as a 1 in the intersectoral PiN. Different from other sectors, for the Protection subsectors, an individual needs to have a deprivation in the respective sectoral dimension and a 1 in the Protection PiN. The codes then automatically produce the pin_final Excel file that will generate the PiN for each sector and intersector.
