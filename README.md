# eutopsFQ

A package, based on Kate Pogue's [foodquotient](https://github.com/katepogue222/foodquotient), to estimate individual-level nutrients and macros from a modified German version of the Food Frequency Questionnaire in adults.

# Usage
To perform FFQ analysis using the foodquotient package, the user must posses data gathered using the FFQ, including each participant's coded responses to ~90 food frequency questions. A mapping and reference database is provided in `eutopsFQ_db`.

## List of functions and their purpose

### fq()
The fq() function serves to convert coded frequency responses from the FFQ to the number of daily servings consumed for each food that each participant eats. fq() is used on a dataframe and will spit out another dataframe of the same dimensions, with the frequency factors replaced with average daily servings. 

### grams()
The grams() function uses the frequency responses of each participant to calculate the daily grams (or ml) of each food consumed for each participant. grams() uses a dataframe and will spit out another dataframe of the same dimension, with each value representing daily grams of each food consumed for that participant. This uses a subfunction named alcohol_g_ethanol to estimate the grams of ethanol per person based on ml intake.

### nutrients()
The nutrients() function uses the frequency responses to calculate the total amount of protein, carbohydrates, and fat, as well as micronutrients and calories, consumed per day for the participant. The function will output a list with two dataframes, named `summary` and `per food`, whereby "calories_kcal", "protein_g", "fat_g", "carb_g", "calcium_mg", "iron_mg", "zinc_mg", "vitamin_c_mg", "vitamin_b6_mg", "vitamin_a_mg", and "folate_mg" are returned overall or per food group, with an identical number of rows to the dataframe that was plugged into the function. 

### micros()
The macros() function uses the age of each participant as well as their 85 frequency responses to calculate the total amount of calcium, 
iron, zinc, vitamin.c, vitamin.b6, vitamin.a, and folate consumed per day for each participant. A dataframe identical in structure to the "age_freq" example dataframe is needed to plug into the macros() function. The function will output a dataframe with 7 columns labeled "calcium", "iron", "zinc", "vitamin.c", "vitamin.b6", "vitamin.a", and "folate" with an identical number of rows to the dataframe that was plugged into the function.

### quotient()
The quotient() function uses the frequency responses to calculate individual level food quotients, which are used to calculate Total Energy Expenditure using the Doubly Labeled Water method. The function will add a column called RQ to the output of nutrients(). Each entry represents the individual level food quotient for each participant. 

### make_components()
The make_components() function uses the output of grams() to provide the daily amount (grams/ml) of components used later for food indices: "fruits", "vegetables", "legumes", "nuts_seeds", "fish", "whole_grains", "refined_grains", "dairy_total", "lowfat_dairy", "red_meat", "processed_meat", "sweets", "ssb", "alcohol", and "fats", using the pick_components() helper function. Indices leveraging these components are:

* modifiedDASH(): 7-component DASH score excluding sodium, which was not evaluated in eutops FFQ
* mds_trichopoulou(): Mediterranean diet score
* hPDI(): healthy plant-based diet index
* LCD(): low-carb diet score

# Installation
You can install eutopsFQ from GitHub using the `devtools` package:
devtools::install_github("chiaraherzog/eutopsFQ")
