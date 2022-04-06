# Dengue
These are datasets and codes for 'A Small Variation in Weather Conditions Could Lead to Dramatic Changes of Dengue Outbreaks' and there are three files.

1: File 'Weather' contains dataset of weather factors for Southeast Asian and South American regions mentioned in the text and we have handled the missing values by replacing with the mean value. (air-pollution is not available for South American regions.)

2: File 'Dengue_number' contains dataset of dengue case count for all Southeast Asian and South American regions mentioned in the text and we have handled the missing values by replacing with the mean of the same year. (Originally, Singapore and South American regions have weekly data while Thailand has monthly data but we have converted the monthly data of Thailand to weekly data.)

3: File 'Code' contains R-code for the models and simulations in the project. 'some_code' is the code to illustrate the multi-step prediction as well as calculating p-values for each coefficient. You can first execute 'some_code' to import the packages used for other files. Other files are codes for figures in the main context and SI. Please add all datasets into your computer first.
