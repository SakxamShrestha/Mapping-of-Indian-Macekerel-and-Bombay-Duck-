Fish Species Distribution and Sediment Analysis
This project analyzes the distribution and sedimentary environments of three fish species in the subcontinent of India over the past fifty years. The species studied are:

Indian Mackerel (Rastrelliger kanagurta)
Hilsa (Tenualosa ilisha)
Bombay Duck (Harpadon nehereus)
Project Overview
The project involves retrieving data from the iDigBio database, cleaning the data, creating shapefiles for mapping, and performing regression analysis to study the trends in fish distribution over time. Additionally, the project integrates sediment data to understand the environmental conditions in which these fish species are found.

Requirements
The following R packages are required for this project:

- ridigbio
- sp
- sf
- ggplot2
- raster
- 
Setup and Installation
Install the required packages by running:

install.packages("ridigbio")
install.packages("sp")
install.packages("raster")
install.packages("sf")
install.packages("ggplot2")

Clone or download this repository to your local machine.
Ensure you have the sediment data file (US9_EXT.csv) and the shoreline shapefile (us_medium_shoreline.shp) in the correct directories.
