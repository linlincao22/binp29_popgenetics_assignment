---
title: "Circos plot and R shiny to visualize Identity Breed Descent data for interested populations "
author: "linlin Cao"
date: 2024.02.24 - 2024.02.26

---

# 1. Purpose 
This project generated an user fridendly web application to draw a circos plot showing the identity breed descent data for 
selected populations people interested from a csv file they uploaded.

# 2. Steps
After randomly selected four populations to do test for circos plot generation and R shiny, we will allow uers to upload 
their populatin data including popname , popsize and mean_IBD data to a web, then allow users to select their interested populations to draw circos plot.

# 3. Install packages
All the code are runned in R version 4.3.2
packages: 
Shiny (version = 1.7.5)
bslib (version = 0.6.1)
shinyWidgets (version = 0.8.1)
DT (version = 0.32)
bsicons (version = 0.1.2)
data.table (version = 1.15.0)
circlize (version = 0.4.16)
RColorBreWer (version = 1.1.3)
ComplexHeatmap (version = 2.16.0)
gridBase (version = 0.4.7)
shinycssloaders (version = 1.0.0)

# 4. Circos plot and R shiny
for code please check the app.R code 
please put the app.R script and two CSV files in the same file folder, after installing all packages and make sure you have the right R version (4.3.2), you can run this app.

# 5. For quckily use this app: 
http://127.0.0.1:6625/
![截屏2024-02-26 14 10 09](https://github.com/linlincao22/binp29_popgenetics_assignment/assets/112622493/3d6d4ed9-e2c7-48a4-8e39-0cc30b948f0d)





you should check the example data table format before uploading your own data,detailed information can be found in information besides the uploading location, yu can also check the example table format

![截屏2024-02-26 14 17 01](https://github.com/linlincao22/binp29_popgenetics_assignment/assets/112622493/3468fdd1-638d-4603-b6d8-ccd9b9bbafa9)



