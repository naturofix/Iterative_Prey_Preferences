# Prey Preference Shiny App

One Paragraph of project description goes here

## Getting Started

This shiny app runs on R. I order to sue it you need to install R and Rstudio
https://rstudio.com/products/rstudio/download/

The program app can also be hosted on a shiny-server running on a linux server

### Prerequisites

This shiny app is written in R. I order to use it you need to install R and Rstudio
https://rstudio.com/products/rstudio/download/

### Installing
Packages used by this app can be installed by by running the package_installation.R

Packages requiring installation are dplyr,tidyr,tibble and ggplot2

shiny apps consist of 3 files.

global.R sets up the environment within which the app runs. 
server.R containes reactive elements, where are the computation is done
ui.R produces the user interface. 

The app is run using the "Run App" button in R studio

### Data
Data is uploaded as a comma separated file with four columns site, species, amount eater and density. The first row may contain headings. It is essential to use the correct column order. 


