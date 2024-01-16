# Meta Data ---------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2024-01-15
#
# Description:  This script installs all required packages for 
#               the TMM Analysis project if the R and Rstudio are
#               freshly installed.
#
# Location:     script/_install_packages.R
#


# Install Packages --------------------------------------------------------


install.packages(
    c(  
        'devtools',
        'ggplot2',
        'janitor',
        'jsonlite',
        'httr', 
        'glue',
        'lubridate',
        'plumber',
        'readxl',
        'Rook',
        'tidyverse'
    )
)