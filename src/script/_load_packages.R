# Meta Data ---------------------------------------------------------------
#
# Version:      1.1
# Author:       Oleksii Dovhaniuk
# Created on:   2023-12-13
# Updated on:   2024-01-15
#
# Description:  This script loads all required packages for
#               the TMM Analysis project. Can be called 
#               from any other job script.
#
# Location:     script/_load_packages.R
#


# Load Packages --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(ggplot2)
library(glue)
library(plumber)
library(Rook)
library(xlsx)
library(readxl)
library(httr)
library(jsonlite)


