# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-23
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) launch dashboard in RShiny (creates Shiny app objects from an explicit UI/server pair)
# Notes:
# 1) This script is part of an RProject
# 2) the ui.R and server.R files need to be in the working directory 

#shiny must already be an installed package to launch app
#this library also gets installed in the packages_to_install.R file which is part of global.R
library("shiny")

runApp(launch.browser = TRUE)