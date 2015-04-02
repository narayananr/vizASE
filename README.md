# vizASE
A Shiny app for visualizing gene-level and allele-level expressions from NODxPWK reciprocal F1 stududy, interactively.

Software requirements:
-- R
-- Shiny (http://shiny.rstudio.com/)
-- ggplot2
-- reshape2

---vizASENxP---

vizASENxE directory contains two R scripts, server.R & ui.R,  needed to run the Shiny web app
In server.R code, change the fifth line
"""""setwd("~/Work/Projects/NodPwkF1/data")"""""
so that it points to the right path to data directory

After installing the software requirements,  run the Shiny app by typing the command in R

 runApp(appDir="~/vizASENxP/")

the argument to "appDir" should point to the right path to vizASENxP directory containing the Shiny R scripts
