**To run the program use R application and follow these steps**
  
**install all necessary libraries using**

install.packages(c("shiny", "shinyjs", "DBI", "RSQLite", "dplyr", "ggplot2", "tidytext", "sentimentr"))

**next set location**

setwd("D:/VS Code/CustomerFeedbackManagementSystem")  #your file location in disk

**run the app using**

library(shiny)

runApp("app.R")
