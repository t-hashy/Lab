source("renv/activate.R")

# LOADING FUNCTIN ------------
load.pkg <- function(pkg, install.only = FALSE, install.available = TRUE) {

  # Install the package
  if(!pkg %in% library()$results[,1]){

    if(install.available){
      install.packages(pkg)
      library(pkg, character.only = TRUE)
      print(paste(pkg, "has been installed and Loaded."))
    }else{
      message(paste(pkg, "has not been installed, however not done because the argument of 'install.avairable' is FALSE. "))
      return("LOAD ERROR")
    }
  }else{
    print(paste(pkg, "has already been installed."))

    # Load the package
    if(!pkg %in% (.packages())){

      if(install.only){
        return(paste(pkg, "has not been loaded because the argument of 'install.only' is TRUE."))
      }else{
        library(pkg, character.only = TRUE)
        return(paste(pkg, "has been loaded."))
      }
    }else{
      return(paste(pkg, "has alredy been loaded."))
    }
  }
}


# Loading basic packages -----------

# Configuration and authenticatoin
load.pkg("config", install.only = TRUE)
load.pkg("httpuv") # Authentiation


# Google
load.pkg("googlesheets4") # Google sheets
load.pkg("googledrive") # Google drive

# Data manipulation
load.pkg("tidyverse") # Data manipulation
load.pkg("zoo") # Year month and ma
load.pkg("DT") # Interactive data table

# Prediction and models
load.pkg("smooth") # prediction

# Data base management
load.pkg("DBI") # Data base management
load.pkg("RSQLite") # SQLite

# Plotting
load.pkg("ggplot2") # Plot
load.pkg("scales") # Plot formatting
load.pkg("plotly") # Interactive plot

# Utility functions ----------
generate_uid <- function(current.uids = list(), uid.len = 8){
  # Check requirements
  if(uid.len <= 0){
    return("'uid.len' must be longer than 0L, especially longer than or equal to 8L would be recommended.")
  }

  # Set basics
  chars <- c(LETTERS, letters, 0:9)
  new.uid <- NULL
  checker <- FALSE

  # Generate new uid
  while(isFALSE(checker)){

    # Create uid
    for(i in 1:uid.len){
      char <- sample(chars, 1)
      new.uid <- paste(new.uid, char, sep = "")
    }

    # Check the duplication
    if(!new.uid %in% current.uids){
      checker <- TRUE
    }
  }

  # Return
  return(new.uid)
}
