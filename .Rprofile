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
load.pkg("config", install.only = TRUE)
