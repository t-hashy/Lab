source("renv/activate.R")

# LOADING FUNCTIN ------------
load.pkg <- function(pkg, install.only = FALSE, install.available = TRUE) {

  # Library location
  libpath <- "./renv/library/R-4.2/x86_64-w64-mingw32"

  # Install the package
  if(!pkg %in% library(lib.loc=libpath)$results[,1]){

    if(install.available){
      install.packages(pkg, lib = libpath)
      print(paste(pkg, "has been installed."))
    }else{
      message(paste(pkg, "has not been installed, however not done because the argument of 'install.avairable' is FALSE. "))
      return("LOAD ERROR")
    }

  }else{
    print(paste(pkg, "has already been installed."))
  }

  # Load the package
  if(!pkg %in% (.packages())){

    if(install.only){
      return(paste(pkg, "has not been loaded because the argument of 'install.only' is TRUE."))
    }else{
      library(pkg, character.only = TRUE, lib.loc = libpath)
      return(paste(pkg, "has been loaded."))
    }

  }else{
    return(paste(pkg, "has alredy been loaded."))
  }
}


# Loading basic packages -----------
load.pkg("config", install.only = TRUE)
