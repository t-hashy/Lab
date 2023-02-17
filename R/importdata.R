# Loading -----------
conf <- config::get()
load.pkg("googlesheets4") # Google sheets
load.pkg("googledrive") # Google drive
load.pkg("httpuv") # Authentiation
load.pkg("tidyverse") # Data manipulation

# Google sheets authentication ---------
options(gargle_oauth_cache= ".secrets")
drive_auth(cache = ".secrets", email = conf$G_EMAIL)
gs4_auth(token = drive_token())

# Import the data ---------
df <- read_sheet(conf$G_SSID_PL, conf$G_SHTNAME_PL)
