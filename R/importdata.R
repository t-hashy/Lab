# Loading -----------
conf <- config::get()
load.pkg("googlesheets4") # Google sheets
load.pkg("googledrive") # Google drive
load.pkg("httpuv") # Authentiation
load.pkg("tidyverse") # Data manipulation
load.pkg("zoo") # Year month and ma
load.pkg("smooth") # prediction

# Google sheets authentication ---------
options(gargle_oauth_cache= ".secrets")
drive_auth(cache = ".secrets", email = conf$G_EMAIL)
gs4_auth(token = drive_token())

# Import the data ---------
df <- read_sheet(conf$G_SSID_PL, conf$G_SHTNAME_PL) %>%
  select(date, name, pl, amount, `pl-payment-way`,`pl-category`, `pl-detail`, target, `pl-purpose`, `ex-type`, `substitution-paied`, `substitution-paid-date`) %>%
  mutate(
    date = as.Date(date)
  )
names(df) <- gsub("-", ".", names(df))

# Aggregate the data ---------
# Simple plot
df.datescum <- df %>%
  group_by(pl) %>%
  mutate(
    cum = cumsum(amount),
    cum.cal = ifelse(pl == 'Expenditure', -cumsum(amount), cumsum(amount))
  ) %>%
  ungroup(pl) %>%
  mutate(
    diff = cumsum(cum.cal)
  )
ggplot(df.datescum) +
  geom_line(aes(date, cum, color = pl)) +
  geom_line(aes(date, diff, color = "diff"))

# monthly plot
df.ym <- df %>%
  mutate(
    ym = as.yearmon(date)
  ) %>%
  group_by(ym, pl) %>%
  mutate(
    cum = cumsum(amount),
    cum.cal = ifelse(pl == "Expentidure", -cumsum(amount), cumsum(amount))
  ) %>%
  ungroup(ym, pl) %>%
  mutate(
    diff = cumsum(cum.cal)
  )
ggplot(df.ym) +
  geom_line(aes(ym, sma(cum), color = pl))
forecast(object = lm(diff ~ date, df.ym))
