# Get config ----------
conf <- config::get()

# Google sheets authentication ---------
options(gargle_oauth_cache= ".secrets")
drive_auth(cache = ".secrets", email = conf$G_EMAIL)
gs4_auth(token = drive_token())

# Import the data ---------
df.raw <- read_sheet(conf$G_SSID_PL, conf$G_SHTNAME_PL) %>%
  select(date, name, pl, amount, `pl-payment-way`,`pl-category`, `pl-detail`, target, `pl-purpose`, `ex-type`, `substitution-paied`, `substitution-paid-date`) %>%
  .[!is.na(.$date),] %>%
  mutate(
    date = as.Date(date)
  )  %>%
  mutate(
    uid = apply(., 1, generate_uid),
    .before = date
  )
names(df.raw) <- c("uid", "date", "name", "session", "amount", "way", "category", "details", "target", "purpose", "type", "substitution_paid", "substitution_paid_at")

# Shape the data ------------

# year and yearmonth column and cumulatives
df <- df.raw %>%
  mutate(
    year = format(date, "%Y"),
    ym = as.yearmon(date),
    .after = date
  ) %>%
  mutate(
    pl.cal = ifelse(session != "Expenditure", amount, -amount)
  ) %>%
  mutate(
    cash.cal = case_when(
      way != '立替' ~ pl.cal,
      substitution_paid == TRUE ~ pl.cal,
      .default = 0
    )
  ) %>%
  mutate(
    pl.all = cumsum(pl.cal),
    cash.all = cumsum(cash.cal),
    .after = amount
  ) %>%
  group_by(session) %>%
  mutate(
    cum.all = cumsum(amount),
    .after = amount
  ) %>%
  select(!c(pl.cal, cash.cal)) %>%
  ungroup()


# Write tables of each year ------------

# Set basics
years <- unique(format(df$date, "%Y"))

# Check each year
for(year.this in years){

  # Extract and add column
  df.this <- df %>%
    filter(
      year == year.this
    ) %>%
    mutate(
      pl.cal = ifelse(session != "Expenditure", amount, -amount)
    ) %>%
    mutate(
      cash.cal = case_when(
        way != '立替' ~ pl.cal,
        substitution_paid == TRUE ~ pl.cal,
        .default = 0
      )
    ) %>%
    mutate(
      pl.year = cumsum(pl.cal),
      cash.year = cumsum(cash.cal),
      .after = cash.all
    ) %>%
    group_by(session) %>%
    mutate(
      cum.year = cumsum(amount),
      .after = cash.all
    ) %>%
    select(uid, date, year, ym, name, session, amount, cum.all, cum.year, pl.all, pl.year, cash.all,cash.year, way, category, details, target, purpose, type, substitution_paid, substitution_paid_at) %>%
    ungroup()

  # Save the data to Google speradsheet
  df.this.toSave <- df.this %>%
    mutate(
      ym = as.character(ym)
    )
  write_sheet(df.this.toSave,conf$G_SSID_PERF, year.this)

  # Merge tables
  df <- merge.data.frame(df, df.this, all.x = TRUE) %>%
    select(uid, date, year, ym, name, session, amount, cum.all, cum.year, pl.all, pl.year, cash.all,cash.year, way, category, details, target, purpose, type, substitution_paid, substitution_paid_at)
}

# Update all time sheet ------------------
df.toSave <- df %>%
  mutate(
    ym = as.character(ym)
  )
allsht.old <- sheet_names(conf$G_SSID_PERF) %>%
  grep("all", ., ignore.case = TRUE, value = TRUE)
allsht.new <- paste("all_at", format(Sys.Date(), "%Y%m%d"), sep = "")
sheet_rename(conf$G_SSID_PERF, allsht.old, allsht.new)
write_sheet(df.toSave, conf$G_SSID_PERF, allsht.new)

# Check the data ----------------
plot <- ggplot(data = df) +
  geom_line(aes(ym, cum.all, color = paste(session,"_all", sep = ""))) +
  geom_line(aes(ym, pl.all, color = "PL.all")) +
  geom_line(aes(ym, cash.all, color = "Cash.all")) +
  geom_line(aes(ym, cum.year, color = paste(session,"_year", sep = ""))) +
  geom_line(aes(ym, pl.year, color = "PL.year")) +
  geom_line(aes(ym, cash.year, color = "Cash.year")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "PL",
    caption = "data from PL",
    x = "",
    y = "amount(yen)"
  )
ggplotly(plot)
