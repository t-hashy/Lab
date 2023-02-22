# Get config ----------
conf <- config::get()



### GET THE RAW DATA ### -------
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
  .[order(.$date),] %>%
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
df$substitution_paid[df$way != "立替"] <- FALSE


# Write tables of each year ------------

# Set basics
years <- unique(format(df$date, "%Y"))
df.years <- data.frame()

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
        isTRUE(substitution_paid) ~ pl.cal,
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
    select(uid, date, year, ym, name, session, amount, cum.all, cum.year, pl.all, pl.year, cash.all,cash.year, way, category, details, target, purpose, type, substitution_paid) %>%
    ungroup() %>%
    .[order(.$date),]

  # Save the data to Google speradsheet
  df.this.toSave <- df.this %>%
    mutate(
      ym = as.character(ym)
    )
  write_sheet(df.this.toSave,conf$G_SSID_PERF, year.this)

  # Merge tables
  df.years <- rbind(df.years, df.this) %>%
    select(uid, date, year, ym, name, session, amount, cum.all, cum.year, pl.all, pl.year, cash.all,cash.year, way, category, details, target, purpose, type, substitution_paid)

}

# Update all time sheet ------------------
df.toSave <- df.years %>%
  mutate(
    ym = as.character(ym)
  )
allsht.old <- sheet_names(conf$G_SSID_PERF) %>%
  grep("all", ., ignore.case = TRUE, value = TRUE)
allsht.new <- paste("all_at", format(Sys.Date(), "%Y%m%d"), sep = "")
sheet_rename(conf$G_SSID_PERF, allsht.old, allsht.new)
write_sheet(df.toSave, conf$G_SSID_PERF, allsht.new)

# Check the data ----------------
plot <- ggplot(data = df.years) +
  geom_line(aes(ym, cum.all, color = paste(session,"_all", sep = ""))) +
  geom_line(aes(ym, pl.all, color = "PL_all")) +
  geom_line(aes(ym, cash.all, color = "Cash_all")) +
  geom_line(aes(ym, cum.year, color = paste(session,"_", year, sep = ""))) +
  geom_line(aes(ym, pl.year, color = paste("PL_", year, sep = ""))) +
  geom_line(aes(ym, cash.year, color = paste("Cash_", year, sep = ""))) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "PL",
    caption = "data from PL",
    x = "",
    y = "amount(yen)"
  )
ggplotly(plot)


### FIX THE CASH ### ----
# Fix cash remaining and substitution paid data ------

# Get the cash now
cash.now.account <- conf$CASH_NOW
cash.now.pl <- df.years$cash.all[length(df.years$cash.all)]

# Check the difference between pl and real amount
cash.lack <- cash.now.pl - cash.now.account

# Extract the substitution unpaid data
df.unpaid <- df.years %>%
  filter(
    substitution_paid == FALSE,
    way == "立替"
  ) %>%
  select(uid, date, name, amount) %>%
  mutate(
    cum.all = cumsum(amount)
  ) %>%
  group_by(name) %>%
  mutate(
    cum.each = cumsum(amount)
  ) %>%
  ungroup()

# Get the latest total unpaid amount -----
unpaid.total <- max(df.unpaid$cum.all)
names <- unique(df.unpaid$name)
df.unpaid.check <- data.frame(
 name = names
) %>%
  mutate(
    unpaid.amount = unlist(map(names, function(name.this) df.unpaid$cum.each[df.unpaid$name == name.this] %>% max))
  ) %>%
  mutate(
    unpaid.ratio = unlist(map(unpaid.amount, function(am) am / unpaid.total))
  ) %>%
  mutate(
    to.fix = unlist(map(unpaid.ratio, function(am) ceiling(am * cash.lack)))
  )

# Detect where to fix with substitution paid
df.toFix <- df.unpaid %>%
  mutate(
    sub.toFix = unlist(map2(cum.each, name, function(cum.this = .x, name.this = .y){
      if(cum.this <= df.unpaid.check$to.fix[df.unpaid.check$name == name.this]){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }))
  )

# Fix the substitution paid
df.fixed <- df.years %>%
  mutate(
    substitution_paid = ifelse(uid %in% df.toFix$uid[df.toFix$sub.toFix == TRUE], TRUE, substitution_paid)
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
    cash.all = cumsum(cash.cal),
    .after = amount
  ) %>%
  select(!cash.cal)

# Remove substition_paid is TRUE and not substituted
df.fixed$substitution_paid[df.fixed$way != "立替"] <- FALSE

# Write and Check the fixed data ----------
# Set basics
years <- unique(format(df$date, "%Y"))
df.fixed.new <- data.frame()

# Check each year
for(year.this in years){

  # Extract and add column
  df.this <- df.fixed %>%
    filter(
      year == year.this
    ) %>%
    mutate(
      pl.cal = ifelse(session != "Expenditure", amount, -amount)
    ) %>%
    mutate(
      cash.cal = case_when(
        way != '立替' ~ pl.cal,
        isTRUE(substitution_paid) ~ pl.cal,
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
    select(uid, date, year, ym, name, session, amount, cum.all, cum.year, pl.all, pl.year, cash.all,cash.year, way, category, details, target, purpose, type, substitution_paid) %>%
    ungroup() %>%
    .[order(.$date),]

  # Save the data to Google speradsheet
  df.this.toSave <- df.this %>%
    mutate(
      ym = as.character(ym)
    )
  write_sheet(df.this.toSave,conf$G_SSID_PERF, year.this)

  # Merge tables
  df.fixed.new <- rbind(df.fixed.new, df.this) %>%
    select(uid, date, year, ym, name, session, amount, cum.all, cum.year, pl.all, pl.year, cash.all,cash.year, way, category, details, target, purpose, type, substitution_paid)

}

# Update all time sheet ------------------
df.toSave <- df.fixed.new %>%
  mutate(
    ym = as.character(ym)
  )
allsht.old <- sheet_names(conf$G_SSID_PERF) %>%
  grep("all", ., ignore.case = TRUE, value = TRUE)
allsht.new <- paste("all_at", format(Sys.Date(), "%Y%m%d"), sep = "")
sheet_rename(conf$G_SSID_PERF, allsht.old, allsht.new)
write_sheet(df.toSave, conf$G_SSID_PERF, allsht.new)

# Check the data ----------------
plot <- ggplot(data = df.fixed.new) +
  geom_line(aes(ym, cum.all, color = paste(session,"_all", sep = ""))) +
  geom_line(aes(ym, pl.all, color = "PL_all")) +
  geom_line(aes(ym, cash.all, color = "Cash_all")) +
  geom_line(aes(ym, cum.year, color = paste(session,"_", year, sep = ""))) +
  geom_line(aes(ym, pl.year, color = paste("PL_", year, sep = ""))) +
  geom_line(aes(ym, cash.year, color = paste("Cash_", year, sep = ""))) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "PL",
    caption = "data from PL",
    x = "",
    y = "amount(yen)"
  )
ggplotly(plot)





### FIX THE SUBSTITUTINO PAID DATES ### ----

# Substitution data ----
df.subs <- df.fixed.new %>%
  select(uid, date, session, amount, way, substitution_paid) %>%
  mutate(
    isSub = ifelse(way == "立替", "sub", "others"),
    isPaid =ifelse(substitution_paid == TRUE, "paid", "others")
  ) %>%
  group_by(session,way) %>%
  mutate(
    cum = cumsum(amount),
    .after = amount
  ) %>%
  ungroup() %>%
  mutate(
    pl.cal = ifelse(session == "Expenditure", -amount, amount),
    .after = amount
  ) %>%
  mutate(
    pl = cumsum(pl.cal),
    .after = amount
  ) %>%
  group_by(isSub) %>%
  mutate(
    cum.with.sub = cumsum(pl.cal)
  ) %>%
  ungroup() %>%
  group_by(isSub, isPaid) %>%
  mutate(
    cum.along.with.sub = cumsum(pl.cal)
  ) %>%
  select(!pl.cal)

# See the data -----
plot <- ggplot(df.subs) +
  geom_line(aes(date, cum.along.with.sub, colour = paste(isSub, isPaid)))
ggplotly(plot)

# Add substitution_paid_at column with condition where the cash total not goes to negative ----

# Set variables
uids.paid <- df.subs$uid[df.subs$isPaid == "paid"]
uids.others <- df.subs$uid[df.subs$isSub == "others"]
df.paid_at.added <- df.subs %>%
  ungroup() %>%
  mutate(
    substitution_paid_at = as.Date(NA)
  )

# Check for each substitutinos; set these as after cummulative values goes positieve ----
for(uid.this in uids.paid){

  # Set substitution to pay
  cum.paid.this <- -df.paid_at.added$cum.along.with.sub[df.paid_at.added$uid == uid.this]

  # Loop until payment goes over pl
  for(uid.others in uids.others){
    cum.this <- df.paid_at.added$cum.along.with.sub[df.paid_at.added$uid == uid.others]
    if(cum.this > cum.paid.this) {
      df.paid_at.added$substitution_paid_at[df.paid_at.added$uid == uid.this] <- df.paid_at.added$date[df.paid_at.added$uid == uid.others] %>%
        as.Date()
      break
    }
  }
}

# See the data ----
plot <- ggplot(df.paid_at.added) +
  geom_line(aes(date, cum.along.with.sub, colour = paste(isSub, isPaid))) +
  geom_line(aes(substitution_paid_at, cum.along.with.sub, colour = paste(isSub, isPaid))) +
  geom_hline(yintercept = 0, alpha = 0.5)
ggplotly(plot)

# Shape up the data -----------
df.shaped <- df.paid_at.added %>%
  mutate(
    date_paid.at = ifelse(substitution_paid == FALSE, date, substitution_paid_at) %>% as.Date(),
    .after = date
  ) %>%
  select(!c(isPaid, isSub, cum.with.sub, cum.along.with.sub, pl, cum)) %>%
  order_by(order(date),)
  mutate(
    amount.cal = ifelse(session == "Expenditure", -amount, amount),
    .after = amount
  ) %>%
  mutate(
    pl.total = cumsum(amount.cal),
    .after = amount
  )
