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
cash.diff <- cash.lack

# Extract the substitution unpaid or paid data
df.target <- data.frame()
if(cash.lack < 0){
  df.target <- df.years %>%
    filter(
      substitution_paid == TRUE,
      way == "立替"
    ) %>%
    select(uid, date, name, amount) %>%
    mutate(
      cum.all = order_by(desc(date) ,cumsum(amount))
    ) %>%
    group_by(name) %>%
    mutate(
      cum.each = order_by(desc(date), cumsum(amount))
    ) %>%
    ungroup()
  cash.diff <- -cash.lack
}else{
  df.target <- df.years %>%
    filter(
      substitution_paid == FALSE,
      way == "立替"
    ) %>%
    select(uid, date, name, amount) %>%
    mutate(
      cum.all = order_by(date, cumsum(amount))
    ) %>%
    group_by(name) %>%
    mutate(
      cum.each = order_by(date, cumsum(amount))
    ) %>%
    ungroup()
}

# Get the latest total unpaid amount -----
df.check <- df.target%>%
  group_by(name) %>%
  summarise(
    total = sum(amount)
  ) %>%
  mutate(
    ratio = total/sum(.$total),
    total.toFix = round( cash.diff*ratio)
  )

# Detect where to fix with substitution paid ----
df.toFix <- df.target %>%
  mutate(
    sub.toFix = unlist(map2(cum.each, name, function(cum.this = .x, name.this = .y){
      if(cum.this <= df.check$total.toFix[df.check$name == name.this]){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }))
  )
uids.toFix <- df.toFix$uid[df.toFix$sub.toFix == TRUE]

# Fix the substitution paid ----
df.fixed <- df.years %>%
  mutate(
    substitution_paid = case_when(
      uid %in% uids.toFix ~ ifelse(cash.lack < 0, FALSE, TRUE),
      TRUE ~ substitution_paid
    )
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
  .[order(.$date, .$session),] %>%
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
uids.subs.paid <- df.subs$uid[df.subs$isPaid == "paid"]
uids.pl.others <- df.subs$uid[df.subs$isSub == "others"]
df.paid_at.added <- df.subs %>%
  ungroup() %>%
  mutate(
    substitution_paid_at = as.Date(NA)
  )

# Check for each substitution; set these as after cumulative values goes positive ----
for(uid.subs.paid.this in uids.subs.paid){

  # Set substitution to pay
  cum.subs.paid.this <- -df.paid_at.added$cum.along.with.sub[df.paid_at.added$uid == uid.subs.paid.this]
  date.subs.paid.this <- df.paid_at.added$date[df.paid_at.added$uid == uid.subs.paid.this] %>%
    as.Date()

  # Loop until payment goes over pl
  for(uid.pl.others.this in uids.pl.others){
    cum.pl.others.this <- df.paid_at.added$cum.along.with.sub[df.paid_at.added$uid == uid.pl.others.this]
    date.pl.others.this <- df.paid_at.added$date[df.paid_at.added$uid == uid.pl.others.this] %>%
      as.Date()
    if((cum.pl.others.this > cum.subs.paid.this + 180000) && (date.pl.others.this >= date.subs.paid.this)) {
      year.pl.others.this <- format(date.pl.others.this, "%Y") %>% as.numeric()
      mon.pl.others.this <- format(date.pl.others.this, "%m") %>% as.numeric() + 1
      if(mon.pl.others.this > 12){
        mon.pl.others.this <- 1
        year.pl.others.this <- year.pl.others.this + 1
      }
      df.paid_at.added$substitution_paid_at[df.paid_at.added$uid == uid.subs.paid.this] <- paste(year.pl.others.this, mon.pl.others.this, 1, sep = "-") %>% as.Date() -1
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




### SHAPE UP THE WHOLE DATA ### ----
# Update the substitution paid --------
sht.raw <- read_sheet(conf$G_SSID_PL, conf$G_SHTNAME_PL)
sht.added <- sht.raw %>%
  mutate(rownum = row_number() + 1)
col.chr <- index(colnames(sht.raw)) %>% .[colnames(sht.raw) == "substitution-paied"] %>%
  LETTERS[.]
df.replaced <- data.frame()
replaceTo <- data.frame(
  substitution_paid = ifelse(cash.lack < 0, FALSE, TRUE)
)
for(uid.this in uids.toFix){

  # Get the data
  data.this <- df.fixed[df.fixed$uid == uid.this,]

  # Find the row
  row <- sht.added %>%
    filter(
      date == data.this$date,
      amount == data.this$amount,
      name == data.this$name,
      pl == data.this$session,
      `pl-payment-way` == data.this$way,
      `pl-category` == data.this$category,
      `pl-detail` == data.this$details,
      `target` == data.this$target,
      `pl-purpose`== data.this$purpose,
      `ex-type` == data.this$type
    ) %>%
    .[1,]

  if(row$`substitution-paied` != replaceTo[1,1]){

    # Overwrite the sheet
    row.num <- row$rownum
    range_write(
      ss = conf$G_SSID_PL,
      data = replaceTo,
      sheet = conf$G_SHTNAME_PL,
      range = paste(col.chr, row.num, sep = ""),
      col_names = FALSE
    )

    # Add to replaced
    sheet_append(conf$G_SSID_PL, row %>% mutate(updated_at = Sys.time()) %>% select(!rownum), "Fixed")
  }
}

# Shape the all-time data -----------

# Set a basic array
df.shaped.base <- df.paid_at.added %>%
  mutate(
    date_paidat = ifelse(substitution_paid == FALSE, date, substitution_paid_at) %>% as.Date(),
    .after = date
  ) %>%
  select(uid, date, date_paidat, session, amount, way, substitution_paid) %>%
  merge.data.frame(df.years %>% select(uid,name, category, details, target, purpose, type), all.x = TRUE) %>%
  select(uid,date, date_paidat, session, amount, details, way, category, target, purpose, type, name, substitution_paid) %>%
  .[order(.$date, .$date_paidat, .$session, .$purpose, .$type, .$category),]

# Shape the all
df.shaped <- df.shaped.base %>%
  mutate(
    cal.amount.pl = ifelse(session == "Expenditure", -amount, amount),
    pl_total =  order_by(date, cumsum(cal.amount.pl)),
    .after = amount
  ) %>%
  mutate(
    isPaid = case_when(
      way != "立替"　~ "paid",
      substitution_paid == TRUE ~ "paid",
      TRUE ~ "unpaid"
    )
  ) %>%
  group_by(isPaid) %>%
  mutate(
    cash_total = order_by(date_paidat, cumsum(cal.amount.pl)),
    .after = pl_total
  ) %>%
  group_by(isPaid, name) %>%
  mutate(
    unpaid_each = ifelse(isPaid == "unpaid", order_by(date, cumsum(cal.amount.pl)), NA),
    .after = cash_total
  ) %>%
  ungroup()

# See the data ----
par(mfrow = c(2,1))
plot <- ggplot(df.shaped)+
  geom_line(aes(date, pl_total, colour = "pl")) +
  geom_line(aes(date_paidat, cash_total, colour = ifelse(isPaid == "paid", "cash", "unpaid all"))) +
  geom_line(aes(date, unpaid_each , colour = paste("unpaid", name)), na.rm = TRUE, data = df.shaped %>% filter(isPaid == "unpaid")) +
  geom_hline(yintercept = 0, colour = "black", alpha = 0.5, linetype = "solid") +
  geom_hline(yintercept = 200000, colour = "black", alpha = 0.5, linetype = "dashed") +
  scale_y_continuous(labels = comma, n.breaks = 10, name = "amount(yen)")
ggplotly(plot)

# Add each-year column -----

# Add column
df.shaped.year <- df.shaped %>%
  mutate(
    year = format(date, "%Y"),
    year_paidat = format(date_paidat, "%Y"),
    mon = format(date, "%m") %>% as.numeric(),
    mon_paidat = format(date_paidat, "%m") %>% as.numeric(),
    .after = date_paidat
  ) %>%
  group_by(year) %>%
  mutate(
    pl_year = order_by(date, cumsum(cal.amount.pl)),
    .after = pl_total
  ) %>%
  ungroup() %>%
  group_by(isPaid, year_paidat) %>%
  mutate(
    cash_year = order_by(date_paidat, cumsum(cal.amount.pl)),
    .after = cash_total
  ) %>%
  ungroup()


##
# Add yearmonth column -----

# Add column
df.shaped.ym <- df.shaped.year %>%
  mutate(
    yearmon = as.yearmon(date),
    yearmon_paidat = as.yearmon(date_paidat),
    day = format(date, "%d") %>% as.numeric(),
    day_paidat = format(date_paidat, "%d") %>% as.numeric(),
    .after = mon_paidat
  ) %>%
  group_by(yearmon) %>%
  mutate(
    pl_yearmon = order_by(date, cumsum(cal.amount.pl)),
    .after = pl_total
  ) %>%
  ungroup() %>%
  group_by(isPaid, yearmon_paidat) %>%
  mutate(
    cash_yearmon = order_by(date_paidat, cumsum(cal.amount.pl)),
    .after = cash_total
  ) %>%
  ungroup()
##


# See the data -----
df.agr.pl <- df.shaped.ym %>%
  group_by(year,mon, yearmon) %>%
  summarise(
    pl_mon = sum(cal.amount.pl)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    pl_year = cumsum(pl_mon),
    .before = pl_mon
  ) %>%
  ungroup() %>%
  mutate(
    pl_total = cumsum(pl_mon),
    .before = pl_year
  )
df.agr.cf <- df.shaped.ym %>%
  group_by(year_paidat,mon_paidat, yearmon_paidat) %>%
  summarise(
    cf_mon = sum(cal.amount.pl)
  ) %>%
  ungroup() %>%
  group_by(year_paidat) %>%
  mutate(
    cf_year = cumsum(cf_mon),
    .before = cf_mon
  ) %>%
  ungroup() %>%
  mutate(
    cf_total = cumsum(cf_mon),
    .before = cf_year
  )
df.agr <- merge.data.frame(df.agr.pl, df.agr.cf, by.x = c("year", "mon", "yearmon"), by.y = c("year_paidat", "mon_paidat", "yearmon_paidat"))
plot <-
  ggplot(df.agr) +
  geom_line(aes(mon, pl_year, colour = paste("pl", year), title = "color")) +
  geom_line(aes(mon, cf_year, colour = paste("cf", year))) +
  geom_hline(yintercept = 0, colour = "black", alpha = 0.5, linetype = "solid") +
  geom_hline(yintercept = 200000, colour = "black", alpha = 0.5, linetype = "dashed") +
  scale_y_continuous(labels = comma, n.breaks = 10, name = "amount_yen") +
  scale_x_continuous(labels = label_number(accuracy = 1), limits = c(1,12), name = "month")
ggplotly(plot)

### FINIAL PUSH ### ----

# Format the sheet ----
df.formatted <- df.shaped.ym %>%
  select(
    uid,
    session,
    details,
    way,
    category,
    target,
    purpose,
    type,
    name,
    date,
    date_paidat,
    year,
    year_paidat,
    yearmon,
    yearmon_paidat,
    mon,
    mon_paidat,
    day,
    day_paidat,
    amount,
    pl_total,
    pl_year,
    pl_yearmon,
    cash_total,
    cash_year,
    cash_yearmon,
    unpaid_each,
    substitution_paid,
    isPaid
  ) %>%
  .[order(.$date, .$session, .$date_paidat),]

# Write tables of each year ----
years <- unique(df.formatted$year)
for(year.this in years){

  # Extract and add column
  df.this <- df.formatted %>%
    filter(
      year == year.this
    ) %>%
    mutate(
      yearmon = as.Date(yearmon),
      yearmon_paidat = as.Date(yearmon_paidat)
    ) %>%
    write_sheet(conf$G_SSID_PERF, year.this)
}

# Update all time sheet -----
df.formatted.toSave <- df.formatted %>%
  mutate(
    yearmon = as.Date(yearmon),
    yearmon_paidat = as.Date(yearmon_paidat)
  )
allsht.old <- sheet_names(conf$G_SSID_PERF) %>%
  grep("all", ., ignore.case = TRUE, value = TRUE)
allsht.new <- paste("all_at", format(Sys.Date(), "%Y%m%d"), sep = "")
sheet_rename(conf$G_SSID_PERF, allsht.old, allsht.new)
write_sheet(df.formatted.toSave, conf$G_SSID_PERF, allsht.new)




### Open the sheet ### ----------
gs4_browse(conf$G_SSID_PERF)
