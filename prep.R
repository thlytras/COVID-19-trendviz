
# Adjust this to the pathname where the JHU CSSE data can be found
# First clone their repository with:
# git clone https://github.com/CSSEGISandData/COVID-19.git
# and then update it regularly with: git pull
input_dir <- "../JHU CSSE/COVID-19/daily_case_updates"

# Load the data
dat.raw <- lapply(list.files(input_dir, ".csv", full.names=TRUE), 
    read.csv, stringsAsFactors = FALSE)
names(dat.raw) <- gsub(".csv", "", list.files(input_dir, ".csv"), fixed=TRUE)

# Process the data
dat.raw[["02-01-2020_1800"]] <- dat.raw[["02-01-2020_1800"]][,1:6]
for (n in names(dat.raw)) {
  dat.raw[[n]] <- dat.raw[[n]][, c("Province.State", "Country.Region", "Last.Update", "Confirmed", "Deaths", "Recovered")]
  dat.raw[[n]]$date <- as.Date(n, "%m-%d-%Y_%H%M")
  dat.raw[[n]]$datetime <- as.POSIXct(n, tz="UTC", format="%m-%d-%Y_%H%M")
}

# Store the datetimes that are at the end of the day
eod <- rev(rev(as.POSIXct(names(dat.raw), tz="UTC", format="%m-%d-%Y_%H%M"))[!duplicated(rev(as.Date(names(dat.raw), format="%m-%d-%Y_%H%M")))])

# Bind all datasets together
dat <- do.call(rbind, dat.raw)

# Process the last update datetime
dat$LUpd <- as.POSIXct(dat$Last.Update, 
    tz="UTC", format="%Y-%m-%d %H:%M:%S")
dat$LUpd[is.na(dat$LUpd)] <- as.POSIXct(dat$Last.Update[is.na(dat$LUpd)], 
    tz="UTC", format="%m/%d/%y %H:%M")
dat$LUpd[is.na(dat$LUpd)] <- as.POSIXct(dat$Last.Update[is.na(dat$LUpd)], 
    tz="UTC", format="%m/%d/%Y %H:%M")
l <- Sys.getlocale(category="LC_TIME")
Sys.setlocale(category="LC_TIME", "C")
dat$LUpd[is.na(dat$LUpd)] <- as.POSIXct(dat$Last.Update[is.na(dat$LUpd)], 
    tz="UTC", format="%m/%d/%Y %I%p")
Sys.setlocale(category="LC_TIME", l)
dat$Last.Update <- NULL

# Give a name to unknown provinces, so that we can search for them
dat$Province.State[which(dat$Province.State=="")] <- "unknown"

# Give names that are easier to handle
names(dat) <- c("prov", "cnt", "conf", "dead", "recov", "date", "datetime", "LUpd")

# MANUALLY correct the initial cases from the Diamond Princess cruise ship
# (the first 20 of which were "assigned" to Japan)
dat$conf[with(dat, cnt=="Japan" & date=="2020-2-6")] <- 25

# Select End-of-day estimates
datEOD <- subset(dat, datetime %in% eod)

save(dat, file="dat.RData")

# Remove observations with unchanged number of cases
#dat <- dat[!duplicated(dat[,c("Province.State", "Country.Region", "Confirmed", "Deaths", "Recovered")]),]



