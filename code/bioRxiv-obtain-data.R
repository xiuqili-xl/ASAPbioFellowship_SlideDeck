################################################################################

# Obtain bioRxiv data

################################################################################

# Load libraries ----
library(tidyverse)
library(lubridate)
library(rbiorxiv)


# Obtain monthly new preprint statistics from bioRxiv ----
biorxiv_newpaper <- biorxiv_summary(interval = "m", format = "df") %>%
  select(year.month = month, new_papers) %>%
  mutate(year = as.character(year(ym(year.month))), .before = year.month)

saveRDS(biorxiv_newpaper, file = "data/bioRxiv-new-paper-stats.rds")



# Obtain montly preprint posting by Harvard researchers ----
## define start dates and end dates (monthly increment)
startdates <- seq(ymd("2013-11-01"), ymd("2024-05-01"), by = "months")
enddates <- startdates %m+% months(1)
no_month <- length(startdates)

## run for loop to retrieve bioRxiv preprints in monthly increments
## and look for Harvard in the author_corresponding_institution variable
## return number of preprints by from Harvard researchers
## IMPORTANT: do not run the loop lightly. will take a long time...
no_preprint <- vector(mode = "numeric", length = no_month)

for (x in 1:no_month) {
  filtered <- biorxiv_content(from = startdates[x], to = enddates[x], 
                              limit = 10000, format = "df") %>%
    filter(str_detect(author_corresponding_institution, "Harvard"))
  
  no_preprint[x] <- nrow(filtered)
}

## create summary dataframe
biorxiv_newpaper_H <- tibble(start.date = startdates, end.date = enddates, 
                             h.preprint = no_preprint) %>%
  mutate(year.month = str_sub(as.character(start.date), start = 1, end = -4),
         year = as.character(year(start.date))) %>%
  select(year, year.month, start.date, end.date, h.preprint)

saveRDS(biorxiv_newpaper_H, file = "data/bioRxiv-new-paper-Harvard.rds")



# Obtain bioRxiv data on published preprints ----
## define start dates and end dates (yearly increment)
firstday.year <- seq(ymd("2013-01-01"), ymd("2024-01-01"), by = "years")
lastday.year <- seq(ymd("2013-12-31"), ymd("2024-12-31"), by = "years")
no_year <- length(firstday.year)

## run for loop to obtain info on preprint that gets published
## IMPORTANT, do not run the loop lighty, will take a long time...
for (i in 1:no_year) {
  published <- biorxiv_published(from = firstday.year[i], to = lastday.year[i], 
                                 limit = "*", format = "df") %>%
    select(biorxiv_doi, published_doi, preprint_date, published_date)
  
  assign(paste0("published_", i+2012), published)
}

biorxiv_published_raw <- bind_rows(published_2013, published_2014, published_2015, 
                                   published_2016, published_2017, published_2018, 
                                   published_2019, published_2020, published_2021, 
                                   published_2022, published_2023, published_2024)

# fix a few entries with incorrect date formatting
biorxiv_published_cleaned <- biorxiv_published_raw %>%
  separate(published_date, into = c("published_date", NA), sep = ":") %>%     
  mutate(month = str_sub(preprint_date, start = 1, end = -4),
         year = str_sub(month, start = 1, end = 4),
         preprint_date = ymd(preprint_date),
         published_date = ymd(published_date)) %>%
  select(year, year.month = month, biorxiv_doi, published_doi, preprint_date, published_date)

glimpse(biorxiv_published_cleaned)
saveRDS(biorxiv_published_cleaned, "data/bioRxiv-published.rds")


