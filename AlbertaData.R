########################################################################
# Gather the data for the Alberta Economic Conditions Index (ECI)
#
# Everything comes from Statistics Canada in ONE batched request to the
# Web Data Service (WDS), asking only for the vectors we use. No full-table
# downloads, no {cansim} dependency, and no non-StatCan APIs.
#
# Creates:
#   monthly     one row per month, one column per indicator (real, SA)
#   gdp_annual  Alberta real GDP at basic prices (36-10-0711), by year
#   releases    last available month for each indicator
########################################################################

# StatCan vectors used directly (table in comments)
vectors <- c(
  mfg            = "807466",     # 16-10-0048 Manufacturing sales, SA
  trade          = "52367703",   # 20-10-0074 Wholesale trade, SA
  retail_old     = "52367215",   # 20-10-0008 Retail sales, SA (to 2022)
  retail_new     = "1446859973", # 20-10-0056 Retail sales, SA (2017+)
  food           = "42170714",   # 21-10-0019 Food services and drinking places receipts, SA
  housing        = "52299962",   # 34-10-0156 Housing starts
  vehicles       = "1617815652", # 20-10-0085 New motor vehicle sales, units, NSA
  exports        = "1567090443", # 12-10-0175 Domestic merchandise exports, all products
  exports_energy = "1567090501", # 12-10-0175 Domestic merchandise exports, energy products
  employment     = "2064512",    # 14-10-0287 LFS employment, SA
  seph           = "79311016",   # 14-10-0223 SEPH payroll employment, SA
  earnings       = "79311387",   # 14-10-0223 SEPH average weekly earnings, SA
  oil_old        = "18050",      # 25-10-0014 Crude oil production (1985 to Feb 2016)
  oil_new        = "107757710",  # 25-10-0063 Crude oil production, m3 (2016+)
  cpi            = "41692327",   # 18-10-0004 Alberta CPI, all-items
  gdp            = "1793287969", # 36-10-0711 Alberta real GDP at basic prices (annual)
  population     = "15"          # 17-10-0009 Alberta population (quarterly)
)
# National monthly GDP by industry (36-10-0434) and Alberta annual GDP by industry (36-10-0711)
industry_vectors <- read.csv("Data/industry_vectors.csv", colClasses = "character")

# One request for everything (about 60 series; takes a few seconds)
all_ids <- unique(c(vectors, industry_vectors$vector))
url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorByReferencePeriodRange?vectorIds=",
              URLencode(paste0('"', all_ids, '"', collapse = ","), reserved = TRUE),
              "&startRefPeriod=1999-01-01&endReferencePeriod=2100-01-01")
response <- fromJSON(url, simplifyVector = FALSE)
raw <- bind_rows(lapply(response, function(x) tibble(
  vector = as.character(x$object$vectorId),
  When   = as.Date(sapply(x$object$vectorDataPoint, `[[`, "refPer")),
  value  = sapply(x$object$vectorDataPoint, function(p) if (is.null(p$value)) NA_real_ else p$value))))
stopifnot(all(all_ids %in% raw$vector))  # fail loudly if StatCan drops or renames a vector
raw <- raw %>% filter(!is.na(value))
get_series <- function(nm) raw %>% filter(vector == vectors[[nm]]) %>% select(When, value) %>% arrange(When)

#######################
# Build each indicator #
#######################

# Retail sales: splice the old series onto the new one at the first overlapping month
retail_old <- get_series("retail_old")
retail_new <- get_series("retail_new")
splice <- retail_new$value[1] / retail_old$value[retail_old$When == retail_new$When[1]]
retail <- bind_rows(retail_old %>% filter(When < min(retail_new$When)) %>% mutate(value = value * splice),
                    retail_new) %>%
  rename(retail = value)

# Crude oil production: splice the 1985-2016 table onto the current one (overlap Jan-Feb 2016)
oil_old <- get_series("oil_old")
oil_new <- get_series("oil_new")
overlap <- oil_new$When[oil_new$When %in% oil_old$When]
splice <- mean(oil_new$value[oil_new$When %in% overlap]) / mean(oil_old$value[oil_old$When %in% overlap])
oil <- bind_rows(oil_old %>% filter(When < min(oil_new$When)) %>% mutate(value = value * splice),
                 oil_new) %>%
  rename(oil = value)

# New vehicle sales are not seasonally adjusted; use X-13 via {seasonal}
vehicles <- get_series("vehicles")
vehicles$vehicles <- as.numeric(final(seas(ts(vehicles$value, frequency = 12,
                                              start = c(year(vehicles$When[1]), month(vehicles$When[1]))))))
vehicles <- vehicles %>% select(When, vehicles)

# Shift-share indicator: national monthly real GDP by industry, weighted by Alberta's own
# industry mix (previous year's shares), with oil & gas extraction replaced by Alberta crude production.
ab_industry <- industry_vectors %>% filter(set == "alberta_annual") %>%
  left_join(raw, by = "vector") %>% mutate(year = year(When)) %>% select(sector, year, value)
mining_total <- ab_industry %>% filter(sector == "Mining, quarrying, and oil and gas extraction")
oil_gas_support <- ab_industry %>%
  filter(sector %in% c("Oil and gas extraction", "Support activities for mining, and oil and gas extraction")) %>%
  group_by(year) %>% summarise(og = sum(value))
other_mining <- mining_total %>% left_join(oil_gas_support, by = "year") %>%
  transmute(sector = "Other mining", year, value = value - og)
shares <- ab_industry %>%
  filter(!sector %in% c("All industries", "Mining, quarrying, and oil and gas extraction")) %>%
  bind_rows(other_mining) %>%
  group_by(year) %>% mutate(share = value / sum(value)) %>% ungroup() %>%
  transmute(sector, year = year + 1, share)          # weights for year t come from year t-1
oil_sa <- oil %>% filter(When >= as.Date("2000-01-01"))
oil_sa$value <- as.numeric(final(seas(ts(oil_sa$oil, frequency = 12, start = c(2000, 1)))))
drivers <- industry_vectors %>% filter(set == "canada_monthly", sector != "All industries") %>%
  left_join(raw, by = "vector") %>%
  mutate(sector = ifelse(sector == "Mining, quarrying, and oil and gas extraction", "Other mining", sector)) %>%
  filter(sector != "Oil and gas extraction") %>%
  bind_rows(oil_sa %>% transmute(sector = "Oil and gas extraction", When, value)) %>%
  select(sector, When, value) %>% arrange(sector, When) %>%
  group_by(sector) %>% mutate(growth = log(value) - log(lag(value))) %>% ungroup()
latest_share_year <- max(shares$year)
shiftshare <- drivers %>%
  mutate(year = pmin(year(When), latest_share_year)) %>%   # carry the latest known shares forward
  inner_join(shares, by = c("sector", "year")) %>%
  filter(!is.na(growth)) %>%
  group_by(When) %>%
  filter(n() >= 20) %>%                                    # require nearly all industries for that month
  summarise(growth = sum(share * growth) / sum(share)) %>%
  filter(When >= as.Date("2000-02-01")) %>%
  mutate(shiftshare = exp(cumsum(growth))) %>%
  select(When, shiftshare)

# Combine into one monthly data frame and convert nominal series to real with Alberta CPI
monthly <- get_series("mfg") %>% rename(mfg = value) %>%
  full_join(get_series("trade") %>% rename(trade = value), by = "When") %>%
  full_join(retail, by = "When") %>%
  full_join(get_series("food") %>% rename(food = value), by = "When") %>%
  full_join(get_series("housing") %>% rename(housing = value), by = "When") %>%
  full_join(vehicles, by = "When") %>%
  full_join(get_series("exports") %>% rename(exports = value), by = "When") %>%
  full_join(get_series("exports_energy") %>% rename(exports_energy = value), by = "When") %>%
  full_join(get_series("employment") %>% rename(employment = value), by = "When") %>%
  full_join(get_series("seph") %>% rename(seph = value), by = "When") %>%
  full_join(get_series("earnings") %>% rename(earnings = value), by = "When") %>%
  full_join(oil, by = "When") %>%
  full_join(shiftshare, by = "When") %>%
  full_join(get_series("cpi") %>% rename(cpi = value), by = "When") %>%
  arrange(When) %>%
  mutate(exports_nonenergy = exports - exports_energy,   # energy exports mostly track prices, not volumes
         payroll = seph * earnings,                      # total weekly payroll (avoids composition effects in avg earnings)
         across(c(mfg, trade, retail, food, exports_nonenergy, payroll), ~ .x / cpi)) %>%
  select(-exports, -exports_energy, -earnings, -cpi) %>%
  filter(When >= as.Date("2000-01-01"))

# Annual real GDP (current table; 36-10-0402 was discontinued after the 2024 reference year)
gdp_annual <- get_series("gdp") %>% transmute(year = year(When), gdp = value)

# Population: quarterly estimates (1st day of each quarter), interpolated to months, then
# year-over-year growth. Months after the latest estimate carry its growth rate forward.
population <- get_series("population") %>% rename(pop = value)
pop_growth <- tibble(When = seq(min(population$When), max(monthly$When), by = "month")) %>%
  left_join(population, by = "When") %>%
  mutate(pop = na.approx(pop, na.rm = FALSE),
         pop_growth = log(pop) - log(lag(pop, 12)),
         pop_growth = na.locf(pop_growth, na.rm = FALSE)) %>%
  select(When, pop_growth)

# What is the latest month available for each indicator?
releases <- monthly %>% pivot_longer(-When) %>% filter(!is.na(value)) %>%
  group_by(name) %>% summarise(last_month = as.yearmon(max(When))) %>% arrange(last_month)
print(releases, n = 20)
