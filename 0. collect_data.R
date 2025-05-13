# install.packages('eurostat')
# install.packages('ecb')
# install.packages('fredr')
library(tidyverse)
library(seasonal)
library(eurostat)
library(fredr)
library(ecb)
fredr_set_key("6891362fdb8005836cf86f8b3144b6fb")
select <- dplyr::select
options(scipen=99999)
# fill in country codes below
CODES <- c('DE', 'FR', 'AT')
# fill in sample dates below
SAMPLE_START <- as.Date('2000-01-01')
SAMPLE_END <- as.Date('2023-10-01')
# INDUSTRIAL PRODUCTION ----
dat <- get_eurostat('sts_inpr_m', time_format = "date", stringsAsFactors = TRUE,
                    filters = list(NACE_R2 = 'B-D', # all categories
                                   S_ADJ = 'SCA',   # seasonally and calendar adjusted
                                   UNIT = 'I21'     # index (2021=100) 
                                   ))
IP <- dat %>%
  filter(geo %in% CODES) %>%
  filter(time >= SAMPLE_START & time <= SAMPLE_END) %>%
  select(geo, date = time, y = values)
# Plot
IP %>%
  ggplot() +
  geom_line(aes(x=date, y=y, group = geo))
# PRICE INDICES ----
dat <- get_eurostat('prc_hicp_midx', time_format = "date", stringsAsFactors = TRUE,
                    filters = list(COICOP = 'CP00', # all items
                                   UNIT = 'I15'     # index (2015=100) 
                    ))
CPI <- dat %>%
  filter(geo %in% CODES) %>%
  filter(time >= SAMPLE_START & time <= SAMPLE_END) %>%
  select(geo, date = time, value_nsa = values)
# Plot
CPI %>%
  ggplot() +
  geom_line(aes(x=date, y=value_nsa, group = geo))
# Seasonal adjustment
CPI$P <- NA
for (i in unique(CPI$geo)) {
  df <- CPI %>%
    filter(geo == i)
  startdate <- df$date[1]
  series <- ts(df$value_nsa, start = c(year(startdate), month(startdate)), freq = 12)
  sa <- seas(series)
  CPI[CPI$geo == i,]$P <- as.numeric(sa %>% final())
}
# Plot
CPI %>%
  ggplot() +
  geom_line(aes(x=date, y=P, group = geo))
CPI <- CPI %>%
  select(-value_nsa)
# INTEREST RATES ----
dat <- get_eurostat('irt_h_euryld_m', time_format = "date", stringsAsFactors = TRUE,
                    filters = list(GEO = 'EA',          # Euro area
                                   MATURITY = 'Y1',     # 1-year
                                   YLD_CURV = "SPOT_RT" # spot rate
                    ))
dat2 <- get_eurostat('irt_euryld_m', time_format = "date", stringsAsFactors = TRUE,
                    filters = list(GEO = 'EA',           # Euro area
                                   MATURITY = 'Y1',      # 1-year
                                   YLD_CURV = "SPOT_RT", # spot rate
                                   BONDS = "CGB_EA_AAA"   # AAA government bonds
                    ))
IR <- dat %>%
  select(date = time, i = values) %>%
  filter(date <= as.Date('2004-08-01')) %>%
  rbind(dat2 %>%
          select(date = time, i = values)
        )
# OIL PRICES ----
OIL <- fredr(
  series_id = "MCOILBRENTEU",
  observation_start = SAMPLE_START,
  observation_end = SAMPLE_END
) %>%
  select(date, oil = value)
# ECB SHOCKS ----
# see https://github.com/marekjarocinski/jkshocks_update_ecb_202310
# make sure to read and reference properly
dat <- read.csv('https://github.com/marekjarocinski/jkshocks_update_ecb_202310/raw/refs/heads/main/shocks/shocks_ecb_mpd_me_m.csv')
SHOCKS <- dat %>%
  mutate(date = seq.Date(as.Date('1999-01-01'), as.Date('2023-10-01'), by = 'month')) %>%
  select(date, shock = MP_median)
# CORPORATE CREDIT RATES ----
IR_DE <- get_data('MIR.M.DE.B.A2A.A.R.A.2240.EUR.N') %>%
  mutate(obstime = paste0(obstime,'-01'),
    date = as.Date(obstime)) %>%
  select(date, geo = ref_area, i_c = obsvalue)
# ASSEMBLE DATA ----
full <- IP %>%
  left_join(CPI, by = c('geo', 'date')) %>%
  left_join(OIL, by = 'date') %>%
  left_join(IR, by = 'date') %>%
  left_join(SHOCKS, by = 'date') %>%
  left_join(IR_DE, by = c('geo', 'date')) %>%
  mutate(y = log(y)*100,
         P = log(P)*100,
         oil = log(oil)*100) %>%
  mutate(covid = date %in% as.Date(c('2020-03-01', '2020-04-01', '2020-05-01'))
         )
write.csv(full, 'data.csv', row.names = FALSE)
