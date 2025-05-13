#devtools::install_github("martinbaumgaertner/varexternal")
library(tidyverse)
library(varexternal)
select <- dplyr::select
options(scipen=99999)
full <- read.csv('data.csv')
cntrs = c('AT','BE','DE','IE','IT','ES','NL','PT','FR','FI')
final <- data.frame(country = cntrs, i_c = c(0), P = c(0), y = c(0))

mod = 'DE'
DE <- full %>% filter(geo == mod) %>% drop_na()

VARS <- c('i', 'i_c', 'y', 'P') #(в тг подробности)
# https://www.sciencedirect.com/science/article/pii/S0304407620302311?via%3Dihub
# we have a "shock" variable which can be used as an instrumental variable in SVAR estimation
# some description of using instrumental variables can be found in the "Macroshocks seminar.pdf"
# basic idea: instrumental variable is correlated with the monetary policy shock (relevance)
# but uncorrelated with other shocks (exogeneity)

# including oil prices as an endogenous variable is questionable but this package does not 
# seem to support exogenous variables. They could be omitted in the baseline specification 

m <- SVARIV(y = DE[,VARS] %>% as.matrix(),    #Endogenous variables
            z = DE$shock,       # Instrument for the shock
            p = 12,             # Number of lags in the VAR model
            confidence = 0.75,  # Confidence level for CIs
            NWlags = 0,         # Newey-West lags (if it is necessary to account for time series autocorrelation)
            norm = 1,           # Variable used for normalization (interest rate - first one)
            scale = 0.25,       # Scale of the shock (0.25 increase in interest rate)
            horizons = 48,      # Number of horizons for the Impulse Response Functions (IRFs)
            instrument_name = "shock")

ii <- m$irfs
ii
final[final$country == mod, 'i_c'] = max(filter(ii, variable == 'i_c')[1:10,'value'])
final[final$country == mod, 'P'] = min(filter(ii, variable == 'P')[1:30,'value'])
final[final$country == mod, 'y'] = min(filter(ii, variable == 'y')[1:20,'value'])
ii %>% filter(variable == 'y')
ii %>%
  filter(confi_type == 'plugin') %>%
  pivot_wider(names_from = 'type', values_from = 'value') %>%
  mutate(variable = as.character(variable)) %>%
  ggplot() +
  geom_line(aes(x=horizon, y=point)) +
  geom_ribbon(aes(x=horizon, ymin=lower, ymax=upper), alpha = 0.5) +
  facet_wrap(~variable, scales='free_y')

write_csv(final, 'final.csv')

sh.col<-      c("#90CEFA")
names(sh.col)<-c("shock")
pretty_irf(data=list(m$irfs),shock_names="shock",pretty_names=c('Краткосрочные прцентные ставки', 'Рыночные ставки по кредитам', 'Индекс промышленного производства', 'Гармонизированный индекс потребительских цен'),manual_color=sh.col, title = NULL)
?pretty_irf
