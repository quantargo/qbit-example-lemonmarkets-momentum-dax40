library(tibble)
library(quantmod)
library(PerformanceAnalytics)
library(lemonmarkets)
library(tidyr)
library(dplyr)

dax40_components <- tibble::tribble(
    ~Symbol,                                 ~Company.Name, ~ISIN,
   "DPW.DE",                            "Deutsche Post AG", "DE0005552004",
   "DWNI.DE",                          "Deutsche Wohnen SE", "DE000A0HN5C6",
   "IFX.DE",                    "Infineon Technologies AG", "DE0006231004",
   "ENR.DE",                           "Siemens Energy AG", "DE000ENER6Y0",
   "DTE.DE",                         "Deutsche Telekom AG", "DE0005557508",
   "LIN.DE",                                   "Linde plc", "IE00BZ12WP82",
   "HEI.DE",                         "HeidelbergCement AG", "DE0006047004",
   "SIE.DE",                  "Siemens Aktiengesellschaft", "DE0007236101",
   "ALV.DE",                                  "Allianz SE", "DE0008404005",
   "BMW.DE", "Bayerische Motoren Werke Aktiengesellschaft", "DE0005190003",
   "RWE.DE",                      "RWE Aktiengesellschaft", "DE0007037129", 
   "DB1.DE",                          "Deutsche Boerse AG", "DE0005810055",
   "SHL.DE",                     "Siemens Healthineers AG", "DE000SHL1006",
   "FME.DE",        "Fresenius Medical Care AG & Co. KGaA", "DE0005785802",
   "MRK.DE",      "MERCK Kommanditgesellschaft auf Aktien", "DE0006599905",
   "FRE.DE",                     "Fresenius SE & Co. KGaA", "DE0005785604",
  "BAYN.DE",                    "Bayer Aktiengesellschaft", "DE000BAY0017",
   "ZAL.DE",                                  "Zalando SE", "DE000ZAL1111",
   "PUM.DE",                                     "PUMA SE", "DE0006969603",
   "BAS.DE",                                     "BASF SE", "DE000BASF111",
   "AIR.DE",                                   "Airbus SE", "NL0000235190",
   "SY1.DE",                                  "Symrise AG", "DE000SYM9999",
   "CON.DE",              "Continental Aktiengesellschaft", "DE0005439004",
   "MTX.DE",                         "MTU Aero Engines AG", "DE000A0D9PT0",
  "DHER.DE",                            "Delivery Hero SE", "DE000A2E4K43",
   "ADS.DE",                                   "adidas AG", "DE000A1EWWW0",
   "HFG.DE",                               "HelloFresh SE", "DE000A161408",
  "VOW3.DE",                               "Volkswagen AG", "DE0007664039", # or "DE0007664005"?
   "DBK.DE",            "Deutsche Bank Aktiengesellschaft", "DE0005140008",
  "1COV.DE",                                 "Covestro AG", "DE0006062144"
  )

from <- "2007-01-02"
sym <- lapply(dax40_components$Symbol, getSymbols, auto.assign = FALSE)
out <- do.call(c, lapply(sym, function(X) min(index(X))))
sym_sel <- sym[out == from]

sym_ad_close <- do.call(cbind, lapply(sym_sel, Ad))
colnames(sym_ad_close) <- gsub(".Adjusted$", "", colnames(sym_ad_close))

ret1 <- ROC(sym_ad_close, 1, type = "discrete")
R1M <- ROC(sym_ad_close, 25, type = "discrete")

rank <- do.call(rbind, apply(R1M, 1, rank, na.last = NA, ties.method = "first"))
rank <- xts(rank, order.by = as.Date(rownames(rank)))

ntop <- 10
sel_top <- rank <= ntop
sel_top_lag <- lag.xts(sel_top, 1)
  
port_weights <- xts(matrix(0, 
                           nrow=nrow(sel_top_lag), 
                           ncol = ncol(sel_top_lag),
                           dimnames = list(NULL, colnames(sel_top_lag))), order.by = index(sel_top_lag))
port_weights[sel_top_lag] <- 1/ntop

perf <- xts(rowSums(port_weights * ret1[index(sel_top_lag)]), order.by = index(sel_top_lag))
dax <- getSymbols("^GDAXI", auto.assign = FALSE)
dax_ret <- ROC(Ad(dax), 1, type = "discrete")
comp <- cbind(perf, dax_ret)
charts.PerformanceSummary(comp, geometric = FALSE)

# 1. Authenticate at lemonmarkets API
auth()

# Set space_id, can also be retrieved through list_spaces()
space_id <- "e44907b7-d131-4ec9-9647-a2649480003d"

# 1. Get total space value (portfolio+cash)
bal <- balance(space_id) # Get current cash balance
portfolio_current <- portfolio(space_id)
total_value <- sum(portfolio_current$latest_total_value) + bal

# 2. Get target exposure
trade_target <- fortify.zoo(tail(port_weights, 1)) %>%
  pivot_longer(-Index, values_to = "weight") %>%
  mutate(exposure = weight * total_value) %>%
  left_join(dax40_components, by = c(name = "Symbol")) %>%
  rowwise() %>%
  mutate(quote = quotes(ISIN)) %>%
  mutate(position = floor(exposure / quote$a)) %>%
  full_join(portfolio_current, by = c(ISIN = "instrument.isin")) %>%
  mutate(position = replace_na(position, 0),
         quantity = replace_na(quantity, 0),
         delta = position - quantity) %>%
  filter(delta != 0)
 
if (nrow(trade_target) < 1) {
   cat("No trades to be executed\n")
   return()
}

cat("The following trades are being executed: \n")
trade_target %>% select(Company.Name, ISIN, delta)

if (trade_target %>% filter(delta < 0) %>% nrow() > 0) {
   trade_orders_sell <- trade_target %>%
      filter(delta < 0) %>%
      mutate(orders = create_order(space_id, ISIN, delta))
   
   cat("The following sell trades have been executed: \n")
   trade_orders_sell %>% select(Company.Name, ISIN, delta)
   time_sleep <- 10
   cat(sprintf("Sleeping %d sec...\n", time_sleep))
   Sys.sleep(time_sleep)
}

if (trade_target %>% filter(delta > 0) %>% nrow() > 0) {
   trade_orders_buy <- trade_target %>%
      filter(delta > 0) %>%
      mutate(orders = create_order(space_id, ISIN, delta)) %>%
      unnest(orders)
   
   cat("The following buy trades have been executed: \n")
   trade_orders_buy %>% select(Company.Name, ISIN, delta)
}

