get.stock.dividend.yield <- function(ticker) {
  quote <- tryCatch(getQuote(ticker, what=yahooQF("Dividend Yield")), error = function(e) NA)
  if (any(is.na(quote))){
    return (NA)
  }
  dividend.yield <- quote[ticker,"Dividend Yield"]
  if(toString(dividend.yield) == "N/A"){
    return (0)
  }
  return (dividend.yield/100)
}

get.stock.annual.sd <- function(ticker, num.of.year) {
  if (is.na(num.of.year)) {
    return (NA)
  }
  if (num.of.year == 0) {
    return (NA)
  }
  current.date <- as.POSIXlt(Sys.Date())
  current.date$year <- current.date$year - num.of.year
  from.date <- as.Date(current.date)
  annual.sd <- NA
  annual.sd <- tryCatch(getSymbols(ticker, from = from.date), error = function(e) NA)
  if (any(is.na(annual.sd))) {
    return (NA)
  }
  historic.data <- na.omit(get(annual.sd)[,paste(ticker, "Adjusted", sep = ".")])
  historic.data <- na.omit(diff(historic.data)/lag(historic.data))
  return (sd(historic.data) * sqrt(252))
}

get.ticker.last.price <- function(ticker) {
  last.price <- tryCatch(getQuote(ticker, src = "yahoo"), error = function(e) NA)
  if (any(is.na(last.price))) {
    return (NA)
  }
  return (last.price[ticker, "Last"])
}

euro.call.price <- function(S, K, sigma, r, t, q) {
  d1 <- (log(S/K) + (r - q + 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  d2 <- (log(S/K) + (r - q - 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  return (exp(-q * t) * S * pnorm(d1) - exp(-r * t) * K * pnorm(d2))
}

cash.or.nothing.price <- function(S, K, sigma, r, t, q) {
  d2 <- (log(S/K) + (r - q - 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  return (exp(-r * t) * pnorm(d2))
}

euro.put.price <- function(S, K, sigma, r, t, q) {
  d1 <- (log(S/K) + (r - q + 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  d2 <- (log(S/K) + (r - q - 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  return (- exp(-q * t) * S * pnorm(-d1) + exp(-r * t) * K * pnorm(-d2))
}

risk.free.asset.price <- function(r, t, rf) {
  return (exp(-r * t) * rf)
}

yield.seeking.eln.delta <- function(S, K1, K2, sigma, r, t, q, rf) {
  d1.k1 <- (log(S/K1) + (r - q + 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  d1.k2 <- (log(S/K2) + (r - q + 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  d2.k1 <- (log(S/K1) + (r - q - 0.5 * sigma * sigma)) / (sigma * sqrt(t))
  delta.put <- - exp(-q * t) * pnorm(-d1.k1)
  delta.call <- exp(-q * t) * pnorm(d1.k2)
  delta.cash.or.nothing <- exp(-r * t) * dnorm(d2.k1) / (S * sigma * sqrt(t))
  delta <- -delta.put + (K2 - K1) * delta.cash.or.nothing + delta.call
  return (delta)
}

yield.seeking.eln.profit.percentage <- function(S, K1, K2, sigma, r, t, q, rf) {        
  price <- yield.seeking.eln.price(S, K1, K2, sigma, r, t, q, rf)
  return (1 - (price / S))  
}

yield.seeking.eln.price <- function(S, K1, K2, sigma, r, t, q, rf) {        
  call.k2 <- euro.call.price(S, K2, sigma, r, t, q)
  cash.or.nothing <- (K2 - K1) * cash.or.nothing.price(S, K1, sigma, r, t, q) 
  put.k1 <- euro.put.price(S, K1, sigma, r, t, q) 
  risk.free.asset <- risk.free.asset.price(r, t, rf) 
  return (call.k2 + cash.or.nothing - put.k1 + risk.free.asset)
}

yield.seeking.eln.payoff <- function(S, K1, K2) {
  risk.free.asset <- K1
  ifelse(S < K1, S - K1, ifelse(S > K2, S - K1, K2 - K1)) + risk.free.asset
}

yield.seeking.eln.payoff.par <- function(S, K1, K2) {
  yield.seeking.eln.payoff(S, K1, K2) / S
}

yield.seeking.eln.square.diff <- function(S, coupon, sigma, r, t, q, rf, price) (price - yield.seeking.eln.price(S, S, S * (1 + coupon), sigma, r, t, q, rf)) ^ 2

yield.seeking.eln.coupon <- function(S, sigma, r, t, q, rf, price) {
  coupon <- optimize(yield.seeking.eln.square.diff, interval = c(0, 100), maximum = FALSE, S = S, sigma = sigma, r = r, t = t, q = q, rf = rf, price = price)$minimum
  recal.price <- yield.seeking.eln.price(S, S, S * (1+coupon), sigma, r, t, q, rf)
  if (!isTRUE(all.equal(recal.price, price, tolerance = 0.01))) {
    return (NA)
  } else {
    return (coupon)
  }
}

yield.seeking.eln.tenor <- function(S, sigma, r, q, rf, coupon, price) {
  tenor <- optimize(yield.seeking.eln.square.diff, interval = c(0, 100), maximum = FALSE, S = S, coupon = coupon, sigma = sigma, r = r, q = q, rf = rf, price = price)$minimum
  recal.price <- yield.seeking.eln.price(S, S, S * (1+coupon), sigma, r, tenor, q, rf)
  if (!isTRUE(all.equal(recal.price, price, tolerance = 0.01))) {
    return (NA) 
  } else {
    return (tenor)
  }
}

get.price.from.profit.percentage <- function(stock.price, profit.percentage) {
  return (stock.price * (1 - profit.percentage))
}

yield.seeking.eln.payoff.par <- function(S, K1, K2) {
  yield.seeking.eln.payoff(S, K1, K2) / K1
}
