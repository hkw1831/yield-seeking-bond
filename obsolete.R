

output$impliedCoupon <- reactive({
  return (implied.coupon())
})

option.tendor <- reactive({
  S <- input$s
  r <- input$r
  profit <- input$profit
  coupon <- input$coupon
  q <- input$q
  sigma <- input$sigma
  profit <- input$profit
  price <- get.price.from.profit.percentage(S, profit)
  rf <- S
  if (!any(is.na(c(S, r, t, q, sigma, rf, price)))) {
    tendor <- bonus.enhancement.eln.tendor(S, sigma, r, q, rf, coupon, price)
    return (tendor)
  } else {
    return ("N/A")
  }
})

option.coupon <- reactive({
  S <- input$s
  r <- input$r
  t <- input$t
  q <- input$q
  sigma <- input$sigma
  profit <- input$profit
  price <- get.price.from.profit.percentage(S, profit)
  rf <- S
  if (!any(is.na(c(S, r, t, q, sigma, rf, price)))) {
    coupon <- bonus.enhancement.eln.coupon(S, sigma, r, t, q, rf, price)
    return (coupon)
  } else {
    return ("N/A")
  }
})


optionDelta <- reactive({
  S <- input$s
  K1 <- S
  K2 <- S * (1 + input$coupon)    
  r <- input$r
  t <- input$t
  q <- input$q
  sigma <- input$sigma
  rf <- S
  return (bonus.enhancement.eln.delta(S, K1, K2, sigma, r, t, q, rf))
})

optionProfitPercentage <- reactive({
  S <- input$s
  K1 <- S
  K2 <- S * (1 + input$coupon)
  r <- input$r
  t <- input$t
  q <- input$q
  sigma <- input$sigma
  rf <- S
  return (bonus.enhancement.eln.profit.percentage(S, K1, K2, sigma, r, t, q, rf))
})

optionValue <- reactive({
  S <- input$s
  K1 <- S
  K2 <- S * (1 + input$coupon)
  r <- input$r
  t <- input$t
  q <- input$q
  sigma <- input$sigma
  rf <- S
  return (bonus.enhancement.eln.price(S, K1, K2, sigma, r, t, q, rf))
})

# no longer used..
output$impliedCouponGraph <- renderPlot({
  S <- input$impliedS
  r <- input$impliedR
  t <- input$impliedT
  q <- input$impliedQ
  sigma <- input$impliedSigma
  input.price <- input$impliedPrice
  fee <- input$impliedFee
  rf <- S
  if (!any(is.na(c(S, r, t, q, sigma, rf, input.price)))) {
    maxValue <- input.price * 2
    length <- 50
    price <- seq(from = 0, to = maxValue, length = length)
    coupon <- rep(0, length)
    for (i in 1:length) {
      coupon[i] <- bonus.enhancement.eln.coupon(S, sigma, r, t, q, rf, price[i] * (1 - fee))
    }
    #plot(price, coupon, type = "l")
    qplot(price, coupon, geom = "path")
  }
})


output$payoffGraph <- renderPlot({
  info <- info.class()
  S <- info$s
  K1 <- info$K1
  K2 <- info$K2
  if (!any(is.na(c(S, K1, K2)))) {
    maxValue <- max(K1, K2, S) * 2
    StockPrice <- seq(from = 0, to = maxValue, length.out = 10^5)
    Payoff <- bonus.enhancement.eln.payoff(StockPrice, K1, K2)
    #plot(StockPrice, Payoff, type = "l")
    qplot(StockPrice, Payoff, geom = "path")
  }
})