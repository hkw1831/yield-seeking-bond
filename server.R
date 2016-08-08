library(quantmod)
library(ggplot2)
library(shinyjs)

source("pricingUtil.R")
source("displayUtil.R")
source("validationUtil.R")

shinyServer(
  function(input, output, session) {
    
    observe({
      if (input$type == "Symbol") {
        stock.price <- get.ticker.last.price(input$ticker)
        updateNumericInput(session, "s", value = stock.price)
        
        stock.q <- get.stock.dividend.yield(input$ticker)
        updateNumericInput(session, "q", value = stock.q)
        
        stock.sigma <- get.stock.annual.sd(input$ticker, input$t)
        updateNumericInput(session, "sigma", value = stock.sigma)
      }
      
      if (input$impliedType == "Symbol") {
        implied.stock.price <- get.ticker.last.price(input$impliedTicker)
        updateNumericInput(session, "impliedS", value = implied.stock.price)
        
        implied.product.price <- get.ticker.last.price(input$impliedTicker)
        updateNumericInput(session, "impliedPrice", value = implied.product.price)
        
        implied.stock.q <- get.stock.dividend.yield(input$impliedTicker)
        updateNumericInput(session, "impliedQ", value = implied.stock.q)
        
        implied.stock.sigma <- get.stock.annual.sd(input$impliedTicker, input$impliedT)
        updateNumericInput(session, "impliedSigma", value = implied.stock.sigma) 
      }

    })
    
    ############## Banker Page here
    
    output$priceS <- renderPlot({
      info <- info.class()
      if (!any(is.na(c(info$s, info$K1, info$K2, info$r, info$t, info$q, info$sigma, info$rf)))) {
        maxValue <- info$K1 * 2
        length <- 1000
        S <- seq(from = 0, to = maxValue, length = length)
        price <- rep(0, length)
        pnl <- rep(0, length)
        for (i in 1:length) {
          price[i] <- yield.seeking.eln.price(S[i], info$K1, info$K2, info$sigma, info$r, info$t, info$q, info$rf)
          pnl[i] <- ifelse(price[i] < info$K1, "#0099CC" , "#999999")
        }
        DF <- data.frame(S, price, pnl)
        x.pnl <- which(DF$pnl == "#999999")
        if (length(x.pnl) > 1){
          x.pnl.max <- max(x.pnl)
          x.pnl.min <- min(x.pnl)     
        } else {
          x.pnl.max <- x.pnl
          x.pnl.min <- x.pnl
        }
        
        ggplot(DF, aes(x = S / info$K1 - 1, y = price / info$K1 * 100), fill = pnl) +
          geom_line(aes(color = pnl, group = 1), size = 2) +
          scale_color_identity(guide = "legend", name = "", 
                               breaks = c("#0099CC" , "#999999"), 
                               labels = c("profit", "loss")) +
          scale_y_continuous(name = "ELN Value / Stock Price (%)", 
                             breaks = c(info$price / info$K1 * 100, 100, max(price) / info$K1 * 100, min(price) / info$K1 * 100), 
                             labels = round(c(info$price / info$K1 * 100, 100, max(price) / info$K1 * 100, min(price) / info$K1 * 100),2)) +
          scale_x_continuous(name = "Stock Movement (%)", 
                             breaks = c(min(S) / info$K1 - 1, 0, S[x.pnl.max] / info$K1 - 1, S[x.pnl.min] / info$K1 - 1, max(S) / info$K1 - 1),
                             labels = round(c(min(S) / info$K1 - 1, 0, S[x.pnl.max] / info$K1 - 1, S[x.pnl.min] / info$K1 - 1,  max(S) / info$K1 - 1) * 100,0)) +
          geom_point(aes(x = 0, y = info$price / info$K1 * 100), pch = 3, size = 5) +
          theme_bw()
      }
    })
    
    
    output$priceSigma <- renderPlot({
      info <- info.class()
      if (!any(is.na(c(info$s, info$K1, info$K2, info$r, info$t, info$q, info$sigma, info$rf)))) {
        maxValue <- info$sigma * 2
        length <- 1000
        sigma <- seq(from = 0, to = maxValue, length = length)
        price <- rep(0, length)
        pnl <- rep(0, length)
        for (i in 1:length) {
          price[i] <- yield.seeking.eln.price(info$s, info$K1, info$K2, sigma[i], info$r, info$t, info$q, info$rf)
          pnl[i] <- ifelse(price[i] < info$s, "#0099CC" , "#999999")
        }
        DF <- data.frame(sigma, price, pnl)
        x.pnl <- which(DF$pnl == "#999999")
        if (length(x.pnl) > 1){
          x.pnl.max <- max(x.pnl)
          x.pnl.min <- min(x.pnl)     
        } else {
          x.pnl.max <- x.pnl
          x.pnl.min <- x.pnl
        }
        
        ggplot(DF, aes(x = sigma * 100, y = price / info$s * 100), fill = pnl) +
          geom_line(aes(color = pnl, group = 1), size = 2) +
          scale_color_identity(guide = "legend", name = "", 
                               breaks = c("#0099CC" , "#999999"), 
                               labels = c("profit", "loss")) +
          scale_y_continuous(name = "ELN Value / Stock Price (%)", 
                             breaks = c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100), 
                             labels = round(c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100),2)) +
          scale_x_continuous(name = "Volatility (%)", 
                             breaks = c(info$sigma * 100, 0, sigma[x.pnl.max]*100, sigma[x.pnl.min]*100, maxValue * 100), 
                             labels = round(c(info$sigma * 100, 0, sigma[x.pnl.max]*100, sigma[x.pnl.min]*100, maxValue * 100),2)) +
          geom_point(aes(x = info$sigma * 100, y = info$price / info$s * 100), pch = 3, size = 5) +
          theme_bw()
      }
    })
    
    output$priceR <- renderPlot({
      info <- info.class()
      if (!any(is.na(c(info$s, info$K1, info$K2, info$r, info$t, info$q, info$sigma, info$rf)))) {
        maxValue <- info$r * 2
        length <- 1000
        r <- seq(from = 0, to = maxValue, length = length)
        price <- rep(0, length)
        pnl <- rep(0, length)
        for (i in 1:length) {
          price[i] <- yield.seeking.eln.price(info$s, info$K1, info$K2, info$sigma, r[i], info$t, info$q, info$rf)
          pnl[i] <- ifelse(price[i] < info$s, "#0099CC" , "#999999")
        }
        DF <- data.frame(r, price, pnl)
        x.pnl <- which(DF$pnl == "#999999")
        if (length(x.pnl) > 1){
          x.pnl.max <- max(x.pnl)
          x.pnl.min <- min(x.pnl)     
        } else {
          x.pnl.max <- x.pnl
          x.pnl.min <- x.pnl
        }
        
        ggplot(DF, aes(x = r * 100, y = price / info$s *100), fill = pnl) +
          geom_line(aes(color = pnl, group = 1), size = 2) +
          scale_color_identity(guide = "legend", name = "", 
                               breaks = c("#0099CC" , "#999999"), 
                               labels = c("profit", "loss")) +
          scale_y_continuous(name = "ELN Value / Stock Price (%)", 
                             breaks = c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100), 
                             labels = round(c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100),4)) +
          scale_x_continuous(name = "Interest rate (%)", 
                             breaks = c(info$r * 100, 0, r[x.pnl.max]*100, r[x.pnl.min]*100, maxValue * 100), 
                             labels = round(c(info$r * 100, 0, r[x.pnl.max]*100, r[x.pnl.min]*100, maxValue * 100),2)) +
          geom_point(aes(x = info$r * 100, y = info$price / info$s * 100), pch = 3, size = 5) +
          theme_bw()
      }
    })
    
    ### For plotting T
    output$priceT <- renderPlot({
      info <- info.class()
      if (!any(is.na(c(info$s, info$K1, info$K2, info$r, info$t, info$q, info$sigma, info$rf)))) {
        maxValue <- info$t * 2
        length <- 1000
        t <- seq(from = 0.01, to = maxValue, length = length)
        price <- rep(0, length)
        pnl <- rep(0, length)
        for (i in 1:length) {
          price[i] <- yield.seeking.eln.price(info$s, info$K1, info$K2, info$sigma, info$r, t[i], info$q, info$rf)
          pnl[i] <- ifelse(price[i] < info$s, "#0099CC" , "#999999")
        }
        DF <- data.frame(t, price, pnl)
        DF2 <- DF[3:length,]
        x.pnl <- which(DF2$pnl == "#999999")
        if (length(x.pnl) > 1){
          x.pnl.max <- max(x.pnl)
          x.pnl.min <- min(x.pnl)     
        } else {
          x.pnl.max <- x.pnl
          x.pnl.min <- x.pnl
        }
        
        ggplot(DF, aes(x = t, y = price / info$s *100), fill = pnl) +
          geom_line(aes(color = pnl, group = 1), size = 2) +
          scale_color_identity(guide = "legend", name = "", 
                               breaks = c("#0099CC" , "#999999"), 
                               labels = c("profit", "loss")) +
          scale_y_continuous(name = "ELN Value / Stock Price (%)", 
                             breaks = c(info$price / info$s * 100, 100, min(price) / info$s * 100), 
                             labels = round(c(info$price / info$s * 100, 100, min(price) / info$s * 100),2)) +
          scale_x_continuous(name = "Tenor", 
                             breaks = c(info$t, 0, t[x.pnl.max], t[x.pnl.min], maxValue), 
                             labels = round(c(info$t , 0, t[x.pnl.max], t[x.pnl.min], maxValue),2)) +
          geom_point(aes(x = info$t, y = info$price / info$s * 100), pch = 3, size = 5) +
          theme_bw()
      }
    })
    
    ### For output Q
    
    output$priceQ <- renderPlot({
      info <- info.class()
      if (!any(is.na(c(info$s, info$K1, info$K2, info$r, info$t, info$q, info$sigma, info$rf)))) {
        maxValue <- info$q * 2
        length <- 1000
        dividendYield <- seq(from = 0, to = maxValue, length = length)
        price <- rep(0, length)
        pnl <- rep(0, length)
        for (i in 1:length) {
          price[i] <- yield.seeking.eln.price(info$s, info$K1, info$K2, info$sigma, info$r, info$t, dividendYield[i], info$rf)
          pnl[i] <- ifelse(price[i] < info$s, "#0099CC" , "#999999")        
        }
        DF <- data.frame(dividendYield, price, pnl)
        x.pnl <- which(DF$pnl == "#999999")
        if (length(x.pnl) > 1){
          x.pnl.max <- max(x.pnl)
          x.pnl.min <- min(x.pnl)     
        } else {
          x.pnl.max <- x.pnl
          x.pnl.min <- x.pnl
        }
        
        ggplot(DF, aes(x = dividendYield * 100, y = price / info$s *100), fill = pnl) +
          geom_line(aes(color = pnl, group = 1), size = 2) +
          scale_color_identity(guide = "legend", name = "", 
                               breaks = c("#0099CC" , "#999999"), 
                               labels = c("profit", "loss")) +
          scale_y_continuous(name = "ELN Value / Stock Price (%)", 
                             breaks = c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100), 
                             labels = round(c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100),2)) +
          scale_x_continuous(name = "Dividend Yield (%)", 
                             breaks = c(info$q * 100, 0, dividendYield[x.pnl.max]*100, dividendYield[x.pnl.min]*100, maxValue * 100), 
                             labels = round(c(info$q * 100, 0, dividendYield[x.pnl.max]*100, dividendYield[x.pnl.min]*100, maxValue * 100),2)) +
          geom_point(aes(x = info$q * 100, y = info$price / info$s * 100), pch = 3, size = 5) +
          theme_bw()
      }
    })
  
    ### For plotting coupon
    
    output$priceCoupon <- renderPlot({
      info <- info.class()
      
      if (!any(is.na(c(info$s, info$K1, info$K2, info$r, info$t, info$q, info$sigma, info$rf)))) {
        maxValue <- info$coupon * 2
        length <- 1000
        coupon <- seq(from = 0, to = maxValue, length = length)
        price <- rep(0, length)
        pnl <- rep(0, length)
        for (i in 1:length) {
          price[i] <- yield.seeking.eln.price(info$s, info$K1, info$s * (1 + coupon[i]), info$sigma, info$r, info$t, info$q, info$rf)
          pnl[i] <- ifelse(price[i] < info$s, "#0099CC" , "#999999")
        }
        DF <- data.frame(coupon, price, pnl)
        x.pnl <- which(DF$pnl == "#999999")
        if (length(x.pnl) > 1){
          x.pnl.max <- max(x.pnl)
          x.pnl.min <- min(x.pnl)     
        } else {
          x.pnl.max <- x.pnl
          x.pnl.min <- x.pnl
        }
        
        ggplot(DF, aes(x = coupon * 100, y = price / info$s *100), fill = pnl) +
          geom_line(aes(color = pnl, group = 1), size = 2) + 
          scale_color_identity(guide = "legend", name = "", 
                               breaks = c("#0099CC" , "#999999"), 
                               labels = c("profit", "loss")) +
          scale_y_continuous(name = "ELN Value / Stock Price (%)", 
                             breaks = c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100), 
                             labels = round(c(info$price / info$s * 100, 100, max(price) / info$s * 100, min(price) / info$s * 100),2)) +
          scale_x_continuous(name = "Coupon rate (%)", 
                             breaks = c(info$coupon * 100, 0, coupon[x.pnl.max]*100, coupon[x.pnl.min]*100, maxValue * 100), 
                             labels = round(c(info$coupon * 100, 0, coupon[x.pnl.max]*100, coupon[x.pnl.min]*100, maxValue * 100),2)) +
          geom_point(aes(x = info$coupon * 100, y = info$price / info$s * 100), pch = 3, size = 5) +
          theme_bw()
      }
    })
    
    ### For payoff graph
    output$payoffGraph <- renderPlot({
      info <- info.class()
      if (is.valid(c(info$s, info$K1, info$K2))) {
        maxValue <- info$K2 * 1.5
        StockPrice <- seq(from = 0, to = maxValue, length.out = 10^3)
        Payoff <- yield.seeking.eln.payoff.par(StockPrice, info$K1, info$K2)
        
        pnl <- ifelse(StockPrice >= info$K1, 1, 0)
        DF <- data.frame(StockPrice, Payoff, pnl)
        shade1 <- data.frame( x1 = c(0, 0, 100 * info$coupon), y1 = c(100, 100 * (1 + info$coupon), 100 * (1 + info$coupon)))
        shade2 <- with(DF, data.frame(x2 = c(0, max((StockPrice / info$K1 -1)*100), max((StockPrice / info$K1 - 1) * 100)), y2 = c(100, 100, max(Payoff) * 100)))
        label <- paste("coupon ", round(info$coupon * 100, 2), "%", sep = " ")
        
       ggplot(DF, aes(x = (StockPrice / info$K1 -1) * 100, y = Payoff * 100), fill = pnl) +
          geom_polygon(data = shade1, aes(x = x1, y = y1), fill="#66CCFF", alpha = 0.5) +
          geom_polygon(data = shade2, aes(x = x2, y = y2), fill="#CCFFCC", alpha = 0.5) +
          geom_abline(slope = 1, intercept = 100, color = "#999999", linetype = "dotted", size = 1) +
          geom_line(aes(group = pnl), colour = "#0099CC", size = 2) +
          coord_fixed(ratio = 1) +
          scale_y_continuous(name = "Payoff %", breaks = round(c(100 *(1 + info$coupon), c(0, 100, 200)),0)) +
          scale_x_continuous(name = "Stock Movement %", breaks = round(c(info$coupon * 100, c(-100, 0, 100)),0)) +
           geom_text(aes(x=-75, y = 200, label = label), size = 3.5) +
          theme_bw()
        
      }
    })

    
    output$errorMessagePayoff <- renderText({
      info <- info.class()
      if (!is.valid(c(info$s, 
                      info$K1, 
                      info$K2
                      ))) {
        return (red.color("Some input is invalid or no solutions for the calculation. Please modify your input."))
      } else {
        return ("")
      }
    })
    
    
    
    # for calculation anything
    info.class <- reactive({
      S <- input$s
      r <- input$r
      q <- input$q
      sigma <- input$sigma
      notional <- input$notional
      rf <- S
      
      if (input$type == "Manual") {
        t <- input$t
        coupon <- input$coupon
        K1 <- S
        K2 <- S * (1 + coupon)
        if (is.valid(c(S, K1, K2, sigma, r, t, q, rf))) {
          value <- yield.seeking.eln.price(S, K1, K2, sigma, r, t, q, rf)
          profit.percentage <- yield.seeking.eln.profit.percentage(S, K1, K2, sigma, r, t, q, rf)
        } else {
          value <- NA
          profit.percentage <- NA
        }
        
      } else if (input$bankerType == "Find Profit") {
        t <- input$t
        coupon <- input$coupon
        K1 <- S
        K2 <- S * (1 + coupon)
        if (is.valid(c(S, K1, K2, sigma, r, t, q, rf))) {
          value <- yield.seeking.eln.price(S, K1, K2, sigma, r, t, q, rf)
          profit.percentage <- yield.seeking.eln.profit.percentage(S, K1, K2, sigma, r, t, q, rf)
        } else {
          value <- NA
          profit.percentage <- NA
        }
      } else if (input$bankerType == "Find Coupon") {
        t <- input$t
        value <- get.price.from.profit.percentage(S, input$profit)
        profit.percentage <- input$profit
        K1 <- S
        if (is.valid(c(S, sigma, r, t, q, rf, value))) {
          coupon <- yield.seeking.eln.coupon(S, sigma, r, t, q, rf, value)
          K2 <- S * (1 + coupon)
        } else {
          coupon <- NA
          K2 <- NA
        }
      } else if (input$bankerType == "Find Tenor") {
        value <- get.price.from.profit.percentage(S, input$profit)
        coupon <- input$coupon
        K1 <- S
        K2 <- S * (1 + coupon)
        profit.percentage <- input$profit
        
        if (is.valid(c(S, sigma, r, q, rf, coupon, value))) {
          t <- yield.seeking.eln.tenor(S, sigma, r, q, rf, coupon, value)
        } else {
          t <- NA
        }
      } else {
        # error case
      }
      
      if (is.valid(c(S, K1, K2, sigma, r, t, q, rf))){
        delta <- yield.seeking.eln.delta(S, K1, K2, sigma, r, t, q, rf)
      } else {
        delta <- NA
      }
      
      if (is.valid(c(delta, notional, S))) {
        num.share.hedge <- delta * (notional / S)
      } else {
        num.share.hedge <- NA
      }
      
      if (is.valid(c(value, S))) {
        value.percentage <- value / S  
      } else {
        value.percentage <- NA
      }
      
      info <- list(s = S, 
                   r = r, 
                   q = q, 
                   sigma = sigma, 
                   notional = notional, 
                   rf = rf, 
                   t = t, 
                   coupon = coupon, 
                   K1 = K1, 
                   K2 = K2, 
                   price = value, 
                   profit = profit.percentage, 
                   delta = delta, 
                   num.share.hedge = num.share.hedge, 
                   price.percentage = value.percentage)
      return (info)
    })
    
    
    output$optionTable <- renderTable({
      info <- info.class()
      name <- c("Stock Price", 
                "K1", 
                "K2", 
                "Risk Free Rate (%)", 
                "Tenor", 
                "Volatility (%)", 
                "Dividend Yield (%)", 
                "Coupon (%)",
                "Notional"
                )    
      value <- c(info$s, 
                 info$K1, 
                 info$K2, 
                 as.percentage(info$r), 
                 info$t, 
                 as.percentage(info$sigma), 
                 as.percentage(info$q), 
                 as.percentage(info$coupon),
                 info$notional
                 )
      df <- data.frame(name, value)
    }, include.rownames=FALSE, include.colnames=FALSE, digits = 2)
    
    output$optionTable2 <- renderTable({
      info <- info.class()
      name <- c("ELN Value",
                "Delta", 
                "Number of shares to hedge", 
                "ELN Value / Stock Price (%)", 
                "Banker Profit (%)", 
                "Banker Profit per notional ($)"
      )    
      value <- c(info$price, 
                  info$delta, 
                  info$num.share.hedge, 
                  as.percentage((1 - info$profit)), 
                  as.percentage(info$profit), 
                  info$notional * info$profit
      )
      df <- data.frame(name, value)
    }, include.rownames=FALSE, include.colnames=FALSE, digits = 2)
    
    output$errorMessageTable <- renderText({
      info <- info.class()
      if (!is.valid(c(info$s, 
                    info$K1, 
                    info$K2, 
                    info$r, 
                    info$t, 
                    info$sigma, 
                    info$q, 
                    info$coupon,
                    info$price, 
                    info$delta, 
                    info$num.share.hedge, 
                    info$profit, 
                    info$notional))) {
        return (red.color("Some input is invalid or no solutions for the calculation. Please modify your input."))
      } else {
        return ("")
      }
    })
    
    
    ############## Message
    error.message.pricing <- reactive({
      info <- info.class()
      if (!is.valid(c(info$s, 
                      info$K1, 
                      info$K2, 
                      info$r, 
                      info$t, 
                      info$sigma, 
                      info$q, 
                      info$rf))) {
        return (red.color("Some input is invalid or no solutions for the calculation. Please modify your input."))
      } else {
        return ("")
      }
    })
    
    output$errorMessageCoupon <- renderText({
      return (error.message.pricing())
    })
    
    output$errorMessageQ <- renderText({
      return (error.message.pricing())
    })
    
    output$errorMessageT <- renderText({
      return (error.message.pricing())
    })
    
    output$errorMessageR <- renderText({
      return (error.message.pricing())
    })
    
    output$errorMessageSigma <- renderText({
      return (error.message.pricing())
    })
    
    output$errorMessageS <- renderText({
      return (error.message.pricing())
    })
    
    
    ############## Client Page here
    
    implied.coupon <- reactive({
      S <- input$impliedS
      r <- input$impliedR
      t <- input$impliedT / 12
      q <- input$impliedQ
      sigma <- input$impliedSigma
      fee <- 0.015
      price <- input$impliedPrice * (1 - fee)
      rf <- S
      if (is.valid(c(S, r, t, q, sigma, rf, price))) {
        coupon <- yield.seeking.eln.coupon(S, sigma, r, t, q, rf, price)
        return (coupon)
      } else {
        return (NA)
      }
    })
    
    output$impliedptionTable <- renderTable({
      impliedS <- input$impliedS
      impliedSigma <- as.percentage(input$impliedSigma)
      impliedT <- input$impliedT / 12
      impliedR <- as.percentage(input$impliedR)
      impliedQ <- as.percentage(input$impliedQ)
      impliedPrice <- input$impliedPrice
      impliedFee <- as.percentage(input$impliedFee)
      impliedCoupon <- implied.coupon()
      name <- c("Stock Price", 
                "Volatility (%)", 
                "Tenor (Year)", 
                "Risk Free Rate (%)", 
                "Dividend Yield (%)", 
                "Fee", 
                "Coupon (%)")
      value <- c(impliedS, 
                 impliedSigma, 
                 impliedT, 
                 impliedR, 
                 impliedQ, 
                 impliedFee, 
                 as.percentage(impliedCoupon))
      df <- data.frame(name, value)
    }, include.rownames=FALSE, include.colnames=FALSE)

    output$impliedPayoffGraph <- renderPlot({
      S <- input$impliedS
      r <- input$impliedR
      t <- input$impliedT / 12
      q <- input$impliedQ
      sigma <- input$impliedSigma
      fee <- 0.015
      price <- input$impliedPrice * (1 - fee)
      rf <- S
      if (!any(is.na(c(S, r, t, q, sigma, fee, rf, price)))) {
        coupon <- yield.seeking.eln.coupon(S, sigma, r, t, q, rf, price)
        K1 <- S
        K2 <- S * (1 + coupon)
      } else {
        coupon <- NA
        K1 <- NA
        K2 <- NA
      }
      if (!any(is.na(c(S, K1, K2)))) {
        maxValue <- K2 * 1.5
        StockPrice <- seq(from = 0, to = maxValue, length.out = 10^3)
        Payoff <- yield.seeking.eln.payoff.par(StockPrice, K1, K2)
        
        pnl <- ifelse(StockPrice >= K1, 1, 0)
        DF <- data.frame(StockPrice, Payoff, pnl)
        shade1 <- data.frame( x1 = c(0, 0, 100*coupon), y1 = c(100,100*(1+coupon), 100*(1+coupon)))
        shade2 <- with(DF, data.frame( x2 = c(0, max((StockPrice/K1-1)*100), max((StockPrice/K1-1)*100)), y2 = c(100,100,max(Payoff)*100)))
        label <- paste("coupon ", round(coupon * 100, 2), "%", sep = " ")
        
        ggplot(DF, aes(x = (StockPrice/K1-1)*100, y = Payoff * 100), fill = pnl) + 
          geom_polygon(data = shade1, aes(x = x1, y = y1), fill="#66CCFF", alpha = 0.5) +
          geom_polygon(data = shade2, aes(x = x2, y = y2), fill="#CCFFCC", alpha = 0.5) +
          geom_abline(slope = 1, intercept = 100, color = "#999999", linetype = "dotted", size = 1) +
          geom_line(aes(group = pnl), colour = "#0099CC", size = 2) +
          coord_fixed(ratio = 1) +
          scale_y_continuous(name = "Payoff %", 
                             breaks = c(100 *(1 + coupon), c(0, 100, 200)), 
                             labels = round(c(100 *(1 + coupon), c(0, 100, 200)),2)) +
          scale_x_continuous(name = "Stock Movement %", 
                             breaks = c(coupon * 100, c(-100, 0, 100)),
                             labels = round(c(coupon * 100, c(-100, 0, 100)),2)) +
          geom_text(aes(x=-75, y = 200, label = label), size = 3.5) +
          theme_bw()
      }
    })
    

    output$errorMessageImpliedPricing <- renderText({
      impliedS <- input$impliedS
      impliedSigma <- input$impliedSigma
      impliedT <- input$impliedT / 12
      impliedR <- input$impliedR
      impliedQ <- input$impliedQ
      impliedPrice <- input$impliedPrice
      impliedFee <- input$impliedFee
      rf <- impliedS
      coupon <- implied.coupon()
      if (!is.valid(c(impliedS, 
                      impliedSigma, 
                      impliedT, 
                      impliedR, 
                      impliedQ, 
                      impliedPrice, 
                      impliedFee,
                      coupon))) {
        return (red.color("Some input is invalid or no solutions for the calculation. Please modify your input."))
      } else {
        return ("")
      }
    })
  }  
)