library(shinyjs)
shinyServer(
  fluidPage(
    useShinyjs(), 
    fluidPage(
      fluidRow(
        column(9, titlePanel("Yield Seeking Equity Linked Note")),
        column(3, selectInput("userType", "", c("For Client" = "Client Page", "For Banker" = "Banker Page"))
        )
      )
    ),
    conditionalPanel(condition="input.userType == 'Banker Page'",
      sidebarLayout(
        sidebarPanel(
          radioButtons("type", "Please Select Type", choices=c("General Pricing Calculator" = "Manual", "Parameter Solver" = "Symbol")),
          conditionalPanel(condition="input.type == 'Symbol'", radioButtons("bankerType", "Please Select Type", choices=c("Find Profit (by Coupon and Tenor)" = "Find Profit", "Find Coupon (by Tenor and Profit)" = "Find Coupon", "Find Tenor (by Coupon and Profit)" = "Find Tenor"))),
          conditionalPanel(condition="input.type == 'Symbol'", textInput("ticker", "Please Select stock ticker:", "2800.HK")),
          numericInput("notional", "Notional:", 1000000),
          conditionalPanel(condition="input.type == 'Manual'", numericInput("s", "Stock Price:", 50)),
          conditionalPanel(condition="input.type == 'Manual' || (input.type == 'Symbol' && input.bankerType != 'Find Coupon')", numericInput("coupon", "Coupon:", 0.4, max = 1, min = 0)),
          conditionalPanel(condition="input.type == 'Manual'", numericInput("sigma", "Volatility:", 0.3)),
          conditionalPanel(condition="input.type == 'Manual' || (input.type == 'Symbol' && input.bankerType != 'Find Tenor')", numericInput("t", "Tenor (Years):", 3)),
          conditionalPanel(condition="input.type == 'Symbol' && input.bankerType != 'Find Profit'", numericInput("profit", "Profit desired:", 0.03)),
          conditionalPanel(condition="input.type == 'Manual'", numericInput("r", "Risk Free Rate:", 0.015)),
          conditionalPanel(condition="input.type == 'Manual'", numericInput("q", "Dividend yield:", 0.06))
          ),
        mainPanel(
          tabsetPanel(
            tabPanel("Summary", 
              fluidPage(
                fluidRow(
                  htmlOutput("errorMessageTable"), 
                  fluidPage(
                    fluidRow(
                      column(5, tableOutput("optionTable")),
                      column(7, tableOutput("optionTable2"))
                    )
                  )
                )
              )
            ),
            tabPanel("Payoff", fluidPage(fluidRow(htmlOutput("errorMessagePayoff"), plotOutput("payoffGraph")))),
            tabPanel("S", fluidPage(fluidRow(htmlOutput("errorMessageS")), fluidRow(plotOutput("priceS")))),
            tabPanel("Volatility", fluidPage(fluidRow(htmlOutput("errorMessageSigma")), fluidRow(plotOutput("priceSigma")))),
            tabPanel("Risk free", fluidPage(fluidRow(htmlOutput("errorMessageR")), fluidRow(plotOutput("priceR")))),
            tabPanel("Tenor", fluidPage(fluidRow(htmlOutput("errorMessageT")), fluidRow(plotOutput("priceT")))),
            tabPanel("Dividend", fluidPage(fluidRow(htmlOutput("errorMessageQ")), fluidRow(plotOutput("priceQ")))),
            tabPanel("Coupon", fluidPage(fluidRow(htmlOutput("errorMessageCoupon")), fluidRow(plotOutput("priceCoupon"))))
          )
        )
      )
    ),
    conditionalPanel(condition="input.userType == 'Client Page'",
     sidebarLayout(
       sidebarPanel(
         hidden(radioButtons("impliedType", "Please Select Type", choices=c("Symbol", "Manual"))),
         conditionalPanel(condition="input.impliedType == 'Symbol'", selectInput("impliedTicker", "Please select stock ticker:", c("CKH HOLDINGS (0001.HK)" = "0001.HK", "ICBC (1398.HK)" = "1398.HK", "TRACKER FUND (2800.HK)" = "2800.HK"))),
         hidden(numericInput("impliedS", "S:", 50)),
         hidden(numericInput("impliedSigma", "Sigma:", 0.2)),
         sliderInput("impliedT", "Tenor (Months):", min=3, max=12, value=3, step=3),
         hidden(numericInput("impliedR", "r:", 0.0125)),
         hidden(numericInput("impliedQ", "Dividend yield:", 0.06)),
         hidden(numericInput("impliedPrice", "Market Price:", 49.62)),
         hidden(numericInput("impliedFee", "Management Fee:", 0.015)),
         tableOutput("impliedptionTable")
        ),
        mainPanel(
          fluidPage(
            fluidRow(
              h4("Payoff Diagram"),
              htmlOutput("errorMessageImpliedPricing"), 
              plotOutput("impliedPayoffGraph")
            )
          )
        )
      )
    )
  )
)