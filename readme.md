# Yield Seeking Equity Linked Note Webpage user guide

## Introduction

This webpage created by [R](https://www.r-project.org) using [Shiny framework](http://shiny.rstudio.com) is for both client and banker to query information of _Yield Seeking Equity Linked Note_ (the product).

## How to run the code

The code contains six files:

1. `trigger.R` which is the file to start the shiny server.
2. `ui.R` which contains the user interfaces of the web.
3. `server.R` which contains the backend logic.
4. `pricingUtil.R` which contains functions of price calculation.
5. `displayUtil.R` which contains functions to format the message for the web to display.
6. `validationUtil.R` which contains functions to validation the input parameters.

To run the server. Please change the input parameter of `setwd()` (in line 6 in `trigger.R` to the directory containing the files mentioned above. After that please run all codes in `trigger.R` in [RStudio](https://www.rstudio.com). Then the website should be started.

## Feature of the web

The webpage contains 2 main pages for banker and client respectively.

### Client Page

The client page is as following:

![Client page](https://github.com/piccture74/MFIN7009Pic/blob/master/MFIN7009%20project%20client%20page.png?raw=true)

This is a page for potential buyer of this product to query the coupon they can get by themself (on public website).  
To query the coupon, what can need to do is to select the stock and the tenor (time to maturity of the product). After that the webpage will show the coupon and the payoff diagram to the client.  
The client can decide whether it is a good investment after seeing the information.

### Banker Page

The banker page is for banker to decide which parameter they can set to achieve their goal. It contains two feature as show as follows:

#### 1. General price calculator

The general price calculator page is as following:

![General Price Calculator page](https://github.com/piccture74/MFIN7009Pic/blob/master/MFIN7009%20project%20banker%20page%20calculator.png?raw=true)

It is a page for banker to enter arbitrary parameter to the product to calculate the price of the product and the profit of the banker can get. Since client will buy the product with the same prices as its underlying regardless of the price of the real price of the product. The banker can get the price different of the real price of the product and the underlying as the profit. The banker can also view the payoff diagram and the relationship of the price/Stock ratio with different parameters.

![General Price Calculator page with graph](https://github.com/piccture74/MFIN7009Pic/blob/master/MFIN7009%20project%20banker%20page%20calculator%20graph.png?raw=true)

#### 2. Parameter solver

The parameter solver page is for banker to determine the parameter when they want to approach to their potential client.  
The page is shown as following:

![Parameter Solver page](https://github.com/piccture74/MFIN7009Pic/blob/master/MFIN7009%20project%20banker%20page%20solver.png?raw=true)

The banker can do the following in this page:

* Find profit rate by entering coupon rate and tenor
* Find coupon rate by entering profit rate and tenor
* Find tenor by entering profit rate and coupon rate

The result displayed is similar to the General Pricing Calculator shown above.

