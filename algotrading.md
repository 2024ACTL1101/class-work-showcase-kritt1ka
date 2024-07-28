
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA 
amd_df$accumulated_shares <- 0  

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  
  # if the previous price is 0 (meaning it is the first row/day1 - 2019/05/20), 
  # then buy shares
  if (previous_price == 0) {
    
    # set the trade type to buy, and update the costs proceeds, accumulated shares, 
    # and previous price. 
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    previous_price <- amd_df$close[i]
    accumulated_shares <- amd_df$accumulated_shares[i]
    
  # if the close price is less than yesterdays, then buy shares
  } else if (amd_df$close[i] < amd_df$close[i-1]) {
    
    # set the trade type to buy, and update the costs proceeds, accumulated shares, 
    # and previous price. 
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- accumulated_shares+share_size
    previous_price <- amd_df$close[i]
    accumulated_shares <- amd_df$accumulated_shares[i]
  }
  
  # if it is the last day of trading, sell the shares
  if (i == nrow(amd_df)){
    # set the cost proceeds as the profit made from selling the shares, and set 
    # accumulated shares to 0.
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares*amd_df$close[i]
    accumulated_shares <- 0
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# define dates
start_date <- as.Date('2021-09-01') 
end_date <- as.Date('2022-09-01')    

# filter data to include only the trading period
custom_amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
custom_amd_df$trade_type <- NA
custom_amd_df$costs_proceeds <- NA  
custom_amd_df$accumulated_shares <- 0  

# variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# iterate through the rows 
for (i in 1:nrow(custom_amd_df)) {
  
  # if the previous price is 0 (meaning it is the first row/day1 of period), 
  # then buy shares
  if (previous_price == 0) {
    
    # set the trade type to buy, and update the costs proceeds, accumulated shares, 
    # and previous price. 
    custom_amd_df$trade_type[i] <- "buy"
    custom_amd_df$costs_proceeds[i] <- -custom_amd_df$close[i]*share_size
    custom_amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    previous_price <- custom_amd_df$close[i]
    accumulated_shares <- custom_amd_df$accumulated_shares[i]
    
  # if the close price is less than yesterdays, then buy shares
  } else if (custom_amd_df$close[i] < custom_amd_df$close[i-1] && i != nrow(custom_amd_df)) {

    # set the trade type to buy, and update the costs proceeds, accumulated shares, 
    # and previous price. 
    custom_amd_df$trade_type[i] <- "buy"
    custom_amd_df$costs_proceeds[i] <- -custom_amd_df$close[i]*share_size
    custom_amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    previous_price <- custom_amd_df$close[i]
    accumulated_shares <- custom_amd_df$accumulated_shares[i]

  }
  
  # if it is the last day of trading in the period, sell the shares
  if (i == nrow(custom_amd_df)) {
    # set the cost proceeds as the profit made from selling the shares, and set 
    # accumulated shares to 0.
    custom_amd_df$trade_type[i] <- "sell"
    custom_amd_df$costs_proceeds[i] <- accumulated_shares*custom_amd_df$close[i]
    custom_amd_df$accumulated_shares[i] <- 0
  }
}

```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r

# initialise the variables for the calculations
total_profit_or_loss <- 0
total_capital_invested <- 0

# iterate through the rows and calculate the profit/loss and the capital invested
for (i in 1:nrow(custom_amd_df)) {
  
  # calculate the total profit and loss by adding up the total costs_proceeds
  if(!is.na(custom_amd_df$costs_proceeds[i])) {
    total_profit_or_loss <- custom_amd_df$costs_proceeds[i] + total_profit_or_loss
  }
  
  # sum up the total capital invested by adding the negative of the cost_proceeds 
  # when trade_type == buy
  if (!is.na(custom_amd_df$trade_type[i]) && custom_amd_df$trade_type[i] == "buy" ) {
    total_capital_invested <- (-(custom_amd_df$costs_proceeds[i])+total_capital_invested)
  }
}

# calculate the ROI using the formula
ROI <- (total_profit_or_loss/total_capital_invested)*100

# print the results
print(paste("The total profit/loss: ", total_profit_or_loss))
print(paste("The total capital invested: ", total_capital_invested))
print(paste("The Return on Investment (ROI) is: ", ROI))

```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df

custom_amd_df$trade_type <- NA
custom_amd_df$costs_proceeds <- NA  # Corrected column name
custom_amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

expenditure <- 0
avg_purchase_price <- 0


total_profit_or_loss <- 0
total_capital_invested <- 0

# iterate through the rows 
for (i in 1:nrow(custom_amd_df)) {

  # if the previous price is 0 (meaning it is the first row/day1 of period), 
  # then buy shares
  if (previous_price == 0) {
    
    # set the trade type to buy, and update the costs proceeds, accumulated shares, 
    # and previous price. 
    custom_amd_df$trade_type[i] <- "buy"
    custom_amd_df$costs_proceeds[i] <- -custom_amd_df$close[i]*share_size
    custom_amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    previous_price <- custom_amd_df$close[i]
    accumulated_shares <- custom_amd_df$accumulated_shares[i]
    
    # update the expenditure and then average purchase price for the comparison 
    # later to sell. 
    expenditure <- expenditure + (-custom_amd_df$costs_proceeds[i])

    avg_purchase_price <- expenditure/custom_amd_df$accumulated_shares[i]
    
    
  # if the close price is less than yesterdays, then buy shares
  } else if (custom_amd_df$close[i] < custom_amd_df$close[i-1] && i != nrow(custom_amd_df)) {
  
    # set the trade type to buy, and update the costs proceeds, accumulated shares, 
    # and previous price. 
    custom_amd_df$trade_type[i] <- "buy"
    custom_amd_df$costs_proceeds[i] <- -custom_amd_df$close[i]*share_size
    custom_amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    previous_price <- custom_amd_df$close[i]
    accumulated_shares <- custom_amd_df$accumulated_shares[i]
    
    # update the expenditure and then average purchase price for the comparison 
    # later to sell. 
    expenditure <- expenditure + (-custom_amd_df$costs_proceeds[i])

    avg_purchase_price <- expenditure/custom_amd_df$accumulated_shares[i]
      
  # if the share price is greater than a 20% increase from the average purchase price, 
    # then sell half of the shares.  
  } else if (custom_amd_df$close[i] > 1.20*avg_purchase_price) {
    
    # set the trade type to sell, update the accumulated shares so they are halved, 
    # and update the costs proceeds to be the 
    # profit made from selling the shares
    custom_amd_df$trade_type[i] <- "sell"
    custom_amd_df$accumulated_shares[i]<- accumulated_shares*(1/2)
    custom_amd_df$costs_proceeds[i] <- custom_amd_df$accumulated_shares[i]*custom_amd_df$close[i]
    
    # half the expenditure and accumulated shares
    expenditure <- expenditure*(1/2)
    accumulated_shares <- accumulated_shares*(1/2)

  }

  # if it is the last day of trading in the period, sell the shares
  if (i == nrow(custom_amd_df)) {
    
    # set the cost proceeds as the profit made from selling the shares, and set 
    # accumulated shares to 0.
    custom_amd_df$trade_type[i] <- "sell"
    custom_amd_df$costs_proceeds[i] <- accumulated_shares*custom_amd_df$close[i]
  }
  
}

#iterate through the rows and calculate the profit/loss and the capital invested
for (i in 1:nrow(custom_amd_df)) {

  # calculate the total profit and loss by adding up the total costs_proceeds
  if(!is.na(custom_amd_df$costs_proceeds[i])) {
    total_profit_or_loss <- custom_amd_df$costs_proceeds[i] + total_profit_or_loss
  }
  
  # sum up the total capital invested by adding the negative of the cost_proceeds 
  # when trade_type == buy
  if (!is.na(custom_amd_df$trade_type[i]) && custom_amd_df$trade_type[i] == "buy" ) {
    total_capital_invested <- (-(custom_amd_df$costs_proceeds[i])+total_capital_invested)
  }
}

#calculate the roi using the roi formula
ROI <- (total_profit_or_loss/total_capital_invested)*100

#print the results 
print(paste("The total profit/loss: ", total_profit_or_loss))
print(paste("The total capital invested: ", total_capital_invested))
print(paste("The Return on Investment (ROI) is: ", ROI))

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.

Though remaining negative, the ROI increased by 6.99% using the profit-taking strategy, reduing the ROI from -24.56% (with $353878.96 in total loss), to -17.57% (with $253107.85 in total loss), thereby, saving approximately $10,000. Despite the increase in ROI using the new strategy, the loss in investment over the course of 2021-09-01 to 2022-09-01 can be attributed to the macroeconomic conditions, competitive pressures, as well as supply chain disruptions as a result of COVID-19. 

The tightening monetary policies and increasing interest rates amidst COVID-19 led to significant fluctuations and volatility within the stock market, along with decreasing buyer confidence. As central banks shifted from the previous stimulus measures during the pandemic, to more restrictive policies with higher interest rates, growth stocks which rely on future earning predictions, like AMD holistically experience a downturn, as stock valuations decrease due to the higher interest rates.

Moreover the global supply chain disruptions heavily impacted the semiconductor companies, including AMD, faced component shortages and other logistic issues during 2021/2022, which negatively impacted production capabilities and reduced profitability, eventually leading to concern and a lack of confidence amongst investors. 

In addition to the external economic and supply chain challenges, AMD faced continual competition from industry rivals; Intel and NVIDIA. Intel's launch of new processor lines and NVIDIA'S dominance of the GPU market increased the competitive pressure on AMD, strengthening their respective market positions, while directly influencing investor sentiment towards AMD's market position and value, thus contributing further to the companies declining share prices. 
