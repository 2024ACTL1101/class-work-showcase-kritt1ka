
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# create new columns for the AMD and S&P 500 daily returns and initialise them as N/A
df$AMD_daily_return <- NA
df$GSPC_daily_return <- NA

for (i in 2:nrow(df)) {
  
  # iterate through rows and calculate the daily return, updating the column for each day
  df$AMD_daily_return[i] <- ((df$AMD[i]-df$AMD[i-1])/df$AMD[i-1])
  df$GSPC_daily_return[i] <- ((df$GSPC[i]-df$GSPC[i-1])/df$GSPC[i-1])
  
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
# create a new column for the risk-free rate and intialise the entire column as N/A
df$daily_RF_rate <- NA

for (i in 2:nrow(df)) {
  
  # iterate through each row and calcuate the risk-free rate 
  # for each day, using the formula above
  df$daily_RF_rate[i] <- ((1 + (df$RF[i]/100))^(1/360))-1
  
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r

# create new columns for the AMD and S&P 500 excess returns and initialise them as N/A
df$AMD_excess_returns <- NA
df$GSPC_excess_returns <- NA

for (i in 2:nrow(df)) {
  # iterate through the rows and calculate the excess return for each day
  df$AMD_excess_returns[i] <- df$AMD_daily_return[i] - df$daily_RF_rate[i]
  df$GSPC_excess_returns[i] <- df$GSPC_daily_return[i] - df$daily_RF_rate[i]
}

```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r

# remove the first row as it is N/A
df <- df[-1,] 

# create a linear model (lm) to estimate the beta 
capm_lm <- lm(AMD_excess_returns ~ GSPC_excess_returns, data = df)

# use the summary() function to create and print a summary of the linear model
summary(capm_lm)

# extract the beta coefficient from the summary
beta <- coef(summary(capm_lm))[2]

# print the beta value
cat("Beta:", beta, "\n")

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**

The regression analysis yields a \(\beta\) value of 1.57, indicating that AMD's share prices tend to fluctuate by an average of 1.57% for every 1% change in the S&P 500's share prices, suggesting that AMD exhibits more changes, and thus, is more volatile than the broader market.

Investing in stocks with higher volatility, such as AMD involves greater risks, but also potential for higher returns. While small decreases in the market can cause substantial losses, insignificant increases in the market can lead to more significant gains in AMD share prices, ultimately benefiting investors. As a result, risk-averse investors are encouraged to invest in the S&P 500 which offers more stability, whereas the investors willing to take on more risk for greater potential returns should invest in AMD. For the most lucrative and stable investment, investors should invest in both AMD and the S&P 500, as diversifying their portfolio with high and low beta stocks will assist in benefiting from the higher returns from AMD as well as minimising overall portfolio volatility. 


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r

# use ggplot to plot the AMD excess returns vs the S&P excess returns
ggplot(df, aes(x = GSPC_excess_returns, y = AMD_excess_returns)) + 
# create the scatter plot with 0.5 size points, for more clarity and detail
geom_point(size = 0.5) + 
# add a linear regression line with a shaded confidence interval, and make the line purple
geom_smooth(method = "lm", se = TRUE, colour = "purple")+
# add titles to the graph and axis.
labs(title = "AMD vs. S&P 500 Excess Returns", 
     x = "S&P 500 Excess Returns", 
     y = "AMD Excess Returns")

```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
# define the:

# risk free rate,
rf_rate <- 0.05

# annual expected return,
annual_expected_return <- 0.133

# the z score for a 90% confidence interval
z <- 1.645

# extract the standard error of the linear regression model
std_error <- summary(capm_lm)$sigma

# calculate the annual standard error using the formula given above. 
annual_std_error <- std_error * sqrt(252)

# calculate the expected annual return
expected_annual_return <- rf_rate + ((annual_expected_return - rf_rate) * beta)

# calculate the lower and upper bounds
lower_bound <- expected_annual_return - z * annual_std_error
upper_bound <- expected_annual_return + z * annual_std_error

# print the lower and upper bounds for the 90% prediction interval
cat(sprintf("The 90%% prediction interval for AMD's annual expected return is: [%.5f,%.5f]", 
            lower_bound, upper_bound))
```

This wide range (-49.0% to 85.1%) is reflective of the volatility of AMD's returns and highlights the risk associated with investing solely in AMD in comparison to investing in the S&P 500.
