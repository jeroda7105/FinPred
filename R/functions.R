
# Takes in a vector of price data and converts it to returns
prices_to_returns <- function(prices){

  # If data is null or has length less than 2 return an error
  if (is.null(prices) | length(prices) < 2) {
    stop("input data must have length of at least 2")
  }

  # Get number of points
  n = length(prices)

  # Calculate the relative change in price across each time point
  returns = (prices[2 : n] - prices[1 : (n - 1)]) /  prices[1 : (n - 1)]

  return(returns)
}

# takes in a single or multiple time series of returns and
# outputs a table with summary statistics for the time series
time_summary <- function(data){
  return(data)
}


# Takes in multiple time series and produces a graph
# of their correlations
# NOTE: Requires igraph package
cor_graph <- function(data){
  return(data)
}


# Performs model selection for time series data using a rolling window
# for arma and outputs this model
arma_selection <- function(data){
  return(data)
}

# Performs model selection for time series data using a rolling window
# for random forests and outputs this model
# NOTE: Requires randomForest package
rf_selection <- function(data){
  return(data)
}

# Performs model selection for time series data using a rolling window
# for Support Vector Regression and outputs this model
# NOTE: Requires e1071 package
svr_selection <- function(data){
  return(data)
}

# Performs model selection for time series data using a rolling window
# for xgboost and outputs this model
# NOTE: Requires xgboost package
xgboost_selection <- function(data){
  return(data)
}

