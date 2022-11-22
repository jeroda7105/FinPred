
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
# Assumes a matrix input with columns as time series
# Names should either be in the first row of the matrix or given as
# an argument
# NOTE: Requires gt package
time_summary <- function(data){

  data = data.frame(data)

  # get number of columns in the data



  return(data)
}


# Takes in multiple time series and produces a graph
# of their correlations
# NOTE: Requires igraph package
cor_graph <- function(data){

  # Calculate adjacency matrix of correlations


  return(data)
}


# Performs model selection for time series data using a rolling window
# for arma and outputs this model
# Inputs:
# data - a univariate time series
# p_vals - a list of values for the number of autoregressive components in the model
# q_vals - a list of values for the number of moving average components in the model
# n_splits - number of splits on the data for the rolling window
arma_selection <- function(data, n_splits, p_vals, q_vals){

  # Get the number of p and q values
  len_p = length(p_vals)
  len_q = length(q_vals)

  # Get the length of the data
  n = length(data)

  # Assign folds to data
  fold_ids = sort(sample((1:n) %% n_splits + 1, n))

  # Initialize matrix of error values for combinations of parameters
  err_matrix = matrix(0, nrow = len_p, ncol = len_q)

  # Iterate over each fold
  for (i in 1:(n_splits - 1)) {

    # fit arma model for each combination of the values
    for (p in 1:len_p) {

      for (q in 1:len_q) {

        arma_fit = arima(data, order=c(p, 0, q), method="ML")

      }

    }

  }



  return(arma_model)
}

# Performs model selection for time series data using a rolling window
# for random forests and outputs this model
# NOTE: Requires randomForest package
rf_selection <- function(data){
  return(rf_model)
}

# Performs model selection for time series data using a rolling window
# for Support Vector Regression and outputs this model
# NOTE: Requires e1071 package
svr_selection <- function(data){
  return(svr_model)
}

# Performs model selection for time series data using a rolling window
# for xgboost and outputs this model
# NOTE: Requires xgboost package
xgboost_selection <- function(data){
  return(xgboost_model)
}

