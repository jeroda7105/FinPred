


#' Title
#'
#' @param prices
#'
#' @return
#' @export
#' @examples
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


#' Title
#'
#' @param data
#'
#' @return
#' @export
#' @examples
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



#' Title
#'
#' @param data
#'
#' @return
#' @export
#' @examples
# Takes in multiple time series and produces a graph
# of their correlations
# NOTE: Requires igraph package
cor_graph <- function(data){

  # Calculate adjacency matrix of correlations


  return(data)
}



#' Title
#'
#' @param data
#' @param n_splits
#' @param p_vals
#' @param q_vals
#'
#' @return
#' @export
#' @examples
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

    # Get the train and test sets for this iteration
    train_data = data[fold_ids == i]
    test_data = data[fold_ids == i + 1]

    # fit arma model for each combination of the values
    for (p in 1:len_p) {

      for (q in 1:len_q) {

        # Fit the arma model on the training data
        arma_fit = arima(train_data, order=c(p, 0, q), method="ML")

        # Predict future values up to the length of the test data
        pred_vals = predict(arma_fit, n.ahead = length(test_data))

        # Calculate the mean-squared error and add it to the error matrix
        mse = sum((pred_vals - test_data)^2) / length(test_data)

        err_matrix[p , q] = err_matrix[p , q] + mse
      }

    }

  }

  # Get location of minimum value from the error matrix to get the best p and q values
  best_p_q = arrayInd(which.min(err_matrix), dim(err_matrix))

  best_p = best_p_q[1, 1]
  best_q = best_p_q[1, 2]

  arma_model = arima(data, order=c(best_p, 0, best_q), method="ML")

  return(arma_model)
}



#' Title
#'
#' @param data
#'
#' @return
#' @export
#' @examples
# Performs model selection for time series data using a rolling window
# for random forests and outputs this model
# NOTE: Requires randomForest package
rf_selection <- function(data){
  return(rf_model)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#' @examples
# Performs model selection for time series data using a rolling window
# for Support Vector Regression and outputs this model
# NOTE: Requires e1071 package
svr_selection <- function(data){
  return(svr_model)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#' @examples
# Performs model selection for time series data using a rolling window
# for xgboost and outputs this model
# NOTE: Requires xgboost package
xgboost_selection <- function(data){
  return(xgboost_model)
}

