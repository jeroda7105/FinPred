


#' Convert price series to returns series
#'
#' @param prices
#' A vector of price data in order of time
#'
#' @return A vector of returns, the relative change in prices for the given vector,
#'  with length of one less than the input
#'
#' @export
#' @examples
#'
#' # Initialize data acting as simulated price data
#' prices = rnorm(50)
#'
#' # Convert to vector of returns
#' returns = prices_to_returns(prices)
#'
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

#' Get windowed data from a given time series
#'
#' @param data A vector containing a univariate time series
#' @param window_size The size of the window that precedes the time steps
#'
#' @return A matrix containing windowed data having dimensions
#'  (# of time points - window_size) x window_size
#'
#'
#' @export
#' @examples
#'
#' # Initializing data
#' data = rnorm(50)
#'
#' # Get windowed data from this vector using window size of 5
#' windowed = windowed_data(data, 5)
#'
windowed_data <- function(data, window_size){

  # Get size of data
  n = length(data)

  # Return error if length of data is less than window_size + 1
  if (n < window_size + 1) {
    error("Length of data must be at least the window size plus 1")
  }

  # Initialize matrix for the windowed data
  window_mat = matrix(nrow = n - window_size, ncol = window_size)

  for (i in 1:(n - window_size)) {

    window_mat[i, ] = data[i:(i + window_size - 1)]

  }


  return(window_mat)
}

#' Get a table of summary statistics for one or more time series
#'
#' @param data A vector, matrix or dataframe containing a time series
#' @param has_names Boolean which is FALSE by default meaning the data has no
#' column names, or TRUE if the data has column names
#'
#' @return
#' A table containing summary statistics for the given data
#' @export
#' @examples
#'
#' # Generate two series of data as one matrix
#' series = matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#'
#' # Create table without names
#' time_summary(series, has_names = FALSE)
#'
#' # Create table with names
#' colnames(series) = c("series_1", "series_2")
#'
#' time_summary(series, has_names = TRUE)
#'
time_summary <- function(data, has_names = FALSE){

  # If names are given, extract them
  if (has_names) {

    # Get the column names
    row_names = colnames(data)

    # Convert data to matrix
    data = data.matrix(data)

    # get number of time series
    n_series = ncol(data)
  }

  # If no names are given, use index numbers
  else {

    data = data.matrix(data)

    # get number of time series
    n_series = ncol(data)

    row_names = c(1:n_series)

  }

  # Initialize matrix for the table
  table = matrix(nrow = n_series, ncol = 4)

  for (i in 1:n_series) {

    # Calculate the summary statistics for the i-th time series
    table[i , 1] = round(mean(data[ , i]), 5)
    table[i , 2] = round(sd(data[ , i]), 5)
    table[i , 3] = round(skewness(data[ , i]), 5)
    table[i , 4] = round(kurtosis(data[ , i]) - 3, 5)
  }

  col_names = c("mean", "sd", "skewness", "ex. kurtosis")

  colnames(table) = col_names
  rownames(table) = row_names


  # Convert the matrix to a table
  table = as.table(table)

  return(table)
}



#' Create a graph of correlations between given time series
#'
#' @inheritParams time_summary
#' @param data A matrix or dataframe whose columns are time series
#'
#' @return
#' A visualization of a weighted undirected graph whose vertices are the
#' labels of the series and edge weights are the correlations between two vertices
#'
#' @export
#' @examples
#'
#' # Generate three series of data as one matrix
#' series = matrix(rnorm(50 * 3), nrow = 50, ncol = 3)
#'
#' # Create graph without names
#' cor_graph(series, has_names = FALSE)
#'
#' # Create graph with names
#' colnames(series) = c("series_1", "series_2", "series_3")
#'
#' cor_graph(series, has_names = TRUE)
#'
cor_graph <- function(data, has_names = FALSE){

  # Check that the data has more than one column
  if (nrow(data.matrix(data)) < 2) {

    stop("Data must have at least two columns")

  }

  # If names are given, extract them
  if (has_names) {

    # Get the column names
    names = colnames(data)

    # Convert data to matrix
    data = data.matrix(data)

    # get number of time series
    n_series = ncol(data)
  }

  # If no names are given, use index numbers
  else {

    data = data.matrix(data)

    # get number of time series
    n_series = ncol(data)

    names = c(1:n_series)

  }



  # initialize adjacency matrix of correlations
  adj_mat = matrix(0, nrow = n_series, ncol = n_series)

  # Calculate adjacency matrix of correlations
  for (i in 1:n_series) {
    for (j in i:n_series) {
      # Making graph undirected with no self-relation
      if (i != j) {
        adj_mat[i, j] = cor(data[ , i], data[ , j])
        adj_mat[j, i] = adj_mat[i, j]
      }
    }
  }

  # Add names to the adjacency matrix
  colnames(adj_mat) = names

  # Create the graph visualization
  graph = graph_from_adjacency_matrix(adj_mat, mode = "undirected",
                                           weighted = TRUE)
  igraph::plot.igraph(graph, edge.label = round(igraph::E(graph)$weight, 3))

}



#' Title
#'
#' @inheritParams windowed_data
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
        pred_vals = pred_vals$pred

        # Calculate the mean-squared error and add it to the error matrix
        mse = sum((pred_vals - test_data)^2) / length(test_data)

        err_matrix[p , q] = err_matrix[p , q] + mse / (n_splits - 1)
      }

    }

  }

  # Get location of minimum value from the error matrix to get the best p and q values
  best_p_q = arrayInd(which.min(err_matrix), dim(err_matrix))

  best_p = best_p_q[1, 1]
  best_q = best_p_q[1, 2]


  return(list(p = best_p, q = best_q, mse = min(err_matrix)))
}



#' Title
#'
#' @inheritParams arma_selection
#' @param window_size
#' @param n_trees
#' @param node_sizes
#'
#' @return
#' @export
#' @examples
# Performs model selection for time series data using a rolling window
# for random forests and outputs this model
# NOTE: Requires randomForest package
rf_selection <- function(data, n_splits, window_size, n_trees, node_sizes){

  # Get lengths of vectors of parameters
  len_n_trees = length(n_trees)
  len_node_sizes = length(node_sizes)



  # Initialize matrix of error values for combinations of parameters
  err_matrix = matrix(0, nrow = len_n_trees, ncol = len_node_sizes)

  # Create the windowed data
  X = windowed_data(data, window_size = window_size)

  n = nrow(X)

  # Account for the size of the window in the data
  y = data[window_size + 1 : n]



  # Assign folds to data
  fold_ids = sort(sample((1:n) %% n_splits + 1, n))


  # Iterate over each fold
  for (i in 1:(n_splits - 1)) {

    # Get the train and test sets for this iteration
    X_train = X[fold_ids == i, ]
    X_test = X[fold_ids == i + 1, ]

    y_train = y[fold_ids == i]
    y_test = y[fold_ids == i + 1]

    # fit randomForest model for each combination of the values
    for (j in 1:len_n_trees) {
      for (k in 1:len_node_sizes) {

        rf_model = randomForest(x = X_train, y = y_train, keep.forest=TRUE,
                                ntree = n_trees[j], nodesize = node_sizes[k])
        # Get predicted values
        pred_vals = predict(rf_model, X_test)

        # Calculate the error and add to this combination of parameters
        cur_error = sum((pred_vals - y_test)^2) / length(y_test)

        err_matrix[j, k] = err_matrix[j, k] + cur_error / (n_splits - 1)

      }
    }
  }


  # Get location of minimum value from the error matrix to get the best parameters
  best_params = arrayInd(which.min(err_matrix), dim(err_matrix))

  best_ntree = n_trees[best_params[1, 1]]
  best_nodesize = node_sizes[best_params[1, 2]]


  return(list(ntree = best_ntree, nodesize = best_nodesize, mse = min(err_matrix)))
}


#' Title
#'
#' @inheritParams rf_selection
#' @param gamma
#' @param C
#' @param epsilon
#'
#' @return
#' @export
#' @examples
# Performs model selection for time series data using a rolling window
# for Support Vector Regression and outputs this model
# NOTE: Requires e1071 package
svr_selection <- function(data, n_splits, window_size, gamma_vals, C_vals,
                          epsilon_vals){

  # Get lengths of vectors of parameters
  len_gamma = length(gamma_vals)
  len_C = length(C_vals)
  len_epsilon = length(epsilon_vals)


  # Initialize tensor of error values for combinations of parameters
  err_tensor = array(rep(0, len_gamma * len_C * len_epsilon),
                     dim = c(len_gamma, len_C, len_epsilon))

  # Create the windowed data
  X = windowed_data(data, window_size = window_size)

  n = nrow(X)

  # Account for the size of the window in the data
  y = data[window_size + 1 : n]



  # Assign folds to data
  fold_ids = sort(sample((1:n) %% n_splits + 1, n))


  # Iterate over each fold
  for (i in 1:(n_splits - 1)) {

    # Get the train and test sets for this iteration
    X_train = X[fold_ids == i, ]
    X_test = X[fold_ids == i + 1, ]

    y_train = y[fold_ids == i]
    y_test = y[fold_ids == i + 1]

    # fit svr model for each combination of the values
    for (j in 1:len_gamma) {
      for (k in 1:len_C) {
        for(l in 1:len_epsilon) {


          svr_model = svm(X_train, y_train, gamma = gamma_vals[j], C = C_vals[k],
                          epsilon = epsilon_vals[l])

          # Get predicted values
          pred_vals = predict(svr_model, X_test)

          # Calculate the error and add to this combination of parameters
          cur_error = sum((pred_vals - y_test)^2) / length(y_test)
          err_tensor[j, k, l] = err_tensor[j, k, l] + cur_error / (n_splits - 1)

        }
      }
    }
  }


  # Get location of minimum value from the error tensor to get the best parameters
  best_params = arrayInd(which.min(err_tensor), dim(err_tensor))

  best_gamma = gamma_vals[best_params[1, 1]]
  best_C = C_vals[best_params[1, 2]]
  best_epsilon = epsilon_vals[best_params[1, 3]]


  return(list(gamma = best_gamma, C = best_C, epsilon = best_epsilon,
              mse = min(err_tensor)))
}


#' Title
#'
#' @inheritParams rf_selection
#' @param nthread
#' @param eta_vals
#' @param gamma_vals
#' @param max_depths
#' @param lambda_vals
#' @param nrounds
#'
#' @return
#' @export
#' @examples
# Performs model selection for time series data using a rolling window
# for xgboost and outputs this model
# NOTE: Requires xgboost package
xgboost_selection <- function(data, nthread = 1, n_splits, window_size, eta_vals,
                              gamma_vals, max_depths, lambda_vals, nrounds){

  # Get lengths of vectors of parameters
  len_eta = length(eta_vals)
  len_gamma = length(gamma_vals)
  len_max_depths = length(max_depths)
  len_lambda = length(lambda_vals)
  len_nrounds = length(nrounds)


  # Initialize tensor of error values for combinations of parameters
  err_tensor = array(rep(0, len_eta * len_gamma * len_max_depths * len_lambda * len_nrounds),
                     dim = c(len_eta, len_gamma, len_max_depths, len_lambda, len_nrounds))

  # Create the windowed data
  X = windowed_data(data, window_size = window_size)

  n = nrow(X)

  # Account for the size of the window in the data
  y = data[window_size + 1 : n]



  # Assign folds to data
  fold_ids = sort(sample((1:n) %% n_splits + 1, n))


  # Iterate over each fold
  for (i in 1:(n_splits - 1)) {

    # Get the train and test sets for this iteration
    X_train = X[fold_ids == i, ]
    X_test = X[fold_ids == i + 1, ]

    y_train = y[fold_ids == i]
    y_test = y[fold_ids == i + 1]

    # fit xgboost model for each combination of the values
    for (j in 1:len_eta) {
      for (k in 1:len_gamma) {
        for (l in 1:len_max_depths) {
          for(m in 1:len_lambda) {
            for (o in 1:len_nrounds) {


              xgb_model = xgboost(data = X_train, label = y_train, nthread = nthread,
                                  max.depth = max_depths[l], eta = eta_vals[j],
                                  gamma = gamma_vals[k], lambda = lambda_vals[m],
                                  nrounds = nrounds[o], verbosity = 0, verbose = 0)

              # Get predicted values
              pred_vals = predict(xgb_model, X_test)

              # Calculate the error and add to this combination of parameters
              cur_error = sum((pred_vals - y_test)^2) / length(y_test)
              err_tensor[j, k, l, m, o] = err_tensor[j, k, l, m, o] + cur_error / (n_splits - 1)

            }

          }
        }
      }
    }
  }


  # Get location of minimum value from the error tensor to get the best parameters
  best_params = arrayInd(which.min(err_tensor), dim(err_tensor))

  best_eta = eta_vals[best_params[1, 1]]
  best_gamma = gamma_vals[best_params[1, 2]]
  best_max_depth = max_depths[best_params[1, 3]]
  best_lambda = lambda_vals[best_params[1, 4]]
  best_nrounds = nrounds[best_params[1, 5]]


  return(list(eta = best_eta, gamma = best_gamma, max_depth = best_max_depth,
              lambda = best_lambda, nrounds = best_nrounds, mse = min(err_tensor)))
}

