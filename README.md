
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FinPred

<!-- badges: start -->
<!-- badges: end -->

FinPred is an R package to be used for the analysis and prediction of
financial time series. It contains functions for analysis such as ones
to the create tables of summary statistics and visualizations of graphs
for correlations between assets. It can also be used to perform model
selection for various time series prediction and machine learning models
given data and sets of hyperparameters to tune.

## Installation

You can install the development version of FinPred from
[GitHub](https://github.com/jeroda7105/FinPred) with:

``` r
# install.packages("devtools")
devtools::install_github("jeroda7105/FinPred", build_vignettes = TRUE)
```

The package can then be imported with the command

``` r
library(FinPred)
```

## Example

This is an example of how the package can be used for prediction on
\$GLD, an exchange-traded fund (ETF) which attempts to track the price
of gold. The dataset contains adjusted closing prices from yahoo
finance, extracted using the quantmod R package (Ryan & Ulrich, 2022).

``` r
library(FinPred)

# Getting daily data for an ETF which tracks the price of the gold
gld_data = as.data.frame(quantmod::getSymbols('GLD', env = NULL))
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
gld_adj_close = gld_data$GLD.Adjusted
```

Once we have the time series of prices, we can use the function
`prices_to_returns` to convert them to series of returns and then
process this data:

``` r
# Convert the prices to returns
gld_ret = prices_to_returns(gld_adj_close)

# Standardizing the data
std_gld_ret = (gld_ret - rep(mean(gld_ret), length(gld_ret))) / sd(gld_ret)


# Get training data 
n = length(std_gld_ret)
train_idxs = floor(n*0.95)
std_gld_ret_train = std_gld_ret[1:train_idxs]
```

One machine learning method that can be used for prediction is the
Random Forest model (Breiman, 2001). The function `rf_selection` can be
used to tune hyperparameters for this model using the implementation
from the randomForest R package (Liaw & Wiener, 2002). An example of its
use on this data is:

``` r
n_trees = c(500, 750, 1000)
node_sizes = c(3, 5, 7)

rf_params = rf_selection(std_gld_ret_train, n_splits = 10, window_size = 10, n_trees = n_trees,
                         node_sizes = node_sizes)
rf_params
#> $ntree
#> [1] 1000
#> 
#> $nodesize
#> [1] 7
#> 
#> $mse
#> [1] 1.032633
```

The following code shows how to use the previously obtained
hyperparameters for Random Forests to fit a model:

``` r

# Create supervised data for svm
window_size = 10
X = windowed_data(std_gld_ret, window_size = window_size)

n = nrow(X)

# Account for the size of the window in the data
y = std_gld_ret[window_size + 1 : n]

train_idxs = floor(n*0.95)

# Split into train and test
X_train =  X[1:train_idxs, ]
X_test =  X[(train_idxs + 1):n, ]

y_train = y[1:train_idxs]
y_test = y[(train_idxs + 1):n]

# Fit a model with chosen parameters on data
rf_model = randomForest::randomForest(x = X_train, y = y_train, keep.forest=TRUE,
                                ntree = rf_params$ntree, 
                        nodesize = rf_params$nodesize)
```

Now that the model is fit, prediction performance on future values is
assessed:

``` r
# Get predicted values
pred_vals = predict(rf_model, X_test)

# Calculate the error 
error = sum((pred_vals - y_test)^2) / length(y_test)
error
#> [1] 0.8151546

# Get proportion of correct directional movements predicted
sum(sign(y_test) == sign(pred_vals)) / length(y_test)
#> [1] 0.4676617


# Overlay predictions
plot(y_test, type = "l")
lines(pred_vals, type = "l", col = "blue")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## References

Breiman, L. (2001). Random Forests. Machine Learning, 45(1), 5???32.
<https://doi.org/10.1023/a:1010933404324>

Liaw A., Wiener M. (2002). Classification and Regression by
randomForest. R News 2(3), 18???22.

Ryan JA, Ulrich JM (2022). *quantmod: Quantitative Financial Modelling
Framework*. R package version 0.4.20,
<https://CRAN.R-project.org/package=quantmod>.
