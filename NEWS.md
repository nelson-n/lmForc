# lmForc 0.0.1

* Added `Forecast()` S4 class.

* Added `is_forc()` function.

* Added `oos_realized_forc()` function.

* Added `oos_lag_forc()` function.

* Added `oos_vintage_forc()` function.

* Added `conditional_forc()` function.

* Added `historical_mean_forc()` function.

* Added `random_walk_forc()` function.

* Added `autoreg_forc()` function.

* Added `mse_weighted_forc()` function.

# lmForc 0.1.0

* Changed the name of the `collect()` function to `forc2df()` to avoid namespace conflict with `dplyr::collect()`.

* Altered `mse()` and `rmse()` methods so that forecast accuracy can be calculated if there are NA `forecast` or `realized` values.

* Altered `autoreg_forc()` so that AR models are properly computed using one to `ar_lags` number of lags and `h_ahead` forecasts are computed iteratively.

* Added `mae()`, `mape()`, and `R2()` methods for evaluating forecast accuracy.

* Altered `estimation_end` argument so that the origin of the first forecast is always greater than or equal to the `estimation_end` time.

* Changed `historical_mean_forc()` to `historical_average_forc()` and altered the function so that forecasts can be calculated using either the historical mean or historical median. Also altered the function so that forecasts can be calculated if there are NA values in `realized_vec`.

* Added `return_betas` argument to all applicable functions. If set to TRUE, returns a data frame of the coefficients used to create the forecast in each time period to the Global Environment.

* Created `str` method for `Forecast` objects.

* Added `states_weighted_forc()` function for computing state weighted forecasts.

* Changed name of `mse_weighted_forc()` to `performance_weighted_forc()` to reflect that errors may be either MSE or RMSE.

# lmForc 0.1.1

* Added `mae()` and `mape()` as options for the `errors` argument in `states_weighted_forc()` and `performance_weighted_forc()`.

* Altered `forc2df()` so that if only one Forecast object is converted to a data.frame the forecast column is named "forecast".

# lmForc 1.0.0

* Added `is_forc_general()` function for evaluating in-sample forecasts with any general model.

* Added `oos_realized_forc_general()` function for evaluating out-of-sample forecasts with any general model.

* Added `oos_vintage_forc_general()` function for evaluating out-of-sample forecasts conditioned on vintage forecasts with any general model.

* Added `conditional_forc_general()` function for computing out-of-sample conditional forecasts with any general model.

* Added a number of functions for subsetting and extracting information from `Forecast` objects: `subset_forcs()`, `subset_bytime()`, `subset_identical()`.

* Added a number of functions for transforming `Forecast` objects: `convert_bytime()`, `transform_bytime()`, `convert_byh()`, `transform_byh()`.
