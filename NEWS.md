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
