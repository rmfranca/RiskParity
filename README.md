
# RiskParity

<!-- badges: start -->
<!-- badges: end -->

RiskParity computes and plots a chart of the cumulative return of a risk parity portfolio containing three assets, comparing it to the returns of the individual assets.

The Bloomberg API is used to get historical prices and compute covariance matrices for a rolling 3 month period. The porfolio is then rebalanced every 3 months.

Rebalancing periodicity and time frame for the analysis can be easily customized by the user.

Any improvements on this code are welcomed.


