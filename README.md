hkupop
======

hkupop: bootstrap mean

This analysis is attempted to solve the alleged problem of non-normal distribution in the HKUPOP dataset and provide a robust estimation on the central tendency of CY Leung's rating.

The method is simple:

1. Generate a bootstrap sample. Calculate the gender / age-group weighted mean rating
2. Lather, rinse, repeat step 1 for 10K times. Calculate the median and 0.05% and 99.95% percentile.

