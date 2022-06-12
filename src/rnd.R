#*******************************************************************************
#
# Function: `rnd()`
# Date:     11-Oct-2021
# Author:   Rob Fletcher
# Purpose:  Correctly round numbers (`round()` function in R works weirdly)
# 
#*******************************************************************************

rnd = function(x, decimals) {
  # Round numbers correctly to a specified number of decimal places
  #
  # Arguments
  # --------- 
  # x : double (number to round)
  # decimals : integer (number of decimal places to round to)
  #
  # Returns
  # -------
  # tibble
  
  pos_neg = sign(x)
  y = abs(x) * 10^decimals
  y = y + 0.5 + sqrt(.Machine$double.eps)
  y = trunc(y)
  y = y / 10^decimals
  y * pos_neg
}