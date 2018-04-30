### Data
`SPX.csv` contains the SPX close price data; `SPXOption.csv` contains the SPX Option data with matuirity less than 29 trading days. Data are obtained from Optionmetrics.

### Code
First install some packages:

```r
install.packages(c("tidyverse", "data.table", "bizdays"))
```
 
 `functions.R` does the data preprocessing. It also contains several functions to provide data in different format for further use in investigating the SF.  
 usage:
 
 ```r
 source('functions.R')
 # get all dates in order
 get_date()
 # get all maturity in order
 get_tau()
 # get slope matrix [day, tau] at fixed k
 slope_mat = get_atm_skew(k = 0)
  # get convexity matrix [day, tau] at fixed k
  ...
 ```
   
### Changes  
Previously I used the data in `mat` format. Data are stored in a structure and traversing through it is a little inconvenient. I think providing some funnctions to get data frame is more straightforward.  
 New code is in `SF.R`. Code along with results can be seen in `SF.html`. 
 I've also attached the old data and code. 
 
   