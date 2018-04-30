library(tidyverse)
library(data.table)
library(bizdays)

setwd('/Users/steven/Desktop/DataCleaning/SPX/')
# prepare the data
raw.df = fread('./data/SPXOption.csv', sep = ',')
df = raw.df[complete.cases(raw.df), ]
underlying = fread('./data/SPX.csv')

underlying = underlying %>%
    mutate(date = as.Date(as.character.Date(underlying[["date"]]), format = "%Y%m%d")) %>%
    select(-one_of(c('secid', 'volume')))
df = df %>% 
    mutate(date = as.Date(as.character.Date(df[["date"]]), format = "%Y%m%d")) %>%
    mutate(exdate = as.Date(as.character.Date(df[["exdate"]]), format = "%Y%m%d")) %>%
    mutate(tau = bizdays(date, exdate))%>%
    select(-one_of(c('symbol', 'cp_flag', 'optionid', 'exercise_style', 'issuer', 'sic', 'index_flag', 'exdate')))
df = inner_join(df, underlying, by = c('date' = 'date'))
df = df[which(df$volume != 0), ]
df = df  %>% mutate(strike_price = strike_price / 1000) %>% mutate(k = log(strike_price / close))

# functions
get_date <- function() {
    date.set = unique(df[["date"]])
    return(sort(date.set))
}

get_tau <- function() {
	tau.set = unique(df[["tau"]])
	return(sort(tau.set))
} 

n1 = length(get_date())
n2 = length(get_tau())

get_atm_skew <- function(k = 0, method = "natural") {
# get slope matrix (day, tau) at fixed k
    myDate = get_date()
    myTau = get_tau() 
    n1 = length(myDate)
    n2 = length(myTau)

    ret = matrix(rep(0, n1*n2), n1, n2)

    ncol = 1
    for(thetau in myTau) {
        df.tau = df %>% filter(tau == thetau)
        date.tau = unique(df.tau[["date"]])
        
        nrow = 1
        for(day in date.tau) {
            theDay.df.tau = df.tau %>% filter(date == day)
            the.log.money = theDay.df.tau[['k']]
            the.IV = theDay.df.tau[['impl_volatility']]
            ss = splinefun(the.log.money, the.IV, method = method)
            ret[nrow, ncol] = ss(k, deriv = 1)
            nrow = nrow + 1
        }
        ncol = ncol + 1
    }
    return(ret)
}

get_atm_convex <- function(k = 0, method = "natural") {
# get convexity matrix (day, tau) at fixed k
    myDate = get_date()
    myTau = get_tau() 
    n1 = length(myDate)
    n2 = length(myTau)

    ret = matrix(rep(0, n1*n2), n1, n2)

    ncol = 1
    for(thetau in myTau) {
        df.tau = df %>% filter(tau == thetau)
        date.tau = unique(df.tau[["date"]])
        
        nrow = 1
        for(day in date.tau) {
            theDay.df.tau = df.tau %>% filter(date == day)
            the.log.money = theDay.df.tau[['k']]
            the.IV = theDay.df.tau[['impl_volatility']]
            ss = splinefun(the.log.money, the.IV, method = method)
            ret[nrow, ncol] = ss(k, deriv = 2)
            nrow = nrow + 1
        }
        ncol = ncol + 1
    }
    return(ret)
}


by.vals <- function(x, probs = c(0.05, 0.05, 0.5, 0.95, 0.95)) {
    r <- quantile(x, probs, na.rm = T)
    names(r) <- c('ymin', 'lower', 'middle', 'upper', 'ymax')
    return(r)
}



get_df_k <- function(k, method = "natural") {
    df.ret = data.frame()
    for(thetau in get_tau()) {
        df.tau = df %>% filter(tau == thetau)
        date.tau = unique(df.tau[["date"]])
        df.tmp = data.frame(date = date.tau, tau = rep(thetau, length(date.tau)), level = rep(0, length(date.tau)),  slope = rep(0, length(date.tau)), convexity = rep(0, length(date.tau)))
        i = 1
        for(day in date.tau) {
            theDay.df.tau = df.tau %>% filter(date == day)
            the.log.money = theDay.df.tau[['k']]
            the.IV = theDay.df.tau[['impl_volatility']]
            ss = splinefun(the.log.money, the.IV, method = method)
            df.tmp[i, 3] = ss(k)
            df.tmp[i, 4] = ss(k, deriv = 1)
            df.tmp[i, 5] = ss(k, deriv = 2)
            i = i+1
        }
        df.tmp = df.tmp %>% filter(level > 0.01) %>% filter(slope > -10) %>% filter(slope < 20) %>% filter(abs(convexity) < 2000)
        df.ret = rbind(df.ret, df.tmp)
    }
    return(df.ret)
} 

get_df_k_term_level_mat <- function(k) {
    mydate = get_date()
    mydf = get_df_k(k)
    n1 = length(mydate)
    n2 = 28
    ret = matrix(rep(0, n1*n2), n1, n2)
    i = 1
    for(day in mydate) {
        df.day = mydf %>% filter(date == day)
        the.tau = df.day[['tau']]
        the.level = df.day[['level']]
        ss = splinefun(the.tau, the.level, method = "natural")
        level.all = ss(2:29)
        ret[i, ] = level.all
        i = i + 1
    }
    return(ret)
}

get_df_k_term_slope_mat <- function(k, tau_seq = 2:29) {
    mydate = get_date()
    mydf = get_df_k(k)
    n1 = length(mydate)
    n2 = length(tau_seq)
    ret = matrix(rep(0, n1*n2), n1, n2)
    i = 1
    #day = mydate[1]
    for(day in mydate) {
        df.day = mydf %>% filter(date == day)
        if(nrow(df.day) == 0) {
            ret[i, ] = rep(NA, n2)
            i = i + 1
            next
        }
        the.tau = df.day[['tau']]
        the.level = df.day[['level']]
        
        ss = splinefun(the.tau, the.level, method = "natural")
        level.all = ss(tau_seq, deriv = 1)
        ret[i, ] = level.all
        i = i + 1
    }
    return(ret)
}

get_sf6 <- function(t, k_seq = seq(-0.15, 0.05, 0.05)) {
    n2 = length(k_seq)
    n1 = length(get_date())
    ret = matrix(rep(0, n1*n2), n1, n2)
    i = 1
    for(k in k_seq) {
        mat.k = get_df_k_term_slope_mat(k, tau_seq = c(t))
        ret[, i] = as.vector(mat.k[, 1])
        i = i + 1
    }
    return(ret)
}

get_df_smile <- function(tau_seq = (2:29)) {
    mydate = get_date()
    n1 = length(mydate)
    ret = matrix(rep(NA, 200000), 100000, 2)
    i = 1
    #day = get_date()[1]
    #t = 14
    for(day in mydate) {
        for(t in tau_seq) {
            df.day.tau = df %>% filter(date == day) %>% filter(tau == t)
            if(nrow(df.day.tau) == 0) {
                next;       
            }
            the.level = df.day.tau[['impl_volatility']]
            the.k = df.day.tau[['k']]
            min_k = the.k[which(the.level == min(the.level))][1]
            ret[i, ] = c(t, min_k)
            i = i + 1
        }
    }
    ret = data.frame(ret)
    names(ret) = c('tau', 'k_min')
    return(ret[complete.cases(ret), ])
}
