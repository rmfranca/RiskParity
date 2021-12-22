library(tidyverse)
library(bizdays)
library(Rblpapi)
library(dplyr)
library(scales)
library(zoo)
library(ExcelFunctionsR)
blpConnect()

#selected assets
tickers <- c("IBOV Index",
             "BZRFIMAB Index",
             "USDBRL CURNCY")

#number of portfolio assets
n <- length(tickers)

#first date for the return series
first_date <- as.Date("2011-01-03")

#get data from Bloomberg
px_last <- bdh(tickers, 
               fields = "PX_LAST",
               start.date = offset(first_date,-2,"Brazil/ANBIMA"),
               end.date = offset(Sys.Date(),-1,"Brazil/ANBIMA")
)

#take all data to a df
prices <- px_last %>%
  reduce(right_join, by = "date") %>%
  arrange(date) %>%
  na.locf

colnames(prices) <- c("Date", "ibov_px","imab_px","usdbrl_px")

#compute returns
returns <- prices %>%
  mutate(ibov_rt = (ibov_px - lag(ibov_px))/lag(ibov_px),
         imab_rt = (imab_px - lag(imab_px))/lag(imab_px),
         usdbrl_rt = (usdbrl_px - lag(usdbrl_px))/lag(usdbrl_px)
  ) %>%
  na.locf

sigma <- cov(returns[5:7])

#weights for the 1/n portfolio
wt_naive <- matrix(c(1/3,1/3,1/3))

#algo that gets the asset weights for risk parity
rp_weights_algo <- function(wt_fun, sigma_fun, rc_fun){
  
  counter <- 0
  convergence_speed <- 0.2
  
  repeat{
    counter <- counter + 1
    wt_fun[1] <- wt_fun[1] - convergence_speed*(rc_fun[1]-1/n)
    wt_fun[2] <- wt_fun[2] - convergence_speed*(rc_fun[2]-1/n)
    wt_fun[3] <- wt_fun[3] - convergence_speed*(rc_fun[3]-1/n)
    
    var_fun <- t(wt_fun) %*% sigma_fun %*% wt_fun
    
    weights_cov_fun <- t(wt_fun) %*% sigma_fun
    
    rc_fun[1] <- weights_cov_fun[1]*wt_fun[1]/ var_fun
    rc_fun[2] <- weights_cov_fun[2]*wt_fun[2]/ var_fun
    rc_fun[3] <- weights_cov_fun[3]*wt_fun[3]/ var_fun
    
     if(counter>200){
       
       convergence_speed <- convergence_speed - 0.01
       counter <- 0
       
     }
    
    if(round(max(rc_fun),10)==round(min(rc_fun),10)){
      
      return(wt_fun)
      
    }
  }
  
  
}

#subsets the returns data-frame to compute a cov matrix to each quarter
#then computed the risk-parity weights using that cov matrix

subset_sigma <- function(returns){
  
  df <- data.frame(Date=character(0),ibov_wt=character(0),imap_wt=numeric(0),usdbrl_wt=integer(0))
  
  for (i in 1:44) {
    
    start_date <- offset(EOMONTH(first_date,-1+3*(i-1)),1,"Brazil/ANBIMA")
    end_date <- EOMONTH(first_date,2+3*(i-1))
    
    subset_returns <- subset(returns,(Date >= start_date) & (Date <= end_date))
    
    cov_sub <- cov(subset_returns[5:7])
    
    weights_cov_sub <- t(wt_naive) %*% cov_sub
    var_sub = t(wt_naive) %*% cov_sub %*% wt_naive
    
    ibov_rc <- weights_cov_sub[1]*wt_naive[1]/var_sub
    imab_rc <- weights_cov_sub[2]*wt_naive[2]/var_sub
    usdbrl_rc <- weights_cov_sub[3]*wt_naive[3]/var_sub
    rc <- c(ibov_rc,imab_rc,usdbrl_rc)
    
    a <- rp_weights_algo(wt_naive,cov_sub,rc)
    
    df2 <- data.frame(start = offset(end_date,1,"Brazil/ANBIMA"), 
                      end = EOMONTH(end_date + 1,2),
                      ibov_wt = a[1],
                      imab_wt = a[2],
                      usdbrl_wt = a[3]
                     )
    
    df <- rbind(df,df2)
    
  }
  
    return(df)                         
  
}

final_rp_weights <- subset_sigma(returns)

#compute cumulative returns along with weights
weights_and_returns <- 
  returns %>% 
    left_join(final_rp_weights, by=c("Date"="start"),all = TRUE) %>%
     na.locf() %>%
  mutate(
        portfolio_rt = (ibov_rt*ibov_wt + imab_rt*imab_wt + usdbrl_rt*usdbrl_wt),
        portfolio_rt_acc = cumprod(1 + portfolio_rt) - 1,
        ibov_rt_acc = cumprod(1 + ibov_rt) - 1,
        imab_rt_acc = cumprod(1 + imab_rt) - 1,
        usdbrl_rt_acc = cumprod(1 + usdbrl_rt) - 1
        )

write.csv(weights_and_returns,'weights_and_returns.csv')

#select returns from the table and rename them dynamically
tidy_rt <- 
  weights_and_returns %>% select(c(Date,
                                   ibov_rt_acc,
                                   imab_rt_acc,
                                   usdbrl_rt_acc,
                                   portfolio_rt_acc))

colnames(tidy_rt) <- c("date",tickers[1],tickers[2],tickers[3],"RP Portfolio")

#pivot the data so it can be best used with ggplot
tidy_rt  <-
  tidy_rt %>%
    pivot_longer(colnames(tidy_rt[,-1]),
                  values_to = "all_rt",
                  names_to = "assets") 

#plotting returns
ggplot(tidy_rt, aes(x = date, y = all_rt, color = assets)) +
  geom_line(size = 1.0) +
  labs(y="Returns", 
       x = "Date", 
       title = "Risk parity - cumulative returns as of 2Q2011") +
  scale_y_continuous(labels = percent_format(accuracy = 0.01),breaks = seq(0, max(tidy_rt$all_rt), by = 0.5)) + 
  scale_color_manual(name = "Assets",
                     values = c("lightseagreen",
                                "indianred1",
                                "darkorange1",
                                "dodgerblue2")
  )


