

flip_function <- function() {
  coin_flip <- 0
  i<-0
  price <- 0
  
  while (coin_flip == 0) {
    coin_flip <- round(runif(1), digits = 0)
    #print(coin_flip)
  
    i <- i+1
    price <- 2^i
    #print(price)
  }
  return(price-10)
}


total_price <- 0
price_df <- data.frame()

for (x in 1:1000) {
  price <- flip_function()
  total_price <- total_price+price
  
  temp_df <- data.frame(x, price)
  price_df <- rbind(price_df, temp_df)
}


# Libraries
library(ggplot2)
library(dplyr)

ggplot(price_df, aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) 

