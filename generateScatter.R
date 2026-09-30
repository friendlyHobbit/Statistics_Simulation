library(ggplot2)
library(MASS)


# generate data
x = rnorm(1000, mean=0, sd=1)
y = rnorm(1000, mean=0, sd=1)


# add to df
df = data.frame(x, y)


ggplot(df, aes(x, y))+
  geom_point() 



set.seed(123) # Set a seed for reproducibility
bvnData <- mvrnorm(
  n = 1000, 
  mu = c(1, 1), 
  Sigma = matrix(c(1, 0.2, 0.2, 1), 
                 ncol = 2)
)
bvnData <- as.data.frame(bvnData)

ggplot(bvnData, aes(V1, V2))+
  geom_point()



