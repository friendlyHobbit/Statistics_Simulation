library(ggplot2)
library(dplyr)
library(tidyr)


################# generate population ######################

# generate Data
IQ <- round(rnorm(1000000, 100, 15), 0)
sex <- sample(0:1, 1000000, replace=TRUE)

# merge in df
population_df <- data.frame(sex, IQ)


# visualize
ggplot(data=population_df, aes(x=IQ, group=sex, fill=sex)) + geom_density(adjust=1.5, alpha=.4) 



################# Take random sample #######################
i=0
res$p.value=1

while(res$p.value > 0.05){
  pop_sample <- sample_n(population_df, 50)
  # t test
  res <- t.test(IQ ~ sex, data = pop_sample, var.equal = TRUE)
  print(res$p.value)
}

# visualize
ggplot(data=pop_sample, aes(x=IQ, group=sex, fill=sex)) + geom_density(adjust=1.5, alpha=.4) 


