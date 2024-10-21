library(ggplot2)
library(dplyr)
library(tidyr)


################# generate population ######################

# set population characteristics
population_size <- 10000000
# height - H: there is a difference between men and women (men are taller than women)
population_group0_height <- 175
population_group1_height <- 165
population_height <- (population_group0_height - population_group1_height) 
population_height_sd <- 7
# IQ - H: there is no difference between men and women
population_group0_IQ <- 100
population_group1_IQ <- 100
population_IQ <- population_group0_IQ - population_group0_IQ
population_IQ_sd <- 15

# generate Data women
height <- round(rnorm((population_size/2), population_group1_height, population_height_sd), 0)
IQ <- round(rnorm((population_size/2), population_group1_IQ, population_IQ_sd), 0)
sex <- 1
women_df <- data.frame(sex, height, IQ)

# generate data men
height <- round(rnorm((population_size/2), population_group0_height, population_height_sd), 0)
IQ <- round(rnorm((population_size/2), population_group0_IQ, population_IQ_sd), 0)
sex <- 0
men_df <- data.frame(sex, height, IQ)


# merge in df
population_df <- rbind(women_df, men_df)
population_df$sex <- as.factor(population_df$sex)


# visualize the population height
ggplot(data=population_df, aes(x=height, group=sex, fill=sex)) + geom_density(adjust=1.5, alpha=.4) 

# visualize the population IQ
ggplot(data=population_df, aes(x=IQ, group=sex, fill=sex)) + geom_density(adjust=1.5, alpha=.4) 




################# Set up simulation ######################

# create DF to hold results from all iterations
results_df <- data.frame() 


# Function than runs an experiment (t-test). Takes a random sample from the population each time it's called.
random_sample_experiment <- function(dep_var, pop_mean, n_i, sample_size) {

  # each iteration represents an experiment that takes a random sample 
  # (of the set sample size) from the population
  for (i in 1:n_i) {
    # get sample
    sample_women <- sample_n(women_df, (sample_size*0.5))
    sample_men <- sample_n(men_df, (sample_size*0.5))
    # merge sample
    sample <- rbind(sample_men, sample_women)
    
    # t test
    formula <- as.formula(paste(dep_var, "~ sex"))
    res <- t.test(formula, data = sample, var.equal = TRUE, paired = FALSE)
    
    # does CI include true mean?
    true_CI <- ifelse(res$conf.int[1] < pop_mean & pop_mean < res$conf.int[2], 1, 0)
    # is result statistically significant?
    sig <- ifelse(res$p.value <= 0.05, 1, 0)

    # add results to df
    res_df <- data.frame(experiment = i, 
                         true_mean = pop_mean, 
                         p.value = res$p.value, 
                         df = res$stderr, 
                         t.value = res$statistic, 
                         conf.low = res$conf.int[1], 
                         conf.high = res$conf.int[2], 
                         mean_men = res$estimate[1], 
                         mean_women = res$estimate[2], 
                         mean_diff = res$estimate[1] - res$estimate[2], 
                         trueCI = as.factor(true_CI),
                         sig = as.factor(sig),
                         sample_size = sample_size,
                         dep_var = dep_var
                         )
    
    # update global results_df
    results_df <<- rbind(results_df, res_df)
    
    # show results
    #print(res)
  }
}


############## run the dance of the CIs #####################

# This runs the simulation of the experiments N times
# enter the dependent variable, population mean, number of experiments and sample size per experiment
random_sample_experiment("IQ", population_IQ,  100, 20)
#random_sample_experiment("height", population_height,  100, 20)


# visualize the dance of the p-values
p_plot <- ggplot(data=results_df, aes(y=p.value, x=experiment, color=sig)) +
  geom_hline(yintercept=0.05, linetype="dotted", color="black", linewidth = 1) +
  geom_point() +
  labs(
    title = "Dance of the p-values of a t-test (unpaired)",
    subtitle = paste("Dependent variable: ", results_df$dep_var, " ; Sample size: ", results_df$sample_size),
    color = "significant at 0.05?",
  ) +
  xlab("experiment number") +
  scale_color_discrete(labels=c('No', 'Yes'))
p_plot

# visualize density of p values from all samples
ggplot(data=results_df, aes(x=p.value)) + geom_density(adjust=1.5, alpha=.4) 



# visualize the dance of the CIs
ci_plot <- ggplot(data=results_df, aes(y=mean_diff, x=experiment, color=trueCI)) + 
  geom_hline(yintercept=results_df$true_mean, linetype="dotted", color="black", linewidth = 1) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.5) +
  geom_point() 
print(ci_plot)

# visualize density of means received from all samples
ggplot(data=results_df, aes(x=mean_diff)) + geom_density(adjust=1.5, alpha=.4) 







