library(ggplot2)
library(dplyr)
library(tidyr)



################# generate population ######################


# set population characteristics
population_size <- 10000000
# set number of treatment groups
n_treatment_groups <- 4
# treatment - H: there is no difference
treatment_res <- 50
treatment_res_sd <- 5


# generate population data
treatment_res <- round(rnorm(population_size, treatment_res, treatment_res_sd), 0)
population_df <- data.frame(treatment_res)


summary(population_df)  



################# Simulate experiments ######################

sample_size <- 100
n_groups <- 4


# take sample from population, divide into treatment groups
sample_df <- population_df %>%
  sample_n(sample_size) %>%
  mutate(group = as.factor(rep(1:n_groups, each = (sample_size/n_groups)))) 

group_by(sample_df, group) %>%
  summarise(
    count = n(),
    mean = mean(treatment_res, na.rm = TRUE),
    sd = sd(treatment_res, na.rm = TRUE)
  )

# Perform one-way ANOVA
anova_result <- aov(treatment_res ~ group, data = sample_df)
anova_summary <- summary(anova_result)

# Tukey multiple pairwise-comparisons
pairwise_res <- TukeyHSD(anova_result)
tukey_df <- as.data.frame(pairwise_res[[1]])
tukey_df$comparison <- rownames(tukey_df)

# plot pairwise comparisins
ggplot(tukey_df, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Tukey HSD Pairwise Comparisons",
    y = "Mean Difference (with 95% CI)",
    x = "Comparison"
  ) +
  theme_minimal()







# Function than runs an experiment (one-way ANOVA). Takes a random sample from the population each time it's called.
random_sample_experiment <- function(n_i, sample_size, n_groups) {

  # each iteration represents an experiment that takes a random sample 
  # (of the set sample size) from the population
  for (i in 1:n_i) {
    
    # take sample from population, divide into treatment groups
    sample_df <- population_df %>%
      sample_n(sample_size) %>%
      mutate(group = as.factor(rep(1:n_groups, each = (sample_size/n_groups)))) 
  
    # Perform one-way ANOVA
    anova_result <- aov(treatment_res ~ group, data = sample_df)
    anova_summary <- summary(anova_result)
    # Extract F-value and p-value
    anova_f_value <- anova_summary[[1]]["group", "F value"]
    anova_p_value <- anova_summary[[1]]["group", "Pr(>F)"]
  
    # is result statistically significant?
    sig <- ifelse(anova_p_value <= 0.05, 1, 0)
  
    # add results to df
    res_df <- data.frame(experiment = i,
                         p.value = anova_p_value, 
                         F_value = anova_f_value, 
                         sig = as.factor(sig),
                         sample_size = sample_size,
                         n_groups = n_groups)
  
    # update global results_df
    results_df <<- rbind(results_df, res_df)
  }

}




############## run the dance #####################

# run experiment. Takes number of experiments (n_i), sample size (sample_size) and number of treatment groups
random_sample_experiment(100, 60, 6)


# Show P value dance
p_plot <- ggplot(data=results_df, aes(y=p.value, x=experiment, color=sig)) +
  geom_hline(yintercept=0.05, linetype="dotted", color="black", linewidth = 1) +
  geom_point() +
  labs(
    title = "Dance of the p-values of a one-way ANOVA",
    subtitle = paste("Sample size: ", results_df$sample_size, "; number of groups: ", results_df$n_groups),
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


