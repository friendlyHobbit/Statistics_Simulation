library(ggplot2)
library(MASS)
library(brms)
#install.packages("ggExtra")
library(ggExtra)
# install.packages("bayestestR")
#library(bayestestR)
#install.packages("EnvStats")
#library(EnvStats)

# Set a seed for reproducibility
#set.seed(123) 


############## Bivariate normal scatterplot ############## 


GenScatter <- function(n_points, variance, covariance){
  # generate data
  bvnData <- mvrnorm(
    n = n_points, 
    mu = c(1, 1), 
    Sigma = matrix(c(variance, covariance, covariance, variance), 
                   ncol = 2)
  )
  
  # create df
  bvnData <- as.data.frame(bvnData)
  
  # visualize scatterplot - neutral
  simple_plot <- ggplot(bvnData, aes(V1, V2))+
    geom_point(alpha = 0.9) +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank())  +   
    labs(x = "X",
         y = "Y")
  
  return(simple_plot)
}


# set parameters
n <- 400
v <- 0.1
r <- 0.6

# calc covariance
cov <- r * sqrt(v) * sqrt(v)
print(cov)
print(GenScatter(n, v, cov))


for (i in 1:5){
  v <- i
  # calc covariance
  cov <- r * sqrt(v) * sqrt(v)
  print(cov)
  
  print(GenScatter(n, v, cov))
}



############## 2 cluster scatterplot ############## 


GenScatter <- function(n_points, set_sd, set_m1, set_m2){
  
  n_clusters <- 2
  
  # data
  x <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  df1 <- data.frame(x,y)
  
  x <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  df2 <- data.frame(x, y)
  
  clusterData <- rbind(df1, df2)
  
  # visualize scatterplot - neutral
  simple_plot <- ggplot(clusterData, aes(x, y))+
    geom_point(alpha = 0.9) +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank()) +
    labs(x = "X",
         y = "Y")
  
  return(simple_plot)
}

n_p <- 400
sd <- 1
m1 <- 0
m2 <- 4

for (i in 1:5){
  print(i)
  print(GenScatter(n_p, sd, m1, i))
  }



############## 3 cluster scatterplot ############## 

GenScatter <- function(n_points, set_sd, set_m1, set_m2){
  n_clusters <- 3
  
  # data
  x <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  df1 <- data.frame(x,y)
  
  x <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  df2 <- data.frame(x, y)
  
  x <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  df3 <- data.frame(x, y)
  
  clusterData <- rbind(df1, df2, df3)
  
  # visualize scatterplot - neutral
  simple_plot <- ggplot(clusterData, aes(x, y))+
    geom_point(alpha = 0.9) +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank())  + 
    labs(x = "X",
         y = "Y")
  
  return(simple_plot)
}


n_p <- 400
sd <- 1
m1 <- 0
m2 <- 4

for (i in 1:5){
  print(i*0.2+0.4)
  print(GenScatter(i*100, sd, m1, m2))
}




############## 4 cluster scatterplot ############## 

GenScatter <- function(n_points, set_sd, set_m1, set_m2){
  n_clusters <- 4
  
  # data
  x <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  df1 <- data.frame(x,y)
  
  x <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  df2 <- data.frame(x, y)
  
  x <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  df3 <- data.frame(x, y)
  
  x <- rnorm(n_points/n_clusters, mean = set_m2, sd = set_sd)
  y <- rnorm(n_points/n_clusters, mean = set_m1, sd = set_sd)
  df4 <- data.frame(x, y)
  
  
  clusterData <- rbind(df1, df2, df3, df4)
  
  # visualize scatterplot - neutral
  simple_plot <- ggplot(clusterData, aes(x, y))+
    geom_point(alpha = 0.9) +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank())  +
    labs(x = "X",
         y = "Y")
  
  return(simple_plot)
}


n_p <- 400
sd <- 1
m1 <- 0
m2 <- 4

for (i in 1:5){
  print(i*0.2+0.4)
  print(GenScatter(n_p, sd, m1, i))
}



############## two cluster normal scatterplot ############## 

GenScatter <- function(n_points, Sigma, mu2){
  # cluster 1
  cluster1 <- mvrnorm(
    n = n_points*0.5,
    mu = c(0, 0),
    Sigma = Sigma,
    empirical = TRUE
  )
  
  # cluster 2
  cluster2 <- mvrnorm(
    n = n_points*0.5,
    mu = mu2,
    Sigma = Sigma,
    empirical = TRUE
  )
  
  # combine
  bvnData <- rbind(cluster1, cluster2) |>
    as.data.frame()
  
  print(cor(cluster1))
  print(cor(cluster2))
  print(cor(bvnData$V1, bvnData$V2))
  
  # plot
  plot <- ggplot(bvnData, aes(V1, V2)) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(x = "X", y = "Y") 
  
  return(plot)
}


# parameters
n_points <- 300
# correlation of 0.6 for each cluster
Sigma6 <- matrix(c(0.3, 0.18, 0.18, 0.3), ncol = 2)
# correlation of 0.4 for each cluster
Sigma4 <- matrix(c(0.3, 0.12, 0.12, 0.3), ncol = 2)

mu2.1 <- c(1.5, 1.5)  # overlapping
mu2.2 <- c(2, 2)      # little separation
mu2.3 <- c(3, 3)      # moderately separated
mu2.4 <- c(4, 4)      # clearly separated


print(GenScatter(n_points, Sigma4, mu2.3) + geom_point(alpha = 1))


for (i in 1:4){
  temp_plot <- GenScatter(n_points, Sigma4, mu2.1)
  # simple
  print(temp_plot + geom_point(alpha = 1))
  # regression 95 CI band
  print(temp_plot + geom_point(alpha = 1) + 
          stat_smooth(method = "lm",
                      formula = y ~ x,
                      geom = "smooth",
                      se = TRUE,
                      level = .95,
                      color = NA))
  # smoothend
  #print(temp_plot +
  #        stat_density_2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
  #        scale_fill_viridis_c() +
  #        geom_point(alpha = 1, color = "white") +
  #        guides(fill = "none") )
  # contours 
  print(temp_plot +
          #geom_density_2d() +
          geom_point() )
  # elipse
  #print(temp_plot +
  #        stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  #        geom_point(alpha = 1) )
  #
}





############## Bivariate exponential ############## 

# set parameters
n_points <- 600

# generate data
x <- rexp(n_points, rate = .1)
y <- rexp(n_points, rate = .1)

# create df
Data <- data.frame(x,y)

# visualize scatterplot
simple_plot <- ggplot(Data, aes(x, y))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")



############## Bivariate lognormal ############## 

# set parameters
n_points <- 600

# generate data
x <- rlnorm(n_points, meanlog = 0, sdlog = 0.5)
y <- rlnorm(n_points, meanlog = 0, sdlog = 0.5)
# dlnorm, plnorm, qlnorm, rlnorm

# create df
Data <- data.frame(x,y)

# visualize scatterplot
simple_plot <- ggplot(Data, aes(x, y))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")


############## Bivariate gamma ############## 

# generate data points
x <- rgamma(n_points, shape = 10, rate = 10)
y <- rgamma(n_points, shape = 10, rate = 10)

# create df
df <- data.frame(x,y)

# visualize scatterplot
simple_plot <- ggplot(df, aes(x, y))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")


############## Bivariate beta ############## 

# generate data
x <- rbeta(n_points, shape1 = 1, shape2 = 1, ncp = 0)
y <- rbeta(n_points, shape1 = 10, shape2 = 10, ncp = 2)

#plot(y)

# create df
Data <- data.frame(x,y)

# visualize scatterplot
simple_plot <- ggplot(Data, aes(x, y))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")




############## Bivariate uniform ############## 

# set parameters
n_points <- 600

# generate data
x <- runif(n_points, 0, 10)
y <- runif(n_points, 0, 10)

# create df
Data <- data.frame(x,y)

# visualize scatterplot
simple_plot <- ggplot(Data, aes(x, y))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")



############## Bivariate pareto ############## 

# generate data
x <- rpareto(n_points, location = 5, shape = 100)
y <- rpareto(n_points, location = 2, shape = 100)

# create df
Data <- data.frame(x,y)

# visualize scatterplot
simple_plot <- ggplot(Data, aes(x, y))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")




############## summ stats visualizations ###########

# regression line
ggplot(bvnData, aes(V1, V2))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y") +                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = FALSE)

# regression line + 95 CI band
ggplot(bvnData, aes(V1, V2))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y") +                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = TRUE,
              level = .95)

# regression 95 CI band
ggplot(bvnData, aes(V1, V2))+
  geom_point(alpha = 0.9) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y") +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = TRUE,
              level = .95,
              color = NA)

# heatmap
ggplot(bvnData, aes(V1, V2)) +
  geom_density_2d_filled() +
  geom_point(color = "white") +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")

ggplot(bvnData, aes(V1, V2)) +
  geom_density_2d() +
  geom_point(alpha = 1) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")

ggplot(bvnData, aes(V1, V2)) +
  stat_density_2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
  scale_fill_viridis_c() +
  geom_point(alpha = 1, color = "white") +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")


# CI ellipse (levels from https://projecteuclid.org/journalArticle/Download?urlId=10.1214%2F12-STS402)
ggplot(bvnData, aes(V1, V2)) +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.68, type = "norm") +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.40, type = "norm") +
  geom_point(alpha = 1) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")

ggplot(bvnData, aes(V1, V2)) +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  geom_point(alpha = 1) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")

ggplot(bvnData, aes(V1, V2)) +
  stat_ellipse(level = 0.95, type = "norm") +
  geom_point(alpha = 1) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")


# marginal plots
scatter <- ggplot(bvnData, aes(V1, V2)) +
  geom_point(alpha = 1) +
  theme(panel.border =  element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "X",
       y = "Y")

ggMarginal(scatter,  type="boxplot")
ggMarginal(scatter, type="density")
ggMarginal(scatter, type="histogram")



############## calculate outliers ###########

# IQR outlier - point_size
iqr_size <- boxplot.stats(bvnData$point_size)$out   #IQR
Q1 <- quantile(iqr_size, probs=c(.25))
Q3 <- quantile(iqr_size, probs=c(.75))
upper_iqr <- Q3 + (1.5 * iqr_size)
lower_iqr <- Q1 - (1.5 * iqr_size)
bvnData$iqr_out_size <- bvnData$point_size < lower_iqr | bvnData$point_size > upper_iqr

ggplot(bvnData, aes(V1, V2, color=iqr_out_size))+
  geom_point(size=point_size) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "IQR outlier - size",
       color="outlier")

# MAD outlier - point size
lower_bound <- median(bvnData$point_size) - 3 * mad(bvnData$point_size, constant = 1) # Hampel filter (MAD)
upper_bound <- median(bvnData$point_size) + 3 * mad(bvnData$point_size, constant = 1)
bvnData$mad_out_size <- bvnData$point_size < lower_bound | bvnData$point_size > upper_bound

ggplot(bvnData, aes(V1, V2, color=mad_out_size))+
  geom_point(size=point_size) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "MAD outlier - size",
       color="outlier")

# z score outlier - point size
z_point_size <- scale(bvnData$point_size)  # Z score
bvnData$z_out_size <-  z_point_size < -3 | z_point_size > 3   # < -3 or > 3 is considered as extremely rare

ggplot(bvnData, aes(V1, V2, color=z_out_size))+
  geom_point(size=point_size) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "Z outlier - size",
       color="outlier")



# IQR outlier - color
iqr_color <- boxplot.stats(bvnData$point_color)$out  #IQR
Q1 <- quantile(iqr_color, probs=c(.25))
Q3 <- quantile(iqr_color, probs=c(.75))
upper_iqr <- Q3 + (1.5 * iqr_color)
lower_iqr <- Q1 - (1.5 * iqr_color)
bvnData$iqr_out_color <- bvnData$point_color < lower_iqr | bvnData$point_color > upper_iqr

ggplot(bvnData, aes(V1, V2, color=iqr_out_color))+
  geom_point() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "IQR outlier - color",
       color="outlier")

# z-score outlier - color
z_point_color <- scale(bvnData$point_color)  # Z score
summary(z_point_color)
bvnData$z_out_color <- z_point_color < -3 | z_point_color > 3   # < -3 or > 3 is considered as extremely rare

ggplot(bvnData, aes(V1, V2, color=z_out_color))+
  geom_point() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "z-scored outlier - color",
       color="outlier")

# MAD outlier - color
lower_bound <- median(bvnData$point_color) - 3 * mad(bvnData$point_color, constant = 1) # Hampel filter (MAD)
upper_bound <- median(bvnData$point_color) + 3 * mad(bvnData$point_color, constant = 1)
bvnData$mad_out_color <- bvnData$point_color < lower_bound | bvnData$point_color > upper_bound

ggplot(bvnData, aes(V1, V2, color=mad_out_color))+
  geom_point() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "MAD outlier - color",
       color="outlier")



# positional outlier BVN
df <- subset(bvnData, select = c(V1, V2))
df$md <- mahalanobis(df, colMeans(df), cov(df))
df$md_p <- pchisq(df$md, df=1, lower.tail=FALSE)
bvnData$md_out <- ifelse(df$md_p<0.001, TRUE, FALSE)

# scatter with MD outlier
ggplot(bvnData, aes(V1, V2, color=md_out))+
  geom_point() +
  #stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, color="black") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "mahalanobis distance outlier (positional)",
       color="outlier")






########## Iris dataset ########

iris

# Petal.Length X Petal.Width
iris_plot <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

# scatter simple
iris_plot + geom_point()

# regression 95 CI band
iris_plot + 
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = TRUE,
              level = .95,
              color = NA)

# elipse
iris_plot +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  geom_point(alpha = 1) 


# heatmap
iris_plot + 
  geom_density_2d_filled() +
  #scale_fill_manual(values = colorRampPalette(c("#c9c8c5", "#fee890"))(12))+
  geom_point(color = "white") +
  guides(fill = "none")

# smoothend
iris_plot +
        stat_density_2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
        scale_fill_viridis_c() +
        geom_point(alpha = 1, color = "white") +
        guides(fill = "none") +
  guides(fill = "none")

# contours  
iris_plot +
  geom_density_2d() +
  geom_point() 



# Petal.Length X Petal.Width include species
iris_plot2 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

# scatter simple
iris_plot2 + geom_point()

# regression 95 CI band
iris_plot2 + 
  geom_point() +
  stat_smooth(method = "lm",
              fullrange=FALSE,
              se = TRUE,
              level = .95)

# elipse
iris_plot2 +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  geom_point(alpha = 1) 




########## Simpson's Paradox ########
# https://easystats.github.io/bayestestR/reference/simulate_simpson.html

sim_data <- simulate_simpson(
  n = 100,
  r = -0.5,
  groups = 3,
  difference = 1.6,
  group_prefix = "G_"
)

plot <- ggplot(sim_data, aes(V1, V2)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

plot + geom_point()

# regression 95 CI band
plot + 
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = TRUE,
              level = .95,
              color = NA)

# heatmap
plot + 
  geom_density_2d_filled() +
  geom_point(color = "white") +
  guides(fill = "none")

# smoothend
plot +
  stat_density_2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
  scale_fill_viridis_c() +
  geom_point(alpha = 1, color = "white") +
  guides(fill = "none") +
  guides(fill = "none")

# contours  
plot +
  geom_density_2d() +
  geom_point() 

plot +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  geom_point()

# grouped plot
grouped_plot <- ggplot(sim_data, aes(V1, V2, color= Group)) +
  scale_color_manual(values = c(G_1 = "black", G_2 = "black", G_3 = "black"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

grouped_plot + geom_point()

# regression 95 CI band
grouped_plot + 
  geom_point() +
  stat_smooth(method = "lm",
              fullrange=FALSE,
              se = TRUE,
              level = .95)

# elipse
grouped_plot +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  geom_point(alpha = 1) 



