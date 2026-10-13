library(ggplot2)
library(MASS)

# Set a seed for reproducibility
set.seed(123) 

# set parameters
n_points <- 100
point_size <- rnorm(n_points, mean = 4, sd = 1)
point_color <- rnorm(n_points, mean = 0, sd = 1) 

# generate data
bvnData <- mvrnorm(
  n = n_points, 
  mu = c(0.1, 0.1), 
  Sigma = matrix(c(0.3, 0.18, 0.18, 0.3), 
                 ncol = 2)
)

# create df
bvnData <- as.data.frame(bvnData)
bvnData$point_size <- point_size
bvnData$point_color <- point_color



# visualize scatterplot
ggplot(bvnData, aes(V1, V2, color = point_color))+
  geom_point(size = point_size) +
  scale_color_viridis_c() +
  # scale_color_gradient2(low="red", mid="gray", high="blue") +
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
       y = NULL)


############## calculate outliers ###########

# point_size
boxplot.stats(bvnData$point_size)$out   #IQR
lower_bound <- median(bvnData$point_size) - 3 * mad(bvnData$point_size, constant = 1) # Hampel filter (MAD)
upper_bound <- median(bvnData$point_size) + 3 * mad(bvnData$point_size, constant = 1)
outlier_ind <- which(bvnData$point_size < lower_bound | bvnData$point_size > upper_bound)
outlier_ind # outlying row numbers according to hampel filter
bvnData$z_point_size <- scale(bvnData$point_size)  # Z score
summary(bvnData$z_point_size)
which(bvnData$z_point_size < -3 | bvnData$z_point_size < -3)   # < -3 or > 3 is considered as extremely rare


# point_color
boxplot.stats(bvnData$point_color)$out  #IQR
lower_bound <- median(bvnData$point_color) - 3 * mad(bvnData$point_color, constant = 1) # Hampel filter (MAD)
upper_bound <- median(bvnData$point_color) + 3 * mad(bvnData$point_color, constant = 1)
outlier_ind <- which(bvnData$point_color < lower_bound | bvnData$point_color > upper_bound)
outlier_ind # outlying row numbers according to hampel filter
bvnData$z_point_color <- scale(bvnData$point_color)  # Z score
summary(bvnData$z_point_color)
which(bvnData$z_point_color < -3 | bvnData$z_point_color < -3)   # < -3 or > 3 is considered as extremely rare


# positional outlier BVN
df <- subset(bvnData, select = c(V1, V2))
df$md <- mahalanobis(df, colMeans(df), cov(df))
df$md_p <- pchisq(df$md, df=1, lower.tail=FALSE)
df$md_out <- ifelse(df$md_p<0.001, TRUE, FALSE)


# scatter without MD outlier
ggplot(df, aes(V1, V2, color=md_out))+
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
       y = NULL)





