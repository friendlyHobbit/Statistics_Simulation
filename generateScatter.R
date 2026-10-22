library(ggplot2)
library(MASS)
library(brms)
#install.packages("ggExtra")
library(ggExtra)

# Set a seed for reproducibility
#set.seed(123) 


############## Bivariate normal scatterplot ############## 

# set parameters
n_points <- 300
point_size <- rnorm(n_points, mean = 4, sd = 1)
point_color <- rnorm(n_points, mean = 6, sd = 1) 

# generate data
bvnData <- mvrnorm(
  n = n_points, 
  mu = c(1, 1), 
  Sigma = matrix(c(0.3, 0.18, 0.18, 0.3), 
                 ncol = 2)
)

# create df
bvnData <- as.data.frame(bvnData)
bvnData$point_size <- point_size
bvnData$point_color <- point_color



# visualize scatterplot
ggplot(bvnData, aes(V1, V2, color = point_color))+
  geom_point(size = point_size, alpha = 0.9) +
  scale_color_gradient2(low = "#817e7f", mid = "#bf4073", high = "#ff0066", midpoint = mean(point_color), space = "Lab",) +
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
        axis.text.y=element_blank()) 

simple_plot +
  labs(x = "X",
       y = "Y")

simple_plot +
  labs(x = "N Trees",
       y = "N Lawers")

simple +
  labs(x = "N Trees",
       y = "N Lawers")



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




############## skewed plot ###########

bvnData$Vskew <- rskew_normal(n_points, mu = 10, sigma = 10, alpha = 5, xi = NULL, omega = NULL)

ggplot(bvnData, aes(x = Vskew)) +
  geom_density()

# visualize scatterplot
ggplot(bvnData, aes(V1, Vskew))+
  geom_point() +
  scale_color_viridis_c() +
  #scale_color_continuous(palette = c("#FEE0D2", "#FC9272", "#DE2D26"), transform = "log") +
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



############## two cluster scatterplot ############## 

# set parameters
n_points <- 200
set_sd <- 1.5

# cluster 1
V1 <- rnorm(n_points*0.5, mean = 4, sd = set_sd)
V2 <- rnorm(n_points*0.5, mean = 4, sd = set_sd) 
df1 <- data.frame(V1, V2)

# cluster 2
V1 <- rnorm(n_points*0.5, mean = 8, sd = set_sd)
V2 <- rnorm(n_points*0.5, mean = 8, sd = set_sd) 
df2 <- data.frame(V1, V2)

df <- rbind(df1, df2)

two_clusters <- ggplot(df, aes(V1, V2))+
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

# regular
two_clusters + 
  geom_point() 


# regression 95 CI band
two_clusters + 
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = TRUE,
              level = .95,
              color = NA)

# heatmap
two_clusters + 
  geom_density_2d_filled() +
  scale_fill_manual(
    values = colorRampPalette(c("#c9c8c5", "#fee890"))(10)
  )+
  geom_point(color = "black") 

# smoothend
two_clusters +
  stat_density_2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradient(low = "#c9c8c5", high = "#fee890") +
  geom_point(alpha = 1, color = "black") 

# contours  
two_clusters +
  geom_density_2d() +
  geom_point() 

# elipse
two_clusters +
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, type = "norm") +
  geom_point(alpha = 1) 




############## three cluster scatterplot ############## 

# set parameters
n_points <- 300
c_sd <- 0.8

# cluster 1
V1 <- rnorm(n_points*0.3, mean = 4, sd = c_sd)
V2 <- rnorm(n_points*0.3, mean = 4, sd = c_sd) 
df1 <- data.frame(V1, V2)

# cluster 2
V1 <- rnorm(n_points*0.3, mean = 8, sd = c_sd)
V2 <- rnorm(n_points*0.3, mean = 8, sd = c_sd) 
df2 <- data.frame(V1, V2)

# cluster 3
V1 <- rnorm(n_points*0.3, mean = 4, sd = c_sd)
V2 <- rnorm(n_points*0.3, mean = 8, sd = c_sd) 
df3 <- data.frame(V1, V2)

df <- rbind(df1, df2, df3)

ggplot(df, aes(V1, V2))+
  geom_point() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 





