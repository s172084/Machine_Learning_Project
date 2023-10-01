
# -- Libraries
library(MASS)
library(ggplot2)
library(GGally)
library(tidyr)
library(data.table)
library(mltools)
library(reshape)
library(reshape2)
library(rmarkdown)
library(tidyverse)
library(magrittr)

# setwd("C:/Users/Alessandro/Desktop/PIETRO/Università/3_Machine Learning and Data Mining/Exercises/02450Toolbox_R")
# render("C:/Users/Alessandro/Desktop/PIETRO/Università/3_Machine Learning and Data Mining/Project/Machine_Learning_Project/Report.Rmd")

# -----------------------------------------------------------------------------------------------------------
# -------------------------------- The Chosen Dataset  ------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------

# -- Diamonds
data(diamonds)
?diamonds
str(diamonds)

# -----------------------------------------------------------------------------------------------------------
# -------------------------------- Question 1 and Question 2 ------------------------------------------------
# -----------------------------------------------------------------------------------------------------------

# 1. glimpse an overview of the variables
glimpse(diamonds)

# 2. look at the structure of the dataset
str(diamonds)

# 3. Summary Statistics about the dataset. 
summary(diamonds)

# 4. Descriptive Analysis
perform_descriptive_analysis <- function(the_frame){
  
  a = dim(the_frame)
  b = names(the_frame)
  c = data.frame(head(the_frame))
  
  cat("There are this many rows and columns:", a, "\n")
  cat("The column names are:", b, "\n")
  cat("The first part of the table looks like:\n")
  return(c)
}

perform_descriptive_analysis(diamonds)

# 5. Are there any missing data? 
are_there_NAs <- function(D){
  # check if there are any NAs in the dataframe
  output <- apply(D, 2, function(x) any(is.na(x)))
  return(output)
}

# call the function on the dataset. 
are_there_NAs(diamonds)


# 6. Summary Statistics Function
calculate_summary_statistics <- function(group) {
  
  hist(group, 
       prob = TRUE, 
       col = viridisLite::viridis(8, alpha = 0.8))
  
  m <- mean(group)
  cat("\nMEAN: The sample mean value is", m , "\nThis is the center of the data.\n")
  
  med <- median(group)
  cat("\nMEDIAN: The sample median value is", med,"\n\n")
  
  v <- var(group)
  cat("VARIANCE: The variance is", round(v,4), "\n\n")
  
  s <- sd(group)
  cat("STANDARD DEVIATION: The standard deviation is", round(s,3) ,"\nThis is the square root of the variance.\n\n")
  
  q <- quantile(group, type = 2)
  cat("QUARTILES: The five quartiles at \n 0%   25 %   50 %   75 %   100 % are:\n",  q, "\n\n")
  
  d <- quantile(group, c(0.2, 0.4, 0.6, 0.8) , type = 2)
  cat("The Percentiles:\n
      20%,  40%,   60% ,  80% \n
      ", d, "\n\n")
  
  n <- length(group)
  cat("SAMPLE SIZE: The sample size is ", n , "\n\n")
  
  sem <- sd(group) / sqrt(n)
  cat("STANDARD ERROR OF THE MEAN: ", round(sem ,4), "\n\n")
  
  that_bloody_iqr <- IQR(group, type = 2)
  cat("The interquartile range is :", that_bloody_iqr, "\n\n")
  
  # Empirical density
  hist(group,
       prob = TRUE, 
       main = "Histogram of the group",
       family = "Avenir",
       breaks = 10,
       col = viridisLite::viridis(7, alpha = 0.8))
  
  lines(density(group), # density plot
        lwd = 2, # thickness of line
        col = "cyan")
  
}

# 7. Quickly show me summary statistics
calculate_summary_statistics(diamonds$carat)
calculate_summary_statistics(diamonds$depth)
calculate_summary_statistics(diamonds$x)
calculate_summary_statistics(diamonds$y)
calculate_summary_statistics(diamonds$z)
calculate_summary_statistics(diamonds$table)
calculate_summary_statistics(diamonds$price)

# 8. The Correlation 
cor(diamonds$x, diamonds$y)
cor(diamonds$y, diamonds$z)
cor(diamonds$x, diamonds$z)
cor(diamonds$carat, diamonds$x)


# 9.Brief Plots
# Length and Width, Separated by Cut... 
fig1 <- ggplot(data = diamonds,
       mapping = aes(x = x, y = y, fill = cut))+
  geom_point(mapping = aes(fill=color), pch=21)+
  theme_minimal()+
  facet_wrap(vars(cut))+
  labs(title = "The Relationship between Diamond Length and Width",
       subtitle = "Separated by Cut",
       caption = "Scatterplot",
       x = "x",
       y = "y")

# View the plot >
fig1

# Save the Image. 
# save for the report. 
ggsave("Images/figure1.png", fig1, width = 5, height = 5)
plot.new()

# 10. Relationship Between Carat and Price, Separated by colour. 
fig2<- ggplot(data = diamonds,
       mapping = aes(x = carat, y = price, fill = color))+
  geom_point(mapping = aes(fill=color), pch=21)+
  theme_minimal()+
  facet_wrap(vars(color))+
  labs(title = "Carat vs. Price",
       subtitle = "Separated by Colour",
       caption = "Line Plot",
       x = "carat",
       y = "price")

# View the plot >
fig2

# Save the Image. 
# save for the report. 
ggsave("Images/figure2.png", fig2, width = 5, height = 5)
plot.new()

# 11.Relationship Between Carat and Price, Separated by Clarity.  
fig3<- ggplot(data = diamonds,
       mapping = aes(x = carat, y = price, col = clarity))+
  geom_point(mapping = aes(fill=cut), colour="black", pch=21)+
  geom_line()+
  theme_minimal()+
  facet_wrap(vars(clarity))+
  labs(title = "Carat vs. Price",
       subtitle = "Separated by Clarity",
       caption = "Line Plot",
       x = "carat",
       y = "price")

# View the plot >
fig3
# Save the Image. 
# save for the report. 
ggsave("Images/figure3.png", fig3, width = 5, height = 5)
plot.new()

# -----------------------------------------------------------------------------------------------------------
# -------------------------------- Question 3 and Question 4 ------------------------------------------------
# -----------------------------------------------------------------------------------------------------------


# One-hot-encode for regression of price ###
# Maybe it is not necessary to transform in one-hot-encode because all the 3 factor
# attributes are ordered (we know that cut "fair" is worse than cat "good")

# D <- as.data.frame(diamonds[-c(2:4)]) # drop "price", that is the target variable, and nominal variables

# cut_names <- as.character(levels(diamonds$cut))
# onehot_cut <- as.matrix(one_hot(as.data.table(factor(as.character(diamonds$cut)))))
# colnames(onehot_cut) <- make.names(cut_names)
# D <- as.matrix(cbind(D, onehot_cut))
# 
# color_names <- as.character(levels(diamonds$color))
# onehot_color <- as.matrix(one_hot(as.data.table(factor(as.character(diamonds$color)))))
# colnames(onehot_color) <- make.names(color_names)
# D <- as.matrix(cbind(D, onehot_color))
# 
# clarity_names <- as.character(levels(diamonds$clarity))
# onehot_clarity <- as.matrix(one_hot(as.data.table(factor(as.character(diamonds$clarity)))))
# colnames(onehot_clarity) <- make.names(clarity_names)
# D <- as.matrix(cbind(D, onehot_clarity))

# N <- dim(D)[1] # number of rows of the new one-hot-encoded dataframe
# M <- dim(D)[2] # number of columns of the new one-hot-encoded dataframe
# 
# head(D)

Dia <- as.data.frame(diamonds)
Dia$cut <- as.numeric(Dia$cut)
Dia$color <- as.numeric(Dia$color)
Dia$clarity <- as.numeric(Dia$clarity)

#dim(diamonds)
N <- dim(Dia)[1] # number of rows
M <- dim(Dia)[2] # number of columns
attributeNames <- colnames(Dia)

head(Dia)
str(Dia)

# DATA VISUALIZATION AND REPRESENTATION ########################################

# histogram
yvals <- c()
for (m in 1:M)
{
  res <- hist(Dia[, m], plot = FALSE)
  yvals <- c(yvals, res$counts)
}

# The argument ylim ensures that all histograms are plotted on the same y-axis
{
  par(mfrow = c(2, 5))
  hist(Dia[, 1], main = attributeNames[1], xlab = NULL, ylim = c(min(yvals), max(yvals)))
  hist(Dia[, 2], main = attributeNames[2], xlab = NULL, ylim = c(min(yvals), max(yvals)), breaks=0.5+seq(0,5))
  hist(Dia[, 3], main = attributeNames[3], xlab = NULL, ylim = c(min(yvals), max(yvals)), breaks=0.5+seq(0,7))
  hist(Dia[, 4], main = attributeNames[4], xlab = NULL, ylim = c(min(yvals), max(yvals)), breaks=0.5+seq(0,8))
  for (m in 5:M)
  {
    hist(Dia[, m], main = attributeNames[m], xlab = NULL, ylim = c(min(yvals), max(yvals)))
  }
}

####################### Outlier detection
i <- 6 # table
IdxOutlier <- Dia[,i] > quantile(Dia[,i],.9999)
(Outlier <- Dia[which(IdxOutlier), ])
i <- 9 # width (y)
IdxOutlier <- Dia[,i] > quantile(Dia[,i],.99994)
(Outlier <- Dia[which(IdxOutlier), ])
i <- 10 # depth (z)
IdxOutlier <- Dia[,i] > quantile(Dia[,i],.99995)
(Outlier <- Dia[which(IdxOutlier), ])

nr <- c(24068,49190,48411,24933)
Dia <- Dia[-nr, ]
N <- N - length(nr)

yvals <- c()
for (m in 1:M)
{
  res <- hist(Dia[, m], plot = FALSE)
  yvals <- c(yvals, res$counts)
}

# The argument ylim ensures that all histograms are plotted on the same y-axis
{
  par(mfrow = c(2, 5))
  hist(Dia[, 1], main = attributeNames[1], xlab = NULL, ylim = c(min(yvals), max(yvals)))
  hist(Dia[, 2], main = attributeNames[2], xlab = NULL, ylim = c(min(yvals), max(yvals)), breaks=0.5+seq(0,5))
  hist(Dia[, 3], main = attributeNames[3], xlab = NULL, ylim = c(min(yvals), max(yvals)), breaks=0.5+seq(0,7))
  hist(Dia[, 4], main = attributeNames[4], xlab = NULL, ylim = c(min(yvals), max(yvals)), breaks=0.5+seq(0,8))
  for (m in 5:M)
  {
    hist(Dia[, m], main = attributeNames[m], xlab = NULL, ylim = c(min(yvals), max(yvals)))
  }
}

################# Correlation of variables

Dia_qty <- Dia[,c(1:2,5:10)]
Dia_qty <- Dia_qty[sample(nrow(Dia_qty), size=1000), ]
Cut <- Dia_qty[,2]
Dia_qty <- as.data.frame(Dia_qty[,c(1,3:8)])
colnames(Dia_qty) <- c("carat","depth","table","price","x","y","z")
observationColors <- c("blue", "green3", "red","orange","purple")[unclass(Cut)]
classNames <- as.character(levels(diamonds$cut))
{
  par(xpd=TRUE)
  pairs(Dia_qty, bg=observationColors, pch=21)
  legend(0, 1, classNames, fill=unique(observationColors))
}

# PCA ##########################################################################

# Manipulation of the dataset for the PCA
D_pca <- as.data.frame(diamonds[,c(1,5:10)]) # drop categorical attibutes
N <- dim(D_pca)[1] # number of rows
M <- dim(D_pca)[2] # number of columns
attributeNames <- colnames(D_pca)

# Stardardization of the dataset

stds <- apply(D_pca, 2, sd)
(round(stds,digits=2))
par(mfrow = c(1, 1))
barplot(stds, ylab = "Diamonds: attribute standard deviation")

# For each column: subtract the mean (of the col) and divide by the std (of the col)
D_pca <- t(apply(D_pca, 1, "-", colMeans(D_pca)))
D_pca <- t(apply(D_pca, 1, "*", 1 / stds))
D_pca <- as.data.frame(D_pca)

head(D_pca)


# Singular value decomposition
S <- svd(D_pca)
diagS <- S$d
rho <- diagS^2 / sum(diagS^2)
rho
threshold <- 0.9

xlimits <- c(1, M)

plot(rho,
     type = "o",
     main = "Variance explained by principal components",
     xlab = "Principal components",
     ylab = "Variance explained",
     xlim = xlimits,
     ylim = c(0, 1),
     col = "blue"
)

lines(cumsum(rho), type = "o", col = "orange")
lines(xlimits, c(threshold, threshold), lty = "dashed")

legend("right", # Define position
       legend = c("Cumulative", "Individual", "Threshold"), # Set strings for legend
       col = c("orange", "blue", "black"), lty = c(1, 1, 2), # Match appereance of lines
       cex = 1, bg = "lightblue"
) # Setup how the box looks (cex controls size)

# Principal directions interpreted in terms of features
Z <- S$u %*% diag(S$d)
V <- S$v

pcs <- 1:3
test <- as.data.frame(melt(data.table(V[, pcs])))
ggplot(test, aes(x = rep(1:7, length(pcs)), y = value, fill=variable)) +
  ggtitle("Principal directions interpreted in terms of features") +
  geom_bar(position="dodge", stat = "identity") +
  labs(fill="PC", x = "Attributes", y = "Component coefficients")

# Plot coefficients in the PC-space
i <- 1
j <- 2

dev.new()
par(mfcol = c(1, 1), pty = "s")
plot(c(-1, 1), c(-1, 1),
     xlab = paste("PC", toString(i)), ylab = paste("PC", toString(j)),
     type = "n",
     main = "Coefficients in the PC-space"
)
arrows(integer(M), integer(M),
       V[, i], V[, j],
       length = .1,
       col = "blue"
)
text(V[, i] * 1.1, V[, j] * 1.1, attributeNames, cex = 1.5)
# Add a unit circle
th <- seq(0, 2.1 * pi, 0.1)
lines(cos(th), sin(th))

# Data projected onto the first two principal components

library(scatterplot3d)
library(rgl)
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,as.character(diamonds$cut))
Z_sample <- Z[sample(nrow(Z),1000),]

cols <- rep("black", times = length(Z_sample[,8]))
cols[Z_sample[,8] == "Fair"] <- "purple"
cols[Z_sample[,8] == "Good"] <- "blue"
cols[Z_sample[,8] == "Very Good"] <- "green3"
cols[Z_sample[,8] == "Premium"] <- "orange"
cols[Z_sample[,8] == "Ideal"] <- "red"

plot3d(Z_sample[, 1:3], col = cols, xlab="PC1", ylab="PC2", zlab="PC3", size = 4)

Z <- S$u %*% diag(S$d)
Z <- cbind(Z,as.character(diamonds$clarity))
Z_sample <- Z[sample(nrow(Z),1000),]

cols <- rep("black", times = length(Z_sample[,8]))
cols[Z_sample[,8] == "I1"] <- "purple"
cols[Z_sample[,8] == "SI2"] <- "azure"
cols[Z_sample[,8] == "SI1"] <- "blue"
cols[Z_sample[,8] == "VS2"] <- "green4"
cols[Z_sample[,8] == "VS1"] <- "green1"
cols[Z_sample[,8] == "VVS2"] <- "orange"
cols[Z_sample[,8] == "VVS1"] <- "red"
cols[Z_sample[,8] == "IF"] <- "brown"

plot3d(Z_sample[, 1:3], col = cols, xlab="PC1", ylab="PC2", zlab="PC3", size = 4)

Z <- S$u %*% diag(S$d)
Z <- cbind(Z,as.character(diamonds$color))
Z_sample <- Z[sample(nrow(Z),1000),]

cols <- rep("black", times = length(Z_sample[,8]))
cols[Z_sample[,8] == "D"] <- "purple"
cols[Z_sample[,8] == "E"] <- "brown"
cols[Z_sample[,8] == "F"] <- "blue"
cols[Z_sample[,8] == "G"] <- "green4"
cols[Z_sample[,8] == "H"] <- "green1"
cols[Z_sample[,8] == "I"] <- "orange"
cols[Z_sample[,8] == "J"] <- "red"

plot3d(Z_sample[, 1:3], col = cols, xlab="PC1", ylab="PC2", zlab="PC3", size = 4)

plot(1,1)
legend("topright",c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"),fill=c("purple","azure","blue","green4","green1","orange","red","brown"))

# CUT
Z <- S$u %*% diag(S$d)

i <- 2
j <- 3

ggplot() +
  ggtitle('Cut') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=as.character(diamonds$cut)), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# COLOR
Z <- S$u %*% diag(S$d)

i <- 2
j <- 3

ggplot() +
  ggtitle('Color') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=as.character(diamonds$color)), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# CLARITY
Z <- S$u %*% diag(S$d)

i <- 1
j <- 2

ggplot() +
  ggtitle('Clarity') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=as.character(diamonds$clarity)), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# PRICE, important
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$price)

i <- 1
j <- 2

ggplot() +
  ggtitle('Price') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# CARAT, important
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$carat)

i <- 1
j <- 2

ggplot() +
  ggtitle('Carat') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=log(Z[,11])), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# DEPTH
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$depth)

i <- 2
j <- 3

ggplot() +
  ggtitle('Depth') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# TABLE
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$table)

i <- 1
j <- 4

ggplot() +
  ggtitle('Table') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)



# -----------------------------------------------------------------------------------------------------------
# -------------------------------- Exam Problems         ------------------------------------------------
# -----------------------------------------------------------------------------------------------------------

# Question 2 

# Define your vectors
a <- c(26, 0, 2, 0, 0, 0, 0)
b <- c(19, 0, 0, 0, 0, 0, 0)

# Maximum Distance 
a <- as.matrix(c(26, 0, 2, 0, 0, 0, 0))
distanz <- norm(a - b, type = "M")
cat("Maximum p-norm distance:", distanz, "\n")

# Question 3 
# Obtain the diagonal singular Values from Sigma (S)
dee <- c(13.9, 12.47, 11.48, 10.03, 9.45)

# Calculate rho as a percentage. 
dee^2/sum(dee^2) * 100

28.81991 + 23.19508 + 19.65833 + 15.00600  # A = True. 

19.65833 +  15.00600  + 13.32069 # B = False

28.81991 + 23.19508 # C = False

28.81991 +  23.19508 + 19.65833 # D = False



# Question 6 



