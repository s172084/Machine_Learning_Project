
# -- Libraries
library(MASS)
library(ggplot2)
library(data.table)
library(mltools)
library(reshape)
library(reshape2)
library(rmarkdown)
library(tidyverse)

# setwd("C:/Users/Alessandro/Desktop/PIETRO/Università/3_Machine Learning and Data Mining/Exercises/02450Toolbox_R")
# render("C:/Users/Alessandro/Desktop/PIETRO/Università/3_Machine Learning and Data Mining/Project/Machine_Learning_Project/Report.Rmd")

# -- Diamonds
data(diamonds)
?diamonds
str(diamonds)

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

# PCA ##########################################################################

Dia <- as.data.frame(diamonds)
Dia$cut <- as.numeric(Dia$cut)
Dia$color <- as.numeric(Dia$color)
Dia$clarity <- as.numeric(Dia$clarity)

N <- dim(Dia)[1] # number of rows
M <- dim(Dia)[2] # number of columns
attributeNames <- colnames(Dia)

head(Dia)
str(Dia)

# Stardardization of the dataset

stds <- apply(Dia, 2, sd)
print(round(stds,digits=2))
par(mfrow = c(1, 1))
barplot(stds, ylab = "Diamonds: attribute standard deviation")

# For each column: subtract the mean (of the col) and divide by the std (of the col)
D_pca <- t(apply(Dia, 1, "-", colMeans(Dia)))
D_pca <- t(apply(D_pca, 1, "*", 1 / stds))
D_pca <- as.data.frame(D_pca)

head(D_pca)


# Singular value decomposition
S <- svd(D_pca)
diagS <- S$d
rho <- diagS^2 / sum(diagS^2)

threshold <- 0.9

xlimits <- c(1, M)
plot(rho,
     type = "o",
     main = "Variance explained by principal componenets",
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

pcs <- 1:4
test <- as.data.frame(melt(data.table(V[, pcs])))
ggplot(test, aes(x = rep(1:10, length(pcs)), y = value, fill=variable)) +
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

# COLOR
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$color)

i <- 2
j <- 4

ggplot() +
  ggtitle('Color') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# CUT, PC 2-3, very evident also in PC 1-2
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$cut)

i <- 1
j <- 2

ggplot() +
  ggtitle('Cut') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# CLARITY
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$clarity)

i <- 2
j <- 4

ggplot() +
  ggtitle('Clarity') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
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
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)

# DEPTH
Z <- S$u %*% diag(S$d)
Z <- cbind(Z,diamonds$depth)

i <- 1
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
j <- 2

ggplot() +
  ggtitle('Table') +
  geom_point(aes(x = Z[, i], y = Z[, j], color=Z[,11]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.8), legend.title = element_blank()) +
  labs(x = paste('PC ',i), y = paste('PC ',j)) +
  xlim(-5,10) + ylim(-5,7.5)
