
# -- Libraries
library(MASS)
library(ggplot2)
library(data.table)
library(mltools)
library(reshape)
library(reshape2)
library(rmarkdown)
library(tidyverse)

render("C:/Users/Alessandro/Desktop/PIETRO/Università/3_Machine Learning and Data Mining/Project/Machine_Learning_Project/Report.Rmd")

# -- Diamonds
data(diamonds)
?diamonds
str(diamonds)

# One-hot-encode for regression of price ###
# Maybe it is not necessary to transform in one-hot-encode because all the 3 factor
# attributes are ordered (we know that cut "fair" is worse than cat "good")

D <- as.data.frame(diamonds[-c(2:4,7)]) # drop "price", that is the target variable, and nominal variables

cut_names <- as.character(levels(diamonds$cut))
onehot_cut <- as.matrix(one_hot(as.data.table(factor(as.character(diamonds$cut)))))
colnames(onehot_cut) <- make.names(cut_names)
D <- as.matrix(cbind(D, onehot_cut))

color_names <- as.character(levels(diamonds$color))
onehot_color <- as.matrix(one_hot(as.data.table(factor(as.character(diamonds$color)))))
colnames(onehot_color) <- make.names(color_names)
D <- as.matrix(cbind(D, onehot_color))

clarity_names <- as.character(levels(diamonds$clarity))
onehot_clarity <- as.matrix(one_hot(as.data.table(factor(as.character(diamonds$clarity)))))
colnames(onehot_clarity) <- make.names(clarity_names)
D <- as.matrix(cbind(D, onehot_clarity))

N <- dim(D)[1] # number of rows of the new one-hot-encoded dataframe
M <- dim(D)[2] # number of columns of the new one-hot-encoded dataframe

head(D)

# PCA ##########################################################################

Dia <- as.data.frame(diamonds[-7]) # drop the target variable "price"
Dia$cut <- as.numeric(Dia$cut)
Dia$color <- as.numeric(Dia$color)
Dia$clarity <- as.numeric(Dia$clarity)

N <- dim(Dia)[1] # number of rows of the new one-hot-encoded dataframe
M <- dim(Dia)[2] # number of columns of the new one-hot-encoded dataframe

head(Dia)

stds <- apply(Dia, 2, sd)
par(mfrow = c(1, 1))
barplot(stds, ylab = "Diamonds: attribute standard deviation")

# Stardardization of the dataset:
# For each column: subtract the mean (of the col) and divide by the std (of the col)
D_pca <- t(apply(Dia, 1, "-", colMeans(Dia)))
D_pca <- t(apply(D_pca, 1, "*", 1 / stds))

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
       legend = c("Individual", "Cumulative", "Threshold"), # Set strings for legend
       col = c("orange", "blue", "black"), lty = c(1, 1, 2), # Match appereance of lines
       cex = 1, bg = "lightblue"
) # Setup how the box looks (cex controls size)

# Principal directions interpreted in terms of features
Z <- S$u %*% diag(S$d)
V <- S$v

pcs <- 1:2
test <- as.data.frame(melt(data.table(V[, pcs])))
ggplot(test, aes(x = rep(1:9, length(pcs)), y = value, fill=variable)) +
  geom_bar(position="dodge", stat = "identity") +
  labs(fill="PC", x = "Attributes", y = "Component coefficients")

# Data projected onto the first two principal components

i <- 1
j <- 2

ggplot() +
  geom_point(aes(x = Z[, i], y = Z[, j]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.22), legend.title = element_blank()) +
  labs(x = colnames(Z)[i], y = colnames(Z)[j])
