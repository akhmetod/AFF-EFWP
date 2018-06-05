install.packages("AER")
library(AER)
data("CigarettesSW")
rprice <- with(CigarettesSW, price/cpi)
tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
packs <- CigarettesSW$packs

lm(packs ~ rprice)
ivreg(packs ~ rprice | tdiff)

# first stage
lms1 <- lm(rprice ~ tdiff)

# manually obtain fitted values
lmXhat <- lms1$coefficients[2]*tdiff + lms1$coefficients[1]

# estimate second stage using Xhat
(lms2 <- lm(packs ~ lmXhat))

library(nnet)
set.seed(123)

#first-stage 
nns1 <- nnet(rprice ~ tdiff, size = 0, skip = TRUE, linout = TRUE)

# manually obtain fitted values
nnXhat <- nns1$fitted.values

# estimate second stage using Xhat
nns2 <- nnet(packs ~ nnXhat, size = 0, skip = TRUE, linout = TRUE)

summary(nns2)

lms2$coefficients - nns2$wts

library(ggplot2)
qplot(lms2$fitted.values - nns2$fitted.values)

#######
n <- 5000
dropout_rate <- min(1000/(1000 + n), 0.5)
epochs <- as.integer(1500000/as.double(n)) #heuristic number of epochs
batch_size <- 100
images = FALSE

datafunction <- function(n, s, images = images, test = FALSE) {
  data_generator.demand(n = n,
                        seed = s,
                        ypcor = 0.5,
                        use_images = images,
                        test = test)
  
}

x, z, t, y, g_true <- datafunction(n, 1)
