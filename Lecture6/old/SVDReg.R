##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R Part 2 ----
##
## ----- The SVD of the model -------

males.ext = males[, c('mother', 'father', 'childHeight')]
males.ext[, c('mother.sqr', 'father.sqr', 'motherfather')] = 
  data.frame(males$mother^2, males$father^2, 
             males$mother * males$father)

R = males.ext[, c('father', 'mother',
                  'father.sqr', 'mother.sqr',
                  'motherfather')]
RSVD = svd(R) # The SVD 

RSVD$v %*% t(RSVD$v)  # Is nearly orthogonal

# The diagonal matrix of singular values
d = diag(RSVD$d) 
d


## Plot the singular vectors
plot.vec = function(u, n = 5){
  par(mfrow = c(n,1))
  par(mar=c(1,6,1,2))
  for(i in 1:n){
    barplot(u[,i])
    abline(h = 0, lwd = 2, col = 'blue')
  }
  par(mfrow = c(1,1))
}
plot.vec(RSVD$u)
plot.vec(t(RSVD$v))

## Plot the singular values
plot.sing = function(u){
  par(mar=c(5,5,5,5))
  nrows = nrow(u)
  d = rep(0,nrows)
  for(i in 1:nrows) d[i] = u[i,i]
  plot(1:nrows, d, col = 'red',
       main = ('Singular values'),
       xlab = 'Singular value order',
       ylab = 'Singular value')
}
plot.sing(d)

## Recompute the original matrix
M = RSVD$u[, 1:5] %*% d %*% t(RSVD$v)
dim(M)
head(R - M) ## Check the matrices are almost the same

## ---- Pseudo inverse and SVD regression

## Compute the inverse the singular values
dInv = diag(1/RSVD$d) 
dInv
plot.sing(dInv)

## Compute the pseudo inverse
pInv = RSVD$v %*% t(dInv) %*% t(RSVD$u[, 1:5])
dim(pInv)

## Is the product I?
pInv %*% M
pInv %*% as.matrix(R)

## Compute the model coeficients
b = pInv %*% as.matrix(males.ext$childHeight)
b
  

## 
## ---- Eliminate the least stable singular vectors
dInv2 = dInv
dInv2[5,5] = 0.0
dInv2

pInv2 = RSVD$v %*% t(dInv2) %*% t(RSVD$u[, 1:5])
b2 = pInv2 %*% as.matrix(males.ext$childHeight)
b2

## Try reducting the dimensionality again
dInv3 = dInv2
dInv3[4,4] = 0.0
dInv3

pInv3 = RSVD$v %*% t(dInv3) %*% t(RSVD$u[, 1:5])
b3 = pInv %*% as.matrix(males.ext$childHeight)
b3

## Use the 4 dimensional pseudo inverse
## Have a look at the residuals
males.ext$pred = as.matrix(R) %*% b2
males.ext$resid = males.ext$pred - males.ext$childHeight

plot.diagnostic = function(df){
  ## Plot the histogram and Q-Q of the residuals
  par(mfrow = c(1,2))
  hist(df$resid,
     main = 'Histogram of residuals',
     xlab = 'Model residuals')
  qqnorm(df$resid)
  par(mfrow = c(1,1))

  ## Plot the residuals vs the predicted values
  require(ggplot2)
  ggplot(df, aes(pred, resid)) +
    geom_point(size = 2, alpha = 0.3) + 
    ggtitle('Residuals vs predicted value') + 
    xlab('Predicted values') + ylab('Residual')
}
plot.diagnostic(males.ext)


## --- Another approach to regularization
d
dReg = diag(RSVD$d + 10)
dReg

dInvReg = diag(1/(RSVD$d + 10))
dInvReg

pInvReg = RSVD$v %*% t(dInvReg) %*% t(RSVD$u[, 1:5])
bReg = pInvReg %*% as.matrix(males.ext$childHeight)
bReg

males.ext$pred = as.matrix(R) %*% bReg
males.ext$resid = males.ext$pred - males.ext$childHeight
plot.diagnostic(males.ext)
