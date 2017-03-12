# vignette: https://cran.r-project.org/web/packages/PlayerRatings/vignettes/AFLRatings.pdf
library(PlayerRatings)
afl <- aflodds[,c(2,3,4,7)]
train <- afl[afl$Week < 100,]
test <- afl[afl$Week >= 100 & afl$Week < 150,]
valid <- afl[afl$Week >= 150,]
head(train,12)

# generate ratings
sobj <- steph(train, history = TRUE)

plot(sobj, npl=16)
abline(v=c(27,55),lty=2,lwd=2,col="grey")
text(c(14,42),c(2500,2500),c("2009","2010"),cex=1.5)

