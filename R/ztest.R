z_test_from_data <- function(data,col1,col2,sub1,sub2) {
 data <- as.data.frame(data)
 V1<-data[,col1]
 V2<-data[,col2]
 #data clean and subset, either
 X <- subset(data, V1 == sub2)
 Y <- subset(data, V1 == sub1)
 x <- X[,col2]
 y <- Y[,col2]
 #z score
 zeta<-(mean(x)-mean(y))/(sqrt(sd(x)^2/length(x) +sd(y)^2/length(y)))
 print(paste(zeta," is the z-value"))
 #plot red line
 r <- max(zeta+0.5, 5)
 plot(x=seq(from = -r, to= r, by=0.1),y=dnorm(seq(from = -r, to= r,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
 abline(v=zeta, col='red')
 #get p
 p = 1-pnorm(zeta)
 print(paste(p, " is the p-value"))
 return(p)
}

z_test_from_agg<-function(mean_a,mean_b,sd_a,sd_b, n_a, n_b){
 zeta<-(mean_b-mean_a)/(sqrt(sd_a^2/n_a + sd_b^2/n_b))
 print(paste(zeta," is the z-value"))
 #plot red line
 r <- max(zeta+0.5, 5)
 plot(x=seq(from = -r, to= r, by=0.1),y=dnorm(seq(from = -r, to= r,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
 abline(v=zeta, col='red')
 #get p
 p = 1-pnorm(zeta)
 print(paste(p, " is the p-value"))
 return(p)
}
