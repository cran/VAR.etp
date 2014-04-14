VAR.irf <-
function(b,p,sigu,h=10)
{
k <- nrow(b)
mf <- VAR.mainf(b,p,h)
q <- t(chol(sigu))
index <- 1:k
impmat <- matrix(0,nrow=k*k,ncol=h+1)
for( i in 1:(h+1) ){
impmat[,i] <- t( t( as.vector(mf[,index] %*% q)) )
index <- index+k
}
colnames(impmat) <- paste("h",0:h,sep="")
tem1 <- rownames(b)
tem2 <- character()
for (i in 1:length(tem1))
for (j in 1:length(tem1))
tem2 <- c(tem2,paste(tem1[i],tem1[j],sep="->"))
rownames(impmat) <- tem2

return(t(impmat))
}
