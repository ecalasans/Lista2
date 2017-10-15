library(scales)


normalCurve <- function(xbarra, variancia, n, ic){
        alfa <- (100-ic)/100
        intervaloMediaN <- xbarra + c(-1,1)*qnorm(1-(alfa/2))*(sqrt(variancia/n))
}

tsCurve <- function(xbarra, variancia, n, ic){
        alfa <- (100-ic)/100
        intervaloMediaT <- xbarra + c(-1,1)*qt(1-(alfa/2),df = n-1)*(sqrt(variancia/n))
}

quiCurve <- function(variancia, n, ic){
        alfa <- (100-ic)/100
        limSup <- (n-1)*variancia/qchisq((alfa/2),df = n-1)
        limInf <- (n-1)*variancia/qchisq(1-(alfa/2),df = n-1)
        intervaloVarChi <- c(limInf,limSup)
}

fCurve <- function(n1, s1, n2, s2, ic){
        alfa <- (100 - ic)/100
        
        limSup <- (s1/s2)/qf((alfa/2),df1 = (n1-1),df2 = (n2-1))
        limInf <- (s1/s2)/qf(1-(alfa/2),df1 = (n1-1),df2 = (n2-1))
        
        intervaloRVarF <- c(limInf,limSup)
}