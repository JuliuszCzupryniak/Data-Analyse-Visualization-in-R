data(Investment, package="sandwich")
Investment <- as.data.frame(Investment)
library(lattice)

InvestmentYears <- data.frame(Investment,c(1963:1982))
colnames(InvestmentYears)[8]<-"Year"
xyplot(Investment+GNP+RealGNP~Year,data=InvestmentYears,type="o",
       auto.key=list(x=0,y=1,text=c("Investment","GNP","Real GNP"),
                     points=FALSE, lines=TRUE,col=c("steelblue3","deeppink","darkgreen")),
       main="Nominal and Real Gross National Product against Investments",
       xlab="Year",
       ylab="Value of GNPs",
       scales=list(y=list(at=seq(0, 3200, 200)),
                   x=list(at=seq(1963,1982,1))))
library(latticeExtra)
Inv <- xyplot(Investment~Year,data=InvestmentYears,type="o",
              auto.key=list(x=0,y=1,text=c("Investment","Interest"),
                            points=FALSE, lines=TRUE,col=c("steelblue3","deeppink")),
              main="Interest vs Investment",
              ylab="Investment",
              scales=list(y=list(at=seq(0,500,50)),
                          x=list(at=seq(1963,1982,1))))
Int <- xyplot(Interest~Year,data=InvestmentYears,type="o",ylab="Interest",
              scales=list(y=list(at=seq(0,14,1))))

doubleYScale(Inv,Int)
Inv <- xyplot(Investment~Year,data=InvestmentYears,type="o",
              auto.key=list(x=0,y=1,text=c("Investment","Price"),
                            points=FALSE, lines=TRUE,col=c("steelblue3","deeppink")),
              main="Price vs Investment",
              ylab="Investment",
              scales=list(y=list(at=seq(0,500,50)),
                          x=list(at=seq(1963,1982,1))))
Price <- xyplot(Price~Year,data=InvestmentYears,type="o",ylab="Price",
              scales=list(y=list(at=seq(0,3,0.25))))

doubleYScale(Inv,Price)