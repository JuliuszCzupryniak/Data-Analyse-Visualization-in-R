par(mai = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
library(lattice)
#1. Zadanie
#Program 1
angle <- seq(0, 2*pi, length=19)[-19]
xx <- c(9.20, 6.00, 6.00, 11.25, 11.00, 7.25, 9.7, 13.25, 14.00, 8.00)
histogram( ~ xx | "Histogram",
           breaks = c(6, 8, 10, 12, 14),
           right=FALSE, 
           ylab="Frequency in %")

#Program 2
set.seed(591)
xx1 <- rnorm(20, mean = 3, sd = 3.6)
xx2 <- rpois(40, lambda = 3.5)
xx3 <- rchisq(31, df = 5, ncp = 0)

x1 <- data.frame(xx1,"Rozk³ad Normalny")
x2 <- data.frame(xx2,"Rozk³ad Poissona")
x3 <- data.frame(xx3,"Rozk³ad Chi kwadrat")
colnames(x1)[1] <- "Wartosc"
colnames(x1)[2] <- "Grupa"
colnames(x2)[1] <- "Wartosc"
colnames(x2)[2] <- "Grupa"
colnames(x3)[1] <- "Wartosc"
colnames(x3)[2] <- "Grupa"
X<-rbind(x1,x2,x3)

bwplot(Wartosc~Grupa,data=X,
       cex = 0.7,
       add=TRUE)

#2. Zadanie , Wykres 1
library(lattice)
Eggs<-read.csv2("C:/Users/Juliusz/Downloads/Eggs.csv",header=TRUE, stringsAsFactors = FALSE)
dfrm <- data.frame( y=c(as.numeric(Eggs$Egg.Pr),
                        as.numeric(Eggs$Beef.Pr),
                        as.numeric(Eggs$Pork.Pr),
                        as.numeric(Eggs$Chicken.Pr),
                        as.numeric(Eggs$Cereal.Pr)),
                    x=1:105, 
                    grp=rep(c("Eggs Price","Beef Price","Pork Price","Chicken Price","Cereal Price"),each=105))
xyplot(y~x,group=grp,data=dfrm,type="o",
       auto.key=list(x=0,y=1,text=c("Beef","Cereal","Chicken","Eggs","Pork"),
                     points=FALSE, lines=TRUE,col=c("steelblue3","deeppink","darkgreen","red","orange")),
       main = "Product Prices over 2 years, by week number",
       ylab = "Price",
       xlab="Week number")
#2. Zadanie , Wykres 2
t<-aggregate(. ~Eggs$Month+Eggs$First.Week+Eggs$Easter,
             data=data.frame(Eggs$Cases,Eggs$Egg.Pr),
             mean)
t[t$`Eggs$First.Week`=="Yes",2] <- "1"
t[t$`Eggs$First.Week`=="No",2] <- "0"
t[t$`Eggs$Easter`=="Easter",3]<-"2"
t[t$`Eggs$Easter`=="Pre Easter",3]<-"1"
t[t$`Eggs$Easter`=="Post Easter",3]<-"3"
t[t$`Eggs$Easter`=="Non Easter",3]<-"0"
tt<-data.frame(t$`Eggs$First.Week`,
               t$`Eggs$Easter`,
               (t$Eggs.Cases/max(t$Eggs.Cases)),
               (t$Eggs.Egg.Pr/max(t$Eggs.Egg.Pr)))
x<-tt$X.t.Eggs.Cases.max.t.Eggs.Cases..
y<-tt$X.t.Eggs.Egg.Pr.max.t.Eggs.Egg.Pr..
easter<-tt$t..Eggs.Easter.

panel = function (x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.text(x, y, labels=t$`Eggs$Month`, pos=1,cex=0.8)   
}

xyplot(y~x,pch=19,col=ifelse(easter==0,"steelblue3",
                             ifelse(easter==1,"deeppink",
                                    ifelse(easter==2,"darkgreen","red"))),
       auto.key=list(x=0,y=1,text=c("Non Easter","Pre Easter","Easter","Post Easter"),
                     points=TRUE, lines=FALSE,col=c("steelblue3","deeppink","darkgreen","red")),
       main="Chart presenting eggs price and sold cases",
       xlab="Eggs cases, % of Maximal Value",
       ylab="Eggs price, % of Maximal Value",
       panel = panel
       )
#2. Zadanie , Wykres 3
bwplot(tt$X.t.Eggs.Cases.max.t.Eggs.Cases..~tt$t..Eggs.First.Week., data=tt,
       ylab="Eggs Cases",
       xlab="First week of month?",
       main="Sold cases depending on the first week",
       auto.key=list(x=0,y=1,text=c("0 - No","1 - Yes"),
                     points=FALSE, lines=FALSE))


