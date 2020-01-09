#Juliusz Czupryniak, I7E2S1, Analiza i Wizualizacj Danych, Laboratorium_03
#Wykres 1
Eggs<-read.csv2("http://jolej.linuxpl.info/Eggs.csv",header=TRUE, stringsAsFactors = FALSE)
Products<-cbind(Eggs$Egg.Pr,Eggs$Beef.Pr,Eggs$Pork.Pr,Eggs$Chicken.Pr,Eggs$Cereal.Pr)
kol <- c("mediumorchid4","coral3","blue3","darkcyan","red")
matplot(x=Eggs$Week,
        y=Products,type="o",
        pch = 15:20,xaxp=c(0,110,55),
        yaxp=c(50,200,15),
        xlab="Week Number",
        ylab="Products prices",
        col=kol,
        lwd=2,
        lty=1)
legend("topright",
       legend=c("Eggs","Beef","Pork","Chicken","Cereal"),
       fill=kol,
       cex=0.8
       )
#Wykres 2
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
plot(x,y,pch=19,col=ifelse(easter==0,"Blue",
                           ifelse(easter==1,"seagreen4",
                                  ifelse(easter==2,"Red","Orange"))),
     xlab="Eggs cases, % of Maximal Value",
     ylab="Eggs price, % of Maximal Value")
text(x=tt$X.t.Eggs.Cases.max.t.Eggs.Cases..,
     y=tt$X.t.Eggs.Egg.Pr.max.t.Eggs.Egg.Pr..,
     labels=t$`Eggs$Month`,
     cex=0.8,
     pos=1)
legend("topright",
       legend=c("Non Easter","Pre Easter","Easter","Post Easter"),
       fill=c("Blue","seagreen4","Red","Orange"))

#Wykres 3
boxplot(tt$X.t.Eggs.Cases.max.t.Eggs.Cases..~tt$t..Eggs.First.Week.,
        data=tt,
        ylab="Eggs Cases",
        xlab="First week of month?")
legend("topright",
  legend=c("0 = No","1 = Yes"))












