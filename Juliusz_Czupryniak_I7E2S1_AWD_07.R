install.packages("sqldf")
library("sqldf")
library(ggplot2)
economics_dataframe <- data.frame(format(economics$date, "%C%y"),economics$pce,
                                  economics$pop, economics$psavert,
                                  economics$uempmed,economics$unemploy)
colnames(economics_dataframe)<-colnames(economics)
economics_agr<-sqldf('select date, avg(pce),avg(pop),avg(psavert),avg(uempmed),avg(unemploy) from economics_dataframe
      group by date')

barchart_pop <- ggplot(economics_agr,aes(x=date,y=economics_agr$`avg(pop)`))
barchart_pce<- ggplot(economics_agr,aes(x=date,y=economics_agr$`avg(pce)`))
barchart_pop + geom_bar(stat="identity",width = 0.8,fill = "#EEAA99") + 
  ggtitle("Population over the years") + xlab("Year") + 
  ylab("Population in thousands")+
  theme(axis.text=element_text(size=6))+
  scale_y_continuous(breaks=seq(0, 300000, 50000))
barchart_pce + geom_bar(stat="identity",width=0.8,fill="#BB4444") +
  ggtitle("Personal consumption expenditures over the years") +
  xlab("Year") + ylab("Personal consumption expenditures\nin billions of dollars") +
  theme(axis.text=element_text(size=6))+
  scale_y_continuous(breaks=seq(0, 13000, 1000))

economics_agr2<-sqldf('select (cast(date as int)/5)*5, avg(pce),avg(pop),avg(psavert),avg(uempmed),avg(unemploy) from economics_dataframe
      group by (cast(date as int)/5)*5')
colnames(economics_agr2)[1]<-"date"
colnames(economics_agr2)[4]<-"Personal savings rate"

bubblechart<-ggplot(economics_agr2,
                    aes(x=economics_agr2$`avg(uempmed)`,
                        y=economics_agr2$`avg(unemploy)`,
                        size=`Personal savings rate`,
                        label=economics_agr2$date),
                    guide=FALSE)

bubblechart + 
  geom_point(fill=rainbow(11),shape=21) +
  geom_text(size=3,hjust = 0.5, nudge_x = 0,vjust=1.5,nudge_y = 2)+
  ggtitle("Bubble chart") +
  xlab("median duration of unemployment in weeks")+
  ylab("number of unemployed in thousands")
