#Juliusz Czupryniak I7E2S1 AWD
#Wykres 1
par(adj=0.5,mar=c(10,5.5,8,0.5))

barplot(LifeCycleSavings[,1],
        col=ifelse(LifeCycleSavings[,1]>mean(LifeCycleSavings[,1]),"palegreen3","indianred"),
        names.arg=row.names(LifeCycleSavings),
        las=2,
        yaxp=c(0,22,22),
        space=0.3,
        cex.names = 1.1)

title(main = "Chart representing personal savings by country", 
      cex.main =2,
      line=5)

title(xlab = "Country",
      cex.lab=2,
      line = 8)

title(ylab="Aggregate of personal savings",
      cex.lab=1.5,
      line = 3.5)

abline(h=mean(LifeCycleSavings[,1]),
       col="darkorchid",lwd=3)

legend("topleft",
       legend=c(paste("Mean Value =",mean(LifeCycleSavings[,1],sep=" ")),"Under","Over"),
       fill=c("darkorchid4","indianred","palegreen3"))

#Wykres 2     
par(mar=c(10,6,3,3))
barplot(rbind(LifeCycleSavings[,2],LifeCycleSavings[,3],(100-LifeCycleSavings[,2]-LifeCycleSavings[,3])),
        col = c("steelblue","tan2","lavender"),
        yaxp=c(0,100,20),
        las=2,
        names.arg = row.names(LifeCycleSavings))
legend("topright",
       legend=c("% of population under 15",
                "% of population over 75",
                "% of population between 15-75"),
       fill=c("steelblue","tan2","lavender")
)
title(main = "% Distribution age groups by country")
title(xlab = "Country",line = 7,cex.lab=1.5)
title(ylab="% Share of the group in the total population",cex.lab=1.5)

#Wykres 3
par(xpd=FALSE,mar=c(5.1, 4.1, 4.1, 10))
colors <- sample(colours(),50)
plot(
  x=LifeCycleSavings[,4],
  y=LifeCycleSavings[,5],
  col = colors,
  pch =16,
  cex=1.5,
  yaxp=c(0,17,17),
  xaxp=c(0,4200,21),
  xlab = "Real per-capita disposable income",
  ylab= "% Growth rate of dpi",
  main = "Chart representing map measures of disposable per-capita income")
text(LifeCycleSavings[,4],
     LifeCycleSavings[,5],
     labels=row.names(LifeCycleSavings),
     cex=0.7,
     pos = 1)
par(xpd=TRUE)
legend(
  "topright",
  legend = row.names(LifeCycleSavings),
  fill = colors,
  cex=0.7,
  xpd = TRUE,
  inset = c(-0.07,0)
)




