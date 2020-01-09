install.packages("sqldf")
install.packages("ggrepel")
library("sqldf")
library(ggplot2)
library(ggrepel)
movies<-read.csv(file="C:/Users/Juliusz/Downloads/movies.csv", header=TRUE, sep=",")
movies_genre<-sqldf('select genre,count(title),avg(duration),avg(gross),avg(budget) from movies
      group by genre')
p <- ggplot(movies_genre)
print(
  p+
  geom_point(shape=21,aes(x=movies_genre$`avg(budget)`,
                   y=movies_genre$`avg(gross)`,
                   fill=movies_genre$genre,
                   size=movies_genre$`avg(duration)`),
            alpha = 0.7)+
  geom_text_repel(aes(label=movies_genre$genre,
                  x=movies_genre$`avg(budget)`,
                  y=movies_genre$`avg(gross)`),
                  size=4,hjust=0.5,vjust=1.5,nudge_y = 1)+
    xlab("Average of Budget")+ylab("Average of Gross")+ggtitle("Average Budget, Gross and Duration by Genre")+ scale_size_continuous(name="Average duration")+
    scale_fill_discrete(name="Genre"))


mb1 <- sqldf('select title,director,genre,year,max(rating),cast_facebook_likes from movies
group by genre')

sqldf('select title,director,genre,year,max(rating),cast_facebook_likes from 
(select * from movies except select * from movies where title in (select title from mb1)) 
group by genre')



movies_best_1 <- sqldf('select title Title,director Director ,genre Genre,year Year ,max(rating) Rating,cast_facebook_likes "Facebook Likes" from movies
                 group by genre having max(rating)>8
                      union
                     select title Title,director Director ,genre Genre,year Year ,max(rating) Rating,cast_facebook_likes "Facebook Likes" from 
(select * from movies except select * from movies where title in (select title from mb1)) 
group by genre
having max(rating)>8
                     order by genre')

movies_best_2 <- sqldf('select director, count(*) "Times in top", avg(a) "Average of ratings" from (select title,director,genre,year,max(rating) a,cast_facebook_likes from movies
                 group by genre 
                      union
                     select title,director,genre,year,max(rating) a,cast_facebook_likes from 
(select * from movies except select * from movies where title in (select title from mb1)) 
group by genre

                     order by genre)
                       group by director
                       order by count(*) desc,3 desc')

p2 <- ggplot(movies_best_1)
print(p2+ geom_point(shape=19,aes(x=movies_best_1$Year,
                                 y=movies_best_1$Rating,
                                 color=movies_best_1$Genre,
                                 size=movies_best_1$`Facebook Likes`))+
        geom_text_repel(aes(label=movies_best_1$Title,x=movies_best_1$Year,y=movies_best_1$Rating))+
        scale_y_continuous(name="Ratings",breaks = seq(8,10,0.1))+
        scale_x_continuous(name="Year",breaks = seq(1930,2016,2))+
        ggtitle("2 Best films of each genre that had a minimum rating of 8")+scale_color_discrete(name="Genre")+scale_size_continuous(name="Facebook Likes"))

p3 <-ggplot(movies_best_2,aes(x=movies_best_2$director,y=movies_best_2$`Times in top`,fill=movies_best_2$`Average of ratings`))
print(p3 + geom_bar(stat="identity",width=0.8)+
        theme(axis.text.x = element_text(face="bold",size=8,angle=45))+xlab("Director")+ylab("Movies in top")+scale_fill_continuous(name = "Average of ratings",
                                                                                                                                    high="#800563",
                                                                                                                                    low="#FDE1F4")+
        ggtitle("Number of appearances of movies of a given director in the top ranking"))
        
                      
movies_budgets <- sqldf('select genre,year,budget from movies
      where (genre in ("Action","Adventure","Comedy","Drama") and year in (2013,2014,2015)) ')
p4<-ggplot(movies_budgets,aes(y=movies_budgets$budget))
print(p4+geom_boxplot()+facet_wrap(genre~year,nrow=4,ncol=3)+ggtitle("Budget boxplots by genre and years")+ylab("Genre")+xlab("Year"))
