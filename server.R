#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sqldf)
library (ggplot2)
library(EnglishPremierLeague)
library(gridExtra)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Home_cards = renderPlot({
        choose_r <- input$chosenReferee
        r_cards <- referee_cards[referee_cards$Referee==choose_r,]
        choose_s <- input$Season
        shots_c <-shots[shots$Season==choose_s,]
        choose_ha <-input$HA
        drawsc <- draws[draws$HA==choose_ha,]
    
        p1<-(ggplot(data=r_cards,aes(x=r_cards$`Home Team Yellow Cards`,y=r_cards$`Home Team Red Cards`,size=r_cards$Appearances))+
          geom_point(color="Blue")+facet_wrap(~r_cards$Season,ncol=5)+xlab("Yellow Cards, average")+ ylab("Red Cards, average")+scale_size_continuous(name ="Appearances")+
          ggtitle("Average of Yellow and Red Cards for Home Team by Referee"))
        p2<-(ggplot(data=r_cards,aes(x=r_cards$`Away Team Yellow Cards`,y=r_cards$`Away Team Red Cards`,size=r_cards$Appearances))+
                 geom_point(color="Red")+facet_wrap(~r_cards$Season,ncol=5)+xlab("Yellow Cards, average")+ ylab("Red Cards, average")+scale_size_continuous(name ="Appearances")+
                 ggtitle("Average of Yellow and Red Cards for Away Team by Referee "))
        p3<-(ggplot(data=shots_c,aes(x=shots_c$HomeTeam,y=shots_c$Shots,color=shots_c$`"Home"`))+geom_point()+
               facet_wrap(~"Home",ncol=2)+ggtitle("Shots by Season and Team depending on Home or Away Match")+
               xlab("Football Team")+ylab("All Shots"))
        p4<-(ggplot(data=shots_c,aes(x=shots_c$HomeTeam,y=shots_c$`Shots on target`,color=shots_c$`"Home"`))+geom_point()+
               facet_wrap(~"Home",ncol=2)+ggtitle("Shots on target by Season and Team depending on Home or Away Match")+
               xlab("Football Team")+ylab("Shots on target"))
        p5<-(ggplot(data=drawsc,aes(x=drawsc$Team,y=drawsc$`Number of draws`,fill=drawsc$`Full Time Goals`))+geom_bar(stat="identity",width=0.8)+
                 xlab("Football Team")+ylab("Number of draws")+theme(axis.text=element_text(angle=30,size=10))+scale_fill_continuous(name="Average of goals")+ggtitle("Number of draws by Team"))
        grid.arrange(p1,p2,p3,p4,p5,nrow=5)



    })


})
