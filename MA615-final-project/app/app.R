#This Shinny application allows users to explore the data interractively.
#It contains a cluster plot and a bar plot, and you can use navigation bar
#to navigate between them.

#The cluster plot defines some clusters of tweets about alienware.X-axis represents
#the sentiment score:higher score means a more positive attitude.Y-axis represents
#the number of words showing sentiment.

#The bar plot shows the most popular words which appear in tweets about alienware.It's
#ordered from most popular to least popular according to number of appearances.

library(shiny)
library(ggplot2)
library(datasets)

# Define UI for application that draws cluster plot and bar plot.
ui <- fluidPage(
   
   # Application title
   titlePanel("Shinny App for Fianl Project"),
   navbarPage(title="plots",
      tabPanel("cluster",sidebarLayout(
        sidebarPanel(
        numericInput('clusters','Cluster Count',4,min=2,max=6)
        ),
        mainPanel(
          plotOutput("Plot")
        ))
   ),tabPanel("bar chart",sidebarLayout(
        sidebarPanel(
          sliderInput("num","Choose words in plot",10,min=5,max=15)
          ),
        mainPanel(
          plotOutput("Plot1"))
        )
   )))
   
    
   


#Define the server side codes which link input and output dynamicly.
server <- function(input, output) {
   
   output$Plot <- renderPlot({
     set.seed(7)
     dat<-read.csv("myData.csv")
     km1<- kmeans(dat, input$clusters, nstart=100)
     plot(dat, col =(km1$cluster +1) , main="K-Means result with chosen clusters", pch=20, cex=2)
  
   })
   output$Plot1<-renderPlot({
     dat1<-read.csv("myData1.csv")
     dat2<-dat1[1:input$num,]
     ggplot(dat2,aes(x=word,y=n))+geom_bar(stat="identity")+scale_x_discrete(limits=dat2$word)
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

