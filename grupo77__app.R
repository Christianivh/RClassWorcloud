# Grupo_Ataque77: Javier Almir Cardenas Apasa / Christian Vasquez Hernandez

library(shiny)

ui <- fluidPage(
  headerPanel('Cluster WordCloud'),
  
  sidebarPanel(
    sliderInput("clusters", "Maximo de Cluster:", min = 1,  max = 6, value = 1)
      ),
  mainPanel(
    plotOutput('plot1')
  )
)


server <- function(input, output) {

  data1 =reactive({
    library(shiny)
    library(slam)
    library(tm)
    library(RColorBrewer)
    library(wordcloud)
    library(NLP)
    library(dplyr)
    library(fpc)
    setwd("D:/Christian/Cursos/Diplomado/R_Bigdata")
    load("grupo77_kmeansmodel.RModel")
    load("grupo77_classified.rdocs")
    
    for.wordcloud <- classified.docs
    current.group <- for.wordcloud[for.wordcloud$clasification== input$clusters,]
    set.seed(1234)
    termFqcy <- rowSums(t(current.group[,-dim(for.wordcloud)[2]]))
    wordfreq <- sort(termFqcy, decreasing = T)
    grayLevels <- gray(wordfreq/max(wordfreq))
    wordfreq
    })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot1 <- renderPlot({
    data2<- data1()
    
    wordcloud_rep(words = names(data2), freq = data2, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
    
  })
  
  
}

shinyApp(ui = ui, server = server)