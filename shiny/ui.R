library(shiny)


shinyUI(pageWithSidebar(
  
  # Application title
 headerPanel(
  h3("Profil d'un joueur pour la saison 2013",align = "center")),

  # Left Panel
   sidebarPanel(
     helpText("This application is dedicated to coach"),
     selectInput("id.frappeur", label = h5("Selectionner un frappeur"), 
                 choices = liste.frappeur),
     selectInput("id.lanceur", label = h5("Selectionner un lanceur"), 
                 choices = liste.lanceur)),

  # Principal panel with graphics. the tabPanel enable 
  # to show the graphics in different tabs

  mainPanel(
    tabsetPanel(
      tabPanel("frappeur",
               h3(textOutput("caption.frappeur")),
               plotOutput("frappeur", width = "auto", height = "300px")
               ),
      tabPanel("lanceur",
               h3(textOutput("caption.lanceur")),
               plotOutput("lanceur",width = "auto", height = "300px")
              )
                )
              )
          )
      )

