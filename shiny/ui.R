library(shiny)


shinyUI(pageWithSidebar(
  
  # Titre de l'application
 headerPanel(
  h3("Profil d'un joueur pour la saison 2013",align = "center")),

  # Paneau Gauche
   sidebarPanel(
     helpText("Cette application est dediee a Monsieur Plante et Monsieur
	Ben-Ameur"),
     selectInput("id.frappeur", label = h5("Selectionner un frappeur"), 
                 choices = liste.frappeur),
     selectInput("id.lanceur", label = h5("Selectionner un lanceur"), 
                 choices = liste.lanceur)),

  # Paneau principal ou les graphiques sont affiches. Le tabPanel permet 
  # de faire afficher les graphiques dans des onglets differents

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

