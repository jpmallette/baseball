## Server initialisation.  
## Important to modify directory setwd

library(shiny)
library(ggplot2)
library(dplyr)
library(lattice)
library(RSQLite)
setwd("C:\R_Files\shiny")
MLB <- src_sqlite("MLB.2013")


### Join data. Joining upfront improve the performance since 
##  the data will only be read at the begining

## Requete frappeur

table.hip  <- select(tbl(MLB, "hip"),des,x,y,batter,type)
frappeur <- collect(table.hip)   ## collect data in R

# Pitcher query
# The query will be done inside shiny to improve performance

table.pitch <- select(tbl(MLB, "pitch"), 
                      pitch_type, px, pz, num, gameday_link)

table.atbat <- select(tbl(MLB, "atbat"), pitcher,batter,pitcher_name, batter_name, 
                      num, gameday_link,stand)

## server section  

shinyServer(function(input, output) {
  
## input section 
  formula.text.frappeur <- reactive({
    paste("Entrer le id du frappeur :", input$id.batteur)
  })
  
  formula.text.lanceur <- reactive({
    paste("Entrer le id du lanceur :", input$id.lanceur)
  })

## pitching section 

 output$lanceur <- renderPlot({
   
    requete.lanceur<- inner_join(table.pitch, filter(table.atbat, 
                                pitcher == input$id.lanceur),
                          by = c("num", "gameday_link"))
   
   pitchfx <- collect(requete.lanceur)
   
 ## Errpr message
   if(nrow(pitchfx)==0){
    formule.text.lanceur <- reactive({
     paste("Malheureusement, ce numero de joueur n'est pas dans notre base de donnees")
     })
    
    output$caption.lanceur <- renderText({
      formule.text.lanceur()
    }) 
    }
 
  else{
   
   output$caption.lanceur <- renderText({""})
   
 ## Pitcher graphic legend 
   pitchnames <- c("change-up", "curveball", "4S-fastball"
                   , "2S-fastball", "slider")
   
   myKey <- list(space = "right",
                 border = TRUE,
                 cex.title = .8,
                 title = "pitch type",
                 text = pitchnames,
                 padding.text = 4)
   
 ## draw the strike zone 
   
   zone.haut <- 3.5
   zone.bas <- 1.6
   zone.interieur <- -.95
   zone.exterieur <- 0.95
   
 ## draw the pitch graph
   
   xyplot(pz ~ px | stand, data=pitchfx, groups=pitch_type,
          main =list(label="Emplacement des lancers en fonction de la position du frappeur",
                     cex=0.85),
          auto.key = myKey,
          aspect = "iso",
          xlim = c(-2.2, 2.2),
          ylim = c(0, 5),
          xlab = "Coordonnee horizontale\n(en pieds du milieu du marbre)",
          ylab = "Coordonnee verticale\n(en pieds a partir du sol)",
          panel = function(...){
            panel.xyplot(...)
            panel.rect(zone.interieur, zone.bas, zone.exterieur, zone.haut,
                       border = "black", lty = 3)
        }
      )
   }  
 })
 
## output hitter section 
  output$frappeur <- renderPlot({
    hitfx <- frappeur[which(frappeur$batter==input$id.frappeur),]
 
## error message
    if(nrow(hitfx)==0){
      formule.text.frappeur <- reactive({
        paste("Malheureusement, ce numero de joueur n'est pas dans notre base de donnees")
      })
      
      output$caption.frappeur <- renderText({
        formule.text.frappeur()
      }) 
    }  
    
    else{
      output$caption.frappeur <- renderText({""})
      
  ## Reduce nb Level to 3 == "erreur", "retire","sur_les_buts)
    categorie.frappe <- rep(NA,length(hitfx[,1]))
    for(i in 1:length(hitfx$des)){
      categorie.frappe[i]=ifelse(hitfx$des[i]=="Flyout" |hitfx$des[i]== "Bunt Pop Out" |
                                   hitfx$des[i]=="Groundout" |hitfx$des[i]=="Pop Out"|
                                   hitfx$des[i]=="Bunt Groundout"| hitfx$des[i]=="Lineout" |
                                   hitfx$des[i]=="Bunt Lineout","retire",
                                 
                                 
                                 ifelse(hitfx$des[i]=="Field Error" | hitfx$des[i]=="Error" |
                                          hitfx$des[i]=="Runner interference" | hitfx$des[i]=="Batter interference" |
                                          hitfx$des[i]=="Catcher interference"| 
                                          hitfx$des[i]=="Fan interference","rrreur",
                                        
                                        ifelse(hitfx$des[i]=="Home Run" | hitfx$des[i]=="Single" |
                                                 hitfx$des[i]=="Double" | hitfx$des[i]=="Triple","sur_les_buts",NA)))}
    
    hitfx <- cbind(hitfx,categorie.frappe)
    as.factor(hitfx$categorie.frappe)
    
## Draw the hitter graph
    qplot(data=hitfx,x = x,y=y,
          xlab = "Coordonnee horizontale",ylab= "Coordonnee verticale",     
          main = "Trajectoire des frappes")                           +
      geom_point(aes(color=categorie.frappe))                        +
      coord_equal()                                                  +
      geom_segment(x=125,xend=225,y=225,yend=125)                    +
      geom_segment(x=125,xend=25,y=225,yend=125)                     +
      scale_y_continuous(limits=c(0, 250))                           +
      scale_x_continuous(limits=c(0, 250))
    }
  })
})


