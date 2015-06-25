##############################################################
# 6-613-11 Logiciels statistiques pour l'analyse de donn�es  #
# Automone 2014                                              #
#                                                            #
# Pr�nom        Nom      Matricule                           #
# Dave          Beyokol  11054670                            #
# Jean-Philippe Mallette 11139865                            #
# Dmitry        Masyutin 11191672                            #
#                                                            #
# Projet d'optimisation sous contraintes et descriptions     #
# statistiques des joueurs d'une �quipe de Baseball          #
#                                                            #
# Pr�sent� � Jean-Fran�ois Plante                            #
##############################################################

install.packages("reshape") # Seulement la premi�re fois 
install.packages("plyr") # Seulement la premi�re fois 
install.packages("Rglpk") # Seulement la premi�re fois
install.packages("dplyr") # Seulement la premi�re fois
install.packages("pitchRx") # Seulement la premi�re fois
install.packages("shiny") # Seulement la premi�re fois
install.packages("ggplot2") # Seulement la premi�re fois
install.packages("lattice") # Seulement la premi�re fois
install.packages("RSQLite") # Seulement la premi�re fois
install.packages("sqldf")   # Seulement la premi�re fois
library(sqldf)   # � toutes les fois que R est red�marr�
library(RSQLite) # � toutes les fois que R est red�marr�
library(ggplot2) # � toutes les fois que R est red�marr�
library(lattice) # � toutes les fois que R est red�marr�
library(dplyr) # � toutes les fois que R est red�marr�
library(pitchRx) # � toutes les fois que R est red�marr�
library(shiny) # � toutes les fois que R est red�marr�
library(reshape) # � toutes les fois que R est red�marr�
library(plyr) # � toutes les fois que R est red�marr�
library(Rglpk) # � toutes les fois que R est red�marr�

# Mettre les fichiers CSV dans le r�pertoire de votre choix 
# Ex�cuter la requ�te ci-dessous apr�s avoir renomm� le chemin d'acc�s 
setwd("C:/R_Files")



# Lecture du fichier "Batting.csv" qui contient l'ID des joueurs
# Cr�ation du data frame contenant la liste des IDs pour l'ann�e 2013
id_jo <- read.csv(file="Batting.csv",head=TRUE,sep=",")
id_jo2 <- subset(id_jo, yearID=="2013", select=c(playerID))
id_jo3 <- unique(id_jo2)

# Lecture du fichier "Master.csv" qui contient le pr�nom et le nom des joueurs
# Cr�ation du data frame contenant l'ID, le pr�nom et le nom
nom_pre_jo <- read.csv(file="Master.csv",head=TRUE,sep=",")
nom_pre_jo2 <- subset(nom_pre_jo, select=c(playerID, nameFirst, nameLast))
name <- paste(nom_pre_jo2$nameFirst, nom_pre_jo2$nameLast, sep = " ")
nom_pre_jo3 <- cbind(nom_pre_jo2,name)

# Lecture du fichier "Fielding.csv" qui contient la position des joueurs
# Cr�ation du data frame contenant l'ID, la position du joueur et le flag pour chacune de ces positions
# 1B = first_base
# 2B = second_base
# 3B = third_base
# C = catcher
# CF = center_field
# LF = left_field
# P = pitcher
# RF = right_field
# SS = short_stop



pos_jo <- read.csv(file="Fielding.csv",head=TRUE,sep=",")
pos_jo2 <- subset(pos_jo, yearID=="2013", select=c(playerID, POS, G)) # Position de chaque joueur en 2013 avec le nombre de matchs
pos_jo3 <- ddply(pos_jo2,~playerID,function(x){x[which.max(x$G),]}) # Unicit� des positions via le nombre maximal de matchs jou�s


first_base <- ifelse(pos_jo3$POS=="1B",1,0)
second_base <- ifelse(pos_jo3$POS=="2B",1,0)
third_base <- ifelse(pos_jo3$POS=="3B",1,0)
catcher <- ifelse(pos_jo3$POS=="C",1,0)
center_field <- ifelse(pos_jo3$POS=="CF",1,0)
left_field <- ifelse(pos_jo3$POS=="LF",1,0)
pitcher <- ifelse(pos_jo3$POS=="P",1,0)
right_field <- ifelse(pos_jo3$POS=="RF",1,0)
short_stop <- ifelse(pos_jo3$POS=="SS",1,0)

Position <- ifelse(pos_jo3$POS=="1B","IsFirstBase",
                   ifelse(pos_jo3$POS=="2B","IsSecondBase",
                          ifelse(pos_jo3$POS=="3B","IsThirdBase",
                                 ifelse(pos_jo3$POS=="C","IsCatcher",
                                        ifelse(pos_jo3$POS=="CF","IsCenterField",
                                               ifelse(pos_jo3$POS=="LF","IsLeftField",
                                                      ifelse(pos_jo3$POS=="P","IsPitcher",
                                                             ifelse(pos_jo3$POS=="RF","IsRightField",
                                                                    "IsShortStop"))))))))
                                                        

pos_jo4 <- cbind(pos_jo3,Position,first_base,second_base,third_base,catcher,center_field,
                 left_field,pitcher,right_field,short_stop)



# Lecture du fichier "Salaries.csv" qui contient l'ID de chaque joueur
sal_jo <- read.csv(file="Salaries.csv",head=TRUE,sep=",")
sal_jo2 <- subset(sal_jo, yearID=="2013", select=c(playerID, salary))
sal_jo3 <- ddply(sal_jo2,~playerID,function(x){x[which.max(x$salary),]}) # Unicite des salaires pour chaque joueur


# Lecture du fichier "playerid_list_mlbcode_clean.csv" qui contient le code MLB de chaque joueur
# Cr�ation du data frame contenant le nom complet du joueur et son code MLB
mlb_jo <- read.csv(file="playerid_list_mlbcode_clean.csv",head=TRUE,sep=",")
mlb_jo2 <- subset(mlb_jo,select=c(FIRSTNAME,LASTNAME,MLBCODE))
name <- paste(mlb_jo2$FIRSTNAME,mlb_jo2$LASTNAME, sep = " ")
mlb_jo3 <- cbind(mlb_jo2,name)
mlb_jo4 <- subset(mlb_jo3, select=c(name, MLBCODE))


# Lecture du fichier "FanGraphs Leaderboard.csv" qui contient la valeur WAR de chaque joueur
# WAR = combinaison des valeurs offensive et defensive des joueurs
# Cr�ation du data frame contenant le nom complet du joueur et sa valeur WAR
war_jo <- read.csv(file="FanGraphs Leaderboard.csv",head=TRUE,sep=",")
war_jo <- rename(war_jo, c(�..Name="name"))
war_jo2 <- subset(war_jo, select=c(name, WAR,G))
war_jo3 <- ddply(war_jo2,~name,function(x){x[which.max(x$G),]}) # Unicit� des cotes WAR pour chaque joueur
war_jo4 <- subset(war_jo3, select=c(name, WAR))


# Cr�ation du data frame contenant la combinaison de toutes les donnees
Baseball_Data <- merge(id_jo3,nom_pre_jo3,by="playerID")
Baseball_Data <- merge(Baseball_Data,pos_jo4,by="playerID")
Baseball_Data <- merge(Baseball_Data,sal_jo3,by="playerID")
Baseball_Data <- merge(Baseball_Data,mlb_jo4,by="name")
Baseball_Data <- merge(Baseball_Data,war_jo4,by="name")
Baseball_Data$nb=1 # Cette colonne nous servira pour le nombre max de joueurs dans le mod�le d'optimisation lin�aire





# Formation d'une �quipe de joueurs qui donnent la valeur maximale de WAR en respectant les contraintes suivantes:
# Le budget de 400M$
# Pas plus de 3 joueurs pour chacune des positions
# Pas plus de 25 joueurs au total


# Attribution des valeurs aux arguments de la fonction que sera utilis�e pour l'optimisation
joueurID<-Baseball_Data[,1]
pos<-Baseball_Data[,7] 
batting<-Baseball_Data[,19]
salaire<-Baseball_Data[,17]
nb_joueur<-Baseball_Data[,20]

# Budget maximal
budget=100000000
nbre_joueur_max=25

nombreDeJoueurs <- length(joueurID)

# Objectif -> maximiser le total de WAR
toMaximize <- batting
var.types <- rep("B", nombreDeJoueurs)

A <- rbind(as.numeric(pos == "IsRightField"), 
           as.numeric(pos == "IsLeftField"), 
           as.numeric(pos == "IsCenterField"), 
           as.numeric(pos == "IsPitcher"), 
           as.numeric(pos == "IsCatcher"), 
           as.numeric(pos == "IsFirstBase"), 
           as.numeric(pos == "IsSecondBase"), 
           as.numeric(pos == "IsThirdBase"),
           as.numeric(pos == "IsShortStop"), 
           salaire,
           nb_joueur,
           as.numeric(pos == "IsRightField"), 
           as.numeric(pos == "IsLeftField"), 
           as.numeric(pos == "IsCenterField"), 
           as.numeric(pos == "IsPitcher"), 
           as.numeric(pos == "IsCatcher"), 
           as.numeric(pos == "IsFirstBase"), 
           as.numeric(pos == "IsSecondBase"), 
           as.numeric(pos == "IsThirdBase"),
           as.numeric(pos == "IsShortStop"))

# Les contraintes
# Maximum de 3 joueurs pour chacune des positions
dir <- c("<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=",">=",">=",">=",">=",">=",">=",">=",">=",">=")
constr <- c(3,3,3,3,3,3,3,3,3, budget,nbre_joueur_max,2,2,2,2,2,2,2,2,2)

# Fonction d'optimisation
res<- Rglpk_solve_LP(obj = toMaximize, mat = A, dir = dir, rhs = constr,types = var.types, max = TRUE)

Baseball_Data_2 <- cbind(Baseball_Data,res)
Solution_optimale <- subset(Baseball_Data_2, solution==1, select=c(name, playerID, MLBCODE, Position, salary, WAR))
# joueurID[res$solution==1]


#Mettre sur l'�cran le WAR maximal et les noms de joueurs s�lectionn�s
cat("Maximum WAR",res$optimum)
cat("L'�quipe optimale est compos�e des joueurs suivants:")
Solution_optimale


# Cr�ation du fichier RDS contenant l'�quipe de baseball optimale selon les contraintes ci-dessus
# Ce fichier permettra d'afficher via Shiny les trajectoires des lanceurs et des frappeurs
saveRDS(Solution_optimale,"C:\\R_Files\\Solution_optimale.rds")


# Lecture du fichier RDS 
solution <-  readRDS("Solution_optimale.rds")

## Creation d'un objet contenant uniquement les frappeurs afin de cr�er une liste 
## d�roulante dans shiny

liste.frappeur <- c(solution$MLBCODE)

## Creation d'un fichier contenant uniquement les lanceurs afin de cr�er une liste 
## d�roulante dans shiny

liste.lanceur.totale <- subset(solution, solution$Position == "IsPitcher")
liste.lanceur <- liste.lanceur.totale$MLBCODE 

## connect� � la base de donnees (test afin de s'assurer quelle 
## est fonctionnelle)

MLB <- src_sqlite("MLB.2013")

### Lancer l'application shiny

runApp("shiny")

## Afin d'aller recolter les donnees manuellement

# MLB <- src_sqlite("MLB.2013",create=T)

#  c("inning/inning_hit.xml","inning/inning_all.xml")
#        scrape(start = "2013-03-31", end = "2013-10-30", 
#          suffix = files, connect = MLB$con)


