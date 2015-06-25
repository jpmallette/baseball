##############################################################
# 6-613-11 Statistical analysis for data analysis            #
# Automone 2014                                              #
#                                                            #
# Author                                                     #
# First         Last                                         #
# Dave          Beyokol                                      #
# Jean-Philippe Mallette                                     #
#                                                            #
#                                                            #
# Optimal team formation under constraint &                  #
# descriptive statistics of player                           #                       #  
#                                                            #
#                                                            #
#                                                            #
##############################################################

install.packages("reshape") 
install.packages("plyr") 
install.packages("Rglpk") 
install.packages("dplyr") 
install.packages("pitchRx") 
install.packages("shiny") 
install.packages("ggplot2") 
install.packages("lattice") 
install.packages("RSQLite") 
install.packages("sqldf")   
library(sqldf)   
library(RSQLite) 
library(ggplot2) 
library(lattice) 
library(dplyr) 
library(pitchRx) 
library(shiny) 
library(reshape) 
library(plyr) 
library(Rglpk) 

# Put all the CSV file in the repository of your choice
setwd("C:/R_Files")

# Read the file "Batting.csv" that contain the players ID 
# for the year 2013
id_jo <- read.csv(file="Batting.csv",head=TRUE,sep=",")
id_jo2 <- subset(id_jo, yearID=="2013", select=c(playerID))
id_jo3 <- unique(id_jo2)

# Read the file "Master.csv" that countain the first and last name of players
nom_pre_jo <- read.csv(file="Master.csv",head=TRUE,sep=",")
nom_pre_jo2 <- subset(nom_pre_jo, select=c(playerID, nameFirst, nameLast))
name <- paste(nom_pre_jo2$nameFirst, nom_pre_jo2$nameLast, sep = " ")
nom_pre_jo3 <- cbind(nom_pre_jo2,name)

# read the file "fielding that contain the folowing fields
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
pos_jo2 <- subset(pos_jo, yearID=="2013", select=c(playerID, POS, G))

# We only consider that a player has a unique field position. 
# The position selected is the position with the maximum of game played

pos_jo3 <- ddply(pos_jo2,~playerID,function(x){x[which.max(x$G),]}) 

# Cleaning and transformation of categorical variables
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


# Read the file "Salaries.csv" that contain the id of each player
sal_jo <- read.csv(file="Salaries.csv",head=TRUE,sep=",")
sal_jo2 <- subset(sal_jo, yearID=="2013", select=c(playerID, salary))
sal_jo3 <- ddply(sal_jo2,~playerID,function(x){x[which.max(x$salary),]}) # Unicite des salaires pour chaque joueur

# Read the file of "playerid_list_mlbcode_clean.csv" that contain the MLB code of each player
mlb_jo <- read.csv(file="playerid_list_mlbcode_clean.csv",head=TRUE,sep=",")
mlb_jo2 <- subset(mlb_jo,select=c(FIRSTNAME,LASTNAME,MLBCODE))
name <- paste(mlb_jo2$FIRSTNAME,mlb_jo2$LASTNAME, sep = " ")
mlb_jo3 <- cbind(mlb_jo2,name)
mlb_jo4 <- subset(mlb_jo3, select=c(name, MLBCODE))


# Read the file "FanGraphs Leaderboard.csv" that contain the WAR of each player 
# WAR = combination of offensive and defensive values of players
war_jo <- read.csv(file="FanGraphs Leaderboard.csv",head=TRUE,sep=",")
war_jo <- rename(war_jo, c(ï..Name="name"))
war_jo2 <- subset(war_jo, select=c(name, WAR,G))
war_jo3 <- ddply(war_jo2,~name,function(x){x[which.max(x$G),]})
war_jo4 <- subset(war_jo3, select=c(name, WAR))


# Merge all data frame
Baseball_Data <- merge(id_jo3,nom_pre_jo3,by="playerID")
Baseball_Data <- merge(Baseball_Data,pos_jo4,by="playerID")
Baseball_Data <- merge(Baseball_Data,sal_jo3,by="playerID")
Baseball_Data <- merge(Baseball_Data,mlb_jo4,by="name")
Baseball_Data <- merge(Baseball_Data,war_jo4,by="name")
Baseball_Data$nb=1 # Cette colonne nous servira pour le nombre max de joueurs dans le modèle d'optimisation linéaire


# Optimal team creation that maximize sum of WAR (linear optimnisation) 
# Constraint are :
# budget of 400M$
# maximum of 3 players for a given positions
# maximum of 25 players in total

# Column needed for the linear optimisation  
joueurID<-Baseball_Data[,1]
pos<-Baseball_Data[,7] 
batting<-Baseball_Data[,19]
salaire<-Baseball_Data[,17]
nb_joueur<-Baseball_Data[,20]

# maximal Budget 
budget=100000000
nbre_joueur_max=25

nombreDeJoueurs <- length(joueurID)

# Objective -> maximze sum of WAR
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

# Constraints
# Maximum of 3 players for a given positions
dir <- c("<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=",">=",">=",">=",">=",">=",">=",">=",">=",">=")
constr <- c(3,3,3,3,3,3,3,3,3, budget,nbre_joueur_max,2,2,2,2,2,2,2,2,2)

# Linear optimisation function
res<- Rglpk_solve_LP(obj = toMaximize, mat = A, dir = dir, rhs = constr,types = var.types, max = TRUE)

Baseball_Data_2 <- cbind(Baseball_Data,res)
Solution_optimale <- subset(Baseball_Data_2, solution==1, select=c(name, playerID, MLBCODE, Position, salary, WAR))

#Print maximal WAR and name of selected player
cat("Maximum WAR",res$optimum)
cat("L'équipe optimale est composée des joueurs suivants:")
Solution_optimale

# Save the optimal team file 
# To file will be useful to analyse the trajectory of pitchs and hits
saveRDS(Solution_optimale,"C:\\R_Files\\Solution_optimale.rds")

# Read the RDS File 
solution <-  readRDS("Solution_optimale.rds")

## Consider only the unique hiter to create a list in shiny 

liste.frappeur <- c(solution$MLBCODE)

## Consider only the unique pitcher to create a list in shiny

liste.lanceur.totale <- subset(solution, solution$Position == "IsPitcher")
liste.lanceur <- liste.lanceur.totale$MLBCODE 


# download detailed pitch and hit data with pitchfx packages

MLB <- src_sqlite("MLB.2013",create=T)

  c("inning/inning_hit.xml","inning/inning_all.xml")
      scrape(start = "2013-03-31", end = "2013-10-30", 
          suffix = files, connect = MLB$con)

# Connect to the data base

MLB <- src_sqlite("MLB.2013")

### run the shiny application

runApp("shiny")





