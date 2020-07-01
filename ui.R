library(RMySQL)
library(tidyverse)
library(shiny)

con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")

recup_formation <- function(){
  #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql("SELECT id, libelle as formation FROM choix WHERE question_id=14 LIMIT 11")
  )%>% collect
  
}
#recup_formation()


#récup nom de tous les élèves2
recup_nom_eleves2 <- function(){
  #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql("SELECT reponse_prenom.session_id as id, reponse_nom.texte AS nom, reponse_prenom.texte as prenom
        FROM reponse
        INNER JOIN reponse AS reponse_nom
            ON reponse_nom.session_id = reponse.session_id
            AND reponse_nom.question_id = 2
        INNER JOIN reponse AS reponse_prenom
            ON reponse_prenom.session_id = reponse.session_id
            AND reponse_prenom.question_id = 3
        WHERE reponse.question_id = 14")
  )%>% collect
}


shinyUI(fluidPage(
  headerPanel("Dahboard reponses au formulaire"),
  sidebarPanel(
    conditionalPanel(condition = "input.tabselected ==1",
                     selectInput("page", "Choisissez une page",
                                 choices = c(setNames(recup_formation()$id, recup_formation()$formation))
                     ),
                     selectInput("questions","Questions",
                                 choices = "", selected = ""
                     ),
                     br(),
                     sliderInput("bins", "1. Select number of bins", min = 5, max = 25, value = 15),
                     br(),
                     radioButtons("color", "2. Select the color", choices = c("Green", "Red", "Yellow"), selected = "Green"),
    ),
    
    conditionalPanel(condition = "input.tabselected ==2",
                     selectInput("name2", "Name",
                                 choices =  c(setNames(recup_nom_eleves2()$id, paste(recup_nom_eleves2()$nom, recup_nom_eleves2()$prenom, sep=" "))))
    ),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Statistique Formations", 
               value = 1, 
               plotOutput("hist"),
               plotOutput("hist2"),
               dataTableOutput("commentaires")
      ),
      # tabPanel(
      #   "Data", value = 3,
      #   conditionalPanel(condition = "input.questions== ''", tableOutput("data")),
      #   conditionalPanel(condition = "input.option==2", verbatimTextOutput("structure")),
      #   conditionalPanel(condition = "input.option==3", verbatimTextOutput("summ"))
      # ),
      tabPanel("Statistiques etudiants", value = 2,
               dataTableOutput("df")
      ),
      id ='tabselected'
    )
  )
)
)


