library(shiny)
library(tidyverse)
library(DT)
con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
#lapply(dbListConnections(MySQL()), dbDisconnect)
#

# recup_questions_formation<- function(choix_id){
#   con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
#   tbl(con,sql(paste("select distinct question.libelle as question from reponse 
#                                 inner join question on question.id = reponse.question_id
#                                 inner join session on session.id = reponse.session_id
#                                 WHERE session.id in (SELECT DISTINCT session.id
#                                         FROM session 
#                                         inner join reponse on session.id = reponse.session_id
#                                         where reponse.question_id = 14
#                                         AND reponse.choix_id= ",choix_id,")"))
#   )%>% collect
# }
recup_questions_formation2<- function(choix_id){
  #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("select distinct(question.id), libelle from reponse
                                inner join question on question.id = reponse.question_id
                                inner join session on session.id = reponse.session_id
                                WHERE session.id in (SELECT DISTINCT session.id
                                        FROM session 
                                        inner join reponse on session.id = reponse.session_id
                                        where reponse.question_id = 14
                                        AND reponse.choix_id = ",choix_id,")"))
  )%>% collect
}

#recup_questions_formation2(5)$libelle

dessine_graphique <- function(id_question){
  #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("select score from question
  inner join reponse 
  on question.id = reponse.question_id
  and question.id =", id_question ))) %>% collect() %>% 
    ggplot( aes(x = score))+
    geom_histogram()
}

tentative_graph <- function(id_formation, id_question){
  tbl(con,
      sql(paste("select reponse.score, question.libelle
          FROM choix
          INNER JOIN reponse as reponse_formation
          	ON reponse_formation.choix_id = choix.id
          	AND reponse_formation.question_id = 14
          INNER JOIN reponse
          	ON reponse.session_id = reponse_formation.session_id
          INNER JOIN question
          	ON question.id = reponse.question_id
          INNER JOIN page
          	ON page.id = question.page_id
          and choix.id =", id_formation, "and reponse.question_id = ", id_question))
  ) %>% collect() %>%
    ggplot( aes(x = score))+
    geom_histogram(bins = 10)
}

#dessine_graphique()

# récupère les questions selon le session ID de l'étudiant 
recup_ <- function(id){
  #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("SELECT question.libelle as question , concat(coalesce(reponse.texte,''),coalesce(reponse.date,''),coalesce(reponse.score,'')) as reponse from reponse
                              inner join question on question.id = reponse.question_id
                              inner join session on session.id = reponse.session_id
                              WHERE session.id in (SELECT DISTINCT session.id
                                      FROM session 
                                      inner join reponse on session.id = reponse.session_id
                                      where reponse.question_id = 14
                                      and session.id =",id,")"))
  )%>% collect
}


get_data <- function(id_formation, id_question){
  tbl(con,
      sql(paste("select reponse.score, question.libelle, reponse.texte
FROM choix
INNER JOIN reponse as reponse_formation
ON reponse_formation.choix_id = choix.id
AND reponse_formation.question_id = 14
INNER JOIN reponse
ON reponse.session_id = reponse_formation.session_id
INNER JOIN question
ON question.id = reponse.question_id
INNER JOIN page
ON page.id = question.page_id
and choix.id = ",id_formation," and reponse.question_id = ",id_question))
  ) %>% collect()
}




function(session, input, output) {
 
  observeEvent(
    input$page,
    updateSelectInput(session, "questions", "question",
              choices =  c(setNames(recup_questions_formation2(input$page)$id, recup_questions_formation2(input$page)$libelle)), 
              print(input$page))
  )
  # output$osef <- renderText({
  #   print(input$page)
  # })
  
  output$hist <- renderPlot({
    print(input$questions)
    dessine_graphique(input$questions)
  })
  
  output$hist2 <- renderPlot({
    print(input$questions)
    tentative_graph(input$page, input$questions)
  })
  
   reactive({
    print(input$page)
  })
  
   reactive({
    print(input$questions)
  })
   
  #Dataframe
  output$df <- renderDataTable({
    datatable(recup_(input$name2), extensions="Buttons", options=list(
      dom='Bfrtip', buttons =c('copy','csv','excel','pdf','print')))
  })
  
  #outputText if a text question is selected
  output$commentaires <- renderDataTable({
    get_data(input$page, input$questions) %>%
      select(texte)
  })
  
  onStop(function(){
    dbDisconnect(con)
    print("ok")
  })

}













# #récup nom de tous les élèves
# recup_nom_eleves <- function(){
#   con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
#   tbl(con,sql("SELECT reponse_nom.texte AS nom, reponse_prenom.texte AS prenom
#         FROM reponse
#         INNER JOIN reponse AS reponse_nom
#             ON reponse_nom.session_id = reponse.session_id
#             AND reponse_nom.question_id = 2
#         INNER JOIN reponse AS reponse_prenom
#             ON reponse_prenom.session_id = reponse.session_id
#             AND reponse_prenom.question_id = 3
#         WHERE reponse.question_id = 14 AND reponse.choix_id = 2")
#   )%>% collect
# }

# choices = recup_questions_formation2(input$page)$libelle)


#fonction pour update select question
# recup_question_page <- function(id_page){
#   #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
#   tbl(con,sql(paste("select id, libelle from question where page_id=",id_page))) %>% collect
# }

#Si une formation est selectionne -> nom - prénom des élèves
  
  # output$text <- renderDataTable({
  #   print(recup_questions_formation(input$page)$question)
  #   recup_questions_formation(input$page)$question
  # })

#test <- 5
#c(setNames(recup_questions_formation(test)$id, recup_questions_formation(test)$libelle))

#recup_questions_formation(5)$id
#recup_question_page(1)$libelle
