library(RMySQL)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)

con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
#lapply(dbListConnections(MySQL()), dbDisconnect)




recup_formation <- function(){
  tbl(con,sql("SELECT id, libelle as formation FROM choix WHERE question_id=14 LIMIT 11")
  )%>% collect
}

#recup_formation()
#récup nom de tous les élèves2
recup_nom_eleves2 <- function(){
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










list_of_text_question_id <- function(){
  tbl(con, sql("SELECT  id FROM evaluation.question where type like 'texte_%'")
  ) %>% 
    collect
}
list_of_text_question_id()
recup_questions_formation2<- function(choix_id){
  
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
#Graphique de tous les étudiants (toutes formations confondues)
dessine_graphique <- function(id_question){
  
  tbl(con,sql(paste("select score from question
  inner join reponse 
  on question.id = reponse.question_id
  and question.id =", id_question ))) %>% collect() %>% 
    ggplot( aes(x = score))+
    geom_histogram()
}


#Graophique des réponses des étudiants d'une formation
tentative_graph <- function(id_formation, id_question, nb_bins, couleur){
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
    geom_histogram(bins = nb_bins, fill = couleur)
}


#dessine_graphique()
# récupère les questions selon le session ID de l'étudiant 
recup_ <- function(id){
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


# Récupère le texte ou le score pour une formation donnée
get_data <- function(id_formation, id_question){
  tbl(con,
      sql(paste("select reponse.score, question.libelle, reponse.texte as reponse
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

?geom_histogram
