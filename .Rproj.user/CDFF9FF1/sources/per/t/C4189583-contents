library(RMySQL)
library(tidyverse)

con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
recup_titre_page <- function(){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con, sql("SELECT id,titre from page")) %>% collect()
}

recup_titre_page()$titre

recup_question_page <- function(id_page){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("select id, libelle from question where page_id=",id_page))) %>% collect
}

recup_question_page(1)$libelle

#Si une formation est selectionne -> nom - prénom des élèves
recup_nom_eleves_formation <- function(choix_id){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("SELECT reponse_nom.texte AS nom, reponse_prenom.texte AS prenom
        FROM reponse
        INNER JOIN reponse AS reponse_nom
            ON reponse_nom.session_id = reponse.session_id
            AND reponse_nom.question_id = 2
        INNER JOIN reponse AS reponse_prenom
            ON reponse_prenom.session_id = reponse.session_id
            AND reponse_prenom.question_id = 3
        WHERE reponse.question_id = 14 AND reponse.choix_id = ",choix_id))
  )%>% collect
}

recup_nom_eleves_formation(2)$nom 
#récup nom de tous les élèves
recup_nom_eleves <- function(){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql("SELECT reponse_nom.texte AS nom, reponse_prenom.texte AS prenom
        FROM reponse
        INNER JOIN reponse AS reponse_nom
            ON reponse_nom.session_id = reponse.session_id
            AND reponse_nom.question_id = 2
        INNER JOIN reponse AS reponse_prenom
            ON reponse_prenom.session_id = reponse.session_id
            AND reponse_prenom.question_id = 3
        WHERE reponse.question_id = 14 AND reponse.choix_id = 5")
      )%>% collect
}
  

recup_nom_eleves()

#récup nom de tous les élèves
recup_nom_eleves <- function(){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql("SELECT reponse_nom.texte AS nom, reponse_prenom.texte AS prenom
        FROM reponse
        INNER JOIN reponse AS reponse_nom
            ON reponse_nom.session_id = reponse.session_id
            AND reponse_nom.question_id = 2
        INNER JOIN reponse AS reponse_prenom
            ON reponse_prenom.session_id = reponse.session_id
            AND reponse_prenom.question_id = 3
        WHERE reponse.question_id = 14 AND reponse.choix_id = 5")
  )%>% collect
}

# récupère questions via le nom de la formation 
recup_questions_formation <- function(nom_formation){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("select libelle
from page
inner join question
ON question.page_id = page.id
where titre =",nom_formation))
  )%>% collect
}

recup_questions_formation('AEC')

nom_formation <- 'AEC'

paste('select libelle
from page
inner join question
ON question.page_id = page.id
where titre =',nom_formation)





recup_questions_formation2<- function(choix_id){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
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

recup_questions_formation2(5)$id

recup_formation <- function(){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql("SELECT id, libelle as formation FROM choix WHERE question_id=14 LIMIT 11")
  )%>% collect
  
}

dessine_graphique <- function(id_question){
  con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
  tbl(con,sql(paste("select score from question
  inner join reponse 
  on question.id = reponse.question_id
  and question.id =", id_question ))) %>% collect() %>% 
    ggplot( aes(x = score))+
    geom_histogram(bins = 10)
}


get_data <-


dessine_graphique(19)

tbl(con,sql(paste(("select score from question
  inner join reponse 
  on question.id = reponse.question_id
  and question.id = 19" )))) %>% collect() %>% 
  ggplot( aes(x = score))+
    geom_histogram(bins = 10)

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

tentative_graph(2,5)

# get_data <- function(id_formation, id_question){
#   tbl(con,
#       sql(paste("select reponse.score, question.libelle
#           FROM choix
#           INNER JOIN reponse as reponse_formation
#           	ON reponse_formation.choix_id = choix.id
#           	AND reponse_formation.question_id = 14
#           INNER JOIN reponse
#           	ON reponse.session_id = reponse_formation.session_id
#           INNER JOIN question
#           	ON question.id = reponse.question_id
#           INNER JOIN page
#           	ON page.id = question.page_id
#           and choix.id =", id_formation, "and reponse.question_id = ", id_question))
#   ) %>% collect()
# }
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


give_me_option <- reactive(){
  if( input$questions %in% list_of_text_question_id()$id){
    return ('texte')
  }else if ( input$questions  == ""){
    return ('pas_infos')
  }else {
    return ('graph')
  }
}

give_me_option(12)


# 
# is_empty(get_data(1,7))
# 
# is.na(get_data(5,19)$score)

if (any(c(1,2,3,4) %in% get_data(11,2)$score)) {
  #get_data(11,2)$score
  # j'envoi ces données pour le plot graph
}else if  (dim(get_data(11,2))[1] == 0) {
  print("pas d'info en base")
  # j'affiche un message d'erreur ou try / catch 
}else {
  #get_data(11,2)$texte
  #  render Text  
}

if(1 %in% list_of_text_question_id()$id){
  print("reponse texte j'outblut table")
}else if (1 == ""){
  print("Pas d'infos en base")
}else {
  print("on graph")
}



typeof(get_data(1,2))
list_of_text_question_id <- function(){
  tbl(con, sql("SELECT  id FROM evaluation.question where type like 'texte%'")
      ) %>% 
    collect
}
list_of_text_question_id()$id


#select id of text question
SELECT  id FROM evaluation.question
where type like 'texte%';

        
dim(get_data_texte(1,2))[1] == 0

(any(c(1,2,3,4) %in% get_data_texte(5,6)$score))
# a <- tbl(con, "question") %>% 
#   filter(id <= 3) %>% 
#   collect()
# 
# 
# tbl(con, sql("SELECT * FROM question WHERE id IN (10, 15)")) %>% 
#   show_query()
# 
# nrow(a)
# a
tbl(con, "reponse") %>% 
  group_by(question_id) %>%
  summarise(
    n = n(),
    mean_score = mean(score, na.rm = T)
  ) %>% 
  # show_query()
  collect()




c(recup_titre_page()$titre)

if (between(6, 5,15)){
  print('ok')
}else {
  print("not")
}

