function(session, input, output) {
  onStop(function(){
    dbDisconnect(con)
  })
  
  
  observeEvent(
    input$page,
    updateSelectInput(session, "questions", "question",
                      choices =  c(setNames(recup_questions_formation2(input$page)$id, recup_questions_formation2(input$page)$libelle))
    ),
    ignoreNULL = F
  )
  output$hist2 <- renderPlot({
    tentative_graph(input$page, input$questions, input$bins, input$color)
  })

  
  # Boutton pour télécharger le graph en PNG
  output$downloadPlotPNG <- downloadHandler(
    filename = function() { paste('Graph_Question_', recup_questions_formation2(input$page)$libelle, '.png', sep='') },
    content = function(file) {
      ggsave(file,)
    })
  
  # Boutton pour télécharger le graph en PDF
    output$downloadPlotPDF <- downloadHandler(
      filename = function() { paste('Graph_Question_', recup_questions_formation2(input$page)$libelle, '.pdf', sep='') },
      content = function(file) {
        ggsave(file,)
      })
   
  #Dataframe
  output$df <- renderDataTable({
    datatable(recup_(input$name2), extensions="Buttons", options=list(
      dom='Bfrtip', pageLength = 100 ,buttons =c('copy','csv','excel','pdf','print'), 
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#f85d28', 'color': '#fff'});",
        "}")))
  })
  #outputText if a text question is selected
  output$commentaires <- renderDataTable({
    datatable(
    get_data(input$page, input$questions) %>%
      select(reponse),extensions="Buttons", options=list(
        dom='Bfrtip', pageLength = 100 ,buttons =c('copy','csv','excel','pdf','print'), 
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#f85d28', 'color': '#fff'});",
          "}")))
  })
  # conditionne l'affichage des panels sur Data
  output$graph_or_text <- reactive({
    if(input$questions %in% list_of_text_question_id()$id){
      return("texte")
    }else if (input$questions == "vide"){
      return("nothing")
    }else {
      return("graph")
    }
  })
  
  outputOptions(output, "graph_or_text", suspendWhenHidden = FALSE)

  onStop(function(){
    dbDisconnect(con)
  })
}
