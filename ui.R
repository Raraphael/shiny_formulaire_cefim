shinyUI(fluidPage(
  
  # change le thème de la page
  theme = shinytheme("united"),

  # Permet d'insérer un logo et un titre grâce aux requêtes HTML
  navbarPage(tags$script(HTML("var header = $('.navbar > .container-fluid');
  header.append('<div style=\"float:left\"><ahref=\"www.cefim.eu\"><img src=\"test.png\" alt=\"alt\" style=\"float:left;width:55px;height:55px;padding-top:5px;\"> </a>`</div> <div><h2 style = \"color:white;\">Dahboard reponses au formulaire</h2></div>');"))
             ),   
  
  
  chooseSliderSkin("Modern"),
  setSliderColor('#f85d28',1),
  
  
  
  sidebarPanel(
    conditionalPanel(condition = "input.tabselected ==3",
                     selectInput("page", "Choisissez une formation",
                                 choices = c(setNames(recup_formation()$id, recup_formation()$formation)), 
                                 selected = 2
                     ),
                     selectInput("questions","Choisissez une question",
                                 choices = " ", selected = " "
                     ),
                     sliderInput("bins", "1. Select number of bins", min = 5, max = 25, value = 15),
                     br(),
                     radioButtons("color", "2. Select the color", choices = c("Green", "Red", "Yellow", "Grey", "#f85d28"), selected = "#f85d28"),
    ),
    conditionalPanel(condition = "input.tabselected ==2",
                     selectInput("name2", "Name",
                                 choices =  c(setNames(recup_nom_eleves2()$id, paste(recup_nom_eleves2()$nom, recup_nom_eleves2()$prenom, sep=" "))))
                    
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Data", value = 3,
        conditionalPanel(condition = "output.graph_or_text == 'texte'", dataTableOutput("commentaires")),
        conditionalPanel(condition = "output.graph_or_text == 'nothing'", verbatimTextOutput("error_msg")),
        conditionalPanel(condition = "output.graph_or_text == 'graph'", plotOutput("hist2"), downloadButton('downloadPlotPNG', 'Télécharger PNG'), downloadButton('downloadPlotPDF', 'Télécharger PDF'))
      ),
      tabPanel("Statistiques etudiants", value = 2,
               dataTableOutput("df")
      ),
      id ='tabselected'
    )
  )
))



