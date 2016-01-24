library(shiny)
library(shinyjs)

shinyUI(fluidPage(theme = "bootstr.css",
  
  navbarPage("",
             tabPanel("Sprawdź zalecenia",
                      
                      titlePanel("Kontrola podawanych leków"),
                      
                      fluidRow(column(4, wellPanel(
                        h3("Pacjent"),
                        hr(),
                        textInput('fname',
                                  label = 'Imię'
                        ),
                        
                        textInput('sname',
                                  label = 'Nazwisko'
                        ),
                        textInput('id',
                                  label = "PESEL"
                        ),
                        fluidRow(column(4, offset = 5,(
                          actionButton('sub',
                                       label = 'Wyszukaj',
                                       icon = icon("search")
                          )
                        )))
                      )),
                      
                      column(8, 
                             fluidRow(column(4, 
                                             h4(format(Sys.time(), "%d-%b-%Y %H:%M"))
                             ),
                             column(6, offset = 2,  
                                    uiOutput("color"), uiOutput("image")
                             ),
                             
                             fluidRow(column(6, offset = 1, 
                                             h3("Dane pacjenta"),
                                             tableOutput("retable"), 
                                             useShinyjs(),
                                             uiOutput("checks")))
                             ))
                      
                      
                      )
             ),
             tabPanel("Dodaj receptę",
                      
                      titlePanel("Dodawanie nowego leku"),
                      
                      fluidRow(column(4, wellPanel(
                        h3("Pacjent"),
                        hr(),
                        textInput('infname',
                                  label = 'Imię'
                        ),
                        
                        textInput('insname',
                                  label = 'Nazwisko'
                        ),
                        textInput('inid',
                                  label = "PESEL"
                        ),
                        textInput('ingodzina',
                                  label = "Godzina podania"
                        ),
                        textInput('inlek',
                                  label = "Nazwa leku"
                        ),
                        textInput('indawka',
                                  label = "Dawka i jednostka"
                        ),
                        fluidRow(column(4, offset = 5,(
                          actionButton('drugsub',
                                       label = 'Dodaj',
                                       icon = icon("fa fa-check-square-o")
                        )
                      )))
                    )),
                    column(6, tags$span(style="color:green", h2(textOutput("dodano"))))
             )),
             tabPanel("Usuń receptę",
                      
                      titlePanel("Usuwanie przepisanego leku"),
                      
                      fluidRow(column(4, wellPanel(
                        h3("Pacjent"),
                        hr(),
                        textInput('inidrm',
                                  label = "PESEL"
                        ),
                        textInput('inlekrm',
                                  label = "Nazwa leku"
                        ),
                        textInput('ingodzinarm',
                                  label = "Godzina podania",
                                  value = ""
                        ),
                        fluidRow(column(4, offset = 5,(
                          actionButton('drugsubrm',
                                       label = 'Usuń',
                                       icon = icon("fa fa-minus-square-o")
                          )
                        )))
                      )),
                      column(6, tags$span(style="color:red", h2(textOutput("usunieto"))))
                      
                      )
                      
             )
  
)))