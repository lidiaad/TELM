library(shiny)

library(dplyr)
library(shinyjs)

# validate functions
select_patient <- function(name, surname, pesel) {
  if (name == "" && surname == "" && pesel == "") {
    "Podaj imię i nazwisko lub PESEL"
  } else if (name == "" && surname!= "" && pesel == "") {
    "Podaj imię i nazwisko lub PESEL"
  } else if (name != "" && surname == "" && pesel == "")
    "Podaj imię i nazwisko lub PESEL"
}

insert_patient <- function(infname, insurname, inid, ingodzina, inlek, indawka) {
  if (infname == "" && insurname == "" && inid == "" && ingodzina == "" && inlek == "" && indawka == "") {
    "Podaj wszystkie informacje"
  }
}

delete_patient <- function(inidrm, inlekrm, ingodzinarm) {
  if (inidrm == "" || inlekrm == "" && (ingodzinarm == "" || ingodzinarm != "")) {
    "Podaj PESEL i nazwę leku, jeśli pacjent nie będzie go w ogóle przyjmował. %n Podaj PESEL, nazwę leku i godzinę podania do usunięcia, jeśli pozostałe godziny podania tego leku mają być zachowane."
  }
}


shinyServer(function(input, output, session) {

  data <- read.table("data/pacjenci.txt", header = TRUE, sep = ',', as.is = TRUE)
  data$PESEL <- as.character(data$PESEL)
  timenow <- as.numeric(unlist(strsplit(format(Sys.time(), "%H:%M"), ':')))
  
  #find patient
   tab1 <- eventReactive(input$sub, {
     validate(select_patient(input$fname, input$sname, input$id))
     if (input$id != "") 
      {
       pacjent <- filter(data, PESEL == input$id)
       properrecord <- pacjent
       validate(if(nrow(pacjent) == 0) "Nieprawidłowy PESEL") 
       timepatient <- as.numeric(unlist(strsplit(pacjent$godzina, ':')))
       if (nrow(pacjent) > 1)
       {
         properind <- which.min(abs(sapply(seq(1, length(timepatient), 2), 
                           function (x) (timepatient[x]+timepatient[x + 1]/60 - (timenow[1]+timenow[2]/60)))))
         properrecord <- pacjent[properind, ]
         }
     }
     validate(if ((input$fname != "" && input$sname == "") || (input$sname != "" && input$fname == "")) "Podaj imię i nazwisko lub PESEL") 
     if (input$id == "" && input$fname != "" && input$sname != "")
     {
       pacjent <- filter(data, imie == input$fname & nazwisko == input$sname)
       properrecord <- pacjent
       timepatient <- as.numeric(unlist(strsplit(pacjent$godzina, ':')))
       validate(if (nrow(pacjent) == 0) "Nieprawidłowe dane") 
       validate(if (length(unique(pacjent$PESEL)) != 1) "Więcej niż jeden pacjent o tym samym imieniu i nazwisku. Podaj PESEL")
       if (nrow(pacjent) > 1)
       {
         properind <- which.min(abs(sapply(seq(1, length(timepatient), 2), 
                                           function (x) (timepatient[x]+timepatient[x + 1]/60 - (timenow[1]+timenow[2]/60)))))
         properrecord <- pacjent[properind, ]
       }
     }
     if (input$id != "" && input$fname != "" && input$sname != "")
     {
       pacjent <- filter(data, imie == input$fname & nazwisko == input$sname & PESEL == input$id)
       properrecord <- pacjent
       validate(if (nrow(pacjent) == 0) "Nieprawidłowe dane") 
       timepatient <- as.numeric(unlist(strsplit(pacjent$godzina, ':')))
       if (nrow(pacjent) > 1)
       {
         properind <- which.min(abs(sapply(seq(1, length(timepatient), 2), 
                                           function (x) (timepatient[x]+timepatient[x + 1]/60 - (timenow[1]+timenow[2]/60)))))
         properrecord <- pacjent[properind, ]
       }
     }
     output$podac <- renderUI(properrecord$godzina)
     output$color <- renderUI(
       {tags$span(style="color:green", h4("Podać o:", uiOutput("podac")))})
     output$image <- renderUI(img(src="ok.png", width = 20))
     
     #is the difference in patient's and local time less or equal to 2 hours?
     thistime <- as.numeric(unlist(strsplit(properrecord$godzina, ':')))
     if (abs((timenow[1]+timenow[2]/60) - (thistime[1]+thistime[2]/60))>2)
        { output$color <- renderUI(
         tags$span(style="color:red", h4("Podać o", uiOutput("podac"))))
         output$image <- renderUI(img(src="notok.png", width = 20))
     }
     
     if (properrecord$podano == "nie")
      output$checks <- renderUI(actionButton("podano", label = "Podano lek"))
     if (properrecord$podano == "tak") {}
      
    properrecord
   })
   
  output$retable <- renderTable( { 
      tab1() 
      }, include.rownames = FALSE)
  

  observeEvent(input$podano, {
    data$podano[which(data$imie == tab1()$imie & data$nazwisko == tab1()$nazwisko & 
                        data$PESEL == tab1()$PESEL & data$godzina == tab1()$godzina & 
                        data$lek == tab1()$lek & data$dawka == tab1()$dawka)] <- "tak"
    data2 <- select(data, imie, nazwisko, PESEL, godzina, lek, dawka, podano)
    write.table(data2, file = "data/pacjenci.txt", sep = ',')
    data <- read.table("data/pacjenci.txt", sep = ",", as.is = TRUE)
    disable("checks")
    updateTextInput(session, "fname", value = "")     
    updateTextInput(session, "sname", value = "")
    updateTextInput(session, "id", value = "")

  })
  
  #dodawanie nowego leku dla pacjenta
  dodano <- eventReactive(input$drugsub, {
    validate(insert_patient(input$infname, input$insname, input$inid, input$ingodzina, input$indawka, input$ilek)) 
    data[nrow(data) + 1, ] <- cbind(input$infname, input$insname, input$inid, input$ingodzina, input$inlek, input$indawka, "nie")
    write.table(data, file = "data/pacjenci.txt", sep = ',')
    disable("drugsub")
    updateTextInput(session, "infname", value = "")     
    updateTextInput(session, "insname", value = "")
    updateTextInput(session, "inid", value = "")
    updateTextInput(session, "ingodzina", value = "")     
    updateTextInput(session, "inlek", value = "")
    updateTextInput(session, "indawka", value = "")
    enable("drugsub")
    paste0("Pomyślnie zapisano zalecenie dla pacjenta: ", input$infname, " ", input$insname)
    })
  
  output$dodano <- renderText({
    dodano()
  })
  
  #usuwanie recepty
  usunieto <- eventReactive(input$drugsubrm, {
    validate(delete_patient(input$inidrm, input$inlekrm, input$ingodzinarm)) 
    if (input$ingodzinarm == "" & nrow(filter(data, PESEL == input$inidrm & lek == input$inlekrm )) == 0) {
      ext <- "Ten pacjent nie otrzymuje wpisanego leku!"
    } else {
      if (input$ingodzinarm != "" & nrow(filter(data, PESEL == input$inidrm & lek == input$inlekrm & godzina == input$ingodzinarm)) == 0) {
        ext <- "Ten pacjent nie otrzymuje tego leku o podanej godzinie!"
      } else { 
        if (nrow(filter(data, PESEL == input$inidrm)) == 0) {
          ext <- "Nie ma pacjenta o takim numerze PESEL!"
        } else {
          disable("drugsubrm")
          updateTextInput(session, "inidrm", value = "")
          updateTextInput(session, "ingodzinarm", value = "")     
          updateTextInput(session, "inlekrm", value = "")
          enable("drugsubrm")
          if (input$ingodzinarm != "") {
            data2 <- data[-which(data$PESEL == input$inidrm & data$lek == input$inlekrm & data$godzina == input$ingodzinarm), ]
          } else {
            data2 <- data[-which(data$PESEL == input$inidrm & data$lek == input$inlekrm), ]
          }
          ext <- paste0("Pomyślnie usunięto receptę dla pacjenta.")
          write.table(data2, file = "data/pacjenci.txt", sep = ',')
        }
      }
    }
    ext
 
  })
  
  output$usunieto <- renderText({
    usunieto()
  })
    
  
})
