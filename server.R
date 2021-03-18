##### SERVER #####
library(DT)
library(tidyverse)
library(dkstat)
library(openxlsx)

# Set time zone
Sys.setenv(TZ="CET")

# Load model
partivalg <- readRDS(file = "partivalg_f19.rds")

# Load function
source("clean_dst.R", encoding = "UTF-8", local = TRUE)

# Server
shinyServer(
  function(input, output, session) {
    
    # Køn table
    gender <- reactive({
      
      if(input$submitbutton > 0 & input$køn=="køn") {
        
        
        DT::datatable(
          
          clean_dst(variable = "køn")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(køn = c("Kvinde", "Mand"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Alder table
    age <- reactive({
      
      if(input$submitbutton > 0 & input$alder=="alder") {
        
        DT::datatable(
          
          clean_dst(variable = "alder")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(alder = c("18-34 år", "35-55 år", "56 år eller derover"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Combination table
    comb <- reactive({
      
      if(input$submitbutton > 0 & input$køn_alder=="køn_alder") {
        
        DT::datatable(
          
          clean_dst(variable = "køn_alder")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(køn_alder = c("Mand 18-34 år", "Mand 35-55 år", "Mand 56 år eller derover",
                                   "Kvinde 18-34 år", "Kvinde 35-55 år", "Kvinde 56 år eller derover"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 
                               'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Area table
    area <- reactive({
      
      if(input$submitbutton > 0 & input$region=="region") {
        
        DT::datatable(
          
          clean_dst(variable = "region")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(region = c("Hovedstaden", "Midtjylland", "Nordjylland", "Sjælland", "Syddanmark"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 
                               'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # uddannelse table
    udd <- reactive({
      
      if(input$submitbutton > 0 & input$uddannelse2=="uddannelse2") {
        
        DT::datatable(
          
          clean_dst(variable = "uddannelse2")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(uddannelse2 = c("Grundskole og erhvervsfaglig uddannelse", "Gymnasie og videregående uddannelse"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Person table
    per <- reactive({
      
      if(input$submitbutton > 0 & input$person=="person") {
        
        DT::datatable(
          
          clean_dst(variable = "person")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(person = c("Grundskole og erhvervsfaglig uddannelse", "Gymnasie og videregående uddannelse"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Husstand table
    hus <- reactive({
      
      if(input$submitbutton > 0 & input$husstand=="husstand") {
        
        DT::datatable(
          
          clean_dst(variable = "husstand")
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(husstand = c("Stuehus og parcelhus", "Række-, kæde- eller dobbelthus", 
                                  "Etageboligbebyggelse, kollegium, fritidshuse, døgninstitution samt andet"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Partivalg table
    parti <- reactive({
      
      if(input$submitbutton > 0 & input$partivalg_sidste_fv=="partivalg_sidste_fv") {
        
        DT::datatable(
          
          partivalg
          
        )
        
      } else {
        
        DT::datatable(
          
          data.frame(partivalg_sidste_fv = c("A: Socialdemokratiet", "B: Radikale", "C: Konservative", "D: Nye Borgerlige", 
                                             "F: Socialistisk Folkeparti","I: Liberal Alliance", "O: Dansk Folkeparti", 
                                             "V: Venstre", "Ø: Enhedslisten", "Å: Alternativet", "Andre partier", "Ikke stemt"),
                     share = c('Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 
                               'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 
                               'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 
                               'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere', 'Tryk "Hent" for at opdatere'))
          
        )
        
      }
    })
    
    # Combine data for download
    data <- reactive({
      
      if(input$submitbutton > 0) {
        
        population <- list(gender()$x[["data"]], age()$x[["data"]], comb()$x[["data"]], area()$x[["data"]],
                           udd()$x[["data"]], hus()$x[["data"]], per()$x[["data"]], parti()$x[["data"]])
        
        names(population) <- c("køn", "alder", "køn_alder", "region", "uddannelse2", "husstand", "person", "partivalg_sidste_fv")
        selected <- c(input$køn, input$alder, input$køn_alder, input$region, input$uddannelse2, input$husstand, input$person, input$partivalg_sidste_fv)
        
        population[names(population) %in% selected]
        
      } else {
        
        DT::datatable(
          
          data.frame(variabel = "Please specify variables")
          
        )
        
      }
    })
    
    output$table_køn <- DT::renderDataTable(gender())
    output$table_alder <- DT::renderDataTable(age())
    output$table_køn_alder <- DT::renderDataTable(comb())
    output$table_region <- DT::renderDataTable(area())
    output$table_uddannelse2 <- DT::renderDataTable(udd())
    output$table_husstand <- DT::renderDataTable(hus())
    output$table_person <- DT::renderDataTable(per())
    output$table_partivalg_sidste_fv <- DT::renderDataTable(parti())
    
    # Status text box
    output$contents <- renderPrint({
      if (input$submitbutton > 0) {
        
        isolate( paste("Fordelingerne er opdateret per ", Sys.time(), " fra Danmarks Statistik.", sep = "") )
        
      } else {
        
        return("Serveren er klar.")
      }
      
    })
    
    
    
    # Download
    output$download <- downloadHandler(
      
      filename = function() {
        
        paste0("populationsmatrice.xlsx")
        
      },
      
      content = function(file) {
        
        write.xlsx(data(), file)
        
      }
    )
  }
)