##### USER INTERFACE #####
library(shiny)

# Side bar
pageWithSidebar(
  
  # Page header
  titlePanel(
    tagList(
      
      # Title
      title = "Opdatér din vejematrice",
      
      # Company image
      span(img(src = "epinion.png",
               height = 50,
               width = 140),
           style = "position:absolute;right:1em;")
    ),
  ),
  
  # Input values
  sidebarPanel(
    
    # Header of side bar
    tags$label(h3("Vælg variable")),
    
    # Input
    selectInput(inputId = "køn",
                label = "Køn",
                choices = c("Nej" = "Nej",
                            "Ja" = "køn")),
    selectInput(inputId = "alder",
                label = "Aldersgrupper (tre kategorier)",
                choices = c("Nej" = "Nej",
                            "Ja" = "alder")),
    selectInput(inputId = "køn_alder",
                label = "Kombination af køn og alder",
                choices = c("Nej" = "Nej",
                            "Ja" = "køn_alder")),
    selectInput(inputId = "region",
                label = "Region",
                choices = c("Nej" = "Nej",
                            "Ja" = "region")),
    selectInput(inputId = "uddannelse2",
                label = "Uddannelse (to kategorier)",
                choices = c("Nej" = "Nej",
                            "Ja" = "uddannelse2")),
    selectInput(inputId = "husstand",
                label = "Husstandstype (tre kategorier)",
                choices = c("Nej" = "Nej",
                            "Ja" = "husstand")),
    selectInput(inputId = "person",
                label = "Antal personer i husstand (tre kategorier)",
                choices = c("Nej" = "Nej",
                            "Ja" = "person")),
    selectInput(inputId = "partivalg_sidste_fv",
                label = "Partivalg ved folketingsvalget 2019",
                choices = c("Nej" = "Nej",
                            "Ja" = "partivalg_sidste_fv")),
    
    # Submit buttom
    actionButton(inputId = "submitbutton", 
                 label = "Hent", 
                 class = "btn btn-primary",
                 style = "background-color: #FF4646; border-color: #FF4646")
    
  ),
  
  # Main panel
  mainPanel(
    
    tabsetPanel(type ="tabs",
                tabPanel("Status", verbatimTextOutput("contents")),
                tabPanel("Køn", "Kønsfordeling i Danmark", DT::dataTableOutput("table_køn")),
                tabPanel("Aldersgruppe", "Aldersfordeling i Danmark (tre kategorier)", DT::dataTableOutput("table_alder")),
                tabPanel("Køn og alder", "Kombination af køn og alder", DT::dataTableOutput("table_køn_alder")),
                tabPanel("Region", "Fordelinger på region", DT::dataTableOutput("table_region")),
                tabPanel("Uddannelse", "Uddannelsesfordeling i Danmark (to kategorier)", DT::dataTableOutput("table_uddannelse2")),
                tabPanel("Husstandstype", "Husstandstyper i Danmark", DT::dataTableOutput("table_husstand")),
                tabPanel("Personer i husstand", "Antal personer i husstand", DT::dataTableOutput("table_person")),
                tabPanel("Partivalg", "Partivalg ved folketingsvalget 2019 (Ikke stemt skal opdateres med stikprøveandel)", 
                         DT::dataTableOutput("table_partivalg_sidste_fv"))
    ),
    
    # Fluid row
    fluidRow(
      
      column(width = 12, 
             downloadButton(outputId = "download", 
                            label = "Download",
                            class = "btn btn-primary",
                            style = "background-color: #FF4646; border-color: #FF4646")
             
      )
    )
  )
)