library(dplyr)
library(magrittr)
library(wbstats)
library(tidyr)
library(DT)
library(plotly)
library(sparkline)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(openxlsx)
library(rmarkdown)



myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download1","Download table as csv"),
                  br(),
                  br(),
                  downloadButton("download2","Download table as xlsx"),
                  
                  easyClose = TRUE, title = "Download Table")
  )
}

##--------------------------------------

header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(disable=T)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tags$head(
    tags$style(HTML(
      ".tabbable ul li:nth-child(1) { float: left; }
      .tabbable ul li:nth-child(2) { float: left; }
      .tabbable > .nav > li > a  {background-color: white;  color:black}"
    ))
  ),
  
  
  fluidRow(
    
    column(width = 12,
           
           boxPlus(
             title = "Set criteria for data retrieval", 
             closable = FALSE,
             width = 12,
             
             fluidRow(
               
               column(width = 3,
                      radioButtons(inputId = "wb_update",
                                   label = "Update",
                                   choices = c("Cached", "Latest update"),
                                   selected = "Cached",
                                   inline = T)
               ),
               column(width = 3,
                      uiOutput("wb_lang_display")),
               
               column(width = 3,
                      
                      textInput(inputId = "wb_search_term",
                                label = "Search for a specific topic",
                                value = "pollution"),
                      tags$code("for multiple choice use '|' e.g. 'health|pollution'")
               ),
               
               column(width = 3,
                      
                      radioButtons(inputId = "wb_freq",
                                   label = "Frequency",
                                   choices = c("Yearly " = "Y",
                                               "Monthly" = "M",
                                               "Quarterly " = "Q"),
                                   selected = "Y",
                                   inline = T
                      )
                      
               )
             ),
             
             fluidRow(
               
               column(width = 3, uiOutput("wb_field_group_choice")),
               
               column(width = 3, uiOutput("wb_field_choice")),
               
               column(width = 6,
                      
                      uiOutput("wb_indicator"))),
             
             fluidRow(
               
               column(width = 6,
                      
                      sliderInput(inputId = "wb_dates",
                                  label = "Years covered in the dataset",
                                  min = 1960,
                                  max = as.numeric(lubridate::year(Sys.Date())),
                                  value = c(2000, 2019),
                                  step = 1,
                                  sep=""))
               
             )
             
           )
           
           
    )
    
  ),
  
  fluidRow(
    
    column(width = 6,
           
           gradientBox(
             title = "Full table",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "purple",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      
                      fluidRow(column(width = 6,
                                      
                                      
                                      radioButtons(inputId = "selectAllCountries",
                                                   label = "",
                                                   choices = c("Select all countries", "Choose specific country/-s"),
                                                   selected = "Choose specific country/-s",
                                                   inline = T),
                                      
                                      uiOutput("wb_countries")),
                               
                               column(width = 6,
                                      radioButtons(inputId = "selectAllVariables",
                                                   label = "",
                                                   choices = c("Select all variables", "Choose specific variable/-s"),
                                                   selected = "Select all variables",
                                                   inline = T),
                                      
                                      uiOutput("wb_variables")
                               )
                               
                      ))
               
             ),
             
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      uiOutput("wb_data")
                      
               )
               
             )
             
             
           )
    ),
    
    
    column(width = 6,
           
           gradientBox(
             title = "Overview",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "maroon",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      
                      fluidRow(
                        
                        column(width = 12,
                               
                               htmlOutput("wb_countries_selSummary")
                        )
                        
                      )
               )
             ),
             
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      plotlyOutput("d1") %>% withSpinner(color="#0dc5c1"),
                      
                      
                      downloadButton("report", "Generate report")
                      
               )
               
             )
             
             
           )
    )
    
  )
)



shinyApp(
  
  ui = dashboardPage(header, sidebar, body),
  
  server = function(input, output, session) { 
    
    options(shiny.usecairo=T)
    
    
    output$wb_lang_display <-renderUI(
      
      if(input$wb_update == "Latest update") {
        
        htmlOutput("wb_lang")
      }
      
    )
    
    output$wb_lang <- renderUI({
      
      selectInput(inputId = "wb_lang",
                  label = "Language in which to return the results",
                  c("English" = "en",
                    "Spanish" = "es",
                    "French" = "fr",
                    "Arabic" = "ar",
                    "Mandarin" = "zh"),
                  selected = "en")
      
    })
    
    
    
    wb_cachelist_choice <- reactive({
      
      wbcache(lang = input$wb_lang)
      
    })
    
    ff <- reactive({
      
      if(input$wb_update == "Latest update") {
        
        lapply(wb_cachelist_choice(), function(df) cbind(names(df))) %>%
          plyr::ldply(data.frame) %>%
          as.data.frame() %>%
          mutate_if(is.factor, as.character) %>%
          set_colnames(c("fieldGroup", "fieldName"))
      }
      
      else if(input$wb_update == "Cached") {
        
        
        lapply(wb_cachelist, function(df) cbind(names(df))) %>%
          plyr::ldply(data.frame) %>%
          as.data.frame() %>%
          mutate_if(is.factor, as.character) %>%
          set_colnames(c("fieldGroup", "fieldName"))
      }
      
      
      
    }) 
    
    
    output$wb_field_group_choice <- renderUI({
      
      selectInput(inputId = "wb_field_group_choice",
                  label = "Field category",
                  choices = unique(ff()$fieldGroup),
                  selected = "indicators"
      )
      
      
    })
    
    output$wb_field_choice <- renderUI({
      
      ff_sel <- ff() %>% filter(fieldGroup == input$wb_field_group_choice)
      
      selectInput(inputId = "wb_field_choice",
                  label = "Field in selected category to search by",
                  choices = ff_sel$fieldName,
                  selected = "indicatorDesc"
      )
    })
    
    
    data_wb_search <- reactive({
      
      if(input$wb_update == "Latest update") {wbsearch(pattern = input$wb_search_term, 
                                                       fields = input$wb_field_choice,
                                                       cache = wb_cachelist_choice())}
      
      
      else if(input$wb_update == "Cached") {wbsearch(pattern = input$wb_search_term, 
                                                     fields = input$wb_field_choice,
                                                     cache = wb_cachelist)}
      
      
    })
    
    output$wb_indicator <- renderUI({
      
      selectInput(inputId = "wb_indicator",
                  label = "Specific indicator to retrieve",
                  choices = data_wb_search()$indicator,
                  selected = data_wb_search()$indicator[4]
      )
      
    })
    
    
    output$wb_countries_sel <- renderUI({
      
      selectizeInput(inputId = "wb_countries_sel",
                     label = "Select countries to retrieve the data for",
                     choices = sort(wbcountries()$country),
                     selected = "Spain",
                     multiple = T)
      
    })
    
    output$wb_countries <- renderUI({
      
      
      if(input$selectAllCountries == "Choose specific country/-s") { htmlOutput("wb_countries_sel")}
      
    })
    
    
    output$wb_vars_sel <- renderUI({
      
      selectizeInput(inputId = "wb_vars_sel",
                     label = "Select variables to add to the table",
                     choices = names(wb_data_all_vars_all()),
                     selected = c("date", "country", "indicator", "value"),
                     multiple = T)
      
    })
    
    output$wb_variables <- renderUI({
      
      
      if(input$selectAllVariables == "Choose specific variable/-s") { htmlOutput("wb_vars_sel")}
      
    })
    
    
    
    wb_data_all_vars_all <- reactive({
      
      wbstats::wb(
        country = wbcountries()$iso2c
        , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
        , startdate = as.numeric(input$wb_dates[1])
        , enddate = as.numeric(input$wb_dates[2])
        , freq = input$wb_freq
        , POSIXct = TRUE
      )
      
    })
    
    wb_data_all_vars_sel <- reactive({
      
      wbstats::wb(
        country = wbcountries()$iso2c
        , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
        , startdate = as.numeric(input$wb_dates[1])
        , enddate = as.numeric(input$wb_dates[2])
        , freq = input$wb_freq
        , POSIXct = TRUE
      ) %>% select(input$wb_vars_sel)
      
    })
    
    
    wb_data_sel_vars_all <- reactive({
      
      wbstats::wb(
        country = wbcountries()[wbcountries()$country %in% input$wb_countries_sel, "iso2c"]
        , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
        , startdate = as.numeric(input$wb_dates[1])
        , enddate = as.numeric(input$wb_dates[2])
        , freq = input$wb_freq
        , POSIXct = TRUE
      )
      
    })
    
    wb_data_sel_vars_sel <- reactive({
      
      wbstats::wb(
        country = wbcountries()[wbcountries()$country %in% input$wb_countries_sel, "iso2c"]
        , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
        , startdate = as.numeric(input$wb_dates[1])
        , enddate = as.numeric(input$wb_dates[2])
        , freq = input$wb_freq
        , POSIXct = TRUE
      ) %>% select(input$wb_vars_sel)
      
    })
    
    
    output$wb_dt_all_vars_all <- renderDataTable(
      
      datatable( wb_data_all_vars_all(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    output$wb_dt_sel_vars_all <- renderDataTable(
      
      datatable( wb_data_sel_vars_all(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    output$wb_dt_all_vars_sel <- renderDataTable(
      
      datatable( wb_data_all_vars_sel(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    output$wb_dt_sel_vars_sel <- renderDataTable(
      
      datatable( wb_data_sel_vars_sel(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    output$wb_data <- renderUI({
      
      if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Choose specific variable/-s") {
        
        dataTableOutput("wb_dt_sel_vars_sel")%>% withSpinner(color="#0dc5c1")
      }
      
      else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Select all variables") {
        
        dataTableOutput("wb_dt_all_vars_all")%>% withSpinner(color="#0dc5c1")
      }
      
      else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Select all variables") {
        
        dataTableOutput("wb_dt_sel_vars_all")%>% withSpinner(color="#0dc5c1")
      }
      
      else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Choose specific variable/-s") {
        
        dataTableOutput("wb_dt_all_vars_sel")%>% withSpinner(color="#0dc5c1")
      }
      
      
      
    })
      
    
    
    wb_data_all_vars_all_summ <- reactive({
    
      wbstats::wb(
        country = wbcountries()[wbcountries()$country %in% input$wb_countries_selSummary, "iso2c"]
        , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
        , startdate = as.numeric(input$wb_dates[1])
        , enddate = as.numeric(input$wb_dates[2])
        , freq = input$wb_freq
        , POSIXct = TRUE
      )
      
    })
    
    
    output$wb_countries_selSummary <- renderUI({
      
      selectizeInput(inputId = "wb_countries_selSummary",
                     label = "Select countries to summarise the data for",
                     choices = sort(wbcountries()$country),
                     selected = sample(wbcountries()$country,10),
                     multiple = T)
      
    })
    
   wb_data_summary_filtered <-  reactive({
      
      wb_data_all_vars_all_summ() %>%
        select(country, 
               value) %>%
        na.omit()
      
      })
   
   
   output$d1 <- renderPlotly({
       
       p <- plot_ly(wb_data_summary_filtered(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
       
       p
       
       
     })
   
   
   observeEvent(input$test, {
     print("hello")
     showModal(myModal())
   })
   
   
   output$download1 <- downloadHandler(
     filename = function() {
       paste(input$wb_indicator, ".csv", sep="")
     },
     content = function(file) {
       
       if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Choose specific variable/-s") { write.csv(wb_data_sel_vars_sel(), file)}
       else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Select all variables") { write.csv(wb_data_all_vars_all(), file)}
       else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Select all variables") { write.csv(wb_data_sel_vars_all(), file)}
       else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Choose specific variable/-s") { write.csv(wb_data_all_vars_sel(), file)}
     }
   )
   
   output$download2 <- downloadHandler(
     filename = function() {
       paste(input$wb_indicator, ".xlsx", sep="")
     },
     content = function(file) {
       if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Choose specific variable/-s") { write.xlsx(wb_data_sel_vars_sel(), file)}
       else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Select all variables") { write.xlsx(wb_data_all_vars_all(), file)}
       else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Select all variables") { write.xlsx(wb_data_sel_vars_all(), file)}
       else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Choose specific variable/-s") { write.xlsx(wb_data_all_vars_sel(), file)}
       
     }
   )
   
   
   output$report <- downloadHandler(
     # For PDF output, change this to "report.pdf"
     filename = "report.html",
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "report.Rmd")
       file.copy("report.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(n = wb_data_summary_filtered(),
                      y1 = input$wb_dates[1],
                      y2 = input$wb_dates[2])
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
   
    
    
  }
)

