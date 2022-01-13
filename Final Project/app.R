library(shiny)
library(rmarkdown)
library(markdown)

ui <- navbarPage('Elect your favorite song',
                 tabPanel('2020', basicPage(uiOutput('checkbox2020'), uiOutput('actionbutton2020'), h2(textOutput('txt')))),
                 tabPanel('2021', basicPage(uiOutput('checkbox2021'), uiOutput('actionbutton2021'), h2(textOutput('txt1')))),
                 tabPanel('About', 
                          fluidRow(
                            column(4, includeMarkdown(file.path('about.rmd'))), 
                            column(4,img(src=paste0('
                            https://portalpopline.com.br/wp-content/uploads/2019/05/billboard-logo-b-20-billboard-1548.jpg'),
                            height='100%',width='100%'))
                          )))
  
  


server <- function(input,output, session) {
  
  songs2020 <- c('Blinding Lights [The Weeknd]','Circles [Post Malone]','The Box
                 [Roddy Ricch]','Don\'t Start Now [Dua Lipa]','Rockstar [DaBaby 
                 ft. Roddy Ricch]','Adore You [Harry Styles]','Life Is Good 
                 [Future feat. Drake]','Memories [Maroon 5]','The Bones [Marren 
                 Morris]','Someone You Loved [Lewis Capaldi]')
  
  songs2021 <- c('Levitating [Dua Lipa]','Save Your Tears [The Weeknd & Ariana Grande]',
                 'Blinding Lights [The Weeknd]','Mood [24kGold ft. iann dior]',
                 'Good 4 U [Olivia Rodrigo]','Kiss Me More [Doja Cat ft. SZA]',
                 'Leave The Door Open [Silk Sonic]','Drivers License [Olivia
                 Rodrigo]','Montero (Call Me By Your Name) [Lil Nas X]','Peaches 
                 [Justin Bieber ft. Daniel Caesar & Giveon]')
  
  
  output$checkbox2020 <- renderUI({checkboxGroupInput("checkGroup", 
                     h3("What song is your favorite?"), choices=c(songs2020[1],songs2020[2]))})
  output$checkbox2021 <- renderUI({checkboxGroupInput("checkGroup1", 
                    h3("What song is your favorite?"), choices=c(songs2021[1],songs2021[2]))})
  output$actionbutton2020 <- renderUI({actionButton(inputId ='ac2020',label='Submit')})
  output$actionbutton2021 <- renderUI({actionButton(inputId ='ac2021',label='Submit')})
  chosen2020 <- reactiveValues()
  chosen2021 <- reactiveValues()
  observeEvent(input$ac2020, {
    c(chosen2020$a <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(songs2020[3],
                                                                  songs2020[4]))
      , output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20201',label='Submit')}))
  })
  observeEvent(input$ac20201, {
    c(chosen2020$b <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(songs2020[5],
                                                                  songs2020[6]))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20202',label='Submit')}))
  })
  observeEvent(input$ac20202, {
    c(chosen2020$c <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(songs2020[7],
                                                                  songs2020[8]))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20203',label='Submit')}))
  })
  observeEvent(input$ac20203, {
    c(chosen2020$d <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(songs2020[9],
                                                                  songs2020[10]))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20204',label='Submit')}))
  })
  observeEvent(input$ac20204, {
    c(chosen2020$e <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(isolate(chosen2020$a),
                                                                  isolate(chosen2020$b)))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20205',label='Submit')}))
  })
  chosen20201 <- reactiveValues()
  observeEvent(input$ac20205, {
    c(chosen20201$a <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(isolate(chosen2020$c),
                                                                  isolate(chosen2020$d)))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20206',label='Submit')}))
  })
  observeEvent(input$ac20206, {
    c(chosen20201$b <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(isolate(chosen2020$e),
                                                                  isolate(chosen20201$a)))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20207',label='Submit')}))
  })
  chosen20202 <- reactiveValues()
  observeEvent(input$ac20207, {
    c(chosen20202$a <- input$checkGroup, 
      updateCheckboxGroupInput(session, 'checkGroup', choices = c(isolate(chosen20201$b),
                                                                  isolate(chosen20202$a)))
      ,output$actionbutton2020 <- renderUI({actionButton(inputId ='ac20208',label='Submit')}))
  })
  observeEvent(input$ac20208,{c(chosen20202$b <- input$checkGroup,removeUI(selector="actionbutton2020"),
    output$txt <- renderText(paste0('Your favorite song is ',
                                    isolate(chosen20202$b))),
    output$actionbutton2020 <- renderUI({}), output$checkbox2020 <- renderUI({}))})
  
  # Code for 2021 starts here
  
  observeEvent(input$ac2021, {
    c(chosen2021$a <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(songs2021[3],
                                                                  songs2021[4]))
      , output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20211',label='Submit')}))
  })
  observeEvent(input$ac20211, {
    c(chosen2021$b <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(songs2021[5],
                                                                  songs2021[6]))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20212',label='Submit')}))
  })
  observeEvent(input$ac20212, {
    c(chosen2021$c <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(songs2021[7],
                                                                  songs2021[8]))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20213',label='Submit')}))
  })
  observeEvent(input$ac20213, {
    c(chosen2021$d <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(songs2021[9],
                                                                  songs2021[10]))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20214',label='Submit')}))
  })
  observeEvent(input$ac20214, {
    c(chosen2021$e <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(isolate(chosen2021$a),
                                                                  isolate(chosen2021$b)))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20215',label='Submit')}))
  })
  chosen20211 <- reactiveValues()
  observeEvent(input$ac20215, {
    c(chosen20211$a <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(isolate(chosen2021$c),
                                                                  isolate(chosen2021$d)))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20216',label='Submit')}))
  })
  observeEvent(input$ac20216, {
    c(chosen20211$b <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(isolate(chosen2021$e),
                                                                  isolate(chosen20211$a)))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20217',label='Submit')}))
  })
  chosen20212 <- reactiveValues()
  observeEvent(input$ac20217, {
    c(chosen20212$a <- input$checkGroup1, 
      updateCheckboxGroupInput(session, 'checkGroup1', choices = c(isolate(chosen20211$b),
                                                                  isolate(chosen20212$a)))
      ,output$actionbutton2021 <- renderUI({actionButton(inputId ='ac20218',label='Submit')}))
  })
  observeEvent(input$ac20218,{c(chosen20212$b <- input$checkGroup1,removeUI(selector="actionbutton2021"),
                                output$txt1 <- renderText(paste0('Your favorite song is ',
                                                                isolate(chosen20212$b))),
                                output$actionbutton2021 <- renderUI({}), output$checkbox2021 <- renderUI({}))})
}





shinyApp(ui=ui, server=server)
  
