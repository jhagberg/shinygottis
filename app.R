#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(rgl.useNULL = TRUE)
library(shiny)
library(shinyjs)
library(optiSel)
library(readxl)
library(anytime)
library(shinydashboard)

filename<-"kaninlista2019g2"
Pedifilename<-paste0('Pedi',filename, '.rdata')
pKinfilename<-paste0('pKin',filename, '.rdata')

start_time <- Sys.time()

if (file.exists(Pedifilename)){
  load(file=Pedifilename)
}else {
  gottis <-read_excel(paste0('./',filename, '.xlsx'), range = cell_cols("A:K"), col_types = c("text", "skip", "skip","text", "text", "numeric", "date", "skip", "text","skip", "text"))
  gottis <- gottis[,c("Nummer","Far nr","Mor nr","Kön","År","Namn","Född")]
  names(gottis)<-c("Indiv","Sire","Dam","Sex","Born","Name","Född")
  #Ta bort tomma rader
  gottis<-gottis[!is.na(gottis$Indiv),]
  #rensa bort kastrat
  gottis=subset(gottis,Sex!="kastrat")
  gottis=subset(gottis,Sex!="?")
  Pedi <- prePed(gottis)
  #Gör om till Datum igen
  Pedi$Född<-anydate(Pedi$Född)
  save(Pedi,file=Pedifilename)
  }
  if (file.exists(pKinfilename)){
    cat(file=stderr(),"in Load pKin")
    cat(file=stderr(),load(file=pKinfilename,verbose = TRUE))
    }else {
      pKin   <- pedIBD(Pedi)
      save(pKin,file=pKinfilename)
    }
males<-subset(Pedi[with(Pedi,order(Born,decreasing =TRUE)),],Sex=="male")
females<-subset(Pedi[with(Pedi,order(Born,decreasing =TRUE)),],Sex=="female")


end_time <- Sys.time()
cat(file=stderr(),end_time - start_time)

Selevctrender <- 
  '{option: function(item, escape) {return "<div><strong>" + escape(item.Indiv) + "</strong> " + 
                          "<ul>" +  (item.Name ? "<li>" +"Namn: "+ escape(item.Name) + "</li>" : "") +
               (item.Name ?  "<li>Född: " + escape(item.Born) + "</li>" : "")+"</ul>" + "</div>"; },
  item: function(item, escape){
    return "<div>" + escape(item.Indiv) + " " + escape(item.Name)   + "</div>";
  } }'
noresultJSmale <-  
  "function(text) {
    if ( ! this.currentResults.items.length) {
        shinyjs.show('ingenHane');
    } else {
            shinyjs.hide('ingenHane');
            } }"

noresultJSfemale <-  
  "function(text) {
    if ( ! this.currentResults.items.length) {
        shinyjs.show('ingenHona');
    } else {
            shinyjs.hide('ingenHona');
            } }"

noresultJS1hane <-  
  "function(text) {
    if ( ! this.currentResults.items.length) {
        shinyjs.show('ingen1Hane');
    } else {
            shinyjs.hide('ingen1Hane');
            } }"

noresultJS1hona <-  
  "function(text) {
    if ( ! this.currentResults.items.length) {
        shinyjs.show('ingen1Hona');
    } else {
            shinyjs.hide('ingen1Hona');
            } }"


library(shinydashboard)
header <- dashboardHeader(title = "Gotlandskaninen")

sidebar<-  dashboardSidebar(
  sidebarMenu(id = "tabs",
    menuItem("Provparning", tabName = "provparning", icon = icon("dna"),startExpanded = TRUE,
    menuSubItem("Normal", tabName = "provparning", icon = icon("magic")),
    menuSubItem("En oreg hane", tabName = "1hane", icon = icon("male")),
    menuItem("En oreg hona", tabName = "1hona", icon = icon("female")),
    menuItem("Oreg hona och hane", tabName = "hanehona", icon = icon("restroom")))
  )
)

regulartest <-tabItem(tabName = "provparning", box(title = "Provparning",status = "primary",solidHeader = TRUE,
                   collapsible = TRUE,width = 12,
                   fluidRow(
                                          div(id="SIREBOX", box(width=6,title = span(tagList("Far",HTML("&nbsp;"),icon("male"))),status = "primary",solidHeader = TRUE,
                                           collapsible = FALSE, 
                                           selectizeInput('SIRE','', choices = NULL,  options = list(
                                             valueField = 'Indiv',
                                             searchField = c('Indiv','Name','Born'),
                                             labelField ='Name',
                                             render = I(Selevctrender),
                                             onType = I(noresultJSmale),
                                             placeholder='Välj Far'))
                     ,div(style="text-align: center;",actionButton("ingenHane", "Inget Resultat! Vill du para en oregisterad Hane?",icon=icon("exclamation-triangle"))))),
                     
                     div(id="DAMBOX",box(width=6,
                                         title = span(tagList("Mor",HTML("&nbsp;"),icon("female"))),status = "warning",solidHeader = TRUE,
                                         collapsible = FALSE,
                                         selectizeInput('DAM','', choices = NULL,  options = list(
                                           valueField = 'Indiv',
                                           searchField = c('Indiv','Name','Born'),
                                           labelField ='Indiv',
                                           render = I(Selevctrender),
                                           onType = I(noresultJSfemale),
                                           placeholder='Välj Mor'))
                                         ,
                                         div(style="text-align: center;",actionButton("ingenHona", "Inget Resultat! Vill du para en oregisterad Hona?",icon=icon("exclamation-triangle"))))
                     ))
                   ),
       
      box(title = "Resultat",status = "primary",solidHeader = TRUE,
      collapsible = TRUE,width = 12,div(id="DEG",textOutput("inavelkoff")),plotOutput("subPlot"))
)

oregSiretest <-
  tabItem(
    tabName = "1hane",
    box(
      title = "Provparning med en oregisterad hane",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        box(
          title = span(tagList(
            "Oregistrerad Far", HTML("&nbsp;"), icon("male")
          )),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Farfar", HTML("&nbsp;"), icon("male"))),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'SIRE1',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField =
                  'Indiv',
                render = I(Selevctrender),
                placeholder =
                  'Välj Farfar'
              )
            )
          ),
          box(
            width = 6,
            title = span(tagList("Farmor", HTML("&nbsp;"), icon("female"))),
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'DAM1',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField =
                  'Indiv',
                render = I(Selevctrender),
                placeholder = 'Välj Farmor'
              )
            )
          )
        )
        ,
        
        div(
          id = "DAMBOX",
          box(
            width = 6,
            title = span(tagList("Mor", HTML("&nbsp;"), icon("female"))),
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'DAM1hane',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField =
                  'Indiv',
                onType = I(noresultJS1hona),
                render = I(Selevctrender),
                placeholder = 'Välj Mor'
              )
            )
            ,div(style="text-align: center;",actionButton("ingen1Hona", "Inget Resultat! Vill du provpara med en oregisterad Hona också?",icon=icon("exclamation-triangle"))) )
        )
      )
    ),
    box(
      title = "Resultat",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      div(id = "DEG2", textOutput("inavelkoff2"), plotOutput("subPlot2"))
    )
  )

oregDamtest <-
  tabItem(
    tabName = "1hona",
    box(
      title = "Provparning med en oregisterad Hona",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        box(
          width = 6,
          title = span(tagList("Far", HTML("&nbsp;"), icon("male"))),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          selectizeInput(
            'SIRE1hona',
            '',
            choices = NULL,
            options = list(
              valueField = 'Indiv',
              searchField = c('Indiv', 'Name', 'Born'),
              labelField = 'Indiv',
              onType = I(noresultJS1hane),
              render = I(Selevctrender),
              placeholder = 'Välj Far'
            )
          )
          ,div(style="text-align: center;",actionButton("ingen1Hane", "Inget Resultat! Vill du provpara med en oregisterad Hane också?",icon=icon("exclamation-triangle")))),
        box(
          title = span(tagList(
            "Oregistrerad Mor", HTML("&nbsp;"), icon("female")
          )),
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Morfar", HTML("&nbsp;"), icon("male"))),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'morfar',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField = 'Indiv',
                render = I(Selevctrender),
                placeholder = 'Välj Morfar'
              )
            )
          ),
          box(
            width = 6,
            title = span(tagList("Mormor", HTML("&nbsp;"), icon("female"))),
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'mormor',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField = 'Indiv',
                render = I(Selevctrender),
                placeholder = 'Välj Mormor'
              )
            )
          )
        )
      )
    ),
    box(
      title = "Resultat",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      textOutput("inavelkoff1hona"),
      plotOutput("subPlot1hona")
    )
  )

oregSireandDam <-
  tabItem(
    tabName = "hanehona",
    box(
      title = "Provparning med två oregisterade kaniner",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        box(
          title = span(tagList(
            "Oregistrerad Far", HTML("&nbsp;"), icon("male")
          )),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Farfar", HTML("&nbsp;"), icon("male"))),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'farfar2',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField =
                  'Indiv',
                render = I(Selevctrender),
                placeholder =
                  'Välj Farfar'
              )
            )
          ),
          box(
            width = 6,
            title = span(tagList("Farmor", HTML("&nbsp;"), icon("female"))),
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'farmor2',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField =
                  'Indiv',
                render = I(Selevctrender),
                placeholder = 'Välj Farmor'
              )
            )
          )
        ),
        box(
          title = span(tagList(
            "Oregistrerad Mor", HTML("&nbsp;"), icon("female")
          )),
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Morfar", HTML("&nbsp;"), icon("male"))),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'morfar2',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField = 'Indiv',
                render = I(Selevctrender),
                placeholder = 'Välj Morfar'
              )
            )
          ),
          box(
            width = 6,
            title = span(tagList("Mormor", HTML("&nbsp;"), icon("female"))),
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            selectizeInput(
              'mormor2',
              '',
              choices = NULL,
              options = list(
                valueField = 'Indiv',
                searchField = c('Indiv', 'Name', 'Born'),
                labelField = 'Indiv',
                render = I(Selevctrender),
                placeholder = 'Välj Mormor'
              )
            )
          )
        )
      )
    ),
    box(
      title = "Resultat",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      textOutput("inavelkoffHanehona"),
      plotOutput("subPlotHanehona")
    )
  )


body <-dashboardBody(
    # Boxes need to be put in a row (or column)
  useShinyjs(),
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  ),
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="https://use.fontawesome.com/releases/v5.7.2/css/all.css", integrity="sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr", crossorigin="anonymous")
  ),
  
  tabItems( 
    regulartest,
    oregSiretest,
    oregDamtest,
    oregSireandDam

   
  ),
  textOutput("keepAlive")
      
  )


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session) {
  start_time <- Sys.time() 
  updateSelectizeInput(session, 'SIRE', choices =males ,  server=TRUE)
  updateSelectizeInput(session, 'DAM', choices =females , server=TRUE)
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  observeEvent(input$tabs,
               {
                 if(input$tabs=='1hane')
                 {
                   updateSelectizeInput(session, 'SIRE1', choices =males , server=TRUE)
                   updateSelectizeInput(session, 'DAM1', choices =females, server=TRUE)
                   updateSelectizeInput(session, 'DAM1hane', choices =females , selected=input$DAM,server=TRUE)                 
                 }
                 if(input$tabs=='1hona')
                 {
                   updateSelectizeInput(session, 'SIRE1hona', choices =males,selected=input$SIRE, server=TRUE)
                   updateSelectizeInput(session, 'morfar', choices =males , server=TRUE)
                   updateSelectizeInput(session, 'mormor', choices =females , server=TRUE)
                 }
                 if(input$tabs=='hanehona')
                 {
                   updateSelectizeInput(session, 'farfar2', choices =males , server=TRUE)
                   updateSelectizeInput(session, 'farmor2', choices =females , server=TRUE)
                   updateSelectizeInput(session, 'morfar2', choices =males , server=TRUE)
                   updateSelectizeInput(session, 'mormor2', choices =females , server=TRUE)
                 }
                 
  })
  
  
  
  end_time <- Sys.time()
  cat(file=stderr(),end_time - start_time)
  
  
  shinyjs::hide("ingenHane")
  shinyjs::hide("ingenHona")
  shinyjs::hide("ingen1Hane")
  shinyjs::hide("ingen1Hona")
  observeEvent(input$ingenHane, {
    #updateSelectizeInput(session, 'DAM1hane', choices =females, selected=input$DAM, server=TRUE)
    updateTabItems(session, "tabs", "1hane")
  })
  
  observeEvent(input$ingenHona, {
    updateSelectizeInput(session, 'SIRE1hona', choices =males, selected=input$SIRE, server=TRUE)
    updateTabItems(session, "tabs", "1hona")
  })
  
  observeEvent(input$ingen1Hane, {
    
    updateTabItems(session, "tabs", "hanehona")
  })
  
  observeEvent(input$ingen1Hona, {
    updateTabItems(session, "tabs", "hanehona")
  })
  
  
  
   observeEvent(input$SIRE1, {
    output$inavelkoff2 = renderText(
      {validate(
        need(input$SIRE1, message = 'Vänligen välj en farfar'),
        need(input$DAM1, 'Vänligen välj en farmor'),
        need(input$DAM1hane, 'Vänligen välj en mor')
      )
        isolate({testmate2(Pedi,c(input$SIRE1,input$DAM1),input$DAM1hane)}
        )})
    output$subPlot2 = renderPlot({
      validate(
        need(input$SIRE1, message = FALSE),
        need(input$DAM1, FALSE),
        need(input$DAM1hane, FALSE)
      )
      pedplot(sub2,label=c("Indiv", "Name"), cex=0.5)})
    
  })
  
  observeEvent(input$farfar2, {
    output$inavelkoffHanehona = renderText(
      {validate(
        need(input$farfar2, message = 'Vänligen välj en farfar'),
        need(input$farmor2, 'Vänligen välj en farmor'),
        need(input$morfar2, 'Vänligen välj en morfar'),
        need(input$mormor2, 'Vänligen välj en mormor')
      )
        isolate({testmate2(Pedi,c(input$farfar2,input$farmor2),c(input$morfar2,input$mormor2))}
        )})
    output$subPlotHanehona = renderPlot({
      validate(
        need(input$farfar2, message = FALSE),
        need(input$farmor2, FALSE),
        need(input$morfar2, FALSE),
        need(input$mormor2, FALSE)
      )
      pedplot(sub2,label=c("Indiv", "Name"), cex=0.5)})
    
  })
  
  observeEvent(input$SIRE1hona, {
    output$inavelkoff1hona = renderText(
      {validate(
        need(input$SIRE1hona, message = 'Vänligen välj en far'),
        need(input$morfar, 'Vänligen välj en morfar'),
        need(input$mormor, 'Vänligen välj en mormor')
      )
        isolate({testmate2(Pedi,input$SIRE1hona,c(input$morfar,input$mormor))}
        )})
    output$subPlot1hona = renderPlot({
      validate(
        need(input$SIRE1hona, message = FALSE),
        need(input$morfar, FALSE),
        need(input$mormor, FALSE)
      )
      pedplot(sub2,label=c("Indiv", "Name"), cex=0.5)})
    
  })
    
    
  observeEvent(input$SIRE, {
    
    output$inavelkoff = renderText(
      {validate(
        need(input$SIRE, message = 'Vänligen välj en far'),
        need(input$DAM, 'Vänligen välj en mor')
      )
        isolate({sprintf("Inavelskoefficient	: %1.2f%% ", pKin[input$SIRE,input$DAM]*100)}
        )})
    
    output$subPlot = renderPlot({
      validate(
        need(input$SIRE, message = FALSE),
        need(input$DAM, FALSE)
      )
      imagOff<-data.frame("999-99999",input$SIRE,input$DAM,NA,NA,NA, paste0(getNamefromID(input$SIRE),getNamefromID(input$DAM)), "FALSE","2018")
      tmp<-data.frame(rbind(as.matrix(Pedi), as.matrix(imagOff)))
      sub2<<-subPed(tmp,"999-99999",prevGen = 4,succGen = 0)
      pedplot(sub2,label=c("Indiv", "Name"), cex=0.5)})
  })
  
}



"testmate2" <- function(PED,sire, dam){
  if(!is.data.frame(PED))
    stop("PED should be data.frame")
  
  if(length(sire)+length(dam)==2)
    imagOff<-data.frame("999-99999",sire,dam,NA,NA,NA, "noname", "FALSE","2018")
  
  else if (length(sire)+length(dam)==3){
    if(length(sire)==2){
      imagOff<-data.frame(c("SIRE1","999-99999"),c(sire[1],"SIRE1"),c(sire[2],dam),NA,NA,NA, c("noname","noname2"), "FALSE","2018")
      row.names(imagOff)<-c("SIRE1","999-99999")
    }
    else if (length(dam)==2){
      imagOff<-data.frame(c("DAM1","999-99999"),c(dam[1],sire),c(dam[2],"DAM1"),NA,NA,NA, "noname", "FALSE","2018")
      row.names(imagOff)<-c("DAM1","999-99999")
    }
  }
  else if (length(sire)+length(dam)==4){
    imagOff<-data.frame(c("SIRE1","DAM1","999-99999"),c(sire[1],dam[1],"SIRE1"),c(sire[2],dam[2],"DAM1"),NA,NA,NA, "noname", "FALSE","2018")
    row.names(imagOff)<-c("SIRE1","DAM1","999-99999")
  }
  tmp<-data.frame(rbind(as.matrix(PED), as.matrix(imagOff)))
  inbreeding<-pedInbreeding(tmp)
  sub2<<-subPed(tmp,"999-99999",prevGen = 4,succGen = 0)
  sprintf("Inavelskoefficient	: %1.2f%% ", inbreeding[which(inbreeding$Indiv == "999-99999"),2]*100)
}

getNamefromID <- function(GID){
  Pedi[which(Pedi$Indiv == GID),"Name"]
}


# Run the application 
shinyApp(ui = ui, server = server)
