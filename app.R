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
require(data.table)
require(visNetwork)
suppressPackageStartupMessages(is_installed <- require(visPedigree))


#Uppdatera namn och datum när en ny lista skapats
filename<-"G20200117"
GDBversion<-filename

Pedifilename<-paste0('Pedi',filename, '.rdata')
pKinfilename<-paste0('pKin',filename, '.rdata')

start_time <- Sys.time()

if (file.exists(Pedifilename)){
  load(file=Pedifilename)
}else {
  gottis <-read_excel(paste0('./',filename, '.xlsx'), range = cell_cols("A:V"), col_types = c("text", "skip", "skip","text", "text", "numeric", "date", "skip", "text","skip", "text","skip","skip","skip","skip","skip","skip","skip","skip","skip","numeric","numeric"))
  gottis <- gottis[,c("Nummer","Far nr","Mor nr","Kön","År","Namn","Född","2019","2018")]
  names(gottis)<-c("Indiv","Sire","Dam","Sex","Born","Name","Född","Nitton","Arton")
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
males<-subset(Pedi[with(Pedi,order(Born,decreasing =TRUE)),],Sex=='male' & (!is.na(Arton) | !is.na(Nitton)))
females<-subset(Pedi[with(Pedi,order(Born,decreasing =TRUE)),],Sex=="female" & (!is.na(Arton) | !is.na(Nitton)))
Pedi<-Pedi[,c("Indiv","Sire","Dam","Sex","Born","I","Name","Född","Offspring")]
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
    menuSubItem("En oreg hane", tabName = "1hane", icon = icon("mars")),
    menuItem("En oreg hona", tabName = "1hona", icon = icon("venus")),
    menuItem("Oreg hona och hane", tabName = "hanehona", icon = icon("venus-mars")))
  ),
  tags$br(),
  tags$div(style="text-align: center", tags$i(paste("Databasversion",GDBversion)))
)

regulartest <-tabItem(tabName = "provparning", box(title = "Provparning",status = "primary",solidHeader = TRUE,
                   collapsible = TRUE,width = 12,
                   fluidRow(
                                          div(id="SIREBOX", box(width=6,title = span(tagList("Far",HTML("&nbsp;"),icon("mars"))),status = "primary",solidHeader = TRUE,
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
                                         title = span(tagList("Mor",HTML("&nbsp;"),icon("venus"))),status = "warning",solidHeader = TRUE,
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
      collapsible = TRUE,width = 12,div(id="DEG",uiOutput("reporting")))
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
            "Oregistrerad Far", HTML("&nbsp;"), icon("mars")
          )),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Farfar", HTML("&nbsp;"), icon("mars"))),
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
            title = span(tagList("Farmor", HTML("&nbsp;"), icon("venus"))),
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
            title = span(tagList("Mor", HTML("&nbsp;"), icon("venus"))),
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
      div(id = "DEG2",  uiOutput("inavelkoff2"))
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
          title = span(tagList("Far", HTML("&nbsp;"), icon("mars"))),
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
            "Oregistrerad Mor", HTML("&nbsp;"), icon("venus")
          )),
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Morfar", HTML("&nbsp;"), icon("mars"))),
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
            title = span(tagList("Mormor", HTML("&nbsp;"), icon("venus"))),
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
      uiOutput("inavelkoff1hona")
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
            "Oregistrerad Far", HTML("&nbsp;"), icon("mars")
          )),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Farfar", HTML("&nbsp;"), icon("mars"))),
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
            title = span(tagList("Farmor", HTML("&nbsp;"), icon("venus"))),
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
            "Oregistrerad Mor", HTML("&nbsp;"), icon("venus")
          )),
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          box(
            width = 6,
            title = span(tagList("Morfar", HTML("&nbsp;"), icon("mars"))),
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
            title = span(tagList("Mormor", HTML("&nbsp;"), icon("venus"))),
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
      uiOutput("inavelkoffHanehona")
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

   
  )
  #,tags$div(style="opacity:0.5", textOutput("keepAlive"))
      
  )


ui <- dashboardPage(header, sidebar, body)
SIRE1name<-"FISTEL"
DAM1name<-"DEG"

server <- function(input, output, session) {
  start_time <- Sys.time() 
  updateSelectizeInput(session, 'SIRE', choices =males ,  server=TRUE)
  updateSelectizeInput(session, 'DAM', choices =females , server=TRUE)
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
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
    output$inavelkoff2 = renderUI(
      {validate(
        need(input$SIRE1, message = 'Vänligen välj en farfar'),
        need(input$DAM1, 'Vänligen välj en farmor'),
        need(input$DAM1hane, 'Vänligen välj en mor')
      )
        #isolate({testmate2(Pedi,c(input$farfar2,input$farmor2),c(input$morfar2,input$mormor2))}
        sire<<-paste0('(',input$SIRE1,'+',input$DAM1,')')
        dam<<-input$DAM1hane
        SIRE1name<<-paste0(getNamefromID(input$SIRE1),getNamefromID(input$DAM1))
        DAM1name<<-getNamefromID(dam)
        imagOff<-data.frame(c("SIRE1","999-99999"),c(input$SIRE1,"SIRE1"),c(input$DAM1,dam),NA,NA,NA, c(SIRE1name, paste0(SIRE1name,DAM1name)), Sys.Date(),"FALSE")
        tmp<-data.frame(rbind(as.matrix(Pedi), as.matrix(imagOff)))
        sub2<<-subPed2(tmp,"999-99999",prevGen = 5,succGen = 0) 
        sub2<-tidyped(sub2,cand = "999-99999")
        sub3<<-ped2igraph2(sub2,compact=FALSE)
        inbreeding<-pedInbreeding(tmp)
        Inaveltext<<-sprintf("Inavelskoefficient	: %1.2f%% ", inbreeding[which(inbreeding$Indiv == "999-99999"),2]*100)
        
        output$subPlotSIRE1 = renderVisNetwork({
          graf<-visNetwork(sub3$node, sub3$edge)%>%visEdges(arrows = "to")  %>%visOptions(highlightNearest = TRUE) %>%
            visIgraphLayout(layout="layout_as_tree",type="full") 
          #%>% visHierarchicalLayout(direction = "DU",sortMethod = "directed",levelSeparation = 250, nodeSpacing = 150) 
        })
        tagList(
          
          isolate({Inaveltext}),
          visNetworkOutput("subPlotSIRE1",height = "600",width="100%"),
          downloadButton("report4", "Ladda ned rapport")
        )
        
        
      })
    
  })
  
  
  observeEvent(input$farfar2, {
    output$inavelkoffHanehona = renderUI(
      {validate(
        need(input$farfar2, message = 'Vänligen välj en farfar'),
        need(input$farmor2, 'Vänligen välj en farmor'),
        need(input$morfar2, 'Vänligen välj en morfar'),
        need(input$mormor2, 'Vänligen välj en mormor')
      )
        #isolate({testmate2(Pedi,c(input$farfar2,input$farmor2),c(input$morfar2,input$mormor2))}
        sire<<-paste0('(',input$farfar2,'+',input$farmor2,')')
        dam<<-paste0('(',input$morfar2,'+',input$mormor2,')')
        SIRE1name<<-paste0(getNamefromID(input$farfar2),getNamefromID(input$farmor2))
        DAM1name<<-paste0(getNamefromID(input$morfar2),getNamefromID(input$mormor2))
        imagOff<-data.frame(c("SIRE1","DAM1","999-99999"),c(input$farfar2,input$morfar2,"SIRE1"),c(input$farmor2,input$mormor2,"DAM1"),NA,NA,NA, c(SIRE1name,DAM1name,paste0(SIRE1name,DAM1name)), Sys.Date(),"FALSE")
        tmp<-data.frame(rbind(as.matrix(Pedi), as.matrix(imagOff)))
        sub2<-subPed2(tmp,"999-99999",prevGen = 5,succGen = 0) 
        sub2<-tidyped(sub2,cand = "999-99999")
        sub3<<-ped2igraph2(sub2,compact=FALSE)
        inbreeding<-pedInbreeding(tmp)
        Inaveltext<<-sprintf("Inavelskoefficient	: %1.2f%% ", inbreeding[which(inbreeding$Indiv == "999-99999"),2]*100)
        
        output$subPlothh = renderVisNetwork({
          graf<-visNetwork(sub3$node, sub3$edge)%>%visEdges(arrows = "to")  %>%visOptions(highlightNearest = TRUE) %>%
            visIgraphLayout(layout="layout_as_tree",type="full") 
          #%>% visHierarchicalLayout(direction = "DU",sortMethod = "directed",levelSeparation = 250, nodeSpacing = 150) 
        })
        tagList(
          
          isolate({Inaveltext}),
          visNetworkOutput("subPlothh",height = "600",width="100%"),
          downloadButton("report3", "Ladda ned rapport")
        )
        
        
      })
    
  })
      
  observeEvent(input$SIRE1hona, {
    output$inavelkoff1hona = renderUI(
      
      {validate(
        need(input$SIRE1hona, message = 'Vänligen välj en far'),
        need(input$morfar, 'Vänligen välj en morfar'),
        need(input$mormor, 'Vänligen välj en mormor')
      )
      #Inaveltext<-testmate2(Pedi,input$SIRE1hona,c(input$morfar,input$mormor))
        sire<<-input$SIRE1hona
        dam<<-paste('(',input$morfar,'+',input$mormor,')')
        SIRE1name<<-getNamefromID(sire)
        DAM1name<<-paste0(getNamefromID(input$morfar),getNamefromID(input$mormor))
        imagOff<-data.frame(c("DAM1","999-99999"),c(input$morfar,sire),c(input$mormor,"DAM1"),NA,NA,NA, c(DAM1name,paste0(SIRE1name,DAM1name)), Sys.Date(),"FALSE")
        tmp<-data.frame(rbind(as.matrix(Pedi), as.matrix(imagOff)))
        sub2<-subPed2(tmp,"999-99999",prevGen = 5,succGen = 0) 
        sub2<-tidyped(sub2,cand = "999-99999")
        sub3<<-ped2igraph2(sub2,compact=FALSE)
        inbreeding<-pedInbreeding(tmp)
        Inaveltext<<-sprintf("Inavelskoefficient	: %1.2f%% ", inbreeding[which(inbreeding$Indiv == "999-99999"),2]*100)
      
        output$subPlot1hona = renderVisNetwork({
          graf<-visNetwork(sub3$node, sub3$edge)%>%visEdges(arrows = "to")  %>%visOptions(highlightNearest = TRUE) %>%
            visIgraphLayout(layout="layout_as_tree",type="full") 
          #%>% visHierarchicalLayout(direction = "DU",sortMethod = "directed",levelSeparation = 250, nodeSpacing = 150) 
        })
        
        tagList(
          
          isolate({Inaveltext}),
          visNetworkOutput("subPlot1hona",height = "600",width="100%"),
          downloadButton("report2", "Ladda ned rapport")
        )
        
         
        })
    
  })
    
    
  observeEvent(input$SIRE, {
   
    output$reporting = renderUI(
      {validate(
        need(input$SIRE, message = 'Vänligen välj en far'),
        need(input$DAM, 'Vänligen välj en mor')
      )
      sire<<-input$SIRE
      dam<<-input$DAM
      SIRE1name<<-getNamefromID(sire)
      DAM1name<<-getNamefromID(dam)
        
      imagOff<-data.frame("999-99999",sire,dam,NA,NA,NA, paste0(SIRE1name,DAM1name), Sys.Date(),"FALSE")
      tmp<-data.frame(rbind(as.matrix(Pedi), as.matrix(imagOff)))
      sub2<<-subPed2(tmp,"999-99999",prevGen = 5,succGen = 0) 
      sub2<-tidyped(sub2,cand = "999-99999")
      sub3<<-ped2igraph2(sub2,compact=FALSE)
      Inaveltext<<-sprintf("Inavelskoefficient	: %1.2f%% ", pKin[input$SIRE,input$DAM]*100)
      output$subPlot = renderVisNetwork({
          graf<-visNetwork(sub3$node, sub3$edge)%>%visEdges(arrows = "to")  %>%visOptions(highlightNearest = TRUE) %>%
          visIgraphLayout(layout="layout_as_tree",type="full")
        #%>% visHierarchicalLayout(direction = "DU",sortMethod = "directed",levelSeparation = 250, nodeSpacing = 150) 
        })
        tagList(
        
        isolate({Inaveltext}),
        visNetworkOutput("subPlot",height = "600",width="100%"),
        downloadButton("report1", "Ladda ned rapport")
        )
        
        })
      
      
  })
  lapply(1:4, function(i) {
  output[[paste0("report", i)]]<- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){
      paste("Provparning-",paste(sire,collapse="_"),"x",paste(dam,collapse = "_"),"-",format(Sys.Date(), "%Y"), ".html", sep="")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      
      params <- list(SIRE = sire,
                     DAM= dam,
                     SIRENAME = SIRE1name,
                     DAMNAME = DAM1name,
                     node=sub3$node,
                     edge=sub3$edge,
                     inavel= Inaveltext)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )})})
  
}



"testmate2" <- function(PED,sire, dam){
  if(!is.data.frame(PED))
    stop("PED should be data.frame")
  
  if(length(sire)+length(dam)==2)
    imagOff<-data.frame("999-99999",sire,dam,NA,NA,NA, "noname", "FALSE","2018")
  
  else if (length(sire)+length(dam)==3){
    if(length(sire)==2){
      imagOff<-data.frame(c("SIRE1","999-99999"),c(sire[1],"SIRE1"),c(sire[2],dam),NA,NA,NA, c(paste0(getNamefromID(sire[1]),getNamefrodmID(sire[2])),paste0(getNamefromID(sire[1]),getNamefromID(sire[2]),getNamefromID(dam))), Sys.Date(),"FALSE")
      row.names(imagOff)<-c("SIRE1","999-99999")
    }
    else if (length(dam)==2){
      imagOff<-data.frame(c("DAM1","999-99999"),c(dam[1],sire),c(dam[2],"DAM1"),NA,NA,NA, c(paste0(getNamefromID(dam[1]),getNamefromID(dam[2])),paste0(getNamefromID(sire),getNamefromID(dam[1]),getNamefromID(dam[2]))), Sys.Date(),"FALSE")
      row.names(imagOff)<-c("DAM1","999-99999")
    }
  }
  else if (length(sire)+length(dam)==4){
    imagOff<-data.frame(c("SIRE1","DAM1","999-99999"),c(sire[1],dam[1],"SIRE1"),c(sire[2],dam[2],"DAM1"),NA,NA,NA, "noname", Sys.Date(),"FALSE")
    row.names(imagOff)<-c("SIRE1","DAM1","999-99999")
  }
  tmp<-data.frame(rbind(as.matrix(PED), as.matrix(imagOff)))
  sub2<<-subPed2(tmp,"999-99999",prevGen = 5,succGen = 0) 
  sub2<-tidyped(sub2,cand = "999-99999")
  sub3<-ped2igraph2(sub2,compact=FALSE)
  inbreeding<-pedInbreeding(tmp)
  sprintf("Inavelskoefficient	: %1.2f%% ", inbreeding[which(inbreeding$Indiv == "999-99999"),2]*100)
}

getNamefromID <- function(GID){
  Pedi[which(Pedi$Indiv == GID),"Name"]
}

"subPed2"<-function(Pedig, keep, prevGen=3, succGen=0){
  PedigAsDataTable <- "data.table" %in% class(Pedig)
  Pedig <- as.data.frame(Pedig)
  if(PedigAsDataTable){setDF(Pedig)}
  colnames(Pedig)[1:3]<-c("Indiv", "Sire", "Dam")
  if(is.logical(keep)){keep<-Pedig$Indiv[keep]}
  Pedig<-prePed(Pedig, lastNative=1234567)
  selected  <- Pedig$Indiv %in% keep
  inPrevGen <- selected
  if(prevGen>0){
    for(i in 1:prevGen){
      inPrevGen <- inPrevGen | Pedig$Indiv %in% Pedig$Sire[inPrevGen] | Pedig$Indiv %in% Pedig$Dam[inPrevGen] 
    }
  }
  inSuccGen <- selected
  if(succGen>0){
    for(i in 1:succGen){
      inSuccGen <- inSuccGen |  Pedig$Sire %in% Pedig$Indiv[inSuccGen] |  Pedig$Dam %in% Pedig$Indiv[inSuccGen]
    }
    Sires <- Pedig$Sire[inSuccGen & !selected]
    Dams  <- Pedig$Dam[inSuccGen & !selected]
    inSuccGen <- inSuccGen | Pedig$Indiv %in% Sires | Pedig$Indiv %in% Dams 
  }  
  Pedig <- Pedig[selected | inPrevGen | inSuccGen, ]
  Pedig[!(Pedig$Sire %in% Pedig$Indiv), "Sire"] <- NA
  Pedig[!(Pedig$Dam %in% Pedig$Indiv),   "Dam"] <- NA
  #Pedig<-prePed(Pedig, lastNative=1234567)
  Pedig$keep<-Pedig$Indiv %in% keep
  if(PedigAsDataTable){setDT(Pedig)}
  Pedig
}

ped2igraph2 <- function(ped,compact=TRUE) {
  ped_new <- copy(ped)
  ped_col_names <- colnames(ped_new)
  # There is the Cand column in the pedigree if it is traced by the tidyped function
  if (c("Cand") %in% ped_col_names) {
    ped_node <-
      ped_new[, .(
        id = IndNum,
        label = paste0(Name,"\n",Ind,"\n",Född),
        #label = Ind,
        sirenum = SireNum,
        damnum = DamNum,
        sirelabel = Sire,
        damlabel = Dam,
        cand = Cand,
        sex = Sex,
        gen = Gen
      )]
  } else {
    ped_node <-
      ped_new[, .(
        id = IndNum,
        label = Ind,
        sirenum = SireNum,
        damnum = DamNum,
        sirelabel = Sire,
        damlabel = Dam,
        sex = Sex,
        gen = Gen
      )]
  }
  
  max_id <- max(ped_node$id,na.rm = TRUE)
  
  # Adding two new columns family label (column name: familylabel) and it's numeric id
  # (column name: familynum) in the ped_node
  familylabel = NULL # due to NSE notes in R CMD check
  ped_node[!(is.na(sirelabel) &
               is.na(damlabel)),
           familylabel := paste(sirelabel, damlabel, sep = "")]
  family_label <- unique(ped_node$familylabel)
  family_label <- family_label[!is.na(family_label)]
  family_num <-
    setDT(list(
      familynum = seq(
        from = max(ped_node$id,na.rm=TRUE) + 1,
        to = max(ped_node$id,na.rm=TRUE) + length(family_label)
      ),
      familylabel = family_label
    ))
  ped_node <-
    merge(ped_node,
          family_num,
          by = c("familylabel"),
          all.x = TRUE)
  ped_node[is.na(familynum), familynum := 0]
  
  # There will be three node types in the ped_note, including real, compact, and virtual.
  # Real nodes are all individuals in the pedigree.
  # Compact nodes are full-sib individuals with parents, but without progeny,
  # they exist only when the "compact" paramete is TRUE
  nodetype = NULL # due to NSE notes in R CMD check
  ped_node[,nodetype:="real"]
  
  #=== Compact the pedigree============================================================
  # Full-sib individuals with parents but without progeny will be deleted from ped_note.
  # count individuals by family and sex as a number of node  replace full-sib individuals
  if (compact) {
    # Finding the individuals with parents, but without progeny
    sire_dam_label <- unique(c(ped_node$sirelabel,ped_node$damlabel))
    sire_dam_label <- sire_dam_label[!is.na(sire_dam_label)]
    ped_node_1 <- ped_node[!(label %in% sire_dam_label)]
    
    # Moreover, finding full-sib individuals
    familysize <- NULL
    ped_node_1[,familysize:=.N,by=.(familylabel,sex)]
    if (max(ped_node_1$familysize,na.rm=TRUE)>=2) {
      # The full-sib individuals in a family will be compacted if the family size >= 2
      fullsib_id_DT <- ped_node_1[familysize >=2]
      fullsib_ids <- fullsib_id_DT$id
      familylabelsex = NULL # due to NSE notes in R CMD check
      fullsib_id_DT[,familylabelsex:=paste(familylabel,sex,sep="")]
      # Generating a compact family dataset, only including maximum three individuals for
      # each family: male, female and unknown sex individuals
      fullsib_family_label_sex <- unique(fullsib_id_DT$familylabelsex)
      compact_family <- fullsib_id_DT[match(fullsib_family_label_sex,familylabelsex)]
      # The compact families' id are the number of individuals by family and sex.
      compact_family[,":="(label=familysize,nodetype="compact")]
      # Deleting full-sib individuals from families with 2 and more full-sib individuals
      ped_node <- ped_node[!(id %in% fullsib_ids)]
      ped_node <- rbind(ped_node,compact_family,fill=TRUE)
    }
  }
  
  #=== Add virtual nodes between parents and progrenies================================
  # Add id to familynum and familynum to sirenum and damnum as new virtual edges
  ped_edge <-
    rbind(ped_node[, .(from = id, to = sirenum)],
          ped_node[, .(from = id, to = damnum)])
  ped_edge <- ped_edge[!(to == 0)]
  # Delete duplicated edges from familynum to parents
  ped_edge <- unique(ped_edge)
  ped_edge <- ped_edge[order(from, to)]
  size = arrow.size = arrow.width = color = curved = NULL # due to NSE notes in R CMD check
  # grey
  #ped_edge[,":="(size=1,arrow.size=1,arrow.width=1,color="#9d96ad",curved=0.15)]
  #ped_edge[,":="(size=1,arrow.size=1,arrow.width=1,color="#a69f89",curved=0.15)]
  #ped_edge[,":="(size=1,arrow.size=1,arrow.width=1,color="#afa8be",curved=0.10)]
  ped_edge[,":="(size=1,arrow.size=1,arrow.width=1,color="#333333",curved=0.10)]
  # Add familynum as new virtual nodes
  # ped_node <-
  #   rbind(ped_node, unique(ped_node[familynum > 0, .(
  #     id = familynum,
  #     familylabel,
  #     label = familylabel,
  #     sirenum,
  #     damnum,
  #     sirelabel,
  #     damlabel,
  #     gen,
  #     familynum
  #   )]), fill = TRUE)
  ped_node[is.na(nodetype),nodetype:="virtual"]
  layer = NULL # due to NSE notes in R CMD check
  #ped_node[nodetype %in% c("real","compact"),layer:=gen]
  #ped_node[nodetype %in% c("real","compact"),level:=gen]
  ped_node[nodetype %in% c("virtual"),layer:=2*(gen-1)]
  
  
  #=== Set default shape, size and color for male and female===========================
  # Setting the default attributes of nodes
  # Notes: size = 15 means the width of a circle node account for 15% of the whole width
  # of the graph
  #ped_node[, ":="(shape = "circle", frame.color="#8495e8", color="#9daaea",size = 15)]
  #ped_node[, ":="(shape = "circle", frame.color="black", color="#aaa16c",size = 15)]
  shape = frame.color = color = size = label.color = NULL
  ped_node[, ":="(shape = "circle", frame.color="#7fae59", color="#9cb383",size = 15, label.color="#0d0312")]
  #ped_node[, ":="(frame.color="#7fae59", color="#9cb383",label.cex=1, label.color="#0d0312")]
  #ped_node[, ":="(shape = "circle", frame.color=NA, color="#9cb383",size = 15)]
  ped_node[nodetype %in% c("compact"), ":="(shape="square")]
  # Setting virtual size of nodes to 0.0001
  ped_node[id > max_id,":="(shape="none",label="",size=0)]
  # Setting male and female's color
  ped_node[sex %in% c("male"), ":="(frame.color="#0e8dbb", color = "#119ecc", font.size=10,shape ="box",heightConstraint =35)]
  ped_node[sex %in% c("female"), ":="(frame.color="#e6a11f", color = "#f4b131",font.size=10, shape ="circle")]
  #ped_node[sex %in% c("male"), ":="(frame.color="#0e8dbb", color = "#119ecc", shape ="box",heightConstraint =55)]
  #ped_node[sex %in% c("female"), ":="(frame.color="#e6a11f", color = "#f4b131", shape ="circle")]
  
  # The edge color is same with the color of the it's "to" node.
  min_familynum <- min(family_num$familynum)
  ped_edge <- merge(ped_edge,
                    ped_node[,.(id,tonodecolor=color)],
                    by.x="to", by.y="id",all.x=TRUE)
  ped_edge[from >= min_familynum,":="(color=tonodecolor)]
  ped_edge[from < min_familynum,":="(curved=0)]
  
  
  # Sorting the "from" and "to" columns as the first two columns in the ped_edge
  old_names <- colnames(ped_edge)
  new_names <- c(c("from","to"),old_names[!(old_names %in% c("from","to"))])
  ped_edge <- ped_edge[, ..new_names]
  ped_edge <- ped_edge[order(from,to)]
  
  
  # Sorting the "id" column as the first column in the ped_node
  old_names <- colnames(ped_node)
  new_names <- c("id",old_names[!(old_names %in% c("id"))])
  ped_node <- ped_node[, ..new_names]
  ped_node <- ped_node[order(layer,id)]
  
  return(list(node = ped_node, edge = ped_edge))
  
}


#enableBookmarking(store = "url")
# Run the application 
shinyApp(ui = ui, server = server)
