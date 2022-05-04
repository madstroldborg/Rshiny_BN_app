library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(shinydashboard)
library(bnspatial)
library(bnlearn)
library(plyr)
library(dplyr)
library(sf)
library(visNetwork)
library(shinyjqui)
library(rsconnect)
library(gRain)
library(tools)
library(DT)
library(sqldf)
library(AnnotationDbi)

# ============================================================================

# ---------------------------------------------------------------------------
# GLOBAL VARIABLES & PRE-LOADED NETWORKS
# ---------------------------------------------------------------------------

# PRE-LOADED NETWORKS
# The following networks are included:
# (1) Groundwater leaching BN: this network is based on the leaching component  
# from the pesticide risk assessment BN developed by Troldborg et al.(2022), see:
# https://hess.copernicus.org/articles/26/1261/2022/
GW_bn <- read.net("MyTest Discretized.net",debug=FALSE)

# (2) Asia: this network is based on Lauritzen & Spiegelhalter (1988) and is a 
# simple illustration of a net that can be used to diagnose patients arriving
# at a clinic. Specific diseases considered include tuberculosis, lung cancer 
# and bronchitis, the potential causes/contributing factors are visit to asia 
# and smoking, whereas potential symptoms are dyspnea (shortness of breath) or 
# abnormal x-ray.  
# The network is included in the bnlearn package
Asia_bn <- read.net("asia.net",debug=FALSE)
# alternatively - fit network to data
#   data(asia,package = "bnlearn")
#   names(asia) <- c("Asia","smoke","tub","lung","bronc","either","xray","dysp")
#   Asia_data <- asia
#   dag = model2network("[Asia][smoke][tub|Asia][lung|smoke][bronc|smoke][dysp|bronc:either][either|tub:lung][xray|either]")
#   Asia_bn <- bn.fit(dag,Asia_data)

# (3) Stroke: this network is based on the study by Sebastiani, P et al. (2005), see:
# https://pubmed.ncbi.nlm.nih.gov/15778708/
# The network is provided as an R object by Chen et al (2019), see: 
# https://github.com/JiajinChen/shinyBN
# https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-3309-0
load("data/Stroke_bn.rdata")

source("intro_page.R")

Evid_tab  <- data.frame(Evidence=character(),Value = character(),stringsAsFactors=FALSE)
Query_tab <- data.frame(Query=character(),   Value = character(),stringsAsFactors=FALSE)
result    <- data.frame()
n_AE <- 0
n_DE <- 0
n_AQ <- 0
n_DQ <- 0
n_ClearEvi <- 0
n_ClearQue <- 0

# ==============================================================================
# UI DASHBOARD
# ==============================================================================

ui <- dashboardPage(
  header  <- dashboardHeader(titleWidth = 275, #title=tags$a(href='https://www.hutton.ac.uk/',tags$img(height=40,src='logo_Horiz.png'), align="left")),
                             title= h3(strong("BN dashboard"),align="left",style="padding:0px;"), 
                             tags$li(
                               class = "dropdown",
                               fluidRow(
                                 column(
                                   width = 4,
                                   offset = 4,
                                   align = "center"
                                 ),
                                 column(
                                   width = 2,
                                   offset = 10,
                                   #align = "right",
                                   tags$a(href='https://www.hutton.ac.uk/',tags$img(src='logo_Horiz.png',height=20))
                                 )
                               )
                             )
  ),# end of header
  
  
  sidebar <- dashboardSidebar(
    width = 275,
    sidebarMenu(id="sidebar",
                menuItem("Introduction", tabName = 'introduction', icon = icon("info-circle", lib="font-awesome")), #
                menuItem("Open network", tabName = 'opennet',      icon = icon("folder-open", lib="glyphicon")),
                menuItem("Inference",    tabName = 'inference',    icon = icon("th")),
                
                conditionalPanel(condition = "input.sidebar == 'opennet'", 
                                 radioButtons(inputId = "inType", label= strong("Select the type of input:"),
                                              choices = c("Pre-loaded net","Load your own net"),
                                              selected = "Pre-loaded net",width="100%"),
                                 
                                 conditionalPanel(condition = "input.inType == 'Load your own net'", 
                                                  # Input: Select a file ----
                                                  fileInput("inBNfile", "Choose Network File",  # (accepted formats: .net, .dot, .dsc, .bif)
                                                            multiple = FALSE,
                                                            accept = c(".net",".bif", ".dsc", ".dot")),
                                                  div(style = "margin-top: -30px"),
                                                  column(width=12,p("Accepted formats: .net, .dot, .dsc, .bif"))), # end of conditional panel for load your own net 
                                 conditionalPanel(condition = "input.inType == 'Pre-loaded net'", 
                                                  selectInput(inputId = "inBNdb", label = "Choose pre-loaded net", 
                                                              choices=c("GW_bn","Stroke_bnfit", "Asia_bn"), 
                                                              selected = "GW_bn")) # end of conditional panel for preloaded nets
                ), # end of conditional panel for opennet
                
                conditionalPanel(condition = "input.sidebar == 'inference'", 
                                 uiOutput("target"),
                                 column(width=12,tags$hr()), #br("")),
                                 uiOutput("evidence"),
                                 uiOutput("E_value"),
                                 # Add action buttons
                                 #   the style argument within the actionButton function is for changing the colours, e.g.
                                 #   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                 div(style="display:inline-block;width:28%;text-align: center;",
                                     actionButton(inputId = "AddButtonE",  label = "Add",   icon=icon("plus"),   lib="glyphicon",
                                                  style="background-color: #e6e6e6; border-color: #c0c0c0")),
                                 #style="background-color: #ddf1e0; border-color: #008937")),
                                 div(style="display:inline-block;width:32%;text-align: center;",
                                     actionButton(inputId = "delButtonE",  label = "Delete",icon=icon("trash"),  lib="glyphicon", 
                                                  style="background-color: #e6e6e6; border-color: #c0c0c0")),
                                 #style="background-color: #ffe0da; border-color: #ff7c73")),
                                 div(style="display:inline-block;width:32%;text-align: center;",
                                     actionButton(inputId = "ClearButtonE",label = "Clear", icon=icon("refresh"),lib="glyphicon", 
                                                  style="background-color: #e6e6e6; border-color: #c0c0c0")),
                                 column(width=12,br(""))
                                 
                ) # end of conditional panel for inference
                
    ) # end of sidebarMenu
  ),  # end of dashboardSidebar
  
  body <- dashboardBody(
    
    tags$style(
      ".main-header .navbar-custom-menu {
          width: calc(100% - 50px);
        }

        .main-header .navbar-nav,
        .main-header .dropdown {
          width: 100%;
        }

        .main-header img {
          height: 50px
        }"
    ),
    
    
    
    tabItems(
      # First tabItem
      intro_page, 
      
      # Second tabItem
      tabItem(tabName = "opennet",   # NOTE: tabName must match the tabName in the sidebar
              tabsetPanel(id = "network_tab",
  
                          tabPanel("Graph view",
                                   #h4("Plot tab 2 content"),
                                   visNetworkOutput("mygraph",width = "100%", height=750),
                                   fixedPanel(top = 150, right=0, width=380,height=15,draggable = F,
                                              column(width=6,h5()),
                                              column(width=4,selectInput("inLayout",NULL,c("Layer"="layout_with_sugiyama","Circle"="layout_in_circle",
                                                                                           "Star"="layout_as_star","Tree"="layout_as_tree","Grid"="layout_on_grid"))))),
                          tabPanel("CPT",
                                   column(width=4,offset=8,uiOutput("cptnode")),
                                   #column(width=12,br("")),
                                   column(width=12,dataTableOutput("CPT")))#,
                 
              ) # end of tabsetPanel
      ), # end of second tabItem
      
      # Third tab content
      tabItem(tabName = "inference",
              tabsetPanel(id = "inference_tab",
                          tabPanel("Bar plots",
                                   #h2("Inference tab 1 content"),  
                                   column(width=12,
                                          plotOutput("plot1", width="100%", height = "500px")),
                                   column(width=12,dataTableOutput("Evi_table"))
                          ),
                          tabPanel("Graph",
                                   plotOutput("mybargraph2", width="100%",height = "1000"))
                  
              ) # end of tabsetPanel
      ) # end of tabItem
    ) # end of tabItems
  ) # end of dashboardBody
) # end of dashboardPage



# ===============================
# S E R V E R   F U N C T I O N 
# ===============================
server <- function(input, output) {
  
  
  # Function to load existing network from 'database' or from a file in class bn.fit
  getNet <- reactive({
    
    bn_fit <- NULL
    if(input$inType=='Pre-loaded net'){
      inBN <- input$inBNdb
      validate(
        need(try(exists(inBN,mode="list")),"The chosen net does not exist as an object"),
        need(try("bn.fit" %in% class(get(inBN)) | "bn" %in% class(get(inBN))), 
             "The chosen net does not exist as a bn_fit object")
      )
      bn_fit <- get(inBN)
    }
    else if(input$inType=='Load your own net') {
      req(input$inBNfile)
      
      # Check input file format
      myFileExt <- file_ext(input$inBNfile$datapath)
      
      if( myFileExt == "net") bn_fit <- read.net(input$inBNfile$datapath,debug=FALSE)
      if( myFileExt == "dsc") bn_fit <- read.dsc(input$inBNfile$datapath,debug=FALSE)
      if( myFileExt == "bif") bn_fit <- read.bif(input$inBNfile$datapath,debug=FALSE)
      if( myFileExt == "dot") bn_fit <- read.dot(input$inBNfile$datapath,debug=FALSE)
    }
    
    bn_fit
  })
  
  
  # Reactive UI output
  # get evidence nodes
  output$evidence <- renderUI({
    myBBN <- getNet()
    if(! is.null(myBBN) & "bn.fit" %in% class(myBBN)){
      Nodelist  <- bnlearn::nodes(myBBN)
      rootNodes <- root.nodes(myBBN)
      selectInput("inEvidence","Select an evidence node:",Nodelist, selected = rootNodes[1])
    }
  })
  
  # get states for evidence nodes
  output$E_value <- renderUI({
    myBBN <- getNet()
    if(! is.null(myBBN) & ! is.null(input$inEvidence) & "bn.fit" %in% class(myBBN)){
      tmp = myBBN[[input$inEvidence]]
      valuelist = rownames(tmp$prob)
      if(length(valuelist)) selectInput("inEValue","Select state for the evidence node:",choices=valuelist)
    }
  })
  
  # get target node
  output$target <- renderUI({
    myBBN <- getNet()
    if(! is.null(myBBN) & "bn.fit" %in% class(myBBN)){
      Nodelist  = bnlearn::nodes(myBBN)
      leafNodes = leaf.nodes(myBBN)
      selectInput("inTarget","Select the target node:",Nodelist, selected=leafNodes[1])
    }
  })
  
  
  # get cpt node
  output$cptnode <- renderUI({
    myBBN <- getNet()
    if(! is.null(myBBN) & "bn.fit" %in% class(myBBN)){
      Nodelist  = bnlearn::nodes(myBBN)
      leafNodes = leaf.nodes(myBBN)
      selectInput("inTable",label=NULL,Nodelist, selected=leafNodes[1])
    }
  })
  
  
  # Function for recording evidence in a data frame
  # This function is completely based on Chen et al. 2019
  RecE <- reactive({
    inputE <- input$inEvidence
    
    # First, remove all evidence that are not relevant for the chosen network  
    # from evidence table. This is only relevant when the chosen network is changed
    myBBN    <- getNet()
    Nodelist <- bnlearn::nodes(myBBN)
    indexE = Evid_tab$Evidence %in% Nodelist 
    Evid_tab <<- Evid_tab[indexE,]
    
    # add chosen evidence to evidence table if add button is clicked
    if(input$AddButtonE == n_AE + 1) {
      n_AE <<- n_AE + 1
      Evid_tab <<- rbind(Evid_tab,data.frame(Evidence = inputE,Value = input$inEValue,stringsAsFactors=FALSE))
    }
    
    # delete chosen evidence from evidence table if delete button is clicked
    if(input$delButtonE == n_DE + 1) {
      n_DE <<- n_DE + 1
      indexE = which(Evid_tab$Evidence != inputE)  
      Evid_tab <<- Evid_tab[indexE,]
    }
    
    # remove all evidence from evidence table if clear button is clicked
    if(input$ClearButtonE == n_ClearEvi + 1){
      n_ClearEvi <<- n_ClearEvi + 1
      Evid_tab <<- data.frame(Evidence=character(),Value=character(),stringsAsFactors=FALSE)
    }
    
    Evid_tab <<- Evid_tab[!duplicated(Evid_tab$Evidence, fromLast=TRUE), ]
    rownames(Evid_tab) <- NULL
    Evid_tab
  })
  
  output$Evi_table <- renderDataTable(RecE(),rownames = FALSE,options=list(searching=T,
                                                                           columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  
  bn <- reactive({
    myBBN       <- getNet()
    myBBN_grain <- as.grain(myBBN)
    E           <- RecE()
    myBBN_upd   <- setEvidence(myBBN_grain, nodes=E$Evidence, states=E$Value)
    q           <- querygrain(myBBN_upd,nodes=input$inTarget, type="marginal")
    #setEvidence(myBBN_grain, nodes=input$inEvidence, states=input$inEValue)
    
    myresult  <- unlist2(q)
    
  })
  
  # get Conditional Probability Table (CPT)
  getCPT <- reactive({
    myBBN        <- getNet()
    myBBN_grain  <- as.grain(myBBN)
    myStates     <- myBBN_grain$universe$levels
    # find parent nodes and their states for the selected node 
    myparents    <- myBBN[[input$inTable]]$parents
    parentstates <- myStates[myparents]
    targetstates <- unlist(myStates[input$inTable])
    
    np <- length(myparents)
    ns <- length(targetstates)
    nr <- prod(myBBN_grain$universe$nlev[myparents])
    myCPT     <- expand.grid(parentstates)
    myCPTvals <- as.data.frame(matrix(nrow = nr, ncol = ns,myBBN_grain$cptlist[[input$inTable]], byrow=TRUE))
    myCPTvals <- round(myCPTvals,3)*100
    names(myCPTvals) <- targetstates
    if (np>0) myCPT  <- cbind(myCPT,myCPTvals)
    else myCPT <- myCPTvals
    
  })
  
  # write CPT as table
  output$CPT <- renderDataTable(getCPT(),rownames = FALSE,options=list(searching=T,
                                                                       columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  # plot probability of selected target node as bar plot given specified scenario
  output$plot1 <- renderPlot({
    
    barplot(bn(), col="lightblue", ylim=c(0,1), ylab="probability",
            main=paste0("Probability of ", input$inTarget))
    #barplot(querygrain(bn(),input$inTarget)[[1]], main=paste0("probability of ", input$inTarget))
    #barplot(querygrain(bn(),input$target)[[1]], main=paste0("probability of ", input$target))
  })
  
  
  # update BN with evidence
  bn_upd <- reactive({
    myBBN       <- getNet()
    myBBN_grain <- as.grain(myBBN)
    E           <- RecE()
    myBBN_upd   <- setEvidence(myBBN_grain, nodes=E$Evidence, states=E$Value)
  })
  
  # plot network with probability bars for the given specified scenario
  output$mybargraph2 <- renderPlot({
    myBBN <- as.bn.fit(bn_upd(), including.evidence=TRUE) 
    graphviz.chart(myBBN, type="barprob", bar.col = "darkgreen", strip.bg = "lightskyblue")
  })
  
  
  
  # plot the specified scenario
  output$mybargraph <- renderPlot({
    myBBN <- getNet()
    graphviz.chart(myBBN, type="barprob", bar.col = "darkgreen", strip.bg = "lightskyblue")
  })
  
  # plot the network graph using visNetwork
  output$mygraph <- renderVisNetwork({
    myBBN     <- getNet()
    myNodes   <- bnlearn::nodes(myBBN)  # nodes names
    nn        <- nnodes(myBBN) # number of nodes
    rootNodes <- root.nodes(myBBN)
    nr        <- length(rootNodes)
    
    # Processing for vis plotting
    edges = as.data.frame(arcs(myBBN))
    edges$arrows = "to"
    nodes = data.frame(label=bnlearn::nodes(myBBN))
    nodes$id = 1:length(myNodes)
    edges$from <- nodes$id[match(edges$from,nodes$label)] # replace with id
    edges$to   <- nodes$id[match(edges$to,nodes$label)]   # replace with id
    
    nodes$group = "Intermediate"
    nodes$group[match(rootNodes, nodes$label)]         <- "Root"
    nodes$group[match(leaf.nodes(myBBN), nodes$label)] <- "Leaf"
    nodes$shape = "ellipse"
    visNetwork(nodes, edges) %>% visLegend() %>%
      visIgraphLayout(randomSeed=10,layout=input$inLayout)
  })  
  
  # plot the network graph (in a second tab; better done as module)
  output$mygraph2 <- renderVisNetwork({
    myBBN <- getNet()
    myNodes  <- bnlearn::nodes(myBBN)  # nodes names
    nn       <- nnodes(myBBN) # number of nodes
    rootNodes    <- root.nodes(myBBN)
    nr           <- length(rootNodes)
    
    # Processing for vis plotting
    edges = as.data.frame(arcs(myBBN))
    edges$arrows = "to"
    nodes = data.frame(label=nodes(myBBN))
    nodes$id = 1:length(myNodes)
    edges$from <- nodes$id[match(edges$from,nodes$label)] # replace with id
    edges$to   <- nodes$id[match(edges$to,nodes$label)] # replace with id
    nodes$group = "Intermediate"
    nodes$group[match(rootNodes, nodes$label)]         <- "Root"
    nodes$group[match(leaf.nodes(myBBN), nodes$label)] <- "Leaf"
    nodes$shape = "ellipse"
    visNetwork(nodes, edges) %>% visLegend() %>%
      visIgraphLayout(randomSeed=10,layout=input$inLayout)
  })  
  

  
}

shinyApp(ui, server)

