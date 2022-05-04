intro_page <- tabItem(tabName = "introduction",   # NOTE: tabName must match the tabName in the sidebar
                      box(width=12, title=strong("Welcome to the Bayesian Network dashboard"), status="primary",solidHeader = TRUE,
                          p("Here you can open an existing discrete Bayesian Network model, 
                          display the network graphically in different ways as well as 
                          carry out probabilistic inference scenarios and show the results."),
                          p("The dashboard is a R shiny application and is based on the ", 
                            tags$a(href="https://www.bnlearn.com/","bnlearn"),"and ",
                            tags$a(href="https://www.rdocumentation.org/packages/gRain/versions/1.3-6","gRain"),
                            " packages. The design and features included in the app draw heavily from the excellent
                             and more sophisticated ShinyBN app developed by ", 
                            tags$a(href="https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-3309-0",
                                   "Chen et al. (2019).")," Here we were aiming for a simpler app that can be used to explore 
                            and evaluate existing discrete BN models (also see Motivation)."),
                          p("This work was funded by the Rural and Environment Science and Analytical Services (RESAS) Division of the Scottish Government.  ", 
                            tags$a(href='https://www.gov.scot/publications/environment-agriculture-and-food-strategic-research-main-research-providers/',tags$img(src='SG_Tertiary_CMYK.jpg',height=30)))),
                      br(),
                      box(title = strong("Brief guide"),width=12, collapsible = TRUE, collapsed = TRUE,solidHeader = FALSE,status="primary", #info, primary, warning,secondary, dark, danger, success
                          p("In the", strong("Open network"),"menu item you can open an existing discrete BN. Note, only discrete 
                          networks are currently supported. The app comes with some pre-loaded examples (Groundwater leaching BN, Asia BN and Stroke BN). 
                          If you want to open your own network model, it must be saved as an R object in (class bn or bnfit) 
                             or in one of the following network file formats: .net, .dot, .dsc, .bif"),
                          p("Once a network has been succesfully opened, it will be shown graphically. 
                            In the CPT tab, the Conditional Probability Tables (CPTs) for each node in the network can be displayed."), 
                          p("In the", strong("Inference"),"menu item you can carry out probabilistic inference scenarios. Start by selecting a target node 
                          (the node for which you want to calculate the marginal probability). Next choose a node for which you have evidence 
                           and select the state (evidence) for this node. By clicking the Add button, the evidence is added to an evidence table (shown below the bar plot)  
                           and the probabilities of all nodes in the network are updated. You can add evidence for multiple nodes to the evindence table. Use the Delete 
                            or the Clear buttons if you wish to remove the evidence again. The Bar plot tab shows the updated marginal probability distribution for the 
                            selected target node. The Graph view tab shows the full updated network as a barchart plot")),
                      box(title = strong("Motivation"),width=12, collapsible = TRUE, collapsed = TRUE, status="primary", #info, primary, warning, dark, danger, success
                          p("At the",tags$a(href="https://www.hutton.ac.uk/","James Hutton Institute"), 
                            "researchers have increasingly been developing and using Bayesian Networks (BNs) 
                          to model complex socio-environmental systems, due to BNs' ability to explicitly handle
                       uncertainties and to incorporate both qualitative and quantitative data.
                       Examples include probabilitic modelling of", 
                            tags$a(href="https://hess.copernicus.org/articles/26/1261/2022/","spatial pesticide pollution risk"),", ",
                            tags$a(href="https://www.crew.ac.uk/publication/developing-probabilistic-risk-model-estimate-phosphorus-nitrogen-and-microbial-pollution",
                                   "phosphorus, nitrogen and microbial pollution from septic tanks"),"and",
                            tags$a(href="https://www.sciencedirect.com/science/article/pii/S0167198713000925",
                                   "risk of soil threats.")),
                          p("The BN models are typically developed for and in close collaboration with a wide range of 
                      customers and clients using commericial and specialised software packages such as GeNie and Netica, 
                      which limit their use and uptake, and makes it difficult for the customers/users to explore and 
                      critically evaluate the developed BN models. This dashboard has therefore been developed to give 
                      users access to a given BN via a web browser and allow them to explore the model and carry out 
                      inference scenarios.")),
                      box(title = strong("What are Bayesian networks?"),width=12, collapsible = TRUE, collapsed = TRUE,solidHeader = FALSE,status="primary", #info, primary, warning,secondary, dark, danger, success
                          p("Bayesian Networks (BNs) are graphical models for representing multivariate 
                      probability distributions with system variables and their conditional 
                      relationships being represented as nodes and arcs in a directed acyclic 
                      graph (DAG). Variables that depend on other variables are called child nodes, 
                      while directly preceding nodes are referred to as parent nodes. Two variables 
                      are independent if they are not directly linked by an arc."),        
                          p("This app only considers discrete BNs. This means that each node in the BN 
                      is assigned a finite set of mutually exclusive state values and the conditional
                      probabilities between variables are represented by conditional probability tables 
                      (CPTs). These CPTs can be built based on empirical data, existing models or expert opinion."), 
                          p("A major advantage of a BN is that it allows inference based on (uncertain) 
                      evidence. Probabilistic inference is simply the task of calculating the posterior
                      probability distribution of the BN given the available observations and can be both 
                      predictive (i.e. reasoning from new observations of causes to new beliefs about the 
                      effects) and diagnostic (i.e. reasoning from observed effects to updated beliefs about causes)"),
                          br(),
                          p("For more information ", tags$a(href="https://en.wikipedia.org/wiki/Bayesian_network","click here")))
                      
) # end of intro_page