# Simple R shiny BN app
This is a simple R shiny app for opening a discrete Bayesian Network (BN) model file, display the network graphically in different ways as well as carry out probabilistic inference scenarios and show the results.  

The app can be launched from [HERE](https://qqpvju-madstroldborg.shinyapps.io/BBN_shiny_app_fin/).  

The dashboard is a R shiny application and is based on the bnlearn and gRain packages. The design and features included in the app draw heavily from the excellent and more sophisticated ShinyBN app developed by Chen et al. (2019). Here we were aiming for a simpler app that can be used to explore and evaluate existing discrete BN models.

## How to use?
In the Open network menu item you can open an existing discrete BN. Note, only discrete networks are currently supported. The app comes with some pre-loaded examples (Groundwater leaching BN, Asia BN and Stroke BN). If you want to open your own network model, it must be saved as an R object in (class bn or bnfit) or in one of the following network file formats: .net, .dot, .dsc, .bif

Once a network has been succesfully opened, it will be shown graphically. In the CPT tab, the Conditional Probability Tables (CPTs) for each node in the network can be displayed.

In the Inference menu item you can carry out probabilistic inference scenarios. Start by selecting a target node (the node for which you want to calculate the marginal probability). Next choose a node for which you have evidence and select the state (evidence) for this node. By clicking the Add button, the evidence is added to an evidence table (shown below the bar plot) and the probabilities of all nodes in the network are updated. You can add evidence for multiple nodes to the evindence table. Use the Delete or the Clear buttons if you wish to remove the evidence again. The Bar plot tab shows the updated marginal probability distribution for the selected target node. The Graph view tab shows the full updated network as a barchart plot.

More to follow...
