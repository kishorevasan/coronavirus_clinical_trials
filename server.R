# server function
function(input, output, session) {
  
  selectedData <- reactive({
    study_status <- input$status
    filtered_study_data <- study_data[study_data$OverallStatus == study_status,]
    return(filtered_study_data)
  })
  
  interventionData <- reactive({
    drug_name <- input$egonode
    # get interventions data
    logic_res <- rep(F, nrow(study_data))
    for (row in 1:nrow(study_data)) {
      interventions <- unlist(study_data[row,'InterventionMeshTerm'])
      if(drug_name %in% interventions){
        logic_res[row] <- T
      }
    }
    tmp_studies<- paste(study_data[logic_res,'NCTId'],collapse = '; ')
    return(tmp_studies)
  })
  
  output$drug_studies <- renderText({
    interventionData()
  })
  
  result_papers <- reactive({
    if(input$filterpaper){
      subset_papers <- clinical_trial_papers[clinical_trial_papers$InterventionMeshTerm==input$paperintervention,'ReferenceCitation']
      tmp_papers <- paper_citation[paper_citation$ReferenceCitation %in% subset_papers,]
    }else{
      tmp_papers <- paper_citation
    }
    results_papers <- tmp_papers[tmp_papers$Outcome=='result',]
    num_results <- nrow(results_papers)
    results_base_text <- paste0("<b>Results Papers</b><t> Num: ",num_results,"<br/>")
    
    # curate papers
    results_papers_text <- paste(paste0("<em>Paper: </em>",results_papers$ReferenceCitation,'<br/>'),paste0("PubMed ID:",results_papers$ReferencePMID,'<br/>'), paste0("Trials:",results_papers$NCTId,'<br/>'),paste0("Interventions:",results_papers$InterventionMeshTerm,'<br/>'),collapse="<br/>")
    complete_results_text <- paste(results_base_text,results_papers_text)
    return(complete_results_text)    
  })
  
  derived_papers <- reactive({
    if(input$filterpaper){
      subset_papers <- clinical_trial_papers[clinical_trial_papers$InterventionMeshTerm==input$paperintervention,'ReferenceCitation']
      tmp_papers <- paper_citation[paper_citation$ReferenceCitation %in% subset_papers,]
    }else{
      tmp_papers <- paper_citation
    }
    derived_papers <- tmp_papers[tmp_papers$Outcome == 'derived',]
    num_derived <- nrow(derived_papers)    
    derived_base_text <- paste0("<b>Derived Papers</b><t> Num: ",num_derived,"<br/>")
    
    # curate papers
    derived_papers_text <- paste(paste0("<em>Paper: </em>",derived_papers$ReferenceCitation,'<br/>'),paste0("PubMed ID: ",derived_papers$ReferencePMID,'<br/>'), paste0("Trials: ",derived_papers$NCTId,'<br/>'),paste0("Interventions: ",derived_papers$InterventionMeshTerm,'<br/>'),collapse="<br/>")
    complete_derived_text <- paste(derived_base_text,derived_papers_text)
    return(complete_derived_text)
  })  
  
  background_papers <- reactive({
    if(input$filterpaper){
      subset_papers <- clinical_trial_papers[clinical_trial_papers$InterventionMeshTerm==input$paperintervention,'ReferenceCitation']
      tmp_papers <- paper_citation[paper_citation$ReferenceCitation %in% subset_papers,]
    }else{
      tmp_papers <- paper_citation
    }
    background_papers <- tmp_papers[tmp_papers$Outcome == 'background',]
    num_background <- nrow(background_papers)
    background_base_text <- paste0("<b>Background Papers</b><t> Num:",num_background,"<br/>")
    
    # curate papers
    background_papers_text <- paste(paste0("<em>Paper: </em>",background_papers$ReferenceCitation,'<br/>'),paste0("PubMed ID: ",background_papers$ReferencePMID,'<br/>'), paste0("Trials: ",background_papers$NCTId,'<br/>'),paste0("Interventions: ",background_papers$InterventionMeshTerm,'<br/>'),collapse="<br/>")
    complete_background_text <- paste(background_base_text,background_papers_text)
    return(complete_background_text)
  })
  
  output$resultPapers <- renderUI({
    HTML(result_papers())
  })
  
  output$derivedPapers <- renderUI({
    HTML(derived_papers())
  })
  
  output$backgroundPapers <- renderUI({
    HTML(background_papers())
  })
  
  output$coronavirusData <- renderDataTable(
    selectedData(), escape=FALSE,options = list(scrollX=TRUE,scrollY=TRUE)
  )
  
  output$cityplot <- renderPlotly(
    p<-plot_ly(data = city_data,x = ~Num, y = ~City,type='bar',orientation='h')%>%
      layout(yaxis = list(categoryarray = ~City , categoryorder = "array"))
  )
  
  output$countryplot <- renderPlotly(
    p<-plot_ly(data = country_data,x = ~Num, y = ~Country,type='bar',orientation='h')%>%
      layout(yaxis = list(categoryarray = ~Country , categoryorder = "array"))
  )
  
  output$funderplot <- renderPlotly(
    p<-plot_ly(data = funder_data,x = ~Num, y = ~Funder,type='bar',orientation='h')%>%
      layout(yaxis = list(categoryarray = ~Funder , categoryorder = "array"),
             margin = list(l = 250))
  )
  
  output$funderTypePlot <- renderPlotly(
    plot_ly(funder_type_data, labels = ~FunderType, values = ~Num, type = 'pie')%>%
      layout(title = 'Number of Studies Funded by Organization Type',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  output$map <- renderLeaflet(
    m <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap)%>%
      setView(lng = 33.85, lat = 20.45, zoom = 2)
    
  )
  
  observe({
    if(input$outbreak & input$recruiting){
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(lat = both_filter_city_data$lat, 
                         lng = both_filter_city_data$lng,
                         label = as.character(both_filter_city_data$City),
                         popup = as.character(both_filter_city_data$Text),
                         radius = both_filter_city_data$Num,
                         color = both_filter_city_data$color,
                         fillOpacity = 1)
    }else if(input$outbreak){
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(lat = outbreak_city_data$lat, 
                         lng = outbreak_city_data$lng,
                         label = as.character(outbreak_city_data$City),
                         popup = as.character(outbreak_city_data$Text),
                         radius = outbreak_city_data$Num,
                         color = outbreak_city_data$color,
                         fillOpacity = 1)
    }else if(input$recruiting){
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(lat = recruiting_city_data$lat, 
                         lng = recruiting_city_data$lng,
                         label = as.character(recruiting_city_data$City),
                         popup = as.character(recruiting_city_data$Text),
                         radius = recruiting_city_data$Num,
                         color = recruiting_city_data$color,
                         fillOpacity = 1)
    }else{
      leafletProxy('map')%>%
        clearMarkers()%>%
        addCircleMarkers(lat = city_map_data$lat, 
                         lng = city_map_data$lng,
                         label = as.character(city_map_data$City),
                         popup = as.character(city_map_data$Text),
                         radius = city_map_data$Num,
                         fillOpacity = 1)
    }
  })
  
  # network data
  interventionNetworkData <- reactive({
    # if need to viz ego net
    if(input$egonet){
      ego_id <- nodes_data[nodes_data$name==input$egonode,'id']
      tmp_edges <- edges_data[edges_data$from == ego_id | edges_data$to == ego_id,]
      filter_nodes <- c(tmp_edges$from, tmp_edges$to)
      tmp_nodes <- nodes_data%>%filter(id %in% filter_nodes)
      
      tmp_edges <- left_join(tmp_edges,tmp_nodes,by = c('from'='id'))
      tmp_edges <- left_join(tmp_edges,tmp_nodes,by = c('to'='id'))
      tmp_edges <- tmp_edges[,c('name.x','name.y')]
      colnames(tmp_edges) <- c('from','to')
      
      # create node ids for this subgraph
      tmp_nodes$id <- seq(0,nrow(tmp_nodes)-1)
      
      # join the ids back
      tmp_edges <- left_join(tmp_edges,tmp_nodes,by = c('to'='name'))
      tmp_edges <- left_join(tmp_edges,tmp_nodes, by = c("from"="name"))
      tmp_edges <- tmp_edges[,c('id.x','id.y')]
      colnames(tmp_edges) <- c("from",'to')
      
      # return
      return(list(nodes = tmp_nodes,edges = tmp_edges))
    }else{
      return(list(nodes=nodes_data,edges=edges_data))
    }
  })
  
  # display the intervention network
  output$intervention_network <- renderForceNetwork({
    forceNetwork(Links = interventionNetworkData()$edges, Nodes = interventionNetworkData()$nodes,
                 Source = "from", Target = "to",
                 NodeID = "name",Group = "group",
                 Nodesize = 'num_studies',fontSize = 20,
                 opacity = 1,zoom = T,bounded=T,
                 linkDistance = 50,
                 linkWidth = JS("function(d) { return Math.sqrt(d.value);}")
    )
  })
  
  coauthorshipNetworkData <- reactive({
    filter_intervention <- input$paperintervention
    filter_author <- input$paperauthor
    
    if(input$networkfilter == 'author'){
      subset_papers <- pubmed_metadata[pubmed_metadata$authors == filter_author,'ReferencePMID']
      net <- build_coauthor_graph(subset_papers,filter_author)
    }else{
      subset_papers <- pubmed_metadata_intervention[pubmed_metadata_intervention$InterventionMeshTerm == filter_intervention,'ReferencePMID']
      # quality check
      subset_papers <- unique(subset_papers)
      subset_papers <- subset_papers[!is.na(subset_papers)]
      
      net <- build_coauthor_graph(subset_papers)
    }
    
    
    # get the list of nodes and edges
    return(net)
  })
  
  # display the network
  output$coauthor_network <- renderForceNetwork({
    forceNetwork(Links = coauthorshipNetworkData()$edges, Nodes = coauthorshipNetworkData()$nodes,
                 Source = "from", Target = "to",
                 NodeID = "name",Group = "group",
                 Nodesize = 'degree',fontSize = 20,
                 opacity = 1,zoom = T,bounded=T,
                 linkDistance = 50,
                 linkWidth = JS("function(d) { return Math.sqrt(d.value);}")
    )
  })
}

