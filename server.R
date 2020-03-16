# server function
function(input, output, session) {
  
  selectedData <- reactive({
    study_status <- input$status
    tmp_data <- study_data[study_data$OverallStatus == study_status,]
    return(tmp_data)
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
      addProviderTiles(providers$OpenStreetMap)
      
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
                         fillOpacity = 1)%>%
        setView(lng = 33.85, lat = 20.45, zoom = 2)
    }
    
  })
  
  # network data
  selectedData1 <- reactive({
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
  
  # display the network
  output$condition_network <- renderForceNetwork({
    forceNetwork(Links = selectedData1()$edges, Nodes = selectedData1()$nodes,
                 Source = "from", Target = "to",
                 NodeID = "name",Group = "group",
                 Nodesize = 'num_studies',fontSize = 20,
                 opacity = 1,zoom = T,bounded=T,
                 linkDistance = 50,
                 linkWidth = JS("function(d) { return Math.sqrt(d.value);}")
    )
  })
  
  
}
  
