# packages 
library(httr)
library(jsonlite)
library(XML)
library(DT)
library(plyr)
library(dplyr)
library(shinythemes)
library(plotly)
library(leaflet)
library(lubridate)

library(data.table)

library(networkD3)
library(igraph)
library(lubridate)

library(rentrez) # to get pubmed data




# get coronavirus clinical trials
# from clinicaltrials.gov
base <- "http://clinicaltrials.gov/api/"
endpoint <- "query/study_fields/"
call <- paste0(base,endpoint)

# attributes info

fields <- c("NCTId",'Phase','StartDate','StartDateType','InterventionMeshTerm','CompletionDate',"BriefTitle","BriefSummary","Condition","LocationCity",'LocationState','LocationZip','LocationCountry','OverallStatus','LeadSponsorName','LeadSponsorClass','ReferenceCitation','ReferencePMID','ReferenceType')
num_attributes <- length(fields)

# expression
condition <- 'sars cov 2 OR covid 19 OR novel coronavirus'

#get_studies <- GET(call,query=list(expr=condition,fields=paste(fields,collapse = ','),fmt='JSON',max_rnk = 1000))

#get_studies_info <- fromJSON(content(get_studies,'text'))
#print(get_studies_info[[1]]$NStudiesFound)
#tmp_study_info <- get_studies_info[[1]]
#print(paste("Returned:",length(tmp_study_info[[10]])))
#l_of_studies <- lapply(tmp_study_info[[10]],clean_list)
#print(length(l_of_studies))
#data <- as.data.frame(bind_rows(l_of_studies),stringsAsFactors=F)
#print(nrow(data))

# calls the api to get the data
get_virus_data <- function(){
  get_studies <- GET(call,query=list(expr=condition,fields=paste(fields,collapse = ','),fmt='JSON',max_rnk = 1000))
  
  #get_studies_info <- xmlParse(content(get_studies, "text"))
  get_studies_info <- fromJSON(content(get_studies,'text'))
  tmp_study_info <- get_studies_info[[1]]$StudyFields
  
  data <- as.data.frame(tmp_study_info,stringsAsFactors = F)
  # working till here, bind_rows doesnt work.
  #data <- as.data.frame(bind_rows(l_of_studies),stringsAsFactors=F)
  #print(nrow(data))
  #data <- as.data.frame(data)
  # add nct id to this base link
  #study_url <- rep("<a href='https://clinicaltrials.gov/ct2/show/record/",nrow(data))
  #end_url <- rep("' target='_blank'>",nrow(data))
  #final_part <- rep("'</a>",nrow(data))
  #print(head(data,2))
  
  # testing nct id
  # add nct id to this base link
  study_url <- 'https://clinicaltrials.gov/ct2/show/record/'
  data$NCTId <- paste0("<a href='",study_url, data$NCTId,"' target='_blank'>", data$NCTId,"</a>")
  
  #### SHINY DOESNT LIKE
  #data$NCTId <- paste(study_url, data$NCTId,end_url, data$NCTId,final_part,sep='')
  
  return(data)
}

study_data <- get_virus_data()

# global variables used in UI
study_choices <- unique(study_data$OverallStatus)
names(study_choices) <- study_choices

# global variables
num_records <- nrow(study_data)
num_recruiting <- nrow(study_data[study_data$OverallStatus == 'Recruiting',])
num_completed <- nrow(study_data[study_data$OverallStatus == 'Completed',])

# get the latest study
get_latest_study <- function(){
  tmp_study_data <- study_data[study_data$StartDateType=='Actual',]
  tmp_data <- data.frame(unlist(tmp_study_data$StartDate),stringsAsFactors = F)
  colnames(tmp_data) <- "StartDate"
  tmp_data$StartDate <- as.Date(tmp_data$StartDate,format = "%B %d, %Y")
  tmp_data$diff <- Sys.Date() - tmp_data$StartDate
  #tmp_data$diff <- as.Date('March 5, 2020',format='%B %d, %Y') - tmp_data$StartDate
  tmp_data$diff[is.na(tmp_data$diff)] <- 1000
  
  latest_study <- tmp_study_data[tmp_data$diff == min(tmp_data$diff),]
  return(latest_study)
}

latest_clinical_study <- get_latest_study()
# make sure that you get only one study
latest_clinical_study <- latest_clinical_study[1,]

# gets the list of cities for every study
get_city_data <- function(filter){
  tmp_city_data <- list()
  tmp_study_data <- study_data
  if(filter == 'outbreak'){
    tmp_study_data$year <- unlist(lapply(tmp_study_data$StartDate, function(x) if(length(x)>0) year(parse_date_time(x,orders=c('my','mdy'))) else 0))
    tmp_study_data <- tmp_study_data[tmp_study_data$year>2018,]
  }else if(filter == 'recruiting'){
    tmp_study_data$OverallStatus <- unlist(tmp_study_data$OverallStatus)
    tmp_study_data <- tmp_study_data[tmp_study_data$OverallStatus == 'Recruiting',]
  }else if(filter == 'both'){
    tmp_study_data$year <- unlist(lapply(tmp_study_data$StartDate, function(x) if(length(x)>0) year(parse_date_time(x,orders=c('my','mdy'))) else 0))
    tmp_study_data <- tmp_study_data[tmp_study_data$year>2018,]
    tmp_study_data$OverallStatus <- unlist(tmp_study_data$OverallStatus)
    tmp_study_data <- tmp_study_data[tmp_study_data$OverallStatus == 'Recruiting',]
  }
  
  for(i in tmp_study_data$LocationCity){
    partcipating_cities <- unique(i)
    if(length(partcipating_cities) !=0){
      for(j in partcipating_cities){
        if(!exists(j,where=tmp_city_data)){
          tmp_city_data[[j]] = 0
        }
        tmp_city_data[[j]] = tmp_city_data[[j]]+1
      }
    }
  }
  tmp_city_data <- data.frame(unlist(tmp_city_data),stringsAsFactors = FALSE)
  tmp_city_data$City <- rownames(tmp_city_data)
  colnames(tmp_city_data) <- c("Num","City")
  tmp_city_data <- arrange(tmp_city_data,Num)
  return(tmp_city_data)
}

# all studies
city_data <- get_city_data('neither')
# outbreak
outbreak_city_data <- get_city_data('outbreak')
# recruiting
recruiting_city_data <- get_city_data('recruiting')
# both filter
both_filter_city_data <- get_city_data('both')

# returns the number of studies by every country
get_country_data <- function(){
  tmp_country_data <- list()
  for(i in study_data$LocationCountry){
    partcipating_countries <- unique(i)
    if(length(partcipating_countries) !=0){
      for(j in partcipating_countries){
        if(!exists(j,where=tmp_country_data)){
          tmp_country_data[[j]] = 0
        }
        tmp_country_data[[j]] = tmp_country_data[[j]]+1
      }
    }
  }
  tmp_country_data <- data.frame(unlist(tmp_country_data),stringsAsFactors = FALSE)
  tmp_country_data$Country <- rownames(tmp_country_data)
  colnames(tmp_country_data) <- c("Num","Country")
  tmp_country_data <- arrange(tmp_country_data,Num)
  return(tmp_country_data)
}

country_data <- get_country_data()

# funder info
# returns the number of studies by every country
get_funder_data <- function(){
  tmp_funder_data <- list()
  for(i in study_data$LeadSponsorName){
    sponsors <- unique(i)
    if(length(sponsors) !=0){
      for(j in sponsors){
        if(!exists(j,where=tmp_funder_data)){
          tmp_funder_data[[j]] = 0
        }
        tmp_funder_data[[j]] = tmp_funder_data[[j]]+1
      }
    }
  }
  tmp_funder_data <- data.frame(unlist(tmp_funder_data),stringsAsFactors = FALSE)
  tmp_funder_data$Funder <- rownames(tmp_funder_data)
  colnames(tmp_funder_data) <- c("Num","Funder")
  tmp_funder_data <- arrange(tmp_funder_data,Num)
  return(tmp_funder_data)
}

funder_data <- get_funder_data()

# funder info
# returns the number of studies by type of funder
get_funder_type_data <- function(){
  tmp_funder_data <- list()
  for(i in study_data$LeadSponsorClass){
    sponsor_type <- unique(i)
    if(length(sponsor_type) !=0){
      for(j in sponsor_type){
        if(!exists(j,where=tmp_funder_data)){
          tmp_funder_data[[j]] = 0
        }
        tmp_funder_data[[j]] = tmp_funder_data[[j]]+1
      }
    }
  }
  tmp_funder_data <- data.frame(unlist(tmp_funder_data),stringsAsFactors = FALSE)
  tmp_funder_data$FunderType <- rownames(tmp_funder_data)
  colnames(tmp_funder_data) <- c("Num","FunderType")
  tmp_funder_data <- arrange(tmp_funder_data,Num)
  return(tmp_funder_data)
}

funder_type_data <- get_funder_type_data()


## MAP DATA
cities<- read.csv('worldcities.csv')
cities <- cities[!duplicated(cities$city),]

# filter lat-lon
city_map_data <- merge(city_data, cities,by.x='City',by.y='city')[,c('City','Num','lat','lng')]
outbreak_city_data <- merge(outbreak_city_data, cities,by.x='City',by.y='city')[,c('City','Num','lat','lng')]
recruiting_city_data <- merge(recruiting_city_data, cities,by.x='City',by.y='city')[,c('City','Num','lat','lng')]
both_filter_city_data <- merge(both_filter_city_data, cities,by.x='City',by.y='city')[,c('City','Num','lat','lng')]

#popup text
city_map_data$Text <- paste0(city_map_data$City,"</br>Num Studies:",city_map_data$Num)
outbreak_city_data$Text <- paste0(outbreak_city_data$City,"</br>Num Studies:",outbreak_city_data$Num)
recruiting_city_data$Text <- paste0(recruiting_city_data$City,"</br>Num Studies:",recruiting_city_data$Num)
both_filter_city_data$Text <- paste0(both_filter_city_data$City,"</br>Num Studies:",both_filter_city_data$Num)

# color
outbreak_city_data$color <- "Red"
recruiting_city_data$color <- "Green"
both_filter_city_data$color <- "Purple"

# create intervention network

# num papers with interventions
num_papers_with_intervention <- 0
for(i in seq(1,nrow(study_data))){
  if(length(study_data$InterventionMeshTerm[i][[1]]) > 0){
    num_papers_with_intervention <- num_papers_with_intervention + 1
  }
}

# given a vector of values, creates the from-to dataframe
create_network<- function(x){
  v_1 <- c()
  v_2 <- c()
  if(length(x)>1){
    for(i in seq(1,length(x)-1)){
      for(j in seq(i+1,length(x))){
        v_1 <- c(v_1,x[i])
        v_2 <- c(v_2, x[j])
      }
    }
    return(data.frame(from = v_1, to = v_2,stringsAsFactors = F))
  }
  return(data.frame(from = x, to = x,stringsAsFactors = F))
}

t <- lapply(study_data$InterventionMeshTerm, create_network)


#print(t[[1]])
#print(t[[20]])
#print("First list above")
test_df <- as.data.frame(bind_rows(t),stringsAsFactors = F)

g <- graph.data.frame(test_df,directed=F)
print("Created graph")
print("is weighted?")
print(is_weighted(g))
#test_df <- as.data.frame(count(bind_rows(t)))
#print(head(test_df,1))
#print(colnames(test_df))

#test_df <- count(test_df)
#print("Completed")
#print(head(test_df,1))
#print(typeof(test_df))

#colnames(test_df) <- c("from",'to','weight')
#print("Created df")

#g <- graph_from_data_frame(test_df,directed=F)

edges_data <- get.data.frame(g,what = 'edges')
nodes_data <- get.data.frame(g,what = 'vertices')
print("got edges")

# community 
c1 = cluster_infomap(g)
print("got cluster")
mem_nodes <- membership(c1)
nodes_data$group <- as.vector(mem_nodes)
nodes_data$id <- seq(0,nrow(nodes_data)-1)

print("Nodes data columns:")
print(colnames(nodes_data)) # name, group, id
print("Edges data columns:")
print(colnames(edges_data)) # from, to
edges_data <- left_join(edges_data,nodes_data,by = c('from'='name'))
edges_data <- left_join(edges_data,nodes_data,by = c('to'='name'))
print(head(edges_data,1))
edges_data <- edges_data[,c('id.x','id.y')]
colnames(edges_data) <- c("from",'to')

#edges_data <- edges_data[,c('id.x','id.y','weight')]
#colnames(edges_data) <- c("from",'to','weight')
print("completed setting edges")


# top interventions
total_intervention <- unlist(study_data$InterventionMeshTerm)
total_intervention <- plyr::count(total_intervention)
top_intervention_name <- paste(total_intervention[order(-total_intervention$freq),][1:3,'x'],collapse = ', ')
top_intervention_count <- paste(total_intervention[order(-total_intervention$freq),][1:3,'freq'],collapse=', ')

colnames(total_intervention) <- c("name","num_studies")
nodes_data <- left_join(nodes_data, total_intervention,by = "name")
# scale node size
nodes_data$num_studies <- nodes_data$num_studies*2

# years info
years <- unlist(lapply(study_data$StartDate, function(x) year(parse_date_time(x,orders=c('my','mdy')))))
num_years_before_2019 <- length(years[years<2019])
num_new_studies <- length(years) - num_years_before_2019
num_year_unknown <- nrow(study_data) - length(years)

####
# Literature Meta-data
####

# get the papers of the clinical trials
get_papers <- function(){
  study_id <- c()
  reference_citation <- c()
  reference_pmid <- c()
  reference_type <- c()
  for(i in seq(1,nrow(study_data))){
    num_papers <- length(study_data[i,"ReferenceCitation"][[1]])
    # if it has papers
    if(num_papers !=0){
      for(tmp_idx in seq(1,num_papers)){
        study_id <- c(study_id, study_data[i,"NCTId"])
        reference_citation <- c(reference_citation,study_data[i,"ReferenceCitation"][[1]][tmp_idx])
        reference_pmid <- c(reference_pmid,study_data[i,"ReferencePMID"][[1]][tmp_idx])
        reference_type <- c(reference_type,study_data[i,"ReferenceType"][[1]][tmp_idx])
      }
    }
  }
  return(data.frame('NCTId'=study_id, 'ReferenceCitation' = reference_citation,'ReferencePMID'=reference_pmid,'ReferenceType'=reference_type))
}

clinical_trial_papers <- get_papers()

# get the intervention name for every trial
get_intervention_clinical_trials <- function(){
  study_id <- c()
  intervention_name <- c()
  for(i in seq(1,nrow(study_data))){
    interventions <- unique(study_data[i,"InterventionMeshTerm"][[1]])
    # for each intervention
    for(tmp_intervention in interventions){
      study_id <- c(study_id, study_data[i,'NCTId'])
      intervention_name <- c(intervention_name,tmp_intervention)
    }
  }
  return(data.frame('NCTId'=study_id, 'InterventionMeshTerm' = intervention_name))
}

intervention_id <- get_intervention_clinical_trials()
intervention_id <- intervention_id[intervention_id$NCTId %in% clinical_trial_papers$NCTId,]

clinical_trial_papers <- left_join(clinical_trial_papers,intervention_id,by='NCTId')
clinical_trial_papers$ReferenceCitation <- as.vector(clinical_trial_papers$ReferenceCitation)
clinical_trial_papers$InterventionMeshTerm <- as.vector(clinical_trial_papers$InterventionMeshTerm)
clinical_trial_papers$ReferencePMID <- as.vector(clinical_trial_papers$ReferencePMID)

# used in UI
total_clinical_trial_papers <- length(unique(clinical_trial_papers$ReferenceCitation))

unique_interventions <- unique(clinical_trial_papers$InterventionMeshTerm)
unique_interventions <- unique_interventions[!is.na(unique_interventions)]

pubmed_url_base <- "<a href='https://www.ncbi.nlm.nih.gov/pubmed/"

# from the perspective of papers: makes it easy to filter
paper_citation <- clinical_trial_papers%>%group_by(ReferenceCitation)%>%
  summarise(InterventionMeshTerm = paste(unique(InterventionMeshTerm[!is.na(InterventionMeshTerm)]),collapse=";"),
            NCTId = paste(unique(NCTId[!is.na(NCTId)]),collapse="; "),
            ReferencePMID = paste(pubmed_url_base,unique(ReferencePMID[!is.na(ReferencePMID)]),"' target='_blank'>",unique(ReferencePMID[!is.na(ReferencePMID)]),"</a>",collapse=''),
            Outcome = paste(unique(ReferenceType),collapse=''))
paper_citation <- as.data.frame(paper_citation)


##### co-authorship through pubmed


unique_ids <- clinical_trial_papers$ReferencePMID[!is.na(clinical_trial_papers$ReferencePMID)]
unique_ids <- unique(unique_ids)

# get the pubmed data for the pubmed id's
tmp <- entrez_fetch(db='pubmed',id = unique_ids,rettype = 'xml',parsed = T)

pubmed_data <- xmlToList(tmp)

# store the meta data 
pubmed_metadata <- data.frame(stringsAsFactors = F)

# loop through authors
for(i in seq(1,length(pubmed_data))){
  authors <- pubmed_data[[i]]$MedlineCitation$Article$AuthorList
  pmid <- pubmed_data[[i]]$MedlineCitation$PMID$text
  tmp_authors <- c()
  
  # loop through authors
  for(tmp_idx in seq(1,length(authors)-1)){
    tmp_authors <- c(tmp_authors,paste(authors[[tmp_idx]]$ForeName,authors[[tmp_idx]]$LastName))
  }
  
  # if we found authors
  if(length(tmp_authors)>0){
    pubmed_metadata <- rbind(pubmed_metadata, data.frame('ReferencePMID'=pmid,'authors'=tmp_authors,stringsAsFactors = F))
  }
}

# get the list of top authors
top_authors <- plyr::count(pubmed_metadata$authors)
top_author_name <- paste(top_authors[order(-top_authors$freq),][1:3,'x'],collapse = ', ')
top_author_count <- paste(top_authors[order(-top_authors$freq),][1:3,'freq'],collapse=', ')

unique_authors <- unique(pubmed_metadata$authors)
num_authors <- length(unique_authors)
num_papers_with_authors <- length(unique(pubmed_metadata$ReferencePMID))

# use it to filter based on intervention, else use pubmed_metadata
pubmed_metadata_intervention <- left_join(pubmed_metadata,clinical_trial_papers,by='ReferencePMID')
pubmed_metadata_intervention <- pubmed_metadata_intervention[,c("ReferencePMID",'authors','InterventionMeshTerm')]


# given the list of papers, build the coauthorship graph
build_coauthor_graph <- function(vector_of_papers,author_name=''){
  coauthor_data <- data.frame()
  for(paper in vector_of_papers){
    unique_authors <- unique(pubmed_metadata[pubmed_metadata$ReferencePMID == paper,'authors'])
    coauthor_data <- rbind(coauthor_data,create_network(unique_authors))
  }
  
  #filter based on author
  if(author_name != ''){
    coauthor_data <- coauthor_data[(coauthor_data$from == author_name) | (coauthor_data$to == author_name),]
    print(head(coauthor_data))
  }
  coauthor_g <- graph.data.frame(coauthor_data,directed=F)
  
  coauthor_edges_data <- get.data.frame(coauthor_g,what = 'edges')
  coauthor_nodes_data <- get.data.frame(coauthor_g,what = 'vertices')
  
  # community 
  c1 = cluster_infomap(coauthor_g)
  print("got cluster")
  mem_nodes <- membership(c1)
  coauthor_nodes_data$group <- as.vector(mem_nodes)
  coauthor_nodes_data$id <- seq(0,nrow(coauthor_nodes_data)-1)
  
  print("Nodes data columns:")
  print(colnames(nodes_data)) # name, group, id
  print("Edges data columns:")
  print(colnames(edges_data)) # from, to
  coauthor_edges_data <- left_join(coauthor_edges_data,coauthor_nodes_data,by = c('from'='name'))
  coauthor_edges_data <- left_join(coauthor_edges_data,coauthor_nodes_data,by = c('to'='name'))
  print(head(coauthor_edges_data,1))
  coauthor_edges_data <- coauthor_edges_data[,c('id.x','id.y')]
  colnames(coauthor_edges_data) <- c("from",'to')
  
  #edges_data <- edges_data[,c('id.x','id.y','weight')]
  #colnames(edges_data) <- c("from",'to','weight')
  print("completed setting edges")
  
  coauthor_degree_nodes <- as.data.frame(degree(coauthor_g))
  coauthor_degree_nodes$name <- rownames(coauthor_degree_nodes)
  colnames(coauthor_degree_nodes) <- c("degree",'name')
  
  #join to the nodes data
  coauthor_nodes_data <- left_join(coauthor_nodes_data, coauthor_degree_nodes,by = "name")
  
  return(list('nodes'=coauthor_nodes_data,'edges'=coauthor_edges_data))
}

print("Completed Script")