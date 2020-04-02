# packages 
library(httr)
library(jsonlite)
#library(XML)
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



# get coronavirus clinical trials
# from clinicaltrials.gov
base <- "http://clinicaltrials.gov/api/"
endpoint <- "query/study_fields/"
call <- paste0(base,endpoint)

# attributes info

fields <- c("NCTId",'Phase','StartDate','StartDateType','InterventionMeshTerm','CompletionDate',"BriefTitle","BriefSummary","Condition","LocationCity",'LocationState','LocationZip','LocationCountry','OverallStatus','LeadSponsorName','LeadSponsorClass')
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

strength_nodes <- as.data.frame(strength(g))
strength_nodes$name <- rownames(strength_nodes)
colnames(strength_nodes) <- c("num_studies",'name')
nodes_data <- left_join(nodes_data, strength_nodes,by = "name")

# scale node size
nodes_data$num_studies <- nodes_data$num_studies*2

# top study 
top_condition_name <- paste(strength_nodes[order(-strength_nodes$num_studies),][1:3,'name'],collapse = ', ')
top_condition_count <- paste(strength_nodes[order(-strength_nodes$num_studies),][1:3,'num_studies'],collapse=', ')

# years info
years <- unlist(lapply(study_data$StartDate, function(x) year(parse_date_time(x,orders=c('my','mdy')))))
num_years_before_2019 <- length(years[years<2019])
num_new_studies <- length(years) - num_years_before_2019
num_year_unknown <- nrow(study_data) - length(years)

print("Completed Script")