library(XML)
setwd("/Users/haosicheng/Desktop/BIDMC/search_result")
name.list<-list.files()
  
#compare.id<-"NCT00988208.xml"
#compare.list<-xmlToList(compare.id)
#compare<-unlist(compare.list)
#len<-100 #test with a small set of data
len<-length(name.list)

#Columns
nct_id<-rep(0,len)
trial_title<-rep(0,len)
Conditions<-rep(0,len)
Interventions<i-rep(0,len)
intervention_type<-rep(0,len)
Sponsor_Collaborators<-rep(0,len) #Q:Is Collaborators necessary?
Gender<-rep(0,len)
Age_Groups<-rep(0,len)
Enrollment<-rep(0,len)
Start_Date<-rep(0,len)
Completion_Date<-rep(0,len)#primary
title<-rep(0,len)#it is called primary outcome 
z<-rep(0,len)
direction<-rep(0,len)

#June 28 data
sponsor_class<-rep(0,len)

for(i in 1:len){
  if(!(i%%100)){
    print(i)
  }
  list<-xmlToList(name.list[i])
  
  # if(!is.null(list$id_info$nct_id)){
  #   nct_id[i]<-list$id_info$nct_id
  # }
  # 
  # if(!is.null(list$brief_title)){
  #   trial_title[i]<-list$brief_title
  # }
  # 
  # if(!is.null(list$condition)){
  #   Conditions[i]<-list$condition
  # }
  # 
  # if(!is.null(list$intervention$intervention_name)){
  #   Interventions[i]<-list$intervention$intervention_name
  # }
  # 
  # if(!is.null(list$intervention$intervention_type)){
  #   intervention_type[i]<-list$intervention$intervention_type
  # }
  # 
  # if(!is.null(list$sponsors$lead_sponsor$agency)){
  #   Sponsor_Collaborators[i]<-list$sponsors$lead_sponsor$agency
  # }
  # 
  # if(!is.null(list$eligibility$gender)){
  #   Gender[i]<-list$eligibility$gender
  # }
  # #Age_Groups
  # if(!is.null(list$enrollment)){
  #   if(typeof(list$enrollment)=="character"){
  #     Enrollment[i]<-list$enrollment
  #   } else{
  #     Enrollment[i]<-list$enrollment$text
  #   }
  #  
  # }
  # if(!is.null(list$start_date)){
  #   Start_Date[i]<-list$start_date
  # }
  # 
  # if(!is.null(list$primary_completion_date)){
  #   if(typeof(list$primary_completion_date)=="character"){
  #     Enrollment[i]<-list$primary_completion_date
  #   } else{
  #     Enrollment[i]<-list$primary_completion_date$text
  #   }
  #   
  # }
  # if(!is.null(list$primary_outcome$measure)){
  #   title[i]<-list$primary_outcome$measure
  # }
  if(typeof(list$sponsors$sponsor$lead_sponsor$agency_class)=="character"){
      sponsor_class[i]<-list$sponsor$lead_sponsor$agency_class
  }
  
  #z
  #direction
}

