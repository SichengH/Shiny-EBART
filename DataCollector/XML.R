library(XML)
#search a folder with all the .xml file, make a data frame. Only need to modify year(line 6) and directory(line 3)
setwd("/Users/haosicheng/Desktop/BIDMC/search_result")#set directory to where all the .xml files at
name.list<-list.files()
len<-length(name.list)
year<-c(2008,2009,2010,2011,2012)#list of years 
#selection info
p<-0
p.desc<-0
#count
dmc<-0
oversight.c<-0
drug<-0
device<-0


dataf<-NULL
c<-1#count of outcome analysis
for(i in 1:len){
  if(!(i%%1000)){
    print(i)#print process by 1000
  }
  nct_id<-0
  trial_title<-0
  Conditions<-0
  condition<-0
  Interventions<-0
  intervention_type<-0
  Sponsor_Collaborators<-0 #Q:Is Collaborators necessary?
  sponsor_class<-0
  Gender<-0
  Age_Groups<-0
  Enrollment<-0
  Start_Date<-0
  fda.drug<-0
  fda.device<-0
  Completion_Date<-0#primary
  title<-0#it is called primary outcome 
  p<-0
  p.desc<-0
  method<-0
  param.value<-0
  param.type<-0
  ci<-0
  ci.side<-0
  ci.l<-0
  ci.u<-0
  list<-xmlToList(name.list[i])
  if(!is.null(list$oversight_info)){
    if(length(list$oversight_info)>3){
      oversight.c<-oversight.c+1
    }
    if(!is.null(list$oversight_info$is_fda_regulated_drug)){
      if(list$oversight_info$is_fda_regulated_drug=="Yes"){
        drug<-drug+1
        fda.drug<-1
      }
    }
    if(!is.null(list$oversight_info$is_fda_regulated_device)){
      if(list$oversight_info$is_fda_regulated_device=="Yes"){
        device<-device+1
        fda.device<-1
      }
    }
  } else {
    next
  }
   if(!is.null(list$primary_completion_date)){
     if(typeof(list$primary_completion_date)=="character"){
       Completion_Date = list$primary_completion_date
     } else {
       Completion_Date = list$primary_completion_date$text
     }
     l<-unlist(strsplit(Completion_Date,' '))
     if(length(l)==2){
       y<-as.numeric(l[2])
     } 
     if(length(l)==3){
       y<-as.numeric(l[3])
     }
     if(!sum(y==year)){
       next
     }
     
     #search prime completion data
   } else {
     next
   }
  
  if(!is.null(list$sponsors)){
    if(!is.null(list$sponsor$lead_sponsor$agency_class)){
      sponsor_class<-list$sponsor$lead_sponsor$agency_class#need to know the format
    }
    if(!sponsor_class=="Industry"){
      next
    }
    if(!is.null(list$sponsors$lead_sponsor$agency)){
      Sponsor_Collaborators<-list$sponsors$lead_sponsor$agency
    }
    #only keep industry
  } else {
    next
  }
  

  
  if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis)){
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$p_value)){
      p<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$p_value
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$p_value_desc)){
      p.desc<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$p_value_desc
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$method)){
      method<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$method
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$param_value)){
      param.value<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$param_value
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$param_type)){
      param.type<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$param_type
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_percent)){
      ci<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_percent
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_n_sides)){
      ci.side<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_n_sides
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_lower_limit)){
      ci.l<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_lower_limit
    }
    if(!is.null(list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_upper_limit)){
      ci.u<-list$clinical_results$outcome_list$outcome$analysis_list$analysis$ci_upper_limit
    }
  }
  
    if(!is.null(list$id_info$nct_id)){
      nct_id<-list$id_info$nct_id
    } else {
      next
    }
    
    if(!is.null(list$brief_title)){
      trial_title<-list$brief_title
    }
    
    if(!is.null(list$condition)){
      Conditions<-list$condition
    }
    
    if(!is.null(list$intervention$intervention_name)){
      Interventions<-list$intervention$intervention_name
    }
    
    if(!is.null(list$intervention$intervention_type)){
      intervention_type<-list$intervention$intervention_type
    }
    if(!is.null(list$eligibility$gender)){
      Gender<-list$eligibility$gender
    }
    #Age_Groups
    if(!is.null(list$eligibility$minimum_age)){
      min.age<-list$eligibility$minimum_age
     
    }
    if(!is.null(list$eligibility$minimum_age)){
      max.age<-list$eligibility$maximum_age
    
    }
    if(!is.null(list$enrollment)){
      if(typeof(list$enrollment)=="character"){
        Enrollment<-list$enrollment
      } else{
        Enrollment<-list$enrollment$text
      }
   }
  if(!is.null(list$start_date)){
    if(typeof(list$start_date)=="character"){
      Start_Date = list$start_date
    } else {
      Start_Date = list$start_date$text
    }
  }
    if(!is.null(list$primary_outcome$measure)){
      title<-list$primary_outcome$measure
    }
  newdata<-data.frame(nct_id,trial_title,Conditions,condition,Interventions,intervention_type,Sponsor_Collaborators,sponsor_class,fda.drug,fda.device,Gender,min.age,max.age,Enrollment,Start_Date,Completion_Date,title,p,p.desc,method,param.value,param.type,ci,ci.side,ci.l,ci.u)
  dataf<-rbind(dataf,newdata)
  c<-c+1
  
}

