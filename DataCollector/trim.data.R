
#search for exclusion
#merge new data frame with existing data frame
#m2 means month number(year*12+month)
get.m2<-function(date){
  l<-unlist(strsplit(date,' '))
  if(length(l)==2){
    m<-l[1]
    y<-l[2]
  } else if(length(l)==3){
    m<-l[1]
    y<-l[3]
  } else {
    print(date)
  }
  if(m=="January"){
    m2<-1+as.numeric(y)*12
  }  #February March April May June July August September October November December
  if(m=="February") {
    m2<-2+as.numeric(y)*12
  }
  if(m=="March"){
    m2<-3+as.numeric(y)*12
  }
  if(m=="April"){
    m2<-4+as.numeric(y)*12
  }
  if(m=="May"){
    m2<-5+as.numeric(y)*12
  }
  if(m=="June"){
    m2<-6+as.numeric(y)*12
  }
  if(m=="July"){
    m2<-7+as.numeric(y)*12
  }
  if(m=="August"){
    m2<-8+as.numeric(y)*12
  }
  if(m=="September"){
    m2<-9+as.numeric(y)*12
  }
  if(m=="October"){
    m2<-10+as.numeric(y)*12
  }
  if(m=="November"){
    m2<-11+as.numeric(y)*12
  }
  if(m=="December"){
    m2<-12+as.numeric(y)*12
  }
  return(m2)
}

dir<-"/Users/haosicheng/Desktop/BIDMC/search_result"
year<-c(2013,2014)#list of years
phase<-"phase III" #no need to specify Phase again if you did in the .gov website
fda.drug<-TRUE #FALSE is going to search both "Yes" and "No" same as fda.device
fda.device<-TRUE 
sponsor<-"Industry"

search.trial<-function(dir,year,phase,fda.drug,fda.device,sponsor,){
  setwd(dir)
  name.list<-list.files()
  len<-length(name.list)
  c<-1
  
  print(paste("Find",c,"trials"))
}


v<-0
l<-rep(0,300)
c<-1
for(j in 1:length(old.id)){
  a<-sum(as.character(old.id[j])==new.id)
  if(a==0){
    l[c]<-as.character(old.id[j])
    c<-c+1
  }
  v<-v+a
}
print(v)

