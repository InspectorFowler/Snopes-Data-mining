snopes.main.crawl<-function(){
  
  ## Loading library
  library(rvest)
  
  ## Website name
  starturl<-'http://www.snopes.com/info/whatsnew.asp'
  
  ## Finding the categories
  mainlink<-read_html(starturl)
  categories<-as.data.frame(mainlink %>% html_nodes('.navColumn ul li a') 
                            %>% html_text())
  links<-as.data.frame(mainlink %>% html_nodes('.navColumn ul li a') 
                       %>% html_attr('href'))
  maindata<-cbind(categories,links)
  names(maindata)<-c('Category','hyperlink')
  
  ## Manually removing 'cokelore' and 'Fauxtography'
  maindata<-maindata[-c(3,10),]
  row.names(maindata)<-seq(1,nrow(maindata))
  
  ## Temporary data for link subsetting
  tempdata<-maindata
  tempdata[,2]<-gsub("^/.*?/","",tempdata[,2])
  
  ## Finding the subcategories
  subdata<-data.frame(category=character(),subcategory=character(),hyperlink=character())
  for (i in 1:nrow(maindata)){
    sublink<-read_html(paste('http://www.snopes.com',maindata[i,2],sep="/"))
    subcategories<-as.data.frame(sublink %>% html_nodes('table td b') 
                                 %>% html_text())
    links<-as.data.frame(sublink %>% html_nodes('.contentColumn td a') 
                            %>% html_attr('href'))
    
    ## Check categories which do not have any subcategories
    if (nrow(links)!=0){
      data<-cbind(as.data.frame(rep(maindata[i,1],nrow(subcategories))),subcategories,links)
      data[,3]<-paste(substr(tempdata[i,2],1,nchar(tempdata[i,2])-4)
                         ,data[,3],sep="/")
      names(data)<-c('Category','subcategory','hyperlink')
      subdata<-rbind(subdata,data)
    }
    else{
      
      ## Check categories which use the new website model
      subcategories<-as.data.frame(sublink %>% html_nodes('center table td b') 
                                   %>% html_text())
      links<-as.data.frame(sublink %>% html_nodes('center table td a') 
                           %>% html_attr('href'))
      if (nrow(subcategories)>1){
        data<-cbind(as.data.frame(rep(maindata[i,1],nrow(subcategories))),subcategories,links)
        data[,3]<-paste(substr(tempdata[i,2],1,nchar(tempdata[i,2])-4)
                        ,data[,3],sep="/")
        names(data)<-c('Category','subcategory','hyperlink')
        subdata<-rbind(subdata,data)
      }
    }
  }
  
  ##Correcting full address and returning the dataset
  subdata[,3]<-paste('http://www.snopes.com',subdata[,3],sep='/')
  write.csv(subdata,file="C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/subdata.csv",
            row.names=FALSE)
  subdata
  
}