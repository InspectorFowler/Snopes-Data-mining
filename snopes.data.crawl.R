snopes.data.crawl<-function(maindata){
  
  ## Loading library
  library(rvest)
  library(XML)
  
  ## Ensuring all the columns are character format
  maindata<-data.frame(lapply(maindata,as.character), stringsAsFactors=FALSE)
  
  ## Create empty dataframe to store all the datalinks
  data<-data.frame(category=character(),subcategory=character(),
                   sublink=character(),datalink=character())
  
  ## Subsetting and removing 'Holidays' - no subcategories or datalinks
  maindata<-maindata[maindata$Category!='Holidays',]
  
  ## Removing Inboxer rebellion - Class action settlements
  maindata<-maindata[(maindata$Category!='Inboxer Rebellion'&
                        maindata$subcategory!='Class Action Settlements'),]
  maindata<-maindata[(maindata$Category!='Media Matters'&
                        maindata$subcategory!='Not Necessarily the News'),]
  
  for (i in 1:nrow(maindata)){
    sublink<-read_html(maindata[i,3])
    datalink<-as.data.frame(sublink %>% html_nodes('table center table a') 
                            %>% html_attr('href'),stringsAsFactors = FALSE)
 
    ## Sorted data links category
    if(nrow(datalink)==0){
      datalink<-as.data.frame(sublink %>% html_nodes('.article_text b a') 
                              %>% html_attr('href'),stringsAsFactors = FALSE)
      names(datalink)<-'datalink'
      
      ## Checking again if we have pulled the correct data or not
      if(nrow(datalink)!=0){
        for (j in 1:nrow(datalink)){
          if(substring(datalink[j,1],1,1)=='/'){
            datalink[j,1]<-paste('http://www.snopes.com',as.character(datalink[j,1]),sep='')
          }
          else{
            rename<-vapply(strsplit(substr(maindata[i,3],1,nchar(maindata[i,3])-4), "/"),
                           function(x) paste(unique(x), collapse = "/"),character(1L))
            datalink[j,1]<-paste(rename,datalink[j,1],sep='/')
          }
        }
        datalink<-cbind(as.data.frame(rep(maindata[i,1],nrow(datalink))),
                        as.data.frame(rep(maindata[i,2],nrow(datalink))),
                        as.data.frame(rep(maindata[i,3],nrow(datalink))),datalink)
        names(datalink)<-c('Category','subcategory','sublink','datalink')
        data<-rbind(data,datalink)
      }
    }
    
    else{
      ## Unsorted data links category
      datalink<-as.data.frame(datalink[complete.cases(datalink),])
      datalink<-as.data.frame(datalink[-1,1])
      names(datalink)<-'datalink'
      datalink <- data.frame(lapply(datalink, as.character), stringsAsFactors=FALSE)
      
      for (j in 1:nrow(datalink)){
        if(substring(datalink[j,1],1,1)=='/'){
          datalink[j,1]<-paste('http://www.snopes.com',as.character(datalink[j,1]),sep='')
        }
        else{
          rename<-vapply(strsplit(substr(maindata[i,3],1,nchar(maindata[i,3])-4), "/"),
                         function(x) paste(unique(x), collapse = "/"),character(1L))
          datalink[j,1]<-paste(rename,datalink[j,1],sep='/')
        }
      }
      datalink<-cbind(as.data.frame(rep(maindata[i,1],nrow(datalink))),
                      as.data.frame(rep(maindata[i,2],nrow(datalink))),
                      as.data.frame(rep(maindata[i,3],nrow(datalink))),datalink)
      names(datalink)<-c('Category','subcategory','sublink','datalink')
      data<-rbind(data,datalink)
    }
  }
  
  ## Writing file to system and outputting to console
  write.csv(data,file="C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/data.csv",
            row.names=FALSE)
  data
}