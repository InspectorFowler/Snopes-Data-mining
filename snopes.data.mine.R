snopes.data.mine<-function(data){
  
  ## WARNING : The code might crash sometimes due to weak deta connection
  ## In such an event, re-run the code from the last iteration number by
  ## changing the master for loop.
  
  ## Loading the library
  library(XML)
  
  ## Create the csv dataset
  out.data<-data.frame(category=character(),subcategory=character(),
                   sublink=character(),datalink=character(),Text=character())
  write.table(out.data,file="C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/complete.data.csv",
            row.names=FALSE,sep=",")
  
  ## Creating a loop over all the datalinks and extracting the datasets
  for (i in 1:nrow(data)){
    linkparse<-htmlTreeParse(data[i,4],useInternal=TRUE)
    link.text<-unlist(xpathApply(linkparse, '//font', xmlValue))
    text<-link.text[which.max(nchar(link.text))]
    
    if(is.null(text)==FALSE){
      link.text2<-unlist(xpathApply(linkparse, '//div', xmlValue))
      text2<-link.text2[which.max(nchar(link.text2))]
      if(is.null(text2)==FALSE){
        if(nchar(text2)>nchar(text)){
          text=text2
          start<-gregexpr(pattern ='-->',text)
          text<-substr(text,start[[1]][1]+3,nchar(text))
          stops<-gregexpr(pattern ='<!--',text)
          text<-substr(text,1,stops[[1]][1]-1)
        }
      }
    }
    
    else{
      link.text<-unlist(xpathApply(linkparse, '//div', xmlValue))
      text<-link.text[which.max(nchar(link.text))]
      if(is.null(text)==FALSE){
        start<-gregexpr(pattern ='-->',text)
        text<-substr(text,start[[1]][1]+3,nchar(text))
        stops<-gregexpr(pattern ='<!--',text)
        text<-substr(text,1,stops[[1]][1]-1)
      }
      else text="Not Available"
    }
    
    ## Inserting the text into the dataset
    data[i,5]<-text
    
    ## Iteration tracking
    Sys.sleep(0.1)
    cat(paste(i,"-",sep=""))
    
    ## Writing each datapoint iteratively
    write.table(data[i,],file="C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/complete.data.csv",
                row.names=FALSE,col.names=FALSE,append=TRUE,sep=",")
  }
}

    