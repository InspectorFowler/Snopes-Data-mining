snopes.label.process<-function(){
  
  ## Load the necessary libraries for text processing
  library(tm)
  library(SnowballC)
  library(Matrix)
  library(gdata)
  source('C:/Users/nemecys/Desktop/Study/CSCE 689/Project/check.space.R')
  
  ## Load data folder
  path<-"C:/Users/nemecys/Desktop/Study/CSCE 689/Project/Submission"
  setwd(path)
  
  ## Category folders
  list.categories<-data.frame(list.files())
  names(list.categories)<-'Paths'
  
  ## Create empty data folder to store all the links to the text files
  paths<-data.frame()
  
  ## Loop over the category folders
  for (i in 1:nrow(list.categories)){
    
    ## list all the files in the category
    file.paths<-data.frame(paste(path,list.categories[i,1],
                                 list.files(paste(path,list.categories[i,1],sep="/")),sep="/"))
    paths<-rbind(paths,file.paths)
  }
  names(paths)<-'Paths'
  paths<-data.frame(lapply(paths,as.character),stringsAsFactors=FALSE)
  
  ## Create empty dataframe to store all the Data and label indicies
  data<-data.frame(Information.ID=numeric(),feature.name=character(),Value=numeric())
  labeldata<-data.frame(Document.ID=numeric(),label=character(),Code=numeric(),stringsAsFactors = FALSE)
  
  ## Loop over all the files and process
  for (i in 1:nrow(paths)){
    
    text<-paste(readLines(paths[i,1]), collapse=" ")
    text<-removeNumbers(text)
    text<-tolower(text)
    text<-stripWhitespace(text)
    
    ## Search for missing spaces and insert at appropriate places
    text<-check.space(text)

    ## Creating the Label Index
    labeldata[i,1]<-i
    begin.index<-regexpr('@status_begin@@@',text)[1]+nchar('@status_begin@@@')+1
    end.index<-regexpr('@@@status_end@@@',text)[1]-2
    labeldata[i,2]<-as.character(substr(text,begin.index,end.index))
    
    ## Coverting to numeric cases
    if (nchar(labeldata[i,2])>=25) labeldata[i,2]<-substr(labeldata[i,2],1,25)
    if (labeldata[i,2]=='false') labeldata[i,3]=0
    if (labeldata[i,2]=='true') labeldata[i,3]=1
    if (labeldata[i,2]=='legend') labeldata[i,3]=2
    if (grepl('hoax',as.character(labeldata[i,2]))==TRUE||
        grepl('inaccurate',as.character(labeldata[i,2]))==TRUE||
        grepl('false',as.character(labeldata[i,2]))==TRUE){
      labeldata[i,2]<-'false'
      labeldata[i,3]<-0
    }
    if (grepl('real',as.character(labeldata[i,2]))==TRUE||
        grepl('true',as.character(labeldata[i,2]))==TRUE){
      labeldata[i,2]<-'true'
      labeldata[i,3]<-1
    }
    if (grepl('not quite',as.character(labeldata[i,2]))==TRUE||
        grepl('mixture',as.character(labeldata[i,2]))==TRUE||
        grepl('multiple',as.character(labeldata[i,2]))==TRUE||
        grepl('probably not',as.character(labeldata[i,2]))==TRUE){
      labeldata[i,2]<-'mixture'
      labeldata[i,3]<-3
    }
    if (grepl('undetermined',as.character(labeldata[i,2]))==TRUE||
        grepl('incomplete',as.character(labeldata[i,2]))==TRUE){
      labeldata[i,2]<-'undetermined'
      labeldata[i,3]<-4
    }

    ## Iteration tracking
    Sys.sleep(0.005)
    cat(paste(i,"-",sep=""))
    
  }

  ## Write the three dataset to system
  write.table(labeldata,file="C:/Users/nemecys/Desktop/Study/CSCE 689/Project/labels.txt",
              row.names=FALSE)
  labeldata
}