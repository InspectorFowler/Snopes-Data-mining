snopes.text.process<-function(){
  
  ## Load the necessary libraries for text processing
  library(tm)
  library(SnowballC)
  library(Matrix)
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
  labeldata<-data.frame(Information.ID=numeric(),label=character())
  
  ## Loop over all the files and process
  for (i in 1:nrow(paths)){
    
    text<-paste(readLines(paths[i,1]), collapse=" ")
    text<-removeNumbers(text)
    text<-tolower(text)
    text<-stripWhitespace(text)
    
    ## Search for missing spaces and insert at appropriate places
    text<-check.space(text)
    
    ## Converting the text to a corpus
    text.corpus<-Corpus(VectorSource(text))
    
    ## Removing stop words
    text.corpus<-tm_map(text.corpus,removeWords,stopwords('SMART'))
    text.corpus<-tm_map(text.corpus,stripWhitespace)
    
    ## Stem the document
    text.corpus<-tm_map(text.corpus,stemDocument)
    
    ## Create a Term Document Matrix
    TermDocmatrix<-TermDocumentMatrix(text.corpus)
    TermDocmatrix<-as.data.frame(inspect(TermDocmatrix))
    TermDocmatrix<-cbind(row.names(TermDocmatrix),TermDocmatrix[,1])
    TermDocmatrix<-data.frame(TermDocmatrix)
    TermDocmatrix[,1]<-as.character(TermDocmatrix[,1])
    TermDocmatrix[,2]<-as.numeric(TermDocmatrix[,2])
    TermDocmatrix<-TermDocmatrix[!grepl("@",TermDocmatrix[,1]),]
    row.names(TermDocmatrix)<-seq_along(1:nrow(TermDocmatrix))
    TermDocmatrix<-cbind(rep.int(i,nrow(TermDocmatrix)),TermDocmatrix)
    names(TermDocmatrix)<-c("Document.ID","Feature.Name","Value")
    data<-rbind(data,TermDocmatrix)
    
    ## Creating the Label Index
    labeldata[i,1]<-i
    begin.index<-regexpr('@status_begin@@@',text)[1]+nchar('@status_begin@@@')+1
    end.index<-regexpr('@@@status_end@@@',text)[1]-2
    labeldata[i,2]<-substr(text,begin.index,end.index)
    
    ## Iteration tracking
    Sys.sleep(0.1)
    cat(paste("----------------Document ",i," Processed--------------",sep=""))

  }
  
  ## Creating the feature index
  features<-as.data.frame(unique(data[,2]))
  features<-data.frame(lapply(features,as.character),stringsAsFactors=FALSE)
  features<-as.data.frame(features[order(as.character(features[,1])),])
  features<-cbind(seq_along(1:nrow(features)),features)
  names(features)<-c("Feature.ID","Feature.Name")
  
  ## substituting feature ID's for feautre names
  data<-merge(data,features,by.x='Feature.Name',by.y='Feature.Name',all.x=TRUE)
  data<-data[,-1]
  data<-data[,c(1,3,2)]
  row.names(data)<-seq_along(1:nrow(data))
  
  ## Write the three dataset to system
  write.table(data,file="C:/Users/nemecys/Desktop/Study/CSCE 689/Project/UIN_data.txt",
              row.names=FALSE)
  write.table(features,file="C:/Users/nemecys/Desktop/Study/CSCE 689/Project/features.txt",
            row.names=FALSE)
}