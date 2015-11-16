check.space<-function(text){
  
  attributes<-c('claim','status','example','origin','variation','source','told','update')
  
  for (i in 1:length(attributes)){
    tag<-paste('@@@',attributes[i],sep='')
    tag.indicies<-as.vector(gregexpr(tag,text)[[1]])
    if (length(tag.indicies)!=1){
      for (j in 1:length(tag.indicies)){
        if (substr(text,tag.indicies[j]-1,tag.indicies[j]-1)!=' '){
          text<-paste(substr(text,1,tag.indicies[j]-1),substr(text,tag.indicies[j],nchar(text)),sep=' ')
        }
      }
    }
  }
  
  generic.tag.1<-"begin@@@"
  generic.tag.2<-"end@@@"
  tag.1.indicies<-as.vector(gregexpr(generic.tag.1,text)[[1]])
  
  for (i in 1:length(tag.1.indicies)){
    if (substr(text,tag.1.indicies[i]+9,tag.1.indicies[i]+9)!=' '){
      text<-paste(substr(text,1,tag.1.indicies[i]+8),substr(text,tag.1.indicies[i]+9,nchar(text)),sep=' ')
    }
  }
  
  tag.2.indicies<-as.vector(gregexpr(generic.tag.2,text)[[1]])
  
  for (i in 1:length(tag.2.indicies)){
    if (substr(text,tag.2.indicies[i]+9,tag.2.indicies[i]+9)!=' '){
      text<-paste(substr(text,1,tag.2.indicies[i]+8),substr(text,tag.2.indicies[i]+9,nchar(text)),sep=' ')
    }
  }
  
  text
}