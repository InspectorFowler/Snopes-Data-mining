snopes.data.process<-function(data){
  
  ## Ensuring all the columns are character format
  data<-data.frame(lapply(data,as.character),stringsAsFactors=FALSE)
  
  for (i in 1:nrow(data)){
    
    ## Removing remaining html codes
    data[i,5]<-gsub("\n","",data[i,5])
    data[i,5]<-gsub("\r","",data[i,5])
    data[i,5]<-gsub("Â","",data[i,5])
    data[i,5]<-gsub("[\"]","",data[i,5])
    data[i,5]<-gsub("\r","",data[i,5])
    data[i,5]<-gsub("Â","",data[i,5])
    
    ## Creating pointers for model features
    data[i,5]<-gsub("Claim:"," Claimbeginning ",data[i,5])
    data[i,5]<-gsub("Legend:"," Claimbeginning ",data[i,5])
    data[i,5]<-gsub("Virus:"," Claimbeginning ",data[i,5])
    data[i,5]<-gsub("Status:"," Statusbeginning ",data[i,5])
    data[i,5]<-gsub("Example:"," Examplebeginning ",data[i,5])
    data[i,5]<-gsub("Examples:"," Examplebeginning ",data[i,5])
    data[i,5]<-gsub("Origins:"," Originbeginning ",data[i,5])
    data[i,5]<-gsub("Variations:"," Variationbeginning ",data[i,5])
    data[i,5]<-gsub("Sources:"," Sourcebeginning ",data[i,5])
    data[i,5]<-gsub("Last updated:"," Updatebeginning ",data[i,5])
    data[i,5]<-gsub("Example:"," Examplebeginning ",data[i,5])
    data[i,5]<-gsub("Also told in:"," toldbeginning ",data[i,5])
    
    ## Creating Status tag in case of missing ones
    data[i,5]<-gsub("TRUE"," Statusbeginning TRUE ",data[i,5])
    data[i,5]<-gsub("FALSE"," Statusbeginning FALSE ",data[i,5])
    data[i,5]<-gsub("UNDETERMINED"," Statusbeginning UNDETERMINED ",data[i,5])
    data[i,5]<-gsub("LEGEND"," Statusbeginning LEGEND ",data[i,5])
    data[i,5]<-gsub("MIXTURE"," Statusbeginning MIXTURE ",data[i,5])
    
    ## Removing all the extra spaces and tabs
    data[i,5]<-gsub("[^A-Za-z0-9]", " ", data[i,5])
    data[i,5]<-gsub("^ *|(?<= ) | *$", "", data[i,5], perl=T)
    
    
    ## Removing all copyright details
    data[i,5]<-gsub("Urban Legends Reference Pages 1995 2015 by snopes com","",data[i,5],perl=T)
    data[i,5]<-gsub("This material may not be reproduced without permission snopes and the snopes com logo are registered service marks of snopes com","",data[i,5],perl=T)
    
    ## Removing unwanted HTML codes
    data[i,5]<-gsub("var CasaleArgs new Object CasaleArgs version 2 CasaleArgs adUnits 4 CasaleArgs casaleID 159339","",data[i,5],perl=T)
    
    ## Removing unwanted data in specific cases
    index<-gregexpr("\\<Claimbeginning\\>",data[i,5])[[1]][1]
    data[i,5]<-substr(data[i,5],index,nchar(data[i,5]))
    
    ## Rearranging model features
    data[i,5]<-gsub("Claimbeginning","@@@Claim_begin@@@",data[i,5])
    data[i,5]<-gsub("Statusbeginning","@@@Status_begin@@@",data[i,5])
    data[i,5]<-gsub("Examplebeginning","@@@Example_begin@@@",data[i,5])
    data[i,5]<-gsub("Originbeginning","@@@Origin_begin@@@",data[i,5])
    data[i,5]<-gsub("Variationbeginning","@@@Variation_begin@@@",data[i,5])
    data[i,5]<-gsub("Sourcebeginning","@@@Source_begin@@@",data[i,5])
    data[i,5]<-gsub("toldbeginning","@@@Told_begin@@@",data[i,5])
    data[i,5]<-gsub("Updatebeginning","@@@Update_begin@@@",data[i,5])
    
    ## Iteration tracking
    Sys.sleep(0.1)
    cat(paste(i,"-",sep=""))
  }
  
  ## Keeping only those cases starting with 'Claim'
  data<-data[(substr(data[,5],1,8)=="@@@Claim"),]
  
  for (i in 1:nrow(data)){
    ## Creating feature end tags
    split<-unlist(strsplit(data[i,5],"@@@"))
    text<-as.character()
    for(j in seq(from=2,to=length(split),by=2)){
      feature<-unlist(strsplit(split[j],"_"))[1]
      text<-paste(text,"@@@",split[j],"@@@",split[j+1],"@@@",feature,"_end@@@ ",sep="")
    }
    data[i,5]<-text
    
    ## Iteration tracking
    Sys.sleep(0.1)
    cat(paste(i,"-",sep=""))
  }
  
  ## Writing file to system and outputting to console
  write.csv(data,file="C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/processed.data.csv",
            row.names=FALSE)
  data
  
}