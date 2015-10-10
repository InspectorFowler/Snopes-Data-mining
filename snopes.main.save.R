snopes.data.save<-function(data){
  
  ## Create Initial Folder
  dir.create("C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/Dataset")
  
  for (i in 1:nrow(data)){
    
    ## Create Category folder if not already present and save the record
    save.dir<-paste("C:/Users/nemecys/Desktop/Fall 15 Courses/CSCE 689/Project/Dataset/",
                    "823007432_",data[i,1],sep="")
    
    if (dir.exists(save.dir)==TRUE){
      write(data[i,5],paste(save.dir,"/",gsub("/","-",substr(data[i,4],23,nchar(data[i,4])-4)),".txt",sep=""), sep="\t") 
    }
    
    else{
      dir.create(save.dir)
      write(data[i,5],paste(save.dir,"/",gsub("/","-",substr(data[i,4],23,nchar(data[i,4])-4)),".txt",sep=""), sep="\t")
    }
  }
}