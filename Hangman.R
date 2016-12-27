##########################################################################
                       #some preliminary work                            
                                                                         
#choose the dictionary file(must be .csv)                                
words=read.csv(choose.files(),header = FALSE)                            
M=apply(words,1,function(x) as.character(x))                             
#make a sort regarding the number of characters                          
words_sort=data.frame(M,nchar=nchar(M))                                  
words_sort=words_sort[order(words_sort[[2]],decreasing=FALSE),]          
#list_sum[[i]] is the matrix containing the seperated letter of words    
# with length 'i'                                                        
list_sum=list()                                                          
for(i in 1:max(words_sort[,2])){                                         
  if(sum(words_sort[,2]==i)==0){                                         
    list_sum[[i]]=c()                                                    
  }                                                                      
  else{                                                                  
    M=as.character(words_sort[which(words_sort[,2]==i),1])               
    list_sum[[i]]=matrix(unlist(strsplit(M,split = ""))                  
                         ,ncol=i,byrow=TRUE)                             
  }                                                                      
}                                                                        
                                                                         
#alphabet to choose                                                        
alphabet=(strsplit('qwertyuiopasdfghjklzxcvbnm',split=""))[[1]]          
                                                                         
#select word that you want to play                                       
#here I choose 500 random words from the 50000 dictionary                
word=as.character(words[sample(1:50000,500),1])                          
#if you want to test the full dictionary,use the one below one           
#word=as.character(words[,1])                                            
                                                                             
##########################################################################


##main function
Hangman=function(word){
  nchar=nchar(word)
  word=strsplit(word,split="")[[1]]
  #list1 contains the seperated letter of each word with length 'nchar'
  list1=list_sum[[nchar]]
  #cout the number of different letters in list1
  #choose the first letter based on the biggest count
  count=sapply(alphabet,function(x) sum(list1==x))
  chooseletter=names(count[which(count==max(count))])[1]
  #save the letters already choosed or missed;and the mistake times(misnum)
  missed=c()
  chooses=c()
  blanks=rep('_',nchar)
  misnum=0
  
  #begin selecting letters
  while(TRUE){
  if(sum(word!=chooseletter)<nchar) {
    chooses=c(chooses,chooseletter)
    blanks[which(word==chooseletter)]=chooseletter
    cat('guess:',chooseletter,'\n',blanks,"missed :",missed,'\n','\n')
    #select words with the chooseletter on specific positions
    if(!is.null(nrow(list1))&&nrow(list1)!=1){
      matrix=(list1[,which(word==chooseletter)]==chooseletter)
      if(sum(word!=chooseletter)<(nchar-1))
        list1=list1[which(apply(matrix,1,sum)==sum(word==chooseletter)),]
      else list1=list1[which(matrix==TRUE),]
    }
  }
  else {missed=c(missed,chooseletter)
    cat('guess:',chooseletter,'\n',blanks,"missed :",missed,'\n','\n')
    misnum=misnum+1
    #select words without the chooseletter
    if(!is.null(nrow(list1))){
    list1=list1[which(apply(list1!=chooseletter,1,sum)==nchar),]
    }
  }
    #deduced the letter that already counted before
    newalphabet=alphabet[alphabet%in%c(chooses,missed)==FALSE]
    #cout the number of different letters in new list1
    count=sapply(newalphabet,function(x) sum(list1==x))
    chooseletter=names(count[which(count==max(count))])[1]
    
  if(sum(blanks!=word)<1) return("got it!")
  if(misnum>=6) return("failed!")
  }
}


##outcome
#details are saved in 'result'
t=system.time({
  result=sapply(word,Hangman)
  ratio=sum(result=="got it!")/length(result)
  }) 
cat("Number of words tested: ",length(result),"\n")
cat("Number of words tested: ",sum(result=="got it!"),"\n")
cat("Correct Guesses(%): ",ratio*100,"%",'\n')
cat("Time to run: ",as.numeric(t)[3],"seconds")


