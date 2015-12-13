#This part finds the most frequent words in company's profile

library(XML)
#Getting the most frequent words
doc=htmlParse("http://www.englishclub.com/vocabulary/common-words-5000.htm")
frequent_words=unlist(readHTMLList(doc))[1:200]
more_frequent_words=c('are','an','been','has','had','is', 's','was','were')
frequent_words=c(frequent_words,paste(frequent_words,'ed',sep=''),paste(frequent_words,'s',sep=''),
                 more_frequent_words)

shrink=function(array_in,k){
  return(sort(array_in,decreasing=T)[1:k])
}

term_freq=function(text,threshhold,possesive=T,case=T,minus=NULL){
  text=gsub("[^[:alnum:]'-]",' ',text)
  
  if(possesive){
    text=gsub("'s",' ',text)
  }
  text=gsub('[\']',' ',text)
  
  if(case){
    text=tolower(text)
  }
  
  text=gsub("[[:blank:]]+",' ',text)
  text=unlist(strsplit(text,' '))
  
  freq=tapply(rep(1,length(text)),text,sum)
  freq=freq[freq>=threshhold]
  names=setdiff(names(freq),minus)
  freq=freq[names]
  
  return(freq)
}

companies=c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO')

#Creating a word frequency list
keywordsraw=list()
for (i in 1:12){
  text=paste(readLines(paste(companies[i],'.txt',sep=''),encoding='UTF-8'),collapse=' ')
  keywordsraw[[i]]=term_freq(text,2,minus=frequent_words)
}


#Shrinking the size of the frequency list
for (i in 1:12){
  keywordsraw[[i]]=shrink(keywordsraw[[i]],20)
}
names(keywordsraw)=companies
save(keywordsraw,file='keywordsraw.rda')

#reorganizing data
keywords=data.frame(dimnames(keywordsraw[[1]]),keywordsraw[1])
for (x in 2:12){
  keywords=cbind(keywords,data.frame(dimnames(keywordsraw[[x]]),keywordsraw[x]))
}
names(keywords)=paste(c('keywords.','frequency.'),rep(companies,each=2))
rownames(keywords)=NULL
save(keywords,file='keywords.rda')