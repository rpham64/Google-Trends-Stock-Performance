#This part reads googletrends csv files, and convert and save that into a dataframe
#This part assumes that the csv files are in folders with names 
#'AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO'
#and the csv files are named "report.csv"      "report (1).csv"  "report (2).csv" 
#"report (3).csv"..."report (11).csv"

myread=function(file){
  tmp=read.table(file,skip=4,header=T,sep=',',fill=T, colClasses = "character")
  index=which(tmp[,1]=='Region')[1]
  tmp=tmp[1:(index-2),]
  n=length(tmp)
  for (x in 2:n){
    tmp[,x]=as.numeric(tmp[,x])
  }
  return(tmp)
}

mulfactor=function(data.frame1,data.frame2){
  dim1=dim(data.frame1)
  dim2=dim(data.frame2)
  a=data.frame1[dim1[1],2:dim1[2]]
  b=data.frame2[1,2:dim2[2]]
  i=which(b!=0)
  return(mean(as.numeric(a[i]/b[i])))
}

rowcomb=function(mylist){
  for( x in 1:(length(mylist)-1)){
    factor=mulfactor(mylist[[x]],mylist[[x+1]])
    dim2=dim(mylist[[x+1]])
    mylist[[x+1]][,2:dim2[2]]=factor*mylist[[x+1]][,2:dim2[2]]
  }
  tmp=mylist[[1]]
  for (x in 2:length(mylist)){
    tmp=rbind(tmp,mylist[[x]])
  }
  return(tmp)
}

companies=c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO')

googletrends=list()
filenames=c('report.csv',paste('report (',1:11,').csv',sep=''))

for(x in 1:12){
  filenames1=paste(companies[x],'/',filenames,sep='')
  mylist=lapply(filenames1,myread)
  tmp=rowcomb(mylist)
  googletrends[x]=tmp
}
  
googletrends2=googletrends[[1]]
for (x in 2:12){
  googletrends2=cbind(googletrends1,googletrends[[x]][,2:6])
}

adj.factor=100/unlist(lapply(googletrends2[-1],mean))

for(x in 1:60){
  googletrends2[x+1]=googletrends2[x+1]*adj.factor[x]
}

save(googletrends,file='googletrendsraw.rda')
save(googletrends2,file='googletrends.rda')