#This part of the code scrapes stock price data from Yahoo Finance

companies=c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO')
stockpricesraw=list()
for (i in 1:12){
  url=paste("http://ichart.finance.yahoo.com/table.csv?s=",companies[i],"&a=00&b=1&c=2011&d=10&e=30&f=2013&g=d&ignore=.csv",sep='')
  stockpricesraw[[i]]=read.csv(url)
}
names(stockpricesraw)=companies

stockprices=stockpricesraw[[1]]
for (x in 2:12){
  stockprices=merge(stockprices, stockpricesraw[[x]],all=T,
                    by.x="Date",by.y="Date")
}
colnames=c('Date')
for (x in 1:12){
  tmp=paste(c('Open','High','Low','Close','Volume','Adj.Close'),'.',companies[[x]],sep='')
  colnames=c(colnames,tmp)
}
names(stockprices)=colnames

save(stockpricesraw,file='stockpricesraw.rda')
save(stockprices,file='stockprices.rda')
