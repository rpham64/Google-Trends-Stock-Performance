#This part writes company profile into txt files.

library(XML)
companies=c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO')
for(company in companies){

  filename=paste(company,'.txt',sep='')

  #From Wall Street Journal
  url=paste("http://quotes.wsj.com/",company,"/company-people",sep='')
  doc=htmlParse(url)
  wsj=paste(xpathSApply(doc,"//div[@id ='cr_company_desc']/p",xmlValue),collapse=' ')
  write(wsj,filename,append=T,sep=' ')

  #From CNN
  url=paste("http://money.cnn.com/quote/profile/profile.html?symb=",company,sep='')
  doc=htmlParse(url)
  cnn=paste(xpathSApply(doc,"//div[@id ='wsod_companyDescription']",xmlValue),collapse=' ')
  write(cnn,filename,append=T,sep=' ')

  #From Yahoo
  url=paste("http://finance.yahoo.com/q/pr?s=",company,"+Profile")
  doc=htmlParse(url)
  yahoo=paste(xpathSApply(doc,"//td[@class ='yfnc_modtitlew1']/p",xmlValue),collapse=' ')
  write(yahoo,filename,append=T,sep=' ')

  #From Reuters
  url=paste("http://www.reuters.com/finance/stocks/companyProfile?symbol=",company,'.O',sep='')
  doc=htmlParse(url)
  reuter=paste(xpathSApply(doc,"//div[@class ='moduleBody']/p",xmlValue),collapse=' ')
  write(reuter,filename,append=T,sep=' ')

  #From businessweek
  busi=character()
  for(page in 1:4){
    url=paste("http://investing.businessweek.com/research/stocks/snapshot/snapshot_article.asp?ticker=",company,"&page=",page,sep='')
    doc=htmlParse(url)
    busi=paste(busi,xpathSApply(doc,"//div[@id ='article']/p",xmlValue),collapse=' ')
  }
  write(busi,filename,append=T,sep=' ')

  #From Nasdaq
  url=paste("http://www.nasdaq.com/symbol/",company,sep='')
  doc=htmlParse(url)
  nasdaq=paste(xpathSApply(doc,"//div[@id ='content_main']/div/p",xmlValue),collapse=' ')
  write(nasdaq,filename,append=T,sep=' ')
  
}