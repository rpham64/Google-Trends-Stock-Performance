##Note: This portion of the code assumes that the following folders are in the working directory
#Price&Search
#Volume&Search
#Stock & search vs. time
#Volume&Search vs. Time
#Price&Search vs. Time
#Lag
#Corr
#Corr/Controlled for time

load("C:/Users/vaio/Dropbox/Stat 133 Final Project/DATA/keywords.rda")
load("C:/Users/vaio/Dropbox/Stat 133 Final Project/DATA/updated/googletrends.rda")
load("C:/Users/vaio/Dropbox/Stat 133 Final Project/DATA/updated/stockprices.rda")

stock <- as.character(stockprices[, 1])
match <- which(googletrends2[, 1] %in% stock == TRUE)

googletrends2=googletrends2[match,]
row.names(googletrends2)=NULL
adjcloseprices=stockprices[seq(7,73,by=6)]
volumes=stockprices[seq(6,73,by=6)]
companies=c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO')

#Price vs. Search counts
direc="Price&Search/"
t_values_price=matrix(nrow=12,ncol=5)
p_values_price=matrix(nrow=12,ncol=5)
for(i in 1:12){
  j=0
  for (x in (5*i-3):(5*i+1)){
    png(file=paste(direc,companies[i],'_',names(googletrends2)[x],'.png',sep=''))
    plot(googletrends2[[x]],adjcloseprices[[i]],main=paste(companies[i],'vs. \"',names(googletrends2)[x],'\"'),type='p',
          ylab='Price',xlab=paste('Search Counts of \"',names(googletrends2)[x],'\"'),lty=1,pch=19,cex=0.5)
    lmres=lm(adjcloseprices[[i]]~googletrends2[[x]])
    abline(lmres,col='blue',lwd=2)
    dev.off()
    t_values_price[i,j%%5+1]=coef(summary(lmres))[2,3]
    p_values_price[i,j%%5+1]=coef(summary(lmres))[2,4]
    j=j+1
  }
}
best_match_index_price=apply(t_values_price,1,function(x) order(abs(x),decreasing=T)[1:2])
best_match_price=matrix(names(googletrends2)[5*(rep(0:11,each=2))+best_match_index_price+1],nrow=2)
neg_corr_price=apply(t_values_price,1,function(x) x[order(abs(x),decreasing=T)[1]]<0)
p_values_price=t(p_values_price)[1:60]
significance_price=ifelse(p_values_price<0.05,'significant','not significant')
names(p_values_price)=names(googletrends2)[2:61]
names(significance_price)=names(googletrends2)[2:61]

#Volume vs. Search counts
direc="Volume&Search/"
t_values_volume=matrix(nrow=12,ncol=5)
p_values_volume=matrix(nrow=12,ncol=5)
for(i in 1:12){
  j=0
  for (x in (5*i-3):(5*i+1)){
    png(file=paste(direc,companies[i],'_',names(googletrends2)[x],'.png',sep=''))
    plot(googletrends2[[x]],volumes[[i]],main=paste(companies[i],'vs. \"',names(googletrends2)[x],'\"'),type='p',
         ylab='Volume',xlab=paste('Search Counts of \"',names(googletrends2)[x],'\"'),lty=1,pch=19,cex=0.5)
    abline(lmres,col='blue',lwd=2)
    dev.off()
    lmres=lm(volumes[[i]]~googletrends2[[x]])
    t_values_volume[i,j%%5+1]=coef(summary(lmres))[2,3]
    p_values_volume[i,j%%5+1]=coef(summary(lmres))[2,4]
    j=j+1
  }
}
best_match_index_volume=apply(t_values_volume,1,function(x) order(x,decreasing=T)[1:2])
best_match_volume=matrix(names(googletrends2)[5*(rep(0:11,each=2))+best_match_index_volume+1],nrow=2)
p_values_volume=t(p_values_volume)[1:60]
names(p_values_volume)=names(googletrends2)[2:61]
significance_volume=ifelse(p_values_volume<0.05,'significant','not significant')
names(p_values_volume)=names(googletrends2)[2:61]
names(significance_volume)=names(googletrends2)[2:61]


#Stock & search vs. time
direc="Price&Search vs. Time/"
best_match1=best_match_price[1,]
for (i in 1:12){
  a=if (neg_corr_price[i]) -googletrends2[[best_match1[i]]]+200 else googletrends2[[best_match1[i]]]
  datatmp=cbind(a,100*adjcloseprices[[i]]/mean(adjcloseprices[[i]],na.rm=T))
  png(file=paste(direc,companies[i],'_',best_match1[i],'.png',sep=''))
  matplot(datatmp,type='l',main=paste(companies[i],'stock price and search numbers of\'',best_match1[i],'\''),
          xaxt='n',xlab='Date',ylab='Price and Index (Normalized to 100)')
  axis(1,at=c(-1,126,253,378,503,627),label=c('2011.1','2011.7','2012.1','2012.7','2013.1','2013.7'))
  dev.off()
}

#Stock & volume vs. time
direc="Volume&Search vs. Time/"
best_match1=best_match_volume[1,]
for (i in 1:12){
  a=googletrends2[[best_match1[i]]]
  datatmp=cbind(a,100*volumes[[i]]/mean(volumes[[i]],na.rm=T))
  png(file=paste(direc,companies[i],'_',best_match1[i],'.png',sep=''))
  matplot(datatmp,type='l',main=paste(companies[i],'stock exchange volume and search numbers of\'',best_match1[i],'\''),
          xaxt='n',xlab='Date',ylab='Volume and Search numbers (Normalized to 100)')
  axis(1,at=c(-1,126,253,378,503,627),label=c('2011.1','2011.7','2012.1','2012.7','2013.1','2013.7'))
  dev.off()
}

#Finding the lag
mycorr=function(col1,col2,lag){
  if(lag<0){
    col1=col1[-((length(col1)+lag+1):length(col1))]
    col2=col2[lag:(-1)]
  }else if(lag>0){
    col1=col1[-(1:lag)]
    col2=col2[-((length(col2)-lag+1):length(col2))]
  }
  return(cor(col1,col2,use='pairwise.complete.obs'))
}

#lag data array
corrarray=array(dim=c(12,41,5),dimnames=list(c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO'),
                                             -20:20,1:5))
for (y in 1:12){
  for(x in 2:length(googletrends2)){
    corrarray[y,1:41,(x-2)%%5+1]=sapply(-20:20,mycorr,col1=googletrends2[[x]],col2=adjcloseprices[[y]])
  }
}
bestlag=matrix(nrow=12,ncol=5)
for (y in 1:12){
  bestlag[y,]=apply(corrarray[y,1:41,],2,function(x) order(x^2,decreasing=T)[1])-20
}
bestlag2=apply(bestlag,1,mean)
bestlag3=apply(matrix(bestlag2,nrow=3),2,mean)


#lag plot
direc="Lag/"
for( i in 1:12){
  png(file=paste(direc,companies[i],'.png',sep=''))
  matplot(corrarray[i,1:41,best_match_index_price[,i]]^2,main=paste('Correlation vs Lag,',companies[i]),type='l',
        ylab='r^2',
        xlab='Lag',xaxt='n',lty=1)
  legend('bottom',legend=best_match_price[,i],horiz=T,col=1:2,lty=1,cex=0.7)
  axis(1,at=5*(0:8)+1,label=(-4:4)*5)
  dev.off()
}
png(file=paste(direc,'summary2.png',sep=''))
barplot(bestlag3,las=1,names=c('Tech','Eatery','Auto','Attire'),main='Average Lag for Different Industries')
dev.off()


#Strength of correlation over time
mycorr2=function(col1,col2,duration) {
  trend=numeric(length(col1)-duration)
  for (i in 1:length(trend)) {
    trend[i]=cor(col1[i:(i+duration)],col2[i:(i+duration)],use='pairwise.complete.obs')
  }
  return(trend)
}
corrarray2=array(dim=c(12,553,5),dimnames=list(c('AAPL','GOOG','FB','SBUX','PNRA','PBPB','TSLA','GM','F','GPS','COH','SHOO'),
                                            1:553,1:5))
#corr over time data array
for (y in 1:12){
  for(x in (5*i-3):(5*i+1)){
    corrarray2[y,1:553,(x-2)%%5+1]=mycorr2(col1=googletrends2[[x]],col2=adjcloseprices[[y]],180)
  }
}

#Corr vs. date plots
direc="Corr/"
for(i in 1:12){
  png(file=paste(direc,companies[i],'.png',sep=''))
  matplot(corrarray2[i,,best_match_index_price[,i]],main=paste('Correlation vs Time,',companies[i]),type='l',
          ylab='Correlation',
          xlab='Time',xaxt='n',lty=1)
  legend('bottom',legend=best_match_price[,i],horiz=T,col=1:5,lty=1,cex=0.8)
  axis(1,at=c(126,253,378,503,627)-180,label=c('2011.7','2012.1','2012.7','2013.1','2013.7'))
  dev.off()
}


#controlling for time:
mycorr3=function(col1,col2,duration) {
  index=round(seq(0,(length(col1)/duration),length.out=(length(col1)%/%duration)+1)*duration)
  trend=numeric(length(index)-1)
  for (i in 1:length(trend)) {
    trend[i]=cor(col1[(index[i]+1):index[i+1]],col2[(index[i]+1):index[i+1]],use='pairwise.complete.obs')
  }
  return(trend)
}
corr3=matrix(nrow=60, ncol=733%/%30)

#corr over time data array
for(x in 2:61){
    corr3[(x-1),]=mycorr3(col1=googletrends2[[x]],col2=adjcloseprices[[y]],30)
}


#correlation plot for 30-day periods
direc="Corr/Controlled for time/"
for(i in 2:61){
  png(file=paste(direc,names(googletrends2)[i],'.png',sep=''))
  plot(1:24,corr3[i-1,], main=paste('Correlation vs Time,',names(googletrends2)[i]),type='p',
       ylab='Correlation',ylim=c(-1,1),
       xlab='Time',xaxt='n',lty=1)
  axis(1,at=4*(0:5)+1,label=c('2011.1','2011.7','2012.1','2012.7','2013.1','2013.7'))
  dev.off()
}

t.res=apply(corr3,1,function(x) t.test(x)$p.value)
names(googletrends2)[1+which(t.res<0.05)]


#corr vs. industry
png(file=paste(direc,'summary1.png',sep=''))
cor_ind=apply(corrarray[,21,]^2,1,mean)
cor_ind1=apply(matrix(cor_ind,nrow=3),2,mean)
barplot(cor_ind1,las=1,names=c('Tech','Eatery','Auto','Attire'),main='Average Correlation for Different Industries')
dev.off()

png(file=paste(direc,'summary2.png',sep=''))
cor_ind2=apply(corrarray[,21,]^2,1,max)
cor_ind3=apply(matrix(cor_ind,nrow=3),2,max)
barplot(cor_ind3,las=1,names=c('Tech','Eatery','Auto','Attire'),main='Maximum Correlation for Different Industries')
dev.off()