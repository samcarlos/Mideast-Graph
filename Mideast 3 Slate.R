library(RCurl)
library(XML)

url=("http://www.slate.com/blogs/the_world_/2014/07/17/the_middle_east_friendship_chart.html")
doc = htmlTreeParse(url, useInternalNodes = T)
text = xpathSApply(doc, "//*/div[@class='renderHtml section']", xmlValue)   
#save(text, file="text.rdata")
seperate.text=unlist(strsplit(text[1], ":"))
seperate.text=seperate.text[-c(1,2)]

period.loc=gregexpr("[.]", seperate.text)
last.period.loc=unlist(lapply(period.loc, max))

text.after.period=substring(seperate.text, last.period.loc, nchar(seperate.text))
text.after.period[55]="Turkey"
text.after.period=gsub("[.]","",text.after.period)
text.after.period=c("Egypt", text.after.period)
text.after.period[which(text.after.period=="ISIS")]="Isis"


relationships=c("ENEMY", "COMPLICATED","FRIEND")
relationship.score=rep(-99,78)
for(i in 1:3){
  relationship.score[grepl(relationships[i], seperate.text)]=i-2  
}

##one relationship wasn't classified correctly-its negative
relationship.score[6]=-1




#those that have two names have more than one CAPS in a row
start.loc=which(unlist((regexec("^[A-Z]{2,}", text.after.period)))==1)
headers=c("Egypt", "Hamas", "Hezbollah", "Iran", "Iraq", "Isis", "Israel", "Palestinian Authority", "Syria", "Turkey", "Saudi Arabia")
headers1=c("Al-Qaida",headers)
enders=c(headers[-1],"United States")
text.after.period.1=text.after.period[-79]
text.after.period.1[start.loc]=enders
text.after.period.1[which(text.after.period.1=="Palestinian authority")]="Palestinian Authority"

data=data.frame(text.after.period.1,rep(headers1,12:1),relationship.score)
colnames(data)=c("CountryA", "CountryB", "Score")

all.countries=c(headers1, "United States")



country.numA=rep(0,78)
country.numB=rep(0,78)
for(i in 1:78){
country.numA[which(data[,1]==all.countries[i])]=i  
country.numB[which(data[,2]==all.countries[i])]=i  
}


relationship.mat=matrix(0,13,13)

for(i in 1:dim(data)[1]){
  relationship.mat[country.numA[i],country.numB[i]]=relationship.score[i]
}
relationship.mat=relationship.mat+t(relationship.mat)
colnames(relationship.mat)=all.countries
rownames(relationship.mat)=all.countries

laplace.mideast=-relationship.mat
diag(laplace.mideast)=colSums(abs(relationship.mat))
decomp=eigen(laplace.mideast)


plot(decomp$vectors[,12], decomp$vectors[,13] ,ylab="Last Eigen Vector", xlab="2nd To Last Eigen Vector", main="Network Graph Mideast (Last two Eigen Vectors of Laplacian Matrix)")
text(decomp$vectors[,12],decomp$vectors[,13], labels=colnames(relationship.mat), cex=2)

last.two.eigen=decomp$vectors[,12:13]
rownames(last.two.eigen)=all.countries

data.1=data
data.1=data.1[-which(data$Score==0),]
data.1$Score[which(data.1$Score==-1)]=2
data.1$Score[which(data.1$Score==1)]=4
for(i in 1:dim(data.1)[1]){
segments(last.two.eigen[as.character(data.1$CountryA[i]),1],last.two.eigen[as.character(data.1$CountryA[i]),2],last.two.eigen[as.character(data.1$CountryB[i]),1],last.two.eigen[as.character(data.1$CountryB[i]),2],col=data.1$Score[i], lty=2,lwd=2)
}



laplace.mideast.2=-relationship.mat[-c(1,7),-c(1,7)]
diag(laplace.mideast.2)=colSums(abs(relationship.mat[-c(1,7),-c(1,7)]))
decomp.2=eigen(laplace.mideast.2)
plot(decomp.2$vectors[,10], decomp.2$vectors[,11] ,ylab="Last Eigen Vector", xlab="2nd To Last Eigen Vector", main="Network Graph Mideast (Last two Eigen Vectors of Laplacian Matrix)")
text(decomp.2$vectors[,10],decomp.2$vectors[,11], labels=colnames(relationship.mat[,-c(1,7)]), cex=2)

data.2=data.1[-unique(c(which((data.1[,1] %in% c("Al-Qaida", "Isis")==TRUE)),which((data.1[,2] %in% c("Al-Qaida", "Isis")==TRUE)))),]
last.two.eigen.2=decomp.2$vectors[,10:11]
rownames(last.two.eigen.2)=all.countries[-c(1,7)]

for(i in 1:dim(data.2)[1]){
  segments(last.two.eigen.2[as.character(data.2$CountryA[i]),1],last.two.eigen.2[as.character(data.2$CountryA[i]),2],last.two.eigen.2[as.character(data.2$CountryB[i]),1],last.two.eigen.2[as.character(data.2$CountryB[i]),2],col=data.2$Score[i], lty=2,lwd=2)
}

