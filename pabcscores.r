library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(plyr)
library(gridExtra)
library(colorspace)

directory<-getwd()
image_directory<-paste(directory,"/images",sep="")

#############################

print.pdf<-function(geom,directory,name)
{
pdf(file=paste(directory,'/',name,'.pdf', sep=""), width=14,height=7,bg="white")
print(geom)
dev.off()
 }
###############################

data<-read.csv(paste(directory,"/pabcscores.csv",sep=""))

###############################


data.gender<-by(data["PABC.Average.Score"],data["Gender"],colMeans)


data.gender<-ddply(data, "Gender", summarise, total = mean(PABC.Average.Score), count=length(PABC.Average.Score))

data.decade<-ddply(data, "Decade", summarise, total = mean(PABC.Average.Score), count=length(PABC.Average.Score))

#################################

pdf(file=paste(image_directory,'/','PABCgender','.pdf', sep=""), width=14,height=7,bg="white")

result.plot<-ggplot(data.gender,aes(x=factor(Gender), y=total, fill=Gender))+geom_bar(stat="identity")
result.plot<-result.plot+ggtitle("Average Score By Gender")
result.plot<-result.plot+scale_y_discrete(name="Average Score")
result.plot<-result.plot+scale_x_discrete(name="Gender")
#print.pdf(result.plot,image_directory,"gender_score")

result.plot.p<-ggplot(data.gender,aes(x=factor(Gender), y=count, fill=Gender))+geom_bar(stat="identity")
result.plot.p<-result.plot.p+ggtitle("Total Books By Gender")
result.plot.p<-result.plot.p+scale_y_discrete(name="Total Books")
result.plot.p<-result.plot.p+scale_x_discrete(name="Gender")
#print.pdf(result.plot,image_directory,"gender_total")

grid.arrange(result.plot.p, result.plot,ncol=2, nrow=1	)
dev.off()

##############################

pdf(file=paste(image_directory,'/','PABCdecade','.pdf', sep=""), width=14,height=7,bg="white")

result.plot<-ggplot(data.decade,aes(x=factor(Decade), y=total))+geom_bar(stat="identity",fill="blue")
result.plot<-result.plot+ggtitle("Average Score By Decade")
result.plot<-result.plot+scale_y_discrete(name="Average Score")
result.plot<-result.plot+scale_x_discrete(name="Decade")
#print.pdf(result.plot,image_directory,"decade_score")

result.plot.p<-ggplot(data.decade,aes(x=factor(Decade), y=count))+geom_bar(stat="identity",fill="blue")
result.plot.p<-result.plot.p+ggtitle("Total Books By Decade")
result.plot.p<-result.plot.p+scale_y_discrete(name="Total Books")
result.plot.p<-result.plot.p+scale_x_discrete(name="Decade")
#print.pdf(result.plot,image_directory,"decade_total")

grid.arrange(result.plot.p, result.plot,ncol=1, nrow=2)
dev.off()

