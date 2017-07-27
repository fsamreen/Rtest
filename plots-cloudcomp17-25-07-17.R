require('ggplot2')
require('Hmisc')

 
#---voilin plot for VARD-EC2--------
#===============================================
rm(list=ls())
setwd('C:\\dataset\\cloudcom2017')  # for Windows
exec_times=read.csv("combine-node-data-vard.csv",header=T)

unique(exec_times$Nodetype)
node_refs = c("2.1","2.2","3.1","3.2","4.1","4.2")
node_names = c("t2.small","t2.medium","m3.medium","m3.large","c4.large","c4.xlarge")
node_price= c("0.026", "0.052", "0.070", "0.140", "0.116", "0.232")
node_types = data.frame(node_refs,node_names, node_price)
colnames(node_types) <- c("Nodetype","NodeName", "NodePrice")
rm(node_refs,node_names, node_price)
exec_times <- merge(exec_times,node_types)

#exec_times<- exec_times[order(exec_times$NodePrice), ]
#sizes <- factor(c("small", "large", "large", "small", "medium"))
#x <- exec_times[order(exec_times$NodeName), ]
#exec_times <- exec_times[order(exec_times$NodePrice), ]

#exec_times <- ordered(exec_times$NodeName, levels = c("t2.small","t2.medium","m3.medium","m3.large","c4.large","c4.xlarge"))

#mtcars$carb2 <- factor(mtcars$carb, levels = rev(levels(factor(mtcars$carb))))


exec_times$NodeName <- factor(exec_times$NodeName, levels = exec_times$NodeName[order(exec_times$NodePrice)], )

# Basic violin plot
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='vard-ec2-violin.pdf',width=8,height=6)

p <- ggplot(exec_times, aes(x=NodeName, y=Ptime), color=NodeName, alpha=0.3) + 
  geom_violin(trim=FALSE)+
  #scale_y_continuous(expand=c(50,250)) + theme_bw()+
  scale_y_continuous(limits = c(50, 250))+theme_bw()+
  xlab("Instance Types") + ylab("Execution Time (seconds)")
p<-p + stat_summary(fun.y=median, geom="point", size=2, color="red")
p+ theme(text=element_text(size=18))


dev.off()
#---voilin plot for VARD-GCE--------
#===============================================
rm(list=ls())
data1=read.csv('c:\\dataset\\google experiment feb 2016\\combine node data.csv',header=T)
unique(data1$nodetype) # to check unique values of nodetypes in dataset

node_refs = c("n1Standard1","n1Standard2","n1HighCPU2","n1Standard4","n1HighCPU4","n1HighMEM2")
node_names = c("n1S1","n1S2","n1CPU2","n1S4","n1CPU4","n1mem2")
node_price= c("0.036", "0.071", "0.056", "0.142", "0.118", "0.106")
node_types = data.frame(node_refs,node_names, node_price)
colnames(node_types) <- c("nodetype","NodeName", "NodePrice")
rm(node_refs,node_names, node_price)
data <- merge(data1,node_types)
#setwd('C:\\dataset\\google experiment feb 2016\\barplot')
#setwd('C:\\dataset\\cloudcom2017\\GCE')

data$NodeName <- factor(data$NodeName, levels = data$NodeName[order(data$NodePrice)], )

# Basic violin plot
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='vard-gce-violin.pdf',width=8,height=6)

p <- ggplot(data, aes(x=NodeName, y=ttime), color=NodeName, alpha=0.3) + 
  geom_violin(trim=FALSE)+
  #scale_y_continuous(expand=c(0,0)) + theme_bw()+
  scale_y_continuous(limits = c(50, 250))+theme_bw()+
  xlab("Instance Types") + ylab("Execution Time (seconds)")
p<-p + stat_summary(fun.y=median, geom="point", size=2, color="red")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()
#---voilin plot for smallapt-EC2--------
#===============================================
#-----------------------Smallpt-EC2---------------------------------
rm(list=ls())
setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
exec_times=read.csv("combine-node-data-smallpt.csv",header=T)

node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")
node_price= c("0.026", "0.052", "0.070", "0.140", "0.280", "0.116" ,"0.232", "0.239")
node_types = data.frame(node_refs,node_names, node_price)
colnames(node_types) <- c("nodetype","NodeName", "NodePrice")
rm(node_refs,node_names, node_price)

exec_times <- merge(exec_times,node_types)

exec_times$NodeName <- factor(exec_times$NodeName, levels = exec_times$NodeName[order(exec_times$NodePrice)], )

# Basic violin plot
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='smallpt-ec2-violin.pdf',width=8,height=6)

p <- ggplot(exec_times, aes(x=NodeName, y=ttime), color=NodeName, alpha=0.3) + 
  geom_violin(trim=FALSE)+
  #scale_y_continuous(expand=c(0,0)) + theme_bw()+
  scale_y_continuous(limits = c(0, 7000))+theme_bw()+
  xlab("Instance Types") + ylab("Execution Time (seconds)")
p<-p + stat_summary(fun.y=median, geom="point", size=2, color="red")
p+ theme(legend.position="top", text=element_text(size=18))

dev.off()
#---voilin plot for smallapt-GCE--------
#===============================================
#-----------------------Smallpt-GCE---------------------------------
rm(list=ls())

# setwd('C:\\dataset\\google experiment feb 2016\\smallpt-experiment-July2017')  # for Windows
#exec_times=read.csv("combine-node-data.csv",header=T)

data1=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt-experiment-July2017\\combine-node-data.csv',header=T)
#data1=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt\\combine node data smallpt.csv',header=T)
unique(data1$nodetype) # to check unique values of nodetypes in dataset
node_refs = c("n1Standard1","n1Standard2","n1Standard4", "n1HighCPU2","n1HighCPU4","n1HighCPU8", "n1HighMem2")
node_names = c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4","n1cpu8", "n1mem2")
node_price= c("0.036", "0.071", "0.142","0.056", "0.118","0.215", "0.106")
node_types = data.frame(node_refs,node_names, node_price)
colnames(node_types) <- c("nodetype","NodeName", "NodePrice")
rm(node_refs,node_names)
data <- merge(data1,node_types)

data$NodeName <- factor(data$NodeName, levels = data$NodeName[order(data$NodePrice)], )
#setwd('C:\\dataset\\google experiment feb 2016\\barplot')

#setwd('C:\\dataset\\cloudcom2017\\GCE\\smallpt')

# Basic violin plot
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='smallpt-gce-violin.pdf',width=8,height=6)
p <- ggplot(data, aes(x=NodeName, y=ttime), color=NodeName, alpha=0.3) + 
  geom_violin(trim=FALSE)+
  scale_y_continuous(limits = c(0, 7000))+theme_bw()+
  xlab("Instance Types") + ylab("Execution Time (seconds)")
p<-p + stat_summary(fun.y=median, geom="point", size=2, color="red")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()

#-----a function to split 1st and 3rd quartile data----------------------------
quart <- function(x) {
  x <- sort(x)
  n <- length(x)
  m <- (n+1)/2
  if (floor(m) != m) {
    l <- m-1/2; u <- m+1/2
  } else {
    l <- m-1; u <- m+1
  }
  c(Q1=median(x[1:l]), Q3=median(x[u:n]))
}

#-----scenarion 1-chaged 1 hour to 12---cloudcom17===============
#------------ function to separate quantile values for all the node types--------------
# this code is written for Cloudcom17.....to plot best and worst case of running VARD and smallpt for an hour. the scenarion question is:
# How many number of jobs can run in one hour on each node of amazon ec2 and GCE.
#-----plot quantiles..................................
#------------VARD-EC2---------------------------------------
rm(list=ls())
setwd('C:\\dataset\\cloudcom2017')  # for Windows
exec_times=read.csv("combine-node-data-vard.csv",header=T)

node_refs = c("2.1","2.2","3.1","3.2","4.1","4.2")
node_names = c("t2.small","t2.medium","m3.medium","m3.large","c4.large","c4.xlarge")
node_price= c("0.026", "0.052", "0.070", "0.140", "0.116", "0.232")
node_types = data.frame(node_refs,node_names, node_price)
colnames(node_types) <- c("Nodetype","NodeName", "NodePrice")
rm(node_refs,node_names)

# 12 hours job rns-------
exec_times <- merge(exec_times,node_types)
data=exec_times

#data$NodeName <- factor(data$NodeName, levels = data$NodeName[order(data$NodePrice)], )

data1=data[which(data$NodeName=='t2.small'), ]
first=quantile(data1$Ptime, 0.25)
last=quantile(data1$Ptime, 0.75)
avg=mean(data1$Ptime)
df <- data.frame(Nodename='t2.small', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_12hrjobs_best=(0.026*12), cost_of_12hrjobs_best=(0.026*12), average=avg, node_price=0.026)

data2=data[which(data$NodeName=='t2.medium'), ]
first=quantile(data2$Ptime, 0.25)
last=quantile(data2$Ptime, 0.75)
avg=mean(data2$Ptime)
df<-rbind(df, data.frame(Nodename='t2.medium', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_12hrjobs_best=(0.050*12), cost_of_12hrjobs_best=(0.050*12), average=avg, node_price=0.050))

data3=data[which(data$NodeName=='m3.medium'), ]
first=quantile(data3$Ptime, 0.25)
last=quantile(data3$Ptime, 0.75)
avg=mean(data3$Ptime)
df<-rbind(df, data.frame(Nodename='m3.medium', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_12hrjobs_best=(0.070*12), cost_of_12hrjobs_best=(0.070*12), average=avg, node_price=0.070))


data4=data[which(data$NodeName=='m3.large'), ]
first=quantile(data4$Ptime, 0.25)
last=quantile(data4$Ptime, 0.75)
avg=mean(data4$Ptime)
df<-rbind(df, data.frame(Nodename='m3.large', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_12hrjobs_best=(0.140*12), cost_of_12hrjobs_best=(0.140*12), average=avg, node_price=0.140))


data5=data[which(data$NodeName=='c4.large'), ]
first=quantile(data5$Ptime, 0.25)
last=quantile(data5$Ptime, 0.75)
avg=mean(data5$Ptime)
df<-rbind(df, data.frame(Nodename='c4.large', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_12hrjobs_best=(0.116*12), cost_of_12hrjobs_best=(0.116*12), average=avg, node_price=0.116))

data6=data[which(data$NodeName=='c4.xlarge'), ]
first=quantile(data6$Ptime, 0.25)
last=quantile(data6$Ptime, 0.75)
avg=mean(data6$Ptime)
df<-rbind(df, data.frame(Nodename='c4.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_12hrjobs_best=(0.23*12), cost_of_12hrjobs_best=(0.23*12), average=avg, node_price=0.232))

Job_Runs=c(df$num_of_jobs_1hr_best, df$num_of_jobs_1hr_worst)
#cost_of_12hr=c(df$cost_of_12hrjobs_best)
df$Nodename <- factor(df$Nodename, levels = df$Nodename[order(df$node_price)], )

#-------group barplot---------------------
#df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
 #                 Node_Type=rep(c("t2.small", "t2.medium", "m3.medium", "m3.large", "c4.large", "c4.xlarge"),2),
  #                Job_Runs_1hr=c(37, 45, 17, 42, 46, 50, 35, 42, 17, 31, 36, 38),
   #               cost_of_1hr=c(0.02, 0.05, 0.07, 0.14, 0.11, 0.23))

df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
                  Node_Type=rep(c("t2.small", "t2.medium", "m3.medium", "m3.large","c4.large",  "c4.xlarge"),2),
                  Job_Runs_1hr=Job_Runs,
                  cost_of_12hr=df$cost_of_12hrjobs_best,
                  cost_of_1hr=c(0.026, 0.052, 0.070,  0.140, 0.116,  0.232))

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='vard-ec2-1hr.pdf',width=8,height=6)

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

p<-ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Job_Runs_1hr), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sprintf("$%s", substring(cost_of_12hr, 1, 4))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  #scale_y_continuous(expand=c(0,0))+
  scale_y_continuous(limits = c(0, 700))+ ### adjust ymax value
  #ylim=c(0,50)+
  theme_minimal()
#p + labs(title="VARD-EC2: number of job runs in 1 hr", x="Node Types", y = "Number of job runs in 1 hour")
p<-p + labs(x="Node Types", y = "Number of job runs in 12 hours")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()

library(xlsx)
write.xlsx(df,"C:\\dataset\\cloudcom2017\\vard-EC2-avg.xlsx")

write.table(df, "c:\\dataset\\cloudcom2017\\vard-EC2.txt", sep="\t")

#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#-----------------------Smallpt-EC2---------------------------------
rm(list=ls())
setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)

#exec_times<-rbind(exec_times,node_types)
#exec_times<-cbind(exec_times,node_types)
exec_times <- merge(exec_times,node_types)
data=exec_times


data1=data[which(data$NodeName=='t2.small'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='t2.small', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.026*12), node_price=0.026)

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$NodeName=='t2.medium'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='t2.medium', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.052*12), node_price=0.052))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$NodeName=='m3.medium'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.medium', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.070*12), node_price=0.070))

#t=cbind('m3.medium',first, last)
#table=rbind(table,t)

data4=data[which(data$NodeName=='m3.large'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.large', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.140*12), node_price=0.140))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$NodeName=='m3.xlarge'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.280*12), node_price=0.280))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$NodeName=='c4.large'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.large', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.116*12), node_price=0.116))

data7=data[which(data$NodeName=='c4.xlarge'), ]
first=quantile(data7$ttime, 0.25)
last=quantile(data7$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.232*12), node_price=0.232))

data8=data[which(data$NodeName=='c3.xlarge'), ]
first=quantile(data8$ttime, 0.25)
last=quantile(data8$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c3.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.239*12), node_price=0.239))

Job_Runs=c(df$num_of_jobs_1hr_best, df$num_of_jobs_1hr_worst)
#-------group barplot---------------------

#df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
 #                 Node_Type=rep(c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge"),2),
  #                Job_Runs_1hr=c(0, 1, 1, 4, 8, 4, 9, 8, 0, 1, 1, 4, 8, 4, 9, 8))

df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
                  Node_Type=rep(c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge"),2),
                  Job_Runs_1hr=Job_Runs,
                  cost_of_12hr=df$cost_of_12hrjobs_best,
                  cost_of_1hr=c(0.026, 0.052, 0.070, 0.140, 0.280, 0.116, 0.232, 0.239))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='smallpt-ec2-1hr.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s", substring(cost_of_12hr, 1, 4))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=Job_Runs_1hr), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 250))+
  #ylim=c(0,50)+
  theme_minimal()

#p + labs(title="Smallpt-EC2: number of job runs in 1 hr", x="Node Types", y = "Number of job runs in 1 hour")

p<-p + labs(x="Node Types", y = "Number of job runs in 12 hours")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()

library(xlsx)
write.xlsx(df,"C:\\dataset\\cloudcom2017\\smallpt-EC2.xlsx")

#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#------------VARD-GCE---------------------------------------
rm(list=ls())
exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\combine node data.csv',header=T)
unique(exec_times$nodetype) # to check unique values of nodetypes in dataset

node_refs = c("n1Standard1","n1Standard2","n1HighCPU2","n1Standard4","n1HighCPU4","n1HighMEM2")
node_names = c("n1S1","n1S2","n1CPU2","n1S4","n1CPU4","n1mem2")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)
data <- merge(exec_times,node_types)


data1=data[which(data$nodetype=='n1Standard4'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
avg=mean(data1$ttime)
df <- data.frame(Nodename='n1Standard4', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, average=avg, cost_of_12hrjobs_best=(0.142*12), node_price=0.142)


data2=data[which(data$nodetype=='n1Standard2'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
avg=mean(data2$ttime)
df<-rbind(df, data.frame(Nodename='n1Standard2', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, average=avg, cost_of_12hrjobs_best=(0.071*12), node_price=0.071))


data3=data[which(data$nodetype=='n1HighMEM2'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
avg=mean(data3$ttime)
df<-rbind(df, data.frame(Nodename='n1HighMEM2', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, average=avg, cost_of_12hrjobs_best=(0.106*12), node_price=0.106))


data4=data[which(data$nodetype=='n1HighCPU2'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
avg=mean(data4$ttime)
df<-rbind(df, data.frame(Nodename='n1HighCPU2', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, average=avg, cost_of_12hrjobs_best=(0.056*12), node_price=0.056))


data5=data[which(data$nodetype=='n1HighCPU4'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
avg=mean(data5$ttime)
df<-rbind(df, data.frame(Nodename='n1HighCPU4', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, average=avg, cost_of_12hrjobs_best=(0.118*12), node_price=0.118))


data6=data[which(data$nodetype=='n1Standard1'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
avg=mean(data6$ttime)
df<-rbind(df, data.frame(Nodename='n1Standard1', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, average=avg, cost_of_12hrjobs_best=(0.036*12), node_price=0.036))

Job_Runs=c(df$num_of_jobs_1hr_best, df$num_of_jobs_1hr_worst)

#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
                  Node_Type=rep(c("n1S4","n1S2","n1mem2","n1CPU2","n1CPU4", "n1S1"),2),
                  Job_Runs_1hr=Job_Runs,
                  cost_of_12hr=df$cost_of_12hrjobs_best,
                  cost_of_1hr= c(0.142, 0.071, 0.106, 0.056, 0.118, 0.036))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

#df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
 #                 Node_Type=rep(c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4","n1mem2"),2),
  ##                Job_Runs_1hr=Job_Runs,
    #              cost_of_1hr= c(0.036, 0.071, 0.142, 0.056, 0.118, 0.106))

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='vard-gce-1hr.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Job_Runs_1hr), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sprintf("$%s", substring(cost_of_12hr, 1, 4))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 700))+
  #theme(legend.position="top", text=element_text(size=18))+
  #theme(legend.position="top")+
  #ylim=c(0,50)+
  theme_minimal()
#p<-p + labs(title="VARD-GCE: number of job runs in 1 hr", x="Node Types", y = "Number of job runs in 1 hour")
p<-p + labs(x="Node Types", y = "Number of job runs in 12 hours")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()
library(xlsx)
write.xlsx(df,"C:\\dataset\\cloudcom2017\\vard-GCE-avg.xlsx")

#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#-----------------------Smallpt-GCE---------------------------------
rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
#exec_times=read.csv("combine-node-data-smallpt.csv",header=T)

exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt-experiment-July2017\\combine-node-data.csv',header=T)

#exec_times=read.csv('C:\\dataset\\google experiment feb 2016\\smallpt-experiment-July2017\\combine-node-data.csv')
#exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt\\combine node data smallpt.csv',header=T)

node_refs = c("n1Standard1","n1Standard2","n1Standard4", "n1HighCPU2","n1HighCPU4","n1HighCPU8", "n1HighMem2")
node_names = c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4","n1cpu8", "n1mem2")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)
data <- merge(exec_times,node_types)
#data=exec_times

#node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
#node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")

data1=data[which(data$nodetype=='n1Standard1'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='n1Standard1', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.036*12), node_price=0.036)

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$nodetype=='n1Standard2'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard2', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.071*12), node_price=0.071))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$nodetype=='n1Standard4'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard4', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.142*12), node_price=0.142))


data4=data[which(data$nodetype=='n1HighCPU2'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU2', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.056*12), node_price=0.056))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$nodetype=='n1HighCPU4'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU4', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.118*12), node_price=0.118))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$nodetype=='n1HighCPU8'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU8', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.215*12), node_price=0.215))

data7=data[which(data$nodetype=='n1HighMem2'), ]
first=quantile(data7$ttime, 0.25)
last=quantile(data7$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighMem2', first=first, last=last, num_of_jobs_1hr_best=floor(43200/first), num_of_jobs_1hr_worst=floor(43200/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0, cost_of_12hrjobs_best=(0.106*12), node_price=0.106))


Job_Runs=c(df$num_of_jobs_1hr_best, df$num_of_jobs_1hr_worst)

#-------group barplot---------------------
#df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
 #                 Node_Type=rep(c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4", "n1cpu8","n1mem2"),2),
  #                Job_Runs_1hr=c(40, 33, 31, 31, 30, 34, 39, 26, 30, 28, 28, 28),
   #               cost_of_1hr= c(0.036, 0.071, 0.142, 0.056, 0.118, 0.106))

df2 <- data.frame(case=rep(c("Best", "Worst"), each=7),
                Node_Type=rep(c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4", "n1cpu8","n1mem2"),2),
                Job_Runs_1hr=Job_Runs,
                cost_of_12hr=df$cost_of_12hrjobs_best,
             cost_of_1hr= c(0.036, 0.071, 0.142, 0.056, 0.118, 0.215, 0.106))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='smallpt-gce-1hr.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Job_Runs_1hr), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sprintf("$%s", substring(cost_of_12hr, 1, 4))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 250))+
  #theme(legend.position="top", text=element_text(size=18))+
  #theme(legend.position="top")+
  #ylim=c(0,50)+
  theme_minimal()

p<-p + labs(x="Node Types", y = "Number of job runs in 12 hours")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()
#p + labs(title="Smallpt-GCE: number of job runs in 1 hr", x="Node Types", y = "Number of job runs in 1 hour")

library(xlsx)
write.xlsx(df,"C:\\dataset\\cloudcom2017\\smallpt-GCE.xlsx")

#-----ENDDDDDD -----scenarion 1---cloudcom17===============

#-----scenarion 2---cloudcom17===============
# what is cost of running 1000 jobs on each node of amazon and GCE.
#------------VARD-EC2---------------------------------------
rm(list=ls())
setwd('C:\\dataset\\cloudcom2017')  # for Windows
exec_times=read.csv("combine-node-data-vard.csv",header=T)

node_refs = c("2.1","2.2","3.1","3.2","4.1","4.2")
node_names = c("t2.small","t2.medium","m3.medium","m3.large","c4.large","c4.xlarge")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("Nodetype","NodeName")
rm(node_refs,node_names)

exec_times <- merge(exec_times,node_types)
data=exec_times


data1=data[which(data$NodeName=='t2.small'), ]
first=quantile(data1$Ptime, 0.25)
last=quantile(data1$Ptime, 0.75)
df <- data.frame(Nodename='t2.small', first=first, last=last, time_1000jobs_best=ceiling(first*1000), time_1000jobs_worst=ceiling(last*1000), hours_for_1kjobs_best=ceiling((first*1000)/3600), hours_for_1kjobs_worst=ceiling((last*1000)/3600),cost_of_1kjobs_best=(ceiling((first*1000)/3600) * 0.026), cost_of_1kjobs_worst=(ceiling((last*1000)/3600) * 0.026))



data2=data[which(data$NodeName=='t2.medium'), ]
first=quantile(data2$Ptime, 0.25)
last=quantile(data2$Ptime, 0.75)
df<-rbind(df, data.frame(Nodename='t2.medium', first=first, last=last, time_1000jobs_best=ceiling(first*1000), time_1000jobs_worst=ceiling(last*1000), hours_for_1kjobs_best=ceiling((first*1000)/3600), hours_for_1kjobs_worst=ceiling((last*1000)/3600),cost_of_1kjobs_best=(ceiling((first*1000)/3600) * 0.052), cost_of_1kjobs_worst=(ceiling((last*1000)/3600) * 0.052) ))




data3=data[which(data$NodeName=='m3.medium'), ]
first=quantile(data3$Ptime, 0.25)
last=quantile(data3$Ptime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.medium', first=first, last=last, time_1000jobs_best=ceiling(first*1000), time_1000jobs_worst=ceiling(last*1000), hours_for_1kjobs_best=ceiling((first*1000)/3600), hours_for_1kjobs_worst=ceiling((last*1000)/3600),cost_of_1kjobs_best=(ceiling((first*1000)/3600) * 0.070), cost_of_1kjobs_worst=(ceiling((last*1000)/3600) * 0.070)))



data4=data[which(data$NodeName=='m3.large'), ]
first=quantile(data4$Ptime, 0.25)
last=quantile(data4$Ptime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.large', first=first, last=last, time_1000jobs_best=ceiling(first*1000), time_1000jobs_worst=ceiling(last*1000), hours_for_1kjobs_best=ceiling((first*1000)/3600), hours_for_1kjobs_worst=ceiling((last*1000)/3600),cost_of_1kjobs_best=(ceiling((first*1000)/3600) * 0.140), cost_of_1kjobs_worst=(ceiling((last*1000)/3600) * 0.140)))



data5=data[which(data$NodeName=='c4.large'), ]
first=quantile(data5$Ptime, 0.25)
last=quantile(data5$Ptime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.large', first=first, last=last, time_1000jobs_best=ceiling(first*1000), time_1000jobs_worst=ceiling(last*1000), hours_for_1kjobs_best=ceiling((first*1000)/3600), hours_for_1kjobs_worst=ceiling((last*1000)/3600),cost_of_1kjobs_best=(ceiling((first*1000)/3600) * 0.116), cost_of_1kjobs_worst=(ceiling((last*1000)/3600) * 0.116)))



data6=data[which(data$NodeName=='c4.xlarge'), ]
first=quantile(data6$Ptime, 0.25)
last=quantile(data6$Ptime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.xlarge', first=first, last=last, time_1000jobs_best=ceiling(first*1000), time_1000jobs_worst=ceiling(last*1000), hours_for_1kjobs_best=ceiling((first*1000)/3600), hours_for_1kjobs_worst=ceiling((last*1000)/3600),cost_of_1kjobs_best=(ceiling((first*1000)/3600) * 0.232), cost_of_1kjobs_worst=(ceiling((last*1000)/3600) * 0.232)))

Job_Runs_hours=c(df$hours_for_1kjobs_best, df$hours_for_1kjobs_worst)
cost_1k= c(df$cost_of_1kjobs_best, df$cost_of_1kjobs_worst)
#-------group barplot---------------------
#df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
 #                 Node_Type=rep(c("t2.small", "t2.medium", "m3.medium", "m3.large", "c4.large", "c4.xlarge"),2),
  #                cost_1k_jobs=c(0.70, 1.14, 3.92, 3.36, 2.55, 4.64, 0.72, 1.24, 4.06, 4.48, 3.24, 6.03),
   #               hours_for_1kruns=Job_Runs-hours)

df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
                  Node_Type=rep(c("t2.small", "t2.medium", "m3.medium", "m3.large", "c4.large", "c4.xlarge"),2),
                  cost_1k_jobs=cost_1k,
                  hours_for_1kruns=Job_Runs_hours,
                  cost_of_1hr= c(0.026, 0.052, 0.070, 0.140, 0.116, 0.232))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='vard-ec2-1k-runs.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s",(substring(cost_1k_jobs, 1, 4)))), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=print(paste0(hours_for_1kruns, "h"))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  #scale_fill_brewer(palette="Set1")+
  scale_fill_hue(l=50)+
 # scale_fill_brewer()+
  scale_y_continuous(limits = c(0, 7))+
  #ylim=c(0,50)+
  theme_minimal()
#p + labs(title="VARD-EC2: Cost of running 1000 jobs", x="Node Types", y = "Cost ($)")
p<-p + labs(x="Node Types", y = "Cost of running 1000 jobs ($)")
p+ theme(legend.position="top", text=element_text(size=18))


dev.off()
write.xlsx(df,"C:\\dataset\\cloudcom2017\\vard-EC2-1kjob-cost.xlsx")

#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#-----------------------Smallpt-EC2---------------------------------

rm(list=ls())
setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)

#exec_times<-rbind(exec_times,node_types)
#exec_times<-cbind(exec_times,node_types)
exec_times <- merge(exec_times,node_types)
data=exec_times


data1=data[which(data$NodeName=='t2.small'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='t2.small', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.026), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.026))

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$NodeName=='t2.medium'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='t2.medium', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.052), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.052)))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$NodeName=='m3.medium'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.medium', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.070), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.070)))

#t=cbind('m3.medium',first, last)
#table=rbind(table,t)

data4=data[which(data$NodeName=='m3.large'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.large', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.140), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.140)))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$NodeName=='m3.xlarge'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.xlarge', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.280), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.280)))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$NodeName=='c4.large'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.large', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.116), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.116)))

data7=data[which(data$NodeName=='c4.xlarge'), ]
first=quantile(data7$ttime, 0.25)
last=quantile(data7$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.xlarge', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.232), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.232)))

data8=data[which(data$NodeName=='c3.xlarge'), ]
first=quantile(data8$ttime, 0.25)
last=quantile(data8$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c3.xlarge', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.239), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.239)))


Job_Runs_hours=c(df$hours_for_1kjobs_best, df$hours_for_1kjobs_worst)
cost_1k= c(df$cost_of_1kjobs_best, df$cost_of_1kjobs_worst)

#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
                  Node_Type=rep(c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge"),2),
                  cost_1k_jobs=cost_1k,
                  hours_for_1kruns=Job_Runs_hours,
                  cost_of_1hr=c(0.026, 0.052, 0.070, 0.140, 0.280, 0.116, 0.232, 0.239))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='smallpt-ec2-1k-jobs.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s",(substring(cost_1k_jobs, 1, 4)))), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=print(paste0(hours_for_1kruns, "h"))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  #scale_fill_brewer(palette="Paired")+
  scale_fill_hue(l=50)+
  scale_y_continuous(limits = c(0, 60))+
  #ylim=c(0,50)+
  theme_minimal()
#p + labs(title="smallpt-EC2: Cost of running 1000 jobs", x="Node Types", y = "Cost ($)")
p<-p + labs(x="Node Types", y = "Cost of running 1000 jobs ($)")
p+ theme(legend.position="top", text=element_text(size=18))

dev.off()

write.xlsx(df,"C:\\dataset\\cloudcom2017\\smallpt-EC2-1k.xlsx")

#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#------------VARD-GCE---------------------------------------

rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017')  # for Windows
#exec_times=read.csv("combine-node-data-vard.csv",header=T)
exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\combine node data.csv',header=T)
unique(exec_times$nodetype) # to check unique values of nodetypes in dataset

node_refs = c("n1Standard1","n1Standard2","n1HighCPU2","n1Standard4","n1HighCPU4","n1HighMEM2")
node_names = c("n1S1","n1S2","n1CPU2","n1S4","n1CPU4","n1mem2")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)
data <- merge(exec_times,node_types)


data1=data[which(data$nodetype=='n1Standard4'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='n1Standard4', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.142), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.142))

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$nodetype=='n1Standard2'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.071), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.071)))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$nodetype=='n1HighMEM2'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighMEM2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.106), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.106)))

#t=cbind('m3.medium',first, last)
#table=rbind(table,t)

data4=data[which(data$nodetype=='n1HighCPU2'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.056), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.056)))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$nodetype=='n1HighCPU4'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU4', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.118), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.118)))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$nodetype=='n1Standard1'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1Standard1', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.036), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.036)))

Job_Runs_hours=c(df$hours_for_1kjobs_best, df$hours_for_1kjobs_worst)
cost_1k= c(df$cost_of_1kjobs_best, df$cost_of_1kjobs_worst)

#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
                  Node_Type=rep(c("n1S4","n1S2", "n1mem2", "n1CPU2","n1CPU4","n1S1"),2),
                  cost_1k_jobs=cost_1k,
                  hours_for_1kruns=Job_Runs_hours,
                  cost_of_1hr=c(0.142, 0.071, 0.106, 0.056, 0.118, 0.036))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )

# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity")

# Use position=position_dodge()
ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())

#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='vard-gce-1k-runs.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s", substring(cost_1k_jobs, 1 , 4))), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=print(paste0(hours_for_1kruns, "h"))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  #scale_fill_brewer(palette="Paired")+
  scale_fill_hue(l=50)+
  scale_y_continuous(limits = c(0, 7))+
  #ylim=c(0,50)+
  theme_minimal()
#p + labs(title="VARD-GCE: Cost of 1000 Job Runs", x="Node Types", y = "Cost ($)")
p<-p + labs(x="Node Types", y = "Cost of running 1000 jobs ($)")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()
write.xlsx(df,"C:\\dataset\\cloudcom2017\\VARD-GCE-1k.xlsx")

#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#-----------------------Smallpt-GCE---------------------------------
rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
#exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
#exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt\\combine node data smallpt.csv',header=T)


#node_refs = c("n1Standard1","n1Standard2","n1HighCPU2","n1Standard4","n1HighCPU4","n1HighCPU8")
#node_names = c("n1S1","n1S2","n1CPU2","n1S4","n1CPU4","n1cpu8")

exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt-experiment-July2017\\combine-node-data.csv',header=T)

#exec_times=read.csv('C:\\dataset\\google experiment feb 2016\\smallpt-experiment-July2017\\combine-node-data.csv')
#exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\smallpt\\combine node data smallpt.csv',header=T)

node_refs = c("n1Standard1","n1Standard2","n1Standard4", "n1HighCPU2","n1HighCPU4","n1HighCPU8", "n1HighMem2")
node_names = c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4","n1cpu8", "n1mem2")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)
data <- merge(exec_times,node_types)
#data=exec_times

#node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
#node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")

data1=data[which(data$nodetype=='n1Standard1'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='n1Standard1', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.0475), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.0475))

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$nodetype=='n1Standard2'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.0950), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.0950)))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$nodetype=='n1Standard4'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard4', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.142), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.142)))


data4=data[which(data$nodetype=='n1HighCPU2'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.0709), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.0709)))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$nodetype=='n1HighCPU4'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU4', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.1418), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.1418)))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$nodetype=='n1HighCPU8'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU8', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.2836), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.2836)))

data7=data[which(data$nodetype=='n1HighMem2'), ]
first=quantile(data7$ttime, 0.25)
last=quantile(data7$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighMem2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.106), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.106)))

Job_Runs_hours=c(df$hours_for_1kjobs_best, df$hours_for_1kjobs_worst)
cost_1k= c(df$cost_of_1kjobs_best, df$cost_of_1kjobs_worst)

#-------group barplot---------------------
#df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
 #                 Node_Type=rep(c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge"),2),
  #                cost_1k_jobs=cost_1k,
   #               hours_for_1kruns=Job_Runs_hours)

df2 <- data.frame(case=rep(c("Best", "Worst"), each=7),
                  Node_Type=rep(c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4", "n1CPU8","n1mem2"),2),
                  cost_1k_jobs=cost_1k,
                  hours_for_1kruns=Job_Runs_hours,
                  cost_of_1hr=c(0.036, 0.071, 0.142, 0.056, 0.118, 0.215, 0.106))

df2$Node_Type <- factor(df2$Node_Type, levels = df2$Node_Type[order(df2$cost_of_1hr)], )


#Add labels to a dodged barplot :
setwd('C:\\dataset\\cloudcom2017\\graphs180717')  # for Windows
pdf(file='smallpt-gce-1kruns.pdf',width=8,height=6)
p<-ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s",(substring(cost_1k_jobs, 1, 4)))), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=print(paste0(hours_for_1kruns, "h"))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  #scale_fill_brewer(palette="Paired")+
  scale_fill_hue(l=50)+
  scale_y_continuous(limits = c(0, 60))+
  #ylim=c(0,50)+
  theme_minimal()
#p + labs(title="smallpt-EC2: Cost of running 1000 jobs", x="Node Types", y = "Cost ($)")
p<-p + labs(x="Node Types", y = "Cost of running 1000 jobs ($)")
p+ theme(legend.position="top", text=element_text(size=18))
dev.off()

write.xlsx(df,"C:\\dataset\\cloudcom2017\\smallpt-GCE-1k.xlsx")
#-------ENDDDDDDD-----scenario 2-----

##------Scenraio 2 with movierating----------------------
#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#-----------------------Movierating - EC2---------------------------------
rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
#exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
exec_times=read.csv('c:\\dataset\\Movierating\\7 day experiment-191015\\combine node data-movierat-261015.csv',header=T)
node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)

#exec_times<-rbind(exec_times,node_types)
#exec_times<-cbind(exec_times,node_types)
exec_times <- merge(exec_times,node_types)
data=exec_times


data1=data[which(data$NodeName=='t2.small'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='t2.small', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.026), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.026))

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$NodeName=='t2.medium'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='t2.medium', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.052), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.052)))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$NodeName=='m3.medium'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.medium', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.070), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.070)))

#t=cbind('m3.medium',first, last)
#table=rbind(table,t)

data4=data[which(data$NodeName=='m3.large'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.large', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.140), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.140)))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$NodeName=='m3.xlarge'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.xlarge', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.280), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.280)))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$NodeName=='c4.large'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.large', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.116), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.116)))

data7=data[which(data$NodeName=='c4.xlarge'), ]
first=quantile(data7$ttime, 0.25)
last=quantile(data7$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.xlarge', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.232), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.232)))

data8=data[which(data$NodeName=='c3.xlarge'), ]
first=quantile(data8$ttime, 0.25)
last=quantile(data8$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c3.xlarge', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.239), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.239)))

#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
                  Node_Type=rep(c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge"),2),
                  cost_1k_jobs=c(22.90, 19.39, 30.03, 29.68, 57.96, 24.01, 41.29, 47.08, 27.17, 26.93, 30.31, 29.82, 58.52, 24.24, 41.76, 47.32),
                  hours_for_1kruns=c(881, 371, 429, 212, 207, 207, 178, 197, 1045, 518, 433, 213, 209, 209, 180, 198))

# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity")

# Use position=position_dodge()
ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())

#Add labels to a dodged barplot :
p<-ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s",cost_1k_jobs)), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=print(paste0(hours_for_1kruns, "h"))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  #scale_y_continuous(limits = c(0, 50))+
  #ylim=c(0,50)+
  theme_minimal()
p + labs(title="Movie-EC2: Cost of running 1000 jobs", x="Node Types", y = "Cost ($)")

write.xlsx(df,"C:\\dataset\\cloudcom2017\\Movie-EC2-1kjob-cost.xlsx")



#===========================================
#------------Movierating-GCE-------------------
#================================
rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
#exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\movierating\\combine node data movie-TL.csv',header=T)
node_refs = c("n1Standard1","n1Standard2", "n1Standard4", "n1HighCPU2","n1HighCPU4","n1HighCPU8")
node_names = c("n1S1","n1S2", "n1S4", "n1CPU2","n1CPU4","n1cpu8")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)
data <- merge(exec_times,node_types)
#data=exec_times

#node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
#node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")

data1=data[which(data$nodetype=='n1Standard1'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='n1Standard1', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.0475), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.0475))

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$nodetype=='n1Standard2'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.0950), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.0950)))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$nodetype=='n1Standard4'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df <- rbind(df, data.frame(Nodename='n1Standard4', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.190), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.190)))


data4=data[which(data$nodetype=='n1HighCPU2'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU2', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.0709), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.0709)))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$nodetype=='n1HighCPU4'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU4', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.1418), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.1418)))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$nodetype=='n1HighCPU8'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU8', first=first, last=last, time_1000jobs_best=round(first*1000), time_1000jobs_worst=round(last*1000), hours_for_1kjobs_best=(round((round(first*1000))/3600)), hours_for_1kjobs_worst=(round((round(last*1000))/3600)),cost_of_1kjobs_best=((round((round(first*1000))/3600))*0.2836), cost_of_1kjobs_worst=((round((round(last*1000))/3600))*0.2836)))



#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
                  Node_Type=rep(c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4","n1mem2"),2),
                  cost_1k_jobs=c(10.45, 21.755, 42.94, 16.30, 33.60, 66.64, 10.97, 22.61, 43.70, 16.59, 34.45, 67.21),
                  hours_for_1kruns=c(220, 229, 226, 230, 237, 235, 231, 238, 230, 234, 243, 237))

# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity")

# Use position=position_dodge()
ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())

#Add labels to a dodged barplot :
p<-ggplot(data=df2, aes(x=Node_Type, y=cost_1k_jobs, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=sprintf("$%s", cost_1k_jobs)), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=print(paste0(hours_for_1kruns, "h"))), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  #scale_y_continuous(limits = c(0, 50))+
  #ylim=c(0,50)+
  theme_minimal()

p + labs(title="Movie-GCE: Cost of 1000 Job Runs", x="Node Types", y = "Cost ($)")

write.xlsx(df,"C:\\dataset\\cloudcom2017\\movie-gce-1k.xlsx")

#-------ENDDDDDDD-----scenario 2---movierating-----

##------Scenraio 1 with movierating----------------------
#------------ function to separate quantile values for all the node types--------------
#-----plot quantiles..................................
#-----------------------Movierating - EC2---------------------------------
rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
#exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
exec_times=read.csv('c:\\dataset\\Movierating\\7 day experiment-191015\\combine node data-movierat-261015.csv',header=T)
node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)

#exec_times<-rbind(exec_times,node_types)
#exec_times<-cbind(exec_times,node_types)
exec_times <- merge(exec_times,node_types)
data=exec_times


data1=data[which(data$NodeName=='t2.small'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='t2.small', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0)

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$NodeName=='t2.medium'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='t2.medium', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$NodeName=='m3.medium'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.medium', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('m3.medium',first, last)
#table=rbind(table,t)

data4=data[which(data$NodeName=='m3.large'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.large', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$NodeName=='m3.xlarge'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='m3.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$NodeName=='c4.large'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.large', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

data7=data[which(data$NodeName=='c4.xlarge'), ]
first=quantile(data7$ttime, 0.25)
last=quantile(data7$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c4.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

data8=data[which(data$NodeName=='c3.xlarge'), ]
first=quantile(data8$ttime, 0.25)
last=quantile(data8$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='c3.xlarge', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=8),
                  Node_Type=rep(c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge"),2),
                  Job_Runs_1hr=c(1,2,2,4,4,4,5,5,0,1,2,4,4,4,5,5),
                  cost_of_1hr=c(0.02, 0.05, 0.07, 0.14,0.28, 0.11, 0.23, 0.23))

# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity")

# Use position=position_dodge()
ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())

#Add labels to a dodged barplot :
p<-ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Job_Runs_1hr), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sprintf("$%s", cost_of_1hr)), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  #scale_y_continuous(limits = c(0, 50))+
  #ylim=c(0,50)+
  theme_minimal()

p + labs(title="Movie-EC2: number of job runs in 1 hr", x="Node Types", y = "Number of job runs in 1 hour")

library(xlsx)
#write.xlsx(df,"C:\\dataset\\cloudcom2017\\vard-EC2-avg.xlsx")

write.table(df, "c:\\dataset\\cloudcom2017\\movie-EC2-1hr.txt", sep="\t")


#===========================================
#------------Movierating-GCE-------------------
#================================
rm(list=ls())
#setwd('C:\\dataset\\cloudcom2017\\smallpt-EC2')  # for Windows
#exec_times=read.csv("combine-node-data-smallpt.csv",header=T)
exec_times=read.csv('c:\\dataset\\google experiment feb 2016\\movierating\\combine node data movie-TL.csv',header=T)
node_refs = c("n1Standard1","n1Standard2", "n1Standard4", "n1HighCPU2","n1HighCPU4","n1HighCPU8")
node_names = c("n1S1","n1S2", "n1S4", "n1CPU2","n1CPU4","n1cpu8")
node_types = data.frame(node_refs,node_names)
colnames(node_types) <- c("nodetype","NodeName")
rm(node_refs,node_names)
data <- merge(exec_times,node_types)
#data=exec_times

#node_refs = c("2.1","2.2","3.1","3.2", "3.3", "4.1","4.2", "4.3")
#node_names = c("t2.small","t2.medium","m3.medium","m3.large", "m3.xlarge","c4.large","c4.xlarge", "c3.xlarge")

data1=data[which(data$nodetype=='n1Standard1'), ]
first=quantile(data1$ttime, 0.25)
last=quantile(data1$ttime, 0.75)
df <- data.frame(Nodename='n1Standard1', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0)

#table=cbind('t2.small',first, last)
#table=cbind(table,last)

data2=data[which(data$nodetype=='n1Standard2'), ]
first=quantile(data2$ttime, 0.25)
last=quantile(data2$ttime, 0.75)
#df <- data.frame(Nodename='t2.small', first=first, last=last)
df<-rbind(df, data.frame(Nodename='n1Standard2', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('t2.medium',first, last)
#table=rbind(table,t)

data3=data[which(data$nodetype=='n1Standard4'), ]
first=quantile(data3$ttime, 0.25)
last=quantile(data3$ttime, 0.75)
df <- rbind(df, data.frame(Nodename='n1Standard4', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))


data4=data[which(data$nodetype=='n1HighCPU2'), ]
first=quantile(data4$ttime, 0.25)
last=quantile(data4$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU2', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('m3.large',first, last)
#table=rbind(table,t)

data5=data[which(data$nodetype=='n1HighCPU4'), ]
first=quantile(data5$ttime, 0.25)
last=quantile(data5$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU4', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))

#t=cbind('c4large',first, last)
#table=rbind(table,t)

data6=data[which(data$nodetype=='n1HighCPU8'), ]
first=quantile(data6$ttime, 0.25)
last=quantile(data6$ttime, 0.75)
df<-rbind(df, data.frame(Nodename='n1HighCPU8', first=first, last=last, num_of_jobs_1hr_best=floor(3600/first), num_of_jobs_1hr_worst=floor(3600/last), cost_of_1kjobs_best=0, cost_of_1kjobs_best=0))



#-------group barplot---------------------
df2 <- data.frame(case=rep(c("Best", "Worst"), each=6),
                  Node_Type=rep(c("n1S1","n1S2","n1S4","n1CPU2","n1CPU4","n1mem2"),2),
                  Job_Runs_1hr=c(4,4,4,4,4,4,4,4,4,4,4,4),
                  cost_of_1hr= c(0.0475, 0.0950, 0.190, 0.0709, 0.1418, 0.1184))

# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity")

# Use position=position_dodge()
ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())

#Add labels to a dodged barplot :
p<-ggplot(data=df2, aes(x=Node_Type, y=Job_Runs_1hr, fill=case)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Job_Runs_1hr), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sprintf("$%s", cost_of_1hr)), vjust=-0.2, color="black", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  #scale_y_continuous(limits = c(0, 50))+
  #ylim=c(0,50)+
  theme_minimal()

p + labs(title="Movie-GCE: number of job runs in 1 hr", x="Node Types", y = "Number of job runs in 1 hour")

library(xlsx)
write.xlsx(df,"C:\\dataset\\cloudcom2017\\Movie-GCE-1hr.xlsx")

#-------ENDDDDDDD-----scenario 1---movierating-----

