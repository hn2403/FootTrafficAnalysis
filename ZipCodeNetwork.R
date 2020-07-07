# This is R code for my project "An Analysis of Foot Traffic Data in Chicago During the Shelter-In-Place Order by Zip Code"
# Author: Hieu Nguyen and Sherry Liu


library("dplyr")
library(stringr)
library(tidyr)
library(purrr)
library("ggplot2")
library("reshape2")
source("kIC.R")
library("textir")
require(data.table)

ch_week1 <- read.csv("/Users/hieunguyen/Desktop/Spring 2020/BUSN 41201/Data/Covid19/Weekly Pattern/ch_week1.csv")

# remove NA observations
ch_week1 <- na.omit(ch_week1)

# Exploratory Data Analyses
# Check for outliers
par(mfrow=c(1,2))
boxplot(ch_week1$distance_from_home, ylab="distance_from_home",
        col="red")

ch_week1 <- ch_week1 %>%
  filter(distance_from_home < 600000) # only look at point-of-interests with the distance_from_home<600km

boxplot(log(ch_week1$distance_from_home), ylab="log(distance_from_home)",
        main="distance_from_home < 600km",col="red")

# Aggregate the data by Zip code
ch_week1_eda <- ch_week1 %>%
  group_by(postal_code) %>%
  summarise(num=n(),
            all_visits=sum(raw_visit_counts),
            all_visitors=sum(raw_visitor_counts),
            avg_dist_from_home=mean(distance_from_home),
            avg_median_dwell=mean(median_dwell))

# Plot the histograms
par(mfrow=c(2,3))
hist(ch_week1_eda$num, main = "Histogram of Point-of-Interest", 
     xlab = "count of Point-of-Interest", breaks = 50, 
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$all_visits,  main = "Histogram of Visits", 
     xlab = "number of visits", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$all_visitors,  main = "Histogram of Vsitors", 
     xlab = "number of vsitors", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$avg_dist_from_home,  main = "Histogram of average distance from home", 
     xlab = "average distance from home", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$avg_median_dwell,  main = "Histogram of average median dwell", 
     xlab = "average median dwell", breaks = 50,
     col= c("red","yellow","green","pink"))

knitr::kable(summary(ch_week1_eda[c(3,4,5,6)]))


# Plot the histograms after log transformations 
par(mfrow=c(2,3))
hist(ch_week1_eda$num, main = "Histogram of Point-of-Interest", 
     xlab = "count of Point-of-Interest", breaks = 50, 
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$all_visits),  main = "Histogram of Visits", 
     xlab = "log(number of visits)", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$all_visitors),  main = "Histogram of Vsitors", 
     xlab = "log(number of vsitors)", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$avg_dist_from_home),  main = "Histogram of average distance from home", 
     xlab = "log(average distance from home)", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$avg_median_dwell),  main = "Histogram of average median dwell", 
     xlab = "log(average median dwell)", breaks = 50,
     col= c("red","yellow","green","pink"))



## Scatter plots of the main variables

plot(ch_week1_eda[c(3,4)],pch=19,col=4,cex=0.2)

plot(ch_week1_eda[c(4,5)],pch=19,col=4,cex=0.2, ylim =c(0,50000), xlim = c(0,4000))

plot(ch_week1_eda[c(4,6)],pch=19,col=4,cex=0.2)

plot(ch_week1_eda[c(5,6)],pch=19,col=4,cex=0.2) 



# Clean the visits_by_day variable
ch_week1$temp <- ch_week1$visits_by_day %>%
  str_sub(2,-2) 

ch_week1 <- ch_week1 %>%
  separate(temp, into = c("Monday", "Tuesday", "Wednesday", "Thursday",
                          "Friday","Saturday", "Sunday"), convert = TRUE)

ch_week1_sub <- ch_week1 %>%
  group_by(postal_code) %>%
  summarise(Mon_visits=sum(Monday),
            Tue_visits=sum(Tuesday),
            Wed_visits=sum(Wednesday),
            Thu_visits=sum(Thursday),
            Fri_visits=sum(Friday),
            Sat_visits=sum(Saturday),
            Sun_visits=sum(Sunday))


# Histograms for raw visits counts by day of the week
par(mfrow=c(3,3))
hist(ch_week1_sub$Mon_visits, breaks = 50, main="Visits on Monday",xlab="",
     col= c("red","yellow","green","pink"))
hist(ch_week1_sub$Tue_visits,breaks = 50, main="Visits on Tuesday",xlab="",
     col= c("red","yellow","green","pink"))
hist(ch_week1_sub$Wed_visits,breaks = 50, main="Visits on Wednesday",xlab="",
     col= c("red","yellow","green","pink"))
hist(ch_week1_sub$Thu_visits,breaks = 50, main="Visits on Thursday",xlab="",
     col= c("red","yellow","green","pink"))
hist(ch_week1_sub$Fri_visits,breaks = 50, main="Visits on Friday",xlab="",
     col= c("red","yellow","green","pink"))
hist(ch_week1_sub$Sat_visits, breaks = 50, main="Visits on Saturday",xlab="",
     col= c("red","yellow","green","pink"))
hist(ch_week1_sub$Sun_visits, breaks = 50, main="Visits on Sunday",xlab="",
     col= c("red","yellow","green","pink"))

knitr::kable(summary(ch_week1_sub[-1]))


# histgram of log transformation of visits by day of the week
par(mfrow=c(3,3))
hist(log(ch_week1_sub$Mon_visits), breaks = 50, main="Log(Visits) on Monday",xlab="",
     col= c("red","yellow","green","pink"))
hist(log(ch_week1_sub$Tue_visits),breaks = 50, main="Log(Visits) on Tuesday",xlab="",
     col= c("red","yellow","green","pink"))
hist(log(ch_week1_sub$Wed_visits),breaks = 50, main="Log(Visits) on Wednesday",xlab="",
     col= c("red","yellow","green","pink"))
hist(log(ch_week1_sub$Thu_visits),breaks = 50, main="Log(Visits) on Thursday",xlab="",
     col= c("red","yellow","green","pink"))
hist(log(ch_week1_sub$Fri_visits),breaks = 50, main="Log(Visits) on Friday",xlab="",
     col= c("red","yellow","green","pink"))
hist(log(ch_week1_sub$Sat_visits), breaks = 50, main="Log(Visits) on Saturday",xlab="",
     col= c("red","yellow","green","pink"))
hist(log(ch_week1_sub$Sun_visits), breaks = 50, main="Log(Visits) on Sunday",xlab="",
     col= c("red","yellow","green","pink"))

# Correlation Plot
library(corrplot)
corrplot(cor(ch_week1_sub[-1]), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

cor(ch_week1_sub[-1])



# k-means clustering
#First we re-scale the variables
xch_week1_eda <- ch_week1_eda %>%
  mutate_at(c(3,4,5,6), funs(c(scale(.))))

kfit <- lapply(2:100, function(k) kmeans(xch_week1_eda[c(3,4,5,6)],k))
source("kIC.R")
kaicc <- sapply(kfit,kIC)
which.min(kaicc)
kbic <- sapply(kfit,kIC,"B")
which.min(kbic)

## plot AICc and BIC:
par(mfrow=c(1,2))
plot(2:100, kaicc, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
plot(2:100, kbic, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2, col=4)
abline(v=which.min(kbic),col=4)

grp = kmeans(x=xch_week1_eda[c(3,4,5,6)], centers=which.min(kaicc), nstart=20)
#plot(grp,pch=19,col=4,cex=0.2)
grp
grp$cluster

## Scatter plot with cluster labels
par(mfrow=c(2,2))

plot(xch_week1_eda[c(3,5)], type="n")

text(x=as.numeric(unlist(xch_week1_eda[,3])), y=as.numeric(unlist(xch_week1_eda[,5])), labels=xch_week1_eda$postal_code, col=rainbow(which.min(kaicc))[grp$cluster], cex = 0.5 )

plot(xch_week1_eda[c(3,6)], type="n")

text(x=as.numeric(unlist(xch_week1_eda[,3])), y=as.numeric(unlist(xch_week1_eda[,6])), col=rainbow(which.min(kaicc))[grp$cluster], cex = 0.5 )

plot(xch_week1_eda[c(3,4)], type="n")

text(x=as.numeric(unlist(xch_week1_eda[,3])), y=as.numeric(unlist(xch_week1_eda[,4])), col=rainbow(which.min(kaicc))[grp$cluster], cex = 0.5 )

plot(xch_week1_eda[c(5,6)], type="n")

text(x=as.numeric(unlist(xch_week1_eda[,5])), y=as.numeric(unlist(xch_week1_eda[,6])), col=rainbow(which.min(kaicc))[grp$cluster], cex = 0.5 )



# 4.2 Principle Component Analysis

rownames(poi_merge)<- poi_merge$postal_code
corrplot(cor(poi_merge[,-1]), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)

pcpoi <-prcomp(poi_merge[,-1],scale=TRUE)
plot(pcpoi, main = "", col=c("red","yellow","green","pink"))
mtext(side=1, "Principle Components",  line=1, font=2)

zpoi <-predict(pcpoi)
summary(pcpoi)

loadings <-round(pcpoi$rotation,2)
loadings[,1:3]

hist(loadings[,1], main="", xlab="1st Principle Component -Loadings",
     col=8, border=grey(.9), breaks = 25)

loadings[order(abs(loadings[,1]), decreasing=TRUE),1]

zpoi[order(zpoi[,1])[1:5],1]
zpoi[order(-zpoi[,1])[1:5],1]




# Zip code network
# Clean the visitor_home variable
keeps <- c("safegraph_place_id", "postal_code", "poi_cbg", "visitor_home_cbgs")
ch_week1_network <- subset(ch_week1, select = keeps)

ch_week1_network$visitor_home_cbgs <- ch_week1_network$visitor_home_cbgs %>%
  str_sub(2,-2) 

ch_week1_network$visitor_home_cbgs <- gsub(":\\d+","",ch_week1_network$visitor_home_cbgs)
ch_week1_network$visitor_home_cbgs <- gsub('"',"",ch_week1_network$visitor_home_cbgs)

library(splitstackshape)
ch_week1_network <- cSplit(ch_week1_network, "visitor_home_cbgs", sep=",")

ch_week1_network <- melt(ch_week1_network, id.vars = colnames(ch_week1_network)[1:3],
                         measure.vars = colnames(ch_week1_network)[4:dim(ch_week1_network)[2]], na.rm = TRUE, variable.name = "source", value.name = "visitor_home_cbgs")



netw2 <- ch_week1_network

for (j in 1:nrow(netw2)){
  if (netw2$visitor_home_cbgs[j]  %in%  unique(netw2$poi_cbg)){
    netw2$keep[j] <- netw2$visitor_home_cbgs[j]
  }else {
    netw2$keep[j] <- NA
  }
}


sum(is.na(netw2$keep)) 

netw3 <- na.omit(netw2)

netw3$temp <- substr(netw3$postal_code,1,1) 

netw3 <-netw3 %>%
  filter(temp==6) # get rid of IN

netw3$tempp <- substr(netw3$visitor_home_cbgs,1,11) 

# Network graph for poi_cbg and visitor_home_cbgs
edgemat <- cbind(as.character(netw3$poi_cbg),as.character(netw3$visitor_home_cbgs))
cbglink <- graph.edgelist(edgemat)

V(cbglink)$color = "pink"

V(cbglink)$frame.color = 0

V(cbglink)$label.color = "black"

plot(cbglink, edge.arrow.width=0, edge.curved=FALSE, vertex.label=NA, vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75)


# Network graph for POI postal code and visitor home postal code
library("readxl")
zip_cbg_map <- read_excel("ZIP_TRACT_032020.xlsx")
zip_cbg_map <- zip_cbg_map[,1:2]
colnames(zip_cbg_map)[2] <- "tempp"
zip_cbg_map$temp <- substr(zip_cbg_map$ZIP,1,1) 
zip_cbg_map <-zip_cbg_map %>%
  filter(temp==6) %>%
  select(-temp)


netw4<- merge(netw3,zip_cbg_map, by= "tempp",all=FALSE, sort = FALSE,
              allow.cartesian=TRUE)

edgemat1 <- cbind(as.character(netw4$postal_code), as.character(netw4$ZIP))
ziplink <- graph.edgelist(edgemat1)

V(ziplink)$color = "pink"

V(ziplink)$frame.color = 0

V(ziplink)$label.color = "black"

plot(ziplink, edge.arrow.width=0, edge.curved=FALSE, vertex.label=NA, vertex.frame.color=0, vertex.size=6, 
     edge.arrow.width=.75, main="Network for zip code of POI and visitor's home")


# airport 60666
# Neighborhoods for 60666 at orders 1
nei1<-graph.neighborhood(ziplink, 1, V(ziplink)["60666"])[[1]]
V(nei1)$color <- "gold"
V(nei1)["60666"]$color <- "red"
plot(nei1, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Chicago O'Hare International Airport at order 1")

length(V(nei1)) #263

# Neighborhoods for 60666 at orders 2
nei2<-graph.neighborhood(ziplink, 2, V(ziplink)["60666"])[[1]]
V(nei2)$color <- "green"
sav <- V(nei1)$name
V(nei2)[sav]$color <- "gold"
V(nei2)["60666"]$color <- "red"
plot(nei2, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Chicago O'Hare International Airport at order 2")

length(V(nei2))

# Neighborhoods for 60666 at orders 3
nei3<-graph.neighborhood(ziplink, 3, V(ziplink)["60666"])[[1]]
V(nei3)$color <- "blue"
V(nei3)[sav]$color <- "gold"
sav2 <- V(nei2)$name
V(nei3)[sav2]$color <- "green"
V(nei3)["60666"]$color <- "red"
plot(nei3, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Chicago O'Hare International Airport at order 3")

length(V(nei3))

# downtown 60611 Magnificent Mile
# Neighborhoods for 60611 at orders 1
nei1<-graph.neighborhood(ziplink, 1, V(ziplink)["60611"])[[1]]
V(nei1)$color <- "gold"
V(nei1)["60611"]$color <- "red"
plot(nei1, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Magnificent Mile at order 1")

length(V(nei1))

# Neighborhoods for 60611 at orders 2
nei2<-graph.neighborhood(ziplink, 2, V(ziplink)["60611"])[[1]]
V(nei2)$color <- "green"
sav <- V(nei1)$name
V(nei2)[sav]$color <- "gold"
V(nei2)["60611"]$color <- "red"
plot(nei2, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Magnificent Mile at order 2")

length(V(nei2))

# Neighborhoods for 60611 at orders 3
nei3<-graph.neighborhood(ziplink, 3, V(ziplink)["60611"])[[1]]
V(nei3)$color <- "blue"
V(nei3)[sav]$color <- "gold"
sav2 <- V(nei2)$name
V(nei3)[sav2]$color <- "green"
V(nei3)["60611"]$color <- "red"
plot(nei3, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Magnificent Mile at order 3")

length(V(nei3))


# University of Chicago 60637 
# Neighborhoods for 60637 at orders 1
nei1<-graph.neighborhood(ziplink, 1, V(ziplink)["60637"])[[1]]
V(nei1)$color <- "gold"
V(nei1)["60637"]$color <- "red"
plot(nei1, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for University of Chicago at order 1")

length(V(nei1)) 

# Neighborhoods for 60637 at orders 2
nei2<-graph.neighborhood(ziplink, 2, V(ziplink)["60637"])[[1]]
V(nei2)$color <- "green"
sav <- V(nei1)$name
V(nei2)[sav]$color <- "gold"
V(nei2)["60637"]$color <- "red"
plot(nei2, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for University of Chicago at order 2")

length(V(nei2))

# Neighborhoods for 60637 at orders 3
nei3<-graph.neighborhood(ziplink, 3, V(ziplink)["60637"])[[1]]
V(nei3)$color <- "blue"
V(nei3)[sav]$color <- "gold"
sav2 <- V(nei2)$name
V(nei3)[sav2]$color <- "green"
V(nei3)["60637"]$color <- "red"
plot(nei3, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for University of Chicago at order 3")

length(V(nei3))


# Calculate degree and betweeness
code_degree <- degree(ziplink)
order_degree <- order(code_degree, decreasing = T) 
code_degree[order_degree[1:10]]
par(mfrow=c(1,1))
barplot(code_degree[order_degree[1:10]], main = "Degree of Zip Codes",
        xlab = "zip code", ylab = "degree", 
        col = c("red","yellow","green","pink"))

mb<-betweenness(ziplink)
order_mb <- order(mb, decreasing = T) 
mb[order_mb[1:10]]
barplot(mb[order_mb[1:10]], main = "Betweeness of Zip Codes",
        xlab = "zip code", ylab = "betweeness",
        col = c("red","yellow","green","pink"))




# Does the number of visits predict covid-19 cases?
# case data
cases <- read.csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")
cases <- na.omit(cases)
colnames(cases)[1] <- "postal_code"

cases_after1week <- cases%>%
  filter(Week.Start=="04/12/2020" & postal_code!="Unknown")

cases_after2week <- cases%>%
  filter(Week.Start=="04/19/2020" & postal_code!="Unknown")

cases_after3week <- cases%>%
  filter(Week.Start=="04/26/2020" & postal_code!="Unknown")

poi_merge <- merge(ch_week1_eda,ch_week1_sub)

case_merge_after1week <- merge(poi_merge, cases_after1week)
case_merge_after2week <- merge(poi_merge, cases_after2week)
case_merge_after3week <- merge(poi_merge, cases_after3week)


case1 <- case_merge_after1week[,c(1:13,17,18,27,28,31)]
rownames(case1)<- case1$postal_code
case1 <- case1[,-1]

case2 <- case_merge_after2week[,c(1:13,17,18,27,28,31)]
rownames(case2)<- case2$postal_code
case2 <- case2[,-1]


case3 <- case_merge_after3week[,c(1:13,17,18,27,28,31)]
rownames(case3)<- case3$postal_code
case3 <- case3[,-1]


# Lagged 1 week
ols_data1 <- case1[,c(1:5,13,17)]
colnames(ols_data1)[6] <- "WeeklyCases"
hist(ols_data1$WeeklyCases)
hist(log(ols_data1$WeeklyCases))

reg1 <- glm(log(WeeklyCases) ~ log(all_visits)  + log(avg_dist_from_home) + log(avg_median_dwell) + Population, data = ols_data1)
summary(reg1)


# Lagged 2 week
ols_data2 <- case2[,c(1:5,13,17)]
colnames(ols_data2)[6] <- "WeeklyCases"
hist(ols_data2$WeeklyCases)
hist(log(ols_data2$WeeklyCases))

reg2 <- glm(log(1+WeeklyCases) ~ log(all_visits)  + log(avg_dist_from_home) + log(avg_median_dwell) + Population, data = ols_data2)
summary(reg2)



# Lagged 3 week
ols_data3 <- case3[,c(1:5,13,17)]
colnames(ols_data3)[6] <- "WeeklyCases"
hist(ols_data3$WeeklyCases)
hist(log(ols_data3$WeeklyCases))

reg3 <- glm(log(1+WeeklyCases) ~ log(all_visits)  + log(avg_dist_from_home) + log(avg_median_dwell) + Population, data = ols_data3)
summary(reg3)

library(stargazer)
stargazer(reg1, reg2, reg3, title="Results", align=TRUE)




