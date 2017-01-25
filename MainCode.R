#Exp[enditure of each country
library(ggplot2)
data1 <- read.csv(file = "expenditure2.csv", h = TRUE)
result <- aggregate(data1$expend, by=list(data1$year, data1$country), 
                    FUN=mean)
p = ggplot(data = result, aes(x = result$Group.2,y = result$x, colour = result$Group.2, fill = Group.1))+
  geom_bar(width=1, stat = "identity")+
  theme(axis.text.x = element_text(angle=50, vjust = 1, hjust=1)) +
  theme(axis.title.y=element_text(angle=90) )+ ggtitle("Country's Expenditure") + xlab("Country") +ylab("Expenditure") 
p

#-------------------------------
#Expenditure vs year
result1 <- aggregate(result$x, by=list(result$Group.1), FUN=sum)
boxplot(result$x~result$Group.1, main = "Expenditure vs Year", xlab = "Year", ylab = "Expenditure", col = c( "cyan","bisque", "lightblue", "lightgreen", "pink", "violet"))

#-------------------------------
#World rank for countries
library(ggplot2)
qual <- read.csv(file = "cwurData.csv", header = TRUE)
exp <- read.csv(file = "education_expenditure_supplementary_data.csv", h = TRUE)

x <- data.frame(data1$world_rank, data1$country)
x <- data.frame(unique(exp$country))
names(x)[1] = paste("country")

y <- aggregate(qual$world_rank, by=list(qual$country), FUN=mean)
names(y)[1] = paste("country")
names(y)[2] = paste("rank")
names(y)
result <- merge(x,y)

p = ggplot(data = result, aes(x = country,y = rank,colour = country, size = -rank))+
  geom_point() +
  theme(axis.text.x = element_text(angle=50, vjust = 1, hjust=1)) +
  theme(axis.title.y=element_text(angle=90) )+ ggtitle("Ranks for countries") + xlab("Country") +ylab("World Rank") 
p

#-------------------------------
#No. of universities in acountry

library(ggplot2)
library(dplyr)
cwur <- read.csv("cwurData.csv")
# data frames with the universities and count them by country
cwurCount <- 
  cwur %>% 
  group_by(country) %>% 
  summarise(count=n())


p1 <- ggplot(cwurCount, 
             aes(x=reorder(country, -count), y=count, fill=country)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(x="Country",y="Count") +
  ggtitle("Countries by number of universities")

p1

#-------------------------------
#Regression on No. of students vs teaching score

data <- read.csv(file = "timesData.csv", h = TRUE)
data1 <- head(data, 1000)
y<- data1$teaching
x <- data1$num_students
r <- lm(y~x)
plot(x, y, abline(lm(y~x)), main = "No. of Students vs Teaching score", xlab = "No. of Students", ylab = "University score for teaching",
     col = "blue")
#--------------------------------------
#Regression of quality vs world rank

data1 <- read.csv(file = "cwurData.csv", h = TRUE)
#data <- subset(data1, data1$year == 2014)
data3 <- head(data1, 100)
x<- data3$quality_of_education
y <- data3$world_rank
r <- lm(y~x)
plot(x, y, abline(lm(y~x)), pch = 5, main = "Quality of education vs World rank of universities", xlab = "Rank for Citations", ylab = "World rank", lwd = 6, cex = 0.5,
     col ="magenta")

#------------------------------------
#Regression of expenditure vs world rank
qual <- read.csv(file = "cwurData.csv", h = TRUE)
exp <- read.csv(file = "expenditure2.csv", h = TRUE)
names(qual)
names(exp)
x <- aggregate(exp$expend, by=list(exp$country), FUN=sum)
names(x)[1] = paste("country")
names(x)[2] = paste("exp")
names(qual)
y <- aggregate(qual$world_rank, by=list(qual$country), FUN=sum)
y
names(y)[1] = paste("country")
names(y)[2] = paste("rank")
names(y)

data4 <- merge(x, y)
x<- data4$exp
y <- data4$rank
r <- lm(y~x)
plot(x, y, abline(lm(y~x)), main = "Amount spent in 1995-2010 vs World rank in 2010 - 2015", xlab = "Expenditure", ylab = "World rank", col = data4$country, lwd = 5, cex = 0.5)

#----------------------------------------
#Country vs Research
library(ggplot2)

data <- read.csv('timesData.csv', sep = ',', h = TRUE)
ggplot(data = data,
       mapping = aes(x = country, y = research, colour = year)) +
  geom_bar(stat = "identity")+
  facet_grid(facets = year ~ .) +
  theme_bw()+ theme(axis.text.x = element_text(angle=60, vjust = 1, hjust=1))+
  theme(axis.title.y=element_text(angle=90))+ggtitle("Country vs Research")


