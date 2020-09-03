getwd()
setwd("C:/Users/Mohammad/Desktop")

dt <- read.table("C:/Users/Mohammad/Desktop/DirectMarketing.csv", header = T, sep = ",")
head(dt) 
tail(dt)
dt$Age <- factor(dt$Age)
dt$Gender <- factor(dt$Gender)
dt$OwnHome <- factor(dt$OwnHome)
dt$Married <- factor(dt$Married)
dt$Location <- factor(dt$Location)
dt$History <- factor(dt$History)
summary(dt)
print(sum(dt$Gender == "Female") / (nrow(dt) - sum(is.na(dt$Gender)))) * 100

print(sum(dt$Gender == "Male" & dt$Married == "Married" & dt$Salary > 50000, na.rm = T)/sum(dt$Gender == "Male" & dt$Married == "Married", na.rm = T) * 100)

library("ggplot2")

age_groups <- table(dt$Age)
age_groups <- as.data.frame(age_groups)
colnames(age_groups) <- c("age", "count")
age_groups <- age_groups[order(age_groups$count),]

ggplot(age_groups, aes(x= reorder(age, count), count))+
  geom_bar(stat = "identity")+
  ggtitle("Age Distribution")+
  xlab("Age Category")+
  ylim(0,600)+ 
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
  )



result_1 <- as.character(age_groups$age)
dt$Age <- factor(dt$Age, levels = result_1)
ggplot(dt, aes(Age, fill = Gender))+
  geom_bar()+
  ggtitle("Distribution by Age and Gender")+
  xlab("Age Category")+
  ylim(0,600)+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )+
  scale_fill_manual(values = c("Green","Red"))
  


ggplot(dt, aes(x=AmountSpent))+
  geom_histogram(aes(y=..density..),binwidth = 100, color="black", fill = "white")+
  geom_density(alpha = 0.35, fill="#FF6633")+
  stat_function(fun=dexp, geom = "line", size=1.5, col="blue", args = (mean=1/mean(dt$AmountSpent)))


boxplot(AmountSpent~Catalogs, data= dt, main="AmountSpent and #of Catalogs ", xlab = "# of Catalogs", ylab = "AmountSpent")


ggplot(dt,aes(x=Salary,y=AmountSpent))+
  geom_point()+
  geom_smooth(method = lm,size=2)+
  ggtitle("AmountSpent by Salary")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )

ggplot(dt, aes(Gender, AmountSpent, fill=Gender))+
  geom_boxplot()+
  ggtitle("AmountSpent by Gender")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )


mean(dt$AmountSpent[dt$Gender == "Male"])
mean(dt$AmountSpent[dt$Gender == "Female"])

mean(dt$Salary[dt$Gender == "Male"])
mean(dt$Salary[dt$Gender == "Female"])

ggplot(dt, aes(Gender, Salary, fill=Gender))+
  geom_boxplot()+
  ggtitle("Salary by Gender")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )

dt$incomelevel <- ifelse(dt$Salary > median(dt$Salary), "High Income","Low Income")

ggplot(dt, aes(x= incomelevel, y=AmountSpent, group=Gender, colour = Gender)) +
  geom_line() +
  geom_boxplot()


q <- quantile(dt$AmountSpent, c(0.25,0.5,0.75))
f_high <- quantile(dt$AmountSpent[dt$Gender == "Female" & dt$Salary >median(dt$Salary)], c(0.25,0.5,0.75) )
f_low <- quantile(dt$AmountSpent[dt$Gender == "Female" & dt$Salary <=median(dt$Salary)], c(0.25,0.5,0.75)  )
m_high <- quantile(dt$AmountSpent[dt$Gender == "Male" & dt$Salary >median(dt$Salary) ], c(0.25,0.5,0.75) )
m_low <- quantile(dt$AmountSpent[dt$Gender == "Male"& dt$Salary <= median(dt$Salary) ], c(0.25,0.5,0.75) )



gender <- c("Female","Female","Male","Male")
income_level <- c("High Income", "Low Income", "High Income", "Low Income")
amount_spent <- c(f_high[2],
                  f_low[2],
                  m_high[2],
                  m_low[2])

d <- data.frame(gender, income_level, amount_spent)
d

#1
ggplot(d, aes(x= gender, y=amount_spent, group=income_level, colour = income_level)) +
  geom_line() +
  geom_point(shape = 1 , size =3)+
  ggtitle("amount_spent by Gender by decrease effect of salary")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )
#2
ggplot(d, aes(x= income_level, y=amount_spent, group=gender, colour = gender)) +
  geom_line() +
  geom_point(shape = 1 , size =3)+
  ggtitle("amount_spent by income by decrease effect of salary")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )
#3
gender_high <- dt$Gender[dt$Salary >median(dt$Salary)]
as_high <- dt$AmountSpent[dt$Salary >median(dt$Salary)]

d_3 <- data.frame(gender_high,as_high)
d_3


ggplot(d_3, aes(x= gender_high, y= as_high, colour = gender_high))+
  geom_boxplot()+
  ggtitle("high salary box plot")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )

#4
gender_low <- dt$Gender[dt$Salary <=median(dt$Salary)]
as_low <- dt$AmountSpent[dt$Salary <=median(dt$Salary)]

d_3 <- data.frame(gender_low,as_low)
d_3


ggplot(d_3, aes(x= gender_low, y= as_low, colour = gender_low))+
  geom_boxplot()+
  ggtitle("low salary box plot")+
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold"),
    axis.text.x=element_text(size=rel(2)),
    axis.text.y=element_text(size=rel(2)),
    legend.title = element_text( size = 20),
    legend.text = element_text( size =20)
    
  )



















  
