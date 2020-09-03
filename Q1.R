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

gender <- c("Female","Female","Male","Male")
income_level <- c("High Income", "Low Income", "High Income", "Low Income")
amount_spent <- c(mean(dt$AmountSpent[dt$Gender == "Female" & dt$Salary >median(dt$Salary)] ),
                mean(dt$AmountSpent[dt$Gender == "Female" & dt$Salary <=median(dt$Salary)]  ),
                mean(dt$AmountSpent[dt$Gender == "Male" & dt$Salary >median(dt$Salary) ] ),
                mean(dt$AmountSpent[dt$Gender == "Male"& dt$Salary <= median(dt$Salary) ] ))

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


dt$Salary[dt$Salary > median(dt$Salary)]
















  
