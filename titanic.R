titanic=read.csv("titanic.csv")
head(titanic)
drop <- c("Name")
titanic<-titanic[ , !(names(titanic) %in% drop)]

#survived and GENDER _______________________________________________________
#plot(titanic$Survived,titanic$Gender)
barplot(table(titanic$Gender), xlab="Gender", ylab="Frequency", 
        main="Gender Distribution",ylim=c(0,600),col = "aquamarine")
titanic$Gender = as.integer(titanic$Gender)
cor(titanic$Survived,titanic$Gender)
chisq.test(titanic$Survived, titanic$Gender)

#using titanic_temp for graphs and visualisation
titanic_temp=titanic
#SURVIVED AND AGE---------------------------------------------------------------
plot(titanic$Survived,titanic$Age,xlab='Survived',ylab='Age')
#convert age into nominal data
titanic_temp$Age[titanic_temp$Age <= 18] = "minor"
titanic_temp$Age[(titanic_temp$Age > 18) & (titanic_temp$Age <= 65) & (titanic_temp$Age != "minor")] = "adult"
titanic_temp$Age[(titanic_temp$Age != "minor") & (titanic_temp$Age != "adult")] = "senior"
titanic_temp$Age = as.factor(titanic_temp$Age)
table(titanic_temp$Age)
hist(as.numeric(titanic_temp$Age), xlab='Age', main="Age", ylab="", 
     col="lightgreen", ylim=c(0,720))
legend("topright",legend=c("1:Adult","2:Minor","3:Senior"))
#titanic_temp$Age = as.integer(titanic$Age)
cor(titanic$Survived,titanic$Age)

#survived and FARE ______________________________________________________
plot(titanic$Fare, titanic$Survived, xlab = 'Fare', ylab = 'Age')
hist(titanic$Fare, main = "Fare Per Person", xlab = "Fare", col = "grey", breaks = 40, 
     xlim = c(0,300),ylim=c(0,350))
cor(titanic$Survived,titanic$Fare)

#survived and PCLASS________________________________________________
barplot(table(titanic$Pclass), xlab="Class", ylab="Frequency", 
        main="Passenger Class Distribution",col = "brown")
cor(titanic$Survived,titanic$Pclass)
chisq.test(titanic$Survived, titanic$Pclass)

#survived and SIBLING..__________________________________________________
cor(titanic$Survived,titanic$Siblings.Spouses.Aboard)

#survived and PARENTS
cor(titanic$Survived,titanic$Parents.Children.Aboard)

#classifier
LogModel1 = glm(Survived~Age+factor(Gender)+factor(Pclass)+Siblings.Spouses.Aboard
                +Parents.Children.Aboard+Fare, data = titanic, family = binomial)
summary(LogModel1)
step(LogModel1)
LogModel2 = glm(Survived ~ Age+factor(Gender)+Pclass+Siblings.Spouses.Aboard, data = titanic, family = binomial)
summary(LogModel2)

library(caret)
test= titanic[sample(nrow(titanic), 200), ]

#predict and accuracy model1
result1 <- predict(LogModel1,newdata=test,type='response')
result1 <- ifelse(result1 > 0.5,1,0)
confusionMatrix(data=factor(result1), reference=factor(test$Survived))

#predict and accuracy model2
result2 <- predict(LogModel2,newdata=test,type='response')
result2 <- ifelse(result2 > 0.5,1,0)
confusionMatrix(data=factor(result2), reference=factor(test$Survived))

