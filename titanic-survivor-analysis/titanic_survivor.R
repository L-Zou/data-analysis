library(C50)
library(ggplot2)

train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")

test$Survived <- NA 

full <- rbind(train, test)

full$Survived <- as.factor(full$Survived)
full$Pclass <- as.factor(full$Pclass)
full$Name <- as.character(full$Name)
full$Ticket <- as.factor(full$Ticket)

# Exploratory analysis===============
sum(full$Cabin=="") # Missing 1014 - exclude from analysis, too much missing data
sum(is.na(full$Age)) # Missing 263
sum(full$Embarked=="") # Missing 2 

# Replace missing Ages with the average age of passengers
full$Age[is.na(full$Age)] <- mean(full$Age,na.rm=TRUE)
# Plot indicates children are more likely to survive
ggplot(data = full[1:dim(train)[1],][!(is.na(full[1:dim(train)[1],]$Age)),]
       ,aes(x=Age,fill=Survived))+geom_histogram(binwidth =3)

# Replace missing Embarked with the most common Embarked location
count(full, Ticket) # "S" is the most common location
full$Embarked[full$Embarked==""] <- "S"

# Plot indicates relatively similar percentage of survivability, therefore 
# there is unlikely to be a correlation between Embarked and Survival
ggplot(data = full[1:dim(train)[1],],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

# Plot indicates passengers in class 3 are most likely to die
ggplot(data = full[1:dim(train)[1],],aes(x=Pclass,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

# Plot indicates females are most likely to survive
ggplot(data = full[1:dim(train)[1],],aes(x=Sex,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

# Extract Title from Name
full$Title <- regmatches(full$Name, regexpr("([A-z]+\\.)", full$Name))
full$Title <- gsub("\\.", "", full$Title)
full$Title[!(full$Title %in% c("Master", "Miss", "Mr", "Mrs"))] <- "Other"

# Plot indicates that the Title of a passenger can affect their survival
ggplot(data = full[1:dim(train)[1],],aes(x=Title,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

full$Title <- as.factor(full$Title)

# Extract Family_Size from SibSp and Parch
full$Family_Size <- full$SibSp + full$Parch + 1

# Plot indicates that the Family_Size of a passenger can affect their survival
ggplot(data = full[1:dim(train)[1],],aes(x=Family_Size,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

# Extract how many passengers share a Ticket 
# Shared ticket suggests such passengers travelled together, affecting their chance of 
# survival the same way Family_Size does
shared_tickets <- count(full, Ticket)
full$Shared_Tickets <- shared_tickets$n[match(full$Ticket, shared_tickets$Ticket)]
ggplot(data = full[1:dim(train)[1],],aes(x=Shared_Tickets,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

# Plot indicates that Fare affects passenger survival
# The more a passenger paid the more likely they are to survive
ggplot(data = full[1:dim(train)[1],][!(is.na(full[1:dim(train)[1],]$Age)),]
       ,aes(x=Fare,fill=Survived))+geom_histogram(binwidth =3)

# Need to adjust passenger fare after learning some passengers shared tickets
# Passenger_Fare is how much each passenger paid
full$Passenger_Fare <- full$Fare / full$Shared_Tickets

# Therefore, the relevant variables are:
# Pclass, Sex, Age, Family_Size, Share_Tickets, Passenger_Fare

# Generate C5.0 Decision Tree Model===============
# Create training and test dataset
train <- filter(full, !(is.na(Survived)))
test <- filter(full, is.na(Survived))

# Generating model
C5.0_model <- C5.0(train[, c("Pclass", "Age", "Family_Size", "Sex", "Title", 
                             "Shared_Tickets", "Passenger_Fare")], train[, "Survived"])

# Prediction===============
test$Survived <- predict(C5.0_model, test)

# Submission===============
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit,"Submission.csv",row.names=FALSE)
# Final accuracy score: 0.79425

