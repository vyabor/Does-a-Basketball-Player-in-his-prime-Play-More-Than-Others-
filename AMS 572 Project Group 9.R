# Reading in the data
data = read.csv('C:/Users/vyabo/OneDrive/AMS 572/Project/2019-2020 Regular Season NBA Stats.csv', header=T)
head(data)

# Creating the age groups for the analysis
agegroup1 = subset(data,AGE <=21 & AGE >=18)
agegroup2 = subset(data,AGE <=23 & AGE >=22)
agegroup3 = subset(data,AGE <=25 & AGE >=24)
agegroup4 = subset(data,AGE <=27 & AGE >=26)
agegroup5 = subset(data,AGE <=31 & AGE >=28)
agegroup6 = subset(data,AGE >=32)

# Creating the categorical variables by age group
r1 <- rep('18 to 21',length(agegroup1$MIN))
r2 <- rep('22 to 23',length(agegroup2$MIN))
r3 <- rep('24 to 25',length(agegroup3$MIN))
r4 <- rep('26 to 27',length(agegroup4$MIN))
r5 <- rep('28 to 31',length(agegroup5$MIN))
r6 <- rep('32 and over',length(agegroup6$MIN))

# Creating a data frame to use in the one-way ANOVA test
df <- data.frame(c(r1,r2,r3,r4,r5,r6),c(agegroup1$MIN,agegroup2$MIN,agegroup3$MIN,agegroup4$MIN,agegroup5$MIN,agegroup6$MIN))
colnames(df) <- c('age','min')

# Checking for homogeneity of variance
bartlett.test(df$min~df$age)

# Creating the one-way ANOVA table
fit3 <- aov(df$min~factor(df$age))
summary(fit3)

# Scheffe Test to determine which means were different
library(DescTools)
ScheffeTest(fit3)
plot(ScheffeTest(fit3),yaxt='n')
axis(side=2, at=c(1:15))

# Pooled variance t-tests to determine which age groups had greater minutes
t.test(agegroup4$MIN, agegroup2$MIN, var.equal = T, alternative = 'greater')
t.test(agegroup5$MIN, agegroup2$MIN, var.equal = T, alternative = 'greater')
t.test(agegroup5$MIN, agegroup3$MIN, var.equal = T, alternative = 'greater')

# Subsetting the data into the response and predictor varibles for multiple regression
min <- data$MIN
pts <- data$PTS
fgm <- data$FGM
ast <- data$AST
stl <- data$STL

# Creating the regression model and plotting relevant plots for checking assumptions
mult1 <- lm(min~pts+fgm+ast+stl+pts*fgm)
plot(mult1)

# Generating table for regression to determine significance of each predictor variable on the response
summary(mult1)

# Best subsets regression method to determine the optimal regression equation
library(leaps)
leap <- regsubsets(MIN~PTS+FGM+AST+STL+PTS*FGM,data=data)
summary(leap)
plot(leap,scale='Cp')

# Random sample of 80% of original data
adjdata <- data[sample(nrow(data),0.8*nrow(data)),]
nrow(adjdata)

# Subsetting the adjusted data into response and predictor variables
adjmin <- adjdata$MIN
adjpts <- adjdata$PTS
adjfgm <- adjdata$FGM
adjast <- adjdata$AST
adjstl <- adjdata$STL

# Creating the regression model and plotting relevant plots for checking assumptions
mult2 <- lm(adjmin~adjpts+adjfgm+adjast+adjstl+adjpts*adjfgm)
plot(mult2)


# Generating table for regression to determine significance of each predictor variable on the response
summary(mult2)

# Best subsets regression method to determine the optimal regression equation
leap2 <- regsubsets(MIN~PTS+FGM+AST+STL+PTS*FGM,data=adjdata)
summary(leap2)

plot(leap2,scale='Cp')

