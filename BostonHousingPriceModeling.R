#Import Data
library(mlbench)
data(BostonHousing)
help(BostonHousing)
#sanity check
names(BostonHousing)
head(BostonHousing)
tail(BostonHousing)
dim(BostonHousing)

#Distribution of X variables
BostonHousing$chas=as.numeric(BostonHousing$chas)-1
par(mfrow = c(4,4))
for(i in c(1:14))
{
  hist(BostonHousing[,i], xlab = "", main = names(BostonHousing)[i], breaks=50)
}

#Transform Y 
hist(log(BostonHousing[,14]), xlab = "", main = names(BostonHousing)[14], breaks=50)

#Check for collinearity between X variables and linearity between X and Y
pairs(BostonHousing) 
par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[,i], BostonHousing$medv, xlab = names(BostonHousing)[i], ylab = "Median Value of Houses")
}

#Examine the Distribution of Two Groups

par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[,i], m0$res, xlab = names(BostonHousing)[i], ylab = "Residuals")
}


gA=which(BostonHousing$rad==24)
par(mfrow = c(4,4))
for(i in c(1:14))
{
  hist(BostonHousing[gA,i], xlab = "", main = names(BostonHousing)[i], breaks=50)
}

par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[gA,i], BostonHousing$medv[gA], xlab = names(BostonHousing)[i], ylab = "Residuals")
}

par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[-gA,i], BostonHousing$medv[-gA], xlab = names(BostonHousing)[i], ylab = "Residuals")
}

mA = lm(medv~., data = BostonHousing[gA,])
plot(mA$fitted,mA$res)
mB = lm(medv~., data = BostonHousing[-gA,])
plot(mB$fitted,mB$res)

summary(mA)
summary(mB)

#variance test of two groups residuals
var.test(residuals(m0)[BostonHousing$rad==24],residuals(m0)[BostonHousing$rad!=24])

#Diagnostics 
m0 = lm(medv~., data = BostonHousing)
summary(m0)
par(mfrow = c(1, 2))
plot(m0$fitted, m0$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Initial Model")
qqnorm(m0$res)
qqline(m0$res)


##obvious nonlinearity and a downward sloping line --> significant trend, needs transformation 
m1 = lm(log(medv)~., data = BostonHousing)
plot(m1$fitted, m1$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Log Y Model")
summary(m1)
par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[,i], m1$res, xlab = names(BostonHousing)[i], ylab = "Residuals")
}

par(mfrow = c(4, 4))
for(i in 1:13)
{
  prplot(m1,i)
}
qqnorm(m1$res)
qqline(m1$res)

#Transformation of x variables: crim, lstat, dis
m2 = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + log(lstat), data = BostonHousing)
plot(m2$fitted, m2$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model m2")
summary(m2)

par(mfrow = c(4, 4))
for(i in 1:13)
{
  prplot(m2,i)
}

m3 = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat), data = BostonHousing)
plot(m3$fitted, m3$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model m3")
summary(m3)
for(i in 1:13)
{
  prplot(m3,i)
}

#influential observation
m3inf=influence(m3)
plot(m3inf$hat, type = "h")
n = nrow(BostonHousing)
abline(h = 2*14/n)
hi.lev = which(m3inf$hat > (2*14/n))
#standardized predicted residuals 
stud = residuals(m3)/(summary(m3)$sigma*sqrt(1 - m3inf$hat))
qqnorm(stud)
qqline(stud)



#Standardized predicted residuals -> Detecting Outliers
jack = rstudent(m3)
p = 13
tstat=qt(0.05/(n*2), (n- p -2))
which(abs(jack) > abs(tstat))
#372 373 399 401 406 


#Cooks Distance 
cook = cooks.distance(m3)
plot(cook, type = "h")
p=which(cook>0.005) 

rm1=c(413, 416, 417, 419, 420, 427, 467,381,406,419, 369,370,371,372,373)

#re fit model m3 after removal of outliers/influential points 
m3a = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat), data = BostonHousing[-rm1,])
plot(m3a$fitted, m3a$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model m3a")


for(i in 1:13)
{
  prplot(m3a,i)
}

#Examine the Distribution of Two Groups

par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[,i], m3a$res, xlab = names(BostonHousing)[i], ylab = "Residuals")
}


gA=which(BostonHousing$rad==24)
par(mfrow = c(4,4))
for(i in c(1:14))
{
  hist(BostonHousing[gA,i], xlab = "", main = names(BostonHousing)[i], breaks=50)
}

par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[gA,i], BostonHousing$medv[gA], xlab = names(BostonHousing)[i], ylab = "Residuals")
}

par(mfrow = c(4, 4))
for(i in 1:13)
{
  plot(BostonHousing[-gA,i], BostonHousing$medv[-gA], xlab = names(BostonHousing)[i], ylab = "Residuals")
}

mA = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat), data = BostonHousing[gA[-rm1],])
plot(mA$fitted,mA$res, xlab="Fitted Values", ylab="Residuals of Group A")
mB = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat), data = BostonHousing[-gA,])

plot(mB$fitted,mB$res,xlab="Fitted Values", ylab="Residuals of Group B")

summary(mA)
summary(mB)


#Split the data into two groups A and B
class=rep(0,506)
class[gA]=1
bh=data.frame(BostonHousing,class)
m3b = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat) + class + class: (nox + rm + age + log(crim) + log(lstat)), data = bh[-rm1,])
plot(m3b$fitted, m3b$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model m3b")
var.test(residuals(m3b)[BostonHousing$rad==24],residuals(m3b)[BostonHousing$rad!=24])
for(i in 1:13)
{
  prplot(m3b,i)
}



plot(m3b$fitted, m3b$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model of m3b")

cook = cooks.distance(m3b)
plot(cook, type = "h")
r=which(cook>0.02) 
rm2=c(368,410)



#re fitting m3 with more outliers removed 
m3c = lm(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat) + class + class: (nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat)), data = bh[-c(rm1,rm2),])

plot(m3c$fitted, m3c$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model of m3c")

for(i in 1:13)
{
  prplot(m3c,i)
}


cook = cooks.distance(m3c)
plot(cook, type = "h")
which(cook>0.02) 
rm3=c(411,412)

#Model Selection
b1 = regsubsets(log(medv) ~ log(crim) + zn + indus + chas + nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat) + class + class: (nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat)), data = bh[-c(rm1,rm2,rm3),],nvmax=20)
rs1 = summary(b1)

rs1$cp 
rs1$bic #nox,rm,age,log(dis),rad,tax,ptratio,b,log(lstat),rm:class,log(dis):class, rad:class, log(lstat):class
rs$adjr2 

#regression with selected model
m4 = lm(log(medv) ~ nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat) + class + class: (rm + log(dis) + rad + log(lstat)), data = bh[-c(rm1,rm2,rm3),])
summary(m4)

plot(m4$fitted, m4$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model m4")


cook = cooks.distance(m4)
plot(cook, type = "h")
which(cook>0.025) 
rm4=39

#regression with selected model after removing more outliers. 
m5= lm(log(medv) ~ nox + rm + age + log(dis) + rad + tax + ptratio + b + log(lstat) + class + class: (rm + log(dis) + log(lstat)), data = bh[-c(rm1,rm2,rm3,rm4),])
summary(m5)

plot(m5$fitted, m5$res, ylab="Residuals", xlab="Fitted Values", main="Residual Plot of Model m5")
