View(longley)
data("longley")
head(longley,5)
str(longley)
summary(longley)
mod <- lm(formula = Employed~., data=longley)
summary(mod)

plot(longley$Employed,longley$GNP.deflator)
plot(longley$Employed,longley$GNP)
plot(longley$Employed,longley$Unemployed)
plot(longley$Employed,longley$Armed.Forces)
plot(longley$Employed,longley$Population)
plot(longley$Employed,longley$Year)
#### Emplyed vs GNP.Deflator looks like correlation
#### Emplyed vs GNP looks like correlation
#### Emplyed vs Unemployed looks like NO correlation
#### Emplyed vs Armed.Forces looks like a big maybe
#### Emplyed vs Population looks like correlation
#### Emplyed vs year looks like correlation

#### Looks like there are 4

plot(longley$Employed,longley$GNP.deflator)
plot(longley$Employed,longley$GNP)
plot(longley$Employed,longley$Population)
plot(longley$Employed,longley$Year)

lm(longley$Employed ~ longley$GNP.deflator)
lm(longley$Employed ~ longley$GNP)
lm(longley$Employed ~ longley$Population)
lm(longley$Employed ~ longley$Year)
lm(Employed ~., longley)
plot(GNP.deflator~Employed, data=longley)
abline(lm(GNP.deflator~Employed, data=longley))


plot(GNP~Employed, data=longley)
abline(lm(GNP~Employed, data=longley))


plot(Population~Employed, data=longley)
abline(lm(Population~Employed, data=longley))



plot(Year~Employed, data=longley)
abline(lm(Year~Employed, data=longley))


lm(formula = Employed ~ ., data = longley)

#GNP looks to be the best

y <- as.matrix(longley$GNP)
View(y)

x <- as.matrix(longley$Employed)
View(x)

B <- solve(t(x) %*% x) %*% t(x) %*% y
View(B)

lm.mod <- lm(x~y)
lm.mod

fit_1 <- lm(Employed ~ Year, data = longley)
summary(fit_1)

foo <- matrix(longley$GNP)
foo
bar <- matrix(longley$Employed)
bar
results <- foo / bar
results
mean(results) #### 5.875827

X <- cbind(1, matrix(foo))
X
lm(foo ~ bar) #### -1430.48 27.84
