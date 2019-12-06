marathon <- read.csv("MarathonData.csv")

km4week <- marathon$km4week
marathonTime <- marathon$MarathonTime
speed4week <-marathon$sp4week
diff <- marathon$Wall21
category <- marathon$CATEGORY


plot(diff, marathonTime, pch = as.numeric(category))
legend("bottomright", legend = c("A", "B", "C", "D"), pch = 1:4)

plot(category, diff, xlab = "categories", ylab = "diff")

cat_A <-marathon[category == "A", ]
cat_B <-marathon[category == "B", ]
cat_C <-marathon[category == "C", ]
cat_D <-marathon[category == "D", ]

lm.A <- lm(marathonTime~diff, data = cat_A)
lm.B <- lm(marathonTime~diff, data = cat_B)
lm.C <- lm(marathonTime~diff, data = cat_C)
lm.D <- lm(marathonTime~diff, data = cat_D)

abline(lm.A)
abline(lm.B)
abline(lm.C)
abline(lm.D)

AN1 <- lm(marathonTime~diff*category)
summary(AN1)
AN2 <- lm(marathonTime~diff+category)
summary(AN2)

anova(AN1)
anova(AN2)

var.test(lm.A, lm.B)
var.test(lm.B, lm.C)
var.test(lm.C, lm.D)

model<-lm(marathonTime~diff,data=marathon)
bls1 <- model$coefficients
bls1

summary(model)

abline(bls1,col="blue")
a = min(diff)
b = max(diff)
a1 = bls1[2]*a + bls1[1]
b1 = bls1[2]*b + bls1[1]
plot(diff, marathonTime)
lines(c(a, b), c(a1, b1))

#прогноз відгук
plot(model$fitted.values, marathonTime, xlab="marathon time forecast",ylab="true marathon time")
abline(c(0,1),col="red")

#прогноз залишки
plot(model$fitted.values,model$residuals, xlab="prediction",ylab="residuals")
abline(0,0,col="red")

qqnorm(model$residuals)
qqline(model$residuals,col="red")