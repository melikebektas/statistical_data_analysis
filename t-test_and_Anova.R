#shapiro test
attach(hyp_exam_scores)
head(hyp_exam_scores)

mean(ExamScores)
shapiro.test(ExamScores)
library(rcompanion)
plotNormalHistogram(ExamScores)

transforme_sinav <- transformTukey(ExamScores)
plotNormalHistogram(transforme_sinav)

#t-test �rnekleri
attach(pendulum)
head(pendulum)
mean(length)
mean(time10periods)

#Normal da��l�m testi (t-test'in 1. �art�)
#verilerimiz normal da��lmal�d�r shapiro.test ile normal da��l�m� kontrol ettik

shapiro.test(length)
shapiro.test(time10periods)
plotNormalHistogram(length)
plotNormalHistogram(time10periods)

#varyans testi (t-test'in 2. �art�)
var.test(length,time10periods)

#two sample t-test
#Varyans testi sonunda p-value=0.003 oldu�undan var.equal=FALSE
t.test(length, time10periods, var.equal = FALSE)
mptest <- pwr.t.test(d =(mean(length)-mean(time10periods))/sqrt(((sd(length)^2+sd(time10periods)^2))/2), power = 0.8, type = "two.sample")
mptest
#one sample t-test
t.test(time10periods, mu=19, alternative = "less")
t.test(time10periods, mu=19.1)

pwr.t.test(n = 11 , d = 0.4, sig.level = NULL, power =0.8 , type = "one.sample")

attach(t_test2)
head(t_test2)
shapiro.test(`SoleMaterial _A`)
shapiro.test(Sole_Material_B)
plotNormalHistogram(`SoleMaterial _A`)
plotNormalHistogram(Sole_Material_B)

var.test(Sole_Material_B, `SoleMaterial _A`) #varyanslar e�it

#two sample paired t-test
t.test(`SoleMaterial _A`, Sole_Material_B, paired = TRUE, var.equal = TRUE)
t.test(length, time10periods, paired = TRUE, var.equal = FALSE)

mptest <- pwr.t.test(d =(mean(`SoleMaterial _A`)-mean(Sole_Material_B))/sqrt(((sd(`SoleMaterial _A`)^2+sd(Sole_Material_B)^2))/2), power = 0.8, type = "paired")
mptest

#anova
attach(FEED_ANOVA)
head(FEED_ANOVA)

shapiro.test(A)
shapiro.test(B)
shapiro.test(C)
shapiro.test(D)

plotNormalHistogram(A)
plotNormalHistogram(B)
plotNormalHistogram(C)
plotNormalHistogram(D)

sd(A)
sd(B)
sd(C)
sd(D)

bpdata<-stack(FEED_ANOVA)
attach(bpdata)
model<-values~ind

#leveneTest yapabilmek i�in car paketini indirdik
install.packages("car")
library(car)
leveneTest(model)

anova<-aov(model)
summary(anova)

boxplot(model)

myTukey<-TukeyHSD(anova)
myTukey
plot(myTukey)

#power analiz i�in pwr paketini indirdik
install.packages("pwr")
library(pwr)

pwr.anova.test(k = 4 , n = 5, f = 0.4, sig.level =NULL , power = 0.8)
