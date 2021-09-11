#sign test
binom.test(10,25)

#wilcoxon Signed Rank Test
mydat<-attach(Wilcoxon_test_data)

#normal daðilim testi
shapiro.test(dv0)
shapiro.test(dv6)

library(rcompanion)
#verilerimizi transforme etmeye calistik
transformTukey(dv0)
transformTukey(dv6)

#verilerimiz normal dagilmiyor ve transforme edilemiyor. Bu sebeple wilcoxon Signed Rank Test
wilcox.test(dv0, dv6, paired = TRUE)

#grafik cizme islemleri
mynewdata<-data.frame(dv0, dv6)
mynewdata

stackeddata<-stack(mynewdata)
attach(stackeddata)
boxplot(values~ind)


#mann Whitney Wilcoxon Test
notlar<-attach(notlar)
shapiro.test(Matematik)
shapiro.test(Ýngilizce)

transformTukey(Matematik)
transformTukey(Ýngilizce)

wilcox.test(Matematik,Ýngilizce)
boxplot(Matematik,Ýngilizce)

#Kuruskal Wallis Test

attach(OrchardSprays)
shapiro.test(decrease)
transformTukey(decrease)

kruskal.test(decrease~treatment, data = OrchardSprays)
boxplot(decrease~treatment)


install.packages("pgirmess")
library(pgirmess)

kruskalmc(decrease~treatment, OrchardSprays, probs=0.05)

head(Wilcoxon_test_data)
new_notlar<-data.frame(Matematik, Ýngilizce)
head(new_notlar)

head(winequality.red)
head(OrchardSprays)
