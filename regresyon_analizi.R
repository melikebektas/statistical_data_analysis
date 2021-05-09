mydata <-stock_price_data_set
attach(mydata)
head(stock_price_data_set)

#bagimli degisken normal dagılım testi
shapiro.test(Stock_Index_Price)
plotNormalHistogram(Stock_Index_Price)

#single lineer regression model
model<-lm(Stock_Index_Price~Interest_rate)

plot(Stock_Index_Price~Interest_rate)
abline(model)

summary(model)

#korelasyon analizi
veri<-glucose_level_data
attach(veri)
head(glucose_level_data)

shapiro.test(Age)
plotNormalHistogram(Age)
shapiro.test(`Glucose Level`)
plotNormalHistogram(`Glucose Level`)

cor.test(Age, `Glucose Level`, method = "pearson")
plot(Age,`Glucose Level`)


#korelasyon power analizi
library(pwr)
pwr.r.test(n = 10, r =0.5357319 , sig.level =0.05 , power =NULL )

#regression power analizi
pwr.f2.test (u =1, v =22, f2 =6.69, sig.level =0.05, power =  NULL)
