# ------------------------------------------------------------------------------
library(Hmisc)
library(moments)
library(corrplot)
library(car)
library(lmtest)
library(QuantPsyc)
# ------------------------------------------------------------------------------

# Duomenų rinkinį '50_Startups.csv' sudaro finansinė 50-ies JAV startuolių
# informacija.

# Duomenų failo (csv formatu) nuskaitymas ir išsaugojimas duomenų lentelėje
# dataset

dataset <- read.csv('data/50_Startups.csv', header=TRUE)
attach(dataset)

# Pirmos penkios duomenų lentelės dataset eilutės

head(dataset)

# Trumpa duomenų lentelelės apžvalga, kurioje apskaičiuojami įvairios kiekvieno
# kintamojo empirinės charakteristikos 

summary(dataset)

# Iš trumpos duomenų apžvalgos matome, jog tarp kintamųjų R.D.Spend ir
# Marketing.Spend yra trūkstamų reikšmių. Trūkstamų reikšmių nustatymui galėsime
# panaudoti daugialypio regresinio modelio prognozes.
# Galime apžvelgti kiekvieno iš kintamųjų vidurkius bei standartinius nuokrypius.
# Taip pat galime pastebėti, jog kintamųjų R.D.Spend ir Marketing.Spend min
# reikšmės yra labai mažos, lyginant su vidurkiu, todėl tai galimos išskirtys
# Norint nustatyti, ar duomenys tikrai yra išskirtys, reikalinga išsamesnė analizė

# Tolimesnei analizei eilutes su trūkstamomis reikšmėmis apdorosime, pakeičiant
# jas atitinkamo kintamojo vidurkio reikšme.

#dataset$R.D.Spend = ifelse(is.na(dataset$R.D.Spend),
#                     ave(dataset$R.D.Spend, FUN = function(x) mean(x, na.rm = TRUE)),
#                     dataset$R.D.Spend)
#dataset$Marketing.Spend = ifelse(is.na(dataset$Marketing.Spend),
#                        ave(dataset$Marketing.Spend, FUN = function(x) mean(x, na.rm = TRUE)),
#                        dataset$Marketing.Spend)

na_index <- which(apply(is.na(dataset), 1, any))
dataset_with_na <- dataset[na_index, ]
dataset_without_na <- dataset[-na_index, ]
data <- dataset_without_na

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Kiekvieno duomenų rinkinyje esančio kintamojo grafinis pavaizdavimas

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------

# Grafinis MTEP išlaidų duomenų pavaizdavimas

# ------------------------------------------------------------------------------


# MTEP išlaidų histograma ir teorinės tankio funkcijos grafikas parodo, jog
# duomenų pasiskirstymas yra panašus į varpo formos pasiskirstymą.

plot.new()
par(mfrow = c(2, 2))

# Histograma
hist(data$R.D.Spend, 
     main = "R&D Spend Distribution",
     xlab = "R&D Spend",
     ylab = "Frequency",
     col = "skyblue", 
     border = "white",
     xlim = c(0, 200000),
     ylim = c(0, 12),
     breaks = seq(0, 170000, 21250),
     labels = TRUE,
     lines(density(data$R.D.Spend))
     )

# ------------------------------------------------------------------------------

# Duomenyse esantiems kintamiesiems apskaičiuosime asimetrijos koeficientą (skew)
# ir ekscesą (kurtosis). Šios charakteristikos mums parodo, ar daug tiriamasis 
# skirstinys skiriasi nuo normaliojo skirstinio su tokiu pačiu vidurkiu ir 
# standartiniu nuokrypiu.

skewness(data$R.D.Spend)
kurtosis(data$R.D.Spend)

# Gautas MTEP išlaidų duomenų asimetrijos koeficientas lygus 0.2. Vadinasi,
# asimetrija yra dešinioji, tačiau ji nežymi, kadangi asimetrijos koeficientas
# artimas 0.
# Gauta MTEP išlaidų duomenų eksceso reikšmė lygi 2.22. Tai reiškia, jog skirstinys
# yra smailesnis, negu normaliosios kreivės.

# ------------------------------------------------------------------------------

# Palyginimui nubrėžkime duomenų teorinės tankio funkcijos ir normaliojo skirstinio
# grafikus.

# Teorinės tankio funkcijos grafikas
plot(density(data$R.D.Spend),
     main = "R&D Spend with Normal Distribution",
     xlab = "R&D Spend",
     ylab = "Density",
     ylim = c(0,0.00001),
     col = "steelblue")

# Normaliojo skirstinio grafikas su tokiu pačiu vidurkiu ir standartiniu nuokrypiu
x <- seq(-75000, 225000, length = 100)
lines(x, dnorm(x, mean = mean(data$R.D.Spend), sd = sd(data$R.D.Spend)),
      col = "red", lwd = 2)

# Legendos pridėjimas dešiniajame apatiniame kampe

legend("bottomright", legend = c("R&D Spend", "Normal Distribution"),
       col = c("steelblue", "red"), lty = c(1, 1), lwd = c(1, 2))

# ------------------------------------------------------------------------------

# Skirstinių normalumui tirti ir vizualizuoti taip pat galime panaudoti dėžinį
# grafiką ir kvantilių grafiką.

# Dėžinis grafikas naudojamas duomenų asimetriškumui (bet ne tankio formai)
# pavaizduoti.

# Dėžinis grafikas
boxplot(data$R.D.Spend, 
        main = "R&D Spend Box Plot",
        ylab = "R&D Spend",
        col = "lightblue",
        border = "darkgrey",
        notch = TRUE,
        horizontal = FALSE)

text(x = 1, y = median(data$R.D.Spend), labels = median(data$R.D.Spend), pos = 3,
     cex=0.8, col = "black")

# Linija, esanti žemiau vidurio, rodo duomenų medianą. Matome, jog
# MTEP išlaidų duomenų medianos linija yra ne per vidurį, tai paaiškina duomenų
# dešininį asimetriškumą.

# Figūrų apatinė ir viršutinė kraštinės rodo duomenų kvartilius, o brūkšniukai
# viršuje ir apačioje rodo trijų standartinių nuokrypių nuo vidurkio ribą.
# Taip pat iš dėžinio grafiko matome, jog duomenyse nėra vizualių išskirčių.
# Išskirčių nebuvimą detaliau patikrinsime truputį vėliau.

# ------------------------------------------------------------------------------

# Teorinių ir MTEP išlaidų duomenų kvantilių grafikas
qqnorm(data$R.D.Spend, 
       main = "QQ-Plot of R&D Spend",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = "steelblue",
       pch = 20,
       cex = 1.5)

# Tiesė, kurioje išsidėstę teoriniai kvantiliai
qqline(data$R.D.Spend, col = "red", lwd = 2)

# Matome, jog MTEP išlaidų duomenų kvantiliai yra išsidėstę gan arti teorinių
# kvantilių tiesės, tačiau ne visi kvantiliai yra artimi tiesei. Kuo duomenys 
# arčiau kraštų, tuo labiau nutolę nuo teorinių kvantilių tiesės. Panašu, jog
# duomenys yra artimi normaliesiems.

# ------------------------------------------------------------------------------

# Patikrinti, ar tiriami duomenys yra iš normaliosios populiacijos galime naudodami
# Šapiro-Vilko testą. Šapiro-Vilko testo nulinė hipotezė teigia, jog duomenys yra
# iš normaliosios populiacijos. Vadinasi, kriterijaus p-reikšmė > 0.05 rodo, kad
# nulinę hipotezę galime priimti ir teigti, jog duomenys turi normalųjį skirstinį.

shapiro.test(data$R.D.Spend)

# Gauta Šapiro-Vilko kriterijaus p-reikšmė lygi 0.322 > 0.05. Vadinasi, 
# galime teigti, jog duomenys yra iš normaliosios populiacijos.

# ------------------------------------------------------------------------------
par(mfrow = c(1, 1))
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Grafinis Administravimo išlaidų duomenų pavaizdavimas
# ------------------------------------------------------------------------------

# Administravimo išlaidų histograma ir teorinės tankio funkcijos grafikas parodo,
# jog duomenų pasiskirstymas yra panašus į varpo formos pasiskirstymą.

par(mfrow = c(2, 2))

# Histograma
hist(data$Administration, 
     main = "Administration Spend Distribution",
     xlab = "Administration Spend",
     ylab = "Frequency",
     col = "skyblue", 
     border = "white",
     xlim = c(0, 250000),
     ylim = c(0, 14),
     #breaks = 10,
     labels = TRUE
)

# ------------------------------------------------------------------------------

# Duomenyse esantiems kintamiesiems apskaičiuosime asimetrijos koeficientą (skew)
# ir ekscesą (kurtosis). Šios charakteristikos mums parodo, ar daug tiriamasis 
# skirstinys skiriasi nuo normaliojo skirstinio su tokiu pačiu vidurkiu ir 
# standartiniu nuokrypiu.

skewness(data$Administration)
kurtosis(data$Administration)

# Gautas Administravimo išlaidų duomenų asimetrijos koeficientas lygus -0.3.
# Vadinasi, asimetrija yra kairioji, tačiau ji nežymi, kadangi asimetrijos
# koeficientas artimas 0.
# Gauta Administravimo išlaidų duomenų eksceso reikšmė lygi 2.96. Tai reiškia,
# jog skirstinys yra smailesnis, negu normaliosios kreivės.

# ------------------------------------------------------------------------------

# Palyginimui nubrėžkime duomenų teorinės tankio funkcijos ir normaliojo skirstinio
# grafikus.

# Teorinės tankio funkcijos grafikas
plot(density(data$Administration),
     main = "Administration Spend with Normal Distribution",
     xlab = "Administration Spend",
     ylab = "Density",
     ylim = c(0,0.000015),
     col = "steelblue")

# Normaliojo skirstinio grafikas su tokiu pačiu vidurkiu ir standartiniu nuokrypiu
x <- seq(-75000, 225000, length = 100)
lines(x, dnorm(x, mean = mean(data$Administration), sd = sd(data$Administration)),
      col = "red", lwd = 2)

# Legendos pridėjimas dešiniajame apatiniame kampe

legend("bottomright", legend = c("Administration Spend", "Normal Distribution"),
       col = c("steelblue", "red"), lty = c(1, 1), lwd = c(1, 2))

# ------------------------------------------------------------------------------

# Skirstinių normalumui tirti ir vizualizuoti taip pat galime panaudoti dėžinį
# grafiką ir kvantilių grafiką.

# Dėžinis grafikas naudojamas duomenų asimetriškumui (bet ne tankio formai)
# pavaizduoti.

# Dėžinis grafikas
boxplot(data$Administration, 
        main = "Administration Spend Box Plot",
        ylab = "Administration Spend",
        col = "lightblue",
        border = "darkgrey",
        notch = TRUE,
        horizontal = FALSE)

text(x = 1, y = median(data$Administration), labels = median(data$Administration),
     pos = 3, cex=0.8, col = "black")

# Administravimo išlaidų duomenų medianos linija yra ganėtinai tiksliai viduryje,
# tai parodo, jog duomenys visgi yra simetriškai pasiskirstę.

# Figūrų apatinė ir viršutinė kraštinės rodo duomenų kvartilius, o brūkšniukai
# viršuje ir apačioje rodo trijų standartinių nuokrypių nuo vidurkio ribą.
# Taip pat iš dėžinio grafiko matome, jog duomenyse nėra vizualių išskirčių.
# Išskirčių nebuvimą detaliau patikrinsime truputį vėliau.

# ------------------------------------------------------------------------------

# Teorinių ir Administravimo išlaidų duomenų kvantilių grafikas
qqnorm(data$Administration, 
       main = "QQ-Plot of Administration Spend",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = "steelblue",
       pch = 20,
       cex = 1.5)

# Tiesė, kurioje išsidėstę teoriniai kvantiliai
qqline(data$Administration, col = "red", lwd = 2)

# Matome, jog Administravimo išlaidų duomenų kvantiliai yra išsidėstę gan arti teorinių
# kvantilių tiesės, tačiau ne visi kvantiliai yra artimi tiesei. Kuo duomenys 
# arčiau kraštų, tuo labiau nutolę nuo teorinių kvantilių tiesės. Panašu, jog
# duomenys yra artimi normaliesiems.

# ------------------------------------------------------------------------------

# Patikrinti, ar tiriami duomenys yra iš normaliosios populiacijos galime naudodami
# Šapiro-Vilko testą. Šapiro-Vilko testo nulinė hipotezė teigia, jog duomenys yra
# iš normaliosios populiacijos. Vadinasi, kriterijaus p-reikšmė > 0.05 rodo, kad
# nulinę hipotezę galime priimti ir teigti, jog duomenys turi normalųjį skirstinį.

shapiro.test(data$Administration)

# Gauta Šapiro-Vilko kriterijaus p-reikšmė lygi 0.711 > 0.05. Vadinasi, 
# galime teigti, jog duomenys yra iš normaliosios populiacijos.

# ------------------------------------------------------------------------------
par(mfrow = c(1, 1))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Grafinis Rinkodaros išlaidų duomenų pavaizdavimas
# ------------------------------------------------------------------------------

# Rinkodaros išlaidų histograma ir teorinės tankio funkcijos grafikas parodo,
# jog duomenų pasiskirstymas yra panašus į varpo formos pasiskirstymą.

par(mfrow = c(2, 2))

# Histograma
hist(data$Marketing.Spend, 
     main = "Marketing Spend Distribution",
     xlab = "Marketing Spend",
     ylab = "Frequency",
     col = "skyblue", 
     border = "white",
     xlim = c(0, 600000),
     ylim = c(0, 10),
     #breaks = 10,
     labels = TRUE
)

# ------------------------------------------------------------------------------

# Duomenyse esantiems kintamiesiems apskaičiuosime asimetrijos koeficientą (skew)
# ir ekscesą (kurtosis). Šios charakteristikos mums parodo, ar daug tiriamasis 
# skirstinys skiriasi nuo normaliojo skirstinio su tokiu pačiu vidurkiu ir 
# standartiniu nuokrypiu.

skewness(data$Marketing.Spend)
kurtosis(data$Marketing.Spend)

# Gautas Marketingo išlaidų duomenų asimetrijos koeficientas lygus -0.009.
# Vadinasi, asimetrijos beveik nėra.
# Gauta Marketingo išlaidų duomenų eksceso reikšmė lygi 2,48. Tai reiškia,
# jog skirstinys yra smailesnis, negu normaliosios kreivės.

# ------------------------------------------------------------------------------

# Palyginimui nubrėžkime duomenų teorinės tankio funkcijos ir normaliojo skirstinio
# grafikus.

# Teorinės tankio funkcijos grafikas
plot(density(data$Marketing.Spend),
     main = "Marketing Spend with Normal Distribution",
     xlab = "Marketing Spend",
     ylab = "Density",
     ylim = c(0,0.0000035),
     col = "steelblue")

# Normaliojo skirstinio grafikas su tokiu pačiu vidurkiu ir standartiniu nuokrypiu
x <- seq(-75000, 700000, length = 100)
lines(x, dnorm(x, mean = mean(data$Marketing.Spend), sd = sd(data$Marketing.Spend)),
      col = "red", lwd = 2)

# Legendos pridėjimas dešiniajame apatiniame kampe

legend("bottomright", legend = c("Marketing Spend", "Normal Distribution"),
       col = c("steelblue", "red"), lty = c(1, 1), lwd = c(1, 2))

# ------------------------------------------------------------------------------

# Skirstinių normalumui tirti ir vizualizuoti taip pat galime panaudoti dėžinį
# grafiką ir kvantilių grafiką.

# Dėžinis grafikas naudojamas duomenų asimetriškumui (bet ne tankio formai)
# pavaizduoti.

# Dėžinis grafikas
boxplot(data$Marketing.Spend, 
        main = "Marketing Spend Box Plot",
        ylab = "Marketing Spend",
        col = "lightblue",
        border = "darkgrey",
        notch = TRUE,
        horizontal = FALSE)

text(x = 1, y = median(data$Marketing.Spend), labels = median(data$Marketing.Spend),
     pos = 3, cex=0.8, col = "black")

# Marketingo išlaidų duomenų medianos linija yra aukščiau vidurio, vadinasi, jog
# duomenų viršūnės asimetrija yra kairioji. 

# Figūrų apatinė ir viršutinė kraštinės rodo duomenų kvartilius, o brūkšniukai
# viršuje ir apačioje rodo trijų standartinių nuokrypių nuo vidurkio ribą.
# Taip pat iš dėžinio grafiko matome, jog duomenyse nėra vizualių išskirčių.
# Išskirčių nebuvimą detaliau patikrinsime truputį vėliau.

# ------------------------------------------------------------------------------

# Teorinių ir Administravimo išlaidų duomenų kvantilių grafikas
qqnorm(data$Marketing.Spend, 
       main = "QQ-Plot of Marketing Spend",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = "steelblue",
       pch = 20,
       cex = 1.5)

# Tiesė, kurioje išsidėstę teoriniai kvantiliai
qqline(data$Marketing.Spend, col = "red", lwd = 2)

# Matome, jog Rinkodaros išlaidų duomenų kvantiliai yra išsidėstę labai arti
# teorinių kvantilių tiesės. Keli kraštiniai duomenys yra šiek tiek nutolę nuo
# teorinių kvantilių tiesės. Duomenys yra artimi normaliesiems.

# ------------------------------------------------------------------------------

# Patikrinti, ar tiriami duomenys yra iš normaliosios populiacijos galime naudodami
# Šapiro-Vilko testą. Šapiro-Vilko testo nulinė hipotezė teigia, jog duomenys yra
# iš normaliosios populiacijos. Vadinasi, kriterijaus p-reikšmė > 0.05 rodo, kad
# nulinę hipotezę galime priimti ir teigti, jog duomenys turi normalųjį skirstinį.

shapiro.test(data$Marketing.Spend)

# Gauta Šapiro-Vilko kriterijaus p-reikšmė lygi 0.912 > 0.05. Vadinasi, labai 
# drąsiai galime teigti, jog duomenys yra iš normaliosios populiacijos.

# ------------------------------------------------------------------------------
par(mfrow = c(1, 1))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Grafinis Pelno išlaidų duomenų pavaizdavimas
# ------------------------------------------------------------------------------

# Pelno išlaidų histograma ir teorinės tankio funkcijos grafikas parodo,
# jog duomenų pasiskirstymas yra panašus į varpo formos pasiskirstymą.

par(mfrow = c(2, 2))

# Histograma
hist(data$Profit, 
     main = "Profit Distribution",
     xlab = "Profit",
     ylab = "Frequency",
     col = "skyblue", 
     border = "white",
     xlim = c(0, 250000),
     ylim = c(0, 12),
     #breaks = 10,
     labels = TRUE
)

# ------------------------------------------------------------------------------

# Duomenyse esantiems kintamiesiems apskaičiuosime asimetrijos koeficientą (skew)
# ir ekscesą (kurtosis). Šios charakteristikos mums parodo, ar daug tiriamasis 
# skirstinys skiriasi nuo normaliojo skirstinio su tokiu pačiu vidurkiu ir 
# standartiniu nuokrypiu.

skewness(data$Profit)
kurtosis(data$Profit)

# Gautas Profit išlaidų duomenų asimetrijos koeficientas lygus 0.43.
# Vadinasi, jog duomenų asimetrija yra dešinioji.
# Gauta Profit išlaidų duomenų eksceso reikšmė lygi 2.50. Tai reiškia,
# jog skirstinys yra smailesnis, negu normaliosios kreivės.

# ------------------------------------------------------------------------------

# Palyginimui nubrėžkime duomenų teorinės tankio funkcijos ir normaliojo skirstinio
# grafikus.

# Teorinės tankio funkcijos grafikas
plot(density(data$Profit),
     main = "Profit with Normal Distribution",
     xlab = "Profit",
     ylab = "Density",
     ylim = c(0,0.000012),
     col = "steelblue")

# Normaliojo skirstinio grafikas su tokiu pačiu vidurkiu ir standartiniu nuokrypiu
x <- seq(-75000, 400000, length = 100)
lines(x, dnorm(x, mean = mean(data$Profit), sd = sd(data$Profit)),
      col = "red", lwd = 2)

# Legendos pridėjimas dešiniajame apatiniame kampe

legend("bottomleft", legend = c("Marketing Spend", "Normal Distribution"),
       col = c("steelblue", "red"), lty = c(1, 1), lwd = c(1, 2))

# ------------------------------------------------------------------------------

# Skirstinių normalumui tirti ir vizualizuoti taip pat galime panaudoti dėžinį
# grafiką ir kvantilių grafiką.

# Dėžinis grafikas naudojamas duomenų asimetriškumui (bet ne tankio formai)
# pavaizduoti.

# Dėžinis grafikas
boxplot(data$Profit, 
        main = "Profit Box Plot",
        ylab = "Profit",
        col = "lightblue",
        border = "darkgrey",
        notch = TRUE,
        horizontal = FALSE)

text(x = 1, y = median(data$Profit), labels = median(data$Profit),
     pos = 3, cex=0.8, col = "black")

# Marketingo išlaidų duomenų medianos linija yra gerokai žemiau vidurio, vadinasi,
# jog duomenų asimetrija yra dešinioji. 

# Figūrų apatinė ir viršutinė kraštinės rodo duomenų kvartilius, o brūkšniukai
# viršuje ir apačioje rodo trijų standartinių nuokrypių nuo vidurkio ribą.
# Taip pat iš dėžinio grafiko matome, jog duomenyse nėra vizualių išskirčių.
# Išskirčių nebuvimą detaliau patikrinsime truputį vėliau.

# ------------------------------------------------------------------------------

# Teorinių ir Administravimo išlaidų duomenų kvantilių grafikas
qqnorm(data$Profit, 
       main = "QQ-Plot of Profit",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = "steelblue",
       pch = 20,
       cex = 1.5)

# Tiesė, kurioje išsidėstę teoriniai kvantiliai
qqline(data$Profit, col = "red", lwd = 2)

# Matome, jog Rinkodaros išlaidų duomenų kvantiliai yra išsidėstę ganėtinai arti
# teorinių kvantilių tiesės, tačiau yra ir šiek tiek nutolusių duomenų. Panašu, 
# jog duomenys yra artimi normaliesiems, tačiau patikrinti, ar duomenys yra iš
# normaliosios populiacijos reikės Shapiro Vilko testo.

# ------------------------------------------------------------------------------

# Patikrinti, ar tiriami duomenys yra iš normaliosios populiacijos galime naudodami
# Šapiro-Vilko testą. Šapiro-Vilko testo nulinė hipotezė teigia, jog duomenys yra
# iš normaliosios populiacijos. Vadinasi, kriterijaus p-reikšmė > 0.05 rodo, kad
# nulinę hipotezę galime priimti ir teigti, jog duomenys turi normalųjį skirstinį.

shapiro.test(data$Profit)

# Gauta Šapiro-Vilko kriterijaus p-reikšmė lygi 0.169 > 0.05. Vadinasi, galime
# teigti, jog duomenys yra iš normaliosios populiacijos.

# ------------------------------------------------------------------------------
par(mfrow = c(1, 1))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Duomenų tarpusavio priklausomybė

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Norint ištirti tiesinę duomenų tarpusavio priklausomybę naudojama Pirsono 
# koreliacijos koeficiento reikšmė. Koreliacijos koeficiento reikšmės yra iš
# intervalo [-1; 1]. Kuo koeficientas artimesnis 0, tuo tiesinis sąryšis tarp
# dviejų kintamųjų silpnesnis.

# Norint pavaizduoti kiekvieno kintamojo tarpusavio koreliaciją patogu naudoti
# koreliacijos matricą.

# Suskaičiuojame koreliacijos matricą
correlation_matrix <- cor(data[, c("Profit", "R.D.Spend", "Administration", "Marketing.Spend")], method = "pearson")

# Nubraižome koreliacijos matricos grafiką
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 0,
         tl.cex = 0.8, tl.offset = 1.2, cl.cex = 0.8, col = colorRampPalette(c("#D3D3D3", "#2C7BB6"))(100),
         addCoef.col = "black", number.digits = 2)

# Pridedame juostinę legendą dešinėje
corrplot::corrplot.mixed(correlation_matrix, number.cex = 0.8, number.digits = 2, 
                         col = colorRampPalette(c("#D3D3D3", "#2C7BB6"))(100), mar = c(1, 1, 1, 1))
par(mar = c(1, 1, 1, 1))

# Iš koreliacijos matricos matome, jog Pelno kintamasis turi stiprų tiesinį sąryšį
# su MTEP ir Rinkodaros išlaidomis. Tiesinis sąryšis tarp pelno ir Administravimo
# išlaidų yra ganėtinai silpnas. MTEP išlaidų kintamasis silpnai koreliuoja su
# Administravimo išlaidų kintamuoju, tačiau turi gan stiprią tiesinę priklausomybę
# su Rinkodaros išlaidomis. Administravimo išlaidos su Rinkodaros išlaidomis 
# beveik nekoreliuoja.

# Duomenų tarpusavio ryšį galime pavaizduoti sklaidos diagrama.
# Grafiškai pavaizduojame Pelno sąryšį su kiekvienu kintamuoju.


par(mar = c(5, 4, 4, 2) + 0.1)

par(mfrow = c(2, 2))

# Scatter plot for R&D Spend
plot(data$R.D.Spend, data$Profit, main = "Profit vs R&D Spend",
     xlab = "R&D Spend", ylab = "Profit", col = "#2C7BB6", pch = 16)

# Scatter plot for Administration
plot(data$Administration, data$Profit, main = "Profit vs Administration",
     xlab = "Administration", ylab = "Profit", col = "#2C7BB6", pch = 16)

# Scatter plot for Marketing Spend
plot(data$Marketing.Spend, data$Profit, main = "Profit vs Marketing Spend",
     xlab = "Marketing Spend", ylab = "Profit", col = "#2C7BB6", pch = 16)

par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Tiesinis daugialypės regresijos modelis

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Atlikome išsamią analizę, kurios metu buvo ištirti duomenų empiriniai įverčiai,
# skirstiniai ir jų charakteristikos bei kintamųjų tarpusavio koreliacija. 
# Remiantis gautais rezultatais, padarėme dvi išvadas, t.y, jog duomenys yra
# paimti iš normaliosios populiacijos ir jog pastebėti tam tikri tiesiniai ryšiai
# tarp kintamųjų. Šios išvados yra naudingos kuriant tiesinį daugialypės regresijos
# modelį, kuris leis prognozuoti startuolio pelną remiantis įvairiomis išlaidomis.

# ------------------------------------------------------------------------------
# Sukuriamas tiesinės daugialypės regresijos modelis
linear_model <- lm(data$Profit ~ data$R.D.Spend + data$Administration + data$Marketing.Spend)
summary(linear_model)
# ------------------------------------------------------------------------------

# Prieš įvertinant modelį, būtina įsitikinti, kad mūsų duomenys atitinka
# tiesinio modelio prielaidas. Todėl šioje dalyje patikrinsime pagrindines 
# tiesinės regresijos modelio prielaidas. Tik įsitikinę, kad mūsų duomenys atitinka
# šias prielaidas, galėsime užtikrinti, jog sukurtas patikimas tiesinės regresijos
# modelis ir jo prognozės yra racionalios.

# Tiesinio modelio prielaidos:
# a) Liekamosios paklaidos turi būti normaliosios. Tai ekvivalentu, kad 
# prognozuojamas kintamasis yra normalusis.
# b) Regresoriai neturi stipriai koreliuoti. Kitaip gali iškilti multikolinearumo
# problema.
# c) Duomenyse neturi būti išskirčių.
# d) Duomenys turi būti homoskedastiški.

# ------------------------------------------------------------------------------

# Pirmoji tiesinio modelio prielaida teigia, jog liekamosios paklaidos turi
# turėti normalųjį skirstinį. Ši prielaida yra ekvivalenti prognozuojamo 
# kintamojo normalumui. Pelnas yra mūsų priklausomas kintamasis, kurį norime
# prognozuoti. Šio kintamojo normalumui tirti naudojome histogramą, teorinį
# tankio grafiką, kvantilių grafiką bei Šapiro-Vilko testą. Padarėme išvadą,
# jog kintamasis yra paimtas iš normaliosios populiacijos. Tai reiškia, jog 
# ir liekamosios paklaidos yra normaliosios. Taigi, pirmoji tiesinio modelio
# prielaida yra tenkinama.

# Galime dar karta įsitikinti, jog liekamosios paklaidos turi normalųjį skirstinį.

# Testas, patikrinti liekamųjų paklaidų normalumui
shapiro.test(linear_model$residuals)

# Gauta Šapiro-Vilko kriterijaus p-reikšmė lygi 0.127 > 0.05. Vadinasi, hipotezė,
# jog liekamosios paklaidos yra iš normaliosios populiacijos yra teisinga.

# ------------------------------------------------------------------------------

# Antroji prielaida sako, jog regresoriai tarpusavyje neturi stipriai koreliuoti.
# Tirdami duomenų tarpusavio priklausomybę nustatėme, jog MTEP išlaidos silpnai
# koreliuoja su Administracijos išlaidomis, tačiau su Rinkodaros išlaidomis
# koreliuoja stipriai. Taip pat nustatėme, jog Administravimo išlaidos su 
# rinkodaros išlaidomis beveik nekoreliuoja.
# Tikslesniam duomenų multikolinearumo įvertinimui pasinaudosime dispersijos
# mazejimo daugikliu VIF. VIF koeficientas skaiciuojamas kiekvienam regresoriui.
# Jei VIF koeficientas > 4, tuomet multikolinearumas tarp tų kintamųjų yra.

# Skaičiuojame dispersijos mažėjimo daugiklį VIF kiekvienam regresoriui 
vif(linear_model)

# Gauname, jog dispersijos mazejimo daugikliai mazesni uz 4 kiekvienam kintamajam,
# todel galime teigti, jog multikolinearumo tarp regresorių nėra.

# ------------------------------------------------------------------------------

# Trečioji prielaida teigia, jog duomenyse negali būti išskirčių. Tai nustatyti
# galime naudojant Kuko matą. Šis matas įvertina, kaip pasikeičia bendras modelio
# koeficiento pokytis pašalinus stebinį. Jis apskaičiuojamas kiekvienam stebiniui.
# Stebinys yra laikomas išskirtimi, jeigu to stebinio Kuko mato reikšmė yra didesnė
# negu 4/n, kur n - tiriamųjų duomenų imties dydis.

cooks_dist <- cooks.distance(linear_model)

# Nubrėžiame grafiką, kuris nurodo, kurių stebinių Kuko mato reikšmė viršija 4/n
par(mfrow = c(1, 1))
plot(cooks_dist,
     main = "Cook's Distance",
     xlab = "Observation Index",
     ylab = "Cook's Distance",
     col = "#2C7BB6",
     pch = 16,
     cex = 1.2)
threshold <- 4 / nrow(data)
abline(h = threshold, col = 'red', lwd = 2, lty = 2)

# Išsaugome indeksus, kurie yra laikomi išskirtimis.
outlier_indexes <- which(cooks_dist >= threshold)

# Išsaugome duomenis be išskirčių.
dataset_without_outliers <- data[-outlier_indexes, ]
data_cut <- dataset_without_outliers 

# Taigi, įvertinę Kuko mato reikšmes gavome, jog trys stebiniai yra išskirtys.
# Pašalinus šias išskirtis duomenys jau tenkins trečiąją tiesinio modelio prielaidą.

# ------------------------------------------------------------------------------

# Ketvirtoji prielaida tvirtina, jog duomenys turi būti homoskedastiški. Šią 
# prielaidą galime patikrinti naudodami Breušo-Pagano kriterijų. Nuline
# Breušo-Pagano kriterijaus hipoteze teigia, jog duomenys homoskedastiski.

bptest(linear_model)

# Gauta p-reiksme 0.783 > 0.05, todel nuline hipoteze galime priimti. Vadinasi, 
# duomenys yra homoskedastiški ir ketvirtoji tiesinio modelio prielaida yra 
# tenkinama.

# ------------------------------------------------------------------------------
# Tiesinio modelio tikslumo įvertinimas
# ------------------------------------------------------------------------------

# Atnaujiname tiesinės daugialypės regresijos modelį naudodami duomenis be
# išskirčių.

linear_model_updated <- lm(data_cut$Profit ~ data_cut$R.D.Spend + data_cut$Administration + data_cut$Marketing.Spend)

# Modelio tikslumas matuojamas R-kvardato rodikliu. Šis dydis parodo kiek procentų
# prognozuojamo kintamojo elgesio lemia regresorių elgesys. Kuo R-kvadrato 
# reikšmė artimesnė 1, tuo geriau modelis aprašo duomenis.

# Koreguotas R-kvadrato rodiklis atsižvelgia į duomenų imties dydį ir 
# nepriklausomų kintamujų modelyje skaičių, todėl geriau parodo modelio tinkamumą
# negu R-kvadrato rodiklis.

# T(Stjudento) kriterijai apskaičiuojami atskiriems regresoriams. Jie padeda
# nuspręsti, ar atitinkamas regresorius šalintinas iš modelio. Jeigu atitinkamo
# kriterijaus p-reikšmė mažesnė už 0.05, tariama, kad regresorius yra statistiškai
# reikšmingas ir (dažniausiai) modelyje yra paliekamas. 

summary(linear_model_updated)

# Atlikę regresijos modelio tikslumo įvertinimą gavome, jog koreguotas R-kvadrato
# rodiklis lygus 0.9626. Vadinasi, tiesinės daugialypės regresijos modelis puikiai
# aprašo duomenis. 
# Galime pastebėti, jog Administravimo išlaidų regresoriaus T(Stjudento) 
# kriterijaus reikšmė lygi 0.386 > 0.05. Vadinasi, daugiklis prie regresoriaus
# yra statistiškai nereikšmingas, todėl šį regresorių galime pašalinti.
 
# Atnaujiname tiesinės daugialypės regresijos modelį pašalinę statistiškai
# nereikšmingą regresorių.

linear_model_final <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = data_cut)

summary(linear_model_final)

# Atlikę regresijos modelio tikslumo įvertinimą gavome, jog koreguotas R-kvadrato
# rodiklis lygus 0.9628, t.y. rodiklis pakito labai nežymiai. Vadinasi, šis
# tiesinės daugialypės regresijos modelis taip pat puikiai aprašo duomenis.

# Visi sukurto modelio regresoriai yra statistiškai reikšmingi. Taip pat, duomenys,
# kurie buvo naudojami kuriant modelį, atitinka tiesinio modelio prielaidas.

# Remiantis turimais duomenimis ir atliktais statistiniais analizės metodais,
# galime užtikrinti, jog sukurtas patikimas tiesinės regresijos
# modelis. Modelis leidžia tiksliai prognozuoti startuolio pelną atsižvelgiant
# į įvairias įmonės išlaidas.

# ------------------------------------------------------------------------------
# Tiesinio modelio interpretacija
# ------------------------------------------------------------------------------

# Turėdami daugialypės regresijos modelį galime sužinoti regresijos funkcijos
# koeficientus.

linear_model_final$coefficients

# Matome, jog koeficientas prie R.D.Spend yra lygus 0.747, o koeficientas prie
# Marketing.Spend lygus 0.037. Galima pamanyti, jog kintamasis R.D.Spend yra
# žymiai labiau statistiškai reikšmingas ir modeliui užtektų tik šio regresoriaus.
# Tam, kad regresoriai rodytų tikslius duomenis, juos reikia standartizuoti.

lm.beta(linear_model_final)

# Gauti modelio su standartizuotais kintamaisiais koeficientai. Galima pastebėti,
# jog kintamasis R.D.Spend iš tikrųjų yra beveik 8 kartus statistiškai reikšmingesnis
# negu Marketing.Spend. 

#produce added variable plots
av_plots <- avPlots(linear_model_final, col = "#2C7BB6", col.lines = 'red', pch = 16)
?avPlots

# ------------------------------------------------------------------------------
# Tiesinio modelio pritaikymas
# ------------------------------------------------------------------------------

# Išbandykime sukurtą daugialypės regresijos modelį su naujais duomenimis.
# Tam sukurkime kelis rinkinius hipotetinių duomenų new_data, kuriuos panaudosime
# pelno prognozei.

new_data <- data.frame(R.D.Spend = c(25000, 73540, 108000),
                       Administration = c(120000, 122500, 67400),
                       Marketing.Spend = c(445350, 238650, 104560))
new_data

# Atliekame naujų duomenų prognozę
predict(linear_model_final, newdata = new_data)

# Hipotetinių duomenų prognozės mums parodo logiškai pagrįstus įverčius.

# ------------------------------------------------------------------------------

# Nubrėšime stebimų ir prognozuojamų reikšmių grafikus. Taip palyginsime modelio
# tiklsumą. 

predict(linear_model_final)
# Prognozuojamų Profit reikšmių grafikas
plot(predict(linear_model_final),
     type = 'b',
     pch = 19,
     col = "red",
     main = "Predicted Results vs. Ground Truth",
     xlab = "Observation",
     ylab = "Profit")

# Pridedame tikrųjų reikšmių įverčius
lines(data_cut$Profit, type = 'b', pch = 19, col = "#2C7BB6")

# Pridedame legendą
legend("topright", legend = c("Predicted", "Ground Truth"), col = c("red", "#2C7BB6"), lty = 1, pch = 19)

# Matome, jog modelio linear_model_final prognozuojamos Profit reikšmės labai
# nedaug skiriasi nuo tikrųjų Profit reikšmių.
     