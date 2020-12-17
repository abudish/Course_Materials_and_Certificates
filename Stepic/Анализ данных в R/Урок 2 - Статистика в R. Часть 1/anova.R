### ANOVA

library(ggplot2)

# formulae

DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way

DV ~ IV1:IV2  # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures



# reading data

mydata <- read.csv('shops.csv')


# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data=mydata)
summary(fit)


# Two-way ANOVA

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")


# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)


# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения 
# различных удобрений на урожайность гороха (yield). Нашей задачей будет выяснить,
# существенно ли одновременное применение азота (фактор N) и фосфата (фактор P).
# Примените дисперсионный анализ, где будет проверяться влияние
# фактора применения азота (N), влияние фактора применения фосфата (P) и их взаимодействие.
# В ответе укажите p-value для взаимодействия факторов N и P.
fit_npk <- aov(yield ~ N*P, data = npk) 
summary(fit_npk)

# Теперь проведите трехфакторный дисперсионный анализ, где зависимая переменная
# - это урожайность (yield), а три фактора - типы удобрений (N, P, K). 
# После проведения данного анализа вы получите три значения p - уровня значимости
# (о значимости каждого из факторов).
# 
# Соотнесите названия факторов и значения p - уровня значимости.
fit_npk2 <- aov(yield ~ N+P+K, data = npk) 
summary(fit_npk2)

# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)


TukeyHSD(fit5)

# Проведите однофакторный дисперсионный анализ на встроенных данных iris.
# Зависимая переменная - ширина чашелистика (Sepal.Width), независимая переменная
# - вид (Species). Затем проведите попарные сравнения видов. 
# Какие виды статистически значимо различаются по ширине чашелистика (p < 0.05)?
ggplot(iris, aes(x = Species, y =Sepal.Width )) + 
  geom_boxplot()

iris_fit <- aov(Sepal.Width ~ Species, data=iris)
summary(iris_fit)
# Repeated measures
TukeyHSD(iris_fit)


mydata2 <- read.csv('therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)

# В этой задаче вам дан набор данных, в котором представлена информация о 
# температуре нескольких пациентов, которые лечатся разными таблетками и у разных врачей.
# 
# Проведите однофакторный дисперсионный анализ с повторными измерениями: 
# влияние типа таблетки (pill) на температуру (temperature) с учётом испытуемого (patient).
# Каково p-value для влияния типа таблеток на температуру?
#   Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv
# 
# Не забудьте, важно перевести переменную patient в фактор! 
pills <- read.csv("Pillulkin.csv")
pills$patient <- as.factor(pills$patient)
pills_fit <- aov(temperature ~ pill + Error(patient/pill), data = pills)
summary(pills_fit)

# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ 
# с повторными измерениями: влияние факторов doctor, 
# влияние фактора pill и их взаимодействие на temperature.
# Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной
# принимает разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей! 
# Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?
pills_fit2 <- aov(temperature ~ doctor*pill + Error(patient/doctor*pill), data = pills)
summary(pills_fit2)

# Вспомните графики из лекций и дополните шаблон графика в поле для ответа так 
# (не добавляя еще один geom) , чтобы объединить линиями точки, 
# принадлежащие разным уровням фактора supp. 
# Не забудьте подключить нужный для построение графика пакет.
# Пожалуйста, сохраните график в переменную obj.
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
