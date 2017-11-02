# При помощи функции ggplot() или boxplot() постройте график boxplot,
# используя встроенные в R данные airquality.
# По оси x отложите номер месяца, по оси y — значения переменной Ozone.
library(ggplot2)
df <- airquality
ggplot(df, aes(x = Month, y = Ozone)) + geom_boxplot()

# На графике boxplot отдельными точками отображаются наблюдения,
# отклоняющиеся от 1 или 3 квартиля больше чем на полтора межквартильных размаха. 
# Сколько таких наблюдений присутствует в сентябре (месяц №9)?
df$Month <- factor(df$Month)
ggplot(df, aes(x = Month, y = Ozone, fill = Month)) + geom_boxplot()




# Используем знакомые нам данные mtcars. 

# Нужно построить scatterplot с помощью ggplot из ggplot2, 
#по оси x которого будет mpg, по оси y - disp, а цветом отобразить переменную (hp).

#Полученный график нужно сохранить в переменную plot1.
plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) + geom_point()



# Укажите, при помощи какого варианта кода мы можем построить следующий график по данным iris:

# Гистограмма распределения переменной Sepal.Length,
# в которой цвет заполнения столбцов гистограммы зависит от значения переменной Species.
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))


# Студент Ярослав очень любит строить графики в R.
# Основываясь на данных iris он хочет построить следующий график:
        # Scatterplot (диаграмма рассеивания),
        #где по оси X будет отложена переменная Sepal.Length,
        #по оси Y переменная  Sepal.Width. 
        #За цвет точек будет отвечать переменная  Species,
        #а за размер точек переменная Petal.Length.

#Ярослав написал следующую команду
ggplot(aes(Sepal.Length, Sepal.Width, col = Species)) +
        
        geom_point(iris, size = Petal.Length)

#Однако построить желаемый график не удается!
#Укажите, какие ошибки совершил Ярослав и попробуйте построить данный график самостоятельно.
ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) + geom_point(aes(size = Petal.Length))
