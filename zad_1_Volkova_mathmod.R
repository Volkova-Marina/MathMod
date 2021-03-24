# Волкова Марина ПАЭ 123, вариант 1- для региона 56 рассчитайте урожайность
# пшеницы в период с 2005 по 2017 год взяв для расчета
# средние суммы активных температур за
# эти годы,с метеостанций на расстоянии от 50 до 250 км
# проверяем рабочую директорию
setwd("D:/mathmod/mathmod_2"); getwd()
# устанавливаем пакеты 
#install.packages("tidyverse")
#install.packages("rnoaa")
# открываем нужные нам пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

## устанавливаем список метеостанций
#station_data = ghcnd_stations()
#write.csv(station_data,file = "station_data.csv")
station_data=read.csv("station_data.csv")

## формируем список метеостанций
# создаем таблицу с именем региона и координатами его столицы
orenburg = data.frame(id="ORENBURG", latitude=51.46, longitude=55.06)
# выбираем метеостанции в фиксированном радиусе от Оренбурга,
# которые имеют необходимые данные за определнный период
orenburg_around = meteo_nearby_stations(lat_lon_df = orenburg, 
                                        station_data = station_data,
                                        limit = 20,
                                        var=c("TAVG"),
                                        year_min = 2005, year_max = 2017)
# получили таблицу где указаны все индетификаторы метеостанций,
# отсортированных по удаленности от Оренбурга
# первый из низ будет индентификатор Оренбурга
# получаем индентификатор метеостанции Оренбурга
orenburg_id=orenburg_around[["ORENBURG"]][["id"]][1]
summary(orenburg_id)
# для получения таблицы со всеми метеостанциями вокруг Оренбурга 
# необходимо выбрать целиком первый объект из списка
orenburg_table=orenburg_around[[1]]
summary(orenburg_table)
# отфильтурем метеостанции в радиусе от 50 до 250 км с помщью комнды фильтр
orenburg_stations=orenburg_table %>%filter(distance>=50&distance<=250)
#мы сформировали список необходимых станций, посмотрим, что он содержит
str(orenburg_stations)
# список содержит 12 метеостанций расположенных в радиусе от 50 до 250 км 
# от Оренбурга
# выведем индетификаторы отфильрованных метеостанций 
orenburg_stations$id
## скачивание погодных данных для наших метеостанций
# чтобы получить вск данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_orenburg_data=meteo_tidy_ghcnd(stationid = orenburg_id)
# посмотрим что мы скачали 
summary(all_orenburg_data)
# создать цикл, в котором бы скачивались  нужные данные для всех метеостанций 
# cоздадим объект, куда скачаем все данные всех метеостанций
all_orenburg_meteodata = data.frame()
# создаем цикл для наших 12 метеостанций
stations_names=orenburg_stations$id
stations_names=stations_names[1:12]

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2005-01-01",
                              date_max = "2017-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_orenburg_meteodata=rbind(all_orenburg_meteodata, one_meteo)}
# записываем полученные результаты 
write.csv(all_orenburg_meteodata,"all_orenburg_meteodata.csv")
# считываем данные all_orenburg_meteodata.csv
#all_orenburg_meteodata=read.csv("all_orenburg_meteodata.csv")
# смотрим что получилось
str(all_orenburg_meteodata)

# добавим год, месяц, день
all_orenburg_meteodata=all_orenburg_meteodata %>% mutate(year=year(date), 
                                                month=month(date), 
                                                day=day(date))
# превратим NA в 0 и где tavg<5 
all_orenburg_meteodata[is.na(all_orenburg_meteodata$tavg),"tavg"] = 0
all_orenburg_meteodata[all_orenburg_meteodata$tavg<5, "tavg"] = 0
summary(all_orenburg_meteodata)

# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам 
# для всех метеостанций
group_meteodata =all_orenburg_meteodata %>% group_by(id,year,month) 
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))
## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) 
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) 
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) 
# отношение числа дней i-го месяца, 
#входящих в период вегетации культуры, к общему 
#числу дней в месяце,константа по табл. 1.
y = 1.0 
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300
# Коэффициент использования ФАР посевом 
Qj = 1600
# калорийность урожая культуры 
Lj = 2.2 
# сумма частей основной и побочной продукции 
Ej = 25 
# стандартная влажность культуры 
# Рассчитаем Fi по месяцаv
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Расчитываем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield
# получаем урожай в ц/га т.к если умножать на 10^6 то получим урожай в мг
# получили 20,2 ц/га