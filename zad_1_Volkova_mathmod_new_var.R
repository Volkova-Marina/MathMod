# Волкова Марина ПАЭ 123, вариант 1- для региона 2 рассчитайте урожайность 
# пшеницы в 2016 году взяв для расчета
# средние суммы активных температур за
# предыдущие 12 лет с 16 ближайших метеостанций
# но убирая из рассчета активных температур дни с температурой
# выше 30 градусов
# проверяем рабочую директорию
setwd("D:/mathmod/mathmod_4"); getwd()
# устанавливаем пакеты 
#install.packages("tidyverse")
#install.packages("rnoaa")
# открываем нужные нам пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

## устанавливаем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,file = "station_data.csv")
station_data=read.csv("station_data.csv")

#После получения всписка всех станций, получаем список станций ближайших 
# к столице нашего региона,
#создав таблицу с именем региона и координатами его столицы
ufa = data.frame(id="UFA", latitude=54.44, longitude=55.58)
ufa_around = meteo_nearby_stations(lat_lon_df = ufa, 
                                        station_data = station_data,
                                        limit = 16,
                                        var=c("TAVG"),
                                        year_min = 2003, year_max = 2015)

#ufa_around это список единственным элементом которого является таблица,
# содержащая идентификаторыметеостанций отсортированных по их 
# удалленности от Уфы, 
# очевидно что первым элементом таблицы будет идентификатор метеостанции
# Уфы,его то мы и попытаемся получить
ufa_id=ufa_around[["UFA"]][["id"]][1]
summary(ufa_id)
# для получения таблицы со всеми метеостанциями вокруг Уфы 
# необходимо выбрать целиком первый объект из списка
ufa_table=ufa_around[[1]]
summary(ufa_table)
# в таблице ufa_table оказалось 16 объектов, ранжированных по расстоянию от Уфы
ufa_stations=ufa_table 
#мы сформировали список необходимых станций, посмотрим, что он содержит
str(ufa_stations)
# список содержит 16 метеостанций расположенных вблизи Уфы 
# выведем индетификаторы отфильрованных метеостанций 
ufa_stations$id
## скачивание погодных данных для наших метеостанций
# чтобы получить вск данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_ufa_data=meteo_tidy_ghcnd(stationid = ufa_id)
# посмотрим что мы скачали 
summary(all_ufa_data)
# создать цикл, в котором бы скачивались  нужные данные для всех метеостанций 
# cоздадим объект, куда скачаем все данные всех метеостанций
all_ufa_meteodata = data.frame()
# создаем цикл для наших 16 метеостанций
stations_names=ufa_stations$id
stations_names=stations_names[1:16]

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2003-01-01",
                              date_max = "2015-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_ufa_meteodata=rbind(all_ufa_meteodata, one_meteo)}
# записываем полученные результаты 
write.csv(all_ufa_meteodata,"all_ufa_meteodata.csv")
# считываем данные all_ufa_meteodata.csv
all_ufa_meteodata=read.csv("all_ufa_meteodata.csv")
# смотрим что получилось
str(all_ufa_meteodata)

# добавим год, месяц, день
all_ufa_meteodata=all_ufa_meteodata %>% mutate(year=year(date), 
                                                         month=month(date), 
                                                         day=day(date))
# превратим NA в 0 и где tavg<5>30 
all_ufa_meteodata[is.na(all_ufa_meteodata$tavg),"tavg"] = 0
all_ufa_meteodata[all_ufa_meteodata$tavg<5, "tavg"] = 0
all_ufa_meteodata[all_ufa_meteodata$tavg>30, "tavg"] = 0
summary(all_ufa_meteodata)

# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам 
# для всех метеостанций
group_meteodata =all_ufa_meteodata %>% group_by(id,year,month) 
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
# получили 15.7 ц/га