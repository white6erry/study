# 이동평균법
vts<-c(1325, 1353, 1305, 1275, 1210, 1195)
mean(vts)
# 가중 이동평균법
mean(vts[1:5])*.4 + vts[6]*.6
mean(vts[1:3])*.4 + vts[4:6]*.6
mean(vts[1:3])*.3 + vts[4:6]*.7
mean(vts[1:3])*.2 + vts[4:6]*.8
# 가중 이동평균법
(vts[4]*4)+(vts[3]*3)+(vts[2]*2)+(vts[1]*1)/(4+3+2+1)

(((((((((1325*0.4)+(1353*0.6))*0.4)+(1305*.6))*0.4)+(1275*0.6))*0.4)+(1210*0.6))*0.4)+(1195*0.6)

install.packages("TTR")
library(TTR)
data(ttrc)
class(ttrc)
str(ttrc)

# Simple moving average
sma.20<-SMA(ttrc[,"Close"], 20)
# Exponential moving average
ema.20<-EMA(ttrc[,"Close"], 20)
# Weighted moving average
wma.20<-WMA(ttrc[,"Close"], 20)

# 주기성을 가진 데이터
# 진폭과 주기
# 진폭(amplitude)
amp.1<-2
amp.2<-2
amp.3<-5
amp.4<-5

# 주기(wave-length, cycle)
wav.1<-1
wav.2<-2
wav.3<-3
wav.4<-7

x<-seq(-2*pi, 2*pi, 0.1)

signal.1<-amp.1*sin(wav.1*x)
signal.2<-amp.2*sin(wav.2*x)
signal.3<-amp.3*sin(wav.3*x)
signal.4<-amp.4*sin(wav.4*x)

par(mfrow=c(1,4))
plot(x, signal.1, type='l', ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.2, type='l', ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.3, type='l', ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.4, type='l', ylim=c(-5,5)); abline(h=0, lty=3)

# 
a<-ts(1:20, frequency=12, start=c(2011,3))
print(a)
str(a)
attributes(a)
kings<-scan("C:\\Users\\acorn\\Downloads\\데이터\\kings.dat")
class(kings)
kingstimeseries<-ts(kings)
births<-scan("C:\\Users\\acorn\\Downloads\\데이터\\nybiths.dat")
birthstimeseries<-ts(births, frequency=12, start=c(1946,1))
birthstimeseries
souvenir<-scan("C:\\Users\\acorn\\Downloads\\데이터\\fancy.dat")
souvenirtimeseries<-ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries
plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
logsouvenirtimeseries<-log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
par(mfrow=c(1,2))
kingsSMA3<-SMA(kingstimeseries, n=3)
plot.ts(kingsSMA3)
kingsSMA8<-SMA(kingstimeseries, n=8)
plot.ts(kingsSMA8)

birthstimeseriescomponents<-decompose(birthstimeseries)
names(birthstimeseriescomponents)
birthstimeseriescomponents$random
birthstimeseriescomponents$figure
birthstimeseriescomponents$trend
birthstimeseriescomponents$seasonal
birthstimeseriescomponents$type
plot(birthstimeseriescomponents)

birthstimeseriesseasonallyadjusted<- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

rain<-scan("C:\\Users\\acorn\\Downloads\\데이터\\rain.dat")
rainseries<-ts(rain, start=c(1813))
plot.ts(rainseries)
rainseriesforecasts<-HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
rainseriesforecasts$fitted
plot(rainseriesforecasts)
rainseriesforecasts$SSE
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)

myts<-ts(birth, start=c(1946,1), frequency=12)
myts2<-window(myts, start=c(1946,6), end=c(2048,12))
plot(myts)
plot(myts2)

###
#install.packages("forecast")
library(forecast)
gold
tsdisplay(gold)
library(ggplot2)
ggtsdisplay(gold) #추세선 acf, pacf를 동시에 보여주는 함수. 
#Naive and Random Walk Forecasts = ARIMA(0,1,0) model #예측모델
gold.fcast <- rwf(gold[1:60],h=50)
plot(gold.fcast)
#rwf를 simple하게 ARIMA(0,0,0)으로 분석
plot(naive(gold,h=50), include=200) #Naive and Random Walk Forecasts

wineind
plot(snaive(wineind))

#센서데이터 -> 군집분석 -> DTW비교 -> 파형확인
#DTW(dynamic time wraping) #시간 지연을 갖는 두 그래프의 비교시 사용
#install.packages("dtw")
library(dtw)
idx <- seq(0, 2 * pi, len=100)
a <- sin(idx) + runif(100)/10
b <- cos(idx)

align <- dtw(a, b, step = asymmetricP1, keep=T)
print(align)
dtwPlotTwoWay(align)
plot(align$index1, align$index2, main="Wraping Function")
align$distance

#ARIMA(Autoregressive inegrated moving averages model)

library(tseries)
data(tcm)
class(tcm)
str(tcm)
acf(tcm)
pacf(tcm)

library(forecast)
r <- diff(tcm10y)
summary(r.arma <- arma(r, order=c(1,0)))
summary(r.arma <- arma(r, order=c(2,0)))
summary(r.arma <- arma(r, order=c(0,1)))
summary(r.arma <- arma(r, order=c(0,2)))
summary(r.arma <- arma(r, order=c(1,1)))
plot(r.arma, type="b")
fitted(r.arma)

#########################################################
#센서데이터 -> 군집분석 -> DTW비교 -> 파형확인
#DTW(dynamic time warping) # 시간 지연을 갖는 두 그래프의 비교시 용이함
#정확성 여부는 거리값으로 : distance
# install.packages("dtw")
library(dtw)
idx <- seq(0, 2 * pi, len=100)
(a <- sin(idx) + runif(100)/10)
(b <- cos(idx))

align <- dtw(a, b, step = asymmetricP1, keep=T)
print(align) #근첩한 값을 찾아냄 
dtwPlotTwoWay(align)
plot(align$index1, align$index2, main="Wraping Function")
align$distance

#ARIMA(Autoregressive inegrated moving averages model)
library(tseries)
data(tcm)
class(tcm)
str(tcm)
acf(tcm) #자기상관성
pacf(tcm) #부분자기상관성
head(tcm)
library(forecast)
r <- diff(tcm10y) #차분 : 미분의 효과를 갖고 있다.
r
r.arma
#oder : 차분, 앞  : AR, 뒤 : MR , AIC : 가장 작은 값으로 판단해야 함
summary(r.arma <- arma(r, order=c(1,0))) #c(1,0) 차분 횟수
summary(r.arma <- arma(r, order=c(2,0)))
summary(r.arma <- arma(r, order=c(0,1))) #얘가 제일 좋다던
summary(r.arma <- arma(r, order=c(0,2)))
summary(r.arma <- arma(r, order=c(1,1)))
plot(r.arma, type="b")
fitted(r.arma)
#summary를 봤을 때, 무엇을 보고 판단을 해야 할까??

#######################################
########비행기 승객 데이터_승객수 변화
class(AirPassengers) #ts 
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers) #월별
head(AirPassengers)
summary(AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers) #월별 회
plot(aggregate(AirPassengers, FUN=mean))
boxplot(AirPassengers~cycle(AirPassengers))
#안정성 테스트 stationary => 안정적이 되어야 예측이 가능하다.
#귀무가설 : 비안정성, 대립가설 : 안정적
#귀무가설을 기각하고 대립가설 채택 : p-value _ 0.01나옴
adf.test(diff(log(AirPassengers)), alternative = "stationary", k=0)
acf(log(AirPassengers)) #log를 취하면 안정성 변환, 변환도구를 섞어서 안정성 발견
acf(diff(log(AirPassengers)))  #자기상관성 : 아래  c(0,1,1) 값에 영향을 미침
pacf(diff(log(AirPassengers))) #diff 차분 , 부분상관성
auto.arima(diff(log(AirPassengers))) #추천을 해줌 ARIM옵션을 추천
# Series: diff(log(AirPassengers)) 추천값
# ARIMA(0,0,1)(0,1,1)[12] 

fit <- arima(log(AirPassengers), c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
#arima 모델 생성
pred <- predict(fit, n.ahead = 10*12)

#log차분을 진행했기 때문에 값을 복원하기 위해 2.718값을 사용함
ts.plot(AirPassengers, 2.718^pred$pred, log="y", lty=c(1,3)) #예측그래프

#여기에 껴야함
air.model <- Arima(window(AirPassengers, end=1956+11/12),  #start가 처음부터 end=1956+11/12 여기까지
                   order=c(0,1,1), seasonal = list(order=c(0,1,1), period=12), lambda =0)
plot(forecast(air.model, h=48))
lines(AirPassengers)
air.model2 <- Arima(window(AirPassengers, start=1957), model=air.model) # 1957년부터 끝까지

accuracy(air.model)  #RMSE : 7.89734  얘가 더 좋음
accuracy(air.model2) #RMSE : 12.13132
accuracy(forecast(air.model, h=48, lambda=NULL), log(window(AirPassengers, start=1957)))
#테스트데이터를 이용해서 예측함

# 정상성 확인하는 함수
x = w = rnorm(300)
for( t in 2:300) x[t] = x[t-1] + w[t]
x.ts = ts(x)
#귀무가설 : 안정적, 대립가설 : 비안정성

kpss.test(x.ts) #p-value = 0.01
kpss.test(diff(x.ts)) #차분을 했더니 p-value = 0.1 안정적이 됨
tsdisplay(diff(x.ts))


########
###
class(lynx)
kpss.test(lynx)
fit <- Arima(lynx, order=c(4,0,0), lambda = 0.5)
plot(residuals(fit))
plot(residuals(fit, type = 'response'))

#문제 WWWusage의 60기 까지만 데이터를 분석하고 향수 10기에 대하여 예측해 보시요
library(forecast)
library(tseries)
library(TTR)
library(ggplot2)
class(WWWusage)
plot(WWWusage)
cycle(WWWusage)
head(WWWusage)

kpss.test(WWWusage)
auto.arima(WWWusage)
log(WWWusage) %>%
  Arima(order=c(1,1,1)) %>%
  forecast(h=20) %>%
  autoplot()
# Arima 비정상적 데이터도 분석해주는 함수
# 기간 제한을 두지 않고 예측
WWW.model <- Arima(WWWusage, order=c(1,1,1),
                   seasonal = list(order=c(1,1,1),
                                   period =12), lambda=0)
plot(forecast(WWW.model))
accuracy(WWW.model)

#종료 시점을 두고 예측할 기간을 선택
WWW.model <- Arima(window(WWWusage, end =60), order = c(1,1,1),
                   seasonal = list(order = c(0,1,1), period=12), lambda = 0)
plot(forecast(WWW.model, h=40))
accuracy(WWW.model)


# 지수 평활모델을 이용한 모델
fit <- ets(WWWusage)
plot(WWWusage)
lines(fitted(fit), col='red')
lines(fitted(fit, h=2), col='green')
lines(fitted(fit, h=3), col='blue')
legend("topleft", legend = paste("h=", 1:3), col=2:4, lty=1)

#######################################################################
#오스트레일리아 가스 생산량 예측
data(gas)
plot(gas)
plot.ts(gas)
plot(stl(gas,s.window = "periodic"))

gas.diff<- diff(log(gas))
adf.test(gas.diff, alternative="stationary",k=0)
kpss.test(gas.diff)
acf(gas.diff)
pacf(gas.diff)
plot(gas.diff)
gas.ari <- auto.arima(gas.diff)
tsdiag(gas.ari)

# 종료시점 지정
fit <- arima(window(log(gas), end=1980), c(1,0,2),
             seasonal = list(order =c(0,1,1), period=12))
plot(forecast(fit, h=200))


# 종료시점 미지정
fit <- arima(log(gas), c(1,0,2),
             seasonal = list(order = c(0,1,1),period = 12))
plot(forecast(fit, h=200))

fit <- arima(window(log(gas),end =1985), c(1,0,2),
             seasonal = list(order = c(0,1,1),period=12))

# 예측
pred <- predict(fit, n.ahead = 10*12)
ts.plot(gas, 2.718^pred$pred ,log = "y", lty = c(1,3), h=20)

accuracy(fit)

#########################################shiny를 이용한 reporting 
library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Hello Shiny!"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs", 
                "Number of observations:", 
                min = 1,
                max = 1000, 
                value = 500)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$distPlot <- renderPlot({

    # generate an rnorm distribution and plot it
    dist <- rnorm(input$obs)
    hist(dist)
  })
})

runExample("01_hello")

#######
library(leaflet)
app = shinyApp(
	ui = fluidPage(leafletOutput('mymap')),
	server = function(input, output){
		map = leaflet() %>% addTiles() %>% setView(127, 37.5, zoom = 10)
		map<-addMarkers(map, lng=127, lat=37.5, popup="중심")
		output$myMap = renderLeaflet(map)
		}
	)

if(interactive()){
	ui<-fluidPage(
		titlePanel("Shiny 예제"),
		sidebarLayout(
			sidebarPanel(
				sliderInput("obs", "관측수:", min=0, max=1000, value=500)
			),
			mainPanel(
				plotOutput("distPlot")
			)
		)
	)
	
server <- function(input, output){
	output$distPlot<-renderPlot({ hist(rnorm(input$obs)) })
	}
	
	shinyApp(ui, server)
	ui<-fluidPage(
		title="안녕 Shiny!",
		fluidRow(
			column(width=4, "4"),
			column(width=3, offset=2, "3 offset 2")
			)
		)
		shinyApp(ui=ui, server=function(input, output){})
}

########
library(shinydashboard)
ui<-dashboardPage(
	dashboardHeader(title="Shiny 교육", titleWidth=450),
	dashboardSidebar(
		sidebarMenu(
			menuItem("전국전세가에측 storyboard", tabName="widgets4", icon=icon("th")),
			menuItem("EDA", tabName="widgets", icon=icon("th"))
		)
	),
	dashboardBody(
		tabItems(
			tabItem(tabName="widgets4",
				h2("전국전세가에측"),
				h2(textOutput("currentTime")),
				imageOutput("preImage"),
				imageOutput("preImage2")
			),
			
			tabItem(tabName="widgets",
				h2("히스토그램 출력"),
				box(title = "Histogram1", solidHeader=TRUE,
				collapsible = TRUE,
				plotOutput("plot1",height=250)
				),
				box(title = "Inputs", solidHeader=TRUE,
				"Box content here", br(), "More box content",
				sliderInput("slider1", "Slider input:", 1, 100, 50),
				textInput("text1", "Text input:"),
				verbatimTextOutput("textinputvalue"),
				textInput("Id", "Label"),
				sidebarPanel(
					div("히스토그램"),
					div("출력란")
				)
				)
			)
		)
	)
)
server <- function(input,output, session){
	set.seed(122)
	data<-rnorm(500)
	output$preImage<-renderImage({
		filename<-normalizePath(file.path('C:\\Users\\acorn\\Documents\\R\\images', paste('dabang', '.png', sep="")))
		list(src=filename, alt=paste("Image number", input$n))
		},deleteFile=FALSE)
	output$preImage2 <- renderImage({
		filename<-normalizePath(file.path('C:\\Users\\acorn\\Documents\\R\\images',paste('dabang2','.png', sep='')))
		list(src=filename, alt=paste("Image number", input$n))
		}, deleteFile=FALSE)
		
	output$textinputvalue<-renderText({ input$text1 })
	output$plot1<-renderPlot({
		data<-histdata[seq_len(input$slider1)]
		hist(data)
	})
}
shinyApp(ui=ui,server=server)

######
getwd()
library(DBl)
library(pool)
library(RMySQL)
library(dplyr)
library(igraph)
pool<-dbPool(
	drv = RMySQL::MySQL(),
	dname = "ITtest",
	host = "192.168.",
	username = "gksrlgns",
	password = "gksrlgns1"
)
	
process = function(v)
{
	v = unlist(strsplit(x=v, split=",|\\s+", perl=T))
	l = v[1]
	v = v[-1]
	n = length(v)
	p1 = p2 = label = NULL
	for(i in 1:(n-1))
	{
		for(j in (i+1):n)
		{
			p1 = c(p1,v[i])
			p2 = c(p2,v[j])
			label = c(label, l)
		}
	}
	return(list(p1=p1, p2=p2, l=label))
}


ui <- dashboardPage(
  dashboardHeader(title = "에이콘 아카데미 shiny교육", titleWidth=450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("전국전세가 예측 storyboard", tabName="widgets4", icon= icon("th")),
      menuItem("EDA", tabName="widgets", icon= icon("th"))
    )
  ),
  dashboardBody(
	  tabItems(
			tabItem(tabName = "widgets4",
				h2("전국 전세가 예측"),
				h2(textOutput("currentTime")),
				imageOutput("preImage"),
				imageOutput("preImage2")
			),
			tabItem(tabName = "widgets4",
				h2("히스토그램출력"),
				box(
					title="Histogram1", solidHeader = TRUE,
					collapsible = TRUE,
					plotOutput("plot1", height= 250)
				),
				box(
					title="inputs", solidHeader = TRUE,
					"Box content here", br(), "More box content",
					sliderInput("slider1", "Slider input :", 1, 100, 50),
					textInput("text1", "Text input:"),
					verbatimTextOutput("textinputvalue"), #입력된 텍스트를 출력하기 위하여
					textInput("Id", "Label"),
					sidebarPanel(
						div("히스토그램"),
						div("출력란")
					)
				)
			)
		)
	)
)

#########################################
library(DBI)
# install.packages("pool")
library(pool)
library(dplyr)
library(igraph)
library(leaflet)
# pool <- dbPool( #DB풀 생성 : 연결객체 -웹에서 DB인터페이스 pool : DB연결객체를 여러개 생
#   drv = RMySQL ::MySQL(),
#   dbname="ITems",
#   host = "192.168.100.69",
#   username="",
#   password = ""
# )
#16개의 connction
process = function(v)
{
  v = unlist(strsplit(x = v, split = ",|\\s+", perl=T))
  l = v[1]
  v = v[-1]
  n = length(v)
  p1 = p2 = label=NULL
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      p1 = c(p1, v[i])
      p2 = c(p2, v[j])
      label=c(label,l)
    }
  }
  return(list(p1=p1, p2=p2, l=label))
}
shinyApp(ui = ui, server =server)


