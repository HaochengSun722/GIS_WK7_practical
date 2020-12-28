library(raster)
library(sp)
library(tmap)
library(raster)
library(rgeos)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(stringr)
library(raster)
library(fs)
library(sf)
library(tidyverse)
library(ggplot2)
library(GGally)
library(RStoolbox)
library(plotly)
library(htmlwidgets)
library(rstatix)
###################################################
###Simple example
load(url("http://github.com/mgimond/Spatial/raw/master/Data/raster.RData"))
plot(bath)
summary(bath)
crs(bath)

###GoogleDrive
o<-drive_download("https://drive.google.com/open?id=1MV7ym_LW3Pz3MxHrk-qErN1c_nR0NWXy",
                  path="prac7_data/exampleGoogleDrivedata/LC08_L1TP_203023_20190513_20190521_01_T1.tar.gz", 
                  overwrite=T)
###用于解压.tar.gz的文件，提取一套.tif格式的文件
listfiles<-dir_info(here::here("prac7_data")) %>%
  dplyr::filter(str_detect(path, ".gz")) %>%
  dplyr::select(path)%>%
  dplyr::pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::untar(exdir=here::here("prac7_data", "exampleGoogleDrivedata"))

listfiles2<-dir_info(here::here("prac7_data")) %>%
  dplyr::filter(str_detect(path, ".zip")) %>%
  dplyr::select(path)%>%
  dplyr::pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::untar(exdir=here::here("prac7_data", "Manchester_boundary"))
#######################################################


###Part1_Processing Raster Data
##Step1_Loading Data
#读取所有tif文件，并将其合为一个stack
listlandsat<-dir_info(here::here("prac7_data", "exampleGoogleDrivedata"))%>%
  dplyr::filter(str_detect(path, "[B123456790].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  # Load our raster layers into a stack
  stack()

#读取Manchester_borough数据(当前坐标为WGS84)
manchester_boundary <- st_read(here::here("prac7_data", 
                                          "Manchester_boundary",
                                          "Manchester_boundary.shp"))

##Step2_Resampling重采样
#在该tif数据集中band8未与其他tif重合，因此需要对其重新采样(可以通过plot stack
#来看到没有band8 layer)
#首先单独读取band8数据文件,并将其转化为raster
b8list<-dir_info(here::here("prac7_data", "exampleGoogleDrivedata"))%>%
  dplyr::filter(str_detect(path, "[B8].tif")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  raster()
#对b8list进行重采样
## ngb is a nearest neighbour sampling method
b8correct <- b8list%>%
  resample(., listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B1, 
           method = "ngb") %>%
  # Write out the raster
  writeRaster(.,str_c(here::here("prac7_data", 
                                 "exampleGoogleDrivedata"), 
                      names(b8list), 
                      sep="/"),
              format='GTiff', 
              overwrite=TRUE)
#Load band 8 and add it to our raster stack
b8backin<-dir_info(here::here("prac7_data", "exampleGoogleDrivedata"))%>%
  dplyr::filter(str_detect(path, "[B8].tif")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  raster()

listlandsat <- listlandsat %>%
  addLayer(., b8backin)

#比较新加入的band8与其他band的区别，是否相同
raster::compareRaster(listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B1,
                      listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B8)

##Step3_Clipping剪裁
#在得到tif数据集后，需要将其剪裁为我们研究的区域范围，此时可以用到Manchester_borough的shp文件
lsatmask <- listlandsat %>%
  # now crop our temp data to the extent
  crop(.,manchester_boundary)%>%
  mask(.,  manchester_boundary)

#将当前剪切好的tif文件储存在新的文件夹mask中
# add mask to the filenames within the raster stack
names(lsatmask) <- names(lsatmask)%>%
  str_c(., 
        "mask", 
        sep="_")

# I need to write mine out in another location
outputfilenames <-
  str_c("prac7_data/exampleGoogleDrivedata/", "mask/", names(lsatmask) ,sep="")

lsatmask %>%
  writeRaster(., outputfilenames, 
              bylayer=TRUE, 
              format='GTiff', 
              overwrite=TRUE)



###Part2_Exploring Data探索数据之间的关系
##Step1_加载band1-7数据
#可以通过重新加载数据或提取之前stack中数据的方法进行load
manc<-stack(lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B1_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B2_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B3_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B4_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B7_mask)

names(manc) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2') 
#NIR-现代近红外光谱 SWIR-短波红外线

##Step2_plot数据
#🅰True Color--在human眼中图像的颜色
# true colour composite
manc_rgb <- stack(manc$red, manc$green, manc$blue)%>%
  plotRGB(.,axes=TRUE, stretch="lin")

#🅱False Color--使用非RGB的三个layers
# false colour composite
manc_false <- stack(manc$NIR, manc$red, manc$green)%>%
  plotRGB(.,axes=TRUE, stretch="lin")

##Step3_比较band之间的区别
## How are these bands different?
#set the plot window size (2 by 2)
par(mfrow = c(2,2))
#plot the bands
plot(manc$blue, main = "Blue")
plot(manc$green, main = "Green")
plot(manc$red, main = "Red")
plot(manc$NIR, main = "NIR")

## Look at the stats of these bands
pairs(manc[[1:7]])
#Low statistical significance means that the bands are sufficiently 
#different enough in their wavelength reflectance to show different 
#things in the image. 
manc %>%
  as.data.frame(., na.rm=TRUE)%>%
  sample_n(., 100)%>%
  ggpairs(.,axisLabels="none")


###Part3_Basic Raster calculation基础栅格计算
##Step1 NDVI（normalized difference vegetation index）归一化植被指数
#由于叶绿素可以被红外光反射NIR，可以被红光吸收Red，因此NDVI可以通过NIR和Red光谱图来表达
#👉NDVI=NIR-RED/NIR+RED，通过方程式定义NDVI的计算
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}
#👉通过预设好的function定义参数，ndvi
ndvi <- NDVIfun(manc$NIR, manc$red)
#👉plot出NDVI的空间分布和频率分布
par(mfrow = c(1,2))
ndvi %>%
  plot(.,col = rev(terrain.colors(10)), main = "Landsat-NDVI")
# Let's look at the histogram for this dataset
ndvi %>%
  hist(., breaks = 40, main = "NDVI Histogram", xlim = c(-.3,.8))

#👉对NDVI进行重分类，将大于0.3的NDVI指数定义为植被（需要注意，在实际分析过程中
#需要与literature相结合，不能通过自己定义）
veg <- ndvi %>%
  reclassify(., cbind(-Inf, 0.3, NA))
veg %>%
  plot(.,main = 'Possible Veg cover')
par(mfrow = c(1,1))
par(no.readonly = TRUE)

manc_rgb <- stack(manc$red, manc$green, manc$blue)%>%
  plotRGB(.,axes=TRUE, stretch="lin")
veg %>%
  plot(., add=TRUE, legend=FALSE)


###Part4_Advanced raster calculation先进栅格计算（设计一个研究，探究城市地区与温度的关系）
#✅Step1——使用Landsat data计算温度数据，通过ToA方法进行计算TOA=Grescale*QCAL+Brescale
#👉其中Grescale和Bresale都存储在LANDSAT文件的MTL.txt的文件中，可以通过以下方式提取
MTL<-dir_info(here::here("prac7_data", "exampleGoogleDrivedata")) %>%
  dplyr::filter(str_detect(path, "MTL.txt")) %>%
  dplyr::select(path)%>%
  pull()%>%
  readMeta()
head(MTL)
offsetandgain <-MTL %>%
  getMeta("B10_dn", metaData = ., what = "CALRAD")
TOA <- offsetandgain$gain *
  lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B10_mask + 
  offsetandgain$offset

#👉将TOA转化至Brightness Temperature
#亮度温度是从大气顶部向上传播到卫星的辐射量，单位为等效黑体的温度。
#K1和K2是美国地质勘测局提供用于发射前校准的参数，同样可以通过MTL.txt文件中找到
Calidata <- MTL$CALBT%>%
  as.data.frame()%>%
  mutate(Band=rownames(.))%>%
  filter(Band=="B10_dn")

# subset the columns
K1 <- Calidata %>%
  dplyr::select(K1)%>%
  pull()

K2 <- Calidata %>%
  dplyr::select(K2)%>%
  pull()

Brighttemp <- (K2 / log((K1 / TOA) + 1))

#👉根据之前算出的NDVI值，计算每个像素的植被覆盖率fractional vegetation
#植被覆盖率是植被垂直投影面积与总表面范围的比
facveg <- (ndvi-0.2/0.5-0.2)^2
#这里，0.2代表NDVImin，即为裸土，0.5代表NDVImax，即适宜植被发展的最大值

#👉计算发射率，发射率是吸收的辐射能量与总入射辐射能量之比，与黑体
#（会吸收所有东西）的比率，是吸收率的度量。
emiss <- 0.004*facveg+0.986

#👉计算LST=Tb/(1+(λ*Tb/p))*ln(emiss)
#p=h*c/ϱ
#h为Plank常数；c为真空中光的速度；ϱ为Boltzmann常数
Boltzmann <- 1.38*10e-23
Plank <- 6.626*10e-34
c <- 2.998*10e8
p <- Plank*(c/Boltzmann)
#λ为数据的有效波长(Landsat8band10为10.9)
#define remaining varaibles
lambda <- 1.09e-5
#run the LST calculation
LST <- Brighttemp/(1 +(lambda*Brighttemp/p)*log(emiss))
# check the values
LST#（当前为Kevlin温度）
LST <- LST-273.15
plot(LST)#（当前为°C）

#✅Step2——通过Landsat data计算Urban area
#使用NDBI(Normalized Difference Built-up Index)归一化差异累积指数，
#使用反射带识别累积区域(包括RED,NIR,MIR)
#band6为SWIR,band5为NIR
NDBI=((lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask-
         lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask)/
        (lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask+
           lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask))

#✅Step3——识别NDBI和温度之间的关系
plot(values(NDBI), values(LST))
#由于得到的图像过于密集，难以观察其中的关系，因此可以通过sample的方式减少样本量
# stack the layers

computeddata <- LST%>%
  stack(.,NDBI)%>%
  as.data.frame()%>%
  na.omit()%>%
  # take a random subset
  sample_n(., 500)%>%
  dplyr::rename(Temp="layer.1", NDBI="layer.2")

# check the output
plot(computeddata$Temp, computeddata$NDBI)

heat<-ggplot(computeddata, aes(x = NDBI, y = Temp))+
  geom_point(alpha=2, colour = "#51A0D5")+
  labs(x = "Temperature", 
       y = "Urban index",
       title = "Manchester urban and temperature relationship")+
  geom_smooth(method='lm', se=FALSE)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# interactive plot
ggplotly(heat)

#full point而不是sample
computeddatafull <- LST%>%
  stack(.,NDBI)%>%
  as.data.frame()%>%
  na.omit()%>%
  # take a random subset
  dplyr::rename(Temp="layer.1", NDBI="layer.2")

hexbins <- ggplot(computeddatafull, 
                  aes(x=NDBI, y=Temp)) +
  geom_hex(bins=100, na.rm=TRUE) +
  labs(fill = "Count per bin")+
  geom_smooth(method='lm', se=FALSE, size=0.6)+
  theme_bw()

ggplotly(hexbins)

###Part4_统计总结
Correlation <- computeddatafull %>%
  cor_test(Temp, NDBI, use = "complete.obs", method = c("pearson"))

Correlation#result为0.66

#t-value
abs(qt(0.05/2, 198268))
#其中0.05代表95%的置信区间，2代表双变量测试，198268代表自由度