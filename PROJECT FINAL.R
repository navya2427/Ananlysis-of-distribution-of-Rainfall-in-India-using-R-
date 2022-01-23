##plot width 900, height 700

library(tmap)
library(sf)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(leaflet)

India_Rainfall_state<-read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\Rainfall State wise.csv")
India_state<-readOGR(choose.files(caption = "Select Shapefile", multi = FALSE))
Merged_1 <-merge(India_state,India_Rainfall_state,by="ID_1")

plot_state<-tm_shape(Merged_1)+tm_polygons("Rainfall_statewise",n=5,
                                           title="Range of rainfall \n (in mm)",palette="PuBu")+
  tm_borders(alpha=0.9,lwd=4,col="gray40")+tm_compass()+
  tm_layout(main.title="Annual Rainfall in India",legend.position = c(-.2,-.02),
            legend.text.size=0.8,legend.title.size=1.1,
            main.title.position = "center",frame=FALSE)+
  tm_text("STATE_UT_NAME",size=0.35,auto.placement=F,col="black",)

plot_state


India_Rainfall_district<-read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\Edited rainfall data.csv")
India_district<-readOGR(choose.files(caption = "Select Shapefile", multi = FALSE))
Merged_2<- merge(India_district,India_Rainfall_district,by="ID_2")
View(Merged_2)
plot_district<-tm_shape(Merged_2)+tm_polygons("ANNUAL",title="Range of rainfall \n (in mm)",
                                              palette="Accent",breaks=c(0,500,1000,1500,3000,7500))+
               tm_borders(alpha=0.9)+tm_compass()+
               tm_layout(main.title="Distribution of rainfall \n in India (District-wise)",
                        main.title.size=1.3,legend.position = c(-.2,-.02),legend.text.size=0.8,
                        legend.title.size=1.1,main.title.position = "center" ,frame=FALSE)
               

plot_district



plot_bar <- ggplot(data=India_Rainfall_state, aes(x = reorder(STATE_UT_NAME,Rainfall_statewise), y = Rainfall_statewise,fill= Rainfall_statewise))+
  geom_bar(width=0.8,stat='identity') + 
  geom_point()+coord_flip()+ theme(axis.title.y = element_text(colour="grey20",size=7,face="bold"),
                                   axis.text.x = element_text(colour="grey20",size=7,face="bold"),
                                   axis.text.y = element_text(colour="grey20",size=7,face="bold"),  
                                   axis.title.x = element_text(colour="grey20",size=7,face="bold"),panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                   ,legend.position="none")+ labs(title="Annual Rainfall in Subdivisions",x = "Subdivisions",y = "Annual Rainfall")+theme(plot.title = element_text(hjust = 0.5))
plot_bar

Rainfall_year <- read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\rainfall in india 1951-2000.csv")


India_Rainfall_avg<-read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\Average year wise.csv")
ggplot(data=India_Rainfall_avg, aes(x=Year, y=AVERAGE, group=1)) +
  geom_line(color="blue",size=1)+
  geom_point(color="darkred")+
  theme(axis.title.y = element_text(colour="black",size=10,face="bold"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Average Rainfall in India from the year 1951 to 2000",x = "Year",y = "Average Rainfall(in mm)")
 

India_Seasonal_Rainfall<-read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\Rainfall seasonal.csv")
months<-c("JAN_FEB"="darkblue","MAR_MAY"="orange","JUN_SEP"="green","OCT_DEC"="red")
ggplot(data=India_Seasonal_Rainfall, aes(x=Year)) +
  geom_line(aes(y = JAN_FEB, color = "JAN_FEB"),size=1) +
  geom_line(aes(y = MAR_MAY, color = "MAR_MAY"),size=1) +
  geom_line(aes(y = JUN_SEP, color = "JUN_SEP"),size=1) +
  geom_line(aes(y = OCT_DEC, color = "OCT_DEC"),size=1) +
  theme(axis.title.y = element_text(color="black",size=10,face="bold"),
        axis.text.x = element_text(color="black",size=10,face="bold"),
        axis.text.y = element_text(color="black",size=10,face="bold"),  
        axis.title.x = element_text(color="black",size=10,face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Seasonal Rainfall in India from the year 1951 to 2000",x = "Year",y = "Seasonal Rainfall(in mm)",color="Legend")+
  scale_color_manual(values=months)

Season <- read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\Seasonnn.csv")
India<-readOGR(choose.files(caption = "Select Shapefile", multi = FALSE))

Merged__ <-merge(India,Season_district,by="ID_1")
plot_Jan_Feb<-tm_shape(Merged__)+tm_polygons("Jan.Feb",
                                             title="Range of rainfall \n (in mm)",palette="Accent",breaks=c(0,10,30,50,100,200,300))+
  tm_borders(alpha=0.9)+tm_compass()+
  tm_layout(main.title="Average Rainfall in January-February",legend.position = c(-.2,-.02),
            legend.text.size=0.8,legend.title.size=1.1,
            main.title.position = "center",frame=FALSE)+
  tm_text("STATE_UT_NAME",size=0.45,auto.placement=F,col="black")
 
plot_Jan_Feb 
tmap_mode("view")
tmap_last()
tmap_save(plot_Jan_Feb, "C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\maps and graphs\\Jan-Feb.html")



plot_Mar_May<-tm_shape(Merged__)+tm_polygons("Mar.May",
                                             title="Range of rainfall \n (in mm)",palette="Paired",breaks=c(0,40,100,200,300,500,800))+
  tm_borders(alpha=0.9)+tm_compass()+
  tm_layout(main.title="Average Rainfall in March-May",legend.position = c(-.2,-.02),
            legend.text.size=0.8,legend.title.size=1.1,
            main.title.position = "center",frame=FALSE)+
  tm_text("STATE_UT_NAME",size=0.45,auto.placement=F,col="black")
 
plot_Mar_May
tmap_last()
tmap_save(plot_Mar_May, "C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\maps and graphs\\Mar-May.html")




plot_Jun_Sep<-tm_shape(Merged__)+tm_polygons("Jun.Sep",
                                             title="Range of rainfall \n (in mm)",palette="BuPu",breaks=c(300,500,800,1000,1500,2000))+
  tm_borders(alpha=0.9)+tm_compass()+
  tm_layout(main.title="Average Rainfall in June-September",legend.position = c(-.2,-.02),
            legend.text.size=0.8,legend.title.size=1.1,
            main.title.position = "center",frame=FALSE)+
  tm_text("STATE_UT_NAME",size=0.45,auto.placement=F,col="black")
  
plot_Jun_Sep
tmap_last()
tmap_save(plot_Jun_Sep, "C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\maps and graphs\\Jun-Sep.html")



plot_Oct_Dec<-tm_shape(Merged__)+tm_polygons("Oct.Dec",
                                             title="Range of rainfall \n (in mm)",palette="Accent",breaks=c(0,50,100,200,400,800))+
  tm_borders(alpha=0.9)+tm_compass()+
  tm_layout(main.title="Average Rainfall in October-December",legend.position = c(-.2,-.02),
            legend.text.size=0.8,legend.title.size=1.1,
            main.title.position = "center",frame=FALSE)+
  tm_text("STATE_UT_NAME",size=0.45,auto.placement=F,col="black")
 
plot_Oct_Dec
tmap_last()
tmap_save(plot_Oct_Dec, "C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\maps and graphs\\Oct-Dec.html")




Max_Rainfall_districts<-("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\max rainfall district.csv")
#convert csv to spatial data

Max_rain <- st_as_sf(Max_Rainfall_districts, coords=c("Longitude","Latitude"),crs=4326)


Max_rain<- leaflet()%>%
  addTiles()%>%
  setView(lng=79.99541380788564, lat=24.92712442376889, zoom=4)%>%
  addMarkers(data=Max_rain,popup = paste("<b>State:</b>",Max_rain$STATE_UT_NAME,"<br>","<b>District:</b>",Max_rain$District,"<br>","<b>Rainfall:</b>",Max_rain$Maximum.Rainfall))

Max_rain

htmltools::save_html(Max_rain,"C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\maps and graphs\\Max_rain.html")


India_Rainfall_state<-read.csv("C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\mini prozzectt\\ZONES_1.csv")
#convert csv to spatial data
Merged_1 <-merge(India_state,India_Rainfall_state,by="ID_1")
India_rain_loc <- st_as_sf(India_Rainfall_state, coords=c("Longitude","Latitude"),crs=4326)
View(India_rain_loc)
pal<- colorFactor("BrBG", domain=India_rain_loc$Zones)  ##colorNumeric if you have numeric data

Zone_wise <- leaflet()%>%
  addTiles(group="OSM")%>%
  addProviderTiles(providers$Esri.WorldImagery,group="Esri World Imagery")%>%
  setView(lng=79.99541380788564, lat=24.92712442376889, zoom=7)%>%
  addMarkers(data=India_rain_loc,popup = paste("<b>Zone:</b>",India_rain_loc$Zones,"<br>", "<b>Subdivison:</b>",India_rain_loc$STATE_UT_NAME,"<br>","<b>Rainfall:</b>",India_rain_loc$Rainfall_statewise),group="India_rain_loc")%>%
  addPolygons(data=Merged_1, label=~NAME_1, color=~pal(Zones),fillOpacity = 0.75,weight=1)%>%
  addLayersControl(baseGroups= c("OSM","Esri World Imagery"),
                   overlayGroups = c("India_rain_loc"),
                   options=layersControlOptions(collapsed=FALSE))%>%
  addLegend(pal=pal,values=India_rain_loc$Zones, title="Region")%>%
  addMeasure()%>%
  addMiniMap(toggleDisplay=TRUE,position= "bottomleft", width=150,height=150)

Zone_wise

htmltools::save_html(Zone_wise,"C:\\Users\\libin\\Desktop\\Masters in DS and SA\\SEMESTER 1\\Programming for Spatial Sciences\\MINI PROJECT\\maps and graphs\\Zone_wise.html")

