library(googlesheets)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

##get data for 2008-2015 for pedocks and bumpkin

gs_auth()
rd<-gs_url("https://docs.google.com/spreadsheets/d/1mqCaRwuZ0HnMAeVtQ4k-GktEFdChdJiUoweW5PXfZdU/edit#gid=1368341786")
data<-gs_read_csv(rd,ws=7)
head(data)

## make it tidy

data<-gather(data,is_sex,mass,na.rm=TRUE)
head(data)


data$mass<-as.numeric(data$mass)
head(data)


data<-data %>% separate(is_sex,c("sex_is","year"),sep=2) 
data<-data %>% separate(sex_is,c("island","sex"),sep=1)
data$year<-as.numeric(str_c(20,data$year))
head(data)


data$island[data$island=="B"]<-"Bumpkin Island"
data$island[data$island=="P"]<-"Pedocks Island"
head(data)

## get the data for pedocks and bumpkin for 2016

ped_2016<-gs_key("1eZG0d3R4_vBD7l5H8Zhcwrh9Gv0oJ7WF4DeKFE_Kwxk")
ped_2016<-gs_read(ped_2016)
head(ped_2016)

ped_2016 %>%
	select(Gender,`Weight (g)`,`ID Tag`,Age)%>%
  filter(str_detect(Age,"Adult"))%>%
	mutate(island="Pedocks Island",year=2016) %>%
	rename(sex=Gender,mass=`Weight (g)`)%>%
	group_by(`ID Tag`,island,sex,year)%>%
	mutate(mass=as.numeric(mass))%>%
	filter(mass!="NA")%>%
	summarise(mass=mean(mass))%>%	
	bind_rows(.,data) -> data
	
unique(ped_2016$Age)	

bum_2016<-gs_key("1bVVKT1Rqu5QPn48e7JbtgB-VMLshf_49f0jHlixXfgw")
bum_2016<-gs_read(bum_2016)
head(bum_2016)
unique(bum_2016$ID)
unique(bum_2016$Age)

bum_2016 %>%
	select(`Weight (G)`,Sex,Species,ID,Age) %>%
	filter(Species=="pero" | Species =="spero") %>%
	filter(str_detect(Age,"adult"))%>%
	rename(sex=Sex,mass=`Weight (G)`) %>%
	mutate(sex=str_to_upper(sex))%>%
	mutate(island="Bumpkin Island",year=2016) %>%
	filter(mass!="NA")%>%	
	group_by(ID,island,sex,year)%>%
	summarise(mass=mean(mass))%>%
	bind_rows(.,data) -> data




ggplot(data) +
	aes(x=year,y=mass,color=sex) +
	geom_point(position=position_jitter(width=0.2),size=2) +
	scale_color_manual(name="Sex",values=c("black","grey50"),labels=c("Female","Male")) +
	geom_smooth(method = "lm",se=F,size=0.5,lty=1)+
	xlab("Year")+
	ylab("Mass (grams)")+
	scale_x_continuous(breaks=seq(2008,2016,1))+
	scale_y_continuous(breaks=seq(15,45,6))+
	theme(axis.title=element_text(face="plain",size=12)) +
	theme(axis.text=element_text(size=12,color ="black"))+
	theme(
		axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
		axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
	theme(panel.background=element_rect(colour="black",fill="NA",size=1))+
	theme(legend.position=c(0.1,0.85),legend.key=element_rect(fill="NA"))+
	theme(legend.background=element_rect(color="black",size=0.5))+
	theme(strip.background=element_rect(fill="NA"),strip.text.x=element_text(size=12,face="plain"))+
	facet_grid(~island,scales="free_x",space="free_x")
	