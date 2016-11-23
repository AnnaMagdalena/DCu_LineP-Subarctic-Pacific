library(cowplot)
library(tidyverse)


data <-read_csv('C:/Users/Jeff/Desktop/Ania_coding/Matlab_Oceanographic_data/Cu_NPacific-Fig5-MS.csv')

data$Station <- as.factor(data$Station)
data$Ref<- as.factor(data$Ref)
#glimpse(data)

# subsetting data for region 1
reg1_subset <- data %>%
  filter(Station %in% c("5", "6", "CR27"))

p1 <- reg1_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
  scale_shape_manual(values=c(1, 2, 17))+
  scale_y_reverse(limits=c(6000,0))+
  scale_x_continuous(limits=c(1.1,4.9))+
	ylab("Depth (m)")+
	xlab("DCu (nM)")+
  theme_bw()+
	theme(axis.text = element_text(size=20),
				axis.title.x = element_text(size = 20),
				legend.text = element_text(size=16),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				legend.position = c(.25,.15))

p1 <- ggdraw(switch_axis_position(p1, axis= 'x'))


# region 2
reg2_subset <-data%>%
  filter(Station %in% c("4","19"))

p2 <- reg2_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
  scale_shape_manual(values=c(16,2))+
  scale_y_reverse(limits=c(6000,0))+
  scale_x_continuous(limits=c(1.1,4.9))+
  xlab("DCu (nM)")+
  theme_bw()+
	theme(axis.text = element_text(size=20),
				axis.title.x = element_text(size = 20),
				axis.title.y = element_blank(),
				legend.text = element_text(size=16),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				legend.position = c(.25,.15))

p2 <-ggdraw(switch_axis_position(p2, axis= 'x'))

# region 3
reg3_subset <-data%>%
  filter(Station %in% c("T7a","T7b","P26"))

p3 <- reg3_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
  scale_shape_manual(name= "Station OSP",labels = c("This study","Martin et al 1989", "Coale & Bruland 1980"),values=c(0, 5, 8))+
  scale_y_reverse(limits=c(6000,0))+
  scale_x_continuous(limits=c(1.1,4.9))+
  xlab("DCu (nM)")+
  theme_bw()+
	theme(axis.text = element_text(size=20),
				axis.title.x = element_text(size = 20),
				axis.title.y = element_blank(),
				legend.text = element_text(size=16),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				legend.position = c(.45,.17))

p3 <-ggdraw(switch_axis_position(p3, axis= 'x'))

#region4

reg4_subset <-data%>%
  filter(Station %in% c("BD21","P12"))

p4 <- reg4_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
  scale_shape_manual(values=c(17, 0))+
  scale_y_reverse(limits=c(6000,0))+
  scale_x_continuous(limits=c(1.1,4.9))+
  xlab("DCu (nM)")+
  theme_bw()+
	theme(axis.text = element_text(size=20),
				axis.title.x = element_text (size = 20),
				axis.title.y = element_blank(),
				legend.text = element_text(size=16),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				legend.position = c(.25,.15))

p4 <-ggdraw(switch_axis_position(p4, axis= 'x'))

# region 5
reg5_subset <-data%>%
  filter(Station %in% c("T6a","T6b","17"))

p5 <- reg5_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
  scale_shape_manual(labels = c("T6,Martin et al 1989", "T6,Coale & Bruland 1980","17"),values=c(5, 8, 16))+
  scale_y_reverse(limits=c(6000,0))+
  scale_x_continuous(limits=c(1.1,4.9))+
  xlab("DCu (nM)")+
  theme_bw()+
	theme(axis.text = element_text(size=20),
				axis.title.x = element_text(size=20),
				axis.title.y = element_blank(),
				legend.text = element_text(size=16),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				legend.position = c(.45,.17))

p5 <-ggdraw(switch_axis_position(p5, axis= 'x'))

# region 5
reg6_subset <-data%>%
  filter(Station %in% c("226","TR7"))

p6 <- reg6_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
  scale_shape_manual(values=c(4, 17))+
  scale_y_reverse(limits=c(7500,0))+
  scale_x_continuous(limits=c(0,6.5))+
  xlab("DCu (nM)")+
	ylab("Depth (m)")
  theme_bw()+
	theme(axis.text = element_text(size=20),
				axis.title.x = element_text(size=20),
				axis.title.y = element_text(size=20),
				legend.text = element_text(size=18),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				legend.position = c(.25,.15))

p6 <-ggdraw(switch_axis_position(p6, axis= 'x'))

# region 5
reg7_subset <-data%>%
  filter(Station %in% c("202","H17bru","SAFe"))

p7 <- reg7_subset %>%
  ggplot(aes(x=dCu, y=Depth, group=Station)) +
  geom_path(size=0.2)+
  geom_point(aes(shape=Station), size=6)+
	scale_shape_manual(labels = c("202","H17, Bruland, 1980","SAFe, Biller&Bruland, 2012"),values=c(4, 6, 11))+
  scale_y_reverse(limits=c(7500,0))+
  scale_x_continuous(limits=c(0,6.5))+
	guides(shape=guide_legend(override.aes = list(size=12)))+
	xlab("DCu (nM)")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
  			legend.text = element_text(size=18),
  			axis.title.x = element_text(size=20),
  	    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  			axis.text = element_text(size=20),
        legend.position = c(.47,.17))

p7 <-ggdraw(switch_axis_position(p7, axis= 'x'))


##############################################################################################

# Orientating the plots


plots <- plot_grid (p1,p2,p3,p4,p5,p6,p7,
                   labels = c("R1","R2","R3","R4","R5","R6","R7"),
                   ncol = 5,align = "h")


save_plot("Fig5_MS.tiff",plots, ncol = 5, nrow = 2,base_height = 7, base_width = 5, dpi=1000)
#save_plot("Fig5_MS.pdf",plots, ncol = 5, nrow = 2,base_height = 4.5, base_width = 3.5, dpi=1200)
#save_plot("Fig5_MS.eps",plots, ncol = 5, nrow = 2,base_height = 4.5, base_width = 3.5, dpi=1200)

#map <-readTIFF("Fig5_map.tif")

#plots <- ggdraw()+ draw_plot(map,.5,0,.5,.5)
