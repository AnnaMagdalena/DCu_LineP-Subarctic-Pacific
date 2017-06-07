#Figure 5 of the manuscript

# load packages
library(cowplot)
library(tidyverse)

# load the dataset
data <-read_csv('C:/Users/Ania/DCu_LineP-Subarctic-Pacific/Fig5_Cu-studies-in-NPacific-profiles/Cu_NPacific-Fig5-MS.csv')

# set station and references as factors
data$Station <- as.factor(data$Station)
data$Ref<- as.factor(data$Ref)


#~~~~~~~plotting region 1~~~~~~~~~~~~~~~~

#subsetting data for region 1
p1 <- data %>%
	filter(Station %in% c("5", "6", "CR27","TR15"))%>%
	ggplot(aes(x=dCu, y=Depth, group=Station)) +
	geom_path(size=0.3)+
	geom_point(aes(shape=Station,color=Station), size=2)+
	scale_color_manual(name="Region 1",labels = c("5, Tanita et al., 2015","6, Moffett & Dupont, 2007",
																								"CR27, Takano et al., 2014","TR15, Takano et al., 2014"),
										 values=c("#000000","#000000","#000000","#999999"))+
	scale_shape_manual(name="Region 1",labels = c("5, Tanita et al., 2015","6, Moffett & Dupont, 2007",
																								"CR27, Takano et al., 2014","TR15, Takano et al., 2014"),
										 values=c(1,2,17,17))+
	scale_y_reverse(expand=c(0.01,0),limits=c(6700,0), breaks=seq(0,6700,by=1000))+
	scale_x_continuous(position="top",limits=c(0.8,4.9))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
	guides(shape=guide_legend(override.aes = list(size=2)))+
	ylab("Depth (m)")+
	theme_bw()+
	theme(#legend.key.size = unit(0.25, "cm"),
				legend.position="none",
				#axis.title.y = element_blank(),
				#axis.text.y = element_blank(),
		    #legend.title=element_text(size=8),
				#legend.text = element_text(size=8),
				axis.title = element_text(size=10),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text = element_text(size=9.5))
			#	legend.position = c(.50,.13))

#~~~~~~~~~~~~ plotting region 2~~~~~~~~~~~~~~~~~~

p2 <- data %>%
	filter(Station %in% c("4","19"))%>%
	ggplot(aes(x=dCu, y=Depth, group=Station)) +
	geom_path(size=0.3)+
	geom_point(aes(shape=Station), size=2)+
	scale_shape_manual(name="Region 2",labels = 
										 	c("19, Fujishima et al., 2001","4, Moffett & Dupont, 2007"),values=c(16,2))+
	scale_y_reverse(expand=c(0.01,0),limits=c(6500,0), breaks=seq(0,6500,by=1000))+
	scale_x_continuous(position="top",limits=c(0.8,4.9))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
	guides(shape=guide_legend(override.aes = list(size=2)))+
	#	ylab("Depth (m)")+
	theme_bw()+
	theme( legend.key.size = unit(0.25, "cm"),
		    legend.title=element_text(size=8),
				legend.text = element_text(size=8),
				axis.title = element_text(size=10),
				axis.title.y = element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text = element_text(size=9.5),
				axis.text.y = element_blank(),
				legend.position = c(.47,.17))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ region 3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p3 <- data %>%
	filter(Station %in% c("T7a","T7b","P26"))%>%
	ggplot(aes(x=dCu, y=Depth, group=Station)) +
	geom_path(size=0.3)+
	geom_point(aes(shape=Station), size=2)+
	scale_shape_manual(name= "Region 3,OSP",
										 labels = c("This study","Martin et al., 1989", "Coale & Bruland, 1990"),
										 values=c(0, 5, 8))+
	scale_y_reverse(expand=c(0.01,0),limits=c(6500,0), breaks=seq(0,6500,by=1000))+
	scale_x_continuous(position="top",limits=c(0.8,4.9))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
	guides(shape=guide_legend(override.aes = list(size=2)))+
	#	ylab("Depth (m)")+
	theme_bw()+
	theme( legend.key.size = unit(0.25, "cm"),
		    legend.title=element_text(size=8),
				legend.text = element_text(size=8),
				axis.title = element_text(size=10),
				axis.title.y= element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text.y = element_blank(),
				axis.text = element_text(size=9.5),
				legend.position = c(.41,.15))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~region4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p4 <- data %>%
	filter(Station %in% c("BD21","P12"))%>%
	ggplot(aes(x=dCu, y=Depth, group=Station)) +
	geom_path(size=0.3)+
	geom_point(aes(shape=Station), size=2)+
	scale_shape_manual(name= "Region 4",
										 labels = c("BD21, Takano et al., 2014","P12, This study"),
										 values=c(17, 0))+
	scale_y_reverse(expand=c(0.01,0),limits=c(6500,0), breaks=seq(0,6500,by=1000))+
	scale_x_continuous(position="top",limits=c(1.1,4.9))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
	guides(shape=guide_legend(override.aes = list(size=2)))+
	#	ylab("Depth (m)")+
	theme_bw()+
	theme(legend.key.size = unit(0.25, "cm"),
		    legend.title=element_text(size=8),
				legend.text = element_text(size=8),
				axis.title = element_text(size=10),
				axis.title.y = element_blank(),
				axis.text.y = element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text = element_text(size=9.5),
				legend.position = c(.47,.17))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~region 5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p5 <- data%>%
	filter(Station %in% c("17","T6a","T6b"))%>%
	ggplot(aes(x=dCu,y=Depth, group=Station)) +
	geom_path(size=0.3)+
	geom_point(aes(shape=Station), size=2)+
	scale_shape_manual( name="Region 5",
											labels = c("17, Fujishima et al., 2001","T6, Coale & Bruland, 1990","T6, Martin et al., 1989"),
											values=c(16,8,5))+
	scale_y_reverse(expand=c(0.01,0),limits=c(6500,0), breaks=seq(0,6500,by=1000))+
	scale_x_continuous(position="top",limits=c(0.8,4.9))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
	guides(shape=guide_legend(override.aes = list(size=2)))+
	#	ylab("Depth (m)")+
	theme_bw()+
	theme( legend.key.size = unit(0.25, "cm"),
		    legend.title=element_text(size=8),
				legend.text = element_text(size=8),
				axis.title = element_text(size=10),
				axis.title.y =element_blank(),
				axis.text.y = element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text = element_text(size=9.5),
				legend.position = c(.47,.16))

#~~~~~~~~~~~~~~~~~~~~~~~~region 6~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p7 <- data %>%
	filter(Station %in% c("202","H17bru","SAFe","T4"))%>%
	ggplot(aes(x=dCu, y=Depth, group=Station)) +
	geom_path(size=0.3)+
	geom_point(aes(shape=Station), size=2)+
	scale_shape_manual(name="Region 6",
										 labels = c("202, Boyle et al., 1977",
										 					 "H17, Bruland, 1980","SAFe, Biller & Bruland, 2012",
										 					 "T4, Coale & Bruland, 1990"),values=c(4, 6, 13,8))+
	scale_y_reverse(expand=c(0.01,0),limits=c(8000,0), breaks=seq(0,8000,by=1000))+
	scale_x_continuous(position="top",limits=c(0.3,6.6))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
	guides(shape=guide_legend(override.aes = list(size=2)))+
	#	ylab("Depth (m)")+
	theme_bw()+
	theme(legend.key.size = unit(0.25, "cm"),
		    legend.title=element_text(size=8),
				legend.text = element_text(size=8),
				axis.title = element_text(size=10),
				axis.text.y = element_blank(),
				axis.title.y = element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text = element_text(size=9.5),
				legend.position = c(.47,.18))


#~~~~~~~~~~~~~~~~~~~~~~~~ region 7~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p6 <- data %>%
	filter(Station %in% c("226","TR7"))%>%
	ggplot(aes(x=dCu, y=Depth, group=Station)) +
	geom_path(size=0.4)+
	geom_point(aes(shape=Station), size=2)+
	scale_shape_manual(name="Region 7",
										 labels=c("226, Boyle et al., 1997","TR7, Takano et al., 2014"),
										 values=c(4, 17))+
	scale_y_reverse(expand=c(0.01,0),limits=c(8000,0), breaks=seq(0,8000,by=1000))+
	scale_x_continuous(position="top",limits=c(0.3,6.6))+
	xlab (expression("dCu"~(nmol~kg^{-1})))+
#	guides(shape=guide_legend(override.aes = list(size=2)))+
	ylab("Depth (m)")+
	theme_bw()+
	theme(#legend.key.size = unit(0.25, "cm"),
		    #legend.title=element_text(size=8),
				legend.position="none",
	#			legend.text = element_text(size=8),
	#			axis.text.y = element_blank(),
	#			axis.title.y = element_blank(),
				axis.title = element_text(size=10),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				axis.text = element_text(size=9.5))
		#		legend.position = c(.43,.16))

# Orientating the plots + saving the plots

plots <- plot_grid (p1,p2,p3,p4,p5,p6,p7, ncol = 5,align = "h")

save_plot("Fig5_MS.tiff",plots, ncol = 5, nrow = 2,base_height = 3.3, base_width = 1.9, dpi=1000)
save_plot("Fig5_MS-labels.pdf",plots, ncol = 5, nrow = 2,base_height = 3.3, base_width = 1.9, dpi=1000)
#save_plot("Fig5_MS.eps",plots, ncol = 5, nrow = 2,base_height = 4.5, base_width = 3.5, dpi=1000)