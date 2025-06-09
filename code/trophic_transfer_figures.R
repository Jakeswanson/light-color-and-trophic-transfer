########### Figures 1 and 2 from Swanson & Dudycha 2025
### "Light color and nutrient availability alter trophic transfer from algae to zooplankton"
# Figure 3 (SEM plot) was created by extracting path coefficients from the fitted SEM and then using 
# Microsoft PowerPoint to create the diagram

##################################
setwd("~/Desktop/JS_Comm_Exp/R")
jsgr<-read.csv("jsgr_data.csv")
Survivors<-read.csv("Geddey_Survivorship.csv")
diversity_data<-read.csv("comm_exp_class.csv")
gen_dat<-read_csv("comm_exp_genera.csv")
library(tidyverse)
library(ggpubr)

######## Create algal group proportion data to make figure 1
gen_dat2<-gen_dat%>%pivot_longer(cols=c(9:79),names_to="Taxa",values_to = "biomass",values_drop_na = FALSE)

all_tax<-unique(gen_dat2$Taxa)%>%
  as_tibble()%>%
  rename(Taxa=value)%>%
  mutate(Cyano_Type="")

all_tax2<-all_tax%>%mutate(Cyano_Type=ifelse(Taxa=="Anabaena"|Taxa=="Aphanizomenon"|Taxa=="Jaaginema"|Taxa=="Pseudoanabaena"|Taxa=="Unknown_cyano_filament","Filamentous",Cyano_Type))%>%
  mutate(Cyano_Type=ifelse(Taxa=="Aphanocapsa"|Taxa=="Aphanothece"|Taxa=="Asterococcus"|Taxa=="Cyanodictyon"|Taxa=="Merismopedia"|Taxa=="Synechochoccus"|Taxa=="Unknown_cyano_coccoid"|Taxa=="Unknown_tailed_cyano","Nonfilamentous",Cyano_Type))

tax_type<-left_join(gen_dat2,all_tax2,by="Taxa")

tax_type<-tax_type%>%mutate(Cyano_Type=ifelse(Cyano_Type=="","Other",Cyano_Type))
tax_type2<-tax_type%>%filter(Cyano_Type!="Other")%>%
  mutate(Light=ifelse(Light=="Full","Broad",Light))%>%
  rename(Nutrient=Phosphorus)

tax_type2$Treatment[tax_type2$Treatment=="Full_HP"]<-"Broad_HP"
tax_type2$Treatment[tax_type2$Treatment=="Full_LP"]<-"Broad_LP"

cyano_split<-tax_type2%>%pivot_wider(names_from = Cyano_Type,values_from = biomass)%>%
  replace_na(list(Filamentous=0,Nonfilamentous=0))%>%
  group_by(Treatment, Micro_Rep, Count_Rep)%>%
  summarise_at(c("Filamentous","Nonfilamentous"),sum)

div_data2<-diversity_data%>%rowwise()%>%
  mutate(total_density=sum(c(Unknown_Class, Diatoms,Dinoflagellates,Cryptophytes,Cyanobacteria, Chrysophytes, Green.Algae, Euglena)))

div_data2<-left_join(div_data2,cyano_split)%>%
  mutate(prop_green=Green.Algae/total_density,
         prop_fil=Filamentous/total_density,
         prop_nf=Nonfilamentous/total_density)

div2_long<-div_data2%>%
  pivot_longer(cols = Unknown_Class:prop_nf,names_to = "class",values_to = "proportion")%>%
  filter(class %in%(c("prop_green","prop_nf","prop_fil")))

##### Proportions plot, figure 1 in manuscript
props<-ggplot(data=div2_long,aes(x=factor(Phosphorus,levels=c("Low", "High")),y=proportion,fill=Light,color=Light ))+
  geom_point(aes(fill=Light),size=2.5,position = position_jitterdodge())+
  geom_boxplot(alpha=0.75,outlier.alpha = 1,outlier.size = 2.5)+
  facet_wrap(~factor(class,levels=c("prop_fil","prop_nf","prop_green"),labels = c("prop_fil"="Filamentous Cyanobacteria","prop_nf"="Nonfilamentous Cyanobacteria","prop_green"="Green Algae")),scales = "fixed",labeller = labeller(class=c("prop_fil"="Filamentous Cyanobacteria","prop_green"="Green Algae","prop_nf"="Nonfilamentous Cyanobacteria")),
             axes="all_x",axis.labels = "margins")+
  scale_color_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","grey25","seagreen","firebrick"))+
  theme_classic()+
  labs(x="Nutrient Status",y= "Proportion of algal community")+
  theme(strip.background = element_rect(linetype = "blank"))+
  theme(axis.title =element_text(size=20))+
  theme(axis.text = element_text(size=20,color="black"))+
  theme(axis.line = element_line(linewidth =1.5))+
  theme(axis.ticks = element_line(linewidth=1.5))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white",color="white"))+
  theme(strip.text = element_text(color="black",size = 18))

#ggsave("algal_proportions.pdf",plot=props,device=cairo_pdf,width = 12,height = 8)

######## Juvenile specific growth rate, figure 2a in manuscript
gr<-ggplot(jsgr,aes(Treatment,jsgr_neg,color=Light,fill=Light))+
  scale_color_manual(values=c("dodgerblue3","black","seagreen","firebrick"))+
  scale_fill_manual(values = c("dodgerblue3","black","seagreen","firebrick"))+
  geom_point(size=2.5,position=position_jitterdodge(dodge.width = 1))+
  geom_boxplot(linewidth=1,alpha=0.75,outlier.size = 2.5,outlier.alpha = 1)+
  annotate("text",x=0.75,y=0.65,label="b",size=10)+
  theme_classic()+
  scale_y_continuous(breaks =c(-1.0,-0.5,0,0.5))+
  ylab(expression("Juvenile Specific Growth Rate"~("\u03bc"*g/"\u03bc"*g*~day^-1)))+
  scale_x_discrete(labels=c("Blue_HP"="Blue High","Blue_LP"="Blue Low","Broad_HP"="Broad High","Broad_LP"="Broad Low","Green_HP"="Green High",
                            "Green_LP"= "Green Low","Red_HP"="Red High","Red_LP"="Red Low"),guide = guide_axis(angle=45))+
  theme(axis.title =element_text(size=18))+
  theme(axis.text = element_text(size=18,color = "black"))+
  theme(axis.title.y = element_text(size=16,color = "black"))+
  theme(axis.line = element_line(linewidth =1.5))+
  theme(axis.ticks = element_line(linewidth=1.5))+
  theme(legend.position = "none")
#ggsave("jsgr.pdf",plot=last_plot(),device=cairo_pdf,width = 8,height = 6)

#### Neonate survivorship, figure 2b in main manuscript
surv<-ggplot(Survivors,aes(Treatment,Prop_Surv,color=Light,fill=Light)) +
  scale_color_manual(values=c("dodgerblue3","black","seagreen","firebrick"))+
  scale_fill_manual(values=c("dodgerblue3","black","seagreen","firebrick"))+
  geom_boxplot(size=1,alpha=0.75)+
  geom_point(size=3)+
  theme_classic()+
  ylab(expression("Proportion of Survivors"))+
  scale_y_continuous(breaks =c(0,0.25,0.50,0.75,1))+
  annotate("text",x=0.80,y=1.2,label="a",size=10)+
  scale_x_discrete(labels=c("Blue_HP"="Blue High","Blue_LP"="Blue Low","Broad_HP"="Broad High","Broad_LP"="Broad Low","Green_HP"="Green High",
                            "Green_LP"= "Green Low","Red_HP"="Red High","Red_LP"="Red Low"),guide = guide_axis(angle=45))+
  theme(axis.title =element_text(size=18))+
  theme(axis.text = element_text(size=18,color = "black"))+
  theme(axis.line = element_line(linewidth=1.5))+
  theme(axis.ticks = element_line(linewidth=1.5))+
  theme(legend.position = "none")

two_plot<-ggarrange(surv,gr,nrow = 1)
#ggsave("gr_surv_merged.pdf",plot=two_plot,device=cairo_pdf,width = 10,height = 6)