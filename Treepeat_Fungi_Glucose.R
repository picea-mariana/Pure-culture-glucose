#Glucose assays
#on trial Rhizo and Sui
#ran on Jul 21 2022
#Using sigma aldrich glucose assay kit

#
Time <- c(0,0,0,0,0,0,2,2,2,2,2,2,4,4,4,4,4,4)
Taxa <- c("Sui","Sui","Sui","Rhizo","Rhizo","Rhizo","Sui","Sui","Sui","Rhizo","Rhizo","Rhizo","Sui","Sui","Sui","Rhizo","Rhizo","Rhizo")
Glucose <- runif(18, min = 0, max = 1)
#sample(c("female","male"), 500, replace=TRUE

df <- data.frame(Time, Taxa,Glucose)
df2<-df %>%
  group_by(Time,Taxa)%>%
  summarise(Glucose_mean = mean(Glucose,na.rm=T))

df_plot<-ggplot(df2, aes(x = Time, y = Glucose_mean, color = Taxa,fill= Taxa)) +
  geom_point(shape=21,size=3)+
  geom_path()+
  scale_color_manual(values=c("#00A7C4","#6A4570"))+
  scale_fill_manual(values=c("#00A7C480","#6A457080"))+
  theme_bw()
df_plot

####standard curve#####
Concentration<-c(0,20,40,60,80)
Abs<-c(0.000,0.154,0.219,0.313,0.346)
sc <- data.frame(Concentration, Abs)
sc_plot<-ggplot(sc, aes(x = Concentration, y = Abs)) +
  geom_point(shape=21,size=3)+
  geom_smooth(method="lm",se=F)+
  xlab("Glucose Concentration (ug/ml)")+
  ylab("Absorbance, 540nm")
sc_plot

lmfit<-lm(Concentration ~ Abs, data=sc)
summary(lmfit)
#R2 = 0.9313
#p val = 0.005
#next: y = b0 + b1*x + e

#for calculating glucose []:
#mutate(concentration=-6.007+(Abs*222.900)+8.291)

#t0 calculations
Abs<-c(0.264,0.262,0.284,0.290)
Bottle<-c("Bottle1","Bottle2","Bottle3","Bottle4")
t0_data<-data.frame(Bottle, Abs)
t0_data<-t0_data %>%
  mutate(Concentration=((-6.007+(Abs*222.900)+8.291)/0.4))
View(t0_data)
mean(t0_data$Concentration)
#mean = 63.6
mean(t0_data$Abs)
#mean = 0.275

Abs<-c(0.275,0.275,0.275,0.275,0.037,0.034,0.029,0.278,0.142,0.093,0.020,0.018,0.023,0.043,0.021,0.007)
Taxa<-c("Rhi","Sui","Rhi","Sui","Rhi","Rhi","Rhi","Sui","Sui","Sui","Rhi","Rhi","Rhi","Sui","Sui","Sui")
Trial<-c("One","One","Two","Two","One","One","Two","One","One","Two","One","One","Two","One","One","Two")
Time<-c(0,0,0,0,2,2,2,2,2,2,4,4,4,4,4,4)
glucose<-data.frame(Trial,Time,Taxa,Abs)
glucose1<-glucose %>%
  mutate(Concentration=((-6.007+(Abs*222.900)+8.291)/0.4))


glucose2<-glucose1 %>%
  group_by(Time,Taxa,Trial)%>%
  summarise(Glucose_mean = mean(Concentration,na.rm=T))%>%
  mutate(gL=Glucose_mean*0.001)


glu_plot<-ggplot(glucose2, aes(x = Time, y = Glucose_mean, color = Taxa,fill= Taxa,shape=Trial)) +
  geom_point(size=3)+
  scale_shape_manual(values=c(21,22))+
  geom_path()+
  scale_color_manual(values=c("#00A7C4","#6A4570"))+
  scale_fill_manual(values=c("#00A7C480","#6A457080"))+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Glucose Concentration (μg/ml)")+
  ylim(0,160)
  
glu_plot

glu_plot2<-ggplot(glucose2, aes(x = Time, y = gL, color = Taxa,fill= Taxa,shape=Trial)) +
  geom_point(size=3)+
  scale_shape_manual(values=c(21,22))+
  geom_path()+
  scale_color_manual(values=c("#00A7C4","#6A4570"))+
  scale_fill_manual(values=c("#00A7C480","#6A457080"))+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Glucose Concentration (g/l)")

glu_plot2

glucose3<-glucose1 %>%
  group_by(Time,Taxa)%>%
  summarise(Glucose_mean = mean(Concentration,na.rm=T))%>%
  mutate(gL=Glucose_mean*0.001)
glu_plot3<-ggplot(glucose3, aes(x = Time, y = Glucose_mean, color = Taxa,fill= Taxa)) +
  geom_point(shape=21,size=3)+
  geom_path()+
  scale_color_manual(values=c("#00A7C4","#6A4570"))+
  scale_fill_manual(values=c("#00A7C480","#6A457080"))+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Glucose Concentration (μg/ml)")+
  ylim(0,160)

glu_plot3



