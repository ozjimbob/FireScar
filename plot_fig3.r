## Plot figure 3

# For each sampling method - random or targeted
# For each fire regime (e1, e2, e3, e4)
# For NFR, MFIall, MFI10% MFI25%
# For sample size 10,25,50,100,250,500,census,real

# function to make a mini data frame of our chosen variables
library(ggplot2)

make_plot_frame=function(this_data,x,names){
  this_of=subset(this_data,select=x)
  this_of=unlist(this_of)
  this_names=factor(rep(names,each=100),levels=names)
  this_of=data.frame(value=this_of,var=this_names)
  this_of
}

## Random Figure

data_l1=read.csv("e1_l1/sim_list_stats1.csv")
data_l2=read.csv("e1_l2/sim_list_stats1.csv")
data_l3=read.csv("e1_l3/sim_list_stats1.csv")
data_l4=read.csv("e1_l4/sim_list_stats1.csv")


# Window 1 - MFI = all

col_seq=rep(c("blue","red","darkgreen","orange"),4)
vars=c("MFI_25_T","MFI_50_T","MFI_100_T","MFI_250_T")
names=c("25","50","100","250")
this_frame1=make_plot_frame(data_l1,vars,names)
this_frame1$MFI=1

this_frame2=make_plot_frame(data_l2,vars,names)
this_frame2$MFI=5

this_frame3=make_plot_frame(data_l3,vars,names)
this_frame3$MFI=10

this_frame4=make_plot_frame(data_l4,vars,names)
this_frame4$MFI=20

this_frame_a=rbind(this_frame1,this_frame2,this_frame3,this_frame4)
this_frame_a$Filter="MFI (All)"


vars=c("MFI_25_T_2o","MFI_50_T_2o","MFI_100_T_2o","MFI_250_T_2o")
names=c("25","50","100","250")
this_frame1=make_plot_frame(data_l1,vars,names)
this_frame1$MFI=1

this_frame2=make_plot_frame(data_l2,vars,names)
this_frame2$MFI=5

this_frame3=make_plot_frame(data_l3,vars,names)
this_frame3$MFI=10

this_frame4=make_plot_frame(data_l4,vars,names)
this_frame4$MFI=20

this_frame_b=rbind(this_frame1,this_frame2,this_frame3,this_frame4)
this_frame_b$Filter="MFI (>2)"

vars=c("MFI_25_T_10","MFI_50_T_10","MFI_100_T_10","MFI_250_T_10")
names=c("25","50","100","250")
this_frame1=make_plot_frame(data_l1,vars,names)
this_frame1$MFI=1

this_frame2=make_plot_frame(data_l2,vars,names)
this_frame2$MFI=5

this_frame3=make_plot_frame(data_l3,vars,names)
this_frame3$MFI=10

this_frame4=make_plot_frame(data_l4,vars,names)
this_frame4$MFI=20

this_frame_c=rbind(this_frame1,this_frame2,this_frame3,this_frame4)
this_frame_c$Filter="MFI (10%)"

vars=c("MFI_25_T_25","MFI_50_T_25","MFI_100_T_25","MFI_250_T_25")
names=c("25","50","100","250")
this_frame1=make_plot_frame(data_l1,vars,names)
this_frame1$MFI=1

this_frame2=make_plot_frame(data_l2,vars,names)
this_frame2$MFI=5

this_frame3=make_plot_frame(data_l3,vars,names)
this_frame3$MFI=10

this_frame4=make_plot_frame(data_l4,vars,names)
this_frame4$MFI=20

this_frame_d=rbind(this_frame1,this_frame2,this_frame3,this_frame4)
this_frame_d$Filter="MFI (25%)"

this_frame=rbind(this_frame_a,this_frame_b,this_frame_c,this_frame_d)

this_frame$Filter=factor(this_frame$Filter,levels=c("MFI (All)","MFI (>2)","MFI (10%)","MFI (25%)"))

#aa=boxplot(value~MFI + var,data=this_frame,ylab=expression(MFI[all]),col=col_seq,border=col_seq,outline=FALSE)
pdf("Fig3.pdf",width=8,height=8)
p <- ggplot(this_frame, aes(factor(MFI), value))
p= p + geom_boxplot(aes(fill = factor(MFI),var),outlier.shape = NA) + facet_wrap( ~ Filter)+ coord_cartesian(ylim = c(0,75)) + scale_fill_manual(values = c("blue","red","darkgreen","orange"))
p=p+ theme_bw() + theme(panel.grid.major = element_line(colour = "white"),panel.grid.minor = element_line(colour = "white"))
p+labs(x = "Sample Size",y="Mean Fire Interval",fill="Actual MFI")
dev.off()



