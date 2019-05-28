## Plot figure 2

# For each sampling method - random or targeted
# For each fire regime (e1, e2, e3, e4)
# For NFR, MFIall, MFI10% MFI25%
# For sample size 10,25,50,100,250,500,census,real

# function to make a mini data frame of our chosen variables
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

# Select annual small fires
names=c("10","25","50","100","250","500","All","Actual")

#################

pdf(file="Fig2.pdf",width=14,height=10)

par(mfrow=c(4,4),mai=c(0.8,.8,.010,.05),mar=c(4,4,0,0),oma=c(3,3,3,3),cex=1)


# MFI 1
vars=c("MFI_10","MFI_25","MFI_50","MFI_100","MFI_250","MFI_500","MFI_census","GridRealMFI")
this_frame=make_plot_frame(data_l1,vars,names)
aa=boxplot(value~var,data=this_frame,ylab=expression(MFI[all]),xlab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 2
this_frame=make_plot_frame(data_l2,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 3
vars=c("MFI_10_T","MFI_25_T","MFI_50_T","MFI_100_T","MFI_250_T","MFI_500_T","MFI_census","GridRealMFI")
this_frame=make_plot_frame(data_l3,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 4
vars=c("MFI_10_T","MFI_25_T","MFI_50_T","MFI_100_T","MFI_250_T","MFI_500_T","MFI_census","GridRealMFI")
this_frame=make_plot_frame(data_l4,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")

# MFI 2-trees

# MFI 1
vars=c("MFI_10_2o","MFI_25_2o","MFI_50_2o","MFI_100_2o","MFI_250_2o","MFI_500_2o","MFI_census_2o","MFI_grid_2o")
this_frame=make_plot_frame(data_l1,vars,names)
aa=boxplot(value~var,data=this_frame,ylab=expression(MFI['>2']),xlab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 2
this_frame=make_plot_frame(data_l2,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 3
this_frame=make_plot_frame(data_l3,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 4
this_frame=make_plot_frame(data_l4,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")


#################### MFI 10

# MFI 1
vars=c("MFI_10_10","MFI_25_10","MFI_50_10","MFI_100_10","MFI_250_10","MFI_500_10","MFI_census_10","MFI_grid_10")
this_frame=make_plot_frame(data_l1,vars,names)
aa=boxplot(value~var,data=this_frame,ylab=expression(MFI['10%']),xlab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 2
this_frame=make_plot_frame(data_l2,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 3
this_frame=make_plot_frame(data_l3,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 4
this_frame=make_plot_frame(data_l4,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="",ylab="",las=2,xaxt="n")
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")

### MFI 25


# MFI 1
vars=c("MFI_10_25","MFI_25_25","MFI_50_25","MFI_100_25","MFI_250_25","MFI_500_25","MFI_census_25","MFI_grid_25")
this_frame=make_plot_frame(data_l1,vars,names)
aa=boxplot(value~var,data=this_frame,ylab=expression(MFI['25%']),xlab="Annual Small",las=2)
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 2
this_frame=make_plot_frame(data_l2,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="Intermediate 1",ylab="",las=2)
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 3
this_frame=make_plot_frame(data_l3,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="Intermediate 2",ylab="",las=2)
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")
# MFI 4
this_frame=make_plot_frame(data_l4,vars,names)
aa=boxplot(value~var,data=this_frame,xlab="Bidecadal Large",ylab="",las=2)
rect(-1,aa$stats[2,8],9,aa$stats[4,8],col="#9999FF88",border=NA)
abline(h=aa$stats[3,8],col="#9999FF88")

dev.off()
# 
