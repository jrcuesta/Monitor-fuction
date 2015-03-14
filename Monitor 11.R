monitor10ftest<-function(id,ref,pred){
  options(digits=3)
  class(ref)<-"numeric"
  class(pred)<-"numeric"
  Table1<-cbind(id,ref,pred)
  Table2<-Table1[(Table1[,"id"]!=0)&(Table1[,"ref"]!=0)&(Table1[,"pred"]!=0),]
  id<-Table2[,1]
  ref<-Table2[,2]
  pred<-Table2[,3]
  res<-ref-pred
  Table3<-as.data.frame(cbind(id,ref,pred,res))
  Table4<-Table3[order(Table3$ref),]
  res<-Table4[,4]
  n<-length(Table4[,2])
  if(n<20) cat("More than 20 samples are needed to run the Validation","\n")
  cat("Nº Validation Samples  =",n,"\n")
  ref.avg<-mean(ref)
  pred.avg<-mean(pred)
  max.ref<-summary(ref)[6]
  max.pred<-summary(pred)[6]
  min.ref<-summary(ref)[1]
  min.pred<-summary(pred)[1]
  range.ref<-summary(ref)[6]-summary(ref)[1]
  range.pred<-summary(pred)[6]-summary(pred)[1]
  sd.ref<-sd(ref)
  sd.pred<-sd(pred)
  covxy<-cov(pred,ref)
  var.pred<-var(pred)
  par(mfrow=c(3,2))
  
  #################################################################
  # Superponer histogramas de distribución de
  # población  de las muestras de referencia
  # y de las muestras predichas.(blue and red)
  hist(ref,breaks=10,col=rgb(1,0,0,0.5),xlim=c(min.ref-0.1*min.ref,
                                               max.ref+0.1*max.ref),
       main="Ref vs Predicted populations",sub="Red=Ref / Blue=Pred")
  hist(pred,breaks=10,col=rgb(0,0,1,0.5), add=T)
  #box()
  #################################################################
  hist(res,freq=FALSE,col="blue",main="Residuals Histogram",
       sub="Compared with Normal Density")
  EQSmin<-min(res)
  EQSmax<-max(res)
  EQSmean<-mean(res)
  EQSsd<-sd(res)
  X1<-seq(EQSmin,EQSmax,by=0.01)
  lines(X1,dnorm(X1,EQSmean,EQSsd),col="red")
  l<-seq(1:n)
  boxplot(ref,pred,main="Boxplot Ref vs Pred",col="green")
{rmsep<-sqrt(sum((ref-pred)^2)/n)
 cat("RMSEP    :",rmsep,"\n")}
{(bias<-mean(res))
 cat("Bias     :",bias,"\n")}
{sep<-sd(res)
 cat("SEP      :",sep,"\n")}
{r<-cor(ref,pred)
 cat("Corr     :",r,"\n")}
{rsq<-(r^2)
 cat("RSQ      :",rsq,"\n")}
{slope<-covxy/var.pred
 cat("Slope    :",slope,"\n")}
{intercept<-ref.avg-(slope*pred.avg)
 cat("Intercept:",intercept,"\n")}
{RER<-range.ref/sep        # RER="Range Error Ratio"
 cat("RER      :",RER,"  ")
 if(RER>41) cat("Excellent","\n")
 if((RER>31)&(RER<41))cat("Very Good","\n")
 if((RER>21)&(RER<31)) cat("Good","\n")
 if((RER>13)&(RER<21)) cat("Fair","\n")
 if((RER>7)&(RER<13)) cat("Poor","\n")
 if(RER<7) cat("Very Poor","\n")
}
{RPD<-sd.ref/sep    		# RPD="Ratio SEP to SD"
 cat("RPD      :",RPD,"  ")
 if(RPD>8.1) cat("Excellent","\n")
 if((RPD>6.5)&(RPD<8.1))cat("Very Good","\n")
 if((RPD>5)&(RPD<6.5)) cat("Good","\n")
 if((RPD>3.1)&(RPD<5)) cat("Fair","\n")
 if((RPD>2.4)&(RPD<3.1)) cat("Poor","\n")
 if(RPD<2.4) cat("Very Poor","\n")
}		
######### Bias adjustment  ##################################################
{ttab<-qt(0.975,n)
 BCL<-((ttab*sep)/sqrt(n))
 blimit<-sep*BCL
 cat("Bias limit (+/-):",blimit,"\n")}
{if (abs(bias)>blimit)cat("***Bias adjustment is recommended***",sep="","\n")
 else
   cat("***Bias adjustment in not necessary***",sep="","\n")}  
####### Residiual plot 3 #####################################################
plot(res~l,main="Residuals",ylim=c(-4*sep,4*sep),
     sub="orange 95% prob / red 99,8% prob",col="Blue")
#text(res,col=1)
grid(3,3,col = "lightgray", lty = "dotted") 
abline(h=0,col="blue")
abline(h=(2*sep),col="orange")
abline(h=(-2*sep),col="orange")
abline(h=(3*sep),col="red")
abline(h=(-3*sep),col="red")
####### X-Y Plot ###############################################################
plot(ref~pred,main="X-Y plot",ylim=c((min.ref-0.3*range.ref),
                                     (max.ref+0.3*range.ref)),xlim=c((min.ref-0.3*range.ref),
                                                                     (max.ref+0.3*range.ref)),sub="orange 95% prob / red 99,8% prob",
     col="Blue",xlab="predicted",ylab="reference")
#text(ref,pred,col=1)
abline(0,1,col="blue")
abline(intercept,slope,col="red")
abline(0+(2*sep),1,col="orange",lty=4)
abline(0-(2*sep),1,col="orange",lty=4)
abline(0+(3*sep),1,col="red",lty=4)
abline(0-(3*sep),1,col="red",lty=4)
####### Residuals corrected ####################################################
res.corr2<-(ref-(intercept+(slope*pred)))^2
sumres2<-(sum(res.corr2))/(n-2)
Sres<-sqrt(sumres2)
#Sres=Residual Standard Deviation is like the SEP when the predicted values
#     are corrected for slope and intercept)
cat("Residual Std Dev is :",Sres,"\n")
###### Slope adjustment  #######################################################
{ttab.slp<-qt(0.975,n)
 tobs.slp<-(abs(slope-1))*(sqrt((var.pred*(n-1))/(Sres^2)))}
{if (tobs.slp>=ttab.slp) cat("***Slope adjustment is recommended***",sep="","\n")
 else
   cat("***Slope adjustment in not necessary***",sep="","\n")}	
########## Tablas de Salida #####################################################
res68<-subset(Table4,res<=abs(sep))
N1<-length(res68[,4])
cat("Residuals into 68 %   prob=",N1,"\n")
res95<-subset(Table4,res<=abs(2*sep))
N2<-length(res95[,4])
cat("Residuals into 95 %   prob=",N2,"\n")
res99.5<-subset(Table4,res<=abs(3*sep))
N3<-length(res99.5[,4])
cat("Residuals into 99.5 % prob=",N3,"\n")
reswarning<-subset(Table4,res>abs(2*sep)& res<abs(3*sep))
resaction<-subset(Table4,res>abs(3*sep))
list(Table4=Table4,ResWarning=reswarning,ResAction=resaction)
}
