library("RcmdrMisc", lib.loc="~/R/win-library/3.5")
library("reshape", lib.loc="~/R/win-library/3.5")
source('C:/Users/ajr115/OneDrive/OWL/OWL_colors.R')
OWL <- readXL("C:/Users/ajr115/OneDrive/OWL/OWL.xlsx",sheet="Summary",header=TRUE,
            stringsAsFactors = TRUE)
team=1
rr <- c(levels(OWL$Role),"OWL")
pl <- function(r) {
  s <- OWL[(OWL$Role %in% r)&(OWL$Average>=15),]
  L=length(s$Average)
  num=10
  left=max(L-num,0)+1
  right=L
  o <- s[order(s$Average,decreasing=FALSE)[left:right],]
  par("mar"=c(3,3,2,1))
  p <- barplot(o$Average,horiz = TRUE,names=NULL,
               xaxs="i",yaxs="i",main=r[length(r)],xaxis=NULL,mgp=c(2,1,0.25),
               col=txtcolors[o$Team],las=1)
  text(o$Average/2,p,o$Player,srt=0,col = barcolors[o$Team],
       font=2,cex=1.618)
  text(o$Average-1.25,p,round(o$Average,1),srt=-90,col = barcolors[o$Team],
       font=2,cex=1.1)
  box("figure")
}
if(team==0){
  par(mfrow=c(1,4))
  pl("Tank")
  pl("Support")
  pl("Offense")
  pl(rr)
}
if(team==1){
  o=OWL[order(OWL$Team),]
  o=o[o$Average!=0,]
  o=melt(o,id="Player",na.rm = TRUE,id.vars=c("Player","Team"),
              measure.vars = c('Pts.10.Wk1',
                               'Pts.10.Wk2',
                               'Pts.10.Wk3',
                               'Pts.10.Wk4',
                               'Pts.10.Wk5'))
  o=o[o$value!=0,]
  o=o[order(o$Team),]
  v <- ggplot(o)+
    geom_boxplot(show.legend = FALSE,fill=txtcolors,varwidth=TRUE,aes(x=Team,y=value))+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=2,binwidth = 0.5,
                 show.legend = FALSE,fill=barcolors[o$Team],color="#000000",
                 aes(x=Team,y=value))+
    theme(axis.text.x = element_text(angle=60,hjust=1),axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),legend.position = "none")+
          labs(y="Points per 10")
  plot(v)
}