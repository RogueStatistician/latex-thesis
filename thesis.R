rm(list=ls())
library(ggplot2)
library(msnk)
library(xtable)

dd<-NA
O <- list(
  matrix(c(1,1,1,2.5),nrow = 2 ),
  matrix(c(1,0,0,2.5),nrow = 2 ),
  matrix(c(1,-1,-1,2.5),nrow = 2 ),
  matrix(c(1,0,0,1),nrow = 2 )
)
alfa <- list(
  c(2,10),
  c(0,10),
  c(2,3),
  c(2,-3),
  c(2,1),
  c(2,-1),
  c(-2,1),
  c(1,0),
  c(0,0)
)

results <- list()

k<-1
for(i in 1:length(O)){
  for(j in 1:length(alfa)){
    mard<-mardia(Omega = O[[i]],alpha = alfa[[j]])
    malk<-malkaf(Omega = O[[i]],alpha = alfa[[j]])
     kol<-kollo(Omega = O[[i]],alpha = alfa[[j]],center=T)
     #dd<-ddectional(Omega = O[[i]],alpha = alfa[[j]])
    sriv<-srivastava(Omega = O[[i]],alpha = alfa[[j]])
     mor<-mori.rohatgi.szekeley(Omega = O[[i]],alpha = alfa[[j]])
     results[[k]]<-list(Omega=O[[i]],alpha=alfa[[j]],mardia=mard,malkovich=malk,kollo=kol,direction=dd,srivastava=sriv,mori=mor)
    k<-k+1
  }
}
results
###############################################################################################################

O2 <- list(
  matrix(c(1,0,0,0,2.5,0,0,0,2.5),nrow = 3 ),
  matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3 ),
  matrix(c(1,1,1,1,2.5,1,1,1,2.5),nrow = 3 )
)

alfa2 <- list(
  c(0,0,0),
  c(2,10,10),
  c(2,3,3),
  c(10,0,0),
  c(2,-3,-3),
  c(2,-1,-1),
  c(2,1,1)
)

results2 <- list()

k<-1
for(i in 1:length(O2)){
  for(j in 1:length(alfa2)){
    mard<-mardia(Omega = O2[[i]],alpha = alfa2[[j]])
    malk<-malkaf(Omega = O2[[i]],alpha = alfa2[[j]])
    kol<-kollo(Omega = O2[[i]],alpha = alfa2[[j]],center=T)
    #dd<-ddectional(Omega = O2[[i]],alpha = alfa2[[j]])
    sriv<-srivastava(Omega = O2[[i]],alpha = alfa2[[j]])
    mor<-mori.rohatgi.szekeley(Omega = O2[[i]],alpha = alfa2[[j]])
    results2[[k]]<-list(Omega=O2[[i]],alpha=alfa2[[j]],mardia=mard,malkovich=malk,kollo=kol,direction=dd,srivastava=sriv,mori=mor)
    k<-k+1
  }
}
results2

pr<-function(x){
  return(paste('$',print(x, floating=F, tabular.environment="bmatrix", 
        hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE,comment=F),'$'))
}
# LISTS TO LATEX TABLES

#UNIVARIATE 
row<-""
i<-1
for(i in 1:length(results)){
  attach(results[[i]])
  Om<-xtable(round(Omega,1),align=rep("",ncol(Omega)+1))
  A<-xtable(round(matrix(alpha,ncol=1),1),align=rep("",2))
  MAR<-paste0("$",round(mardia,2),"$")
  MAL<-paste0("$",round(malkovich,2),"$")
  SR<-paste0("$",round(srivastava,2),"$")
  DIR<-paste0("$",direction,"$")
  MO<-xtable(round(mori,1),align=rep("",ncol(mori)+1))
  KO<-xtable(round(kollo,1),align=rep("",ncol(kollo)+1))
  row<-paste(row, paste(pr(Om),pr(A),MAR,MAL,SR,DIR,pr(MO),pr(KO),sep = '&'),"\\\\")
  detach(results[[i]])
}
row<-paste('$\\Omega$& $\\alpha$ & Mardia & MalkAf  & Sriv & Directional & MoRoSze & Kollo \\\\
           \\midrule\\endhead',row,'
\\midrule[\\heavyrulewidth] % use "thick" \\midrule instead of \\bottomrule
           \\caption{Indici di Curtosi scalari per la distribuzione Normale Asimmetrica Multidimensionale al variare di $\\Omega$ e $\\alpha$}',sep='\n')
a<-file('./tab1.tex')
writeLines(row,a)
close(a)



