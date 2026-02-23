library(DDD)
cbt<-branching.times(Anolis.tre)
cbt

bd_res_ddd<-bd_ML(brts=cbt,missnumspec=0,cond=1,btorph=0)
bd_res_ddd

bdvar_res_ddd<-bd_ML(brts=cbt,missnumspec=0,cond=1,tdmodel=1,initparsopt=c(0.1,0.1,0.1,0.1),idparsopt=1:4,btorph=0)
bdvar_res_ddd

dd_res_ddd<-dd_ML(brts=cbt,missnumspec=0,cond=1,btorph=0)
dd_res_ddd

lnL<-c(bd_res_ddd$loglik,bdvar_res_ddd$loglik,dd_res_ddd$loglik)
k<-c(bd_res_ddd$df,bdvar_res_ddd$df,dd_res_ddd$df)
aic<-2*k-2*lnL
data.frame(model=c("birth-death","variable-rate","density-dependent"),logLik=lnL,df=k,AIC=aic)
