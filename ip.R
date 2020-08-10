dirpop = getwd()
diroutp = paste0(dirpop,'ip/Output/')
setwd(dirpop)
nsim=1000

gentable = function(miss.table,true.par,level,miss,method,nsim=1000){
  data.frame(Parameter=miss.table$par,true_value=rep(true.par,nsim),
             Miss=miss,Pct=miss.table$misspct,Method=method,dat.Estimate=miss.table$beta,
             dat.Inci=as.numeric((rep(true.par,nsim) >= miss.table$lb) & (rep(true.par,nsim) <= miss.table$ub)),
             dat.Dat_num=miss.table$sim,Prev=level,dat.Se=miss.table$se)
}

### Pop Data ###
load(paste0(dirpop,'pop.RData'))
par.low = fit.pois.low$coefficients
par.hig = fit.pois.hig$coefficients
rm(list=setdiff(ls(),c('dirpop','diroutp','par.low','par.hig','gentable')))

#### MCAR (Under) ###
load(paste0(diroutp,'mcar.under.bin.pois.ip.RData'))
df.mcar.under.low = df.low
df.mcar.under.hig = df.hi
rm(df.hi,df.low)

#### MCAR (Exact) ###
load(paste0(diroutp,'mcar.bin.pois.ip.RData'))
df.mcar.exact.low = df.low
df.mcar.exact.hig = df.hi
rm(df.hi,df.low)

#### MCAR (Over) ###
load(paste0(diroutp,'mcar.over.bin.pois.ip.RData'))
df.mcar.over.low = df.low
df.mcar.over.hig = df.hi
rm(df.hi,df.low)

table.ipw.mcar = rbind(gentable(miss.table=df.mcar.under.low,true.par=par.low,level='Low',method='IP Under',miss='MCAR'),
                       gentable(miss.table=df.mcar.under.hig,true.par=par.hig,level='High',method='IP Under',miss='MCAR'),
                       gentable(miss.table=df.mcar.exact.low,true.par=par.low,level='Low',method='IP Exact',miss='MCAR'),
                       gentable(miss.table=df.mcar.exact.hig,true.par=par.hig,level='High',method='IP Exact',miss='MCAR'),
                       gentable(miss.table=df.mcar.over.low,true.par=par.low,level='Low',method='IP Over',miss='MCAR'),
                       gentable(miss.table=df.mcar.over.hig,true.par=par.hig,level='High',method='IP Over',miss='MCAR'))

#### MAR (Under) ###
load(paste0(diroutp,'mar.under.bin.pois.ip.RData'))
df.mar.under.low = df.low
df.mar.under.hig = df.hi
rm(df.hi,df.low)

#### MAR (Exact) ###
load(paste0(diroutp,'mar.bin.pois.ip.RData'))
df.mar.exact.low = df.low
df.mar.exact.hig = df.hi
rm(df.hi,df.low)

#### MAR (Over) ###
load(paste0(diroutp,'mar.over.bin.pois.ip.RData'))
df.mar.over.low = df.low
df.mar.over.hig = df.hi
rm(df.hi,df.low)

table.ipw.mar = rbind(gentable(miss.table=df.mar.under.low,true.par=par.low,level='Low',method='IP Under',miss='MAR'),
                      gentable(miss.table=df.mar.under.hig,true.par=par.hig,level='High',method='IP Under',miss='MAR'),
                      gentable(miss.table=df.mar.exact.low,true.par=par.low,level='Low',method='IP Exact',miss='MAR'),
                      gentable(miss.table=df.mar.exact.hig,true.par=par.hig,level='High',method='IP Exact',miss='MAR'),
                      gentable(miss.table=df.mar.over.low,true.par=par.low,level='Low',method='IP Over',miss='MAR'),
                      gentable(miss.table=df.mar.over.hig,true.par=par.hig,level='High',method='IP Over',miss='MAR'))

#### MNAR (Under) ###
load(paste0(diroutp,'mnar.under.bin.pois.ip.RData'))
df.mnar.under.low = df.low
df.mnar.under.hig = df.hi
rm(df.hi,df.low)

#### MNAR (Exact) ###
load(paste0(diroutp,'mnar.bin.pois.ip.RData'))
df.mnar.exact.low = df.low
df.mnar.exact.hig = df.hi
rm(df.hi,df.low)

#### MNAR (Over) ###
load(paste0(diroutp,'mnar.over.bin.pois.ip.RData'))
df.mnar.over.low = df.low
df.mnar.over.hig = df.hi
rm(df.hi,df.low)

table.ipw.mnar = rbind(gentable(miss.table=df.mnar.under.low,true.par=par.low,level='Low',method='IP Under',miss='MNAR'),
                       gentable(miss.table=df.mnar.under.hig,true.par=par.low,level='High',method='IP Under',miss='MNAR'),
                       gentable(miss.table=df.mnar.exact.low,true.par=par.low,level='Low',method='IP Exact',miss='MNAR'),
                       gentable(miss.table=df.mnar.exact.hig,true.par=par.hig,level='High',method='IP Exact',miss='MNAR'),
                       gentable(miss.table=df.mnar.over.low,true.par=par.hig,level='Low',method='IP Over',miss='MNAR'),
                       gentable(miss.table=df.mnar.over.hig,true.par=par.hig,level='High',method='IP Over',miss='MNAR'))

write.csv(table.ipw.mcar,file='IP_MCAR.csv',row.names=F)
write.csv(table.ipw.mar,file='IP_MAR.csv',row.names=F)
write.csv(table.ipw.mnar,file='IP_MNAR.csv',row.names=F)
