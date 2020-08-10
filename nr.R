dirpop = getwd()
diroutp = paste0(dirpop,'nr/Output/')
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

#### MCAR (Exact) ###
load(paste0(diroutp,'mcar.bin.pois.nr.RData'))
df.mcar.exact.low = df.low
df.mcar.exact.hig = df.hi
rm(df.hi,df.low)

table.nr.mcar = rbind(gentable(miss.table=df.mcar.exact.low,true.par=par.low,level='Low',method='MI Exact',miss='MCAR'),
                       gentable(miss.table=df.mcar.exact.hig,true.par=par.hig,level='High',method='MI Exact',miss='MCAR'))

#### MAR (Exact) ###
load(paste0(diroutp,'mar.bin.pois.nr.RData'))
df.mar.exact.low = df.low
df.mar.exact.hig = df.hi
rm(df.hi,df.low)

table.nr.mar = rbind(gentable(miss.table=df.mar.exact.low,true.par=par.low,level='Low',method='MI Exact',miss='MAR'),
                      gentable(miss.table=df.mar.exact.hig,true.par=par.hig,level='High',method='MI Exact',miss='MAR'))

#### MNAR (Exact) ###
load(paste0(diroutp,'mnar.bin.pois.nr.RData'))
df.mnar.exact.low = df.low
df.mnar.exact.hig = df.hi
rm(df.hi,df.low)

table.nr.mnar = rbind(gentable(miss.table=df.mnar.exact.low,true.par=par.low,level='Low',method='MI Exact',miss='MNAR'),
                      gentable(miss.table=df.mnar.exact.hig,true.par=par.hig,level='High',method='MI Exact',miss='MNAR'))

write.csv(table.nr.mcar,file='NR_MCAR.csv',row.names=F)
write.csv(table.nr.mar,file='NR_MAR.csv',row.names=F)
write.csv(table.nr.mnar,file='NR_MNAR.csv',row.names=F)
