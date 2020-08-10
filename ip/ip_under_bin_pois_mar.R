miss='mar'
vers='_v5'
misspct = c(2,5,10,20,30)
seed = 100217
namefile = 'mar.under.bin.pois.ip.RData'
nsim = 1000

library(mice)
library(mitools)
library(survey)

dirdata = '/netscr/baldoni/Cai/Data_aug2017/IPW/'
dirwts = '/netscr/baldoni/Cai/Data_aug2017/IPW_Underspec/'
dirwork = '/netscr/baldoni/Cai/Codes_aug2017/Y_missing/'
diroutp = '/netscr/baldoni/Cai/Output_aug2017/Y_missing/'

setwd(dirwork)

files = paste0(dirdata,'widewt_samp_mar2017_ip_',1:nsim,'.csv')
files.wts = paste0(dirwts,'widewt_samp_mar2017_ip_',1:nsim,'.csv')

foo = function(miss,vers,cut,misspct,seed){
  label = paste0('bin_pois_',cut,'_',miss)
  df = data.frame()
  
  ymiss = paste0('y_',miss,vers,'_',misspct)
  yimp = paste0('y_bin_',cut,'_',miss,'_',misspct,'_imp')
  timp = paste0('x6_',miss,'_',misspct,'_imp')
  allcomp = paste0('allcomp_',misspct)
  
  for(i in 1:length(files)){
    dat = read.csv(file=files[i],header=T)
    dat.wts = read.csv(file=files.wts[i],header=T)
    
    simnum = i
    cat(simnum)
    ###################s
    #Creating variables
    dat$strat1 = 1*(dat$strat==1)
    dat$strat2 = 1*(dat$strat==2)
    dat$strat3 = 1*(dat$strat==3)
    dat$strat4 = 1*(dat$strat==4)
    
    for(j in 1:length(ymiss)){
      dat.anal = dat
      dat.anal[[yimp[j]]] = ifelse(dat.anal[[ymiss[j]]]==0,dat.anal[[paste0('y_bin_gfr_',cut,'_v3')]],NA)
      dat.anal[[timp[j]]] = ifelse(dat.anal[[ymiss[j]]]==0,dat.anal[['x6']],NA)
      dat.anal[[allcomp[j]]] = ifelse(dat.anal[[ymiss[j]]]==0,1,0)

      ### Running Multiple Imputation
      subvar = c('BGid','strat','bghhsub_s2','subid', #Design variables
                 'strat1','strat2','strat3','age_base','x2','x8','x12','x13','x14', #Variables that Poulami is using in her 'Under'-specified models
                 timp[j],yimp[j], #Missing variables
                 paste0('y1_bin_gfr_',cut,'_v3'))
      subdat = subset(dat.anal,select=subvar)
      subdat = merge(x=subdat,y=subset(dat.wts,select=c('subid',paste0('W_ip_',miss,vers,'_',misspct[j]))),by='subid',all.x=T)
      
      subdat$response = subdat[[yimp[j]]]
      subdat$baseline = subdat[[paste0('y1_bin_gfr_',cut,'_v3')]]
      subdat$x6imp = subdat[[timp[j]]]
      subdat$wts = subdat[[paste0('W_ip_',miss,vers,'_',misspct[j])]]
    
      ### Analyzing data ###
      design = svydesign(id=~BGid, strata=~strat, weights=~wts, data=subdat)
      model = svyglm(response~x8+x12+x13+x14+age_base+offset(log(x6imp)),subset=(baseline==0),
                     family=quasipoisson(link = "log"),design=design)
      
      df = rbind(df,data.frame(sim=simnum,missing=misspct[j],
                               par=c('Int','x8','x12','x13','x14','age_base'),
                               results=model$coefficients,se=sqrt(diag(model$cov.unscaled)),
                               X.lower=confint(model)[,1],upper.=confint(model)[,2],missInfo='0'))
    }
  }
  colnames(df) = c('sim','misspct','par','beta','se','lb','ub','missinfo') 
  rownames(df) = NULL
  return(df)
}

df.low = foo(miss=miss,vers=vers,cut='low',misspct=misspct,seed=seed)
df.hi = foo(miss=miss,vers=vers,cut='hi',misspct=misspct,seed=seed)
save(df.low,df.hi,file=paste0(diroutp,namefile))

