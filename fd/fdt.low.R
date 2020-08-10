library(survey)

dirwd = "/pine/scr/b/a/baldoni/Cai/Visit2/Manuscript_MissingData/Output/"
dirdd = "/pine/scr/b/a/baldoni/Cai/Visit2/Manuscript_MissingData/Data/NRW/"

files = paste0(dirdd,'widewt_samp_mar2017_',1:1000,'.csv')

fdt = function(degree){
  
  df.wd = data.frame()
  df.w = data.frame()
  df = data.frame()
  
  cutoff = ifelse(degree=='_low_',90,ifelse(degree=='_',95,100))
  
  for(i in 1:length(files)){cat('File: ',i,'.\n')
    ### Reading data ###
        dat = data.table::fread(file=files[i])
    dat = subset(dat,select=c('strat','BGid','bghhsub_s2',paste0('y1_bin_gfr',degree,'v3'),
                              paste0('y_bin_gfr',degree,'v3'),'x12','x13','x14','x6','x8','age_base'))
    dat$logx6 = log(dat$x6)
    dat$baseline = as.numeric(dat[[paste0('y1_bin_gfr',degree,'v3')]])
    dat$response = as.numeric(dat[[paste0('y_bin_gfr',degree,'v3')]])
    dat$weights = dat$bghhsub_s2
    
    
    ### Analyzing data: Weights and design ###
    design = svydesign(id=~BGid, strata=~strat, weights=~bghhsub_s2, data=dat)
    model = svyglm(response~x12+x13+x14+x8+age_base+offset(logx6),subset=(baseline==0),
                 family=quasipoisson(link='log'),design=design)
    model.summary = summary(model)
    df.wd = rbind(df.wd,data.frame(sim=i,par=c('Int','x12','x13','x14','x8','age_base'),
                             model.summary$coefficients[,1:2],confint(model),0))
    
    ### Analyzing data: Weights and no design ###
    model = glm(response~x12+x13+x14+x8+age_base+offset(logx6),subset=(baseline==0),
                   family=quasipoisson(link='log'),data=dat,weights=bghhsub_s2)
    model.summary = summary(model)
    df.w = rbind(df.w,data.frame(sim=i,par=c('Int','x12','x13','x14','x8','age_base'),
                             model.summary$coefficients[,1:2],confint(model),0))
    
    ### Analyzing data: no Weights and no design ###
    model = glm(response~x12+x13+x14+x8+age_base+offset(logx6),subset=(baseline==0),
                family=quasipoisson(link='log'),data=dat)
    model.summary = summary(model)
    df = rbind(df,data.frame(sim=i,par=c('Int','x12','x13','x14','x8','age_base'),
                                 model.summary$coefficients[,1:2],confint(model),0))
    
  }
  colnames(df.wd) = c('sim','par','beta','se','lb','ub','misspct') 
  rownames(df.wd) = NULL
  colnames(df.w) = c('sim','par','beta','se','lb','ub','misspct') 
  rownames(df.w) = NULL
  colnames(df) = c('sim','par','beta','se','lb','ub','misspct') 
  rownames(df) = NULL
  return(list('df.wd'=df.wd,'df.w'=df.w,'df'=df))
}

fdt.fit = fdt(degree='_low_')
df.wd = fdt.fit$df.wd
df.w = fdt.fit$df.w
df = fdt.fit$df

save(df.wd,df.w,df,file=paste0(dirwd,'fdt.low.RData'))
