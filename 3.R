#!/usr/bin/env Rscript

# clear workspace
rm(list=ls())
# Function to load packages
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) ) }
}

# load libraries
packs=c('cem','haven', 'car', 'ggplot2', 'RColorBrewer', 'texreg', 'stargazer', 'tidyverse') #
loadPkg(packs)

# ---------------------------------------------
# set up data
# ---------------------------------------------
# load
ceo_850 = as.data.frame(read_spss('Data/CEO_850.sav'))
ceo_857 = as.data.frame(read_spss('Data/CEO_857.sav'))

# id treatment
ceo_850$treat = 0
ceo_857$treat = 1

# stack 
df = bind_rows(list(ceo_850, ceo_857))

# recode preferences for indep 2 -> 0
df$P31 = car::recode(df$P31, "2=0")

# recode employment
df$C401 = car::recode(df$C401, "2=0; 1=2; 3=1")

# replace all 98 and 99
df[df == 98] = NaN
df[df == 99] = NaN

# create num_p_cata
df = df[complete.cases(df$'C110'),]
df = df[complete.cases(df$'C120'),]

df$num_p_cata = 0
for (ind in c(1:nrow(df))){
  count = 0
  if( df[ind, 'C110'] == 1){ count = count + 1 }
  if( df[ind, 'C120'] == 1){ count = count + 1 }
  df[ind, 'num_p_cata'] = count
}

df_c_2017 = df[c('P31', 'treat', 'C705', 'num_p_cata', 'C500', 'C800',
                      'C401', 'Edat_CEO', 'SEXE')]

# only complete cases
df_c_2017 = df_c_2017[complete.cases(df_c_2017),]
# only complete cases
df_c_2017 = df_c_2017[complete.cases(df_c_2017),]

# ---------------------------------------------
# matching
# ---------------------------------------------

# compute size of the treated and control groups
tr = which(df_c_2017$treat==1)
ct = which(df_c_2017$treat==0)
ntr = length(tr)
ntr
nct = length(ct)
nct
# unadjust and likely biased difference in means
mean(df_c_2017$P31[tr]) - mean(df_c_2017$P31[ct])

vars = c('C705', 'num_p_cata', 'C500', 'C800',
         'C401', 'Edat_CEO', 'SEXE')
todrop = c('treat', 'P31')
# L1 imbalance before matching
imb = imbalance(group=df_c_2017$treat, data=df_c_2017[vars]) #L1 .781

C705.grp = list(c(0), c(1), c(2), c(3))
C500.grp = list(c(1,2,3), c(4,5,6,7) , c(8,9,10,11))

mat = cem(treatment = 'treat', drop='P31', data=df_c_2017,
          grouping=list(C705=C705.grp, C500=C500.grp), eval.imbalance=TRUE)
# L1 imbalance after matching
mat

# fix table
df_c_2017 = as.data.frame(df_c_2017)

est = att(mat, P31 ~ treat, data=df_c_2017, model = 'logit')
# treatment effect
est


# get table
# pull out estimate and coef for plotting
eff = data.frame(est = est$att.model[1,2],
                 std = est$att.model[2,2])

eff$conf95 = paste0('(', round(eff$est - 1.96*eff$std,3), ',',
                     round(eff$est + 1.96*eff$std,3), ')')


eff %>%
  mutate(Outcome = 'Independence', Estimate = round(est,3)) %>%
  select(Outcome, Estimate, Interval = conf95) %>%
  stargazer(summary = F, rownames = F,
            title = 'Placebo test matched model, where March 2017 poll set to `control` and July 2017 poll set to `treatment.',
            label = 'est-placebo',
            out = 'Figures/effects-table-placebo.tex')
