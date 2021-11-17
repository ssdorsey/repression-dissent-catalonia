#!/usr/bin/env Rscript

#' Catalonia survey matching for estimation of the
#' effects of election suppression
#' Spencer Dorsey
#' started 6 December 2017

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
packs=c('haven', 'MatchIt', 'ggplot2', 'gridExtra', 'tidyverse', 
        'stargazer', 'ggplot2', 'RColorBrewer', 'texreg', 
        'hrbrthemes', 'Zelig')
loadPkg(packs)

# ------------------------------------------------------------------------------
# load + setup data
# ------------------------------------------------------------------------------
# ceo run and final day of fieldwork
ceo = c(712, 723,  733,  746,  758,  760,  774,  795, 804, 806, 816, 826, 835,
        838, 850, 857, 863, 874, 885, 901, 908)
dates = c('2013-02-14', '2013-06-13', '2013-11-14', '2014-04-15', '2014-10-23',
          '2014-12-13', '2015-03-02', '2015-06-24', '2015-10-27', '2015-11-23',
          '2016-03-08', '2016-07-13', '2016-11-03', '2016-12-17', '2017-03-21',
          '2017-07-11', '2017-10-29', '2018-01-30', '2018-04-27', '2018-07-14',
          '2018-11-12')
ceo_date = setNames(data.frame(ceo, dates), c('ceo', 'date'))
ceo_date$date = as.Date(ceo_date$date)

ceo_712 = as.data.frame(read_spss('Data/CEO_712.sav')) # P28 (1-4) and P39 (vote on referrendum) ***
ceo_723 = as.data.frame(read_spss('Data/CEO_723.sav')) # P28 (1-4) and P39 (vote on referrendum) ***
ceo_733 = as.data.frame(read_spss('Data/CEO_733.sav')) # P28 (1-4) and P39 (vote on referrendum) ***
ceo_746 = as.data.frame(read_spss('Data/CEO_746.sav')) # P30 (1-4) and P31a/b (vote on regerrendum)
ceo_758 = as.data.frame(read_spss('Data/CEO_758.sav')) # P30 (1-4) and P31a/b (vote on regerrendum)
ceo_760 = as.data.frame(read_spss('Data/CEO_760.sav')) # P30 (1-4) and P31 (yes or no) IN-PERSON
ceo_774 = as.data.frame(read_spss('Data/CEO_774.sav')) # P30 (1-4) and P31 (yes or no)
ceo_795 = as.data.frame(read_spss('Data/CEO_795.sav'))
ceo_804 = as.data.frame(read_spss('Data/CEO_804.sav'))
ceo_806 = as.data.frame(read_spss('Data/CEO_806.sav')) # IN-PERSON
ceo_816 = as.data.frame(read_spss('Data/CEO_816.sav'))
ceo_826 = as.data.frame(read_spss('Data/CEO_826.sav'))
ceo_835 = as.data.frame(read_spss('Data/CEO_835.sav'))
ceo_838 = as.data.frame(read_spss('Data/CEO_838.sav')) # IN-PERSON
ceo_850 = as.data.frame(read_spss('Data/CEO_850.sav'))
ceo_857 = as.data.frame(read_spss('Data/CEO_857.sav'))
ceo_863 = as.data.frame(read_spss('Data/CEO_863_withdates.sav'))
ceo_874 = as.data.frame(read_spss('Data/CEO_874.sav')) # IN-PERSON
ceo_885 = as.data.frame(read_spss('Data/CEO_885.sav'))
ceo_901 = as.data.frame(read_spss('Data/CEO_901.sav'))
ceo_908 = as.data.frame(read_spss('Data/CEO_908.sav'))


# ------------------------------------------------------------------------------
# recode what doesn't match
# ------------------------------------------------------------------------------
# P30
ceo_712[['P30']] = ceo_712[['P28']]
ceo_723[['P30']] = ceo_723[['P28']]
ceo_733[['P30']] = ceo_733[['P28']]

# C800
ceo_712[['C800']] = ceo_712[['C320']]
ceo_723[['C800']] = ceo_723[['C320']]
ceo_733[['C800']] = ceo_733[['C320']]

# C401
fix_C401 = function(frame){
  frame[['C401']] = NaN
  # Treballa
  frame[(frame[['C400']]==1) | (frame[['C400']]==2), 'C401'] = 1
  # No treballa
  frame[(frame[['C400']]==3) & (frame[['C410']] %in% 1:6), 'C401'] = 2
  # Està temporalment de baixa
  frame[(frame[['C400']]==3) & (frame[['C410']] == 7), 'C401'] = 3
  return(frame)
}

ceo_712 = fix_C401(ceo_712)
ceo_723 = fix_C401(ceo_723)
ceo_733 = fix_C401(ceo_733)

# SEXE
ceo_712[['SEXE']] = ceo_712[['sexe_enq']]
ceo_723[['SEXE']] = ceo_723[['sexe_enq']]
ceo_733[['SEXE']] = ceo_733[['sexe_enq']]

# edat
ceo_908[['Edat_CEO']] = ceo_908[['EDAT_CEO']]

# store all the data for later
all_ceo = list(ceo_712
              , ceo_723
              , ceo_733
              , ceo_746
              , ceo_758
              , ceo_760
              , ceo_774
              , ceo_795
              , ceo_804
              , ceo_806
              , ceo_816
              , ceo_826
              , ceo_835
              , ceo_838
              , ceo_850
              , ceo_857
              , ceo_863
              , ceo_874
              , ceo_885
              , ceo_901
              , ceo_908)

for(ii in 1:length(all_ceo)){
  all_ceo[[ii]]['date'] = as.Date(dates[ii])
  all_ceo[[ii]]['CEO'] = ceo[ii]
}


# columns to keep
keep_p30 = c('P30', 'C705', 'Edat_CEO', 'C500', 'C800',
         'C401', 'SEXE', 'C110', 'C120',
         'ANY', 'MES', 'CEO', 'date')

keep_p31 = c('P31', 'C705', 'C500', 'C800', 'C401', 'C110', 'C120', 'SEXE',
         'Edat_CEO', 'ANY', 'MES', 'DIA', 'CEO', 'date')


sub_bind_p30 = function(list_dfs){
  # function for combining the ceo dataframes for the 2014 analysis

  # remove columns I don't want
  for(ii in 1:length(list_dfs)){
    list_dfs[[ii]] = list_dfs[[ii]][keep_p30]
  }
  # bind them all up
  bound = bind_rows(list_dfs)
  
  # recode some variables
  bound[['P30']] = car::recode(bound[['P30']], "1=0; 2=; 3=0; 4=1")
  bound[['P30']] = as.numeric(bound[['P30']])
  bound[bound == 98] = NaN
  bound[bound == 99] = NaN

  # drop NaN
  bound = bound[complete.cases(bound[c('P30', 'C705', 'C500', 'C800', 'C401', 'C110',
                                       'C120', 'SEXE', 'ANY', 'CEO')]),]
  # create num_p_cata
  bound$num_p_cata = 0
  for (ind in c(1:nrow(bound))){
    count = 0
    if( bound[ind, 'C110'] == 1){ count = count + 1 }
    if( bound[ind, 'C120'] == 1){ count = count + 1 }
    bound[ind, 'num_p_cata'] = count
  }
  # return
  return(bound)
}

sub_bind_p31 = function(list_dfs){
  # function for combining the ceo dataframes for the 2017 analysis

  # remove columns I don't want
  for(ii in 1:length(list_dfs)){
    list_dfs[[ii]] = list_dfs[[ii]][keep_p31]
  }
  # bind them all up
  bound = bind_rows(list_dfs)
  # recode some variables
  bound[['P31']] = car::recode(bound[['P31']], "1=1; 2=0")
  bound[['P31']] = as.numeric(bound[['P31']])
  bound[bound == 98] = NaN
  bound[bound == 99] = NaN
  # drop NaN
  bound = bound[complete.cases(bound[c('P31', 'C705', 'C500', 'C800', 'C401', 'C110',
                                       'C120', 'SEXE')]),]
  # create num_p_cata
  bound$num_p_cata = 0
  for (ind in c(1:nrow(bound))){
    count = 0
    if( bound[ind, 'C110'] == 1){ count = count + 1 }
    if( bound[ind, 'C120'] == 1){ count = count + 1 }
    bound[ind, 'num_p_cata'] = count
  }
  # return
  return(bound)
}


# ------------------------------------------------------------------------------
# 2014 analysis
# ------------------------------------------------------------------------------
df14 = sub_bind_p30(all_ceo[4:7])
# ID the treatment columns
df14[['treat']] = 0
df14[(df14[['CEO']] == 774), 'treat'] = 1
# drop date cols after
df14[['ANY']] = df14[['MES']] = df14[['CEO']] = NULL

# matching vars
vars = c('C705', 'num_p_cata', 'C500', 'C800',
         'C401', 'Edat_CEO', 'SEXE')

# group variables
C705.grp = list(c(0), c(1), c(2), c(3))
C500.grp = list(c(1,2,3), c(4,5,6,7) , c(8,9,10,11))

# matching
mat = matchit(formula(paste0('treat ~ ', paste(vars, collapse = '+'))),
              data = df14, method = 'cem',
              grouping = list(C705=C705.grp,
                               C500=C500.grp))

# estimation
est = Zelig::zelig(P30 ~ treat, data = match.data(mat), model = 'ls',
                   cite = F, weights = 'weights')

# effect plus CI
est14 = data.frame(estimate = est$get_coef()[[1]][2],
                   pvalue = est$get_pvalue()[[1]][[2]],
                   observations = nrow(est$data))

# output to tables
labels = c('Distance', 'Lang: Catalan',
            'Number of Catalan Parents',
             'Education', 'Economic Class',
            'Employment Status', 'Age', 'Gender')

pre = summary(mat)$sum.all
pre = mutate_if(pre, is.numeric, round, 3)
pre = pre[,1:4]
rownames(pre) <- labels

post = summary(mat)$sum.matched
post = mutate_if(post, is.numeric, round, 3)
post = post[,1:4]
rownames(post) <- labels

stargazer(pre,
          summary = F,
          title = 'Pre-Balance Summary Statistics. 2014 Referendum Analysis.',
          label = 'pre-bal-phone-2014',
          out = 'Figures/pre-bal-phone-2014.tex')

stargazer(post,
          summary = F,
          title = 'Post-Balance Summary Statistics. 2014 Referendum Analysis.',
          label = 'Post-bal-phone-2014',
          out = 'Figures/Post-bal-phone-2014.tex')

# ------------------------------------------------------------------------------
# 2017 analysis
# ------------------------------------------------------------------------------
# ceo_850[['P31']]
ceo_date[,'index'] = 1:nrow(ceo_date)
df17 = sub_bind_p31(all_ceo[15:17])
# ceo_date
# id the treatment rows
df17[['treat']] = 0
df17[df17[['CEO']]==863, 'treat'] = 1
# drop the respondents after the 27th
df17 = df17[!((df17[['CEO']]==863) &
                (df17[['DIA']] >= 27)),]

df17['ANY'] = df17['MES'] = df17['DIA'] = NULL

# matching
mat = matchit(formula(paste0('treat ~ ', paste(vars, collapse = '+'))),
              data = df17, method = 'cem',
              grouping = list(C705=C705.grp,
                              C500=C500.grp))

# estimation
est = Zelig::zelig(P31 ~ treat, data = match.data(mat), model = 'ls',
                   cite = F, weights = 'weights')

c(est$get_coef()[[1]][2] - 1.96*est$get_se()[[1]][2],
         est$get_coef()[[1]][2],
         est$get_coef()[[1]][2] + 1.96*est$get_se()[[1]][2])

# effect plus CI
est17 = data.frame(estimate = est$get_coef()[[1]][2],
                   pvalue = est$get_pvalue()[[1]][[2]],
                   observations = nrow(est$data))

# output to tables
labels = c('Distance', 'Lang: Catalan',
           'Number of Catalan Parents',
           'Education', 'Economic Class',
           'Employment Status', 'Age', 'Gender')

pre = summary(mat)$sum.all
pre = mutate_if(pre, is.numeric, round, 3)
pre = pre[,1:4]
rownames(pre) <- labels

post = summary(mat)$sum.matched
post = mutate_if(post, is.numeric, round, 3)
post = post[,1:4]
rownames(post) <- labels

stargazer(pre,
          summary = F,
          title = 'Pre-Balance Summary Statistics. 2017 Referendum Analysis.',
          label = 'pre-bal-phone-2017',
          out = 'Figures/pre-bal-phone-2017.tex')

stargazer(post,
          summary = F,
          title = 'Post-Balance Summary Statistics. 2017 Referendum Analysis.',
          label = 'Post-bal-phone-2017',
          out = 'Figures/Post-bal-phone-2017.tex')


# table comparing referendums
rbind(est14, est17) %>%
  mutate(Referendum = c('2014', '2017')) %>%
  rename_all(funs(tools::toTitleCase(.))) %>%
  stargazer(., summary = F, rownames = F, title = 'Changes in levels of support for independence across the 2014 and 2017 public referenda. Estimates based on results from CEM matching.', label = 'compare-ref', out = 'Figures/compare-ref.tex')

# ------------------------------------------------------------------------------
# 2018 analysis
# ------------------------------------------------------------------------------

ef_2018 = function(frame){
  # id treated
  frame[['treat']] = 0
  frame[(frame['ANY']==2018) | (frame['ANY']==18),'treat'] = 1
  # frame
  frame[frame['P31']==2,'P31'] = 0
  # drop date cols after
  frame[['ANY']] = frame[['MES']] = frame[['DIA']] = NULL
  # groups
  C705.grp = list(c(0), c(1), c(2), c(3))
  C500.grp = list(c(1,2,3), c(4,5,6,7) , c(8,9,10,11))
  # match
  sub_mat = matchit(formula(paste0('treat ~ ', paste(vars, collapse = '+'))),
                data = as.data.frame(frame), method = 'cem',
                grouping = list(C705=C705.grp,
                                C500=C500.grp))
  # estimation
  sub_est = Zelig::zelig(P31 ~ treat, data = match.data(sub_mat), model = 'ls',
                     cite = F, weights = 'weights')
  # effect plus CI
  results = c(sub_est$get_coef()[[1]][2] - 1.96*sub_est$get_se()[[1]][2],
            sub_est$get_coef()[[1]][2],
            sub_est$get_coef()[[1]][2] + 1.96*sub_est$get_se()[[1]][2])
  return(results)
}


# effect in most recent
df18_1 = as.data.frame(sub_bind_p31(all_ceo[c(15:16, 18)]))
df18_2 = as.data.frame(sub_bind_p31(all_ceo[c(15:16, 19)]))
df18_3 = as.data.frame(sub_bind_p31(all_ceo[c(15:16, 20)]))
df18_4 = as.data.frame(sub_bind_p31(all_ceo[c(15:16, 21)]))

# results
est181 = ef_2018(df18_1)
est182 = ef_2018(df18_2)
est183 = ef_2018(df18_3)
est184 = ef_2018(df18_4)

# output table of results
pDat =
  rbind(est181,
        est182,
        est183,
        est184) %>%
  as.data.frame()
pDat

# format
colnames(pDat) <- c('95% (lower)', 'Estimate', '95% (higher)')
rownames(pDat) <- NULL
pDat$Type = c(#'2017 Referendum',
              '2018 Poll: January',
              '2018 Poll: April',
              '2018 Poll: July',
              '2018 Poll: November')

pDat %>%
  mutate_if(is.numeric, round, 3) %>%
  stargazer(summary = F, rownames = F,
            title = "Effects of 2017 referendum in context. We match the respondents from each survey in the post-referendum period to respondents in the pre-referendum period in 2017. Presented are the estimated effects of the repression surrounding the referendum on the question ``Do you want Catalonia to become a State?''",
            label = 'effects-comp',
            out = 'Figures/effects-comp-P31.tex')


# ------------------------------------------------------------------------------
# Summary table
# ------------------------------------------------------------------------------

# create the graphing data
ceo_date[['p_indep']] = 0
ceo_date[['p_nocontesta']] = 0
ceo_date[['p_nohosap']] = 0
for(rn in 1:nrow(ceo_date)){
  my_date = ceo_date[rn, 'date']
  # get the % that support independence
  if(ceo_date[rn, 'ceo'] %in% c(712, 723, 733)){
    P30 = eval(parse(text=paste0('ceo_', ceo_date[rn, 'ceo'])))[['P28']]
  }
  else{
    P30 = eval(parse(text=paste0('ceo_', ceo_date[rn, 'ceo'])))[['P30']]
  }
  # store the non-answer percentages
  ceo_date[rn, 'p_nocontesta'] = length(P30[P30==99]) / length(P30)
  ceo_date[rn, 'p_nohosap'] = length(P30[P30==98]) / length(P30)
  P30 = P30[P30 < 5]
  P30 = P30[complete.cases(P30)]
  p_indep = length(P30[P30==4]) / length(P30)
  # store in ceo_date dataframe
  ceo_date[rn, 'p_indep'] = p_indep
}

ceo_date[['year']] = as.numeric(format(ceo_date[['date']], '%Y'))

# get the count for each survey
ceo_date$n = 0
for(pos in 1:length(all_ceo)){
  # get the values
  cn = ceo[pos]
  n = nrow(all_ceo[[pos]])
  # assign the value
  ceo_date[ceo_date$ceo == cn, 'n'] = n
}

# output table
ceo_date %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Date = date,
         n = n,
         `Support Independence` = p_indep,
         `Refuse to Answer` = p_nocontesta,
         `Do Not Know` = p_nohosap,
         `Survey Name` = ceo) %>%
  mutate(Type = ifelse(ceo %in% c(760, 806, 838, 874),
                       'In-Person', 'Phone')) %>%
  stargazer(summary = F, title = 'Summary of CEO Surveys.',
            label = 'ceo-dates',
            out = 'Figures/ceo-dates.tex', 
            digit.separator = " ")


# ----------------------------------------------------------------------------
# Summary Visualizations
# ----------------------------------------------------------------------------
# -------------
# first viz nan
# -------------
viz_list = all_ceo[c(7, 8, 9, 11, 12, 13, 15, 16, 17, 19, 20, 21)]

ceo_nan = bind_rows(viz_list)

ceo_nan = as.data.frame(ceo_nan)
ceo_nan = ceo_nan[complete.cases(ceo_nan$P31),]

dates_nan = unique(ceo_nan$date)
percent_nan = c()
for(d in dates_nan){
  sub = ceo_nan[ceo_nan$date == d,]
  no_response = sub[(sub$P31 == 98) | (sub$P31 == 99),]
  p_nan = nrow(no_response) / nrow(sub)
  percent_nan = c(percent_nan, p_nan)
}

df_gg_nan = as.data.frame(percent_nan)
df_gg_nan$dates = as.Date(dates_nan)

ggplot(df_gg_nan, aes(x=dates, y=percent_nan)) +
  geom_vline(aes(xintercept=as.Date('2017-9-25')),
            color='gray43',
            linetype='dashed',
            size=1) + 
  geom_line(size=1.25) + 
  geom_point(fill='black', color='black', pch=21, size=3, stroke=1.25) +
  ylab('Percentage Non-respondents - Independence') +
  xlab('Date') +
  theme_ipsum_ps(grid = 'Y') + 
  scale_y_percent(limits = c(0, .2)) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") + 
  annotate(geom="text", x=as.Date('2017-8-20'), y=.02, label="Referendum",
           color="gray43", angle=90)
ggsave('Figures/p31-na-pattern.pdf', device = cairo_pdf)

# -------------
# pull the data
ceo_p31 = sub_bind_p31(viz_list)

# create averages for each date
dates = unique(ceo_p31$date)
no = c()
yes = c()
means = c()
for(date in dates){
  # subset to date
  sub = ceo_p31[ceo_p31$date == date, ]
  # pull the data
  no_count = nrow(sub[sub$P31 == 0, ])
  yes_count = nrow(sub[sub$P31 == 1, ])
  sub_mean = mean(sub$P31)
  # append the data
  no = c(no, no_count)
  yes = c(yes, yes_count)
  means = c(means, sub_mean)
}

# create the df
df_gg = as.data.frame(cbind(no, yes, means))

df_gg$dates = as.Date(dates)

# time series plot
ggplot(df_gg, aes(x = dates, y = means)) +
  geom_vline(aes(xintercept=as.Date('2017-9-25')),
             color='gray43',
             linetype='dashed',
             size=1) + 
  geom_line(size=1.25) + 
  geom_point(fill='black', color='black', pch=21, size=3, stroke=1.25) +
  ylab('Support for Independence') + xlab("") + 
  theme_ipsum_ps(grid = 'Y') +
  scale_y_percent(limits = c(.35,.65)) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") + 
  annotate(geom="text", x=as.Date('2017-8-20'), y=.38, label="Referendum",
              color="gray43", angle=90)
ggsave(filename = 'Figures/p31-time-series-label.pdf', device = cairo_pdf)

