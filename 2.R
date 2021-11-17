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
packs=c('tidyverse', 'haven', 'MatchIt', 'broom', 
        'janitor', 'ggrepel', 'hrbrthemes', 'stargazer') #
loadPkg(packs)


# set up data -------------------------------------------------------------

# load
ceo_850 = as.data.frame(read_spss('Data/CEO_850.sav'))
ceo_857 = as.data.frame(read_spss('Data/CEO_857.sav'))
ceo_863 = as.data.frame(read_spss('Data/CEO_863_withdates.sav'))

# drop observations after the declaration 
ceo_863 = ceo_863[ceo_863$DIA < 27,]

# id treatment
ceo_850$treat = 0
ceo_857$treat = 0
ceo_863$treat = 1

# stack 
df = bind_rows(list(ceo_850, ceo_857, ceo_863))

# subset
keep = c('P31', 'treat', 'C705', 'Edat_CEO', 'C500', 'C800',
         'C401', 'Edat_CEO', 'SEXE', 'P21A', 'P21D', 'P21F',
         'P21H', 'P21I', 'P21J', 'P21L', 'P21M', 'P21R', 'C110',
         'C120', 'ANY')

df = df[keep]

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

# just data from 2017 with period after election marked under treat
df_2017 = data.frame(df[df$ANY == 2017,])

df_c_2017 = df_2017[c('P31', 'treat', 'C705', 'num_p_cata', 'C500', 'C800',
                      'C401', 'Edat_CEO', 'SEXE')]

# only complete cases
df_c_2017 = df_c_2017[complete.cases(df_c_2017),]


# pull out pre- and post- stat tables
labs = c('First Language', 'Num. of Catalan Parents',
         'Education Level', 'Economic Class', 'Work Status',
         'Age', 'Sex')


# sample statistics
samp = df_c_2017 %>%
  select(C705:SEXE) %>%
  data.table::setnames(., old = names(.), new = labs) %>%
  filter(`Education Level` < 60) %>%
  summarise(`First Language (most common)` = names(sort(-table(`First Language`)))[1],
            `Num. of Catalan Parents (average)` = mean(`Num. of Catalan Parents`),
            `Education Level (median)` = median(`Education Level`),
            `Economic Class (median)` = median(`Economic Class`),
            `Work Status (median)` = median(`Economic Class`),
            `Age` = median(`Age`),
            `Female` = .49) %>%
  mutate(`First Language (most common)` = replace(`First Language (most common)`,
                                                  `First Language (most common)` == 2, 'Catalan'),
         `Education Level (median)` = 'HS (higher)',
         `Economic Class (median)` = 'Middle',
         `Work Status (median)` = 'Between Work') %>%
  t()

stargazer(samp, type = 'latex', summary = F, title = 'Sample Characteristics',
          label = 'samp-char', out = 'Figures/samp-char.tex')


# Analysis ----------------------------------------------------------------

eval_dv = function(dv){
  # restrict data
  df_c_2017 = df_2017[c(dv, 'treat', 'C705', 'num_p_cata', 'C500', 'C800',
                        'C401', 'Edat_CEO', 'SEXE')]
  df_c_2017 = df_c_2017[complete.cases(df_c_2017),]
  # set up variables
  vars = c('C705', 'num_p_cata', 'C500', 'C800',
           'C401', 'Edat_CEO', 'SEXE')
  # manual coarsening
  C705.grp = list(c(0), c(1), c(2), c(3))
  C500.grp = list(c(1,2,3), c(4,5,6,7) , c(8,9,10,11))
  # perform the match
  mat = matchit(formula(paste0('treat ~ ', paste(vars, collapse = '+'))), 
                data = df_c_2017, method = 'cem', 
                grouping = list(C705=C705.grp, 
                                C500=C500.grp))
  # treatment effects
  form = as.formula(paste(dv, '~ treat'))
  est = Zelig::zelig(form, data = match.data(mat), model = 'ls', 
                     weights = 'weights',
                     cite = F)
  
  # pull out estimate and coef for plotting
  eff = data.frame(est = est$get_coef()[[1]][2],
                   std = est$get_se()[[1]][2],
                   outcome = dv)
  
  
  # get output table for P31
  if(dv == "P31")
  {
    z_est = Zelig::from_zelig_model(est) %>% 
      broom::tidy() %>% 
      janitor::clean_names() %>% 
      select(Term = term, Estimate = estimate, `Standard Error` = std_error, 
             `Statistic` = statistic, `P-value` = p_value) %>% 
      mutate(Term = c("Intercept", "Treatment")) %>% 
      mutate_if(is_numeric, funs(round(., 3)))
    
    # stargazer
    stargazer(z_est, summary = FALSE, title = "Matching estimate of effect of crackdown on pro-independence attitudes. OLS using matching weights. N = 2,302.", label = "p31-mat", style = "io", out = "Figures/p31-mat.tex")
  }
    
  return(eff)
  
}

# plot results
pDat = rbind(
  eval_dv(dv = 'P31'),
  eval_dv(dv = 'P21A'),
  eval_dv(dv = 'P21D'),
  eval_dv(dv = 'P21F'),
  eval_dv(dv = 'P21H'),
  eval_dv(dv = 'P21I'),
  eval_dv(dv = 'P21J'),
  eval_dv(dv = 'P21L'),
  eval_dv(dv = 'P21M'),
  eval_dv(dv = 'P21R')
)

# add labels
outLabs = c('Independence',
            'Courts of Justice',
            'Central Government',
            'Catalan Government',
            'Catalan Parliament',
            'The EU',
            'Spanish Monarchy',
            'Spanish Police',
            'Catalan Police',
            'Constitutional Court')


# add confidence intervals
pDat$conf95 = paste0('(', round(pDat$est - 1.96*pDat$std,3), ',',
                     round(pDat$est + 1.96*pDat$std,3), ')')

# output table
pDat %>%
  mutate(Outcome = outLabs, Estimate = round(est,3)) %>%
  select(Outcome, Estimate, Interval = conf95) %>% 
  mutate(`Outcome Scale` = c('0-1', rep('0-10', 9))) %>% 
  stargazer(summary = F, rownames = F,
            title = 'Estimates of repression effect by outcome.',
            label = 'est-effect',
            out = 'Figures/effects-table.tex')


# support for key actors plot
pDat$label = outLabs
pDat$actors = ifelse(str_detect(pDat$label, 'Catalan'), 'Catalan', 'Spanish/European')
#pDat[pDat['label']=='The EU', 'actors'] = 'European'
pDat$type = c('Independence', rep('Support for Key Actors', 9))
# pDat$colors = ifelse(str_detect(pDat$label, 'Catalan'), "#0B775E",
#                      "#F2300F")

# key actors
ggplot(pDat[pDat$outcome != 'P31',], aes(label = label, shape = actors)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_pointrange(aes(x = outcome, y = est, ymin = est - std*1.96,
                      ymax = est + std*1.96), size = .8,
                  lwd = 1/2, position = position_dodge(width = 1/2)) +
  hrbrthemes::theme_ipsum_rc(grid = 'X') +
  coord_flip() +
  ggrepel::geom_label_repel(aes(x = outcome, y = est),
                            nudge_x = -.5, nudge_y = -.02) + 
  theme(axis.text.y = element_blank(),
        legend.position = 'top') +
  labs(y = 'Change in level of confidence in actor (0-10 scale)', 
       x = '',
       shape = "Institutional association:",
       NULL)
ggsave(filename = 'Figures/cem_effects_actors.pdf',
       height = 7,
       width = 9,
       device = cairo_pdf)
ggsave(filename = 'Figures/cem_effects_actors.png',
       height = 7,
       width = 9,
       type = 'cairo')

