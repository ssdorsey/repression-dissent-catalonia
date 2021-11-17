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
packs=c('haven', 'tidyverse', 'RColorBrewer', 'stringr', 'hrbrthemes', 
        'wesanderson') #
loadPkg(packs)


# ----------------------------------------------------------------------------------------------------------------------
# victimization models
#-----------------------------------------------------------------------------------------------------------------------

# load the data
vic = read_dta('Data/victimization.dta')
voted = vic[complete.cases(vic$votedref), ]

# hold the results
results = list()

# set up processing
var = c('gender',
        'age_group',
        'adjincome',
        'employed',
        'language_tri_dum3',
        'language_tri_dum1',
        'ideology',
        'educ_3',
        'cat_origins',
        'refvictim',
        'refvictim_close',
        '(Intercept)',
        'N',
        'll'
        )

get_se = function(mod){
    se = sqrt(diag(summary(mod)$cov.unscaled)*summary(mod)$dispersion)
    tib = tibble(se, var=names(se))
    return(tib)
}

create_coef_stack = function(mod){
    coefs = mod$coefficients
    N = length(mod$residuals)
    ll = ll=round(logLik(mod))
    combined = c(coefs, N, ll)
    names(combined) = c(names(coefs), c('N', 'll'))
    tib = tibble(combined, var=names(combined))
    return(tib)
}

create_table = function(mod1, mod2, subset){

    dv = all.vars(mod1$formula)[1]

    coefs1 = create_coef_stack(mod1)
    names(coefs1) = c(dv, 'var')
    coefs2 = create_coef_stack(mod2)
    names(coefs2) = c(paste0(dv, '_1'), 'var')

    se1 = get_se(mod1)
    names(se1) = c('SE_1', 'var')
    se2 = get_se(mod2)
    names(se2) = c('SE_2', 'var')

    tt = tibble('var'=var)
    
    tt = left_join(x=tt, y=coefs1, by='var')
    tt = left_join(x=tt, y=coefs2, by='var')
    tt = left_join(x=tt, y=se1, by='var')
    tt = left_join(x=tt, y=se2, by='var')

    tt$subset = subset

    tt$var = dplyr::recode(tt$var, '(Intercept)'='_cons')

    return(tt)
}

# ----------------------------------------------------------------------------
# regressions for people who VOTED in the 1-0 referendum 
# ----------------------------------------------------------------------------
# -----------------------------
# DV: they would vote YES in a potential legal/agreed referendum
# -----------------------------

m1_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M1 <- glm(m1_form, data=voted[voted$votedref==1, ], family="binomial")

m2_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M2 <- glm(m2_form, data=voted[voted$votedref==1, ], family="binomial")

results[[1]] = create_table(M1, M2, subset="voted (N = 1,423)")

# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if votedref==1, rob
# est store M1
# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if votedref==1, rob
# est store M2
# esttab M1 M2 using Table1.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace

# -----------------------------
# DV: participation in protests
# -----------------------------

m3_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M3 <- glm(m3_form, data=voted[voted$votedref==1, ], family="binomial")

m4_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M4 <- glm(m4_form, data=voted[voted$votedref==1, ], family="binomial")

results[[2]] = create_table(M3, M4, subset="voted (N = 1,423)")

# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if votedref==1, rob
# est store M1
# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if votedref==1, rob
# est store M2
# esttab M1 M2 using Table2.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace


# ----------------------------------------------------------------------------
# regressions for people who VOTED and voted YES in the 1-0 referendum
# ----------------------------------------------------------------------------
# -----------------------------
# DV: they would vote YES in a potential legal/agreed referendum
# -----------------------------

m5_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M5 <- glm(m5_form, data=voted[voted$votedyes==1, ], family="binomial")

m6_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M6 <- glm(m6_form, data=voted[voted$votedyes==1, ], family="binomial")

results[[3]] = create_table(M5, M6, subset="voted yes (N = 1,221)")

# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if votedyes==1, rob
# est store M1
# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if votedyes==1, rob
# est store M2
# esttab M1 M2 using Table3.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace


# -----------------------------
# DV: participation in protests
# -----------------------------

m7_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M7 <- glm(m7_form, data=voted[voted$votedyes==1, ], family="binomial")

m8_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M8 <- glm(m8_form, data=voted[voted$votedyes==1, ], family="binomial")

results[[4]] = create_table(M7, M8, subset="voted yes (N = 1,221)")

# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if votedyes==1, rob
# est store M1
# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if votedyes==1, rob
# est store M2
# esttab M1 M2 using Table4.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace

# ----------------------------------------------------------------------------
# regressions for people who WOULD HAVE VOTED in the 1-0 referendum: 
#   this includes people who voted AND people who did not vote because of logistics 
#   or because the police impeded them to vote (thus, who were willing to vote but they could not)
# ----------------------------------------------------------------------------

voted$reasons_novote = replace_na(voted$reasons_novote, -999)

voted$wvotedref = voted$votedref
voted[voted$reasons_novote==4, 'wvotedref'] = 1
voted[voted$reasons_novote==5, 'wvotedref'] = 1

# /*gen wvotedref = votedref
# replace wvotedref = 1 if reasons_novote == 4 
# replace wvotedref = 1 if reasons_novote == 5
# replace votedyes =. if votedref==.
# */

# -----------------------------
# DV: they would vote YES in a potential legal/agreed referendum
# -----------------------------
m9_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M9 <- glm(m9_form, data=voted[voted$wvotedref==1, ], family="binomial")

m10_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M10 <- glm(m10_form, data=voted[voted$wvotedref==1, ], family="binomial")

results[[5]] = create_table(M9, M10, subset="would have voted (N = 1,574)")

# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if wvotedref==1, rob
# est store M1
# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if wvotedref==1, rob
# est store M2
# esttab M1 M2 using Table5.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace

# -----------------------------
# DV: participation in protests
# -----------------------------

m11_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M11 <- glm(m11_form, data=voted[voted$wvotedref==1, ], family="binomial")

m12_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M12 <- glm(m12_form, data=voted[voted$wvotedref==1, ], family="binomial")

results[[6]] = create_table(M11, M12, subset="would have voted (N = 1,574)")

# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if wvotedref==1, rob
# est store M1
# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if wvotedref==1, rob
# est store M2
# esttab M1 M2 using Table6.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace


# ----------------------------------------------------------------------------
# regressions for people who did NOT VOTE in the 1-0 referendum 
# ----------------------------------------------------------------------------
# -----------------------------
# DV: they would vote YES in a potential legal/agreed referendum
# -----------------------------

m13_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M13 <- glm(m13_form, data=voted[voted$votedref==0, ], family="binomial")

m14_form = as.formula(voteYES_agreedref ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M14 <- glm(m14_form, data=voted[voted$votedref==0, ], family="binomial")

results[[7]] = create_table(M13, M14, subset="did not vote (N = 995)")

# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if votedref==0, rob
# est store M1
# logit voteYES_agreedref gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if votedref==0, rob
# est store M2
# esttab M1 M2 using Table7.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace

# -----------------------------
# DV: participation in protests
# -----------------------------

m15_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim)

M15 <- glm(m15_form, data=voted[voted$votedref==0, ], family="binomial")

m16_form = as.formula(partprotest ~ gender + age_group + adjincome + 
                    employed + language_tri_dum3 + language_tri_dum1 + 
                    ideology + educ_3 + cat_origins + refvictim_close)

M16 <- glm(m16_form, data=voted[voted$votedref==0, ], family="binomial")

results[[8]] = create_table(M15, M16, subset="did not vote (N = 995)")

# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim if votedref==0, rob
# est store M1
# logit partprotest gender age_group adjincome  employed language_tri_dum3 language_tri_dum1 ideology educ_3 cat_origins refvictim_close if votedref==0, rob
# est store M2
# esttab M1 M2 using Table8.tex, b(a2) se(a2) scalars(ll) star(+ 0.10 * 0.05 ** 0.01) nogaps replace


#-----------------------------------------------------------------------------------------------------------------------
# plot
#-----------------------------------------------------------------------------------------------------------------------

# tidy results df
tidy_list = function(frame)
  {
  # victim table
  vict_df = frame[,c(1,2,4, 6)]
  colnames(vict_df) <- c('var', 'est', 'se', 'subset')
  vict_df$outcome = colnames(frame)[2]
  vict_df$type = 'Victim (Any)'
  vict_df = na.omit(vict_df)
  
  # close victim table
  close_vict_df = frame[,c(1,3,5, 6)]
  colnames(close_vict_df) <- c('var', 'est', 'se', 'subset')
  close_vict_df$outcome = colnames(frame)[2]
  close_vict_df$type = 'Victim (Close)'
  close_vict_df = na.omit(close_vict_df)
  
  # combine tidy output
  pDat = rbind(vict_df, close_vict_df)
  return(pDat)
}

# tidy results
tidy_results = map(results, tidy_list)

# combine lists into one tidy df
tidy_frame = do.call(rbind, tidy_results)

# make 95\% CI
tidy_frame$lo95 = tidy_frame$est - 1.96*tidy_frame$se
tidy_frame$hi95 = tidy_frame$est + 1.96*tidy_frame$se

# labels dictionary
labels_dict = data.frame(var = c('gender',
                                'age_group',
                                'adjincome',
                                'employed',
                                'language_tri_dum3',
                                'language_tri_dum1',
                                'ideology',
                                'educ_3',
                                'cat_origins',
                                'refvictim',
                                'refvictim_close',
                                '_cons',
                                'N',
                                'll'
                                ), 
                         label = c('Female',
                                    'Age',
                                    'HH Income',
                                    'Employed',
                                    'Bilingual',
                                    'Catalan Speaker',
                                    'Ideology',
                                    'Education',
                                    'Catalan Family',
                                    'Victim (any)',
                                    'Victim (close)',
                                    'Constant',
                                    'N',
                                    'LL')
                        )
         
outcomes_dict = data.frame(outcome = c('voteYES_agreedref',
                                        'indyaxis',
                                        'partprotest',
                                        'repressionfear_bi'
                                        ), 
                           outcome_lab = c('DV: Would Vote Yes\n in Legal Ref.',
                                           'DV: Support Level\n for Independence', 
                                           'DV: Participated in Protest',
                                           'DV: Fear Repression'
                                           )
                            )


# get rid of intercept and diag stats
pDat = 
  tidy_frame %>% 
  filter(!var %in% c('N', 'll', '_cons')) %>% 
  left_join(labels_dict) %>% 
  left_join(outcomes_dict)


# highlight victim vars
pDat$hilite = ifelse(str_detect(pDat$label, 'Victim'), T, F)

# reorder outcomes
pDat$outcome_lab = forcats::fct_rev(pDat$outcome_lab)

# fix victims plot
pDat = filter(pDat, hilite == TRUE, outcome != 'indyaxis')
pDat = filter(pDat, hilite == TRUE, outcome != 'repressionfear_bi')

ggplot(pDat) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_pointrange(aes(x = type, y = est, 
                      shape = subset,
                      ymin = lo95,
                      ymax = hi95), 
                  position = position_dodge(width = .5), 
                  size = .8) + 
  facet_grid(~outcome_lab) + 
  coord_flip() + 
  theme_ipsum_ps(grid = 'X') + 
  labs(x = 'Victimization Type', 
       y = 'Coefficient Estimate', 
       shape = 'Sample Subset:') +  
  theme(legend.position = 'bottom', axis.title.y = element_text(hjust = .5)) + 
  scale_color_manual(values = wes_palettes$Darjeeling1) + 
  guides(shape = guide_legend(nrow = 2, byrow = T))
ggsave('Figures/victim_coefplot.pdf', device = cairo_pdf)
