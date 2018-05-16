* Encoding: ISO-8859-1.
﻿* Encoding: ISO-8859-1.


title'data prepartation'.
execute.

RECODE Genderg (1=1) (2=0) INTO male.
VARIABLE LABELS  male 'Does the participant identify as male?'.
EXECUTE.

VALUE LABELS
male
0 'no'.
1 'yes'.
execute.

RECODE Ethnicg (1=0) (ELSE=1) INTO minority.
VARIABLE LABELS  minority 'Does the participant belong to a minority ethnicity?'.
EXECUTE.

VALUE LABELS
minority
0 'no'.
1 'yes'.
execute.

RECODE IMD3 (1=1) (ELSE=0) INTO deprived.
VARIABLE LABELS  deprived 'Does the participant live in a deprived area? (by IDM)'.
EXECUTE.

VALUE LABELS
deprived
0 'no'.
1 'yes'.
execute.

compute mwb = wemwbs.
compute mwbi = wemwbsi.

VARIABLE LABELS  mwb 'Mental Well-Being'.
VARIABLE LABELS  mwbi 'Mental Well-Being (w. imputed values)'. 
execute.

RECODE Watchwk Watchwe Compwk Compwe Comphwk Comphwe Smartwk Smartwe (1=0) (2=.5) (3=1) (4=2) (5=3) 
    (6=4) (7=5) (8=6) (9=7) (ELSE=SYSMIS) INTO Watchwk_adj Watchwe_adj Compwk_adj Compwe_adj 
    Comphwk_adj Comphwe_adj Smartwk_adj Smartwe_adj.
EXECUTE.

compute watch_wd = Watchwk_adj.
compute watch_we = Watchwe_adj.
compute play_wd = Compwk_adj.
compute play_we = Compwe_adj.
compute comp_wd = Comphwk_adj.
compute comp_we = Comphwe_adj.
compute sp_wd = Smartwk_adj.
compute sp_we = Smartwe_adj.
compute watch_wd_sq = Watchwk**2.
compute watch_we_sq = Watchwe**2.
compute play_wd_sq = Compwk**2.
compute play_we_sq = Compwe**2.
compute comp_wd_sq = Comphwk**2.
compute comp_we_sq = Comphwe**2.
compute sp_wd_sq = Smartwk**2.
compute sp_we_sq = Smartwe**2.
execute. 

compute weekday_screen = sum(Watchwk_adj, Compwk_adj, Comphwk_adj, Smartwk_adj).
compute weekend_screen = sum(Watchwe_adj, Compwe_adj, Comphwe_adj, Smartwe_adj).
execute. 

VARIABLE LABELS  weekday_screen 'Linear digital screen time (week days)'.
VARIABLE LABELS  weekend_screen 'Linear digital screen time (weekend days)'.
execute.

compute screenwk_sq =  weekday_screen**2.
compute screenwe_sq =  weekend_screen**2.
execute. 

VARIABLE LABELS  screenwk_sq 'Squared total digital screen time (week days)'.
VARIABLE LABELS  screenwe_sq 'Squared total digital screen time (weekend days)'.
execute.

VARIABLE LEVEL watch_wd watch_we play_wd play_we comp_wd comp_we sp_wd sp_we
watch_wd_sq watch_we_sq play_wd_sq play_we_sq comp_wd_sq comp_we_sq sp_wd_sq sp_we_sq (scale) . 

VARIABLE LABELS watch_wd 'Linear Streaming/Watching (week days)'.
VARIABLE LABELS watch_we 'Linear StreamIng/Watching (weekend days)'.
VARIABLE LABELS  play_wd 'Linear Playing (week days)'.
VARIABLE LABELS play_we 'Linear Playing (weeked days)'.
VARIABLE LABELS comp_wd 'Linear Computing (week days)'.
VARIABLE LABELS comp_we 'Linear Computing (weekend days)'.
VARIABLE LABELS sp_wd 'Linear Smartphone (week days)'.
VARIABLE LABELS sp_we 'Linear Smartphone (weekend days'.
VARIABLE LABELS watch_wd_sq 'Squared Streaming/Watching (week days)'.
VARIABLE LABELS watch_we_sq 'Squared StreamIng/Watching (weekend days)'.
VARIABLE LABELS  play_wd_sq 'Squared Playing (week days)'.
VARIABLE LABELS play_we_sq 'Squared Playing (weeked days)'.
VARIABLE LABELS comp_wd_sq 'Squared Computing (week days)'.
VARIABLE LABELS comp_we_sq 'Squared Computing (weekend days)'.
VARIABLE LABELS sp_wd_sq 'Squared Smartphone (week days)'.
VARIABLE LABELS sp_we_sq 'Squared Smartphone (weekend days'.
execute. 

if (watch_wd GT 3.68) wwd_i = 1. 
if (watch_wd LE 3.68) wwd_i = 0. 
if (watch_we GT 4.84)  wwe_i = 1. 
if (watch_we LE 4.84) wwe_i = 0. 
if (play_wd GT 1.66) pwd_i = 1. 
if (play_wd LE 1.66) pwd_i = 0. 
if (play_we GT 3.59)  pwe_i = 1. 
if (play_we LE 3.59) pwe_i = 0.
if (comp_wd GT 4.28) cwd_i = 1. 
if (comp_wd LE 4.28) cwd_i = 0. 
if (comp_we GT 4.65)  cwe_i = 1. 
if (comp_we LE 4.65) cwe_i = 0.
if (sp_wd GT 1.95) swd_i = 1. 
if (sp_wd LE 1.95) swd_i = 0. 
if (sp_we GT 4.17)  swe_i = 1. 
if (sp_we LE 4.17) swe_i = 0.
execute. 

VARIABLE LABELS  wwd_i 'Watching Weekday Inflection'.
VARIABLE LABELS  wwe_i 'Watching Weekend Inflection'.
VARIABLE LABELS  pwd_i 'Playing Weekday Inflection'.
VARIABLE LABELS  pwe_i 'Playing Weekend Inflection'.
VARIABLE LABELS  cwd_i 'Computing Weekday Inflection'.
VARIABLE LABELS  cwe_i 'Computing Weekend Inflection'.
VARIABLE LABELS  swd_i 'Smartphone Weekday Inflection'.
VARIABLE LABELS  swe_i 'Smartphone Weekdend Inflection'.
execute. 

title'trim from full dataset.'

MATCH FILES FILE = *
/KEEP = Serial Genderg Ethnicg IMD3 LACODE LANAME gorcode REGION ONSCLG01 ONSCLG11 ONSSUG11 WBOptimf WBUseful WBRelax WBIntp WBEnergy WBDealpr WBThkclr WBGoodme WBClsep WBConfid WBMkmind
WBLoved WBIntthg WBCheer Watchwk Watchwe Compwk Compwe Comphwk Comphwe Smartwk Smartwe male minority deprived mwb mwbi Watchwk_adj Watchwe_adj Compwk_adj Compwe_adj Comphwk_adj Comphwe_adj Smartwk_adj Smartwe_adj watch_wd watch_we play_wd play_we
comp_wd comp_we sp_wd sp_we watch_wd_sq watch_we_sq play_wd_sq
play_we_sq comp_wd_sq comp_we_sq sp_wd_sq sp_we_sq weekday_screen weekend_screen screenwk_sq screenwe_sq wwd_i wwe_i pwd_i pwe_i cwd_i cwe_i swd_i swe_i.
execute. 

title'descriptive analyses'.

RELIABILITY
  /VARIABLES=WBOptimf WBUseful WBRelax WBIntp WBEnergy WBDealpr WBThkclr WBGoodme WBClsep WBConfid 
    WBMkmind WBLoved WBIntthg WBCheer
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

FREQUENCIES VARIABLES=mwb
  /STATISTICS=STDDEV MEAN
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

SORT CASES  BY male.
SPLIT FILE LAYERED BY male.

DESCRIPTIVES VARIABLES=watch_wd play_wd comp_wd sp_wd
  /STATISTICS=MEAN STDDEV MIN MAX SEMEAN.

SPLIT FILE OFF.

title'analyses'.

* Encoding: UTF-8.

title'unadjusted models'

title'watching'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER watch_wd watch_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER watch_we watch_we_sq.

title'playing'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER play_wd play_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER play_we play_we_sq.

title'computing'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER comp_wd comp_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER comp_we comp_we_sq.

title'smartphone use'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER sp_wd sp_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER sp_we sp_we_sq.

title'adjusted models'

title'watching'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER watch_wd watch_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER watch_we watch_we_sq.

title'playing'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER play_wd play_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER play_we play_we_sq.

title'computing'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER comp_wd comp_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER comp_we comp_we_sq.

title'smartphone use'.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER sp_wd sp_wd_sq.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER sp_we sp_we_sq.


SORT CASES  BY wwd_i.
SPLIT FILE LAYERED BY wwd_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER watch_wd.

SPLIT FILE OFF.

SORT CASES  BY wwe_i.
SPLIT FILE LAYERED BY wwe_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER watch_we.

SPLIT FILE OFF.

SORT CASES  BY pwd_i.
SPLIT FILE LAYERED BY pwd_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER play_wd.

SPLIT FILE OFF.

SORT CASES  BY pwe_i.
SPLIT FILE LAYERED BY pwe_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER play_we.

SPLIT FILE OFF.

SORT CASES  BY cwd_i.
SPLIT FILE LAYERED BY cwd_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER comp_wd.

SPLIT FILE OFF.

SORT CASES  BY cwe_i.
SPLIT FILE LAYERED BY cwe_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER comp_we.

SPLIT FILE OFF.

SORT CASES  BY swd_i.
SPLIT FILE LAYERED BY swd_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER sp_wd.

SPLIT FILE OFF.

SORT CASES  BY swe_i.
SPLIT FILE LAYERED BY swe_i.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mwbi
  /METHOD=ENTER male minority deprived
  /METHOD=ENTER sp_we.

SPLIT FILE OFF.










