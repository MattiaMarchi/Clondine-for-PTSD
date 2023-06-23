# R code for reproducing the dataset and the analyses used in the paper #
# Clonidine for post-traumatic stress disorder: a systematic review of the current evidence #
# R code by Mattia Marchi (mattiamarchimd@gmail.com) 
# June 23, 2023

#Load required packages
library(meta)
library(metafor)
library(tidyverse)
library(metaforest)
library(dmetar)

#Outcomes:
#1. Sleep quality
#2. Amount of sleep
#3. Nightmares
#4. PTSD symptoms
#5. Safety and tolerability

#Generate the dataset
df <- structure(list(ID = c(3L, 6L, 7L, 9L, 14L, 18L, 19L, 24L, 32L,33L),
                     Author..Year = structure(1:10, levels = c("Alao et al., 2012c","Bange et al., 2022", "Burek et al., 2021", "Detweiler et al., 2016", 
                                                               "Hansenne et al., 1991", "Kinzie et al., 1989", "Kinzie et al., 1994", "Ouyang et al., 2015", "Wendell et al., 2015", "Ziegenhorn et al., 2009"), class = "factor"),
                     Year.data.collection = structure(c(5L, 4L, 3L, 2L, 5L, NA, NA, 5L, 1L, 5L), levels = c("2009-2011","2009-2013", "2015-2018", "2021", "NR"), class = "factor"),
                     Study.Design = structure(c(1L, 2L, 5L, 5L, 2L, 4L, 2L, 2L, 5L, 3L), levels = c("2 case reports", "Case report", "Cross-over RCT", "Prospective pilot study", "Retrospective cohort study (chart review)"), class = "factor"),
                     Setting = structure(c(4L, 1L, 4L, 4L, NA, 3L, NA, 2L, 4L, 3L), levels = c("Hospital", "Outpatient Clinic", "Psychiatric Clinic", "Veteran Clinic"), class = "factor"), Country = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L), levels = c("Germany", "US"), class = "factor"),
                     N = c(2L, 1L, 79L, 362L, 1L, 9L, 4L, 1L, 102L, 12L), Female.. = structure(c(4L, 2L, 1L, 3L, 1L, 5L, 2L, 1L, 6L, 7L), levels = c("0", "100", "4.3", "50", "66.7", "7.8", "94.4"), class = "factor"),
                     Age.mean..SD. = structure(c(4L, 5L, 7L, 8L, 1L, 9L, 2L, 10L, 6L, 3L), levels = c("20", "29-46", "32 (8)", "40.5 (10.6)", "42", "43.0 (15.6)", "48.0 (15.1)", "53.7 (NA); range 23-82", "54.6 (SD); range 31-64", "67"), class = "factor"),
                     MeanAge = structure(c(3L, 4L, 6L, 7L, 1L, 8L, NA, 9L, 5L, 2L), levels = c("20", "32", "40.5", "42", "43", "48", "53.7", "54.6", "67"), class = "factor"),
                     Duration = structure(c(NA, NA, NA, NA, 3L, 1L, 2L, 4L, 5L, 6L), levels = c("12-19 months", "2 weeks", "3 weeks", "4 months", "6 months", "6 weeks"), class = "factor"), 
                     T..Clonidine.dose.and.route. = structure(c(3L, 8L, 4L, 5L,6L, 2L, 9L, 1L, 10L, 7L),levels = c("0.1 mg po qhs", "0.1 mg po twice day+imipramine", 
                                                                                                     "0.1-0.3 mg po (0.1 qhs, then titrated up to three times day)", 
                                                                                                     "0.1-0.5 mg po (qhs or up to 0.2mg qAM+0.3mg qhs)", "0.1-4 mg day po (mode: 0.1 mg day)", 
                                                                                                     "0.15 mg iv", "0.15 mg po qAM+0.3 mg po qhs", "0.15 mg po qhs", 
                                                                                                     "0.2-0.3 mg day", "NR"), class = "factor"),
                     Clonidine_Dose = structure(c(4L, 2L, 1L, 1L, 2L, 3L, NA, 1L, NA, 5L), levels = c("0.1", "0.15", "0.2", "0.3", "0.45"), class = "factor"),
                     C = structure(c(NA, NA, 2L, 1L, NA, NA, NA, NA, 4L, 3L), levels = c("alpha2 agonists (prazosine or terazosine), or SGA (risperidone or quetiapine), or antideressant (trazodone or mirtazapine)", "Clonidine <0.1 mg po qhs", "PBO", "Prazosine"), class = "factor"), 
                     N.Clonidine = c(2L, 1L, 50L, 27L, 1L, 9L, 4L, 1L, 42L, 12L), N.Controls = c(NA, NA, 29L, 335L, NA, NA, NA, NA, 60L, 12L), Outcome1_Sleep.quality.assessment = structure(c(1L, NA, NA, NA, NA, 1L, 1L, NA, NA, 1L), levels = "Self-reported", class = "factor"), 
                     Outcome1_T = c(2L, NA, NA, NA, NA, 6L, 4L, NA, NA, NA),
                     Outcome1_C = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Outcome1_T_mean = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), levels = "-1.83", class = "factor"),
                     Outcome1_T_SD = structure(c(NA, NA, NA, NA, NA, NA, 2L, NA, NA, 1L), levels = c("1", "Polysonnography"), class = "factor"), Outcome1_C_mean = structure(c(NA, NA, NA, NA, NA, NA, 2L, NA, NA, 1L), levels = c("0", "no change"), class = "factor"), 
                     Outcome1_C_SD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), Outcome1_Time = structure(c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = "2 weeks", class = "factor"),
                     Outcome2_amount.of.sleep.assessment = structure(c(NA, NA, NA, NA, NA, NA, 2L, NA, NA, 1L), levels = c("Latency in sleep", "Polysonnography"), class = "factor"), Outcome2_T_mean = structure(c(NA, NA, NA, NA, NA, NA, 2L, NA, NA, 1L), levels = c("-2.19", "no change"), class = "factor"),
                     Outcome2_T_SD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), Outcome2_C_mean = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0L), Outcome2_C_SD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), Outcome2_Time = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     Outcome3_nightmares.assessment = structure(c(3L, 3L, NA, 3L, NA, 3L, 2L, 3L, 1L, NA), levels = c("Decrease in nightmare as documented in the clinical chart", "PSG/Self-reported", "Self-reported"), class = "factor"), 
                     Outcome3_T = structure(c(3L, 1L, NA, 2L, NA, 5L, 6L, 1L, 4L, NA), levels = c("1", "17", "2", "24", "7", "Reduction"), class = "factor"), Outcome3_C = c(NA, NA, NA, 199L, NA, NA, NA, NA, 32L, NA),
                     Outcome3_Time = structure(c(1L, 2L, NA, NA, NA, NA, NA, NA, 3L, NA), levels = c("2 weeks", "3 days", "6 months"), class = "factor"),
                     Outcome4_PTSD.symptoms.assessment = structure(c(NA, NA, 2L, NA, 4L, 3L, 5L, NA, NA, 1L), levels = c("CAPS-D", "CGI-Improvement scale indicating at least some improvements", "DSM III PTSD checklist", "IES-R - Results: -41 (i.e., 45 baseline, 4 after 3 weeks treatment)", "Self-reported"), class = "factor"),
                     Outcome4_T = structure(c(NA, NA, 1L, NA, NA, 2L, 3L, NA, NA, NA), levels = c("42", "6", "Decrease but still depressed"), class = "factor"), Outcome4_C = c(NA, NA, 15L, NA, NA, NA, NA, NA, NA, NA), Outcome4_T_mean = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), levels = "-2.67", class = "factor"), 
                     Outcome4_T_SD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L),Outcome4_C_mean = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0L), Outcome4_C_SD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), Outcome4_Time = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Other.Outcome = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     Safety_all.adverse.reactions_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 20L, NA), Safety_all.adverse.reactions_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 17L, NA), Safety_dropout.any.cause_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 21L, NA),
                     Safety_dropout.any.cause_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 32L, NA), Safety_dropout.severe.side.effects_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 13L, NA), Safety_dropout.severe.side.effects_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 13L, NA),
                     Safety_death_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 0L, NA), Safety_death_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 0L, NA)), class = "data.frame", row.names = c(NA, -10L))
#Descriptive statistics
df$MeanAge <- as.character(df$MeanAge)
df$MeanAge <- as.numeric(df$MeanAge)
median(df$MeanAge, na.rm = T)
df$Clonidine_Dose <- as.character(df$Clonidine_Dose)
df$Clonidine_Dose <- as.numeric(df$Clonidine_Dose)
sd(df$Clonidine_Dose, na.rm = T)
###----------------------------------------------------------------------------------------
###------------1. Meta Analysis of Clonidine for PTSD-related sleep quality----------------
###----------------------------------------------------------------------------------------
sl_qual_df <- df%>%
  filter(!is.na(Outcome1_Sleep.quality.assessment))
summary(sl_qual_df$Outcome1_Sleep.quality.assessment)
summary(sl_qual_df$Study.Design)
str(sl_qual_df)
sl_qual_df$Outcome1_T_mean <- as.character(sl_qual_df$Outcome1_T_mean)
sl_qual_df$Outcome1_T_mean <- as.numeric(sl_qual_df$Outcome1_T_mean)

#Eventually not run
# sl_qual_ma_data <- sl_qual_df %>%
#   filter(Author..Year=="Ziegenhorn et al., 2009") #Ziegenhorn et al., 2009 is crossover RCT; the other 2 studies have no control group
#end not run

#Meta analysis
sl_qual_ma <- metacont(n.e = N.Clonidine, mean.e = Outcome1_T_mean, sd.e = Outcome1_T_SD,
                       n.c = N.Controls, mean.c = Outcome1_C_mean, sd.c = Outcome1_C_SD,
                       studlab = Author..Year, data = sl_qual_df, sm = "SMD", method.tau = "DL")
sl_qual_ma
#forestplot
forest.meta(sl_qual_ma, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = T,
            label.left = "Favours clonidine", label.right = "Favours control")
#with case reports
sl_qual_df$Outcome1_C
sl_qual_mabin <- metabin(event.e = sl_qual_df$Outcome1_T, n.e = N.Clonidine,
                         event.c = Outcome1_C, n.c = N.Controls,
                         studlab = Author..Year, data = sl_qual_df, sm = "OR")
sl_qual_mabin
#forestplot
forest.meta(sl_qual_mabin, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = T,
            label.left = "Favours clonidine", label.right = "Favours control")
###----------------------------------------------------------------------------------------
###-----------2. Meta Analysis of Clonidine for PTSD-related amount of sleep---------------
###----------------------------------------------------------------------------------------
sl_quant_df <- df%>%
  filter(!is.na(Outcome2_amount.of.sleep.assessment))
str(sl_quant_df)
sl_quant_df$Outcome2_T_mean <- as.character(sl_quant_df$Outcome2_T_mean)
sl_quant_df$Outcome2_T_mean <- as.numeric(sl_quant_df$Outcome2_T_mean)
#meta-analysis
sl_quant_ma <- metacont(n.e = N.Clonidine, mean.e = Outcome2_T_mean, sd.e = Outcome2_T_SD,
                        n.c = N.Controls, mean.c = Outcome2_C_mean, sd.c = Outcome2_C_SD,
                        studlab = Author..Year, data = sl_quant_df, sm = "SMD", method.tau = "DL")
sl_quant_ma
#forestplot
forest.meta(sl_quant_ma, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = T,
            label.left = "Favours clonidine", label.right = "Favours control")
###----------------------------------------------------------------------------------------
###-------------3. Meta Analysis of Clonidine for PTSD-related nightmares------------------
###----------------------------------------------------------------------------------------
nm <- df%>%
  filter(!is.na(Outcome3_nightmares.assessment))
summary(nm$Study.Design)
#meta-analysis
nm_ma <- metabin(event.e = Outcome3_T, n.e = N.Clonidine,
                 event.c = Outcome3_C, n.c = N.Controls,
                 data = nm, studlab = Author..Year, sm = "OR")
nm_ma
#forestplot
forest.meta(nm_ma, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = F,
            label.left = "Favours clonidine", label.right = "Favours control")
###----------------------------------------------------------------------------------------
###-------------------4. Meta Analysis of Clonidine for PTSD symptoms----------------------
###----------------------------------------------------------------------------------------
ptsd <- df%>%
  filter(!is.na(Outcome4_PTSD.symptoms.assessment))
#continuous
ptsd$Outcome4_T_mean <- as.character(ptsd$Outcome4_T_mean)
ptsd$Outcome4_T_mean <- as.numeric(ptsd$Outcome4_T_mean)
ptsd_ma <- metacont(n.e = N.Clonidine, mean.e = Outcome4_T_mean, sd.e = Outcome4_T_SD,
                    n.c = N.Controls, mean.c = Outcome4_C_mean, sd.c = Outcome4_C_SD,
                    studlab = Author..Year, data = ptsd, sm = "SMD", method.tau = "DL")
ptsd_ma
#forestplot
forest.meta(ptsd_ma, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = T,
            label.left = "Favours clonidine", label.right = "Favours control")
#dichotomous
ptsd$Outcome4_T
ptsd_mabin <- metabin(event.e = Outcome4_T, n.e = N.Clonidine,
                      event.c = Outcome4_C, n.c = N.Controls,
                      data = ptsd, studlab = Author..Year, sm = "OR")
ptsd_mabin
#forestplot
forest.meta(ptsd_mabin, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = T,
            label.left = "Favours control", label.right = "Favours clonidine") #reverse x labels because the event is symptom improvement (therefore a positive outcome)
###----------------------------------------------------------------------------------------
###-----------5. Meta Analysis of Clonidine for PTSD: safety and tolerability--------------
###----------------------------------------------------------------------------------------
#-------------Death
#no death occurred in any trial

#------------All adverse reaction
df$Safety_all.adverse.reactions_C
aev <- metabin(event.e = Safety_all.adverse.reactions_T, n.e = N.Clonidine,
               event.c = Safety_all.adverse.reactions_C, n.c = N.Controls,
               data = df, studlab = Author..Year, sm = "OR")
aev
forest.meta(aev, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = F,
            label.left = "Favours clonidine", label.right = "Favours control")
#-----------Dropout due to any cause
df$Safety_dropout.any.cause_C
dropout_any <- metabin(event.e = Safety_dropout.any.cause_T, n.e = N.Clonidine,
                       event.c = Safety_dropout.any.cause_C, n.c = N.Controls,
                       data = df, studlab = Author..Year, sm = "OR")
dropout_any
forest.meta(dropout_any, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = F,
            label.left = "Favours clonidine", label.right = "Favours control")
#----------Dropout due to severe adverse events
df$Safety_dropout.severe.side.effects_C
dropout_ae <- metabin(event.e = Safety_dropout.severe.side.effects_T, n.e = N.Clonidine,
                      event.c = Safety_dropout.severe.side.effects_C, n.c = N.Controls,
                      data = df, studlab = Author..Year, sm = "OR")
dropout_ae
forest.meta(dropout_ae, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Clonidine", label.c = "Control", allstudies = F,
            label.left = "Favours clonidine", label.right = "Favours control")