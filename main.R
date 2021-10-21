# Title     :   Study3_Quantitative
# Description:  Preparation to Quiz
# Created by:   Mammarella Davide
# Created on:   19/10/2021

#----------------------------------------------------------------------------------------
#	Workspace and Dataset Setup
#----------------------------------------------------------------------------------------

# all necessary packages are installed
install.packages("here")
install.packages("effsize")

# all necessary packages are called
library("here")
library("effsize")

# dataset import
dataset <- read.csv(here("dataset", "study3_quantitative.csv"), sep=",", header = TRUE)
cat("\n ================================================== \n Dataset: \n \n")
str(dataset)
# null values in the dataset
nullvalue_present <- sum(is.na(dataset))
cat("\n ================================================== \n [Dataset] Null values: \n")
cat(nullvalue_present, "\n")

#----------------------------------------------------------------------------------------
#	Descriptive Statistics
#----------------------------------------------------------------------------------------

## CREATE SUBSET ------------------------------------------------------------------------
data_ce<-dataset[which(dataset["Treatment"] == 'ce'),]
data_nce<-dataset[which(dataset["Treatment"] == 'nce'),]
cat("\n ================================================== \n [Descriptive Statistics] Subsets of Completeness: \n")
df <- do.call(rbind, Map(data.frame, CE=data_ce$Completeness, NCE=data_nce$Completeness))
df
rm(df)

## MEAN ---------------------------------------------------------------------------------
ce_mean<-mean(data_ce$Completeness)
nce_mean<-mean(data_nce$Completeness)
cat("\n ================================================== \n [Descriptive Statistics] Completeness Mean: \n")
cat("CE:\t", ce_mean, "\nNCE\t",nce_mean)


## MEDIAN -------------------------------------------------------------------------------
ce_median<-median(data_ce$Completeness)
nce_median<-median(data_nce$Completeness)
cat("\n ================================================== \n [Descriptive Statistics] Completeness Median: \n")
cat("CE:\t", ce_median, "\nNCE\t",nce_median)


## X33 PERCENTILE -----------------------------------------------------------------------
ce_quantile <- quantile(data_ce$Completeness, 0.33)
nce_quantile <- quantile(data_nce$Completeness, 0.33)
cat("\n ================================================== \n [Descriptive Statistics] Completeness Quantile: \n")
cat("CE:\t", ce_quantile, "\nNCE\t",nce_quantile)

## STANDARD DEVIATION -------------------------------------------------------------------
ce_sd <- sd(data_ce$Completeness)
nce_sd <- sd(data_nce$Completeness)
cat("\n ================================================== \n [Descriptive Statistics] Completeness Range: \n")
cat("CE:\t", ce_sd, "\nNCE\t",nce_sd)

## RANGE --------------------------------------------------------------------------------
ce_range <- max(data_ce$Completeness)-min(data_ce$Completeness)
nce_range <- max(data_nce$Completeness)-min(data_nce$Completeness)
cat("\n ================================================== \n [Descriptive Statistics] Completeness Range: \n")
cat("CE:\t", ce_range, "\nNCE\t",nce_range)

#----------------------------------------------------------------------------------------
#	Parametric Test (Distribution Verification) - LO FAI SU STESSA COLONNA (SEMPLICEMENTE ESPERIMENTI DIFFERENTI)
#----------------------------------------------------------------------------------------

## NULL HYPOHESIS: DISTRIBUITION IS NORMALLY DISTRIBUTED (Alpha 0.05)
## if you get a p-value>=0.05 then you do not reject the NULL hypothesis and therefore the distribuition is Normal
## if you get a p-value<0.05 then you reject the hypothesis and so the distribuition is non Normal

#verify that the distributions are Normally distributed (Shapiro-Wilk)
cat("\n ================================================== \n [Distribution Verification] Shapiro-Wilk: \n")

ce_p <- shapiro.test(data_ce$Completeness)
if (ce_p$p.value>=0.05){
  cat("CE is Normal (go with parametric test)!\np-value:\t", ce_p$p.value)
} else {
  cat("CE is not Normal (go with non-parametric test)!\np-value:\t", ce_p$p.value)
}

nce_p <- shapiro.test(data_nce$Completeness)
if (nce_p$p.value>0.05){
  cat("\nNCE is Normal (go with parametric test)!\np-value:\t", nce_p$p.value)
} else {
  cat("\nNCE is not Normal (go with non-parametric test)\np-value:\t", nce_p$p.value)
}

#----------------------------------------------------------------------------------------
#	Correlation/Boxplot
#----------------------------------------------------------------------------------------

#if Normally Distributed: pearson
#if not Normally Distributed: spearman (SPEARMAN if linear, KENDALL if exponential)
firstDistribution <- dataset$ProgYears
secondDistribution <- dataset$Completeness

#Correlation coefficients take the values between minus one and plus one.
#The positive correlation signifies that the ranks of both the variables are increasing.
#On the other hand, the negative correlation signifies that as the rank of one variable is increased, the rank of the other variable is decreased.
#Correlation analyses can be used to test for associations in hypothesis testing.
#The null hypothesis is that there is no association between the variables under study.  Thus, the purpose is to investigate the possible association in the underlying variables.  It would be incorrect to write the null hypothesis as having no rank correlation between the variables.

# CORRELATION ---------------------------------------------------------------------------
#check correlation between Experience and Completeness (ON TWO DIFFERENT COLUMNS, BECAUSE YOU CHECK CORRELATION BETWEEN DIFFERENT THINGS)
correlation <- cor(firstDistribution, secondDistribution, method="pearson")
correlation_modulized <- abs(correlation)

cat("\n ================================================== \n [Correlation] Check: \n")
if(correlation_modulized>0.7){
  cat("Strong correlation between the variables!\n|corr|: ", correlation_modulized)
} else if(correlation_modulized>0.5 && correlation_modulized<0.7){
  cat("Moderate correlation between the variables!\n|corr|: ", correlation_modulized)
} else if(correlation_modulized>0.3 && correlation_modulized<0.5){
  cat("Weak correlation between the variables!\n|corr|: ", correlation_modulized)
} else if(correlation_modulized<=0.3){
  cat("No correlation between the variables!\n|corr|: ", correlation_modulized)
}

## BOXPLOT (visualizing dispersion and skewedness of data) ------------------------------
cat("\n ================================================== \n [Correlation] Boxplot: \n")

dir.create(here("output"), showWarnings = FALSE)
pdf(here("output", "boxplot.pdf"))
boxplot(data_nce$Completeness, data_ce$Completeness,
      col="gray", boxwex = 0.25, axes=TRUE, names=c("NCE","CE"))
invisible(dev.off())

cat("Boxplot printed! (Check output folder)")

#----------------------------------------------------------------------------------------
#	Hypotesis Testing (T-Test / Effect Size)
#----------------------------------------------------------------------------------------

# NULL HYPOTESIS: THERE ISN'T ...
#if Normally Distributed: parametric test (t-test, cohen)
#if not Normally Distributed: non parametric test (wilcox/mann-whitney, cliff)

firstDistribution <- data_ce$Completeness
secondDistribution <- data_nce$Completeness

## T-TEST / WILCOX ----------------------------------------------------------------------
## p-value>=0.05 supporta l'ipotesi nulla
## p-value<0.05 supporta l'ipotesi alternativa

# compare two independent samples ON NORMALLY DISTRIBUITED (PARAMETRIC: T-TEST)
cat("\n ================================================== \n [Hypotesis Testing] T-Test: \n")
t_test <- t.test(variable1, variable2)
t_test

cat("\n ================================================== \n [Hypotesis Testing] Wilcox: \n")
# compare two independent samples ON NON NORMALLY DISTRIBUITED (NON PARAMETRIC: WILCOX)
wilcox_test <- wilcox.test(variable1, variable2, paired=TRUE)
wilcox_test

## EFFECT SIZE --------------------------------------------------------------------------
# check effect size ON NORMALLY DISTRIBUITED (PARAMETRIC: COHEN)
cohen_effect_size <- (mean(firstDistribution) - mean(secondDistribution))/ (sd(firstDistribution) - sd (secondDistribution))

# check effect size ON NON NORMALLY DISTRIBUITED (NON PARAMETRIC: CLIFF)
cliff_effect_size <- cliff.delta(firstDistribution,secondDistribution)

# effect size output (CHANGE THE TYPE OF EFFECT SIZE)
effect_size <- cohen_effect_size
effect_size_modulized <- abs(effect_size)
cat("\n ================================================== \n [Hypotesis Testing] Effect Size: \n")
if(effect_size_modulized>=0.474){
  cat("Large effect size!\n|d|: ", effect_size_modulized)
} else if(effect_size_modulized>=0.33 && effect_size_modulized<0.474){
  cat("Medium effect size!\n|d|: ", effect_size_modulized)
} else if(effect_size_modulized>=0.148 && effect_size_modulized<0.33){
  cat("Small effect size!\n|d|: ", effect_size_modulized)
} else if(effect_size_modulized<0.148){
  cat("Negligible effect size!\n|d|: ", effect_size_modulized)
}