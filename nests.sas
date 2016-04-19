/*****************************************************/
/*                                                   */
/*  Surendra, Thilini   301260074                    */
/*  Stat 340, Spring 2015                            */
/*  Assignment 2, Part 3  - Cigarette butts in bird nests*/
/*                                                   */
/* Read in the nests data set, derive new variables, estimate 
binomial proportions and perform regressions.
/*
/* Change log
/*  2015-01-15 TKS */
/* Assignment 2, Part 3 - Analysis of cigarette butts in bird nests */




 /*This next section of code clears the log and output windows of
   SAS program editor, removes all temporary datasets that are arount
   from previous run, and clears any footnotes that may have been set */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

/* Create the path variable(using the University Edition)*/
%let path=/folders/myfolders/Assignment2;
 
/*Sends output to the pdf file */
ods pdf file="&path/a02p03-nests.pdf" style=styles.printer; 
 
/* Use the title statements to label each page of the output.*/ 
title justify=left 'Surendra, Thilini   301260074'
	  justify=center 'Assignment 2 Part 3';
title2 'Cigarette butts Analysis';

/* Make the page orientation to portrait*/
options orientation =portrait; 


/* Reading in the nests.xls file*/
proc import file="&path/nests.xls" dbms=xls out=nestinfo replace;
	sheet = 'Correlational';
	guessingrows = 9999;
run;

/* Print out first 10 observations */
proc print data=nestinfo(obs=10);
	title3 'First 10 observations';
run;	

/* Summary of number of nests by species and nest contents */
proc tabulate data=nestinfo;
	title3 'Summary of number of nests by species and nest content';
	class species nest_content;
	table nest_content, species*n*f=5.0;
run;

/* Create a new derived variable 'butts_present' based on butts_weight */
data nestinfo;
	set nestinfo; /* access the previous dataset */
	length butts_present $4;
	butts_present = 'no';
	if Butts_weight>0 then butts_present = 'yes';
run;

/* Print out first 10 observations after creating butts_present */
proc print data=nestinfo(obs=10);
	title3 'First 10 observations with butts_present';
run;

/*Sort the data by species*/
proc sort data=nestinfo; by species; run;

/* Produce a table for each species showing the number and proportion */
Proc Freq data=nestinfo;
	by species;
	table butts_present / binomial(level='yes');
	output out=nestprop2 binomial;
	ods output BinomialProp=nestprop;
run;

/*Print nestprop dataset*/
proc print data=nestprop;
   title3 'proportions and other statistics-several lines per species';
run;

/*Print nestprop2 dataset*/
proc print data=nestprop2;
   title3 'proportions and other statistics-one line per species';
run;

/* Provides a plot to compare proportion of nests with butts present (with c.i) among species */
proc sgplot data=nestprop2 noautolegend;
   title3 'Comparison of proportions of nests with butts present (and 95% confidence intervals) among species';
   
   scatter y=_BIN_ x=Species;
   highlow x=Species low=L_BIN high=U_BIN;
   yaxis label="proportion of nests with butts present";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Species';
run;

/*Perform chi_square test for equal proportions */
proc freq data=nestinfo;
	title3 'chi square test for equal proportions';
	table species*butts_present / chisq nocol nopercent;
run;

/* Summary statistics of Butts weight by species */
proc univariate data=nestinfo cibasic;
	by species;
	var Butts_weight;
	ods output basicintervals=mycibuttweight;
run;

/*Print out the table of mean Butts weight along with conf.intervals*/
proc print data=mycibuttweight;
	title3 'Table of mean Butts weight along with conf.intervals';
run;

/* Subset only the mean from mycibuttweight dataset */
data meanonly;
	set mycibuttweight;
	if Parameter = 'Mean';
run;

/* Produces a plot of mean butt weight along with 95% ci. between species */
proc sgplot data=meanonly noautolegend;
   title3 'Comparison of mean butt weight (and 95% confidence intervals) among species';  
   scatter y=Estimate x=Species;
   highlow x=Species low=LowerCL high=UpperCL;
   yaxis label="mean butts weight";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Species';
run;

/* comparison of mean butt weights among species using t-test*/
proc ttest data=nestinfo;
	title3 'comparison of mean butt weights';
	class species;
	var Butts_weight;
run;

/* Summary statistics of Number of mites by species */
proc univariate data=nestinfo cibasic;
	by species;
	var Number_of_mites;
	ods output basicintervals=mynum_mites;
run;

/* Subset only the mean from mynum_mites dataset */
data meanonly_mites;
	set mynum_mites;
	if Parameter = 'Mean';
run;

/* Produces a plot of mean number of mites along with 95% ci. between species */
proc sgplot data=meanonly_mites noautolegend;
   title3 'Comparison of mean no.of mites (and 95% confidence intervals) among species';  
   scatter y=Estimate x=Species;
   highlow x=Species low=LowerCL high=UpperCL;
   yaxis label="mean no.of mites";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Species';
run;

/* comparison of mean no.of mites among species using t-test*/
proc ttest data=nestinfo;
	title3 'comparison of mean no.of mites';
	class species;
	var Number_of_mites;
run;

/* Produces scatterplot of number of mites vs weight of butts by species */
proc sgplot data=nestinfo;
	title3 'number of mites vs weight of butts';
	scatter x=Butts_weight y=Number_of_mites / group=species;
	yaxis label="Number of mites";
	xaxis label="butts weight";
run;

/* Produces scatterplot of number of mites vs weight of butts by species-with splines */
proc sgplot data=nestinfo;
	title3 'number of mites vs weight of butts';
	scatter x=Butts_weight y=Number_of_mites / group=species;
	pbspline x=Butts_weight y=Number_of_mites / nomarkers; 
	yaxis label="Number of mites";
	xaxis label="butts weight";
run;

/*create a new variable log_mites */
data new_nestinfo;
	set nestinfo;
	log_mites=log(Number_of_mites);
run;

/* Regression of log_mites on butt weight */
proc reg data=new_nestinfo;
	title3 'regression of log(number of mites) on butt weight';
	model log_mites = Butts_weight;
	output out=modelfit pred=estmean_log lclm=lclm_log uclm=uclm_log;
run;	

/* Anti-log transform of the predicted mean */
data modelfit;
	set modelfit;
	estmean = exp(estmean_log);
	estmean_lcl = exp(lclm_log);
	estmean_ucl = exp(uclm_log);
run;

/* Print out first few observations*/
proc print data=modelfit(obs=10);
	title3 'First few observations';
run;	

/* Sort the data */
proc sort data=modelfit; by Butts_weight; run;

/* Fitted regression line of log(number mites) vs butt weight on the ANTI-log scale*/
proc sgplot data=modelfit;
	title3 'Fitted regression line of log(number mites) vs butt weight on the ANTI-log scale';
	band x=Butts_weight upper=estmean_ucl lower=estmean_lcl;
	scatter x=Butts_weight  y=Number_of_mites / group=species;
	series x=Butts_weight y=estmean;
run;


ods pdf close;








