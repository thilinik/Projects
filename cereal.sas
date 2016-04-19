/*****************************************************/
/*                                                   */
/* Thilini Surendra                    */
/* Analysis of Cereal dataset                  */
/* Tasks performed */
/*1.look at the relationship between grams of fat
 and calories.
2. create dot,box and 
notched box plots and do an ANOVA to compare means

3. do the bootstrapping to estimate the ci and SE
4. simulate the data that satisfy and 
don't satisfy the assumptions of the model
*/

/* This next section of code clears the log and output windows of
   SAS program editor, removes all temporary datasets that are arount
   from previous run, and clears any footnotes that may have been set */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

/* Create the path variable(using the University Edition)*/
%let path=/folders/myfolders/Assignment2;
 
/* Use the title statements to label each page of the output.*/ 
title justify=left 'Surendra, Thilini   301260074' 
	   justify=center 'Assignment 2 Part 1';
title2 'Cereal Analysis';

/* Make the page orientation to portrait*/
options orientation =portrait; 

/*Sends output to the pdf file */
ods pdf file="&path/a02p01-cereal.pdf" style=styles.printer;

 
/* Reading in the cereal.csv file*/
proc import file="&path/cereal.csv" dbms=csv out=cereal replace;
run;

/* Deal with the missing values: replace -1s with .s */
data new_cereal;
	set cereal;
	array var(*) calories protein fat sodium fiber carbo sugars shelf potass vitamins weight cups; 	
	do i=1 to dim(var);
		if  var(i)=-1 then var(i)= .;
	end;
	drop i;
	
run;

/*Print out first 10 observations after dealing with missing values*/
proc print data=new_cereal(obs=10);
	title3 'First 10 observations after replacing -1 with missing values ';
run;

/* Re-code shelf variable as display_shelf*/
data new_coded_cereal;
	set new_cereal;
	display_shelf='none'; /* Set to a default value incase of a missing value */
	if shelf = 1 then display_shelf='low';
	if shelf=2 then display_shelf='mid';
	if shelf=3 then display_shelf='top'; 	
run;	

/*Print out first 10 observations after recoding the shelf variable*/
proc print data=new_coded_cereal(obs=10);	
	title3 'First 10 observations after recoding shelf as display_shelf ';
run;


/* create a scatter plot matrix (casement plot) */
proc sgscatter data=new_cereal;
   title3 'Scatter plot matrix of variables';
   matrix calories protein fat carbo sugars sodium;
run;

/* create a scatter plot of calories vs. grams of fat with some jittering */
/* add the fitted regression line */
proc sgplot data=new_cereal;
   title3 'Scatter plot of calories vs grams of fat';
   scatter y=calories x=fat / jitter ;
   reg     y=calories x=fat;
   yaxis label='Calories/serving' ;
   xaxis label='Fat (g)'          offsetmin=.05 offsetmax=0.05;
   footnote 'Points jittered to prevent overplotting';
run;


/* Estimate the regression line using Proc Reg */
/* do a simple regression of calories per grams of fat */
/* Estimate the calories at 4 grams of fat with a confidence interval
   for the mean response and a prediction interval for individual values
   by reading off one fo the graphs */
footnote " ";
ods graphics on;
proc reg data=new_cereal; * plots=(all diagnostics(unpack));
   title3 'Regresssion of calories vs. grams of fat';
   model calories = fat; */ clb ;
run;

/* Produces summary table by display shelves */
proc tabulate data=new_coded_cereal;
	title3 'Basic statistics';
	class display_shelf;
	var sugars;
	table display_shelf='Display Shelf', sugars*(n*f=5.0 mean*f=7.1 std*f=7.1);
run;


/* Dot plot of sugars by display shelves-with jittering  */
Proc SGplot data=new_coded_cereal;
	title3 'Basic plot WITH jittering';
	scatter x=display_shelf y=sugars/jitter;
	xaxis label='display shelf' offsetmax=0.10 offsetmin=0.10;
	yaxis label="sugars";
run;

/*Boxplots of sugars by display shelf without notches */
Proc SGplot data=new_coded_cereal;
	title3 'Vertical box-plots WITHOUT notches';
	vbox sugars / category=display_shelf ;
	xaxis label='display_shelf' offsetmax=0.10 offsetmin=0.10;
	yaxis label='sugars';
run;


/* To check whether mean amount of sugar changes with shelves */
proc glm data=new_coded_cereal PLOTS=(DIAGNOSTICS RESIDUALS);
	title3 'Does shelf height affect MEAN amount of sugar per serving?';
	class display_shelf;
	model sugars = display_shelf;
	lsmeans display_shelf / diff cl adjust=tukey lines;
	ods output LSmeanCL = mymeansCL;
run;


/*Print nicer table */
proc print data=mymeansCL label split=' ';
	title3 'Nicer table';
	var display_shelf LSMean LowerCL UpperCL;
	format LowerCL LSMean UpperCL 7.1;
	label display_shelf='Shelf'
		  LSMean = "Estimated mean"	
		  LowerCL = "Lower 95% CI"
		  UpperCL = "Upper 95% CI";	
	
run;	


/* Plot of means of sugar along with confidence intervals */
proc sgplot data=mymeansCL noautolegend;
	title3 'Estimated mean amount of sugar/serving by shelf location along with 95% ci'; 
	scatter x=display_shelf y = LSMean; 
	highlow x=display_shelf low=LowerCL high=UpperCL;
	xaxis label='Display Shelf';
	yaxis label='Sugar/serving (g) - mean and 95% ci';
run;


/*  Estimate mean, sd and sd using gini robust procedure and cis */
proc univariate data=new_cereal cibasic robustscale;
	title3 'Summary statistics of calories';
	var calories;
run; 

/* Set the macro variable(no.of replicates to 5)*/
%let nboot=1000;

/* Generate nboot number of bootstrap samples from the original sample
with replacement */
proc surveyselect data=new_cereal out=bootsample
	method=urs outhits rep=&nboot samprate=1 seed=2342332;
	title3 'Generate 1000 bootstrap samples';
run;	

/* Print first 20 of the bootstrap samples */
proc print data=bootsample(obs=20);
	title3 'Bootstrap samples-First 20 observations';
run;	

/* Table of no.of observations in each replicate*/
proc tabulate data=bootsample;
	title3 'Frequency of bootstrap replicates';
	class replicate;
	table replicate, n*f=5.0;
run;	


/*Summary statistics of calories by each replicate */
proc univariate data=bootsample noprint;
	by replicate;
	var calories;
	output out=bootstat 
			mean=mean_calories
			stddev=sd_calories
			std_gini=gini_std_calories;
run;		

/* summary statistics of bootstrap samples */
proc print data=bootstat;
	title3 'Summary statistics of bootstrap samples';
run;	

/* Find sd, and 2.5th, 97.5 th percentiles of sample means */
proc univariate data=bootstat noprint;
	var mean_calories;
	output out=SE_boot_mean
		stddev=SE_boot_mean
		pctlpts=2.5 97.5 pctlpre5=cl_mean;
run;

/* Print the sd, and 2.5th, 97.5 th percentiles of sample means*/
proc print data=SE_boot_mean;
	title3 'sd, 2.5th, 97.5 th percentiles of sample means';
run;	

/* Annotate the plot */
data sgannods;
	set SE_boot_mean;
	function= 'line';
	y1space='datapercent'; x1space='datavalue';
	y2space='datapercent'; x2space='datavalue';
	x1=cl_mean2_5; y1=0; x2=cl_mean2_5; y2=50; output;
	x1=cl_mean97_5; y1=0; x2=cl_mean97_5; y2=50; output;
run;

/* Print the annotation instructions*/
proc print data=sgannods;
	title3 'annotation instructions';
run;

/* Sampling distribution of sampling mean along with kernel density */
proc sgplot data=bootstat sganno=sgannods;
	title3 'Sampling distribution of sampling mean';
	histogram mean_calories / binwidth=1;
	density mean_calories /type=kernel;
	yaxis label='Frequency of mean calories'; 
	xaxis label='Mean calories';
run;

/* Calculate se of stddev */

/* Find sd, and 2.5th, 97.5 th percentiles of sample stdevs */
proc univariate data=bootstat noprint;
	var sd_calories;
	output out=SE_boot_sd
		stddev=SE_boot_sd
		pctlpts=2.5 97.5 pctlpre=cl_sd;
run;

/* Print the sd, and 2.5th, 97.5 th percentiles of sample stdevs*/
proc print data=SE_boot_sd;
	title3 'sd 2.5th, 97.5 th percentiles of sample stdevs';
run;	

/* Annotate the plot */
data sgannods;
	set SE_boot_sd;
	function= 'line';
	y1space='datapercent'; x1space='datavalue';
	y2space='datapercent'; x2space='datavalue';
	x1=cl_sd2_5; y1=0; x2=cl_sd2_5; y2=50; output;
	x1=cl_sd97_5; y1=0; x2=cl_sd97_5; y2=50; output;
run;

/* Print the annotation instructions*/
proc print data=sgannods;
	title3 'annotation instructions';
run;

/* Sampling distribution of sampling stdev along with kernel density */
proc sgplot data=bootstat sganno=sgannods;
	title3 'Sampling distribution of sampling stdev';
	histogram sd_calories / binwidth=1;
	density sd_calories /type=kernel;
	yaxis label='Frequency of stdev of calories'; 
	xaxis label='stdev calories';
run;

/* Calculate se of gini_sd */

/* Find sd, and 2.5th, 97.5 th percentiles of sample gini stdevs */
proc univariate data=bootstat noprint;
	var gini_std_calories;
	output out=SE_boot_gini
		stddev=SE_boot_gini_sd
		pctlpts=2.5 97.5 pctlpre=cl_gini;
run;

/* Print the sd, and 2.5th, 97.5 th percentiles of sample gini stdevs*/
proc print data=SE_boot_gini;
	title3 'sd 2.5th, 97.5 th percentiles of sample gini stdevs';
run;	

/* Annotate the plot */
data sgannods;
	set SE_boot_gini;
	function= 'line';
	y1space='datapercent'; x1space='datavalue';
	y2space='datapercent'; x2space='datavalue';
	x1=cl_gini2_5; y1=0; x2=cl_gini2_5; y2=50; output;
	x1=cl_gini97_5; y1=0; x2=cl_gini97_5; y2=50; output;
run;

/* Print the annotation instructions*/
proc print data=sgannods;
	title3 'annotation instructions';
run;


/* Sampling distribution of sampling stdev along with kernel density */
proc sgplot data=bootstat sganno=sgannods;
	title3 'Sampling distribution of sampling gini-stdev';
	histogram gini_std_calories / binwidth=1;
	density gini_std_calories /type=kernel;
	yaxis label='Frequency of gini-stdev of calories'; 
	xaxis label='gini-stdev of calories';
run;

/* Create the model space */
data ModelSpace;
	set new_cereal;
	keep protein fat carbo sugars;
run;

/*Printing the first 10 observations*/
proc print data=ModelSpace(obs=10);
	title3 'First 10 observations of the model Space';
run;


/* Set the parameter values for x variables */
%let beta_0 = 0;
%let beta_fat = 9;
%let beta_protein = 4;
%let beta_sugars= 4;
%let beta_complex_carbs= 4;

/* sd of the noise */
%let sigma = 5; 
/* Number of simulations */
%let nsim = 1000;

/* min and maximum of xaxis of histogram */
%let minx=6;
%let maxx=13;

/* Generate simulated data */
data simdata_no_violations;
	call streaminit(314159);
	do sim=1 to &nsim;
		do mycereal=1 to ncereals;
			set ModelSpace point=mycereal nobs=ncereals; 
			mu_calories = &beta_0 + &beta_fat * fat + &beta_protein * protein +
						 &beta_complex_carbs * carbo + &beta_sugars * sugars;
			epsilon = rand('normal', 0, &sigma);
			calories = mu_calories + epsilon;
			output;
		end;
	end;
	stop; 
run;

/*Printing the first 10 observations*/
proc print data=simdata_no_violations(obs=10);
	title3 'First 10 observations of the simulated data';
run;

/* multiple regression of calories on protein,fat, carbo, sugars for each simulated sample */
proc reg data=simdata_no_violations noprint outest=MyEst_no_violations;
   by sim;
   model calories = protein	fat	carbo sugars;
run;

/*Printing the regression coefficients of first 10 simulated samples*/
proc print data=MyEst_no_violations(obs=10);
	title3 'Regression coefficients of first 10 simulated samples';
run;

/* Sampling distribution of the parameter estimates */
proc univariate data=MyEst_no_violations location = &beta_0 &beta_fat &beta_protein &beta_complex_carbs &beta_sugars;
	var intercept fat protein carbo sugars;
	histogram / normal;
run;


/* Sampling distribution of the slope */
proc sgplot data=MyEst_no_violations ;
  title "Sampling distribution of the slope";
  histogram fat;
  density fat;
  xaxis min=&minx max=&maxx;
  keylegend / location=inside position=topright;
  refline &beta_fat / axis=x lineattrs=(thickness=10) label='Population slope';
run; 


/* Regenerate the simulated data when the residuals are not normally distributed */
data simdata_violations;
	call streaminit(314159);
	do sim=1 to &nsim;
		do mycereal=1 to ncereals;
			set ModelSpace point=mycereal nobs=ncereals; 
			mu_calories = &beta_0 + &beta_fat * fat + &beta_protein * protein +
						 &beta_complex_carbs * carbo + &beta_sugars * sugars;
			W = rand('lognormal');
			epsilon=(W-exp(0.5))/sqrt((exp(1)-1)*exp(1))*&sigma;
			calories = mu_calories + epsilon;
			output;
		end;
	end;
	stop; 
run;

/*Printing the first 10 observations*/
proc print data=simdata_violations(obs=10);
	title3 'First 10 observations of the simulated data(with violations)';
run;

/* multiple regression of calories on protein,fat, carbo, sugars for each simulated sample */
proc reg data=simdata_violations noprint outest=MyEst_violations;
   by sim;
   model calories = protein	fat	carbo sugars;
run;

/*Printing the regression coefficients of first 10 simulated samples*/
proc print data=MyEst_violations(obs=10);
	title3 'Regression coefficients of first 10 simulated samples(with violations)';
run;

/* Sampling distribution of the parameter estimates */
proc univariate data=MyEst_violations location = &beta_0 &beta_fat &beta_protein &beta_complex_carbs &beta_sugars;
	var intercept fat protein carbo sugars;
	histogram / normal;
run;

/* Sampling distribution of the slope */
proc sgplot data=MyEst_violations ;
  title "Sampling distribution of the slope(assumptions violated)";
  histogram fat;
  density fat;
  xaxis min=&minx max=&maxx;
  keylegend / location=inside position=topright;
  refline &beta_fat / axis=x lineattrs=(thickness=10) label='Population slope';
run;



ods pdf close;