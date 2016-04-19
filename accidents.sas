/*****************************************************/
/*                                                   */
/* Thilini Surendra                    */
/* Analysis of accidents dataset                  */
/*  Tasks performed                                                 */
/*1. Read in the Road accidents data set, format date and times,
create summary tables, graphs and histograms of road
accidents and compare means of accidents across months
2.summarize the dataset, merge 2 datasets using a common variable and compare odds 
among groups
3. recode the variables and test for the impact of area and speed on fatality

/*




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
	   justify=center 'Assignment 2 Part 2';
title2 'Road Accidents Analysis';

/* Make the page orientation to portrait*/
options orientation =portrait; 

/*Sends output to the pdf file */
ods pdf file="&path/a02p02-accidents.pdf" style=styles.printer;

 
/* Reading in the road-accidents-2010.csv file*/
/* Read only the first 12 variables */
data accidents;
	
	infile "&path/road-accidents-2010.csv" missover dlm=',' dsd firstobs=2;
	length Accident_Index $20;
	input Accident_Index Location_Easting_OSGR Location_Northing_OSGR Longitude
		  Latitude Police_Force Accident_Severity Number_of_Vehicles Number_of_Casualties
		  Date :ddmmyy10. Day_of_Week  Time:hhmmss10.;		
							   	
	attrib Date label='Accident Date' format=yymmdd10.;
	attrib Time label='Accident Time' format=hhmm5.;
run;

/* Print out first 10 observations */
proc print data=accidents(obs=10);
   title3 'First 10 observations';
run;	

/*Summarizing the dataset to get an idea about the data and any unusaul observations */
proc tabulate data=accidents missing;
	
	class Accident_Severity Police_Force Number_of_Vehicles
		 Number_of_Casualties Day_of_Week; 
	table Accident_Severity ALL, n*f=7.0;
	table Police_Force ALL, n*f=7.0;
	table Number_of_Vehicles ALL, n*f=7.0;
	table Number_of_Casualties ALL, n*f=7.0;
	table Day_of_Week ALL, n*f=7.0;
	
run;

/* Sort the dataset by accident dates */
proc sort data=accidents; by Date; run;

/* Produces number of accidents by the day of the year */
proc means data=accidents noprint;
	by Date;
	var Date;
	output out=dailysummary n=naccidents;
run;

/* Print out the number of accidents for the first 10 days */
proc print data=dailysummary(obs=10);
	title3 'Number of accidents for the first 10 days';
run;

/*Plots number of accidents vs. Date */
proc sgplot data=dailysummary;
	title3 'Number of accidents by date';
	scatter y=naccidents x=Date;
	xaxis label='Date';
	yaxis label='Number of accidents';
run;

/*Plots number of accidents vs. Date with the loess curve */
proc sgplot data=dailysummary;
	title3 'Number of accidents by date with loess curve';
	scatter y=naccidents x=Date;           /* also draws scatter plot */
	loess x=Date y=naccidents / nomarkers;  
run;

/* Extract month and date of accident date and append to dailysummary dataset */
data dailysummary;
	set dailysummary;
	month = month(Date);
	day = day(Date);
run;

/*Print out the first 10 observations of modified daily summary  */
proc print data=dailysummary(obs=10);
	title3 'First 10 observations of modified summary';
run;

/* Summarizes the mean and std. dev of daily accidents */
proc tabulate data=dailysummary;
	title3 'Mean and std dev of daily accidents for each month';
	class month;
	var naccidents;
	table month, naccidents*(n*f=5.0 mean*f=5.2 std*f=5.2);
run;

/*  Test whether the mean number of accidents is same for all the months */
proc glm data=dailysummary PLOTS=(DIAGNOSTICS RESIDUALS);
	title3 'Is there a difference in the mean number of accidents by month?';
	class month;
	model naccidents = month;
	lsmeans month / diff cl adjust=tukey lines;
	ods output lsmeancl = mymeanscl;
run;


/*Print nicer table with mean number of accidents along with confidence intervals*/
proc print data=mymeansCL label split=' ';
	title3 'Nicer table';
	var month LSMean LowerCL UpperCL;
	format LowerCL LSMean UpperCL 7.1;
	label month='Month'
		  LSMean = "Estimated mean"	
		  LowerCL = "Lower 95% CI"
		  UpperCL = "Upper 95% CI";	
	
run;


/* Plot of means of accidents along with confidence intervals */
proc sgplot data=mymeansCL noautolegend;
	title3 'Estimated mean number of accidents by months along with 95% ci'; 
	scatter x=month y = LSMean; 
	highlow x=month low=LowerCL high=UpperCL;
	xaxis label='Month';
	yaxis label='number of accidents - mean and 95% ci';
run;



/* Creating new variables (hour and minutes) */
data accidents;
	set accidents;
	hour = hour(Time);
	minute = minute(Time);
run;

/*Print out the first 10 observations of modified accidents data set  */
proc print data=accidents(obs=10);
	title3 'First 10 observations'
run;

/* Histogram of hour of accidents and kernel density estimate is superimposed */
proc sgplot data=accidents;
	title3 'Histogram of hour of accidents';
	histogram hour / binwidth=1;
	density hour /type=kernel;
	yaxis label='percentage of accidents'; 
run;

/* Histogram of hour of accidents and kernel density estimate is superimposed */
proc sgplot data=accidents;
	title3 'Histogram of minutes of accidents';
	histogram minute / binwidth=1;
	density minute /type=kernel;
	yaxis label='percentage of accidents';
run;

/*  Read only 3 cols from the accidents data sets */

data accidents_info;
	infile "&path/road-accidents-2010.csv" missover dlm=',' dsd firstobs=2;
	length  Accident_Index $20; *du $1;
	input Accident_Index du du du du du 
		  Accident_Severity Number_of_Vehicles;
	*drop du;	  
run;

/* Print out first 10 obs */
proc print data=accidents_info(obs=10);
	title3 'First 10 observations of accidents dataset'	;
run;	

/* Subset accidents which involved only 2 cars */
data two_only;
	set accidents_info;
	if Number_of_Vehicles=2;
	
run;	

/* create a derived variable 'fatality' based on severity */
data two_fatal_only;
	set two_only;
	length fatality $5;
	fatality='';
	if Accident_Severity=1 then fatality= 'yes';
	if Accident_Severity =2 or Accident_Severity =3 then fatality= 'no';
run;

/* Print out first 10 obs */
proc print data=two_fatal_only(obs=10) label split=' ';
	title3 'First 10 observations of accidents dataset which involved 2 vehicles'	;
run;
/* check recoding of fatality */
proc tabulate data=two_fatal_only missing;
	title3 'Table of accident severity and fatality';
	class Accident_Severity fatality ;
	table Accident_Severity, fatality*n*f=comma8. ALL;
run;	

/* Read in vehicles data- only Accident_Index  and sex  */
data vehicles_info;
	infile "&path/road-accidents-vehicles-2010.csv" missover dlm=',' dsd firstobs=2;
	length dummy $1 Accident_Index $20; 
	input Accident_Index dummy dummy dummy dummy dummy dummy dummy dummy dummy dummy dummy 
		  dummy dummy Sex;
	drop dummy;	  
run;

/* Print the first 10 observations of vehicles dataset */
proc print data=vehicles_info(obs=10) label split=' ';
	title3 'First 10 observations of vehicles dataset'	;
run;

/* Create a derived variable female based on the sex */
data vehicles_info_coded;
	set vehicles_info;
	
	if Sex=2 then female=1;
	if sex=1 then female=0;
	if sex=3 or sex=-1 then female='';
	/* convert female to numeric variable */
	female=female+0;
	
run;	

/* Print the first 20 observations of vehicles_coded dataset */
proc print data=vehicles_info_coded(obs=20) label split=' ';
	title3 'First 20 observations of vehicles dataset-female coded'	;
run;

/* Check the coding of female */

proc tabulate data=vehicles_info_coded missing;
	title3 'Table of sex and females: check the coding';
	class sex female;
	table sex, female*n*f=comma8.;
run;


/* Sort data by Accident_Index */
proc sort data=vehicles_info_coded; by Accident_Index ; run;

/* Count the number of drivers in each accident, # of female drivers, # of drivers
with missing sex */
proc means data=vehicles_info_coded noprint;
	by Accident_Index ;
	var female;
	output out=vehicle_summary n=ndriver sum(female)=nfemale
	nmiss(female)=nmisssex;
run;

/* Print out the first 10 observations of the vehicle summary */
proc print data=vehicle_summary(obs=10) label split=' ';
	title3 'First 10 observations of vehicles dataset-with number of drivers'	;
run;

/* Subset the data set which only involved 2 drivers with missing sex=0 */
data two_only_drivers;
	set vehicle_summary;
	if ndriver=2 and nmisssex=0;
run;	

/* Print out the first 10 observations of the vehicle summary */
proc print data=two_only_drivers(obs=10) label split=' ';
	title3 'First 10 observations of vehicles dataset-with 2 drivers'	;
run;

/* Sort the two data sets */
proc sort data=two_fatal_only; by Accident_Index; run;
proc sort data=two_only_drivers; by Accident_Index; run;

/* Merge two datasets by accident index */
data both;
	merge two_fatal_only two_only_drivers;
	by Accident_Index;
run;

/* Print out the first 20 observations of the merged */
proc print data=both(obs=20) label split=' ';
	title3 'First 20 observations of merged dataset'	;
run;

/* Summarize no.of females and fatality level */
proc tabulate data=both missing;
	title3 'Table of no.of females and fatality level';
	class nfemale fatality;
	table nfemale, n*f=comma8. fatality*f=comma8. fatality*pctn<fatality>*f=5.1;
run;	


proc genmod data=both descending;
	title3 'Examine if fatality rate is the same across number of females';
	class nfemale;
	model fatality = nfemale/ dist=binomial link=logit type3;
	lsmeans nfemale / cl diff ilink oddsratio;
	ods output lsmeans=myodds;
run;

/* Print out the first 20 observations of the merged */
proc print data=myodds;* label split=' ';
	title3 'estimated odds of fatality with 95% ci'	;
run;

/* Plot of means of sugar along with confidence intervals */
proc sgplot data=myodds noautolegend;

	title3 'Estimated odds of fatality with 95% ci by number of females involved'; 
	scatter x=nfemale y = Mu; 
	highlow x=nfemale  low=LowerMu high=UpperMu;
	xaxis label='Number of females';
	yaxis label='Odds of fatality and 95% ci';
run;


/*  Read only 3 cols from the accidents data sets */

data accidents_info;
	infile "&path/road-accidents-2010.csv" missover dlm=',' dsd firstobs=2;
	length dummy $1 Accident_Index $20; 
	input Accident_Index dummy dummy dummy dummy dummy 
		  Accident_Severity	dummy dummy dummy dummy dummy dummy dummy dummy dummy dummy
			Speed_limit dummy dummy dummy dummy dummy dummy dummy dummy dummy dummy dummy Urban_or_Rural_Area;
	drop dummy;	  
run;

/* Print out first 10 obs */
proc print data=accidents_info(obs=10);
	title3 'First 10 observations of accidents dataset'	;
run;

/* create a derived variable 'fatality' based on severity */
data accidents_fatal;
	set accidents_info;
	length fatality $5;
	fatality='';
	if Accident_Severity=1 then fatality= 'yes';
	if Accident_Severity =2 or Accident_Severity =3 then fatality= 'no';
	
run;

/* create a derived variable 'area' */
data accidents_fatal_area;
	set accidents_fatal;
	length area $10;
	area='';
	if Urban_or_Rural_Area=1 then area='urban';
	if Urban_or_Rural_Area=2 then area='rural';
	if Urban_or_Rural_Area=3 then area='';
	
	if area = '' then delete;
	
run;

/* create a derived variable 'speed' based on speed */
data accidents_fatal_area_speed;
	set accidents_fatal_area;
	length speed $20;
	speed='';
	
	if Speed_limit>0 AND Speed_limit <=30 then speed='00+-30';
	if Speed_limit>30 AND Speed_limit <=50 then speed='30+-50';
	if Speed_limit>=60 then speed='60+'; 
run;
	
/* check recoding of fatality */
proc tabulate data=accidents_fatal missing;
	title3 'Table of accident severity and fatality';
	class Accident_Severity fatality ;
	table Accident_Severity, fatality*n*f=comma7. ALL;
run;

/* check recoding of area */
proc tabulate data=accidents_fatal_area missing;
	title3 'Table of Urban_or_Rural_Area and area';
	class Urban_or_Rural_Area area;
	table Urban_or_Rural_Area, area*n*f=comma7. ALL;
run;

/* check recoding of speed */
proc tabulate data=accidents_fatal_area_speed missing;
	title3 'Table of speed_limit and speed';
	class Speed_limit speed;
	table Speed_limit, speed*n*f=comma7. ALL;
run;


/* Print out first 10 obs after recoding*/
proc print data=accidents_fatal_area_speed (obs=10) label split=' ';
	title3 'First 10 observations of accidents dataset after recoding'	;
run;

/* Examine if fatality rate is the same across area and speed */
proc genmod data=accidents_fatal_area_speed descending;
	title3 'Examine if fatality rate is the same across area and speed';
	class  area speed;
	model fatality = area speed area*speed/ dist=binomial link=logit type3;
	lsmeans area*speed / cl diff ilink oddsratio;
	ods output lsmeans=mylogodds;
run;

/* Print the table of log(odds) with 95% ci */
proc print data=mylogodds label split=' ' noobs;
	title3 'estimated odds of fatality and log(odds of fatality) with 95% ci';
	var area speed Estimate Lower Upper;
	format  Estimate Lower Upper 7.2;
	label Estimate='log(odds)'
		  Lower='Lower 95% CI'
		  Upper='Upper 95% CI';	
	format 			
run;

/* Plot of log(odds) and 95% ci */
proc sgplot data=mylogodds;

	title3 'Estimated log(odds) of fatality with 95% ci by area and speed'; 
	scatter x=speed y = Estimate / group=area;
	highlow x=speed  low=Lower high=Upper/ group=area;;
	series x=speed y = Estimate / group=area;
	xaxis label='Speed';
	yaxis label='log(Odds) of fatality and 95% ci';
run;

/* Close the pdf file */
ods pdf close;

