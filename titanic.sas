/*****************************************************/
/* Thilini Surendra                    */
/* Analysis of Titanic dataset                  */
/* Tasks performed */
/* Read in the Titanic dataset, see how many
/* survived and dies, and then plot the  
/* odds ratio of survival by passenger class
/*
/*****************************************************/

/* This next section of code clears the log and output windows of
   SAS program editor, removes all temporary datasets that are arount
   from previous run, and clears any footnotes that may have been set */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; 
footnote " ";
run;
/* Use the title statements to label each page of the output.*/ 
title justify=left 'Thilini Surendra' 
	   justify=center 'Cereal Analysis';


/* Read the titanic dataset */
data titanic;
   infile 'http://www.statsci.org/data/general/titanic.txt' url  dlm='09'x dsd missover firstobs=2 ;
   length name $70 sex $10;
   input name $  Pclass $ age sex $ survived;
run;


/* Print the titanic dataset */
proc print data=titanic(obs=10);
   title3 'part of the raw data';
run;

/* Tabulate the # of passengers died and lived */
proc tabulate data=titanic missing;
   title3 'How many died and lived';
   class pclass sex survived;
   table pclass*sex, survived*n*f=5.0;
run;


/* Tabulate the proportion of passengers survived */
proc tabulate data=titanic missing;
   title3 'What proportion survived';
   class pclass sex ;
   var survived;
   table pclass, sex*survived*mean*f=5.2;
run;



/* Compute the odds ratios */
proc sort data=titanic; by pclass; run;

/*Compare survival rates between the classes*/
proc freq data=titanic;
   title3 'Compare survival rates between the classes';
   by pclass;
   table sex*survived / chisq nocol nopercent relrisk measures plots=(oddsratioplot) ;
   ods output RelativeRisks=RelRisks;
run;

/* Print the OR*/
proc print data=RelRisks;
   title3 'Odds ratio of survival (and other statistics)';
run;

/* Comparison of OR */
proc sgplot data=RelRisks noautolegend;
   title3 'Comparison of ODDS ratio (and 95% confidence intervals) among passenger classes';
   where studytype='Case-Control (Odds Ratio)';
   scatter y=value x=pclass;
   highlow x=pclass low=lowercl high=uppercl;
   yaxis label="Odds-ratio of SURVIVAL (males:females)";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Passenger Class';
run;

ods pdf close;


	