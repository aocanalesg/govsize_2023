/*
Fecha: Julio 2023
Título: Tamaño Optimo de Gobierno de Nicaragua
Autores: Carl Friedrich Gauss & Isaac Newton
*/

//-------------------------------------------
//START
//-----------------------------------------

// Set Stata Version
version 17

//Initialize environment
	set more off
	cls
	clear all
	pause on 
	
//Set limits
set maxvar 30000
set matsize 11000
set more off

//Set global directories
	global directory = "..\dofiles"
	
//Change directory
	cd "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\"
	
//Import data from Google Sheets
import excel "RAW_DATA_1.xlsx", sheet("RAW_DATA") firstrow clear cellrange(A1:H65)

//Rename variables
	rename DATE date
	rename RAW_GDP gdp
	rename RAW_GOV_CON gov_con
	rename RAW_PUB_INV pub_inv
	rename RAW_PRIV_INV priv_inv
	rename RAW_X x
	rename RAW_M m
	rename RAW_POP pop

	
//Label variables
	label variable gdp "Quarterly GDP from 2006Q1 to 2021Q4, series in millions of source: BCN CNT data"
	label variable gov_con ""
	label variable pub_inv ""
	label variable priv_inv ""
	label variable x ""
	label variable m ""
	label variable pop ""
	
//set time series format
replace date = qofd(date)
format date %tq
tsset date

//rescale variables from BCN to millions of cords
foreach i in gdp gov_con pub_inv priv_inv x m {
replace `i'=`i'*10^6
}

//create dummies 

gen d_2008 = 0
replace d_2008 = 1 if date >= tq(2008q3) & date <= tq(2009q1)
label variable d_2008 "Dummy variable that captures international financial crisis"

gen d_2018 = 0
replace d_2018 = 1 if date > tq(2018q1) 
label variable d_2018 ""

//create gdp_pc and variables adjusted by gdp
gen gdp_pc = gdp/pop
label variable gdp_pc "GDP per capita constructed with GDP and POP series"

gen gov_gdp = (gov_con +pub_inv)/gdp 
label variable gov_gdp "Government size defined as the sum of government consumption plus public fixed investment to gdp"

gen tr_op = (x+m)/gdp 
label variable tr_op "Trade openness index constructed as the sum of exports and imports as a share of gdp"

foreach i in gov_con pub_inv priv_inv {
gen `i'_gdp =`i'/gdp
}

// Export variables to perform Variables Seasonal Adjustment in R

export excel gdp_pc gov_gdp gov_con_gdp pub_inv_gdp priv_inv_gdp tr_op  using pre_seasonal_adj, firstrow(variables) replace

	