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

	