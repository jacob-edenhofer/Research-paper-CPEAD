* Please use original dataset in Stata (.dta) or Excel format (.xlsx) file from Jelle Visser: http://uva-aias.net/en/ictwss

*Declare path where downloaded file is stored (e.g. D:/User/Downloads, or ~/Downloads, etc)
	global path K:/DATEIEN/Data/Andere Datensatze/Visser

*Import .xlsx file
	import excel "$path/20191107_ICTWSS_6_1_Dataset.xlsx", ///
		sheet("ICTWSS6.0") cellrange(A2:ID3249) firstrow clear

	save "$path/ICTWSS_v6_1_Stata_release.dta", replace

*Drop observations
	capture drop if country_code ==  32 & year < 1983
	capture drop if country_code ==  76 & year < 1992
	capture drop if country_code == 100 & year < 1992
	capture drop if country_code == 152 & year < 1991 
	capture drop if country_code == 156
	capture drop if country_code == 158 & year < 1996
	capture drop if country_code == 170
	capture drop if country_code == 188
	capture drop if country_code == 191 & year < 2000
	capture drop if country_code == 196 & year < 1990
	capture drop if country_code == 203 & year < 1991
	capture drop if country_code == 233 & year < 1993
	capture drop if country_code == 300 & year < 1975
	capture drop if country_code == 344
	capture drop if country_code == 348 & year < 1990
	capture drop if country_code == 356 & year < 1952
	capture drop if country_code == 360 & year < 2000
	capture drop if country_code == 410 & year < 1987
	capture drop if country_code == 428 & year < 1993
	capture drop if country_code == 440 & year < 1993
	capture drop if country_code == 458
	capture drop if country_code == 470 & year < 1990
	capture drop if country_code == 484 & year < 1988
	capture drop if country_code == 608 & year < 1988
	capture drop if country_code == 616 & year < 1990
	capture drop if country_code == 620 & year < 1976
	capture drop if country_code == 642 & year < 1993
	capture drop if country_code == 643
	capture drop if country_code == 703 & year < 1993
	capture drop if country_code == 705 & year < 1990
	capture drop if country_code == 710 & year < 1994
	capture drop if country_code == 724 & year < 1978
	capture drop if country_code == 792 & year < 1967

*Rename variables
	rename CENT Cent
	rename Sector Sector
	rename WC_STRUCT WC_struct 
	rename WC_RIGHTS WC_rights
	rename Govint Govint
	rename Level Level 
	rename RI RI
	rename Coord WCoord
	rename EXT Ext
	rename country_code iso
	
*Order variables
	order country iso iso2c year Cent Sector WC_struct WC_rights Govint Level RI WCoord Ext
	keep country - Ext

*Destring variables if .xlsx has been used
	capture destring Cent-Ext, replace
	
* recode because wrong coding in orginal data set (6 is not defined)
	recode Govint (6 = 3) if iso == 840

*Generate new extrapolated variables
	local varlist Cent Sector WC_struct WC_rights Govint Level RI WCoord Ext

	foreach var of varlist `varlist' {
		clonevar `var'_1 = `var'
		sort iso year
		by iso: ipolate `var'_1 year, gen(`var'_a) epolate
	}

*Make additional changes
	recode WCoord_a (5 = 1) if Govint_a == 5 | Govint_a == 3, gen (WCoord_b)
	clonevar WCoord_x = WCoord_b
	replace WCoord_b = WCoord if WCoord_b == .
	recode Govint_a (5 = 1) (3 = 1), gen (Govint_b)
	recode Govint_b (4 = 3) 
	recode WCoord_b (. = 2) if iso == 356 & year > 2014
	recode WCoord_b (. = 3) if iso == 710 & year > 2013
	recode Sector_a (0 = .) if iso == 484 
	recode RI_a (nonmiss = .) if iso == 484 | iso == 608
	recode RI_a (nonmiss = .) if iso == 440 & year == 1993
	recode WC_rights_a (nonmiss = .) if iso == 484 

*Save dataset
	save "$path/Corporatismus_work1.dta" , replace

*Generate core dataset
	keep if inlist(iso,36,40,56,124,196,203,208,246,250,276,300,348,372,380,392,410,528,554,578,616,620,703,705,724,752,756,826,840)

*Generate z-scores
	local varlist WCoord_b Govint_b Sector_a Cent_a Level_a Ext_a RI_a  WC_struct_a WC_rights_a
	
	foreach var of varlist `varlist' {
		quietly sum `var', meanonly
		quietly gen double z_`var' = `var'-`r(mean)'
		format %9.2f z_`var'
	}

*Form an additive index
	gen CorpCORE = (z_WCoord_b + z_Govint_b + z_Sector_a + z_Cent_a + z_Level_a + z_Ext_a + z_RI_a  + z_WC_struct_a + z_WC_rights_a) / 9

*Factor score index
	factor  WCoord_b Govint_b Sector_a Cent_a Level_a Ext_a RI_a  WC_struct_a WC_rights_a , factors(1)
	predict Corp_fCORE

*Generate running averages
	tsset iso year
	tssmooth ma CorpCOREsm = CorpCORE, window(4 1)
	tssmooth ma Corpo_fCOREsm = Corp_fCORE, window(4 1)

	keep country iso year CorpCORE Corp_fCORE CorpCOREsm Corpo_fCOREsm

*Save dataset
	save "$path/Corporatismus_core.dta" , replace
*-------------------------------------------------------------------------------
*EU+ dataset (all core countries plus remaining EU member states)
	use "$path/Corporatismus_work1.dta", clear

	keep if inlist(iso,36,40,56,124,196,203,208,246,250,276,300,348,372,380,392,410,528,554,578,616,620,703,705,724,752,756,826,840,100,191,233,428,440,442,470,642)

*Generate z-scores
	local varlist WCoord_b Govint_b Sector_a Cent_a Level_a Ext_a RI_a  WC_struct_a WC_rights_a
	
	foreach var of varlist `varlist' {
		quietly sum `var', meanonly
		quietly gen double z_`var' = `var'-`r(mean)'
		format %9.2f z_`var'
	}

*Form additive index
	gen CorpEUplus = (z_WCoord_b + z_Govint_b + z_Sector_a + z_Level_a + z_Ext_a + z_RI_a + z_WC_struct_a + z_WC_rights_a) / 8

*Factor score index
	factor  WCoord_b Govint_b Sector_a Level_a Ext_a RI_a  WC_struct_a WC_rights_a , factors(1)
	predict Corp_fEUplus

*Generate running averages
	tsset iso year
	tssmooth ma CorpEUplussm=CorpEUplus, window(4 1)
	tssmooth ma Corpo_fEUplussm=Corp_fEUplus, window(4 1)

	keep country iso year CorpEUplus Corp_fEUplus CorpEUplussm Corpo_fEUplussm

*Save dataset
	save "$path/CorporatismusEUplus.dta" , replace

*-------------------------------------------------------------------------------
*All countries (only additive index)

use "$path/Corporatismus_work1.dta", clear

	keep if inlist(iso,36,40,56,124,196,203,208,246,250,276,300,348,372,380,392,410,470,528,554,578,616,620,703,705,724,752,756,826,840,100,152,191,233,428,440,442,642,792,32,76,152,158,376,484,702,710)

*Generate z-scores
	local varlist WCoord_b Govint_b Sector_a Cent_a Level_a Ext_a RI_a  WC_struct_a WC_rights_a
	
	foreach var of varlist `varlist' {
		quietly sum `var', meanonly
		quietly gen double z_`var' = `var'-`r(mean)'
		format %9.2f z_`var'
	}

*Generate additive index

	gen CorpAll = .

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Sector_a + z_Cent_a + z_Level_a + z_Ext_a + z_RI_a + z_WC_struct_a + z_WC_rights_a) / 9 ///
		if inlist(iso,36,40,56,196,124,203,208,246,250,276,300,348,372,380,392,410,528,554,578,616,620,703,705,724,752,756,826,840)

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Sector_a + z_Level_a + z_Ext_a + z_RI_a  + z_WC_struct_a + z_WC_rights_a) / 8 ///
		if inlist(iso,100,191,233,428,440,442,470,642,152,792)

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Cent_a + z_Level_a + z_Ext_a + z_RI_a + z_WC_rights_a) / 7 ///
		if iso == 702

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Level_a + z_Ext_a + z_RI_a  + z_WC_struct_a + z_WC_rights_a) / 7 ///
		if iso == 76

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Sector_a + z_Level_a + z_Ext_a + z_WC_struct_a + z_WC_rights_a) / 7 ///
		if iso == 32

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Level_a + z_Ext_a + z_RI_a + z_WC_rights_a) / 6 ///
		if iso == 158

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Level_a + z_Ext_a + z_RI_a)   / 5 ///
		if iso == 376

	replace CorpAll = (z_WCoord_b + z_Govint_b + z_Level_a + z_Ext_a + z_WC_struct_a ) / 5 ///
		if iso == 484

	replace CorpAll = (z_WCoord_b + z_Sector_a + z_Level_a + z_Ext_a + z_RI_a  + z_WC_struct_a + z_WC_rights_a) / 7 ///
		if iso == 710

*Generate running averages
	tsset iso year
	tssmooth ma CorpAllsm=CorpAll, window(4 1)

	keep country iso year CorpAll CorpAllsm

*Save dataset
	save "$path/CorporatismusALL.dta" , replace

*Generate dataset that contains all corporatism indices
	use "$path/Corporatismus_core.dta"
		merge 1:1 iso year using "$path/CorporatismusEUplus.dta", nogenerate
		merge 1:1 iso year using "$path/CorporatismusALL.dta", nogenerate

*Make additional changes to display format
	format %3.0f iso
	format %4.0f year
	format %3.2f Corp*
	
*Save dataset
	save "$path/Corporatism2019.dta", replace
