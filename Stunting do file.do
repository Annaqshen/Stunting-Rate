

************************Economic Development Final Paper do file******************
******************** Paper authors: Deviana Dewi & Xiaoqiao Shen******************
******************************** Code author: Xiaoqiao Shen***********************
clear
cd "/Users/annashen/Desktop/Stata Practice/Stunting re-run"

******cleaning FAO stunting rate dataset 
import delimited "FAOSTAT_data_en_10-7-2022.csv", varnames(1) encoding(UTF-8)
drop if value ==.
drop domaincode domain areacodem49 elementcode element itemcode item yearcode unit flag flagdescription 
drop note
rename area country
rename value stuntingrate
destring year, replace
save "Stunting rate clean.dta",replace 

******merge with Agshare dataset
clear
import delimited "agshareGDP.csv", varnames(1) encoding(UTF-8) 
rename agingdp Agshare
drop if year==2021| year==2022
save "AgShare clean.dta",replace

merge 1:1 country year using "Stunting rate clean.dta", force
keep if _merge==3
drop _merge
save "Stunting and Agshare clean.dta",replace

******reshape the WB GDP per capital dataset
clear
import excel "GDPPC 2000-2020.xls", sheet("Data") firstrow clear
gen id=_n
local i = 2000
foreach v of var D-X{
	rename `v' GDPPC`i'
	local i = `i' + 1
}
reshape long GDPPC, i(id)j(year)
rename CountryName country
save "GDPpercapita.dta",replace

******merge the FAO stunting rate dataset with WB GDP per capita dataset
use "GDPpercapita.dta"
merge 1:1 country year using "Stunting and Agshare clean.dta", force
keep if _merge==3
drop _merge
save "3 main variables.dta",replace

encode wb_region, gen(regioncode)
encode country, gen(countrycode)

label variable stuntingrate "stuntingrate"
label variable Agshare "Agshare"
twoway (scatter stuntingrate Agshare)

twoway (scatter stuntingrate Agshare if region==1)(scatter stuntingrate Agshare if region==2)(scatter stuntingrate Agshare if region==3)(scatter stuntingrate Agshare if region==4)(scatter stuntingrate Agshare if region==5)(scatter stuntingrate Agshare if region==6)(scatter stuntingrate Agshare if region==7),legend(label(1 "East Asia and the Pacific") label(2 "Europe & Central Asia")label(3 "Latin America and Carribean")label(4 "Middle East and North Africa")label(5 "Europe & Central Asia")label(6 "South Asia")label(7 "North America"))

save "3 main variables.dta",replace

*********************************** main regressions **************************

*OLS
eststo clear
gen lGDPPC = log(GDPPC)
reg stuntingrate Agshare lGDPPC, r
eststo OLS
 
*run country FE without control 
xtset countrycode year
xtreg stuntingrate Agshare i.year, fe
eststo FE 

*run country FE with control 
xtreg stuntingrate Agshare lGDPPC i.year, fe
eststo FE_control

*export the results

esttab OLS FE FE_control using "coef table.rtf", replace drop(*.year _cons) mtitle(OLS FE FE_controlGFP)

save "3 main variables.dta",replace

********************subgroup analysis by continents groups*********************
use "3 main variables.dta",clear
xtset countrycode year

		preserve
		keep if regioncode == 6 //South Asia
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		xtset countrycode year
		xtreg stuntingrate Agshare i.year, fe
		eststo SouthAsia
		restore

		preserve
		keep if regioncode == 2 //Europe and Central Asia
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		xtset countrycode year
		xtreg stuntingrate Agshare i.year, fe
		eststo Europe
		restore

		preserve
		keep if regioncode == 4 //Middle East and North Africa
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		xtset countrycode year
		xtreg stuntingrate Agshare i.year, fe
		eststo MiddleEasat
		restore

		preserve
		keep if regioncode == 1 //East Asia and the Pacific
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		xtset countrycode year
		xtreg stuntingrate Agshare i.year, fe
		eststo EastAsia
		restore

		preserve
		keep if regioncode == 7 //Sub-Sahara Africa
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		xtset countrycode year
		xtreg stuntingrate Agshare i.year, fe
		eststo Africa
		restore

		preserve
		keep if regioncode == 3 //Latin America and Carribean
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		xtset countrycode year
		xtreg stuntingrate Agshare i.year, fe
		eststo LatAm
		restore

		preserve
		keep if regioncode == 5 //North America
		twoway (scatter stuntingrate Agshare)(lfit stuntingrate Agshare) 
		reg stuntingrate Agshare //only one country has observations
		eststo NorthAmerica
		restore

esttab SouthAsia Europe MiddleEasat EastAsia Africa LatAm NorthAmerica using "coef table2.rtf", replace drop(*.year _cons) mtitle(SouthAsia Europe MiddleEasat EastAsia Africa LatAm NorthAmerica)

*analysis on Cambodia China India Vietnam
xtline stuntingrate Agshare if countrycode == 25,title("Cambodia") name(Cambodia, replace)
xtline stuntingrate Agshare if countrycode == 30,title("China") name(China, replace)
xtline stuntingrate Agshare if countrycode == 60,title("India") name(India, replace)
xtline stuntingrate Agshare if countrycode == 144,title("Vietnam") name(Vietnam, replace)
graph combine Cambodia China India Vietnam
graph save EastAsia, replace

*analysis on Middle East
twoway (scatter stuntingrate Agshare if regioncode==4)(scatter stuntingrate Agshare if region==5),legend(label(1 "Middle East and North Africa")label(2 "North America"))
tab country if regioncode == 4
xtline stuntingrate Agshare if countrycode == 126,title("Syria") name(Syria, replace)
xtline stuntingrate Agshare if countrycode == 63,title("Iraq") name(Iraq, replace) 
xtline stuntingrate Agshare if countrycode == 62,title("Iran") name(Iran, replace) 
xtline stuntingrate Agshare if countrycode == 75,title("Libya") name(Libya, replace) 
graph combine Syria Libya Iran
graph save MiddleEast, replace

*analysis on Latin America
twoway (scatter stuntingrate Agshare if regioncode==3), title("LatAm and Carribean")
tab country if regioncode == 3
xtline stuntingrate Agshare if countrycode == 19,title("Brazil") name(Brazil, replace) 
xtline stuntingrate Agshare if countrycode == 5,title("Argentina") name(Argentina, replace) 
xtline stuntingrate Agshare if countrycode == 84,title("Mexico") name(Mexico, replace) 
xtline stuntingrate Agshare if countrycode == 143,title("Venezuela") name(Venezuela, replace)
graph combine Brazil Argentina Mexico Venezuela
graph save LatinAmerica, replace

*analysis on Sub-Sahara Africa
tab country if regioncode == 7
xtline stuntingrate Agshare if countrycode == 47,title("Ethiopia") name(Ethiopia, replace)
xtline stuntingrate Agshare if countrycode == 38,title("Congo") name(Congo, replace)
xtline stuntingrate Agshare if countrycode == 96,title("Nigeria") name(Nigeria, replace)
xtline stuntingrate Agshare if countrycode == 68,title("Kenya") name(Kenya, replace)
graph combine Ethiopia Kenya Congo Nigeria 
graph save Africa, replace










