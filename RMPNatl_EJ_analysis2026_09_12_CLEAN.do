*RMP Nationwide Panel EJ Analysis
*Created: 04/05/2024
*Created by: D. Guignet
*Last Revised: 09/12/2026
*Last Revised by: D. Guignet

********************************************************************************

*This do-file runs regression analyses for the RMP EJ study. It was updated on
*	8/24/2024 to use cluster robust standard errors, clustered at the state or 
*	county level. 

********************************************************************************

*Set additional folder directories

*folder with final dataset
*global datafolder "C:\Users\cookkl4\EJ_DataFolder"
global datafolder "G:\My Drive\Research\RMP_Hedonics\RMP_EJ_Nationwide\EJ_Panel_Analysis\data"
*global datafolder "G:\.shortcut-targets-by-id\1zjBuu4oWhCdpsxXMLg4MDM-GzhAWc44V\RMP_EJ_Nationwide\EJ_Panel_Analysis\data"

*folder for exporting results
*global resultsfolder "C:\Users\cookkl4\EJ_ResultsFolder"
global resultsfolder "G:\My Drive\Research\RMP_Hedonics\RMP_EJ_Nationwide\EJ_Panel_Analysis\analysis\results\results2026_09"
*global resultsfolder "G:\.shortcut-targets-by-id\1zjBuu4oWhCdpsxXMLg4MDM-GzhAWc44V\RMP_EJ_Nationwide\EJ_Panel_Analysis\analysis\results\results2026_03v2"


*******************************************************************************

*Perform some quick summary stats on census tract data
use "$datafolder\Final_Panel_5yrACS_RMPcnts.dta", clear
	
tab cntrmp5km drmp5km  
tab cntacc5km dacc5km 
tab cntoff5km doff5km
tab cntpostacc5km dpostacc5km
tab cntpostoff5km dpostoff5km
tab drmp5km dacc5km
tab dacc5km doff5km
tab dpostacc5km dpostoff5km

tab dacc5km dpostacc5km
tab doff5km dpostoff5km

corr pctwhite pctblack pctamind pctasian pcthisp

*generate state and county id variables to use for later clustering, and possibly FE. 
*county
gen stcofips=substr(fips, 1, 5)
egen stcoid=group(stcofips)
*state
gen stfips=substr(fips, 1, 2)
egen stid=group(stfips)

*Note: Drop DC from analysis. 
count
drop if real(stfips)==11
	*Note: Double checked and confirmed that we already dropped Puerto Rico, 
	*	Alaska, and Hawaii. 
count

*Clean sample a bit more by eliminating observations where a key dependent variable is missing
count if pctwhite==.
count if pctwhite==.|pctblack==.
count if pcthisp==.
count if pctwhite==.|pcthisp==.
count if pctpov==.
count
drop if pctwhite ==. | pctblack==. | pcthisp==.
count
count if pctpov==.
drop if pctpov==.
count
count if pctwhite ==. | pctblack==. | pcthisp==. | pctamind==. | pctasian==. | pctpov==.
	*Note: Final sample of 715,399 tracts. 


********************************************************************************

*Descriptive statistics
sum  pctwhite pctblack pctamind pctasian pcthisp pctpov ///
	cntrmp5km drmp5km cntacc5km dacc5km cntoff5km doff5km cntpostacc5km dpostacc5km cntpostoff5km dpostoff5km

	
********************************************************************************

*Sociodemographic characteristics as outcome variables

*Percent white regression models
*Model 1 - Bacic model w/ just year FE
reghdfe pctwhite drmp5km dacc5km doff5km, absorb(begin_year) vce(cluster stcofips)
eststo m1
nlcom (rmp_v_acc: _b[drmp5km]+_b[dacc5km]), post
eststo n1
*Model 2 - Add before versus after comparison
reghdfe pctwhite drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(begin_year) vce(cluster stcofips)
eststo m2
eststo n2: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 3 - Add state fixed effects
reghdfe pctwhite drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid begin_year) vce(cluster stcofips)
eststo m3
eststo n3: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 4 - Add state-by-year fixed effects
reghdfe pctwhite drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
eststo n4: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
esttab m1 m2 m3 m4 using "$resultsfolder\EJtractregs_PctWhite.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
esttab n1 n2 n3 n4 using "$resultsfolder\EJNonLinCom_PctWhite.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	
*Percent black regression models
*Model 1 - Bacic model w/ just year FE
reghdfe pctblack drmp5km dacc5km doff5km, absorb(begin_year) vce(cluster stcofips)
eststo m1
nlcom (rmp_v_acc: _b[drmp5km]+_b[dacc5km]), post
eststo n1
*Model 2 - Add before versus after comparison
reghdfe pctblack drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(begin_year) vce(cluster stcofips)
eststo m2
eststo n2: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 3 - Add state fixed effects
reghdfe pctblack drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid begin_year) vce(cluster stcofips)
eststo m3
eststo n3: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 4 - Add state-by-year fixed effects
reghdfe pctblack drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
eststo n4: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
esttab m1 m2 m3 m4 using "$resultsfolder\EJtractregs_PctBlack.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
esttab n1 n2 n3 n4 using "$resultsfolder\EJNonLinCom_PctBlack.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	
*Percent hispanic regression models
*Model 1 - Bacic model w/ just year FE
reghdfe pcthisp drmp5km dacc5km doff5km, absorb(begin_year) vce(cluster stcofips)
eststo m1
nlcom (rmp_v_acc: _b[drmp5km]+_b[dacc5km]), post
eststo n1
*Model 2 - Add before versus after comparison
reghdfe pcthisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(begin_year) vce(cluster stcofips)
eststo m2
eststo n2: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 3 - Add state fixed effects
reghdfe pcthisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid begin_year) vce(cluster stcofips)
eststo m3
eststo n3: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 4 - Add state-by-year fixed effects
reghdfe pcthisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
eststo n4: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]), post
esttab m1 m2 m3 m4 using "$resultsfolder\EJtractregs_PctHisp.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
esttab n1 n2 n3 n4 using "$resultsfolder\EJNonLinCom_PctHisp.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	
*Percent native regression models
*Model 1 - Bacic model w/ just year FE
reghdfe pctamind drmp5km dacc5km doff5km, absorb(begin_year) vce(cluster stcofips)
eststo m1
nlcom (rmp_v_acc: _b[drmp5km]+_b[dacc5km]), post
eststo n1
*Model 2 - Add before versus after comparison
reghdfe pctamind drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(begin_year) vce(cluster stcofips)
eststo m2
eststo n2: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 3 - Add state fixed effects
reghdfe pctamind drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid begin_year) vce(cluster stcofips)
eststo m3
eststo n3: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 4 - Add state-by-year fixed effects
reghdfe pctamind drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
eststo n4: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
esttab m1 m2 m3 m4 using "$resultsfolder\EJtractregs_PctAmInd.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
esttab n1 n2 n3 n4 using "$resultsfolder\EJNonLinCom_PctAmInd.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	
*Percent Asian regression models
*Model 1 - Bacic model w/ just year FE
reghdfe pctasian drmp5km dacc5km doff5km, absorb(begin_year) vce(cluster stcofips)
eststo m1
nlcom (rmp_v_acc: _b[drmp5km]+_b[dacc5km]), post
eststo n1
*Model 2 - Add before versus after comparison
reghdfe pctasian drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(begin_year) vce(cluster stcofips)
eststo m2
eststo n2: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 3 - Add state fixed effects
reghdfe pctasian drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid begin_year) vce(cluster stcofips)
eststo m3
eststo n3: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 4 - Add state-by-year fixed effects
reghdfe pctasian drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
eststo n4: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
esttab m1 m2 m3 m4 using "$resultsfolder\EJtractregs_PctAsian.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
esttab n1 n2 n3 n4 using "$resultsfolder\EJNonLinCom_PctAsian.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
	
	
*Percent poverty regression models
*Model 1 - Bacic model w/ just year FE
reghdfe pctpov drmp5km dacc5km doff5km, absorb(begin_year) vce(cluster stcofips)
eststo m1
nlcom (rmp_v_acc: _b[drmp5km]+_b[dacc5km]), post
eststo n1
*Model 2 - Add before versus after comparison
reghdfe pctpov drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(begin_year) vce(cluster stcofips)
eststo m2
eststo n2: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 3 - Add state fixed effects
reghdfe pctpov drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid begin_year) vce(cluster stcofips)
eststo m3
eststo n3: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
*Model 4 - Add state-by-year fixed effects
reghdfe pctpov drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
eststo n4: nlcom ///
	(rmp_acc: _b[drmp5km]+_b[dacc5km]) ///
	(acc_post: _b[dacc5km]+_b[dpostacc5km]+_b[drmp5km]), post
esttab m1 m2 m3 m4 using "$resultsfolder\EJtractregs_PctPov.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
esttab n1 n2 n3 n4 using "$resultsfolder\EJNonLinCom_PctPov.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	
	
*Total Population by subgroup
**Versions of model 4 (state x year fe)
*totpop
reghdfe totpop drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m1
*white
reghdfe white drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m2
*black
reghdfe black drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m3
*hispanic
reghdfe hisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
*asian
reghdfe asian drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m5
*native
reghdfe amind drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m6
*poverty
reghdfe povhh drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m7
esttab m1 m2 m3 m4 m5 m6 m7 using "$resultsfolder\EJregs_Model4_Populations.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
	

	
********************************************************************************
				*State Delegation Model Revamp
********************************************************************************
*White
reghdfe pctwhite drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 1, absorb(stid#begin_year) vce(cluster stcofips)
eststo m1
reghdfe pctwhite drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 0, absorb(stid#begin_year) vce(cluster stcofips)
eststo m2
*Black
reghdfe pctblack drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 1, absorb(stid#begin_year) vce(cluster stcofips)
eststo m3
reghdfe pctblack drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 0, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
*Hispanic
reghdfe pcthisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 1, absorb(stid#begin_year) vce(cluster stcofips)
eststo m5
reghdfe pcthisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 0, absorb(stid#begin_year) vce(cluster stcofips)
eststo m6
*Poverty
reghdfe pctpov drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 1, absorb(stid#begin_year) vce(cluster stcofips)
eststo m7
reghdfe pctpov drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 0, absorb(stid#begin_year) vce(cluster stcofips)
eststo m8
*Asian
reghdfe pctasian drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 1, absorb(stid#begin_year) vce(cluster stcofips)
eststo m9
reghdfe pctasian drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 0, absorb(stid#begin_year) vce(cluster stcofips)
eststo m10
*Native
reghdfe pctamind drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 1, absorb(stid#begin_year) vce(cluster stcofips)
eststo m11
reghdfe pctamind drmp5km dacc5km doff5km dpostacc5km dpostoff5km if StateDeleg == 0, absorb(stid#begin_year) vce(cluster stcofips)
eststo m12
esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 using "$resultsfolder\SubGroup_StDelg_Regs.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	

********************************************************************************
		*Sensitivity Analysis - Variant of Model 4 with tract Fixed Effects
********************************************************************************	

*Add tract FE as robustness check for post-accident variables (Reviewer 2 response)
*% White
reghdfe pctwhite drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year fips) vce(cluster stcofips)
eststo m1
	*Note: Checked this model in response to Reviewer 2 from AEPP submission 
	*	(9/12/2026). In the end do not include because it drops the time-invariant
	*	variables that we care about. As can be seen here and in below results,
	*	estimates become statistically insignificant. This could be partly 
	*	because only have 10 obervations per tract and that is too few to estimates
	*	association based on only within tract variation. It may also be that 
	*	the change in demographics we estimate reflects more how demographics 
	*	differ across sites that have accidents occur and where those accidents 
	*	occur sooner, although the time-invariant accident variables (dacc5km and 
	*	doff5km) absorb much of that. This issue is exacerbated given that the 
	*	ACS data our 5-year averages, thus dampening any within tract temporal 
	*	variation across the 10 years of ACS data. 
*% Black
reghdfe pctblack drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year fips) vce(cluster stcofips)
eststo m2
*% Hispanic
reghdfe pcthisp drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year fips) vce(cluster stcofips)
eststo m3
*% American India
reghdfe pctamind drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year fips) vce(cluster stcofips)
eststo m4
*% Asian
reghdfe pctasian drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year fips) vce(cluster stcofips)
eststo m5
*% Poverty
reghdfe pctpov drmp5km dacc5km doff5km dpostacc5km dpostoff5km, absorb(stid#begin_year fips) vce(cluster stcofips)
eststo m6

*export results
esttab m1 m2 m3 m4 m5 m6 using "$resultsfolder\TractFE_3km.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2	
	

********************************************************************************
		*Sensitivity Analysis - Alternative 1km and 3km distance Bins
********************************************************************************
	
		******Sensitivity Analysis******
		
**3km Sensitivity Analysis
use "$datafolder\Final_Panel_5yrACS_3kmRMPcnts.dta", clear

**generating stcofips and stid**
gen stcofips=substr(fips, 1, 5)
egen stcoid=group(stcofips)
*state
gen stfips=substr(fips, 1, 2)
egen stid=group(stfips)

*Note: Drop DC from analysis. 
drop if real(stfips)==11
	*Note: Double checked and confirmed that we already dropped Puerto Rico, 
	*	Alaska, and Hawaii. 
	
reghdfe pctwhite drmp3km dacc3km doff3km dpostacc3km dpostoff3km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m1
reghdfe pctblack drmp3km dacc3km doff3km dpostacc3km dpostoff3km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m2
reghdfe pcthisp drmp3km dacc3km doff3km dpostacc3km dpostoff3km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m3
reghdfe pctasian drmp3km dacc3km doff3km dpostacc3km dpostoff3km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
reghdfe pctamind drmp3km dacc3km doff3km dpostacc3km dpostoff3km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m5
reghdfe pctpov drmp3km dacc3km doff3km dpostacc3km dpostoff3km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m6
esttab m1 m2 m3 m4 m5 m6 using "$resultsfolder\3km_SensAnalysis.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2

	
**1km Sensitivity Analysis
use "$datafolder\Final_Panel_5yrACS_1kmRMPcnts.dta", clear
**generating stcofips and stid**
gen stcofips=substr(fips, 1, 5)
egen stcoid=group(stcofips)
*state
gen stfips=substr(fips, 1, 2)
egen stid=group(stfips)

*Note: Drop DC from analysis. 
drop if real(stfips)==11
	*Note: Double checked and confirmed that we already dropped Puerto Rico, 
	*	Alaska, and Hawaii. 
	
reghdfe pctwhite drmp1km dacc1km doff1km dpostacc1km dpostoff1km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m1
reghdfe pctblack drmp1km dacc1km doff1km dpostacc1km dpostoff1km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m2
reghdfe pcthisp drmp1km dacc1km doff1km dpostacc1km dpostoff1km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m3
reghdfe pctasian drmp1km dacc1km doff1km dpostacc1km dpostoff1km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m4
reghdfe pctamind drmp1km dacc1km doff1km dpostacc1km dpostoff1km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m5
reghdfe pctpov drmp1km dacc1km doff1km dpostacc1km dpostoff1km, absorb(stid#begin_year) vce(cluster stcofips)
eststo m6
esttab m1 m2 m3 m4 m5 m6 using "$resultsfolder\1km_SensAnalysis.csv", replace ///
	label csv compress nogaps nolines star (* 0.10 ** 0.05 *** 0.01) b(4) se(4) ar2
	

*END