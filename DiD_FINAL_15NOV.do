clear all
use  "/Volumes/GoogleDrive/Mi unidad/Rohingya Paper/Data and dofiles/DistrictPriceInfrastIDv8"

*preserve 

set more off
set scheme s1color

*****************************
* Previous code
*****************************
	*Rohingyas

	**Nominal indexed prices.
	*** Smoothing of noise
	***Fish is out becasue of lots of issues with the fish market
	

	**** Chandanaish is closest in terms of matching on infrastructure
	**** Main difference with v20 is that we have all the placebos together
	*** Post and Treatment Dummy has been replaced.f

	*This is to takeout Upazilla and other infra variables that we dont want to normalise
	quietly describe, varlist
	local vars `r(varlist)'  
	unab omit:  Upazilla MonthYear
	local want : list vars - omit

	local i=1
	foreach var of varlist `want' { 					
		replace `var'=ln(`var')
		rename `var' price`i'
		local i = `i' + 1			
	}
	
	****** RN
	sort Upazilla MonthYear
	expand 2 if Upazilla=="Teknaf ", gen(dup)
	replace Upazilla="Tek+Ukh" if dup==1
	
	foreach f of numlist 1/56 {
		gen aux=price`f'*0.8 if Upazilla=="Ukhia"
		replace aux=price`f'*0.2 if Upazilla=="Teknaf "
		bys MonthYear: egen price`f'_b=sum(aux) if Upazilla=="Ukhia" | Upazilla=="Teknaf " | Upazilla=="Tek+Ukh"
		replace price`f'=price`f'_b if  Upazilla=="Tek+Ukh"
		drop price`f'_b
		drop aux
	}
	drop dup
	******
	egen id2= group(Upazilla MonthYear)
	sort id2, stable
	
	reshape long price, i(id2) j(fooditem)
	drop id2 

	egen id3 = group (fooditem Upazilla)

	gen monthid=month(MonthYear)
	gen yearid=year(MonthYear)
	gen time = ym(yearid,monthid)

	xtset id3 time, monthly

	tssmooth ma pricels1=price, window(0 1 0) // RN> Why doing this?

	gen pricels=pricels1

	*** 1 cereal, 2 pulses, 3 Fish, 4 Beef Mutton Chicken, 5 Egg, 6 Oil, 7 Spices
	*** 8 Veg, 9 Oth
	recode fooditem (1/8=1) (9/14=2) (15/20=3) (21/25=4) (26/28=5) (19/32=6) (33/43=7) ///
	(44/53=8) (34/56=9), gen(fgroupid)

	*** get the protein together
	replace fgroupid =3 if fgroupid==4
	replace fgroupid=3 if fgroupid==5

	**** Lowquality Rice kept out
	gen fooditem1=1 if fooditem>0
	replace fooditem1 = 0 if fooditem==4 // RN> 384 Obs deleted
	replace fooditem1=0 if fooditem==5 // RN> 384 Bos deleted

	**** drop food items not in the common trends graph

	drop if fooditem==3   // najirshailC
	drop if fooditem==4   // drop atopC *** Atop is now part of food aid analysis
	drop if fooditem==15  //hilshaF
	drop if fooditem==16   // shrimpF
	drop if fooditem==17   // barbelF
	drop if fooditem==18  // rupchandaF
	drop if fooditem==19  // ruiF



	egen groupid =group(Upazilla)
	gen regionid=0
	replace regionid =1 if groupid==4|groupid==6|groupid==9|groupid==12|groupid==13|groupid==15|groupid==16 // Region ID =1 is for Cox's Bazar and 0 is for Chittagong
	egen period =group(MonthYear)
	

	*Creating Region and Time  Dummy Variable
	gen RD =0 //Treatment region dime
	replace RD=1 if groupid==15|groupid==16  // 15 is teknaf and 16 is Ukhia

	*Time Dummy and the interaction term
	forval i=1(1)24{
	gen TD`i'=0
	gen TDA`i'=0
	gen TDR`i'=0
	replace TD`i'=1 if period>=`i'
	replace TDA`i'=1 if period==`i' //only period i is counted
	replace TDR`i'=1 if period==`i'| period==`i'+1
	gen IRTD`i'=RD*TD`i'
	gen IRTDA`i'=RD*TDA`i'
	gen IRTDR`i'=RD*TDR`i'
	}

	* Group Dummy
	forval i=1(1)16{
	gen GD`i'=0
	replace GD`i'=1 if groupid==`i'
	gen TGD`i'= period*GD`i'
	}
	
*********************************
*****    New code
*********************************

*** Treatmement and control regions
label def treat 1 "Treatment" 0 "Control" 

* BALANCED 
gen T0_u=(Upazilla=="Ukhia") if Upazilla=="Ukhia" | ///
		 Upazilla=="Boalkhali" |  Upazilla=="Mirsharai" |  Upazilla=="Chandanaish" | Upazilla=="Pekua"

gen T0_t=(Upazilla=="Teknaf ") if Upazilla=="Teknaf " | ///
		 Upazilla=="Boalkhali" |  Upazilla=="Mirsharai" |  Upazilla=="Chandanaish" | Upazilla=="Pekua"
		
gen T0_both=(Upazilla=="Teknaf " | Upazilla=="Ukhia") if Upazilla=="Teknaf " | Upazilla=="Ukhia" | ///
		 Upazilla=="Boalkhali" |  Upazilla=="Mirsharai" |  Upazilla=="Chandanaish" | Upazilla=="Pekua"

* EXTENDED INLCUDING CENSUS
gen T1_u=(Upazilla=="Ukhia") if Upazilla=="Ukhia" | ///
		 Upazilla=="Boalkhali" |  Upazilla=="Chandanaish" |  Upazilla=="Ramu" |  Upazilla=="Hathazari" | Upazilla=="Pekua"

gen T1_t=(Upazilla=="Teknaf ") if Upazilla=="Teknaf " | ///
		Upazilla=="Chandanaish" |  Upazilla=="Boalkhali" | Upazilla=="Ramu" | Upazilla=="Mirsharai" 
		
gen T1_both=(Upazilla=="Teknaf " | Upazilla=="Ukhia ") if Upazilla=="Teknaf " | Upazilla=="Ukhia " | ///
		 Upazilla=="Boalkhali" |  Upazilla=="Chandanaish" |  Upazilla=="Ramu" |  Upazilla=="Hathazari" | Upazilla=="Pekua" | ///
		Upazilla=="Chandanaish" |  Upazilla=="Boalkhali" | Upazilla=="Ramu" | Upazilla=="Mirsharai" 

* Spill-over free control region
gen T2_u=(Upazilla=="Ukhia") if Upazilla=="Ukhia" | ///
		 Upazilla=="Boalkhali" |  Upazilla=="Mirsharai" |  Upazilla=="Chandanaish" 

gen T2_t=T1_t
gen T2_both=T1_both

foreach i of numlist 0 { //  1 2 {
	foreach r in u t both {
		lab val T`i'_`r' treat
	}
}

* All regions as controls
drop if Upazilla=="Tek+Ukh" 
gen T3_u=(Upazilla=="Ukhia") if Upazilla!="Teknaf "
gen T3_t=(Upazilla=="Teknaf ") if Upazilla!="Ukhia"

* All regions as controls except for Ramu and Cox's Bazar Sadar
gen T4_u=T3_u
replace T4_u=. if Upazilla=="Cox's Bazar Sadar" | Upazilla=="Ramu"
gen T4_t=T3_t
replace T4_t=. if Upazilla=="Cox's Bazar Sadar" | Upazilla=="Ramu"

** Visual Inspection of Parallel Trends
preserve 
	*drop if fooditem==5 // drop low quality rice
	foreach i of numlist 0 {
		foreach r in u  { // t both {
			* filtered
			bys T`i'_`r' time fgroup: egen price_fg_T`i'_`r'=mean(price) if T`i'_`r'!=.
			tssmooth ma f_price_fg_T`i'_`r' = price_fg_T`i'_`r', window(3 1) 
			sum f_price_fg_T`i'_`r' if T`i'_`r'!=. & time==ym(2016,12)
			gen a=r(mean)
			gen ind_f_price_fg_T`i'_`r'=f_price_fg_T`i'_`r'/a if T`i'_`r'!=.
			drop a
			sum price_fg_T`i'_`r' if T`i'_`r'!=. & time==ym(2016,12)
			gen a=r(mean)
			gen ind_price_fg_T`i'_`r'=price_fg_T`i'_`r'/a if T`i'_`r'!=.
			drop a
			
			bys T`i'_`r' time fgroup: gen tag_fg_T`i'_`r'=_n if T`i'_`r'!=.
			bys T`i'_`r' time : egen price_all_T`i'_`r'=mean(price) if T`i'_`r'!=.
			tssmooth ma f_price_all_T`i'_`r' = price_all_T`i'_`r', window(3 1) 
			sum f_price_all_T`i'_`r' if T`i'_`r'!=. & time==ym(2016,12)
			gen a=r(mean)
			gen ind_f_price_all_T`i'_`r'=f_price_all_T`i'_`r'/a if T`i'_`r'!=.
			drop a
			sum price_all_T`i'_`r' if T`i'_`r'!=. & time==ym(2016,12)
			gen a=r(mean)
			gen ind_price_all_T`i'_`r'=price_all_T`i'_`r'/a if T`i'_`r'!=.
			drop a
		}
	}

	** IN LEVELS 
	* Filtered
	foreach i of numlist 0 { // 1 2
		foreach r in u { // t both {
			sum f_price_all_T`i'_`r' if T`i'_`r'!=., det
			gen upper=r(max)
			local barcall upper period if inrange(period, 14, 22), bcolor(gs14)
			twoway (area `barcall')  ///
			       (line f_price_all_T`i'_`r' period if T`i'_`r'==1, lcolor(black)) ///
				   (line f_price_all_T`i'_`r' period if T`i'_`r'==0, lpattern(dash) lcolor(black)) ///
						if period<=22 & period>=6, name(all_`i'_`r', replace) nodraw ///
						legend(row(1) order(1 "Post-Treatment Period" 2 "Treatment Subdistrict" 3 "Control Subdistricts") symxsize(*0.5)) ytitle(" ") xtitle("Month-year") ylabel(, angle(h) labsize(small)) ///
						xlabel(6 "Dec16" 10 "Apr17" 14 "Aug17" 18 "Dec17" 22 "Apr18", labsize(small)) subtitle("Food Price Index")						
			drop upper
		}
	}
	
							*tlabel(2016m12(4)2018m04, labsize(small)) subtitle("Food Price Index") ///


	foreach i of numlist 0 { // 1 2 {
		foreach f of numlist 1 2 3 6 7 8 9 {
			local tit=cond(`f'==1,"Cereals",cond(`f'==3,"Proteins",cond(`f'==8,"Vegetables",cond(`f'==2,"Pulses",cond(`f'==6,"Oils",cond(`f'==7,"Spices",cond(`f'==9,"Others"," ")))))))
			foreach r in u { // t both  {
				sum f_price_fg_T`i'_`r' if T`i'_`r'!=. & fgroup==`f', det
				gen upper=r(max)
				local barcall upper period if inrange(period, 14, 22), bcolor(gs14)
				twoway (area `barcall')  ///
						(line f_price_fg_T`i'_`r' period if T`i'_`r'==1 & fgroup==`f', lcolor(black)) ///
						(line f_price_fg_T`i'_`r' period if T`i'_`r'==0 & fgroup==`f', lpattern(dash) lcolor(black)) ///
						if period<=22 & period>=6, name(fg`f'_`i'_`r', replace) nodraw ///
						legend(row(1) order(1 "Post-Treatment Period" 2 "Treatment Subdistrict" 3 "Control Subdistricts") symxsize(*0.5)) ytitle(" ") xtitle("Month-year") ylabel(, angle(h) labsize(small)) ///
						xlabel(6 "Dec16" 10 "Apr17" 14 "Aug17" 18 "Dec17" 22 "Apr18", labsize(small)) subtitle(`tit') // tline(2017m08)
				drop upper
			}
		}
	}
	
	foreach i of numlist 0 { // 1 2 {
		grc1leg fg1_`i'_u fg3_`i'_u fg8_`i'_u all_`i'_u, name(trends_a_u_`i', replace) // title("Treatment Subdistrict: Ukhia")
		*grc1leg fg1_`i'_t fg3_`i'_t fg8_`i'_t all_`i'_t, name(trends_a_t_`i', replace) // title("Treatment Subdistrict: Teknaf")
		*grc1leg fg1_`i'_both fg3_`i'_both fg8_`i'_both all_`i'_both, name(trends_a_both_`i', replace) // title("Treatment Subdistrict: Ukhia & Teknaf")
	}
	
	foreach i of numlist 0 { // 1 2 {
		grc1leg fg2_`i'_u fg6_`i'_u fg7_`i'_u fg9_`i'_u, name(o_trends_a_u_`i', replace)  // title("Treatment Subdistrict: Ukhia")
		*grc1leg fg2_`i'_t fg6_`i'_t fg7_`i'_t fg9_`i'_t, name(o_trends_a_t_`i', replace) // title("Treatment Subdistrict: Teknaf")
		*grc1leg fg2_`i'_both fg6_`i'_both fg7_`i'_both fg9_`i'_both, name(o_trends_a_both_`i'replace) // title("Treatment Subdistrict: Ukhia & Teknaf")
	}
	
	* Not filtered
	foreach i of numlist 0 { // 1 2
		foreach r in u { // t both {
			sum price_all_T`i'_`r' if T`i'_`r'!=., det
			gen upper=r(max)
			local barcall upper time if inrange(time, ym(2017,8), ym(2018,4)), bcolor(gs14)
			twoway (area `barcall')  ///
			       (line price_all_T`i'_`r' time if T`i'_`r'==1, lcolor(black)) ///
				   (line price_all_T`i'_`r' time if T`i'_`r'==0, lpattern(dash) lcolor(black)) ///
						if time<=ym(2018,4) & time>=ym(2016,12), name(all_`i'_`r'_nf, replace) nodraw ///
						legend(row(1) order(1 "Post-Treatment Period" 2 "Treatment Subdistrict" 3 "Control Subdistricts") symxsize(*0.5)) ytitle("Price level (smooth MA(3))") ylabel(, angle(h) labsize(small)) ///
						xlabel(#8, labsize(vsmall)) title("Food Price Index") // tline(2017m08)
			drop upper
		}
	}
	
	foreach i of numlist 0 { // 1 2 {
		foreach f of numlist 1 2 3 6 7 8 9 {
			local tit=cond(`f'==1,"Cereals",cond(`f'==3,"Proteins",cond(`f'==8,"Vegetables",cond(`f'==2,"Pulses",cond(`f'==6,"Oils",cond(`f'==7,"Spices",cond(`f'==9,"Others"," ")))))))
			foreach r in u { // t both  {
				sum price_fg_T`i'_`r' if T`i'_`r'!=. & fgroup==`f', det
				gen upper=r(max)
				local barcall upper time if inrange(time, ym(2017,8), ym(2018,4)), bcolor(gs14)
				twoway (area `barcall')  ///
						(line price_fg_T`i'_`r' time if T`i'_`r'==1 & fgroup==`f', lcolor(black)) ///
						(line price_fg_T`i'_`r' time if T`i'_`r'==0 & fgroup==`f', lpattern(dash) lcolor(black)) ///
						if time<=ym(2018,4) & time>=ym(2016,12), name(fg`f'_`i'_`r'_nf, replace) nodraw ///
						legend(row(1) order(1 "Post-Treatment Period" 2 "Treatment Subdistrict" 3 "Control Subdistricts") symxsize(*0.5)) ytitle("Price level (smooth MA(2))") ylabel(, angle(h) labsize(small)) ///
						xlabel(#8, labsize(vsmall)) title(`tit') // tline(2017m08)
				drop upper
			}
		}
	}
	
	foreach i of numlist 0 { // 1 2 {
		grc1leg fg1_`i'_u_nf fg3_`i'_u_nf fg8_`i'_u_nf all_`i'_u_nf, name(trends_a_u_`i'_nf, replace) // title("Treatment Subdistrict: Ukhia")
		*grc1leg fg1_`i'_t_nf fg3_`i'_t_nf fg8_`i'_t_nf all_`i'_t_nf, name(trends_a_t_`i'_nf, replace) // title("Treatment Subdistrict: Teknaf")
		*grc1leg fg1_`i'_both_nf fg3_`i'_both_nf fg8_`i'_both_nf all_`i'_both_nf, name(trends_a_both_`i'_nf, replace) // title("Treatment Subdistrict: Ukhia & Teknaf")
	}
	
	foreach i of numlist 0 { // 1 2 {
		grc1leg fg2_`i'_u_nf fg6_`i'_u_nf fg7_`i'_u_nf fg9_`i'_u_nf, name(o_trends_a_u_`i'_nf, replace) // title("Treatment Subdistrict: Ukhia")
		*grc1leg fg2_`i'_t_nf fg6_`i'_t_nf fg7_`i'_t_nf fg9_`i'_t_nf, name(o_trends_a_t_`i'_nf, replace) // title("Treatment Subdistrict: Teknaf")
		*grc1leg fg2_`i'_both_nf fg6_`i'_both_nf fg7_`i'_both_nf fg9_`i'_both_nf, name(o_trends_a_both_`i'_nf, replace) // title("Treatment Subdistrict: Ukhia & Teknaf")
	}
restore 

** "Tests" for Parallel trends.. EVENT STUDY
tab period, gen(time) // Relevant is between 6 and 22. Treatment takes place in 14, so 13 is the last pre-treatment period
gen TP=(period>=14) // Treatment period is August 2017
gen restr_time=(period>=6 & period<=22) // Restricted time span (PreT Dec2016)
gen ATP=(period>=14) if ((period>=2 & period<=4) | (period>=14 & period<=16)) // Treatment period starts from AUG/SEP/OCT 2016 and 2017
gen norice=(fooditem!=4 & fooditem!=5) // 1=Without rice
gen placebo=(period==11 | period==12 | period==13 ) // May, June and July 2017

* Normally we prefer to use 6 to 22
** THIS MAKES A GRAPH POSITING LAG-2 AS REFERENCE
estimates clear
foreach i of numlist 0 { // 1 2 {
	foreach r in u { // t both {
		local vars_pre ""
		foreach t of numlist 6/10 11 13 {
			local vars_pre="`vars_pre' 1.T`i'_`r'#1.time`t'"
		}
		local vars_post ""
		foreach t of numlist 14/22 {
			local vars_post="`vars_post' 1.T`i'_`r'#1.time`t'"
		}

		eststo all_`i'_`r': xtreg price i.period `vars_pre' `vars_post' if restr_time==1, fe vce(cluster groupid)
		mat r= r(table)
		*mat list r
		mat r1=r[1..9,18..23]
		mat r2=J(9,1,0)
		mat r3=r[1..9,24..33]
		mat r=(r1,r2,r3)
		*mat list r
		coefplot matrix(r), se(2) baselevels vertical yline(0, lcolor(black) lpattern(dot)) ci(95) xlabel(,angle(45) labsize(vsmall)) mcolor(black) ciop(recast(rcap) lcolor(black)) ///
				name(f_all_`i'_`r', replace) title("Food Price Index") nolabels label("Estimated Coefficient") xline(9, lcolor(black)) ///
				coeflabel(1.T`i'_`r'#1.time6=Dec16 1.T`i'_`r'#1.time7=Jan17 1.T`i'_`r'#1.time8=Feb17 1.T`i'_`r'#1.time9=Mar17 1.T`i'_`r'#1.time10=Apr17 1.T`i'_`r'#1.time11=May17 ///
						  c1=Jun17 1.T`i'_`r'#1.time13=Jul17 1.T`i'_`r'#1.time14=Aug17 1.T`i'_`r'#1.time15=Sep17 1.T`i'_`r'#1.time16=Oct17 1.T`i'_`r'#1.time17=Nov17 ///
						  1.T`i'_`r'#1.time18=Dec17 1.T`i'_`r'#1.time19=Jan18 1.T`i'_`r'#1.time20=Feb18 1.T`i'_`r'#1.time21=Mar18 1.T`i'_`r'#1.time22=Apr18) 
		

		/*
		coefplot all_`i'_`r', keep(`vars_pre' `vars_post') vertical yline(0, lcolor(black) lpattern(dot)) ci(99) xlabel(,angle(45) labsize(vsmall)) mcolor(black) ciop(recast(rcap) lcolor(black)) ///
				name(f_all_`i'_`r', replace) nodraw title("Food Price Index") nolabels label("Estimated Coefficient") xline(9, lcolor(black)) ///
				coeflabel(1.T`i'_`r'#1.time6=2016m12 1.T`i'_`r'#1.time7=2017m1 1.T`i'_`r'#1.time8=2017m2 1.T`i'_`r'#1.time9=2017m3 1.T`i'_`r'#1.time10=2017m4 1.T`i'_`r'#1.time11=2017m5 ///
						  1.T`i'_`r'#1.time12=2016m6 1.T`i'_`r'#1.time13=2017m7 1.T`i'_`r'#1.time14=2017m8 1.T`i'_`r'#1.time15=2017m9 1.T`i'_`r'#1.time16=2017m10 1.T`i'_`r'#1.time17=2017m11 ///
						  1.T`i'_`r'#1.time18=2017m12 1.T`i'_`r'#1.time19=2018m1 1.T`i'_`r'#1.time20=2018m2 1.T`i'_`r'#1.time21=2018m3 1.T`i'_`r'#1.time22=2018m4) 
		
		*/
		foreach f of numlist 1 3 8 {
			local tit=cond(`f'==1,"Cereals",cond(`f'==3,"Proteins",cond(`f'==8,"Vegetables"," ")))
			eststo f_`f'_`i'_`r': xtreg price i.period `vars_pre' `vars_post' if fgroup==`f' & restr_time==1, fe vce(cluster groupid)
			mat r= r(table)
			*mat list r
			mat r1=r[1..9,18..23]
			mat r2=J(9,1,0)
			mat r3=r[1..9,24..33]
			mat r=(r1,r2,r3)
			*mat list r
			coefplot matrix(r), se(2) baselevels vertical yline(0, lcolor(black) lpattern(dot)) ci(99) xlabel(,angle(45) labsize(vsmall)) mcolor(black) ciop(recast(rcap) lcolor(black)) ///
				xline(9, lcolor(black)) name(f_`f'_`i'_`r', replace) nodraw title(`tit') nolabels label("Estimated Coefficient") ///
				coeflabel(1.T`i'_`r'#1.time6=Dec16 1.T`i'_`r'#1.time7=Jan17 1.T`i'_`r'#1.time8=Feb17 1.T`i'_`r'#1.time9=Mar17 1.T`i'_`r'#1.time10=Apr17 1.T`i'_`r'#1.time11=May17 ///
						  c1=Jun17 1.T`i'_`r'#1.time13=Jul17 1.T`i'_`r'#1.time14=Aug17 1.T`i'_`r'#1.time15=Sep17 1.T`i'_`r'#1.time16=Oct17 1.T`i'_`r'#1.time17=Nov17 ///
						  1.T`i'_`r'#1.time18=Dec17 1.T`i'_`r'#1.time19=Jan18 1.T`i'_`r'#1.time20=Feb18 1.T`i'_`r'#1.time21=Mar18 1.T`i'_`r'#1.time22=Apr18) 
			

		}
	}
}

foreach i of numlist 0 { //1 2 {
	grc1leg f_1_`i'_u f_3_`i'_u f_8_`i'_u f_all_`i'_u, name(trends_u_`i', replace) // title("Parallel trends: Treated=Ukhia")
	*grc1leg f_all_`i'_u f_1_`i'_u, name(trends_u_`i', replace) // title("Parallel trends: Treated=Ukhia")
	*grc1leg f_1_`i'_t f_3_`i'_t f_8_`i'_t f_all_`i'_t, name(trends_t_`i', replace) // title("Parallel trends: Treated=Teknaf")
	*grc1leg f_1_`i'_both f_3_`i'_both f_8_`i'_both f_all_`i'_both, name(trends_b_`i', replace) // title("Parallel trends: Treated=Ukhia & Teknaf")
}
* Tables
local drop_c=""
foreach i of numlist 6/10 11 13 14/22 { // Change reference period according to estimations above
	*local drop_c="`drop_c' `i'.period"
	local drop_c="`drop_c'"
}
esttab all_0_u f_1_0_u f_3_0_u f_8_0_u /*using evstud.tex*/, replace nose not star drop(`drop_c') b(%9.4f) compress label r2
e

** Regresssions

mat A=J(1,14,.)
local seed 1

// With rice, Time Span bounded both sides
// 0 Would be the preferred specification and the rest would be testing for alternative definition of control regions
foreach i of numlist 0 1 2 {
	local or=cond(`i'==0,1,cond(`i'==1,5,cond(`i'==2,6,0)))
	local j=0
	foreach r in u {
		local bboth=cond("`r'"=="both","r","cluster groupid")
		local j=`j'+1
		qui: eststo e_T`i'_`r'_0_`or': xtreg price 1.T`i'_`r'#1.TP i.period if restr_time==1, fe vce(`bboth') 
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,0,1,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',`or') // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: eststo e_T`i'_`r'_`f'_`or': xtreg price 1.T`i'_`r'#1.TP i.period if fgroup==`f' & restr_time==1, fe vce(cluster groupid)
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,0,1,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',`or') //AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

// Without Rice, bounded both sides
foreach i of numlist 0 {
	local j=0
	foreach r in u {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.TP i.period if restr_time==1 & norice==1, fe vce(cluster groupid `bboth') 
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,0,0,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',9) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.TP i.period if fgroup==`f' & restr_time==1 & norice==1, fe vce(cluster groupid `bboth')
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,0,0,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',9) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

// With Rice, ALTERNATIVE TREATMENT PERIOD. THIS INCLUDES CORRECTIONS FOR SEASONAlITY AROUND AUGUST
foreach i of numlist 0 {
	local j=0
	foreach r in u {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.ATP i.period i.monthid, fe r
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.ATP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,1,1,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',7) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.ATP i.period i.monthid if fgroup==`f', fe vce(cluster groupid `bboth')
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.ATP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,1,1,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',7) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

// With Rice, ALTERNATIVE TREATMENT PERIOD. THIS INCLUDES ALL INFO IN PRE AND POST TREATMENT PERIODS
foreach i of numlist 0 {
	local j=0
	foreach r in u {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.TP i.period i.monthid, fe r
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,2,1,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',8) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.TP i.period i.monthid if fgroup==`f' & period<=22, fe vce(cluster groupid `bboth')
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,2,1,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',8) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

// With Rice, subdistrict specific trends
foreach i of numlist 0 {
	local j=0
	foreach r in u {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.TP i.period i.T`i'_`r'#i.period if restr_time==1, fe vce(cluster groupid `bboth')
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma) 
		mat CI=r(CI)
		mat B=(1,0,1,0,`j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',2) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.TP i.period i.T`i'_`r'#i.period  if fgroup==`f' & restr_time==1, fe vce(cluster groupid `bboth')
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(1,0,1,`f',`j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',2) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

// Baseline and all subdistricts as controls
foreach i of numlist 3 {
	local j=0
	foreach r in u { // t both {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.TP i.period if restr_time==1, fe r 
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,0,0,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',5) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.TP i.period if fgroup==`f' & restr_time==1, fe r
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,0,0,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',5) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

// Baseline and all subdistricts as controls ALL TIME PERIODS
foreach i of numlist 3 {
	local j=0
	foreach r in u { // t both {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.TP i.period, fe r 
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,0,0,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',10) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.TP i.period if fgroup==`f', fe r
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,0,0,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',10) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}


// Baseline and SPILL-OVER FREE DISCRICTS as controls ALL TIME PERIODS
foreach i of numlist 4 {
	local j=0
	foreach r in u { // t both {
		local bboth=cond("`r'"=="both","T`i'_both"," ")
		local j=`j'+1
		qui: xtreg price 1.T`i'_`r'#1.TP i.period, fe r 
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,0,0,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',11) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: xtreg price 1.T`i'_`r'#1.TP i.period if fgroup==`f', fe r
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,0,0,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',11) // AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}



** Both subdistricts as one treatement region
foreach i of numlist 0 1 2 {
	local or=cond(`i'==0,1,cond(`i'==1,5,cond(`i'==2,6,0)))
	local j=2
	foreach r in both {
		local j=`j'+1
		qui: eststo e_T`i'_`r'_0_`or': xtreg price 1.T`i'_`r'#1.TP i.period if restr_time==1, fe r
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a=X[1,1]
		local b=X[1,4]
		boottest 1.T`i'_`r'#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		mat CI=r(CI)
		mat B=(0,0,1,0, `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',`or') // AltTrend AltTreatPeriod TwoTimeBounds IncRice
		mat A=A\B
		
		foreach f of numlist 1 3 8 {
			qui: eststo e_T`i'_`r'_`f'_`or': xtreg price 1.T`i'_`r'#1.TP i.period if fgroup==`f' & restr_time==1, fe r
			local r2=e(r2_w)
			local n=e(N)
			_coef_table
			mat X=r(table)'
			local a=X[1,1]
			local b=X[1,4]
			boottest 1.T`i'_`r'#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
			mat CI=r(CI)
			mat B=(0,0,1,`f', `j',`i',`a',`b',r(p),CI[1,1],CI[1,2],`r2',`n',`or') //AltTrend AltTreatPeriod TwoTimeBounds IncRice
			mat A=A\B
		}
	}
}

mat colnames A= AltTrend AltTreatPeriod IncRice FoodGroup Treatment Control Impact pvalue_OG pvalue_WB lb ub r2_w N order

* List of results for fast consultation
preserve
	svmat A, names(col)
	label def IncRice 0 "No" 1 "Yes"
	label def AltTrend 0 "Flexible" 1 "Subd. TE" 
	label def AltTreatPeriod 0 "Bound 2S" 1 "Seasonal" 2 "All Info."
	label def Treatment 1 "Ukhia" 2 "Teknaf" 3 "Both"
	label def Control 0 "Best" 1 "Expanded" 2 "No Spillovers" 3 "All"
	label def FoodGroup 0 "All" 1 "Cereals" 3 "Proteins" 8 "Vegetables"
	label val Treatment Treatment
	label val Control Control
	label val FoodGroup FoodGroup
	label val IncRice IncRice
	label val AltTreatPeriod IncRice
	label val AltTrend AltTrend
	label val AltTreatPeriod AltTreatPeriod
	keep if Treatment!=.
	sort Treatment FoodGroup AltTrend Control AltTreatPeriod IncRice 
	format Impact pvalue_OG pvalue_WB %9.4f
	tostring Impact, gen(ccc) force usedisplayformat
	gen signif=ccc
	replace signif=signif+" *  " if pvalue_WB>0.05 & pvalue_WB<=0.10
	replace signif=signif+" ** " if pvalue_WB>0.01 & pvalue_WB<=0.05
	replace signif=signif+" ***" if pvalue_WB<=0.01
	replace signif=signif+"    " if pvalue_WB>0.10
	list Treatment FoodGroup Impact pvalue_OG pvalue_WB signif AltTrend Control AltTreatPeriod IncRice r2_w N order if Treatment==1, sep(100) // Treatment=1 is Ukhia and Treatment=3 is Both
restore

// Placebo
mat A=J(1,9,.)
* Flexible Trend
	xtreg price 1.T0_u#1.TP 1.T0_u#1.placebo i.period  if restr_time==1, fe vce(cluster groupid)
	local r2=e(r2_w)
	local n=e(N)
	_coef_table
	mat X=r(table)'
	local a1=X[1,1]
	local b1=X[1,4]
	local a2=X[2,1]
	local b2=X[2,4]
	boottest  1.T0_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c1=r(p)
	mat B=(0,`a1',`b1',`c1',`a2',`b2',`b2',`r2',`n') 
	mat A=A\B

	foreach f of numlist 1 3 8 {
		xtreg price 1.T0_u#1.TP 1.T0_u#1.placebo i.period if fgroup==`f' & restr_time==1, fe vce(cluster groupid)
		local r2=e(r2_w)
		local n=e(N)
		_coef_table
		mat X=r(table)'
		local a1=X[1,1]
		local b1=X[1,4]
		local a2=X[2,1]
		local b2=X[2,4]
		boottest  1.T0_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
		local c1=r(p)
		mat B=(`f',`a1',`b1',`c1',`a2',`b2',`b2',`r2',`n') 
		mat A=A\B
	}

mat colnames A= FoodGroup Impact I_pvalue_OG I_pvalue_WB Plac P_pvalue_OG P_pvalue_WB  r2_w N 

preserve
	svmat A, names(col)
	label def IncRice 0 "No" 1 "Yes"
	label def AltTrend 0 "Flexible" 1 "Subd. TE" 
	label def AltTreatPeriod 0 "Bound 2S" 1 "Seasonal" 2 "All Info."
	label def Treatment 1 "Ukhia" 2 "Teknaf" 3 "Both"
	label def Control 0 "Best" 1 "Expanded" 2 "No Spillovers"
	label def FoodGroup 0 "All" 1 "Cereals" 3 "Proteins" 8 "Vegetables"
	label val FoodGroup FoodGroup
	keep if FoodGroup!=.
	format Impact I_pvalue_OG I_pvalue_WB Plac P_pvalue_OG P_pvalue_WB %9.4f
	tostring Impact, gen(ccc) force usedisplayformat
	gen signif_I=ccc
	replace signif_I=signif_I+" *  " if I_pvalue_WB>0.05 & I_pvalue_WB<=0.10
	replace signif_I=signif_I+" ** " if I_pvalue_WB>0.01 & I_pvalue_WB<=0.05
	replace signif_I=signif_I+" ***" if I_pvalue_WB<=0.01
	replace signif_I=signif_I+"    " if I_pvalue_WB>0.10
	drop ccc
	tostring Plac, gen(ccc) force usedisplayformat
	gen signif_P=ccc
	replace signif_P=signif_P+" *  " if P_pvalue_WB>0.05 & P_pvalue_WB<=0.10
	replace signif_P=signif_P+" ** " if P_pvalue_WB>0.01 & P_pvalue_WB<=0.05
	replace signif_P=signif_P+" ***" if P_pvalue_WB<=0.01
	replace signif_P=signif_P+"    " if P_pvalue_WB>0.10
	list  FoodGroup signif_I I_pvalue_OG I_pvalue_WB signif_P P_pvalue_OG P_pvalue_WB r2_w N, sep(100)
restore

// Two arm treatment > FLEXIBLE TREND
gen twoarm_u=T0_u
replace twoarm_u=0 if T0_t==1
gen twoarm_t=T0_t
replace twoarm_t=0 if T0_u==1

mat A==J(1,9,.)
xtreg price 1.twoarm_u#1.TP 1.twoarm_t#1.TP i.period if restr_time==1, fe vce(cluster groupid)
local r2=e(r2_w)
local n=e(N)
_coef_table
mat X=r(table)'
local a1=X[1,1]
local b1=X[1,4]
local a2=X[2,1]
local b2=X[2,4]

boottest  1.twoarm_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local c1=r(p)
boottest  1.twoarm_t#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local c2=r(p)

mat B=(0,`a1',`b1',`c1',`a2',`b2',`c2',`r2',`n') 
mat A=A\B
	
foreach f of numlist 1 3 8 {
	xtreg price 1.twoarm_u#1.TP 1.twoarm_t#1.TP i.period if fgroup==`f' & restr_time==1, fe vce(cluster groupid)
	local r2=e(r2_w)
	local n=e(N)
	_coef_table
	mat X=r(table)'
	local a1=X[1,1]
	local b1=X[1,4]
	local a2=X[2,1]
	local b2=X[2,4]

	boottest  1.twoarm_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c1=r(p)
	boottest  1.twoarm_t#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c2=r(p)

	mat B=(`f',`a1',`b1',`c1',`a2',`b2',`c2',`r2',`n') 
	mat A=A\B
}

mat colnames A= FoodGroup UKH_imp UKH_pvalue_OG UKH_pvalue_WB TEK_imp TEK_pvalue_OG TEK_pvalue_WB  r2_w N 

preserve
	svmat A, names(col)
	label def IncRice 0 "No" 1 "Yes"
	label def AltTrend 0 "Flexible" 1 "Subd. TE" 
	label def AltTreatPeriod 0 "Bound 2S" 1 "Seasonal" 2 "All Info."
	label def Treatment 1 "Ukhia" 2 "Teknaf" 3 "Both"
	label def Control 0 "Best" 1 "Expanded" 2 "No Spillovers"
	label def FoodGroup 0 "All" 1 "Cereals" 3 "Proteins" 8 "Vegetables"
	label val FoodGroup FoodGroup
	keep if FoodGroup!=.
	format UKH_imp UKH_pvalue_OG UKH_pvalue_WB TEK_imp TEK_pvalue_OG TEK_pvalue_WB %9.4f
	tostring UKH_imp, gen(ccc) force usedisplayformat
	gen signif_U=ccc
	replace signif_U=signif_U+" *  " if UKH_pvalue_WB>0.05 & UKH_pvalue_WB<=0.10
	replace signif_U=signif_U+" ** " if UKH_pvalue_WB>0.01 & UKH_pvalue_WB<=0.05
	replace signif_U=signif_U+" ***" if UKH_pvalue_WB<=0.01
	replace signif_U=signif_U+"    " if UKH_pvalue_WB>0.10
	drop ccc
	tostring TEK_imp, gen(ccc) force usedisplayformat
	gen signif_T=ccc
	replace signif_T=signif_T+" *  " if TEK_pvalue_WB>0.05 & TEK_pvalue_WB<=0.10
	replace signif_T=signif_T+" ** " if TEK_pvalue_WB>0.01 & TEK_pvalue_WB<=0.05
	replace signif_T=signif_T+" ***" if TEK_pvalue_WB<=0.01
	replace signif_T=signif_T+"    " if TEK_pvalue_WB>0.10
	list  FoodGroup signif_U UKH_pvalue_OG UKH_pvalue_WB  signif_T TEK_pvalue_OG TEK_pvalue_WB  r2_w N, sep(100)
restore

// TWO ARM TREATMENT WITH ALL TIME PERIODS AND ALL REGIONS
replace T3_u=0 if T3_t==1
replace T3_t=0 if T3_u==1

mat A==J(1,9,.)
xtreg price 1.T3_u#1.TP 1.T3_t#1.TP i.period, fe vce(cluster groupid)
local r2=e(r2_w)
local n=e(N)
_coef_table
mat X=r(table)'
local a1=X[1,1]
local b1=X[1,4]
local a2=X[2,1]
local b2=X[2,4]

boottest  1.T3_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local c1=r(p)
boottest  1.T3_t#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local c2=r(p)

mat B=(0,`a1',`b1',`c1',`a2',`b2',`c2',`r2',`n') 
mat A=A\B
	
foreach f of numlist 1 3 8 {
	xtreg price 1.T3_u#1.TP 1.T3_t#1.TP i.period if fgroup==`f', fe vce(cluster groupid)
	local r2=e(r2_w)
	local n=e(N)
	_coef_table
	mat X=r(table)'
	local a1=X[1,1]
	local b1=X[1,4]
	local a2=X[2,1]
	local b2=X[2,4]

	boottest  1.T3_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c1=r(p)
	boottest  1.T3_t#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c2=r(p)

	mat B=(`f',`a1',`b1',`c1',`a2',`b2',`c2',`r2',`n') 
	mat A=A\B
}

mat colnames A= FoodGroup UKH_imp UKH_pvalue_OG UKH_pvalue_WB TEK_imp TEK_pvalue_OG TEK_pvalue_WB  r2_w N 

preserve
	svmat A, names(col)
	label def IncRice 0 "No" 1 "Yes"
	label def AltTrend 0 "Flexible" 1 "Subd. TE" 
	label def AltTreatPeriod 0 "Bound 2S" 1 "Seasonal" 2 "All Info."
	label def Treatment 1 "Ukhia" 2 "Teknaf" 3 "Both"
	label def Control 0 "Best" 1 "Expanded" 2 "No Spillovers"
	label def FoodGroup 0 "All" 1 "Cereals" 3 "Proteins" 8 "Vegetables"
	label val FoodGroup FoodGroup
	keep if FoodGroup!=.
	format UKH_imp UKH_pvalue_OG UKH_pvalue_WB TEK_imp TEK_pvalue_OG TEK_pvalue_WB %9.4f
	tostring UKH_imp, gen(ccc) force usedisplayformat
	gen signif_U=ccc
	replace signif_U=signif_U+" *  " if UKH_pvalue_WB>0.05 & UKH_pvalue_WB<=0.10
	replace signif_U=signif_U+" ** " if UKH_pvalue_WB>0.01 & UKH_pvalue_WB<=0.05
	replace signif_U=signif_U+" ***" if UKH_pvalue_WB<=0.01
	replace signif_U=signif_U+"    " if UKH_pvalue_WB>0.10
	drop ccc
	tostring TEK_imp, gen(ccc) force usedisplayformat
	gen signif_T=ccc
	replace signif_T=signif_T+" *  " if TEK_pvalue_WB>0.05 & TEK_pvalue_WB<=0.10
	replace signif_T=signif_T+" ** " if TEK_pvalue_WB>0.01 & TEK_pvalue_WB<=0.05
	replace signif_T=signif_T+" ***" if TEK_pvalue_WB<=0.01
	replace signif_T=signif_T+"    " if TEK_pvalue_WB>0.10
	list  FoodGroup signif_U UKH_pvalue_OG UKH_pvalue_WB  signif_T TEK_pvalue_OG TEK_pvalue_WB  r2_w N, sep(100)
restore

// Two arm treatment > SUBDISTRICT SPECIFIC TRENDS
mat A==J(1,9,.)
xtreg price 1.twoarm_u#1.TP 1.twoarm_t#1.TP i.period i.period#1.twoarm_u i.period#1.twoarm_t if restr_time==1, fe vce(cluster groupid)
local r2=e(r2_w)
local n=e(N)
_coef_table
mat X=r(table)'
local a1=X[1,1]
local b1=X[1,4]
local a2=X[2,1]
local b2=X[2,4]

boottest  1.twoarm_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local c1=r(p)
boottest  1.twoarm_t#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local c2=r(p)

mat B=(0,`a1',`b1',`c1',`a2',`b2',`c2',`r2',`n') 
mat A=A\B
	
foreach f of numlist 1 3 8 {
	xtreg price 1.twoarm_u#1.TP 1.twoarm_t#1.TP i.period i.period#1.twoarm_u i.period#1.twoarm_t if fgroup==`f' & restr_time==1, fe vce(cluster groupid)
	local r2=e(r2_w)
	local n=e(N)
	_coef_table
	mat X=r(table)'
	local a1=X[1,1]
	local b1=X[1,4]
	local a2=X[2,1]
	local b2=X[2,4]

	boottest  1.twoarm_u#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c1=r(p)
	boottest  1.twoarm_t#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local c2=r(p)

	mat B=(`f',`a1',`b1',`c1',`a2',`b2',`c2',`r2',`n') 
	mat A=A\B
}

mat colnames A= FoodGroup UKH_imp UKH_pvalue_OG UKH_pvalue_WB TEK_imp TEK_pvalue_OG TEK_pvalue_WB  r2_w N 

preserve
	svmat A, names(col)
	label def IncRice 0 "No" 1 "Yes"
	label def AltTrend 0 "Flexible" 1 "Subd. TE" 
	label def AltTreatPeriod 0 "Bound 2S" 1 "Seasonal" 2 "All Info."
	label def Treatment 1 "Ukhia" 2 "Teknaf" 3 "Both"
	label def Control 0 "Best" 1 "Expanded" 2 "No Spillovers"
	label def FoodGroup 0 "All" 1 "Cereals" 3 "Proteins" 8 "Vegetables"
	label val FoodGroup FoodGroup
	keep if FoodGroup!=.
	format UKH_imp UKH_pvalue_OG UKH_pvalue_WB TEK_imp TEK_pvalue_OG TEK_pvalue_WB %9.4f
	tostring UKH_imp, gen(ccc) force usedisplayformat
	gen signif_U=ccc
	replace signif_U=signif_U+" *  " if UKH_pvalue_WB>0.05 & UKH_pvalue_WB<=0.10
	replace signif_U=signif_U+" ** " if UKH_pvalue_WB>0.01 & UKH_pvalue_WB<=0.05
	replace signif_U=signif_U+" ***" if UKH_pvalue_WB<=0.01
	replace signif_U=signif_U+"    " if UKH_pvalue_WB>0.10
	drop ccc
	tostring TEK_imp, gen(ccc) force usedisplayformat
	gen signif_T=ccc
	replace signif_T=signif_T+" *  " if TEK_pvalue_WB>0.05 & TEK_pvalue_WB<=0.10
	replace signif_T=signif_T+" ** " if TEK_pvalue_WB>0.01 & TEK_pvalue_WB<=0.05
	replace signif_T=signif_T+" ***" if TEK_pvalue_WB<=0.01
	replace signif_T=signif_T+"    " if TEK_pvalue_WB>0.10
	list  FoodGroup signif_U UKH_pvalue_OG UKH_pvalue_WB  signif_T TEK_pvalue_OG TEK_pvalue_WB  r2_w N, sep(100)
restore


**** NEW SEASONALITY 
eststo season_full: xtreg price time 1.FCR#1.TP i.monthid, fe vce(cluster groupid)
local b=_b[1.FCR#1.TP]
local r2=e(r2_w)
local n=e(N)
boottest 1.FCR#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local p=r(p)
mat X=(`b',`p',`r2',`n')

foreach f of numlist 1 3 8 {
	eststo season_full: xtreg price time 1.FCR#1.TP i.monthid if fgroup==`f', fe vce(cluster groupid)
	local b=_b[1.FCR#1.TP]
	local r2=e(r2_w)
	local n=e(N)
	boottest 1.FCR#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
	local p=r(p)
	mat A=(`b',`p',`r2',`n')
	mat X=(X\A)
}
mat list X

*** NEW PLACEBO, RANDOMLY TAKING ONE CONTORL REGION AS TREATMENT
tab groupid T0_u

cap drop plac_reg
gen plac_reg=1 if groupid==3
replace plac_reg=0 if groupid==10 | groupid==5 | groupid==12
xtreg price i.period 1.plac_reg#1.TP if restr_time==1, fe r
local b=_b[1.plac_reg#1.TP]
local r2=e(r2_w)
local n=e(N)
boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
local p=r(p)
mat X3=(`b',`p',`r2',`n')

foreach f of numlist 1 3 8 {
	xtreg price i.period 1.plac_reg#1.TP if fgroup==`f', fe r
	local b=_b[1.plac_reg#1.TP]
	local r2=e(r2_w)
	local n=e(N)
	boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
	local p=r(p)
	mat A=(`b',`p',`r2',`n')
	mat X3=(X3\A)
}

cap drop plac_reg
gen plac_reg=1 if groupid==5
replace plac_reg=0 if groupid==10 | groupid==3 | groupid==12
xtreg price i.period 1.plac_reg#1.TP if restr_time==1, fe r
local b=_b[1.plac_reg#1.TP]
local r2=e(r2_w)
local n=e(N)
boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
local p=r(p)
mat X5=(`b',`p',`r2',`n')

foreach f of numlist 1 3 8 {
	xtreg price i.period 1.plac_reg#1.TP if fgroup==`f', fe r
	local b=_b[1.plac_reg#1.TP]
	local r2=e(r2_w)
	local n=e(N)
	boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
	local p=r(p)
	mat A=(`b',`p',`r2',`n')
	mat X5=(X5\A)
}

cap drop plac_reg
gen plac_reg=1 if groupid==10
replace plac_reg=0 if groupid==3 | groupid==5 | groupid==12
xtreg price i.period 1.plac_reg#1.TP if restr_time==1, fe r
local b=_b[1.plac_reg#1.TP]
local r2=e(r2_w)
local n=e(N)
boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
local p=r(p)
mat X10=(`b',`p',`r2',`n')

foreach f of numlist 1 3 8 {
	xtreg price i.period 1.plac_reg#1.TP if fgroup==`f', fe r
	local b=_b[1.plac_reg#1.TP]
	local r2=e(r2_w)
	local n=e(N)
	boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
	local p=r(p)
	mat A=(`b',`p',`r2',`n')
	mat X10=(X10\A)
}

cap drop plac_reg
gen plac_reg=1 if groupid==12
replace plac_reg=0 if groupid==3 | groupid==5 | groupid==10
xtreg price i.period 1.plac_reg#1.TP if restr_time==1, fe r
local b=_b[1.plac_reg#1.TP]
local r2=e(r2_w)
local n=e(N)
boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
local p=r(p)
mat X12=(`b',`p',`r2',`n')

foreach f of numlist 1 3 8 {
	xtreg price i.period 1.plac_reg#1.TP  if fgroup==`f', fe r
	local b=_b[1.plac_reg#1.TP]
	local r2=e(r2_w)
	local n=e(N)
	boottest 1.plac_reg#1.TP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
	local p=r(p)
	mat A=(`b',`p',`r2',`n')
	mat X12=(X12\A)
}
mat list X3
mat list X5
mat list X10
mat list X12


** NEW EXTRA INDRANIL
gen FTP=(period>=14)
gen FCR=(Upazilla=="Ukhia")
replace FCR=. if Upazilla=="Teknaf "

xtreg price period 1.FCR#1.FTP, fe vce(cluster groupid)
local b=_b[1.FCR#1.FTP]
local r2=e(r2_w)
local n=e(N)
boottest 1.FCR#1.FTP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
local p=r(p)
mat X=(`b',`p',`r2',`n')

foreach f of numlist 1 3 8 {
	xtreg price time 1.FCR#1.FTP i.monthid if fgroup==`f', fe vce(cluster groupid)
	local b=_b[1.FCR#1.FTP]
	local r2=e(r2_w)
	local n=e(N)
	boottest 1.FCR#1.FTP, boot(wild) seed(123) nonull small nograph bootcluster(groupid) reps(5000) weight(normal)
	local p=r(p)
	mat A=(`b',`p',`r2',`n')
	mat X=(X\A)
}
mat list X



*********************************************
*********************************************
*** Regressions for aid narrative
*********************************************
*********************************************

*** Items are: i) Low quality rice (ATOP=4, and LQ=5), ii) Pulses (Moshurnorm=14, iii) Oil (Soybeanpk=30)
replace fooditem=5 if fooditem==4 // Merging atop and lqr (the line will not produce changes if atop is out of the analyis)
local seed 1
foreach fi of numlist 5 14 30 {
		eststo reg_`fi': xtreg price 1.T0_u#1.TP i.period if fooditem==`fi' & restr_time==1, fe vce(cluster groupid)
		boottest 1.T0_u#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
}

gen aidgroup=(fooditem==5 | fooditem==14 | fooditem==30)
eststo reg_group: xtreg price 1.T0_u#1.TP i.period if aidgroup==1 & restr_time==1, fe vce(cluster groupid)
boottest 1.T0_u#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)

estimates table reg*, keep(1.T0_u#1.TP) star(0.1 0.05 0.001) stats(N r2_w)

* Same with all subdistricts as controls
local seed 1
foreach fi of numlist 5 14 30 {
		eststo reg_`fi': xtreg price 1.FCR#1.TP i.period if fooditem==`fi' & restr_time==1, fe vce(cluster groupid)
		boottest 1.FCR#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
}

eststo reg_group: xtreg price 1.FCR#1.TP i.period if aidgroup==1 & restr_time==1, fe vce(cluster groupid)
boottest 1.FCR#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)

estimates table reg*, keep(1.FCR#1.TP) star(0.1 0.05 0.001) stats(N r2_w)


***** No rice, lentils or oil
gen no_aid=(fooditem!=5 & fooditem!=14 & fooditem!=30) // 1=without aid items

mat R=J(1,5,.)

eststo all_with_aidfi: xtreg price 1.T0_u#1.TP i.period if restr_time==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.T0_u#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(1,`a',`b',`r2',`n')
mat R=R\A

eststo all_without_aidfi: xtreg price 1.T0_u#1.TP i.period if no_aid==1 & restr_time==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.T0_u#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(2,`a',`b',`r2',`n')
mat R=R\A

eststo cer_with_aidfi: xtreg price 1.T0_u#1.TP i.period if restr_time==1 & fgroup==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.T0_u#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(3,`a',`b',`r2',`n')
mat R=R\A

eststo cer_without_aidfi: xtreg price 1.T0_u#1.TP i.period if no_aid==1 & restr_time==1 & fgroup==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.T0_u#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(4,`a',`b',`r2',`n')
mat R=R\A

label def x 1 "Food Index with all food items" ///
			2 "Food Index witouth aid-related items" ///
			3 "Cereals with all food items" ///
			4 "Cereals witout aid-related items"

mat colnames R=spec impact p_value r2 n
preserve
	svmat R, names(col)
	drop if spec==.
	label val spec x
	format p_value %9.4f
	keep spec impact p_value r2 n
	list, sep(6)
restore

** Same thing but with all subdistricts as controls

mat R=J(1,5,.)

eststo all_with_aidfi: xtreg price 1.FCR#1.TP i.period if restr_time==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.FCR#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(1,`a',`b',`r2',`n')
mat R=R\A

eststo all_without_aidfi: xtreg price 1.FCR#1.TP i.period if no_aid==1 & restr_time==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.FCR#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(2,`a',`b',`r2',`n')
mat R=R\A

eststo cer_with_aidfi: xtreg price 1.FCR#1.TP i.period if restr_time==1 & fgroup==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.FCR#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(3,`a',`b',`r2',`n')
mat R=R\A

eststo cer_without_aidfi: xtreg price 1.FCR#1.TP i.period if no_aid==1 & restr_time==1 & fgroup==1, fe vce(cluster groupid)
_coef_table
local r2=e(r2_w)
local n=e(N)
mat X=r(table)'
local a=X[1,1]
boottest 1.FCR#1.TP , boot(wild) seed(`seed') nonull small nograph bootcluster(groupid) reps(5000) weight(gamma)
local b=r(p)
mat A=(4,`a',`b',`r2',`n')
mat R=R\A

label def x 1 "Food Index with all food items" ///
			2 "Food Index witouth aid-related items" ///
			3 "Cereals with all food items" ///
			4 "Cereals witout aid-related items"

mat colnames R=spec impact p_value r2 n
preserve
	svmat R, names(col)
	drop if spec==.
	label val spec x
	format p_value %9.4f
	keep spec impact p_value r2 n
	list, sep(6)
restore

