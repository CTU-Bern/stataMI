*! version 1.0.0 24sep2021
cap program drop mig
program mig, rclass

syntax [varlist(default=none)], testsyntax(string) ///
	[pe(string) ve(string) nimp(numlist) ///
	DFMethod(string) DFResidual(string) DFParameter(numlist) ///
	chi2(string) chi2_df(string) converged(string) ///
	fvalue(string) f_df(string) ///
	pvalue(string)]


*default input
cap assert ("`pe'"!="" & "`ve'" !="") | "`chi2'"!="" | "`pvalue'"!="" | "`fvalue'"!=""
if _rc {
	dis "Please specify either pe and ve, chi2 or pvalue options"
}

*testsyntax
if "`varlist'" !="" {
	local ts=subinstr("`testsyntax'","variable","`1'",.)
	local ts=subinstr("`ts'","group","`2'",.)
}
else {
	local ts = "`testsyntax'"
}


*dfmethod
if inlist("`dfmethod'","small","adjusted") {
	local dfmethod adj
}

cap assert inlist("`dfmethod'","adj","old","large","small","")
if _rc {
	dis "dfmethod not recognized, only large and small are allowed, default large is used."
	local dfmethod
}

local collectdf 0


if "`dfmethod'"=="adj" {
	cap assert "`dfresidual'"!="" | "`dfparameter'"!=""
	if _rc {
		dis as text "Degrees of freedom of residuals (dfresidual) or parameter (dfparameter) have to be defined to use small sample adjustment for degress of freedom---large sample approximation used."
		local dfmethod	
	}
	if "`dfresidual'"!="" {
		cap assert "`dfparameter'"=="" 
		if _rc {
			dis as text "Only one of dfresidual and dfparameter should be given---dfparameter ignored."
			local dfparameter
		}
		cap confirm number `dfresidual'
		if _rc {
			local collectdf 1
		}
	}
	if "`dfparameter'"!="" {
		local wc: word count `dfparameter'
		cap assert `wc'==1
		if _rc {
			local dfparameter: word 1 of `dfparameter'
			dis as text "Only first element of dfparameter is used (`dfparameter')"
		}
		cap confirm number `dfparameter'
		if _rc {
			dis as text "dfparameter has to be a number---large sample approximation used"
			local dfmethod
		}	
		else {
			qui count if _mi_m==0
			local nt = `r(N)'
			local dfresidual = `nt'-`dfparameter'
		}
	}
}

if "`nimp'"!="" {
	local wc: word count `nimp'
	cap assert `wc'==1
	if _rc {
		local nimp: word 1 of `nimp'
		dis as text "Only first element of nimp is used (`nimp')"
	}
}



if ("`pe'"!="" & "`ve'" !="")  {
	 
	if "`ts'"=="none"  {
		
		if "`nimp'"=="" {
			qui count
			local M=`r(N)'
		}
		else {
			local M=`nimp'
		}
		
		//point estimate
		sum `pe'
		local qn=`r(mean)'

		//variance between
		local bn=(`r(sd)')^2

		//variance within
		sum `ve'
		local u=`r(mean)'

		//total variance
		local vt=`u'+(1+1/`M')*`bn'
		local se=sqrt(`vt')
		
		//degrees of freedom according to Rubin (1987), Van Buuren (2018)):
		local df=(`M'-1)*(1+`u'/((1+1/`M')*`bn'))^2
		
		if "`dfmethod'"=="adj" {
			if `collectdf'==1 {
				sum `dfresidual'
				local dfresidual = `r(mean)'
			}
			
			//adjusted df according to  Barnard and Rubin (1999), used in R (mice) and Stata if e(df_r) is defined
			local lambda = (`bn' + `bn'/`M')/`vt'
			local dfobs = `dfresidual'*(`dfresidual'+1)/(`dfresidual'+3)*(1-`lambda')
			local dfadj = `df'*`dfobs'/(`df'+`dfobs')
			local dfu = `dfadj'
		}
		else {
			local dfu = `df'
		}
		
		local Mcount = `M'

	}
	else {

		*point estimate and variance
		qui mi query
		local M = r(M)
		
		if "`nimp'"!="" {
			cap assert  `M'==`nimp'
			if _rc {
				if `nimp'<`M' {
					local M = `nimp'
					dis as text "More than `nimp' imputations found, only the first `nimp' were considered."
				}
				else {
					dis as text "Less than `nimp' imputations found, only `M' imputations were considered."
				}
				
			}
		}
	
		tempname z v dfcol
		scalar `z'=0
		scalar `v'=0
		local Mcount
		
		//df (if applicable)
		scalar `dfcol' = 0
		
		if "`dfmethod'"=="adj" {
			if `collectdf'==1 {
				local add scalar `dfcol' = `dfcol' + `dfresidual' 
			}
		}
			
		if "`converged'" == "" {
			qui mi xeq 1/`M': `ts'; scalar `z' = `z' + `pe'; scalar `v' = `v' + `ve'; `add'
			local Mcount = `M'
		}
		else {
			forvalues i=1/`M' {
				preserve
				mi extract `i', clear
				cap `ts'
				if `converged' == 1 {
					scalar `z' = `z' + `pe'
					scalar `v' = `v' + `ve'
					local Mcount = `Mcount' + 1
					`add'
				}
				restore
			}
			
			if `Mcount' != `M' {
				dis "Only `Mcount' imputation converged"
			}
		}
		
	
		//point estimate:
		local qn=`z'/`Mcount'
		
		//variance within:
		local u=`v'/`Mcount'
		
		//adjusted df (if applicable)
		local dfcoll = `dfcol'/`Mcount'
		
		//variance between:
		tempname b
		scalar `b'=0

		if "`converged'" == "" {
			qui mi xeq 1/`M': `ts'; scalar `b' = `b' + (`pe'-`qn')^2; `add'
		}
		else {
			forvalues i=1/`M' {
				preserve
				mi extract `i', clear
				cap `ts' 
				if `converged' == 1 {
					scalar  `b' = `b' + (`pe'-`qn')^2 
				}
				restore
			}
		}

		local bn=1/(`Mcount'-1)*`b'

		//total variance:
		local vt=`u'+(1+1/`Mcount')*`bn'
		local se=sqrt(`vt')

		//degrees of freedom according to Rubin (1987), Van Buuren (2018)):
		local df=(`Mcount'-1)*(1+`u'/((1+1/`Mcount')*`bn'))^2	
		
		//adjusted df (if applicable)
		if `dfcoll' != 0 {
			local dfresidual = `dfcoll'
		}
		
		if "`dfmethod'"=="adj" {
			//adjusted df according to  Barnard and Rubin (1999), used in R (mice) but not in Stata
			//following https://bookdown.org/mwheymans/bookmi/rubins-rules.html#significance-testing
			local lambda = (`bn' + `bn'/`Mcount')/`vt'
			//local riv = (`bn' + `bn'/`Mcount')/`u'
			//local dfold = (`Mcount'-1)*(1+1/(`riv'))^2
			//local dfold = (`Mcount'-1)/(`lambda'^2)
			//the same as df above

			local dfobs = `dfresidual'*(`dfresidual'+1)/(`dfresidual'+3)*(1-`lambda')
			local dfadj = `df'*`dfobs'/(`df'+`dfobs')
			local dfu = `dfadj'
		}
		else {
			local dfu = `df'
		}

	}
	
	local t=`qn'/sqrt(`vt')
	
	if `dfu'>10^10 {
		local pv= 2*(1-normal(abs(`t')))
		local ll=`qn'-invnormal(0.975)*`se'
		local ul=`qn'+invnormal(0.975)*`se'
		local crit=invnormal(0.975)
	}
	else {
		local pv= 2*(1-t(`dfu',abs(`t')))
		local ll=`qn'-invt(`dfu',0.975)*`se'
		local ul=`qn'+invt(`dfu',0.975)*`se'
		local crit=invt(`dfu',0.975)
	}

	tempname R
	matrix `R' = (`qn' \ `se'  \ `t' \ `pv' \ `ll' \ `ul' \ `dfu' \ `crit')
	mat colnames `R' = test
	mat rownames `R' = b se t pvalue ll ul df crit
	 
	return scalar M = `Mcount' 
	return matrix table `R'
	dis ""
	dis "mean:	`qn'"
	dis "se:	`se'"
	dis "t: 	`t'"
	dis "pvalue:	`pv'"
	dis "ll:	`ll'"
	dis "ul:	`ul'"
	dis "df:	`dfu'"
	//dis "dfadj:	`dfadj'"
}

*F-values 

if "`fvalue'" != "" {
	if "`f_df'"=="" {
		dis "Degress of freedom for numerator of F-statistic not given, 1 assumed."
		local f_df = 1
	}
	
	if "`ts'"=="none" {
		tempvar fchi2 
		qui gen `fchi2' = `f_df'*`fvalue'
	}
	else {
		local fchi2 `f_df'*`fvalue'
	}
	
	local chi2_df `f_df'
	
	mig_chi2, ts2(`ts') chi2(`fchi2') chi2_df(`f_df') nimp(`nimp') converged(`coverged')

	return scalar p_chi2 = `r(p_chi2)'
	dis "chi2 pvalue: `r(p_chi2)'"
	return scalar Dx = r(Dx)
	return scalar vx = r(vx)
	return scalar chi2_df = `r(chi2_df)'
	return scalar av_chi2=`r(av_chi2)'
	return scalar M = `r(M)'
	
}
//repeat chi2 



*chi2

if "`chi2'"!="" {
	
	mig_chi2, ts2(`ts') chi2(`chi2') chi2_df(`chi2_df') nimp(`nimp') converged(`coverged')

	return scalar p_chi2 = `r(p_chi2)'
	dis "chi2 pvalue: `r(p_chi2)'"
	return scalar Dx = r(Dx)
	return scalar vx = r(vx)
	return scalar chi2_df = `r(chi2_df)'
	return scalar av_chi2=`r(av_chi2)'
	return scalar M = `r(M)'

}

*median p-value 

if "`pvalue'"!="" {
	
	mig_pv, ts2(`ts') pvalue(`pvalue') nimp(`nimp') converged(`coverged')
	return scalar p_med = `r(p_med)'
	return scalar M = `r(M)'
	dis "median pvalue: `r(p_med)'"

}

end


*******************
*Helper functions
**************

*chi squared 
***************

//https://bookdown.org/mwheymans/bookmi/pooling-methods-for-categorical-variables.html
//Multiple parameter Wald test or D2 method 
//Enders (2012), Marshall (2009)

cap program drop mig_chi2
program mig_chi2, rclass

syntax, ts2(string) chi2(string) [chi2_df(string) nimp(numlist) converged(string)]

if "`chi2_df'"=="" {
	local chi2_df=1
	dis "Degress of freedom for Chi2-statistic not given, 1 assumed."
}

if "`ts2'"=="none" {
	if "`nimp'"=="" {
		qui count
		local M=`r(N)'
	}
	else {
		local M=`nimp'
	}
	qui sum `chi2'
	local av_x2=`r(mean)'
	tempvar sqrt_chi2
	gen `sqrt_chi2'=sqrt(`chi2')
	qui sum `sqrt_chi2'
	local av_sqrt_chi2=`r(mean)'
	tempvar diff2
	gen `diff2'=(`sqrt_chi2'-`av_sqrt_chi2')^2
	qui sum `diff2'
	local sd2=`r(sum)'
	local Mcount = `M'
	qui sum `chi2_df'
	local avchi2_df = `r(mean)'

}
else {
	qui mi query
	local M = r(M)
	
	if "`nimp'"!="" {
		cap assert  `M'==`nimp'
		if _rc {
			if `nimp'<`M' {
				local M = `nimp'
				dis as text "More than `nimp' imputations found, only the first `nimp' were considered."
			}
			else {
				dis as text "Less than `nimp' imputations found, only `M' imputations were considered."
			}
			
		}
	}
	
	scalar X2=0
	scalar sqrtX2=0
	local Mcount
	tempname A
	tempname B
	tempname C
	tempname C2
	matrix `A'=J(1,1,.)
	scalar X2df=0
	
	if "`converged'" == "" {
		qui mi xeq 1/`M': `ts2'; scalar X2 = X2 + `chi2'; ///
		matrix `A' = `A', sqrt(abs(`chi2')); ///
		scalar sqrtX2=sqrtX2 + sqrt(abs(`chi2')); ///
		scalar X2df = X2df + `chi2_df'
		local Mcount = `M'
	}
	else {
		forvalues i=1/`M' {
			preserve
			mi extract `i', clear
			cap `ts2'
			if `converged' == 1 {
				local Mcount = `Mcount' + 1
				scalar X2 = X2 + `chi2'
				matrix `A' = `A', sqrt(abs(`chi2'))
				scalar sqrtX2=sqrtX2 + sqrt(abs(`chi2'))
				scalar X2df = X2df + `chi2_df'
			}
			restore
		}
		if `Mcount' != `M' {
			dis "Only `Mcount' imputation converged"
		}
	}
	matrix `A'=`A'[1...,2...]
	local av_x2=X2/`Mcount'
	matrix `B'=J(1,`Mcount',sqrtX2/`Mcount')
	matrix `C'=(`B'-`A')
	matrix `C2'=`C'*`C''
	local sd2=`C2'[1,1]
	local avchi2_df = X2df/`Mcount'
}

scalar rx=(1+1/`Mcount')*1/(`Mcount'-1)*`sd2'
scalar Dx=(`av_x2'/`avchi2_df'-(`Mcount'+1)/(`Mcount'-1)*rx)/(1+rx)
scalar vx=`avchi2_df'^(-3/`Mcount')*(`Mcount'-1)*(1+1/rx)^2
if vx>(10^15) {
	local pv_chi2=1-chi2(`avchi2_df',Dx)
}
else {
	local pv_chi2=1-F(`avchi2_df',vx,Dx)
}

return scalar p_chi2 = `pv_chi2'
return scalar Dx = Dx
return scalar vx = vx
return scalar chi2_df = `avchi2_df'
return scalar av_chi2=`av_x2'
return scalar M = `Mcount'

end



*median p-value 
***********************

//The Median P Rule
//Eekhout, Wiel, and Heymans (2017)
// MI procedure without the outcome variable in the imputation model?

cap program drop mig_pv
program mig_pv, rclass

syntax, ts2(string) pvalue(string) [nimp(numlist) converged(string)]

if "`ts2'"=="none" {
	if "`nimp'"=="" {
		qui count
		local M=`r(N)'
	}
	else {
		local M=`nimp' 
	}
	qui sum `pvalue',d
	local medp=`r(p50)'
}
else {		
	qui mi query
	local M = r(M)
		
	if "`nimp'"!="" {
		cap assert  `M'==`nimp'
		if _rc {
			if `nimp'<`M' {
				local M = `nimp'
				dis as text "More than `nimp' imputations found, only the first `nimp' were considered."
			}
			else {
				dis as text "Less than `nimp' imputations found, only `M' imputations were considered."
			}
			
		}
	}
	
	local Mcount
	tempname D
	matrix `D'=J(1,1,.)	
	
	if "`converged'" == "" {
		qui mi xeq 1/`M': `ts2'; matrix `D' = `D', `pvalue'
		local Mcount = `M'
	}
	else {
		forvalues i=1/`M' {
			preserve
			mi extract `i', clear
			cap `ts2'
			if `converged' == 1 {
				local Mcount = `Mcount' + 1
				matrix `D' = `D',`pvalue'
			}
			restore
		}
		if `Mcount' != `M' {
			dis "Only `Mcount' imputation converged"
		}
	}
	
	matrix `D'=`D'[1...,2...]
	matrix `D'=`D''
	tempvar dvar
	svmat `D', names(`dvar')
	qui sum `dvar',d
	local medp = `r(p50)'
	drop `dvar'
}

return scalar p_med = `medp'
return scalar M = `Mcount'

end


