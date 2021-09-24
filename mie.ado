*! version 1.0.0 24sep2021
cap program drop mie
program mie, eclass
syntax, testsyntax(string) pe(string) ve(string) [df(string)]

ereturn clear

qui count
local nobs = `r(N)'

`testsyntax'

tempname b V
matrix `b' = `pe'
matrix `V' = `ve' 

if "`df'"=="" {
	if "`e(df_r)'"!="" {
		local df = `e(df_r)'
	}
}

matrix colnames `b' = "group"
matrix colnames `V' = "group"
matrix rownames `V' = "group"

ereturn post `b' `V', obs(`nobs')
ereturn local cmd "mi_est"
if "`df'"!="" {
	ereturn scalar df_r = `df'
}

end

