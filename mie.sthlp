{smcl}
{* *! version 1.0.0  24sep2021}{...}
{hline}
{cmd:help mie} {right:also see:  {help mi}}
{hline}

{title:Title}

{phang}
{bf:mie} {hline 2} Makes an estimation command mi estimable.


{marker syntax}{...}
{title:Syntax}

{p 4 6 2}
{cmd:mi} {cmdab:est:imate} , cmdok [{it:mi_estimate_options}] {cmd::} {cmdab:mie} {cmd:,} {opth testsyntax(string)} {opth pe(string)} {opth ve(string)} [{opth df(string)}]


{marker description}{...}
{title:Description}

{pstd}
{cmd:mie} prepares an estimation command defined by {cmd:testsyntax} for use with prefix {manhelp mi_estimate MI:mi estimate}.
	

{marker options}{...}
{title:Options}
{p 4 6 2}

{phang}
{opth testsyntax(string)} specifies the command applied to each imputation set.

{phang}
{opth pe(string)} specifies where the point estimate is stored. Has to be an object returned by {cmd:testsyntax}.

{phang}
{opth ve(string)} specifies where the variance estimate is stored. Has to be an object returned by {cmd:testsyntax}.

{phang}
{opth df(string)} specifies where the degrees of freedom are stored to apply 
	the small sample correction of {help mie##BR1999:Barnard and Rubin (1999)}. 
	Has to be an object returned by {cmd:testsyntax}.
	If not given the degrees of freedom stored in {cmd:e(df_r)} are used. 
	If none is given,
	the large sample degrees of freedom are used.


{marker results}{...}
{title:Stored results}

{pstd}See {manhelp mi_estimate MI:mi estimate}{p_end}


{marker examples}{...}
{title:Examples}

{pstd}Data{p_end}
{phang2}{cmd:. webuse mheart1s20}{p_end}

{pstd}Linear regression{p_end}
{phang2}{cmd:. mi estimate, cmdok: mie, testsyntax(regress bmi attack) pe(r(table)[1,1]) ve(r(table)[2,1]^2)}{p_end}

{pstd}Risk difference{p_end}
{phang2}{cmd:. mi estimate, cmdok: mie, testsyntax(cs attack gender) pe(r(rd)) ve(((r(ub_rd)-r(rd))/invnormal(0.975))^2)}{p_end}

{pstd}Risk difference{p_end}
{phang2}{cmd:. mi estimate, cmdok: mie, testsyntax(cs attack gender) pe(r(rd)) ve(((r(ub_rd)-r(rd))/invnormal(0.975))^2)}{p_end}


{marker references}{...}
{title:References}

{marker BR1999}{...}
{phang}
Barnard, J., and D. B. Rubin. 1999.  Small-sample degrees of freedom with 
multiple imputation.  {it:Biometrika} 86: 948-955.




