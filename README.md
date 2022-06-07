_v. 1.0.0_  

`stataMI`
========

Collection of ado-files to help with multiple imputations in Stata.

`mie` prepares an estimation command defined for use with prefix mi estimate.

`mig` combines estimates over multiple imputations.
	Either using Rubin's rule on a point and variance estimate,
	the D2 procedure on a chi-squared (or F) statistic,
	or the median P rule on a p-value.
	


Installation
------------

In order to install `stataMI` from github the github-package is required:

	net install github, from("https://haghish.github.io/github/")

You can then install the development version of `stataMI` with:

	github install CTU-Bern/stataMI


Example
------------

	# load example dataset
	webuse mheart1s20
	
	# mie
	mi estimate, cmdok: mie, testsyntax(regress bmi attack) pe(r(table)[1,1]) ve(r(table)[2,1]^2)
	
	# mig
	mig, testsyntax(regress bmi attack) pe(r(table)[1,1]) ve(r(table)[2,1]^2) dfm(small) dfr(e(df_r))
	

Author
------

**Lukas Bütikofer**  
CTU Bern  
lukas.buetikofer@ctu.unibe.ch  
<https://github.com/CTU-Bern/stataMI>  
