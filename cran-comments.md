## R CMD check results

0 errors | 0 warnings | 1 note

* Renamed the existing LifeInsuranceContracts package to LifeInsureR
  So this is not a complately new package, just a new name!
  
* Re-submission of the package taking into account the response of the CRAN team:
  * Wrap Software and package names in quotes
  * There are no external references describing the methods
  * Removed Umlaute (UTF8 characters) to prevent unexecutable code on pure ANSI/ASCII machines
  * replace \dontrun by \donttest wherever possible.
  * Never change the options(..)
  
