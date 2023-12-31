% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportInsuranceContractExample.R
\name{exportInsuranceContractExample}
\alias{exportInsuranceContractExample}
\title{Export the example calculations of an insurance contract}
\usage{
exportInsuranceContractExample(
  contract,
  prf = 10,
  outdir = ".",
  basename = NULL,
  extraname = NULL,
  ...
)
}
\arguments{
\item{contract}{The \code{\link{InsuranceContract}} object to be exported}

\item{prf}{The time of the premium waiver}

\item{outdir}{The output directory (the file names are not configurable)}

\item{basename}{The base output filename (sans .xlsx). If missing, a name of
the form 2020-08-01_TARIFNAME_EXTRANAME_RZ0.01_x35_YoB1977_LZ45_PrZ20_VS100000
is used. If given, the main contract without modification will be
exported to basename.xlsx, while the example with premium waiver will be
exported to basename_PremiumWaiver_t10.xlsx and the text file containing
the examples required by the LV-VMGV is exported to basename_VmGlg.txt.}

\item{extraname}{If basename is not given, this allows a suffix to distinguish
multiple exports.}

\item{...}{Further parameters (passed on to \code{\link{showVmGlgExamples}})}
}
\value{
None
}
\description{
Export the given contract to 'Excel' (full history/timeseries of all cash
flows, reserves, premiums, etc.) and to a text file (sample calculation
required by the Austrian regulation).
}
\details{
Three output files are generated:
\itemize{
\item \{DATE\}_\{TARIFF\}_Example.xlsx: Full history/timeseries
\item \{DATE\}_\{TARIFF\}_Example_PremiumWaiver_t10.xlsx: Full history/timeseries
after a premium waiver at the given time \code{prf}
\item \{DATE\}_\{TARIFF\}_Examples_VmGlg.txt: Example calculation required for the
Austrian regulation (LV-VMGLV)
}
}
\examples{
library("MortalityTables")
mortalityTables.load("Austria_Annuities_AVOe2005R")
# A trivial deferred annuity tariff with no costs:
tariff = InsuranceTarif$new(name="Test Annuity", type="annuity", tarif = "Annuity 1A",
    mortalityTable = AVOe2005R.unisex, i=0.01)
contract = InsuranceContract$new(
    tariff,
    age = 35, YOB = 1981,
    policyPeriod = 30, premiumPeriod = 15, deferralPeriod = 15,
    sumInsured = 1000,
    contractClosing = as.Date("2016-10-01")
);
\donttest{exportInsuranceContractExample(contract, prf = 10, outdir = tempdir())}

}
