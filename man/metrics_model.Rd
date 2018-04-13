\name{metrics_model}
\alias{metrics_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives a PMax, Wbal and mFTP estimations from a given training profile.
}
\description{
From a mean maximum power file, containing the best power marks during a certain period, this function is able to calculate the Pmax, WBal and mFTP estimations.
}
\usage{
metrics_model(mmp_file, wpk = F, weight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mmp_file}{
A vector containing the mean maximum power for each duration.
}
  \item{wpk}{
A boolean to indicate wheter the power information are in watts (wpk=F) or watts per kilo (wpk=T).
}
  \item{weight}{
The weight of the athlete in kilos.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{pmax}{The maximum power estimation, in watts.}
\item{wbal}{The anaerobic capacity estimation, in Joules.}
\item{mftp}{The modeled functional threshold power estimation, in watts.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Power Profiling - R graph on Golden Cheetah, by "fabrylama".
}
\author{
Natan Freitas Leite.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
function (mmp_file, wpk = F, weight)
{
    require(changepoint)
    if (!wpk) {
        mmp_file = mmp_file/weight
    }
    if (mmp_file[1] == 0) {
        mm = mmp_file[-1]
    }
    else {
        mm = mmp_file
    }
    i1 <- 15
    i2 <- 90
    i3 <- 120
    i4 <- 300
    i5 <- 600
    i6 <- 3000
    i7 <- 4000
    i8 <- 40000
    if (i7 > length(mm)) {
        i7 <- (length(mm) - 1)
    }
    if (i8 > length(mm)) {
        i8 <- length(mm)
    }
    paa = 15
    etau = 1
    ecp = 5
    paa_dec = -2
    ecp_del = -0.9
    tau_del = -4.8
    ecp_dec = -1
    ecp_dec_del = -180
    paa_pow = 1.05
    paa_min = 5
    etau_min = 0.5
    paa_dec_max = -0.25
    paa_dec_min = -3
    ecp_dec_min = -5
    etau_delta_max = 1e-04
    paa_delta_max = 0.01
    paa_dec_delta_max = 1e-04
    ecp_del_delta_max = 1e-04
    ecp_dec_delta_max = 1e-08
    max_loops = 100
    iteration = 0
    repeat {
        iteration <- iteration + 1
        if (iteration > max_loops) {
            break
        }
        etau_prev = etau
        paa_prev = paa
        paa_dec_prev = paa_dec
        ecp_del_prev = ecp_del
        ecp_dec_prev = ecp_dec
        ecp = 0
        avg_ecp = 0
        count = 1
        for (i in i5:i6) {
            ecpn = (mm[i] - paa * exp(paa_dec * ((i/60)^(paa_pow))))/(1 -
                exp(tau_del * i/60))/(1 - exp(ecp_del * i/60))/(1 +
                ecp_dec * exp(ecp_dec_del/(i/60)))/(1 + etau/(i/60))
            avg_ecp = ((count - 1) * avg_ecp + ecpn)/count
            if (ecp < ecpn) {
                ecp = ecpn
            }
            count <- count + 1
        }
        etau = etau_min
        avg_etau = 0
        count = 1
        for (i in i3:i4) {
            etaun = ((mm[i] - paa * exp(paa_dec * ((i/60)^(paa_pow))))/ecp/(1 -
                exp(tau_del * i/60))/(1 - exp(ecp_del * i/60))/(1 +
                ecp_dec * exp(ecp_dec_del/(i/60))) - 1) * (i/60)
            avg_etau = ((count - 1) * avg_etau + etaun)/count
            if (etau < etaun) {
                etau = etaun
            }
            count <- count + 1
        }
        paa_dec = paa_dec_min
        avg_paa_dec = 0
        count = 1
        for (i in i1:i2) {
            paa_decn = log((mm[i] - ecp * (1 - exp(tau_del *
                i/60)) * (1 - exp(ecp_del * i/60)) * (1 + ecp_dec *
                exp(ecp_dec_del/(i/60))) * (1 + etau/(i/60)))/paa)/((i/60)^(paa_pow))
            avg_paa_dec = ((count - 1) * avg_paa_dec + paa_decn)/count
            if (is.na(paa_decn)) {
                paa_decn <- paa_dec
            }
            else {
                if ((paa_dec < paa_decn) && (paa_decn < paa_dec_max)) {
                  paa_dec = paa_decn
                }
            }
            count <- count + 1
        }
        paa = paa_min
        avg_paa = 0
        count = 1
        for (i in 1:8) {
            paan = (mm[i] - ecp * (1 - exp(tau_del * i/60)) *
                (1 - exp(ecp_del * i/60)) * (1 + ecp_dec * exp(ecp_dec_del/(i/60))) *
                (1 + etau/(i/60)))/exp(paa_dec * ((i/60)^paa_pow))
            avg_paa = ((count - 1) * avg_paa + paan)/count
            if (paa < paan) {
                paa = paan
            }
            count <- count + 1
        }
        if (avg_paa < 0.95 * paa) {
            paa = avg_paa
        }
        ecp_dec = ecp_dec_min
        avg_ecp_dec = 0
        count = 1
        for (i in seq(i7, i8, 120)) {
            ecp_decn = ((mm[i] - paa * exp(paa_dec * ((i/60)^paa_pow)))/ecp/(1 -
                exp(tau_del * i/60))/(1 - exp(ecp_del * i/60))/(1 +
                etau/(i/60)) - 1)/exp(ecp_dec_del/(i/60))
            avg_ecp_dec = ((count - 1) * avg_ecp_dec + ecp_decn)/count
            if (ecp_decn > 0) {
                ecp_decn = 0
            }
            if (ecp_dec < ecp_decn) {
                ecp_dec = ecp_decn
            }
            count <- count + 1
        }
        if (!((abs(etau - etau_prev) > etau_delta_max) || (abs(paa -
            paa_prev) > paa_delta_max) || (abs(paa_dec - paa_dec_prev) >
            paa_dec_delta_max) || (abs(ecp_del - ecp_del_prev) >
            ecp_del_delta_max) || (abs(ecp_dec - ecp_dec_prev) >
            ecp_dec_delta_max))) {
            break
        }
    }
    pMax = paa * exp(paa_dec * ((1/60)^paa_pow)) + ecp * (1 -
        exp(tau_del * (1/60))) * (1 - exp(ecp_del * (1/60))) *
        (1 + ecp_dec * exp(ecp_dec_del/(1/60))) * (1 + etau/(1/60))
    mmp60 = paa * exp(paa_dec * ((60)^paa_pow)) + ecp * (1 -
        exp(tau_del * (60))) * (1 - exp(ecp_del * 60)) * (1 +
        ecp_dec * exp(ecp_dec_del/60)) * (1 + etau/(60))
    xemmp <- 1:620
    yemmp <- 1:620
    for (i in 1:620) {
        t <- 5 * 10^(i/210) - 3
        yemmp[i] <- paa * exp(paa_dec * (((t/60)^paa_pow))) +
            ecp * (1 - exp(tau_del * (t/60))) * (1 - exp(ecp_del *
                (t/60))) * (1 + ecp_dec * exp(ecp_dec_del/(t/60))) *
                (1 + etau/((t/60)))
        xemmp[i] <- t
    }
    myts = ts(yemmp, start = c(1), end = c(length(yemmp)), frequency = 1)
    disc = cpt.mean(myts, penalty = "Manual", pen.value = "log(n)",
        method = "PELT")
    low_avg = which((disc@cpts > sum(xemmp < i5) + 1) & (disc@cpts <
        sum(xemmp <= i7)))[1]
    if (is.na(low_avg)) {
        low_avg = length(disc@cpts) - 1
    }
    mftp = mean(yemmp[disc@cpts[low_avg]:sum(xemmp <= i7)]) *
        weight
    wbal = ecp * etau * 60 * weight
    pmax = pMax * weight
    return(c(pmax, wbal, mftp))
  }

  data(mmp_alldata)

  metrics=metrics_model(mmp_alldata[[1]],F,65)
  metrics

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
