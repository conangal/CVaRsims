#' Generic loan portfolio generator
#'
#' Function to generate a randomised generic loan portfolio of specified size.
#' @param NC Number of counterparties. Default is 100.
#' @return Outputs a list of 3 elements: portfolio dataframe, covar matrix and R-Squared correlations vector
#' @keywords portfolio cvar list
#' @export
#' @examples
#' cvar.port.gen(125)

cvar.port.gen <- function(NC=100)
{

    # typical selection based on Barclay's internal scale
    CP.pd_rate <- array(c(0.01, 0.03, 0.04, 0.08, 0.13,
                          0.18, 0.23, 0.28, 0.35, 0.45,
                          0.55, 0.90, 1.38, 1.85, 2.60,
                          3.75, 5.40, 7.50, 10.00, 15.00,
                          30.00)/100,
                        dim = 21,
                        dimnames = list(1:21)
                    )

    ########### Generate Randomised portfolio ##########

    # define number of loan sectors and labels
    sector_names <- c("Oil & Gas",
    "Basic Materials",
    "Industrials",
    "Consumer Goods",
    "Health Care",
    "Consumer Services",
    "Telecommunications",
    "Utilities",
    "Financials",
    "Technology")

    n_sector = length(sector_names)

    #assign business lines and countries randomly
    CP.sector = sector_names[ceiling(runif(NC,0,length(sector_names)))]

    # define R-sq correlation factor
    RSq = ceiling(runif(n_sector, 9, 20))/100
    RSq = array(RSq,dimnames = list(sector_names))

    # Define covariance matrix
    sigma <- matrix(rep(0.7,n_sector^2), ncol=n_sector)
    diag(sigma) = rep(1,n_sector)
    dimnames(sigma) = list(sector_names,sector_names)

    ### Lognormal sampling distribution for EAD
    ave_EAD = 5*10^6
    sigma_EAD = 1
    mu_EAD = log(ave_EAD) - sigma_EAD^2/2

    
    ### Beta distribution sampling  for LGD
    ave_LGD = 0.6
    ave_RR = 1 - ave_LGD
    var_RR = (ave_RR*(1-ave_RR))^2

    alpha_RR = ave_RR *(ave_RR*(1-ave_RR)/var_RR - 1)
    beta_RR = alpha_RR * (1-ave_RR)/ave_RR

    
    #### Generate PD rating scale

    #set peak of PD grade distribution
    mode_gr = 13

    CP.pd_grd = rnorm(NC*3, mode_gr+0.5, sqrt(21))
    CP.pd_grd = as.integer(CP.pd_grd)
    CP.pd_grd = CP.pd_grd[CP.pd_grd>0]
    CP.pd_grd = CP.pd_grd[CP.pd_grd<22]
    CP.pd_grd = sample(CP.pd_grd,NC)


    #create portfolio data frame
    portfolio=data.frame(ID=1:NC,Name=paste("Company ",1:NC),Sector=CP.sector,
                         EAD=trunc(rlnorm(NC,mu_EAD,sigma_EAD)),
                         LGD=signif(rbeta(NC, alpha_RR, beta_RR),4),
                         PD_Grade = CP.pd_grd, PD=CP.pd_rate[CP.pd_grd])

    hist(CP.pd_grd, breaks=0:21)
    hist(portfolio$LGD)
    hist(portfolio$EAD)
    
    return(list(portfolio, sigma, RSq))

}

#' Basel II homogeneous mortgage loan portfolio generator
#'
#' Function to generate a homogeneous mortgage loan portfolio of specified size. For use in benchmarking CVaR model outputs vs Basel II capital calculations. Outputs a list of: portfolio dataframe, covar matrix and R-Squared correlations
#' @param NC Number of counterparties. Default is 100
#' @keywords portfolio cvar list
#' @export
#' @examples
#' cvar.port.gen()


cvar.port.b2mort <- function(NC = 100)
{

    mort_sec <- c("Mortgage")

    # define R-sq correlation factor
    mort_rsq = c(0.15) #Basel mortgage correlation
    mort_rsq = array(mort_rsq, dimnames = list(mort_sec))

    # Define covariance matrix
    mort_sig <- matrix(1)
    dimnames(mort_sig) = list(mort_sec,mort_sec)

    #create portfolio data frame
    mort_b2 = data.frame(ID=1:NC,Name=paste("Cust ",1:NC),
                        Sector=rep(mort_sec, NC),
                        EAD=rep(100000, NC),
                        LGD=rep(0.1, NC),
                        PD=rep(.01,NC))

    return(list(mort_b2, mort_sig, mort_rsq))

}

#' Basel II homogeneous revolving loan portfolio generator
#'
#' Function to generate a homogeneous revolving loan portfolio of specified size. For use in benchmarking CVaR model outputs vs Basel II capital calculations. Outputs a list of: portfolio dataframe, covar matrix and R-Squared correlations
#' @param NC Number of counterparties. Default is 100
#' @keywords portfolio cvar list
#' @export
#' @examples
#' cvar.port.gen()

cvar.port.b2cc <- function(NC = 100)
{

    cc_sec <- c("CreditCard")

    # define R-sq correlation factor
    cc_rsq = c(0.04) #Basel correlation
    cc_rsq = array(cc_rsq,dimnames = list(cc_sec))

    # Define covariance matrix
    cc_sig <- matrix(1)
    dimnames(cc_sig) = list(cc_sec,cc_sec)

    #create portfolio data frame
    cc_b2 = data.frame(ID=1:NC,Name=paste("Cust ",1:NC),
                         Sector=rep(cc_sec, NC),
                         EAD=rep(1000, NC),
                         LGD=rep(0.4, NC),
                         PD=rep(.03,NC))

    return(list(cc_b2, cc_sig, cc_rsq))

}
