#' CVaR simulations on a standalone machine
#'
#' Function to estimate the CVaR and Expected Shortfall for a specified portfolio to given confidence levels using a specified number of iterations
#' @param port_list List containing: dataframe of the portfolio, covariance matrix, RSq vector
#' @param alpha confidence levels needed for Value-at-Risk and Expected Shortfall
#' @param N number of iterations required
#' @return prints CVaR and ES values, generates a histogram
#' @keywords portfolio cvar standalone
#' @export
#' @examples
#' cvar.sim(cvar.port.gen(100), c(0.975,0.99,0.995), N=1000)

cvar.sim <- function(port_list, alpha=c(0.99,0.995,0.999), N=1000)
{
    library(mvtnorm)
    
    t0 = Sys.time()

    portfolio = port_list[[1]]
    sigma = port_list[[2]]
    RSq = port_list[[3]]
    
    n_obl = length(portfolio$PD)
    n_seg = length(RSq)

    # get no sectors & and names from the covar matrix
    n_sector = dim(sigma)[1]
    segnames = unlist(dimnames(sigma)[1])
    

    ## simulate sector realisations Using multi-variate random num generator
    ####################################
    
    t1 = Sys.time()
     
    # Use antithetic sampling to increase speed and accuracy
    normvars = rmvnorm(n=N/2, mean=rep(0,n_sector), sigma=sigma, method="chol")
    normvars = rbind(normvars, -normvars)
    
    # define weights for all obligors
    weights = matrix(0, n_obl, n_sector)
    
    for (i in 1:n_seg) {
        weights[,i]= (portfolio$Sector == segnames[i]) * sqrt(RSq[i])
    }
    
    # multiply sector realisations by weights for each obligor
    obl_sector = weights %*% t(normvars)
    
    td = Sys.time() - t1
    cat('\nCalculation Steps:\n',
        '\nCalc sector realisations:\t\t', 
        round(td,2), units(td) )
    
    
    ## simulate obligor realisations with random num generator
    ####################################
    
    t1 = Sys.time()
    
    ## get obligor specific random variables
    obl_idio = matrix(rnorm(N*n_obl, mean = 0, sd = 1),
                    n_obl, N
                    )
        
    # get weights of idiosync part
    inv_wgts = rowSums(weights)
    inv_wgts = (1-inv_wgts^2)^.5
    rm(weights)

    # multiply idiosyncratic risk realisations by weights
    obl_idio_w <- matrix(0,n_obl,N)
    
    for (i in 1:N) {
        obl_idio_w[,i] = obl_idio[,i] * inv_wgts
    }
    rm(obl_idio)
    
    # Add sector and idosyncratic variables
    obl_total = obl_sector + obl_idio_w
    
    rm(obl_sector, obl_idio_w)
    
    td = Sys.time() - t1    
    cat('\nCalc obligor realisations:\t\t', round(td,3), units(td))

    
    ### Get threshold on cumulative normal prob distribution

    t1 = Sys.time()
    
    obl_prob = pnorm(obl_total)
    rm(obl_total)
    
    td = Sys.time() - t1    
    cat('\nCalc inv. cum. normal prob. distr:\t', 
        round(td,3), units(td))
    
    t1 = Sys.time()
  
  
    ### Identify default cases
      
    def_flag <- matrix(0,n_obl,N)
    losses <- matrix(0,n_obl,N)
    for (i in 1:N) {
        def_flag[,i] = obl_prob[,i] < portfolio$PD
        losses[,i] = def_flag[,i] * portfolio$LGD * portfolio$EAD
    }
    
    scen_PD = sum(def_flag)/(N*n_obl)
    rm(def_flag, obl_prob)
    
    # Total portfolio losses for each scenario
    port_loss = colSums(losses)
    
    td = Sys.time() - t1    
    cat('\nScenario Loss Calculation:\t\t', round(td,3), units(td))


    ###### Value-at-Risk and Expected Shortfall calculations
 
    t1 = Sys.time()
   
    VAR_ES = cvar.es(port_loss, alpha)
    VAR = VAR_ES[[1]]
    ExSf = VAR_ES[[2]]
        
    td = Sys.time() - t1    
    cat('\nCalc VAR & ES:\t\t\t\t', round(td,3), units(td), '\n')
    
    
    ###### Outputs results

    cvar.out(portfolio, scen_PD, port_loss, VAR, ExSf)
    
    td0 = Sys.time() - t0
    cat("\nTotal Runtime: ", round(td0,2), units(td0))
    
}