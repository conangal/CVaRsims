#' CVaR simulations with sparklyr
#'
#' Function to estimate the CVaR and Expected Shortfall for a specified portfolio to given confidence levels using a specified number of iterations using sparklyr to achieve parallelised calculations
#' @param sc spark connection
#' @param part number of partitions to split the calculations over
#' @param port_list List containing: dataframe of the portfolio, covariance matrix, RSq vector
#' @param alpha confidence levels needed for Value-at-Risk and Expected Shortfall
#' @param N number of iterations required
#' @return prints CVaR and ES values, generates a histogram
#' @keywords portfolio cvar standalone
#' @export
#' @examples
#' cvar.spk(sc, 2, cvar.port.gen(100), c(0.975,0.99,0.995), N=1000)

cvar.spk <- function(sc, part=2, port_list, alpha=c(0.99,0.995,0.999), N=1000)
{
    library(dplyr)
    library(glue)
    library(rlang)
    library(mvtnorm)

    t0 = Sys.time()

    # Separate elements of portfolio list
    portfolio = port_list[[1]]
    sigma = port_list[[2]]
    RSq = port_list[[3]]

    # get no sectors and names from the covar matrix
    n_sector = dim(sigma)[1]
    segnames = unlist(dimnames(sigma)[1])

    n_obl = length(portfolio$PD)

    # PD and LGD amount vectors for ease of computation
    PD = portfolio$PD
    LGD_amt = portfolio$LGD * portfolio$EAD

    # define names of columns for the sector and obligor realisations
    obl_cols = paste0(rep("V",n_obl), 1:n_obl)
    sect_cols = paste0(rep("S",n_sector), 1:n_sector)

    port_sectno = match(portfolio$Sector, segnames)
    port_sect_cols = paste0(rep("S",n_obl), port_sectno)

    # get weights of idiosync part
    inv_wgts = round((1 - RSq[port_sectno])^.5, 8)
    dimnames(inv_wgts) = NULL
 
    
    ## simulate sector realisations using multi-variate random num generator
    ####################################
    
    t1 = Sys.time()
    
    # sample from multi-variate & multiply by sector weights
    sect_rvs = rmvnorm(n=N/2, mean=rep(0,n_sector), sigma=sigma, method="chol")
    R_vec = sqrt(as.vector(RSq))
    sect_rvs = t( t(sect_rvs) * R_vec )
    
    # Use antithetic sampling to increase speed and accuracy
    sect_rvs = as.data.frame(rbind(sect_rvs, -sect_rvs))
    
    colnames(sect_rvs) <- sect_cols

    td = Sys.time() - t1
    cat('\nCalculation Steps:\n',
        '\nCalc sector realisations:\t\t', 
        round(td,3), units(td) )
    
    
    ### Copy to new Spark DF
    #########################
      
    t1 = Sys.time()
    
    analyis_tbl <- sdf_copy_to(sc, sect_rvs, 
                                name = "analyis_tbl",
                                memory = TRUE, repartition = part,
                                overwrite = TRUE)
    
    td = Sys.time() - t1    
    cat('\nCopy to Spark:\t\t\t\t', round(td,3), units(td))
    
    
    ### Generate idiosyncratic random element and combine with sector

    t1 = Sys.time()

    analyis_tbl = analyis_tbl %>%
        transmute(!!!setNames(glue("{port_sect_cols}+{inv_wgts}*randn()"), obl_cols) %>%
                   lapply(parse_quosure)
                ) %>%
                    sdf_register("analyis_tbl")
    
    #print(dbplyr::sql_render(analyis_tbl))

    td = Sys.time() - t1    
    cat('\nCalc obligor realisations:\t\t', round(td,3), units(td))

   
    ### Threshold on cumulative normal prob distribution
    t1 = Sys.time()

    analyis_tbl = analyis_tbl %>%
        spark_apply(function(sdf) vapply(sdf, pnorm, sdf$V1)) %>%
            sdf_register("analyis_tbl")

    td = Sys.time() - t1    
    cat('\nCalc inv. cum. normal prob. distr:\t', 
        round(td,3), units(td))


    ##### Identify default cases
    t1 = Sys.time()

    analyis_tbl = analyis_tbl %>%
        mutate(!!!setNames(
            glue("({obl_cols}<{PD})"),
            obl_cols) %>%
                lapply(parse_quosure)
            ) %>%
                sdf_register("analyis_tbl")

    #print(dbplyr::sql_render(analyis_tbl))

    td = Sys.time() - t1    
    cat('\nCalc scenario defaults:\t\t\t', round(td,3), units(td))

    
    #### Collect results from Spark and aggregate
    t1 = Sys.time()

    port_loss = analyis_tbl %>%
                    collect() %>%
                        as.matrix()
    
    scen_PD = sum(port_loss)/(N*n_obl)
    port_loss = port_loss %*% LGD_amt
    
    td = Sys.time() - t1    
    cat('\nCollect Results from Spark:\t\t', round(td,3), units(td))
      
    
    ###### Value-at-Risk and Expected Shortfall calculations
 
    t1 = Sys.time()
   
    VAR_ES = cvar.es(port_loss, alpha)
    VAR = VAR_ES[[1]]
    ExSf = VAR_ES[[2]]
        
    td = Sys.time() - t1    
    cat('\nCalc VAR & ES:\t\t\t\t', round(td,3), units(td), '\n')
   
    
    ##### output results
    
    cvar.out(portfolio, scen_PD, port_loss, VAR, ExSf)
    
    td0 = Sys.time() - t0
    cat("\nTotal Runtime: ", round(td0,2), units(td0))

}