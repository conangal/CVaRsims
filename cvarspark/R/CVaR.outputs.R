#' CVAR and Expected Shortfall loss calculation
#'
#' Function to calculate Value-at-Risk and Expected Shortfall 
#' @param port_loss vector of scenario total portfolio loss amounts
#' @param alpha vector of confidence levels for VAR and ES calculation. Defaults to (99%, 99.5%, 99.9%)
#' @return list with a vector VAR and ES with a value relating to each alpha
#' @keywords portfolio CVAR ES
#' @export
#' @examples
#' cvar.es(c(0, 1000, 5000), c(0.95, 0.99))

cvar.es <- function(port_loss, alpha=c(0.99,0.995,0.999))
{

    VAR = quantile(port_loss, probs = alpha, na.rm = FALSE, names = TRUE, type = 7)

    # Expected Shortfall values for given alpha
    n_alpha = length(alpha)
    ExSf = array(0, n_alpha)
    names(ExSf) = names(VAR)
    for (i in 1:n_alpha) {
        # Get mean of losses >= corresponding VAR
        ExSf[i] = mean(port_loss[port_loss >= VAR[i]])
    }
    out = list(VAR, ExSf)
    return(out)

}


#' Print and graph outputs of the CVAR model
#'
#' Function to generate printed and graph outputs of the CVAR model
#' @param portfolio input portfolio
#' @param scen_PD mean default rate over all scenarios
#' @param port_loss vector of losses across all scenarios
#' @param VAR value at risk
#' @param ExSf Expected Shortfall
#' @return prints outputs and histogram
#' @keywords portfolio cvar list
#' @export
#' @examples
#' cvar.out(port, 0.02, port_loss, VAR, ES)

cvar.out <- function(portfolio, scen_PD, port_loss, VAR, ExSf)
{
    EAD_total = sum(portfolio$EAD)
    max_loss = max(port_loss)

    # Calculate EL analytically
    port_EL_act = sum(portfolio$PD * portfolio$LGD * portfolio$EAD)

    # Mean losses across scenarios
    port_EL_sc =  mean(port_loss)

    ### VAR and Expected Shortfall Outputs
    cat("\n\nLoss Calculation Outputs: \n")

    cat("\nScenario vs. Actual Ave. PD:\n\t",
        signif(scen_PD*100, 3),
        "% vs. ", signif(mean(portfolio$PD)*100, 3), "% \n",
        sep="")

    cat("\nScenario vs. Actual Ave. % EL:\n\t",
        signif(port_EL_sc/EAD_total*100, 3),
        "% vs. ", signif(port_EL_act/EAD_total*100, 3), "%\n",
        sep = "")
    
    if(port_EL_sc > port_EL_act) {sign = "+"} 
    else {sign = ""}
        
    cat("\nEL Amount - Scenario vs. Actual % Difference: ",
        sign, signif((port_EL_sc / port_EL_act - 1) * 100, 3), "%\n",
        sep="")

    cat("\nStd Dev. of Losses:", round(sd(port_loss)),"\n")

    cat("\nVaR / EAD: \n")
    print(signif(VAR/EAD_total,4))

    cat("\nExpected Shortfall / EAD: \n")
    print(signif(ExSf/EAD_total,4))
    
    cat("\nHighest loss in all Scenarios:\n\t",
        round(max_loss), " / ",
        round(max_loss/EAD_total*100,4), "%\n",
        sep = "")    
        
   
    library(ggplot2)

    var_es_df = data.frame(CL=names(VAR),
                            VAR=VAR/10^6, 
                            ES = ExSf/10^6, 
                            VAR_perc=VAR/EAD_total*100, 
                            ES_perc = ExSf/EAD_total*100)

    pl_df = data.frame(port_loss_M = port_loss/10^6, 
                        loss_perc = (port_loss/EAD_total*100) )
    
    p1 <- ggplot(pl_df, aes(x=port_loss_M), environment = environment()) +
        geom_density() +
        geom_vline(data=var_es_df, aes(xintercept=(VAR),  colour=CL), linetype="dashed", size=1) +
        ylab("Probability Density") +
        xlab("Loss Amount (M)")        
    print(p1 + ggtitle("Portfolio Loss Distribution with VAR Levels") ) 
    

    p2 <- ggplot(pl_df, aes(x=port_loss_M), environment = environment()) +
        geom_density() +
        geom_vline(data=var_es_df, aes(xintercept=(ES),  colour=CL), linetype="dashed", size=1) +
        ylab("Probability Density") +
        xlab("Loss Amount (M)")   
    print(p2 + ggtitle("Portfolio Loss Distribution with Expected Shortfall Levels") ) 
    
    p3 <- ggplot(pl_df, aes(x=loss_perc), environment = environment()) +
        geom_density() +
        geom_vline(data=var_es_df, aes(xintercept=(VAR_perc),  colour=CL), linetype="dashed", size=1) +
        ylab("Probability Density") +
        xlab("Loss as % of Total (%)") 
    print(p3 + ggtitle("Portfolio Loss Rate Distribution with VAR Levels") ) 
    
    
    p4 <- ggplot(pl_df, aes(x=loss_perc), environment = environment()) +
        geom_density() +
        geom_vline(data=var_es_df, aes(xintercept=(ES_perc),  colour=CL), linetype="dashed", size=1) +
        ylab("Probability Density") +
        xlab("Loss as % of Total (%)")        
    print(p4 + ggtitle("Portfolio Loss Rate Distribution with Expected Shortfall Levels") ) 
    
}



