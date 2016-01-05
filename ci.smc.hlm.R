ci.smc.hlm = function(table, conf = 0.95){
    #table is a matrix from a summary.MerMod object
    #conf is the interval for the CI and can be specified by the user, default is .95
    
    coeffs = dim(table)[1] #get the number of model coefficients
    oldnames = colnames(table)
    
    require(MBESS) #load the package required for finding the critical t values
    d = d.lower = d.upper = matrix(0, coeffs, 1) #preallocate
    for (n in 1:coeffs){
        d[n] = 2*table[n, 4]/sqrt(table[n, 3])
        holder = conf.limits.nct(ncp = table[n, 4], df = table[n, 3], conf.level = conf)
        d.lower[n] = 2*holder$Lower.Limit/sqrt(table[n, 3])
        d.upper[n] = 2*holder$Upper.Limit/sqrt(table[n, 3])
    }
    table = cbind(table, d, d.lower, d.upper)
    colnames(table) = c(oldnames, 'd', 'd.lower', 'd.upper')
    return(table)
}