#  File R/zzz.R in package ergm.ego, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution
#
#  Copyright 2015-2020 Statnet Commons
#######################################################################
#' @import statnet.common
.onAttach <- function(lib, pkg){
  sm <- statnetStartupMessage("ergm.ego", c("statnet"), TRUE)
  if(!is.null(sm)){
    packageStartupMessage(sm)
  }
  
}

.onLoad <- function(libname, pkgname){
  # . is used as a placeholder by stantet.common::NVL3().
  utils::globalVariables(".")

  eval(COLLATE_ALL_MY_CONTROLS_EXPR)
}

#' @name ergm.ego-reexports
#' @title Reexports from other packages
#' @param ... Arguments to reexported functions.
NULL

#' @describeIn ergm.ego-reexports See [statnet.common::sctrl].
#' @param drop,init,init.method,main.method,force.main,main.hessian,checkpoint,resume,MPLE.max.dyad.types,MPLE.samplesize,init.MPLE.samplesize,MPLE.type,MPLE.nonident,MPLE.nonident.tol,MCMC.prop.weights,MCMC.prop.args,MCMC.interval,MCMC.burnin,MCMC.samplesize,MCMC.effectiveSize,MCMC.effectiveSize.damp,MCMC.effectiveSize.maxruns,MCMC.effectiveSize.base,MCMC.effectiveSize.points,MCMC.effectiveSize.order,MCMC.return.stats,MCMC.runtime.traceplot,MCMC.init.maxedges,MCMC.max.maxedges,MCMC.addto.se,MCMC.compress,MCMC.packagenames,SAN.maxit,SAN.nsteps.times,SAN,MCMLE.termination,MCMLE.maxit,MCMLE.conv.min.pval,MCMLE.NR.maxit,MCMLE.NR.reltol,obs.MCMC.samplesize,obs.MCMC.interval,obs.MCMC.burnin,obs.MCMC.burnin.min,obs.MCMC.prop.weights,obs.MCMC.prop.args,obs.MCMC.impute.min_informative,obs.MCMC.impute.default_density,MCMLE.MCMC.precision,MCMLE.MCMC.max.ESS.frac,MCMLE.metric,MCMLE.method,MCMLE.trustregion,MCMLE.dampening,MCMLE.dampening.min.ess,MCMLE.dampening.level,MCMLE.steplength.margin,MCMLE.steplength,MCMLE.steplength.parallel,MCMLE.adaptive.trustregion,MCMLE.sequential,MCMLE.density.guard.min,MCMLE.density.guard,MCMLE.effectiveSize,MCMLE.last.boost,MCMLE.steplength.esteq,MCMLE.steplength.miss.sample,MCMLE.steplength.maxit,MCMLE.steplength.min,MCMLE.effectiveSize.interval_drop,MCMLE.save_intermediates,MCMLE.nonident,MCMLE.nonident.tol,SA.phase1_n,SA.initial_gain,SA.nsubphases,SA.niterations,SA.phase3_n,SA.trustregion,RM.phase1n_base,RM.phase2n_base,RM.phase2sub,RM.init_gain,RM.phase3n,Step.MCMC.samplesize,Step.maxit,Step.gridsize,CD.nsteps,CD.multiplicity,CD.nsteps.obs,CD.multiplicity.obs,CD.maxit,CD.conv.min.pval,CD.NR.maxit,CD.NR.reltol,CD.metric,CD.method,CD.trustregion,CD.dampening,CD.dampening.min.ess,CD.dampening.level,CD.steplength.margin,CD.steplength,CD.adaptive.trustregion,CD.adaptive.epsilon,CD.steplength.esteq,CD.steplength.miss.sample,CD.steplength.maxit,CD.steplength.min,CD.steplength.parallel,loglik,term.options,seed,parallel,parallel.type,parallel.version.check,nsteps,GF.init.maxedges.mul,nsim,network.output,warn.dyads,SAN.tau,SAN.invcov,SAN.invcov.diag,SAN.nsteps.alloc,SAN.nsteps,SAN.samplesize,SAN.init.maxedges,SAN.max.maxedges,SAN.prop.weights,SAN.prop.args,SAN.packagenames,SAN.ignore.finite.offsets,ppopsize,ppopsize.mul,ppop.wt,stats.wt,stats.est,boot.R,ergm,simulate Arguments to the `control` functions.
#' @export
sctrl <- statnet.common::sctrl

eval(UPDATE_MY_SCTRL_EXPR)
