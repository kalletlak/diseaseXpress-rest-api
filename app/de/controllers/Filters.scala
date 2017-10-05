package de.controllers

// ===========================================================================
class Filters @javax.inject.Inject()
  (gzipFilter: play.filters.gzip.GzipFilter)
  extends play.api.http.HttpFilters {
    def filters = Seq(gzipFilter)
  }

// ===========================================================================
