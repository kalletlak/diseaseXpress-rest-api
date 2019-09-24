package de.controllers

import play.filters.cors.CORSFilter

// ===========================================================================
class Filters @javax.inject.Inject()
  (gzipFilter: play.filters.gzip.GzipFilter,corsFilter: CORSFilter)
  extends play.api.http.HttpFilters {
    def filters = Seq(gzipFilter, corsFilter)
  }

// ===========================================================================
