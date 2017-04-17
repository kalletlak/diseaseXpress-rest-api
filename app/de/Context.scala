package de

case class Context(
    url: String,
    size: Int = 10) {

}
// ---------------------------------------------------------------------------
object Context {

  def apply(playConf: play.api.Configuration): Context = {

    Context(
      url = playConf.getString("disease-express.elastic-search.url").get,
      size = playConf.getString("disease-express.elastic-search.size").get.toInt)
  }

}
