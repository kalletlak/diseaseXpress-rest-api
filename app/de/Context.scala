package de

case class Context(
    val url: String, val size: Int = 10) {

}
// ---------------------------------------------------------------------------
object Context {

  def apply(playConf: play.api.Configuration): Context = {
    val url = playConf.getString("disease-express.elastic-search.url").get
    val size = playConf.getString("disease-express.elastic-search.size").get.toInt

    Context(url, size)
  }

}
