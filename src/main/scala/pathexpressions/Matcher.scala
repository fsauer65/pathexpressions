package pathexpressions

import scala.util.matching.Regex

/**
 * A matcher that matches any of the patterns specified.
 * Usage:
 * val matcher = Matcher(List("messaging.queues.*.spooled.*", "messaging.queues.*.quota.*"))
 *
 * @param patterns
 */
case class Matcher(patterns: String*) {
  /**
   * translate a glob-style graphite path to a regex matching the entire input with groups surrounding all wildcards
   */
  private def globToRegex(s:String):Regex = s"""^${s.replace(".","\\.").replace("*", "(.*)")}$$""".r

  val rxs: Iterable[Regex] = patterns map globToRegex

  /**
   * Match a single string against all my patterns
   * @param path string to match
   * @return list of matches with the full match as well as a List with all wildcard values
   */
  def apply(path:String):Iterable[Match] = rxs.flatMap { case rx =>
    rx.findAllIn(path).matchData.flatMap (m => Range(0,m.groupCount+1) map (m.group(_))).toList match {
      case series :: groups => Some(Match(series,groups))
      case _ => None
    }
  }

  /**
   * Find the matches for the list of strings for which the wildcard values match for all inputs
   * @param paths list of paths that have to any of my patterns as long as the wildcards have identical values
   * @return a list with the matches if there are any. Empty list of there are none with matching wildcards
   */
  def apply(paths:Iterable[String]):Iterable[Match] = paths.flatMap(apply).groupBy(_.groups).filter {
    case (_,v) => v.size == patterns.size // only keep the groups for which ALL paths have a match
  }.flatMap (_._2)
}
