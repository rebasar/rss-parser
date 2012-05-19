import scala.collection.JavaConversions._
import scala.xml.NodeSeq
import org.horrabin.horrorss._
import dispatch._
import dispatch.tagsoup.TagSoupHttp._

abstract class FeedProcessor {
  def processData(title: String, synopsis: String, body: String) : Unit
  def fail(message: String) : Unit
}

abstract class FeedParser extends FeedProcessor {
  def title(ns: NodeSeq): String
  def synopsis(ns: NodeSeq): String
  def body(ns: NodeSeq): String

  def processNodeSeq(ns: NodeSeq){
    processData(title(ns), synopsis(ns), body(ns))
  }

  def process(url: String){
    val doc = Http(url as_tagsouped)
    if (doc.length > 0){
      processNodeSeq(doc.elements.next)
    } else {
      fail("Document node not found")
    }
  }
}

trait NTVMSNBCParser extends FeedParser {

  def title(ns: NodeSeq) = (ns \\ "h1")(0).text
  def synopsis(ns: NodeSeq) = (ns \\ "h2")(0).text
  def body(ns: NodeSeq) = (for (node <- ns \\ "p") yield node.text).mkString("\n\n")

}

trait StdoutProcessor extends FeedProcessor {
  def processData(title: String, synopsis: String, body: String){
    println("Title: " + title)
    println("Synopsis: " + synopsis)
    println("Body: \n" + body)
    println()
  }
  def fail(message: String){
    println("ERROR: " + message)
  }
}

object RssParser {

  def processFeed(feedUrl: String){
    val parser = new RssParser(feedUrl)
    val feed = parser.load()
    println(feed.getChannel.getTitle)
    val p = new FeedParser with NTVMSNBCParser with StdoutProcessor
    for(item <- feed.getItems){
      p.process(item.getLink)
    }
  }

  def usage(){
    println("Usage: RssParser <url>")
  }

  def main(args: Array[String]){
    if(args.length < 1){
      usage()
    } else {
      processFeed(args(0))
    }
  }

}
