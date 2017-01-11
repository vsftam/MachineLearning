package dataanalyser

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URI;
import scala.io.Source
import scala.collection.mutable.ListBuffer


trait InternetDataSource[T] {
  
	def retrieveData(address: URI) : List[T] = {
	  val list = ListBuffer[T]()
	  val reader = Source.fromInputStream(address.toURL.openStream)
	  val content = reader.getLines.drop(1) // skip header
	  for(line <- content) {	
	    parseline(line) match {
	      case Some(data) => list += data
	      case None => println("cannot parse line: "+ line)
	    }
	  }
	  
	  list.toList
	}
	
	def parseline(line: String) : Option[T]
}