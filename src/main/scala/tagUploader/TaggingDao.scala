package tagUploader

import java.text.SimpleDateFormat

import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, Observable}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Updates
import org.mongodb.scala.model.Sorts
import org.mongodb.scala.model.Projections

class TaggingDao {

  // Set up and connect to the MongoDB database, assign the client, database, and collection variables
  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[TaggedItem]), MongoClient.DEFAULT_CODEC_REGISTRY)
  val client: MongoClient = MongoClient()
  val db: MongoDatabase = client.getDatabase("tagging").withCodecRegistry(codecRegistry)
  val collection: MongoCollection[TaggedItem] = db.getCollection("taggedItems")

  // helper functions for access and printing, to skip the Observable data type
  def getResults[T](obs: Observable[T]): Seq[T] = {
    Await.result(obs.toFuture(), Duration(10, SECONDS))
  }

  def printResults[T](obs: Observable[T]): Unit = {
    getResults(obs).foreach(println(_))
  }

  def listAllItems(): Unit = {
    var purDate: String = ""
    var tagDate: String = ""
    val dateFormat = new SimpleDateFormat("MM/dd/yyyy")
    val itemsList = getResults(collection.find()).toList
    for (item <- itemsList) {
      purDate = dateFormat.format(item.purchaseDate)
      tagDate = dateFormat.format(item.taggingDate)
      println(s"Tag ${item.tagNum}: ${item.make} ${item.model} SN: ${item.serialNum}")
      println(s"Location: ${item.loc_bldg} ${item.loc_room} Owner: ${item.owner} Inv. Contact: ${item.deptContact}")
      println(s"Purchased: $purDate on ${item.purchasingDoc}  Tagged: $tagDate")
      println(s"Comment: ${item.comment}")
      println()
    }

  }

  def addOne(add: TaggedItem): Unit = {
    printResults(collection.insertOne(add))
  }

  def addMany(adds: List[TaggedItem]): Unit = {
    printResults(collection.insertMany(adds))
  }

  def exists(searchItem : TaggedItem): Unit = {
    printResults(collection.find(Filters.equal("tagNum", searchItem.tagNum)))
  }

  def exists(searchItem : String): Unit = {
    printResults(collection.find(Filters.equal("tagNum", searchItem)))
  }

  def update(updateItem: TaggedItem): Unit = {
    printResults(collection.replaceOne(Filters.equal("tagNum", updateItem.tagNum), updateItem))
  }

//  def removeOne(del: TaggedItem): Unit = {
//    printResults(collection.deleteOne(del))
//  }
//
//  def removeMany(dels: List[TaggedItem]): Unit = {
//    for (item <- dels)
//    printResults(collection.deleteOne(item))
//  }

}
