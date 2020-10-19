package tagUploader

import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.{MongoClient, MongoCollection, Observable}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

class TaggingDao {

  // Set up and connect to the MongoDB database, assign the client, database, and collection variables
  val codecRegistry = fromRegistries(fromProviders(classOf[TaggedItem]), MongoClient.DEFAULT_CODEC_REGISTRY)
  val client: MongoClient = MongoClient()
  val db = client.getDatabase("tagging").withCodecRegistry(codecRegistry)
  val collection: MongoCollection[TaggedItem] = db.getCollection("taggedItems")

  // helper functions for access and printing, to skip the Observable data type
  def getResults[T](obs: Observable[T]): Seq[T] = {
    Await.result(obs.toFuture(), Duration(10, SECONDS))
  }

  def printResults[T](obs: Observable[T]): Unit = {
    getResults(obs).foreach(println(_))
  }

  def listAllItems: Unit = {
    printResults(collection.find())
  }

  def addOne(add: TaggedItem) = {
    printResults(collection.insertOne(add))
  }

  def addMany(adds: List[TaggedItem]): Unit = {
    printResults(collection.insertMany(adds))
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
