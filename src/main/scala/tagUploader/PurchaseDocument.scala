package tagUploader

import org.bson.types.ObjectId
import org.mongodb.scala.bson.conversions.Bson

case class PurchaseDocument (
                              var _id: ObjectId,
                              var documentNum: String,
                              var documentImg: Bson,
                              var associatedTags: List[String],
                              var filingDate: Long
                       ) {}

object PurchaseDocument {
  def apply(documentNum: String, documentImg: Bson, associatedTags: List[String], filingDate: Long)
  : PurchaseDocument =
    new PurchaseDocument(new ObjectId, documentNum, documentImg, associatedTags, filingDate)
}
