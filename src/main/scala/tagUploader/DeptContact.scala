package tagUploader

import org.bson.types.ObjectId
import org.mongodb.scala.bson.conversions.Bson

case class DeptContact (
                       var _id: ObjectId,
                       var contactID: Byte,
                       var name: String,
                       var office_bldg: String,
                       var office_flr: Byte,
                       var office_room: String,
                       var photo: Bson
                  ) {}

object DeptContact {
  def apply(contactID: Byte, name: String, office_bldg: String, office_flr: Byte, office_room: String, photo: Bson)
  : DeptContact = new DeptContact(new ObjectId, contactID, name, office_bldg, office_flr, office_room, photo)
}
