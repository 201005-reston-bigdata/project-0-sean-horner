package tagUploader

import org.bson.types.ObjectId

case class MapLocation(
                      var _id: ObjectId,
                      var bldg: String,
                      var floor: Byte,
                      var room: String
                 ) {}

object MapLocation {
  def apply(bldg: String, floor: Byte, room: String) = new MapLocation(new ObjectId, bldg, floor, room)
}
