package tagUploader

import org.bson.types.ObjectId

case class TaggedItem (
                        var _id: ObjectId = new ObjectId(),
                        var tagNum: String = "",
                        var loc_bldg: String = "",
                        var loc_flr: Byte = 0,
                        var loc_room: String = "",
                        var make: String = "",
                        var model: String = "",
                        var serialNum: String = "",
                        var purchaseDate: Long = 0L,
                        var taggingDate: Long = 0L,
                        var purchasingDoc: String = "",
                        var deptContact: Byte = 0,
                        var owner: String = "",
                        var federalProp: Boolean = false,
                        var comment: String = ""
                 ) {}
object TaggedItem {
  def apply(tagNum: String, loc_bldg: String, loc_flr: Byte, loc_room: String, make: String,
            model: String, serialNum: String, purchaseDate: Long, taggingDate: Long, purchasingDoc: String,
            deptContact: Byte, owner: String, federalProp: Boolean, comment: String): TaggedItem =
            new TaggedItem(new ObjectId(), tagNum, loc_bldg, loc_flr, loc_room, make, model, serialNum, purchaseDate,
                            taggingDate, purchasingDoc, deptContact, owner, federalProp, comment)

}
