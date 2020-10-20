package tagUploader

class Building(name: String,
               abbrev: String,
               top_flr: Int,
               rooms: List[String])
{
  def goodFloor(n: Int) : Boolean = {
    if (n < 0 && n <= top_flr) true
    else false
  }

  def goodRoom(room: String) : Boolean = {
    if (rooms.contains(room)) true
    else false
  }

  def giveName() : String = {
    s"$abbrev: $name"
  }
}
