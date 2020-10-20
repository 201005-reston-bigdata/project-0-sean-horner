package tagUploader

import java.io.FileNotFoundException
import java.text.SimpleDateFormat
import java.util.InputMismatchException

import scala.io.StdIn
import scala.util.matching.Regex
import org.bson.types.ObjectId

/** CLI that interacts with the user in the TagUploader program */
class Cli {
  val dao = new TaggingDao()

  /** commandArgPattern is regex that we get as a command and  arguments to that command from user input */
  val commandArgPattern : Regex = "(\\w+)\\s*(.*)".r

  def printWelcome(): Unit = {
    println("Welcome to the Inventory Tagging Program (ITP)")
  }

  def printOptions(): Unit = {
    println("**********************************************************")
    println("CSV  [filename]: upload multiple tagged items by CSV")
    println("Manual         : manually fill out an item's data")
    println("List Items     : list all items currently in the database")
    println("Exit           : close the TIU program")
    println("**********************************************************")
    println("Please enter an option:")
    println("")
  }

  /** Runs the menu, prompting the user what they would like to do */
  def menu(): Unit = {
    printWelcome()
    var continueMenuLoop = true

    // this loop will repeatedly prompt, listen, run code, and repeat
    while (continueMenuLoop) {
      // print menu options
      printOptions()
      // get user choice of menu item with StdIn.readLine()
      StdIn.readLine() match {
        case commandArgPattern(cmd, arg) if cmd.equalsIgnoreCase("CSV") =>
          try {
            println("How many header rows (non-data rows at the top) are there?")
            val headers = StdIn.readLine().toInt
            csvParser(arg, headers)
          } catch {
            case fnf: FileNotFoundException => println(s"Failed to find file $arg.")
            case imm: InputMismatchException => println("Please enter an integer for the number of headers.")
          }
        case commandArgPattern(cmd, arg) if cmd.equalsIgnoreCase("manual") =>
          val addingTag = new TaggedItem()
          var str = ""
          var checked = ""
          var timeStamp: Long = 0

          println("Manual data entry mode initiated.")
          println("What is the tag number? (6-character alpha-numeric)")
          addingTag.tagNum = StdIn.readLine()

          println("What is the building code?")
          str = StdIn.readLine()
          checked = isBldg(str)
          addingTag.loc_bldg = checked

          println("What floor is the item on?")
          addingTag.loc_flr = StdIn.readLine().toByte

          println("What room is it in?")
          addingTag.loc_room = StdIn.readLine()

          println("What is the item's make?")
          addingTag.make = StdIn.readLine()

          println("What is the item's model?")
          addingTag.model = StdIn.readLine()

          println("What is the serial number?")
          addingTag.serialNum = StdIn.readLine()

          println("What is the purchase date? (mm/dd/yyyy format)")
          str = StdIn.readLine()
          timeStamp = toDate(str)
          if (timeStamp < toDate("01/01/1971"))
            println(s"$str seems too early.")
          else if (timeStamp > System.currentTimeMillis())
            println(s"$str is past today.")
          else
            addingTag.purchaseDate = timeStamp

          println("What is the tagging date? (mm/dd/yyyy format)")
          str = StdIn.readLine()
          addingTag.taggingDate = toDate(str)

          println("What is the purchase document number?")
          addingTag.purchasingDoc = StdIn.readLine()

          println("What is the department inventory rep's ID?")
          addingTag.deptContact = StdIn.readLine().toByte

          println("Who is the owner?")
          addingTag.owner = StdIn.readLine()

          println("Is this federal property? (T/F)")
          str = StdIn.readLine()
          if (str.equalsIgnoreCase("true"))
            addingTag.federalProp = true
          else if (str(0) == 't' || str(0) == 'T')
            addingTag.federalProp = true
          else
            addingTag.federalProp = false

          println("Any location/item comments?")
          addingTag.comment = StdIn.readLine()

          dao.addOne(addingTag)
        case commandArgPattern(cmd, arg) if cmd.equalsIgnoreCase("list") => dao.listAllItems()
        case commandArgPattern(cmd, arg) if cmd.equalsIgnoreCase("exit") => continueMenuLoop = false
        case notRecognized => println(s"$notRecognized not a recognized command")
      }
    }
  }

  /** First argument is the path to the file to be processed, the second is the number of header rows to skip. */
  def csvParser(filename: String, headers: Int): Unit = {
    println("parser started!")
    // Open the file as a source
    val source = io.Source.fromFile(filename)
    println("source linked, starting loop")
    // Pull each line, one by one, from the CSV, split it into a List of Strings at each ",", remove all white
    // space from before or after the value, convert Dates into milliseconds, and copy it into the ArrayBuffer.
    for (item <- source.getLines.drop(headers)) {
      val add = item.split(",").map(_.trim)
      val taggedItem: TaggedItem = new TaggedItem(new ObjectId, add(0), add(1), add(2).toByte, add(3), add(4), add(5),
        add(6), toDate(add(7)), toDate(add(8)), add(9), add(10).toByte, add(11), add(12).toBoolean, add(13))
      dao.addOne(taggedItem)
    }
  }

  def toDate(str: String): Long = {
    var res: Long = 1L

    // Create the three acceptable formats
    val format1 = new SimpleDateFormat("MM/dd/yyyy")
    val format2 = new SimpleDateFormat("MM-dd-yyyy")
    val format3 = new SimpleDateFormat("MM.dd.yyyy")

    // If-Else cascade to parse the three most likely date formats
    if (str.contains("/")) {res = format1.parse(str).getTime}
    else if (str.contains("-")) {res = format2.parse(str).getTime}
    else if (str.contains(".")) {res = format3.parse(str).getTime}
    else {
      println(s"Couldn't understand the date format, $str")
      println("Please use the MM/DD/YYYY format with '/','-', or '.' .")
    }

    // Return the resulting Date
    res
  }

  def isBldg(bldg: String): String = {
    val buildingList: List[String] = List(
      "ACA","ADH","AFP","AHG","ANB","AND","ARC","ART","ATT","BAT","BEL","BEN","BHD","BIO","BLD","BMA","BMC","BME",
      "BOT","BRB","BRG","BTL","BUR","BWY","CAL","CBA","CCJ","CDA","CDL","CEE","CLA","CMA","CMB","CML","COM","CPE",
      "CRB","CRD","CRH","CSA","DCP","DEV","DFA","DFF","EAS","ECJ","ENS","EPS","ERC","ETC","FAC","FDF","FDH","FNT",
      "GAR","GDC","GEA","GEB","GOL","GRE","GRG","GSB","HMA","HRC","HRH","HSM","IC2","JCD","JES","JGB","JHH","JON",
      "KIN","LBJ","LCH","LFH","LLA","LLB","LLC","LLD","LLE","LLF","LTD","LTH","MAG","MAI","MBB","MEZ","MHD","MMS",
      "MNC","MRH","MSB","NEZ","NHB","NMS","NOA","NUR","PAC","PAI","PAR","PAT","PCL","PHD","PHR","POB","PPA","PPB",
      "PPE","PPL","PP1","PP2","PP3","PP4","PP5","PP6","PP7","PP8","RHD","RLM","RSC","SAC","SAG","SBS","SEA","SER",
      "SJG","SJH","SRH","SSB","SSW","STD","SUT","SWG","SZB","TCC","TMM","TNH","TRG","TSC","TSF","TSG","TTC","UA9",
      "UIL","UNB","UPB","UTA","UTC","UTX","WAG","WCH","WEL","WIN","WMB","WRW","WWH"
    )
    if (buildingList.contains(bldg)) {
      bldg
    } else {
      println(s"$bldg is not an appropriate building code, please enter an appropriate one.")
      println(buildingList.toString())
      val retry = StdIn.readLine()
      isBldg(retry)
      retry
    }
  }

}