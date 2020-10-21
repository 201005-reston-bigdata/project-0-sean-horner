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
    println("***********************************************************")
    println("CSV  [filename]: upload multiple tagged items by CSV")
    println("Manual         : manually fill out an item's data")
    println("List Items     : list all items currently in the database")
    println("Delete All     : delete all items currently in the database")
    println("Remove [Tag #] : remove item with specified tag number")
    println("Exit           : close the ITP")
    println("***********************************************************")
    println("Please enter an option:  ")
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

          println("Manual data entry mode initiated.")
          println("What is the tag number?")
          addingTag.tagNum = StdIn.readLine()

          println("What is the building code?")
          addingTag.loc_bldg = isGoodBldg(StdIn.readLine())

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
          addingTag.purchaseDate = toDate(isGoodDate(StdIn.readLine()))

          println("What is the tagging date? (mm/dd/yyyy format)")
          addingTag.taggingDate = toDate(isGoodDate(StdIn.readLine()))

          println("What is the purchase document number?")
          addingTag.purchasingDoc = StdIn.readLine()

          println("What is the department inventory rep's ID?")
          addingTag.deptContact = StdIn.readLine().toByte

          println("Who is the owner?")
          addingTag.owner = StdIn.readLine()

          println("Is this federal property? (T/F)")
          addingTag.federalProp = isTrue(StdIn.readLine())

          println("Any location/item comments?")
          addingTag.comment = StdIn.readLine()

          dao.addOne(addingTag)
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("list") =>
            dao.listAllItems()
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("delete") && arg.equalsIgnoreCase("all") =>
            println("Are you sure you want to delete all database items? (T/F):")
            if(isTrue(StdIn.readLine()))
              dao.removeAll()
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("remove") =>
            dao.removeOne(arg)
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("exit") =>
            continueMenuLoop = false
        case notRecognized => println(s"$notRecognized not a recognized command")
      }
    }
  }

  /** First argument is the path to the file to be processed, the second is the number of header rows to skip. */
  def csvParser(filename: String, headers: Int): Unit = {
    // Open the file as a source
    val source = io.Source.fromFile(filename)
    // Pull each line, one by one, from the CSV, split it into a List of Strings at each ",", remove all white
    // space from before or after the value, convert Dates into milliseconds, and copy it into the ArrayBuffer.
    for (item <- source.getLines.drop(headers)) {
      if (item.length > 2) {
        val add = item.split(",").map(_.trim)
        val taggedItem: TaggedItem = new TaggedItem(new ObjectId, add(0), add(1), add(2).toByte, add(3), add(4), add(5),
          add(6), toDate(add(7)), toDate(add(8)), add(9), add(10).toByte, add(11), isTrue(add(12)), add(13))
        dao.addOne(taggedItem)
      } else
        println(s"Too little data for line, moving on.")
    }
  }

  def toDate(str: String): Long = {
    // Create the six acceptable formats: three split characters, two styles each.
    val format1a = new SimpleDateFormat("MM/dd/yyyy")
    val format1b = new SimpleDateFormat("yyyy/MM/dd")
    val format2a = new SimpleDateFormat("MM-dd-yyyy")
    val format2b = new SimpleDateFormat("yyyy-MM-dd")
    val format3a = new SimpleDateFormat("MM.dd.yyyy")
    val format3b = new SimpleDateFormat("yyyy.MM.dd")

    // if-else cascade to parse the six most likely date formats and checks if the given date is past now.
    if (str.indexOf('/') == 2 && (format1a.parse(str).getTime < System.currentTimeMillis()))
      format1a.parse(str).getTime
    else if (str.indexOf('/') == 4 && (format1b.parse(str).getTime < System.currentTimeMillis()))
      format1b.parse(str).getTime
    else if (str.indexOf('-') == 2 && (format2a.parse(str).getTime < System.currentTimeMillis()))
      format2a.parse(str).getTime
    else if (str.indexOf('-') == 4 && (format2b.parse(str).getTime < System.currentTimeMillis()))
      format2b.parse(str).getTime
    else if (str.indexOf('.') == 2 && (format3a.parse(str).getTime < System.currentTimeMillis()))
      format3a.parse(str).getTime
    else if (str.indexOf('.') == 4 && (format3b.parse(str).getTime < System.currentTimeMillis()))
      format3b.parse(str).getTime
    else {
      println(s"Couldn't understand the date format, $str")
      println("Please enter the date in the MM/DD/YYYY format:")
      format1a.parse(StdIn.readLine()).getTime
    }
  }

  def isGoodBldg(str: String): String = {
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
    var continue = true
    var bldg = str.toUpperCase()

    do {
      if (buildingList.contains(bldg))
        continue = false
      else {
        println(s"$bldg is not an appropriate building code, please enter an appropriate one.")
        println(buildingList.toString())
        bldg = StdIn.readLine().toUpperCase()
      }
    } while (continue)

    bldg
  }

  def isGoodDate(str: String): String = {
    var continue = true
    var date = str

    do {
      if (toDate(date) < toDate("01/01/1950")) {
        println(s"$date seems too early. Please enter the correct date:")
        date = StdIn.readLine()
      } else if (toDate(date) > System.currentTimeMillis()) {
        println(s"$date is past today. Please enter the correct date:")
        date = StdIn.readLine()
      } else
        continue = false
    } while (continue)

    date
  }

  def isTrue(str: String): Boolean = {
    if (str.equalsIgnoreCase("true") || str.head.equals('t') || str.head.equals('T'))
      true
    else
      false
  }

}