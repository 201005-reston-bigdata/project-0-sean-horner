package tagUploader

import java.io.FileNotFoundException
import java.text.SimpleDateFormat
import java.util.InputMismatchException

import scala.io.StdIn
import scala.util.matching.Regex
import org.bson.types.ObjectId

import scala.Int.int2double

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
          addingTag.loc_flr = isGoodFloor(addingTag.loc_bldg, StdIn.readLine().toByte)

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
    val buildingList = List(
      "ADH","AF1","AF2","AFP","AHG","ANB","AND","ARC","ART","ASE","ATT","BAT","BEL","BEN","BGH","BHD","BIO","BLD","BMA",
      "BMC","BME","BMK","BMS","BOT","BRB","BRG","BSB","BTL","BUR","BWY","CAL","CBA","CCG","CCJ","CDA","CDL","CEE","CLK",
      "CMA","CMB","CML","COM","CPC","CPE","CRB","CRD","CRH","CS3","CS4","CS5","CS6","CS7","CSS","CT1","DCP","DEV","DFA",
      "DFF","DPI","DTB","E10","E11","E12","E13","E15","E23","E24","E25","E26","ECG","ECJ","EER","EHZ","EPS","ERC","ETC",
      "FAC","FC1","FC2","FC3","FC4","FC5","FC6","FC7","FC8","FC9","FCS","FDH","FNT","FSB","G11","G17","GAR","GDC","GEA",
      "GEB","GLT","GOL","GRC","GRE","GRF","GRP","GRS","GSB","GUG","GWB","HCG","HDB","HLB","HMA","HRC","HRH","HSM","HTB",
      "IC2","ICB","IMA","IMB","IPF","JCD","JES","JGB","JHH","JON","KIN","LAC","LBJ","LCD","LCH","LDH","LFH","LLA","LLB",
      "LLC","LLD","LLE","LLF","LS1","LTD","LTH","MAG","MAI","MB1","MBB","MEZ","MFH","MHD","MMS","MNC","MRH","MSB","MTC",
      "N24","NEZ","NHB","NMS","NUG","NUR","PA1","PA3","PA4","PAC","PAI","PAR","PAT","PB2","PB5","PB6","PCL","PH1","PH2",
      "PHD","PHR","PMA","POB","PPA","PPE","PPL","PRH","RHD","RHG","RLP","ROW","RRH","RSC","SAG","SBS","SEA","SER","SJG",
      "SJH","SMC","SOF","SRH","SSB","SSW","STD","SUT","SW7","SWG","SZB","TCC","TCP","TES","TMM","TNH","TRG","TSB","TSC",
      "TSP","TTC","UA9","UIL","UNB","UPB","UTA","UTC","UTX","VRX","WAG","WAT","WCH","WCP","WCS","WEL","WGB","WIN","WMB",
      "WWH","Z02"
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

  def isGoodFloor(bldg: String, n: Byte): Byte = {
    val bldgFlr: Map[String, Byte] = Map(
      "ADH" -> 9,"AF1" -> 1,"AF2" -> 1,"AFP" -> 1,"AHG" -> 4,"ANB" -> 3,"AND" -> 5,"ARC" -> 6,"ART" -> 5,"ASE" -> 7,
      "ATT" -> 8,"BAT" -> 5, "BEL" -> 11,"BEN" -> 5,"BGH" -> 1,"BHD" -> 6,"BIO" -> 7,"BLD" -> 7,"BMA" -> 5,"BMC" -> 8,
      "BME" -> 10,"BMK" -> 2,"BMS" -> 5, "BOT" -> 2,"BRB" -> 5,"BRG" -> 8,"BSB" -> 1,"BTL" -> 7,"BUR" -> 8,"BWY" -> 3,
      "CAL" -> 7,"CBA" -> 10,"CCG" -> 4,"CCJ" -> 4,"CDA" -> 1, "CDL" -> 6,"CEE" -> 2,"CLK" -> 1,"CMA" -> 9,"CMB" -> 10,
      "CML" -> 1,"COM" -> 1,"CPC" -> 1,"CPE" -> 7,"CRB" -> 2,"CRD" -> 4,"CRH" -> 3, "CS3" -> 2,"CS4" -> 2,"CS5" -> 2,
      "CS6" -> 5,"CS7" -> 4,"CSS" -> 1,"CT1" -> 2,"DCP" -> 3,"DEV" -> 5,"DFA" -> 5,"DFF" -> 3,"DPI" -> 4, "DTB" -> 1,
      "E10" -> 1,"E11" -> 1,"E12" -> 1,"E13" -> 1,"E15" -> 1,"E23" -> 1,"E24" -> 1,"E25" -> 1,"E26" -> 1,"ECG" -> 9,
      "ECJ" -> 14, "EER" -> 9,"EHZ" -> 1,"EPS" -> 5,"ERC" -> 6,"ETC" -> 10,"FAC" -> 6,"FC1" -> 4,"FC2" -> 2,"FC3" -> 2,
      "FC4" -> 1,"FC5" -> 2,"FC6" -> 2, "FC7" -> 1,"FC8" -> 2,"FC9" -> 1,"FCS" -> 3,"FDH" -> 2,"FNT" -> 9,"FSB" -> 1,
      "G11" -> 1,"G17" -> 1,"GAR" -> 5,"GDC" -> 7,"GEA" -> 7, "GEB" -> 5,"GLT" -> 2,"GOL" -> 5,"GRC" -> 1,"GRE" -> 7,
      "GRF" -> 1,"GRP" -> 1,"GRS" -> 1,"GSB" -> 5,"GUG" -> 6,"GWB" -> 5,"HCG" -> 6, "HDB" -> 9,"HLB" -> 7,"HMA" -> 4,
      "HRC" -> 10,"HRH" -> 5,"HSM" -> 4,"HTB" -> 5,"IC2" -> 4,"ICB" -> 1,"IMA" -> 1,"IMB" -> 1,"IPF" -> 1, "JCD" -> 28,
      "JES" -> 7,"JGB" -> 7,"JHH" -> 3,"JON" -> 7,"KIN" -> 6,"LAC" -> 6,"LBJ" -> 10,"LCD" -> 1,"LCH" -> 2,"LDH" -> 2,
      "LFH" -> 4, "LLA" -> 2,"LLB" -> 2,"LLC" -> 2,"LLD" -> 2,"LLE" -> 2,"LLF" -> 2,"LS1" -> 1,"LTD" -> 5,"LTH" -> 3,
      "MAG" -> 6,"MAI" -> 37,"MB1" -> 1, "MBB" -> 5,"MEZ" -> 6,"MFH" -> 3,"MHD" -> 5,"MMS" -> 2,"MNC" -> 3,"MRH" -> 8,
      "MSB" -> 1,"MTC" -> 1,"N24" -> 17,"NEZ" -> 10,"NHB" -> 9, "NMS" -> 7,"NUG" -> 5,"NUR" -> 6,"PA1" -> 1,"PA3" -> 1,
      "PA4" -> 2,"PAC" -> 11,"PAI" -> 7,"PAR" -> 5,"PAT" -> 9,"PB2" -> 1,"PB5" -> 1, "PB6" -> 1,"PCL" -> 7,"PH1" -> 1,
      "PH2" -> 1,"PHD" -> 6,"PHR" -> 6,"PMA" -> 19,"POB" -> 7,"PPA" -> 1,"PPE" -> 5,"PPL" -> 6,"PRH" -> 1, "RHD" -> 6,
      "RHG" -> 6,"RLP" -> 7,"ROW" -> 1,"RRH" -> 10,"RSC" -> 2,"SAG" -> 7,"SBS" -> 2,"SEA" -> 8,"SER" -> 5,"SJG" -> 8,
      "SJH" -> 7, "SMC" -> 0,"SOF" -> 1,"SRH" -> 5,"SSB" -> 7,"SSW" -> 4,"STD" -> 10,"SUT" -> 5,"SW7" -> 2,"SWG" -> 6,
      "SZB" -> 5,"TCC" -> 4,"TCP" -> 1, "TES" -> 1,"TMM" -> 6,"TNH" -> 5,"TRG" -> 6,"TSB" -> 1,"TSC" -> 6,"TSP" -> 1,
      "TTC" -> 3,"UA9" -> 4,"UIL" -> 4,"UNB" -> 5,"UPB" -> 1, "UTA" -> 10,"UTC" -> 6,"UTX" -> 5,"VRX" -> 1,"WAG" -> 6,
      "WAT" -> 2,"WCH" -> 6,"WCP" -> 7,"WCS" -> 1,"WEL" -> 7,"WGB" -> 1,"WIN" -> 4, "WMB" -> 7,"WWH" -> 4,"Z02" -> 1
    )
    var test: Byte = n
    val top: Byte = bldgFlr.getOrElse(bldg, 0)
    var continue = true

    do {
      if (test > top) {
        println(s"$test is to high, the highest floor is $top")
        println("Please enter the correct floor:")
        test = StdIn.readByte()
      } else {
        continue = false
        test
      }
    } while (continue)
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