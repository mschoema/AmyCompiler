object Dates {

    abstract class Date
    case class Day(day: [1 .. 31], month: [1 .. 12], year: Int) extends Date
    case class Month(month: [1 .. 12], year: Int) extends Date
    case class Year(year: Int) extends Date


    def intToString(i: Int): String = {
        if (i == 0) {
            "0"
        } else {
            loopIntToString(i)
        }
    }

    def loopIntToString(i: Int): String = {
        if (i == 0) {
            ""
        } else {
            val q: Int = i / 10;
            val r: [0 .. 9] =  i - q*10;
            loopIntToString(q) ++ basicIntToString(r)
        }
    }

    def basicIntToString(i: [0 .. 9]): String = {
        i match {
            case 0 =>
                "0"
            case 1 => 
                "1"
            case 2 => 
                "2"
            case 3 => 
                "3"
            case 4 => 
                "4"
            case 5 => 
                "5"
            case 6 => 
                "6"
            case 7 => 
                "7"
            case 8 => 
                "8"
            case 9 =>
                "9" 
        }
    }

    def printDate(dt: Date): Unit = {
        dt match {
            case Day(d, m, y) => 
                Std.printString(intToString(d) ++ "/" ++ intToString(m) ++ "/" ++ intToString(y))
            case Month(m, y) => 
                Std.printString(intToString(m) ++ "/" ++ intToString(y))
            case Year(y) => 
                Std.printString(intToString(y))
        }
    }

    def getMonth(m: [1 .. 12]): String = {
        m match {
            case 1 => 
                "January"
            case 2 => 
                "February"
            case 3 => 
                "March"
            case 4 => 
                "April"
            case 5 => 
                "May"
            case 6 => 
                "June"
            case 7 => 
                "July"
            case 8 => 
                "August"
            case 9 => 
                "September"
            case 10 => 
                "October"
            case 11 => 
                "November"
            case 12 => 
                "December"
        }
    }

    def prettyPrintDate(dt: Date): Unit = {
        dt match {
            case Day(d, m, y) => 
                Std.printString(intToString(d) ++ " " ++ getMonth(m) ++ " " ++ intToString(y))
            case Month(m, y) => 
                Std.printString(getMonth(m) ++ " " ++ intToString(y))
            case Year(y) => 
                Std.printString(intToString(y))
        }
    }

    val x: Date = Day(15,5,1998);
    printDate(x);
    prettyPrintDate(x)

}