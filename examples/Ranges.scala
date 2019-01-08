object Ranges {

    def returnRange(i: Int): [0 .. 7] = {
        if (7 < i) {
            7
        } else {
            if (i < 0) {
                0
            } else {
                i
            }
        }
    }

    val i: [16 .. 16] = 16;
    Std.printInt(i);

    Std.printInt(returnRange(-5));
    Std.printInt(returnRange(2));
    Std.printInt(returnRange(5));
    Std.printInt(returnRange(9))
}