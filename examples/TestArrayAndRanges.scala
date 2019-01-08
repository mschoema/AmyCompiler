object TestArrayAndRanges {
    def returnFirst(a: array[3]): Int = {
        a[0]
    }

    def printBoth(arr1: array[8], arr2: array[8], i: [0 .. 7]): Unit = {
        Std.printString("Yes!");
        Std.printInt(arr1[i]);
        Std.printInt(arr2[i])
    }

    val x: Int = 3;
    Std.printInt(x);
    Std.printString("Hello " ++ "world!");
    val z: array[3] = [2, 4, 3];
    val i: [14 .. 19] = 15;
    val u: Int = z[i-13];
    z[z.length - 1];
    val l: Int = z.length;
    val f: Int = returnFirst(z);
    val r: Int = z[Std.readInt()];
    val someInt: Int = 3;
    printBoth([1,2,3,4,5,6,7,8], [8,7,6,5,4,3,2,1], someInt);

    x match {
        case 4 => 
            Std.printString("4")
        case 3 => 
            Std.printInt(u);
            Std.printInt(l);
            Std.printInt(f);
            Std.printInt(r)
        case _ => 
            Std.printString("default")
    }
}
