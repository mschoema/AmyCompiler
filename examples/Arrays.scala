object Arrays {

    def returnFirst(a: array[3]): Int = {
        a[0]
    }

    def printBoth(arr1: array[8], arr2: array[8], i: [0 .. 7]): Unit = {
        Std.printInt(arr1[i]);
        Std.printInt(arr2[i])
    }

    val i: Int = 4;
    printBoth([1,2,3,4,5,6,7,8], [8,7,6,5,4,3,2,1], i);

    val z: array[3] = [2, 4, 3];
    Std.printInt(z[z.length - 1]);
    Std.printInt(returnFirst(z));

    val x: array[8] = [2,6,7,3,0,1,4,9];
    Std.printString("Give a number between 0 and 7");
    val n: [0 .. 7] = Std.readInt();
    Std.printInt(x[n])
}