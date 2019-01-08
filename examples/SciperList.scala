object SciperList {

    def printArray(arr: array[15]): Unit = {
        val i: [0 .. 14] = 0;
        printFrom(arr, i)
    }

    def printFrom(arr: array[15], i: [0 .. 14]): Unit = {
        Std.printInt(arr[i]);
        if(i < arr.length - 1) {
            printFrom(arr, i + 1)
        } else {
            ()
        }
    }

    val scipers: array[15] = [123456,
                            654321,
                            234567,
                            765432,
                            345678,
                            876543,
                            456789,
                            987654,
                            567890,
                            678901,
                            109876,
                            789012,
                            210987,
                            890123,
                            321098];

    printArray(scipers)
}