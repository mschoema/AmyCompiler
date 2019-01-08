# AmyCompiler

Computer Language Processing Project

This is a compiler for the [Amy language](http://lara.epfl.ch/w/cc18:amy_specification) with the addition of arrays and range types to the language.

## Getting Started

### Prerequisites

To run the code, you will need Java 8 running on your system and a build tool for scala.

[sbt](https://www.scala-sbt.org/), the default build tool for scala can be downloaded [here](https://www.scala-sbt.org/download.html).

To run the compiled code, you will need to download nodejs [here](https://nodejs.org/en/download/).
Make sure you have nodejs 8 or later by running  `nodejs --version`
After you install, run `npm install deasync` at the directory you plan to run amyc, i.e. the toplevel directory of the compiler. 
Make sure the wat2wasm executable is visible, i.e. it is in the system path or you are at the toplevel of the amyc directory.

### Running the code

After downloading the code, go to AmyCompiler directory and start sbt.
Now you can compile any Amy program by typing the following code inside sbt:

    run extraDirectory\extraFileName.scala programDirectory\programName.scala

You can add as much extra files as needed.

To run the compiled code, simply write:

    nodejs wasmout\programName.js
  
### Example programs

Example Amy programs can be found under [examples](/examples/) and Amy libraries used in some of these programs can be found under [library](/library/)

## Authors

* **Maxime Schoemans** - [mschoema](https://github.com/mschoema)
