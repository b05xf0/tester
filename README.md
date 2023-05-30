# tester: Property-based testing application built on QuickCheck

## Description
This tool is a property-based testing application developed in Haskell specifically designed for exams, homeworks. It automatically runs tests and evaluates them based on properties specified by the instructor, generating a report afterwards. The tasks provided by the instructor, along with their properties, are processed and stored in a database for future use.

## Features
- Automated testing based on predefined properties
- Evaluation and report generation
- Processing and storage of tasks and their properties in a database
- CLI-based user interface
- JSON formatted reports

## Requirements
The program is intended to run on a Linux operating system or Windows Subsystem for Linux 2 (WSL2) environment. To compile, install and run this application, the following software is required:

1. Glasgow Haskell Compiler (GHC) 9.2.7 (with base-4.16.4.0)
2. Cabal 3.6.2.0

At present, the simplest and recommended way to ensure a suitable software environment is through the use of [GHCup](https://www.haskell.org/ghcup/). 


## Installation Guide
1. Clone the repository.
2. To compile the program, run the following command:
    ```shell
    cabal build tester
    ```
   Upon issuing this command, the dependencies recorded in the .cabal file will be downloaded for compilation and installation.
3. For the program to operate correctly, the QuickCheck package must also be available in the global GHC environment. To install QuickCheck, run the following command:

    ```shell
    cabal install QuickCheck --lib
    ```
4. Install the application with the following command:
    ```shell
    cabal install tester
    ```
After installing the application, you can check the available commands and options by running:

```shell
tester -h
```

## Dependencies
- QuickCheck: for running property-based tests
- hint: for runtime interpretation of Haskell code
- sqlite-simple: for interfacing with SQLite databases
- optparse-applicative: for parsing command-line options and arguments
- async: for higher-level operations on threads and asynchronous I/O
- random: for generation of random numbers
- aeson: for encoding and decoding JSON
- aeson-pretty: for encoding JSON in a human-readable format
- text: for efficient string manipulation
- time: for time-related functionality
- directory: for file and directory manipulation
- filepath: for manipulating FilePath values in a cross-platform way

## Contributing
If you would like to contribute, please fork the repository and use a feature branch.

## License
The tool is open-sourced under the [GNU LESSER GENERAL PUBLIC LICENSE Version 2.1](./LICENSE).

