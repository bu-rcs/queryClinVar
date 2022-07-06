## Installation

Requirements you will need to have [stack][1] version >= 2.7 available.

On the scc cluster you will need gcc version >= 8.3 (e.g `module load gcc/8.3.0`) and note that both wget/curl are already available.


```
module load gcc/11.2.0
module load haskell/9.2.3
```

1. Clone the repository:   `git clone https://github.com/achilleasNP/queryClinVar`
2. Change to the local repository directory e.g: `cd queryClinVar`
3. Run stack build: `stack build |&tee stack.build.output`
4. Optional run stack install `stack install |&tee stack.install.output`

Alternatively:

1. Clone the repository:   `git clone https://github.com/achilleasNP/queryClinVar`
2. Change to the local repository directory e.g: `cd queryClinVar`
3. Run `make |&tee  make.output`

Once software is installed, test if the executable file works:
```
bin/queryClinVar-exe --help
```


## Usage

```
    queryClinVar-exe INPUT_FILE OUTPUT_FILE
```

## Test

Download the popseq file. The file is csv formatted with two columns as shown below.

```
variant,Amino acid
PALB2 04 1129C>T,p.Gln377X
PALB2 04 509_510delGA,p.Arg170IlefsX14
RET 14 2410G>A,p.Val804Met
SDHC 05 387G>A,p.Trp129X
```

Run the program:
```
queryClinVar-exe popseq_file.csv annotated_popseq_files.csv
```

Check the output file:
```
ls -l
## total 1
## -rw-r--r-- 1 ktrn apps 444 Jun 26 20:11 annotated_popseq_files.csv
## -rw-r--r-- 1 ktrn apps 137 Jun 26 20:10 popseq_file.csv
```

