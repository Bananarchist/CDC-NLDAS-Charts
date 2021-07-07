# Scripts

## collator.awk
Awk script used to collate the sunlight and preciptation data
Expectations: two CSVs that will have unique columns 1+2 (in this case 
`State Name` + `Year`, eg, `Alabama 1988`)

## toelmdatum.awk
Awk script used to take the output of `collator.awk` and convert it
into a list of `Datum` types for export to `src/Data.elm`

Combined use: 
`gawk -f collator.awk sunlight.csv preciptation.csv | gawk -f toelmdatum.csv >> src/Data.elm`

And then this will probably require some cleanup, I've never bothered learning how to trim that final comma...
