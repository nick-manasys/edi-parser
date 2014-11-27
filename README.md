# EDI Parser

#Usage

Normal Maven build handles the generation of binaries. To generate YAML schema
definitions from the X12 table data use `mvn scala:run -Dlauncher=genx12`. This
currently generates only the 5010 version of the schemas, using the table data
at *src/test/resources/x12/5010* as input and outputing the schemas to
*target/schemas/x12/5010*.
