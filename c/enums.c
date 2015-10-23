
enum printFormat
{
	PRINT_NOTHING = 0,			/* to make sure someone initializes this */
	PRINT_UNALIGNED,
	PRINT_ALIGNED,
	PRINT_WRAPPED,
	PRINT_HTML,
	PRINT_ASCIIDOC,
	PRINT_LATEX,
	PRINT_LATEX_LONGTABLE,
	PRINT_TROFF_MS
	/* add your favourite output format here ... */
};

int
main()
{
  return PRINT_UNALIGNED;
}
