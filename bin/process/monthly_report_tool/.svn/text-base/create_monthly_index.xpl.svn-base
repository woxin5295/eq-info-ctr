# Copied /usr/local/bin/create_weekly_index.pl G. Thompson 20121017
# print out the beginning stuff to tell the server you're sending it
# print out the HTML tags that you want to appear before the output
# from the ls command
# 20121017 G. Thompson added input arguments, which ultimately come from monthly_report_tool.pf
($basewebdir_in, $basewebdir_out) = @ARGV;

$web_base = "http://www.aeic.alaska.edu/monthly_archive/";
$outdir = "$basewebdir_out/monthly_archive";
system("mkdir $outdir") unless (-e $outdir);
$htmlfile = "$outdir/index.html";
@data = `cd $basewebdir_in/monthly_archive; /bin/ls *.txt`;
open (INDEX_FILE, "> $htmlfile") ||
  die ( "Can't open file $htmlfile\n");
print "$0: Writing to $htmlfile\n";
print INDEX_FILE "<HTML><HEAD>\n";
print INDEX_FILE "<TITLE>Monthly Archive</TITLE>\n";
print INDEX_FILE "</HEAD><BODY>\n";
print INDEX_FILE "<B>\n";
print INDEX_FILE "<PRE>\n";
# 20121017 G. Thompson: appears to me that the purpose of $num
# below is in fact to decide which column (1 or 2) to list a file link in.
@data = `cd $basewebdir_in/monthly_archive\; /bin/ls *_monthly.txt`;
$num = 1;
@hrefs = "";
foreach $file (sort(@data)) {
  chop $file;
        print "$0: Adding $file\n";
  if($num > 2) {
    $num = 1;
    $cr = "\n";
  } else
    {
    $num++;
    $cr = "  ";
  }
  @hrefs = (@hrefs,sprintf("<A HREF=\"$web_base/$file\" >%s</A>$cr", $file ));
}
@hrefs = (@hrefs,sprintf("\n"));
print INDEX_FILE  @hrefs;
print INDEX_FILE "</PRE>\n";
print INDEX_FILE "</B>\n";
print INDEX_FILE "</BODY>\n";
print INDEX_FILE "</HTML>\n";
