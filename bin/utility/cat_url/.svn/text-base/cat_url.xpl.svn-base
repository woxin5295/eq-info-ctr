# Glenn Thompson 17 September 2007
# This is a program which will parse a document on a webserver protected by standard security such as htaccess

use strict;
use warnings;

my $URL;

# Set the ShakeMap URL
if ($#ARGV == 0 || $#ARGV == 3) {
	$URL = $ARGV[0]; 
}
else
{

	print STDERR<<"EOF";
cat_url

This is a program which will echo a document on a webserver to stdout. See man page (man cat_url)

Usage:
	cat_url url [realm username password]

Examples:

	(1) The following is an example of how to echo to stdout an unprotected webpage:

		cat_url http://www.aeic.alaska.edu/Input/glenn/index.html

	(2) The following is an example of how to echo to stdout the ShakeMap archive webpage, and shows how to access a protected webpage:
		
		cat_url http://www.aeic.alaska.edu/~shake/shake/archive/index.html "Enter username and password for \"ShakeMap\" at http://www.aeic.alaska.edu" shake shake


Author: Glenn Thompson, AEIC, 17 September 2007, glenn\@giseis.alaska.edu

EOF
	exit 1;
}  

# Make sure its using port 80
(my $netloc = $URL) =~ s/edu/edu:80/ if ($URL =~ "80");


# Use the UserAgent module in the Perl Web Page Library
# This allows a program to simulate a user of a web page
use LWP::UserAgent;

# Create a UserAgent object
my $ua = LWP::UserAgent->new;

if ($#ARGV > 0) {
	# Authenticate

	# Set realm, username and password
	my ($realm,$user,$pass) = @ARGV[1..3];

	# Set the realm - the question asked by the webserver for authentication
	# my $realm = "Enter username and password for \"ShakeMap\" at http://www.aeic.alaska.edu";

	# Authenticate by calling the credentials method on the UserAgent object
	$ua->credentials($netloc, $realm, $user, $pass);
}

# Create an HTTP::Request object
my $req = HTTP::Request->new(POST => $URL);

# Create a variable for returning the contents of the web page
my $contents;

# Call the request method on the UserAgent object, passing the Http::Request object as an argument
my $res = $ua->request($req);

# Check the outcome of the response
if ($res->is_success) {
     print $res->content;
}
else 
{
     print $res->status_line, "\n";
}


