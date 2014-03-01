#Author: Eric Spaulding

use strict; #enforce strict mode
use warnings; #give warnings

$| = 1; ##dumps print lines as you go rather than saving up the buffer
binmode STDOUT, ":utf8"; ##alows for the use of some unicode characters
use constant { true => 1, false => 0 }; #define true and false for the rest of the script
my $os = $^O; #get the current operating system that the script is being run under
#os results that I've seen
#windows8 -> MSWin32
#linux mint14 -> linux
#juno -> linux
#cygwin use bash from cmd -> MSWin32 (in other words it reports the native windows os)
#cygwin terminal -> cygwin

#print "running in: $os\n";
#exit;

main();

#wrap all the main code in a function in order to keep the global namespace clean
sub main{
    my $spacesPerTab = 4; #default is to replace tabs with 4 spaces
    my $file = ""; #default is no file. script must abort in this situation.
    my $scriptname = $0; #the name of this script
    my $total = $#ARGV + 1;

    #TODO: look into a rigourous and correct way to process commandline arguments

    if ($total == 1 || $total == 2){ 
        $file = $ARGV[0];
    }

    if ($total == 2){
         $spacesPerTab = $ARGV[1];
    }
     
    if(length($file) == 0){
        print "\nno target file specified\n";
        exit;
    }
    
    writefile({ filename => $file,
                lines    => readfile({filename => $file}),
                spt      => $spacesPerTab});
}


#read a file and put the query sequences into memory
sub readfile{
    my $args = shift;
    my $filename = $args->{'filename'};

    #Open the file
    #print "opening data from $args->{'filename'}\n";
    open(IN,$filename) || die "\n$filename not found\n";
    
    my @lines = <IN>;  #read in all of the lines at once, so the lines are stored in an array
    close(IN);         #close the file
    
    return \@lines;
}

sub writefile{
    my $args = shift;
    my $filename = $args->{'filename'};
    my $lines    = $args->{'lines'};
    my $spt      = $args->{'spt'};
    my $spaces   = (" ") x $spt;
    #my $test = ("x") x $spt;

    #print "\n$test$spaces$test\n";

    #open the file for writing
    open(OUT,">$filename") || die "\ncouldn't open $filename for writing\n";
    foreach my $line (@$lines){
        $line =~ s/\t/$spaces/g;
        print OUT $line;
    }
    close(OUT);
}