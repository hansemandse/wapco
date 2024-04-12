#!/bin/sh
# Generates a Tcl script for creating a new Vivado project and adding the 
# exported adder designs as source files to it, disabling all but RCA_32.v
FILE=build.tcl

echo "set path [pwd]\n" > $FILE
echo "# Create Vivado project
create_project -force Wapco \${path}/build/Wapco -part xc7a35tcpg236-1

set fp [open \${path}/build/LUTs.csv w]
fconfigure \$fp -buffering none
puts \$fp \"Name,LUTs\"

# Run synthesis and implementation for all the considered designs" >> $FILE

for f in "build"/*.v; do
    echo "remove_files [get_files]
add_files -norecurse \${path}/$f
puts -nonewline \$fp [lindex [split [lindex [split $f \"/\"] 1] \".\"] 0]
reset_run synth_1
launch_runs synth_1
wait_on_runs synth_1
open_run synth_1
set lines [split [report_utilization -return_string] \"\\\n\"]
close_design
foreach line \$lines {
    if {[string first \"LUTs\" \$line] != -1} {
        set count [string trim [lindex [split \$line \"|\"] 2]]
        puts \$fp \",\$count\"
        break
    }
}\n" >> $FILE
done
echo "close \$fp" >> $FILE
