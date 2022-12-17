#!/bin/sh
# Generates a Tcl script for creating a new Vivado project and adding the 
# exported adder designs as source files to it, disabling all but RCA_32.v
FILE=build.tcl
JOBS=$(nproc)

echo "set path\n" > $FILE
echo "# Create Vivado project
create_project Wapco \${path}build/Wapco -part xc7a35tcpg236-1
set_property board_part digilentinc.com:basys3:part0:1.1 [current_project]" >> $FILE
echo "\n# Load source files" >> $FILE
for f in "build"/*.v; do
    echo "add_files -norecurse \${path}$f" >> $FILE
    if [ "$f" = "build/RCA_32.v" ]; then
        continue;
    fi
    echo "set_property is_enabled false [get_files \${path}$f]" >> $FILE
done
echo "\n# Launch synthesis
launch_runs -jobs $JOBS synth_1
wait_on_run synth_1" >> $FILE
