set path

# Create Vivado project
create_project Wapco ${path}build/Wapco -part xc7a35tcpg236-1
set_property board_part digilentinc.com:basys3:part0:1.1 [current_project]

# Load source files
add_files -norecurse ${path}build/CLA_32_4.v
set_property is_enabled false [get_files ${path}build/CLA_32_4.v]
add_files -norecurse ${path}build/GeAr_32_6_2.v
set_property is_enabled false [get_files ${path}build/GeAr_32_6_2.v]
add_files -norecurse ${path}build/GeAr_32_8_8.v
set_property is_enabled false [get_files ${path}build/GeAr_32_8_8.v]
add_files -norecurse ${path}build/LOA_32_12.v
set_property is_enabled false [get_files ${path}build/LOA_32_12.v]
add_files -norecurse ${path}build/OFLOCA_32_12_4.v
set_property is_enabled false [get_files ${path}build/OFLOCA_32_12_4.v]
add_files -norecurse ${path}build/RCA_32.v

# Launch synthesis
launch_runs -jobs 8 synth_1
wait_on_run synth_1
