import numpy as num
import os
#make sure to run this while in the 
#autopulse_localfitslightcurve_qatsouttodb branch
kids = []
fits_txt_file_line = []
fits_txt_file = '/astro/users/bvegaff/testGit/autopulse/fits_list.txt'
fits_txt_file_buff = open(fits_txt_file,'r')
fits_txt_file_line_list = fits_txt_file_buff.readlines()

for line in fits_txt_file_line_list:
	stripped_line = line.rstrip()
	kids.append(stripped_line[-31:-23])
	split_stripped_line=stripped_line.split(' ')
	comma_separated_line = []
	for file in split_stripped_line:
		comma_separated_line +=file+','
	comma_separated_line = ''.join(comma_separated_line[0:len(comma_separated_line)-1])
	fits_txt_file_line.append(comma_separated_line)

MainPath = '/astro/store/student-scratch1/bvegaff/QATSruns/KOI_chiSq/test_new_data' #location where condor outputs will be
	
Autopulse_Path = '/astro/users/bvegaff/testGit/autopulse' #where the autopulse files are, along with our executable .bsh file
	
CFGFileName = "small_run_test1.cfg"
Ofile = open(CFGFileName,'w')

print >> Ofile, "Universe = Vanilla\n"
print >> Ofile, "Executable = "+Autopulse_Path+"/run_idlvm_driver_for_compute_qats.bsh"
print >> Ofile, "Initialdir = "+Autopulse_Path
print >> Ofile, "Log = "+MainPath+"/condor.log"
print >> Ofile, "error = "+MainPath+"/condor.err"
print >> Ofile, "getenv = True"
print >> Ofile, "kill_sig = 11"
print >> Ofile, "on_exit_remove = (ExitBySignal == False) || (ExitSignal != 11)"
print >> Ofile, "next_job_start_delay = 1 \n"

#FINISH THIS SO IT LOOPS THROUGH ALL OF THE KIDS IN OUR LIST DUMMY
const_args = ' 0.005 \'[1.0,300.0]\' '
i=0
masks = '0'
print len(kids)
#for j in range(len(kids)):
#for testing purposes, running on two kids, including 757076 and one koi (11359879)
test_list = [0,180094]
for j in test_list:
	directory = MainPath + '/output/'+kids[j][0]+kids[j][1]+kids[j][2]+'/'+kids[j]
 	CFGLines = ["#queue_no =" + str(i),"Output = "+directory+'/printed.out', "Arguments = "+kids[j]+const_args+masks+' '+directory+' '+Autopulse_Path+' '+fits_txt_file_line[j],"Notification = Never","Queue"]
	i+= 1 #counter
	for el in CFGLines:
		print >> Ofile, el
	print >> Ofile, '\n'
																						          
Ofile.close()

