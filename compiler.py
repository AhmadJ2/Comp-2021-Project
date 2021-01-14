import subprocess
import os
os.system('rm -rf testss')
os.system('mkdir testss')
for i in range(1,53):
    cmd = "cat cases/{}.scm > infile; make ttt; ./test/test > testss/anss{}".format(i, i)
    subprocess.check_output(cmd, shell=True)
    cmd = 'scheme < cases/{}.scm >testss/sche{}'.format(i, i)
    subprocess.check_output(cmd, shell=True)
    with open('testss/sche{}'.format(i), 'r') as test_file:
        found = False
        with open('testss/anss{}'.format(i),'r') as ans_file:
            for ans in ans_file.readlines():
                for line in test_file.readlines():
                    found = False

                    if ans in line:
                        found = True
                        break
                if not found:
                    print("test case {}.scm failed".format(i))
                    print('OURS:\n\n')
                    os.system('cat testss/anss{}'.format(i))
                    print('-'*25)
                    print('fucking SCHEME:\n\n')
                    os.system('cat testss/sche{}'.format(i))
                    print('+'*50)
                    break
