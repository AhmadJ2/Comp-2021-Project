import subprocess
import os
os.system('rm -rf testss')
os.system('mkdir testss')
nops = 'PC.X'
def runs():
    for i in range(1, 169):
        cmd = "cat cases/test{}.scm > infile; make ttt>check; ./test/test > testss/anss{}".format(i, i)
        subprocess.check_output(cmd, shell=True)
        cmd = 'scheme < cases/test{}.scm >testss/sche{}'.format(i, i)
        subprocess.check_output(cmd, shell=True)
        with open('test/test.s', 'r') as check:
            if  len(check.readlines())==0:
                print('+' * 50)
                continue

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
                    else:
                        print('test {} passed'.format(i))
                        print('+' * 50)

runs()
