import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  
import matplotlib.patches as mpatches
import numpy as np

tssO = [0]
issO = [0]
ossO = [0]
tssI = [0]
ossI = [0]

it = 0
lastOutputPerTime = 0
tssO2 = [0, 0.1]
ossO2 = [0]
first = False
l = []
with open("outputTime.txt") as file:
    for line in file:
        l = line.split(' ')
        if it != float(l[0]) and first:
            tssO2.append(float(l[0]))
            ossO2.append(lastOutputPerTime)
            lastOutputPerTime = 0
        lastOutputPerTime = float(l[2])
        first = True
        it = float(l[0])
        tssO.append(float(l[0]))
        issO.append(float(l[1]))
        ossO.append(float(l[2]))
        
ossO2.append(float(l[2]))
with open("inputTime.txt") as file:
    for line in file:
        l = line.split(' ')
        tssI.append(float(l[0]))
        ossI.append(float(l[1]))        

print(tssO2)
print(ossO2)
fig = plt.figure()
ax = Axes3D(fig)
ax.plot_trisurf(tssO, issO, ossO, color='cornflowerblue', linewidth=0.1)
print(max(issO))
ax.plot(tssI, np.full(len(tssI), min(issO)), ossI, c='red', linewidth=1.5)
ax.set_title('Time x Iterations x Output')
ax.set_xlabel('Time')
ax.set_ylabel('Iterations')
ax.set_zlabel('Output')
red_legend = mpatches.Patch(color='red', label='Input')
blue_legend = mpatches.Patch(color='cornflowerblue', label='Output')
ax.legend(loc='upper left', handles=[red_legend, blue_legend])
plt.show()

fig2 = plt.figure()
plt.plot(tssO2, ossO2, color='cornflowerblue', label='Output')
plt.plot(tssI, ossI, color='red', label='Input')
plt.xlabel('Time')
plt.ylabel('Value')
plt.title('Time x Value')
plt.legend()
plt.savefig('outputTime2D.png')
#plt.savefig('output.png')