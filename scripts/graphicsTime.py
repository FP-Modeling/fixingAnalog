import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  
from matplotlib import cm
import numpy as np

tss = [0]
iss = [0]
oss = [0]
with open("outputTime.txt") as file:
    for line in file:
        l = line.split(' ')
        tss.append(float(l[0]))
        iss.append(float(l[1]))
        oss.append(float(l[2]))

fig = plt.figure()
ax = Axes3D(fig)
surf = ax.plot_trisurf(tss, iss, oss, cmap=cm.coolwarm, linewidth=0.1)
ax.set_title('Time x Iterations x Output')
ax.set_xlabel('Time')
ax.set_ylabel('Iterations')
ax.set_zlabel('Output')
plt.show()
#plt.savefig('output.png')