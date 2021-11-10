import matplotlib.pyplot as plt

xs = [0]
ys = [0]
with open("outputIterations.txt") as file:
    for line in file:
        l = line.split(' ')
        xs.append(float(l[0]))
        ys.append(float(l[1]))

plt.plot(xs, ys)
plt.axis([0, 9, -65, -45])
plt.xlabel('Iterations')
plt.ylabel('Output')
plt.title('Iterations x Output')
plt.savefig('output.png')