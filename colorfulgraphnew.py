import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.axes as ax
import sys, time, math
import serial
import matplotlib.pyplot.axvline

xsize=100

ser = serial.Serial(
 port='COM11',
 baudrate=115200,
 parity=serial.PARITY_NONE,
 stopbits=serial.STOPBITS_TWO,
 bytesize=serial.EIGHTBITS
)
ser.isOpen()   
   
   
def data_gen():

    labelt = plt.title('Reflow Soldering Profile', fontsize=40)
    labelt.set_color("red")
    
    labely = plt.ylabel('Temperature', fontsize=30)
    labely.set_color("red")
    
    labelx = plt.xlabel('Time', fontsize=30)
    labelx.set_color("red")
    
    plt.legend()
    
    ax.set_axis_bgcolor('black')
    ax.set_axis_bgcolor((0,0,0))#change the backgroudnof the graph
    
    
    
    t = data_gen.t
    while True:
       t+=1
       val = ser.readline()

       if val <= 150:
           line.set_color("yellow")
       else:
           if val > 150:
               if t <=60:
                   line.set_color("blue")
                   if t > 60:
                       if val <= 220:
                           line.set_color("green")
                           if val > 220:
                               if t <= 45:
                                   line.set_color("white")
                                   if t > 45:
                                       line.set_color("cyan")
                                       if val >= 60:
                                           line.set_color("magenta")
                                           
           
    yield t, val

def run(data):
    # update the data
    t,y = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)

    return line,

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot(label = lines, linestyle='-.', lw=5, color = 'red')
ax.set_ylim(0, 300)
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()

