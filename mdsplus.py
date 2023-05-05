import numpy as np
import MDSplus
import matplotlib.pyplot as plt

#Pull raw data from MDSPlus/PTDATA
# shotnum = 189948 #(modulation)
# shotnum = 194762
# shotnum = 191400
# shotnums = [189948,194762,191400]
shotnums = np.linspace(180533, 194762, (194762-180533)+1)
for shotnum in shotnums:
    try:
        shotnum = int(shotnum)
        print(shotnum)
        c = MDSplus.Connection("atlas.gat.com")

        #Density data
        dens_raw = c.get('PTDATA("ecsdensf",' + str(shotnum) + ')')
        t_dens_raw = c.get('DIM_OF(PTDATA("ecsdensf",' + str(shotnum) + '))')

        #PCS CMD data
        sys4_pcs_raw = c.get('PTDATA("dacgyro4",' + str(shotnum) + ')')
        sys5_pcs_raw = c.get('PTDATA("dacgyro5",' + str(shotnum) + ')')
        sys6_pcs_raw = c.get('PTDATA("dacgyro6",' + str(shotnum) + ')')
        sys7_pcs_raw = c.get('PTDATA("dacgyro7",' + str(shotnum) + ')')
        sys8_pcs_raw = c.get('PTDATA("dacgyro8",' + str(shotnum) + ')')
        sys9_pcs_raw = c.get('PTDATA("dacgyro9",' + str(shotnum) + ')')
        sys10_pcs_raw = c.get('PTDATA("dacgyro10",' + str(shotnum) + ')')
        sys11_pcs_raw = c.get('PTDATA("dacgyro11",' + str(shotnum) + ')')
        t_pcs_raw = c.get('DIM_OF(PTDATA("dacgyro4",' + str(shotnum) + '))')

        #Create numpy arrays for each set of data
        dens = np.array(dens_raw)
        t_dens = np.array(t_dens_raw)
        sys4_pcs = (np.array(sys4_pcs_raw)/10).astype(bool)
        sys5_pcs = (np.array(sys5_pcs_raw)/10).astype(bool)
        sys6_pcs = (np.array(sys6_pcs_raw)/10).astype(bool)
        sys7_pcs = (np.array(sys7_pcs_raw)/10).astype(bool)
        sys8_pcs = (np.array(sys8_pcs_raw)/10).astype(bool)
        sys9_pcs = (np.array(sys9_pcs_raw)/10).astype(bool)
        sys10_pcs = (np.array(sys10_pcs_raw)/10).astype(bool)
        sys11_pcs = (np.array(sys11_pcs_raw)/10).astype(bool)
        t_pcs = np.array(t_pcs_raw)

        #Add PCS signals together to find total ech request
        pcs_total = (sys4_pcs + sys5_pcs + sys6_pcs + sys7_pcs + sys8_pcs + sys9_pcs + sys10_pcs + sys11_pcs).astype(int)*5
    
    except:
        print("FAIL. DATAERROR")
        continue
    
    try:
        # Plot PCS data
        plt.figure(1)
        plt.plot(t_pcs,pcs_total)
        plt.xlim(-2000,8000)
        plt.xlabel("Time (ms)")

        #Take the derivative of the data to find the peaks
        derivative_pcs = np.diff(pcs_total)/np.diff(t_pcs)
        derivative_t_pcs = t_pcs[0:len(t_pcs)-1]

        #Check if an index of the derivative array is an up/down point
        pulse_pcs = []
        for i in range(len(derivative_t_pcs)):
            if np.abs(derivative_pcs[i]) > 1:
                # add the time slice to the array of time slices
                pulse_pcs.append(derivative_t_pcs[i])

        #Take the difference between each index pair of the array of time slices
        diff_pcs = np.diff(pulse_pcs)

        t_pulses = []
        modulation = 0
        for i in range(len(diff_pcs)):
            #Check if each value of the differential of the time slices is larger than 100ms
            if diff_pcs[i] > 100:
                if modulation == 0:
                    t_pulses.append(pulse_pcs[i]) # add start/stop points if not modulating
                else:
                    modulation = 0 
                    if i % 2 != 0:
                        t_pulses.append(pulse_pcs[i]) #add last spike in modulated pulse if down spike          
            else:
                if modulation == 0:
                    modulation = 1
                    if i % 2 == 0:
                        t_pulses.append(pulse_pcs[i]) #add first spike in modulated pulse if up spike

        #Add the last down timeslice to the array as well
        if pulse_pcs:
            t_pulses.append(pulse_pcs[len(pulse_pcs)-1])

        #Plot start/end points
        y_pulses = np.zeros(len(t_pulses))
        plt.plot(t_pulses, y_pulses, "ro")

        #Check if the array of pulse start/end timeslices has an odd number of indices
        # print(t_pulses)
        if len(t_pulses) % 2 != 0:
            # remove the first index since that is the down timeslice of a pulse that is not long enough to register < 100ms
            t_pulses = t_pulses[1:len(t_pulses)]

        #Create arrays to separate up and down timeslices
        pulse_ups = []
        pulse_downs = []
        # print(t_pulses)
        for i in range(len(t_pulses)):
            if i%2 == 0:
                pulse_ups.append(t_pulses[i])
            else:
                pulse_downs.append(t_pulses[i])

        # print(pulse_ups)
        # print(pulse_downs)

        #Add the start and stop to each index of a new array for easy data management
        pulse_startend = []
        for i in range(len(pulse_ups)):
            pulse_startend.append([pulse_ups[i],pulse_downs[i]])
            plt.plot(pulse_startend[i],[5,5], "orange", linewidth=8)

        #Find maximum density
        max_dens_values = []
        max_dens_times = []
        for i in range(len(pulse_startend)):
            max_dens = -np.inf
            for j in range(len(t_dens)):
                if t_dens[j] >= pulse_startend[i][0] and t_dens[j] <= pulse_startend[i][1]:
                    if dens[j] > max_dens:
                        max_dens = dens[j]
                        max_dens_time = t_dens[j]
            max_dens_values.append(max_dens)
            max_dens_times.append(max_dens_time)
            plt.plot(max_dens_time, max_dens, "rx")

        max_dens_final = -np.inf
        max_dens_time_final = -np.inf
        for i in range(len(max_dens_values)):
            if max_dens_values[i] > max_dens_final:
                max_dens_final = max_dens_values[i]
                max_dens_time_final = max_dens_times[i]

        #Print final values for max density and time slice
        if max_dens_time_final != -np.inf:
            print("The maximum density is " + str(max_dens_final) + " at time " + str(max_dens_time_final))
        else:
            print("No maximum density found")

        plt.plot(t_dens, dens)
        plt.xlim(-1000, 8000)
        # plt.ylim(-1,2)
        plt.xlabel("Time (ms)")
        plt.title("ECH Peak Densities during ECH PWR " + str(shotnum))
        # plt.show()
        fname = str(shotnum)
        figure = plt.gcf()
        figure.set_size_inches(8, 6)

        plt.savefig("C:/Users/Nikolai DeBoucaud/PycharmProjects/ECH-RES-AUTO/pics/" + fname + ".png", bbox_inches='tight', dpi=300)
        plt.cla()

    except:
        print("FAIL PLOTTING ERROR")
        continue
