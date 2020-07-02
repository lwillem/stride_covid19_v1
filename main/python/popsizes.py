import argparse
import csv
import math
import matplotlib.pyplot as plt
import numpy as np
import os

def getNumExperiments(outputDir, popSize):
    popSizeOutputDir = os.path.join(outputDir, "popSizes_" + popSize)
    summaryFile = os.path.join(popSizeOutputDir, "popsizes_" + popSize + "_summary.csv")
    numExperiments = 0
    with open(summaryFile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            numExperiments += 1
    return numExperiments

def getParams(outputDir, popSize, experimentID):
    params = {}
    experimentDir = os.path.join(outputDir, "popsizes_" + popSize, "exp" + "{:04}".format(experimentID))
    summaryFile = os.path.join(experimentDir, "summary.csv")
    with open(summaryFile) as csvfile:
        reader = csv.DictReader(csvfile)
        row = next(reader)
        params['num_days'] = int(row["num_days"])
        params['r0'] = float(row["r0"])
        params['population_size'] = int(row["population_size"])
    return params

def getNewCasesPerDay(transmissionsFile, numDays):
    transmissions = {}
    for i in range(numDays):
        transmissions[i] = 0
    with open(transmissionsFile) as f:
        for line in f:
            line = line.split(" ")
            if line[0] == "[TRAN]":
                simDay = int(line[6])
                if simDay < numDays:
                    transmissions[simDay] += 1
    return transmissions

def getAttackRatesByR0(outputDir, popSize, numExperiments):
    attackRates = {}
    for experimentID in range(1, numExperiments + 1):
        experimentDir = os.path.join(outputDir, "popsizes_" + popSize, "exp" + "{:04}".format(experimentID))
        transmissionsFile = os.path.join(experimentDir, "event_log.txt")
        params = getParams(outputDir, popSize, experimentID)
        newCasesPerDay = getNewCasesPerDay(transmissionsFile, params["num_days"])
        attackRate = sum([newCasesPerDay[day] for day in newCasesPerDay.keys()]) / params['population_size']
        if params['r0'] in attackRates:
            attackRates[params['r0']].append(attackRate)
        else:
            attackRates[params['r0']] = [attackRate]
    return attackRates

def getPeakIncidenceRatiosByR0(outputDir, popSize, numExperiments):
    peakIncidenceRatios = {}
    for experimentID in range(1, numExperiments + 1):
        experimentDir = os.path.join(outputDir, "popsizes_" + popSize, "exp" + "{:04}".format(experimentID))
        transmissionsFile = os.path.join(experimentDir, "event_log.txt")
        params = getParams(outputDir, popSize, experimentID)
        newCasesPerDay = getNewCasesPerDay(transmissionsFile, params["num_days"])
        peakIncidenceRatio = max([newCasesPerDay[day] for day in newCasesPerDay.keys()]) / params['population_size']
        if params['r0'] in peakIncidenceRatios:
            peakIncidenceRatios[params['r0']].append(peakIncidenceRatio)
        else:
            peakIncidenceRatios[params['r0']] = [peakIncidenceRatio]
    return peakIncidenceRatios

def getDayOfPeakByR0(outputDir, popSize, numExperiments):
    daysOfPeak = {}
    for experimentID in range(1, numExperiments + 1):
        experimentDir = os.path.join(outputDir, "popsizes_" + popSize, "exp" + "{:04}".format(experimentID))
        transmissionsFile = os.path.join(experimentDir, "event_log.txt")
        params = getParams(outputDir, popSize, experimentID)
        newCasesPerDay = getNewCasesPerDay(transmissionsFile, params["num_days"])
        peakIncidence = max([newCasesPerDay[day] for day in newCasesPerDay.keys()])
        dayOfPeak = [day for day in newCasesPerDay.keys() if newCasesPerDay[day] == peakIncidence]
        dayOfPeak = min(dayOfPeak)
        if params['r0'] in daysOfPeak:
            daysOfPeak[params['r0']].append(dayOfPeak)
        else:
            daysOfPeak[params['r0']] = [dayOfPeak]
    return daysOfPeak

def set_box_color(bp, color):
    plt.setp(bp['boxes'], color=color)
    plt.setp(bp['whiskers'], color=color)
    plt.setp(bp['caps'], color=color)
    plt.setp(bp['medians'], color=color)

def plotAttackRates(outputDir, popSizes, attackRates):
    r0s = attackRates[0].keys()
    width = 0.35
    positions=[np.array(range(len(r0s)))*2.0-0.5, np.array(range(len(r0s)))*2.0, np.array(range(len(r0s)))*2.0+0.5]
    colors = ["red", "orange", "green"]
    for i in range(len(popSizes)):
        ar = list(attackRates[i].values())
        bp = plt.boxplot(ar, widths=width, positions=positions[i], sym='')
        set_box_color(bp, colors[i])
    plt.xlabel("Basic reproduction number")
    plt.xlim(-1, 7)
    plt.xticks(range(0, len(r0s) * 2, 2), r0s)
    plt.ylabel("Attack rate")
    # Legend
    for i in range(len(popSizes)):
        plt.plot([], c=colors[i], label=popSizes[i])
    plt.legend()
    plt.savefig("attackRates.png")
    plt.clf()

def plotPeakIncidenceRatios(outputDir, popSizes, peakIncidenceRatios):
    r0s = peakIncidenceRatios[0].keys()
    width = 0.35
    positions=[np.array(range(len(r0s)))*2.0-0.5, np.array(range(len(r0s)))*2.0, np.array(range(len(r0s)))*2.0+0.5]
    colors = ["red", "orange", "green"]
    for i in range(len(popSizes)):
        pir = list(peakIncidenceRatios[i].values())
        bp = plt.boxplot(pir, widths=width, positions=positions[i], sym='')
        set_box_color(bp, colors[i])
    plt.xlabel("Basic reproduction number")
    plt.xlim(-1, 7)
    plt.xticks(range(0, len(r0s) * 2, 2), r0s)
    plt.ylabel("Peak incidence / pop size")
    # Legend
    for i in range(len(popSizes)):
        plt.plot([], c=colors[i], label=popSizes[i])
    plt.legend()
    plt.tight_layout()
    plt.savefig("peakIncidenceRatios.png")
    plt.clf()

def plotDaysOfPeak(outputDir, popSizes, daysOfPeak):
    r0s = daysOfPeak[0].keys()
    width = 0.35
    positions=[np.array(range(len(r0s)))*2.0-0.5, np.array(range(len(r0s)))*2.0, np.array(range(len(r0s)))*2.0+0.5]
    colors = ["red", "orange", "green"]
    for i in range(len(popSizes)):
        pir = list(daysOfPeak[i].values())
        bp = plt.boxplot(pir, widths=width, positions=positions[i], sym='')
        set_box_color(bp, colors[i])
    plt.xlabel("Basic reproduction number")
    plt.xlim(-1, 7)
    plt.xticks(range(0, len(r0s) * 2, 2), r0s)
    plt.ylabel("Day of peak")
    # Legend
    for i in range(len(popSizes)):
        plt.plot([], c=colors[i], label=popSizes[i])
    plt.legend()
    plt.tight_layout()
    plt.savefig("daysOfPeak.png")
    plt.clf()

def main(outputDir, popSizes):
    allAttackRates = []
    allPeakIncidenceRatios = []
    allDaysOfPeak = []
    for size in popSizes:
        #numExperiments = getNumExperiments(outputDir, size)
        numExperiments = 200
        # Get attack rates
        attackRatesByR0 = getAttackRatesByR0(outputDir, size, numExperiments)
        allAttackRates.append(attackRatesByR0)
        # Get peak incidence ratios
        peakIncidenceRatiosByR0 = getPeakIncidenceRatiosByR0(outputDir, size, numExperiments)
        allPeakIncidenceRatios.append(peakIncidenceRatiosByR0)
        # Get day of peak
        dayOfPeakByR0 = getDayOfPeakByR0(outputDir, size, numExperiments)
        allDaysOfPeak.append(dayOfPeakByR0)
    # Plot results
    plotAttackRates(outputDir, popSizes, allAttackRates)
    plotPeakIncidenceRatios(outputDir, popSizes, allPeakIncidenceRatios)
    plotDaysOfPeak(outputDir, popSizes, allDaysOfPeak)

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("outputDir", type=str, help="Directory containing simulation output")
    parser.add_argument("--popSizes", type=str, nargs="+", default=["600k", "1000k", "3000k"])
    args = parser.parse_args()
    main(args.outputDir, args.popSizes)
