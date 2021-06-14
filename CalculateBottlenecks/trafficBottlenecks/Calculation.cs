using System;
using System.Collections.Generic;

namespace trafficBottlenecks
{
    public class Calculation
    {
        public DateTime[] timestamps;
        public List<int> lastDayIterations;
        public CityGraph cityGraph;
        public CalcLoadTrees calcLoadTrees;

        public Calculation()
        {
            timestamps = new DateTime[Config.NUMBER_OF_TOTAL_ITERATIONS];
            lastDayIterations = new List<int>();

            cityGraph = new CityGraph();
            
            ReadStatic.ReadStaticMap(cityGraph);
            ReadRealTraffic.ReadFromDirectory(this.timestamps, this.lastDayIterations, cityGraph);

            cityGraph.CalcAvailabilityPerLink();
            calcLoadTrees = new CalcLoadTrees();

            for (int iteration = 0; iteration < Config.NUMBER_OF_TOTAL_ITERATIONS; iteration++)
            {
                calcLoadTrees.RunCalcLoadTreesForThisIteration(this.timestamps, cityGraph, iteration);
                calcLoadTrees.PrintLoadTrees(cityGraph, iteration);
            }
            calcLoadTrees.CalcTreeOverTimeStatistics();

            Printer.PrintGraphs(this, cityGraph, calcLoadTrees);

        }
    }
}

