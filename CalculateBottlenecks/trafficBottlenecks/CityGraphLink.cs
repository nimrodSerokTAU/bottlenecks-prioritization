using System;
using System.Collections.Generic;

namespace trafficBottlenecks
{
    enum MeasureType {UNKOWN = 0, MEASURED = 1};

    public class CityGraphLink
    {
        public int id;
        public int start_node;
        public int end_node;
        public string roadSegmentId;
        public string locationKey;
        public int linkLength;
        public double[] allVs;
        public int[] lastLoadedCounter;
        public int[] lastLoadedW;
        public Measurement[] allMeasurements;
        public Measurement optFlowMeasure;
        public int lanes;
        public double costInOneLoadTree;
        public int measureType;
        public double percentileV;
        public string name;
        public int[] inTreeIterations; // measurement
        public int[] asTrunkIterations; // measurement
        public int[] totalCostInMinutes; // measurement
        public int[] totalCostInMinutesLoad; // measurement
        public int[] costPerIter;
        public int[] myTrunk; // measurement

        public CityGraphLink(int id, CityGraphNode fromN, CityGraphNode toN, int linkLength, int lanes)
        {
            this.id = id;
            start_node = fromN.id;
            end_node = toN.id;
            locationKey = fromN.locationKey + "," + toN.locationKey;
            this.linkLength = linkLength;
            this.allVs = new double[Config.NUMBER_OF_TOTAL_ITERATIONS];
            this.allMeasurements = new Measurement[Config.NUMBER_OF_TOTAL_ITERATIONS];
            this.costPerIter = new int[Config.NUMBER_OF_TOTAL_ITERATIONS];
            this.lastLoadedCounter = new int[Config.NUMBER_OF_TOTAL_ITERATIONS];
            this.lastLoadedW = new int[Config.NUMBER_OF_TOTAL_ITERATIONS];
            this.lanes = lanes;
            costInOneLoadTree = 0;
            measureType = (int)MeasureType.UNKOWN;
            inTreeIterations = new int[Config.NUMBER_OF_DAYS+1];
            myTrunk = new int[Config.NUMBER_OF_TOTAL_ITERATIONS];
            asTrunkIterations = new int[Config.NUMBER_OF_DAYS + 1];
            totalCostInMinutes = new int[Config.NUMBER_OF_DAYS + 1];
            totalCostInMinutesLoad = new int[Config.NUMBER_OF_DAYS + 1];

            for (int i = 0; i < this.allMeasurements.Length; i++)
            {
                this.allMeasurements[i] = new Measurement();
            }
        }

        internal string CreateMyDebugString()
        {
            string res = this.id + ";" + this.locationKey + ";" + this.percentileV + ";" + this.lanes + ";" + this.linkLength;
            return res;
        }
        
        internal void AddnewSample(int iteration, int time, CityGraph cityGraph)
        {
            double velocity;
            
            velocity = time == 0 ? 0 : (double)(((double)this.linkLength / (double)time) * Utils.VELOCITY_MULTIPLIER);
            if (velocity > 0 && this.measureType == (int)MeasureType.UNKOWN)
            {
                this.MarkMeWithData((int)MeasureType.MEASURED, cityGraph);
            }
            allVs[iteration] = velocity;
            allMeasurements[iteration] = new Measurement(velocity);
        }

        public void FillLinkAvailability(Dictionary<int, int>[] linksOfLoadTreeDic, Dictionary<int, List<int>>[] linksOfLoadTree)
        {
            this.CalcMyPercentileV();
            for (int i = 0; i < allVs.Length; i++)
            {
                FillLastLoadedOnSpecificIteration(i, linksOfLoadTreeDic, linksOfLoadTree);
            }
        }

        internal void CalcMyPercentileV()
        {
            List<double> allVsSorted = new List<double>(this.allVs);
            allVsSorted.Sort();

            int percentile = (int)((double)allVsSorted.Count * Config.AVAILABILITY_PERCENTILE);
            this.percentileV = allVsSorted[percentile];

            switch (Config.macroscopicModel)
            {
                case (int)MacroscopicModel.GREENSHIELD:
                    optFlowMeasure = new Measurement((int)(this.percentileV / 2));
                    optFlowMeasure.density = Config.MAX_ROAD_DENSITY / 2;
                    optFlowMeasure.flow = optFlowMeasure.density * optFlowMeasure.velocity;
                    optFlowMeasure.availability = 50;
                    break;
                case (int)MacroscopicModel.MAY:
                    for (int i = 0; i < this.allMeasurements.Length; i++)
                    {
                        this.allMeasurements[i].ComputeMyAvilability((int)(Math.Round(this.percentileV)));
                    }
                    this.optFlowMeasure = FindHighestFlow(allVsSorted);
                    break;
            }

        }

        private Measurement FindHighestFlow(List<double> allVsSorted)
        {
            List<Measurement> allMeasurementsSorted = new List<Measurement>(this.allMeasurements);
            allMeasurementsSorted.Sort((a, b) => b.flow.CompareTo(a.flow));
            Measurement currentOptFlowMeasure = allMeasurementsSorted[0];
            if (currentOptFlowMeasure.flow == allMeasurementsSorted[allMeasurementsSorted.Count - 1].flow)
            {
                currentOptFlowMeasure.flow = 1;
                if (allVsSorted[0] == 0)
                {
                    currentOptFlowMeasure.velocity = -1;
                }
            }
            return currentOptFlowMeasure;
        }

        private void FillLastLoadedOnSpecificIteration(int iteration, Dictionary<int, int>[] linksOfLoadTreeDic, Dictionary<int, List<int>>[] linksOfLoadTree)
        {
            int thisAvailability = this.allMeasurements[iteration].availability;
            int prevCost = 0;
            int prevCount = 0;
            if (iteration > 0)
            {
                prevCost = lastLoadedW[iteration - 1];
                prevCount = lastLoadedCounter[iteration - 1];
            }
            int dayInx = Utils.GetTestDayInxFromIteration(iteration);
            if (thisAvailability > -1 && this.measureType == (int)MeasureType.MEASURED)
            {
                if (thisAvailability < this.optFlowMeasure.availability)
                {
                    this.allMeasurements[iteration].isLoaded = true;
                    int currentIterationCost = CalcCostOfThisIteration(iteration);
                    lastLoadedCounter[iteration] = prevCount + 1;
                    lastLoadedW[iteration] = prevCost + currentIterationCost;
                    this.totalCostInMinutesLoad[dayInx] += currentIterationCost;
                    this.totalCostInMinutesLoad[Config.NUMBER_OF_DAYS] += currentIterationCost;
                    this.costPerIter[iteration] = currentIterationCost;
                }
            }
            
            if (lastLoadedCounter[iteration] > 0)
            {
                if (!linksOfLoadTreeDic[iteration].ContainsKey(this.id))
                {
                    linksOfLoadTreeDic[iteration].Add(this.id, lastLoadedCounter[iteration]);
                }
                else
                {
                    linksOfLoadTreeDic[iteration][this.id] = lastLoadedCounter[iteration];
                }
                if (!linksOfLoadTree[iteration].ContainsKey(lastLoadedCounter[iteration]))
                {
                    linksOfLoadTree[iteration].Add(lastLoadedCounter[iteration], new List<int>());
                }
                linksOfLoadTree[iteration][lastLoadedCounter[iteration]].Add(this.id);
            }
        }
        private int CalcCostOfThisIteration(int iteration)
        {
            Measurement m = this.allMeasurements[iteration];
            double currentCrossingTime = (double)this.linkLength / (double)(m.velocity * 100 / 6.0);
            double optimalFlowCrossingTime = (double)this.linkLength / (double)(this.optFlowMeasure.velocity * 100 / 6.0);
            return (int)Math.Max((m.flow * this.lanes * (currentCrossingTime - optimalFlowCrossingTime) / 4.0), 0); //in minutes. per 15 minuetes iteration;
        }

        private void MarkMeWithData(int measureType, CityGraph cityGraph)
        {
            this.measureType = measureType;
            cityGraph.allNodes[this.start_node].MarkMyLinkAsMeasured(this.id);
            cityGraph.allNodes[this.end_node].MarkMyLinkAsMeasured(this.id);
        }
    }
}
