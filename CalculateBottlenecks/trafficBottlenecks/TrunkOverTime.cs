using System;
using System.Collections.Generic;


namespace trafficBottlenecks
{
    public class TrunkOverTime
    {
        public int trunkId;
        public int dayInx;
        public List<int> allStartIterations;
        public int[] costInMinutesPerIteration;
        public int[] branchesPerIteration;
        public int totalCost; // measurement
        public int maxCost; // measurement
        public int maxCostIterationsCount;
        public int maxBranchesIterationsCount;
        public int maxCostBranchesCount;
        public int totalIterations; // measurement
        public int maxIterations; // measurement
        public int totalBranchs; // measurement
        public int maxBranches; // measurement
        public int sumOfTempCosts; // measurement
        public int maxTempCosts; // measurement
        double maximalAvgK; // measurement

        public TrunkOverTime(int trunkId, int dayInx)
        {
            this.trunkId = trunkId;
            this.dayInx = dayInx;
            allStartIterations = new List<int>();
            totalCost = 0;
            maxCost = 0;
            totalIterations = 0;
            maxIterations = 0;
            totalBranchs = 0;
            maxBranches = 0;
            sumOfTempCosts = 0;
            maxTempCosts = 0;
            maximalAvgK = 0.0;
            costInMinutesPerIteration = new int[Config.NUMBER_OF_TOTAL_ITERATIONS];
            branchesPerIteration = new int[Config.NUMBER_OF_TOTAL_ITERATIONS];
        }

        public void AddTreeToMe(LoadTreeOverTime tree)
        {
            allStartIterations.Add(tree.startLoadIteration);
            totalCost += tree.maxCost;
            if (maxCost < tree.maxCost)
            {
                maxCostIterationsCount = tree.iterationsCount;
                maxCostBranchesCount = tree.maxBranches;
                maxCost = tree.maxCost;
            }
            totalIterations += tree.iterationsCount;
            maxIterations = Math.Max(this.maxIterations, tree.iterationsCount);
            totalBranchs += tree.maxBranches;
            if (maxBranches < tree.maxBranches)
            {
                maxBranchesIterationsCount = tree.iterationsCount;
                maxBranches = tree.maxBranches;
            }
            maximalAvgK = Math.Max(maximalAvgK, tree.maxAvgKTree);
            sumOfTempCosts += tree.sumOfCost;
            this.maxTempCosts = Math.Max(this.maxTempCosts, tree.maxTempCost);
            foreach (int iteration in tree.costInMinutesPerIteration.Keys)
            {
                costInMinutesPerIteration[iteration] = tree.costInMinutesPerIteration[iteration];
                branchesPerIteration[iteration] = tree.branchesPerIteration[iteration];
            }
        }

        public string PrintMe()
        {
            string res = string.Format("{0},{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11},{12}", trunkId, totalCost / 60.0, maxCost, totalIterations, maxIterations,
                                        totalBranchs, maxBranches, sumOfTempCosts / 60.0, maximalAvgK, allStartIterations.Count, maxCostIterationsCount, maxCostBranchesCount, maxBranchesIterationsCount);
            return res;
        }
    }
}
