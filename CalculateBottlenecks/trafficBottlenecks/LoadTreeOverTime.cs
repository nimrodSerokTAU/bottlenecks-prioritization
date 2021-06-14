using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace trafficBottlenecks
{
    public class LoadTreeOverTime
    {
        public int trunkId;
        public int startLoadIteration;
        public string uniqueKey;
        public Dictionary<int, int> costInMinutesPerIteration;
        public Dictionary<int, int> branchesPerIteration;
        public Dictionary<int, Dictionary<int, int>> costInMinutesPerIterationPerBranch;
        public int maxSplits;
        public int maxCost; // measurement
        public int sumOfCost; // measurement
        public int iterationsCount; // measurement
        public int maxBranches; // measurement
        public int iterationsOnMaxBranches;
        public int totalBrnaches; // measurement
        public double avgK; // measurement
        public double maxAvgKTree; // measurement
        public int totalNodes;
        public int dayInx;//for LoadTreeOTPerDay;
        public int maxTempCost;

        public LoadTreeOverTime(int trunk, int startLoadIteration, int dayInx)
        {
            this.dayInx = dayInx;
            this.trunkId = trunk;
            this.startLoadIteration = startLoadIteration;
            costInMinutesPerIteration = new Dictionary<int, int>();
            branchesPerIteration = new Dictionary<int, int>();
            costInMinutesPerIterationPerBranch = new Dictionary<int, Dictionary<int, int>>();
            uniqueKey = string.Format("{0}_{1}", startLoadIteration, trunkId);
            maxSplits = 0;
            maxCost = 0;
            sumOfCost = 0;
            totalBrnaches = 0;
            totalNodes = 0;
            maxTempCost = 0;
        }

        internal void AddLoadTree(CurrentLoadTree thisTree)
        {
            costInMinutesPerIteration[thisTree.iteration] = (int)thisTree.wTCost;
            branchesPerIteration[thisTree.iteration] = thisTree.allMyBranches.Count;
            this.sumOfCost += thisTree.costOfThisIterOnly;
            this.maxCost = Math.Max(maxCost, thisTree.wTCost);
            if (this.maxBranches < thisTree.allMyBranches.Count)
            {
                this.iterationsOnMaxBranches = thisTree.iteration - this.startLoadIteration;
                this.maxBranches = thisTree.allMyBranches.Count;
            }
            this.totalBrnaches += thisTree.allMyBranches.Count;
            this.maxAvgKTree = Math.Max(this.maxAvgKTree, thisTree.avgK);
            this.maxTempCost = Math.Max(this.maxTempCost, thisTree.costOfThisIterOnly);
            totalNodes += thisTree.numberOfNodes;
        }

        internal void calcMyStatistics(Dictionary<int, CurrentLoadTree>[] allClusters)
        {
            int maxCostIter = -1;
            List<int> iterations = new List<int>(costInMinutesPerIteration.Keys);
            iterations.Sort();
            int lastIteratin = iterations[iterations.Count - 1];
            iterationsCount = lastIteratin - startLoadIteration + 1;
            foreach (int iteratonKey in costInMinutesPerIteration.Keys)
            {
                if (costInMinutesPerIteration[iteratonKey] == maxCost)
                {
                    maxCostIter = iteratonKey;
                }
            }
        }

        public string PrintMe()
        {
            this.avgK = (double)this.totalBrnaches / (double)this.totalNodes;
            string res = string.Format("{0},{1},{2},{3},{4},{5},{6},{7},{8}", trunkId, maxCost / 60.0, sumOfCost / 60.0, iterationsCount, 
                                    maxBranches, totalBrnaches, avgK, maxAvgKTree, iterationsOnMaxBranches);
            return res;
        }
    }
}
