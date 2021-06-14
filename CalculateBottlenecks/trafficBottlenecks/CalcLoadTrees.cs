using System;
using System.Collections.Generic;
using System.IO;

namespace trafficBottlenecks
{
    public class CalcLoadTrees
    {
        public Dictionary<int, CurrentLoadTree>[] allClustersPerIteration; //trunk, LoadCluster;
        public Dictionary<string, LoadTreeOverTime> allTreesOverTime;
        public Dictionary<string, LoadTreeOverTime>[] allTreesOTPerDay;
        public Dictionary<int, TrunkOverTime> allTrunksOverTime = new Dictionary<int, TrunkOverTime>();
        public Dictionary<int, TrunkOverTime>[] allTrunksOverTimePerDay;
        public Dictionary<int, double[][]> costsOnEacHourAndDay; //trunk id, hour, day
        public Dictionary<int, double[][]> branchesOnEacHourAndDay; //trunk id, hour, day
       

        public CalcLoadTrees()
        {
            allClustersPerIteration = new Dictionary<int, CurrentLoadTree>[Config.NUMBER_OF_TOTAL_ITERATIONS];
            allTreesOverTime = new Dictionary<string, LoadTreeOverTime>();
            allTreesOTPerDay = new Dictionary<string, LoadTreeOverTime>[Config.NUMBER_OF_DAYS];
            allTrunksOverTime = new Dictionary<int, TrunkOverTime>();
            allTrunksOverTimePerDay = new Dictionary<int, TrunkOverTime>[Config.NUMBER_OF_DAYS];
            costsOnEacHourAndDay = new Dictionary<int, double[][]>();
            branchesOnEacHourAndDay = new Dictionary<int, double[][]>();
            for (int i = 0; i < Config.NUMBER_OF_DAYS; i++)
            {
                allTreesOTPerDay[i] = new Dictionary<string, LoadTreeOverTime>();
                allTrunksOverTimePerDay[i] = new Dictionary<int, TrunkOverTime>();
            }
        }

        public void RunCalcLoadTreesForThisIteration(DateTime[] timestamps, CityGraph cityGraph, int iteration)
        {
            allClustersPerIteration[iteration] = new Dictionary<int, CurrentLoadTree>();
            List<int> alreadyInUsePerIteration = new List<int>();
            double hour = Utils.GetHourFromIteration(timestamps, iteration);
            int testDayInx = Utils.GetTestDayInxFromIteration(iteration);
            List<int> loadHistory = new List<int>(cityGraph.linksOfLoadTree[iteration].Keys);

            loadHistory.Sort();
            if (loadHistory.Count > 0)
            {
                int loadHistoryWorkingInx = loadHistory.Count - 1;
                int loadCounter = loadHistory[loadHistoryWorkingInx];

                while (loadHistoryWorkingInx > 0)
                {
                    if (cityGraph.linksOfLoadTree[iteration][loadCounter].Count == 0)
                    {
                        loadHistoryWorkingInx--;
                        loadCounter = loadHistory[loadHistoryWorkingInx];
                    }
                    int workingonLinkId = cityGraph.linksOfLoadTree[iteration][loadCounter][0];
                    if (!alreadyInUsePerIteration.Contains(workingonLinkId))
                    {
                        CalcOneLoadTree(loadCounter, workingonLinkId, cityGraph, iteration, alreadyInUsePerIteration);
                    }
                    else
                    {
                        cityGraph.linksOfLoadTree[iteration][loadCounter].Remove(workingonLinkId);
                    }
                }
                int dayInx = Utils.GetTestDayInxFromIteration(iteration);
                foreach (CurrentLoadTree thisTree in allClustersPerIteration[iteration].Values)
                {
                    thisTree.CalcOneLoadTreeCost(cityGraph, iteration);
                    cityGraph.allLinks[thisTree.trunk].asTrunkIterations[dayInx]++;
                    cityGraph.allLinks[thisTree.trunk].asTrunkIterations[Config.NUMBER_OF_DAYS]++;
                    thisTree.FillCurrentLoadTreeStatistics(this, hour, testDayInx, cityGraph.allLinks);
                }
            }
        }

        private void CalcOneLoadTree(int trunkLoadedFor, int trunkId, CityGraph cityGraph, int iteration, List<int> alreadyInUsePerIteration)
        {
            CurrentLoadTree thisTree = new CurrentLoadTree(trunkLoadedFor, trunkId, iteration);
            cityGraph.linksOfLoadTree[iteration][trunkLoadedFor].Remove(trunkId);
            alreadyInUsePerIteration.Add(trunkId); 
            thisTree.AddLinkToTree(new LoadedTreeBranchToBeAdded(trunkId, 0, trunkLoadedFor, false), 
                                        cityGraph, allClustersPerIteration[iteration], alreadyInUsePerIteration, -1, iteration);
            thisTree.PurnLeafs(alreadyInUsePerIteration);
            if (thisTree.allMyBranches.Keys.Count >= Config.MIN_TREE_BRANCHES_COUNT)
            {
                allClustersPerIteration[iteration].Add(thisTree.trunk, thisTree);
            }
            else
            {
                alreadyInUsePerIteration.Remove(thisTree.trunk);
            }
        }

        public void PrintLoadTrees(CityGraph cityGraph, int iteration)
        {
            string fileNameA = "_LoadClusters_" + iteration + ".txt";
            string pathA = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameA;

            string allLoadsfileName = "_allLinksLoad_" + iteration + ".txt";
            string allLoadsPath = Config.OUTPUT_DIRECTORY_PATH + "/" + allLoadsfileName;

            List<string> loadClustersOut = new List<string>
            {
                "trunk,fromLocation,toLocation,loadedFor,lanes, branchCost,wTCost,linkId,thisBranch.myMeasureType"
            };
            foreach (CurrentLoadTree lc in allClustersPerIteration[iteration].Values)
            {
                if (lc.wTCost >= Config.MIN_TREE_COST && lc.linksCount > Config.MIN_TREE_BRANCHES_COUNT)
                {
                    lc.PrintLoadTreeWithBranches(cityGraph, loadClustersOut);
                }
            }
            File.WriteAllLines(pathA, loadClustersOut);

           
            List<string> linksLoadData = new List<string>
            {
                "link_id,fromLocation,toLocation,loadedFor,lanes,measureType,costPerIter,availability, availabilityThreshold"
            };
            foreach (CityGraphLink thisLink in cityGraph.allLinks)
            {
                int loadedFor = -1;
                if (thisLink.measureType == (int)MeasureType.MEASURED)
                {
                    loadedFor = Math.Max(thisLink.lastLoadedCounter[iteration], 0);
                }
                linksLoadData.Add(string.Format("{0:00000},{1},{2:0000},{3:00},{4:0}, {5:00000}, {6:0.00}, {7:0.00}", 
                                thisLink.id, thisLink.locationKey, loadedFor, thisLink.lanes, thisLink.measureType, thisLink.costPerIter[iteration],
                                ((double)thisLink.allMeasurements[iteration].availability / 100.0),
                                (double)(thisLink.optFlowMeasure != null ? thisLink.optFlowMeasure.availability / 100.0 : 0)));
            }
            File.WriteAllLines(allLoadsPath, linksLoadData);
        }

        internal void CalcTreeOverTimeStatistics()
        {
            
            foreach(LoadTreeOverTime ltt in allTreesOverTime.Values)
            {
                ltt.calcMyStatistics(this.allClustersPerIteration);
                if (!allTrunksOverTime.ContainsKey(ltt.trunkId))
                {
                    TrunkOverTime tot = new TrunkOverTime(ltt.trunkId, -1);
                    allTrunksOverTime.Add(tot.trunkId, tot);
                }
                allTrunksOverTime[ltt.trunkId].AddTreeToMe(ltt);
            }
            for(int dayInx = 0; dayInx < Config.NUMBER_OF_DAYS; dayInx++)
            {
                foreach (LoadTreeOverTime ltt in allTreesOTPerDay[dayInx].Values)
                {
                    ltt.calcMyStatistics(this.allClustersPerIteration);
                    if (!allTrunksOverTimePerDay[dayInx].ContainsKey(ltt.trunkId))
                    {
                        TrunkOverTime tot = new TrunkOverTime(ltt.trunkId, dayInx);
                        allTrunksOverTimePerDay[dayInx].Add(tot.trunkId, tot);
                    }
                    allTrunksOverTimePerDay[dayInx][ltt.trunkId].AddTreeToMe(ltt);
                }
            }
        }
    }
}
