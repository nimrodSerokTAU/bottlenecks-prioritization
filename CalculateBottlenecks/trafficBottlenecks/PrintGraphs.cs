using System;
using System.Collections.Generic;
using System.IO;

namespace trafficBottlenecks
{
    public static class Printer
    {
        public static void PrintGraphs(Calculation calculation, CityGraph cityGraph, CalcLoadTrees calcLoadTrees)
        {
            PrintTrunkOverTime(cityGraph.allLinks, calcLoadTrees);

            PrintGraphsForCurrentTreesRes(calcLoadTrees, 0, Config.NUMBER_OF_TOTAL_ITERATIONS, "_G");
 
            for (int i = 0; i < Config.NUMBER_OF_DAYS; i++)
            {
                int startIteration = i > 0 ? calculation.lastDayIterations[i - 1] + 1 : 0;
                int endIteration = (i != Config.NUMBER_OF_DAYS - 1) ? calculation.lastDayIterations[i] : Config.NUMBER_OF_TOTAL_ITERATIONS;
                PrintGraphsForCurrentTreesRes(calcLoadTrees, startIteration, endIteration, "_day" + i);
            }
 
            MapTrunksOTPerDay(new List<int>(calcLoadTrees.allTrunksOverTime.Keys), calcLoadTrees.allTrunksOverTimePerDay);
        }

        static void PrintGraphsForCurrentTreesRes(CalcLoadTrees calcLoadTrees, int startIteration, int endIteration, string nameEx)
        {
            string fileNameH = "_currentTrees" + nameEx + ".csv";
            _ = File.Create(fileNameH);
            string pathH = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameH;
            List<string> currentTrees = new List<string>
            {
                "trunkId, iteration, wTCost, branchesCount, splits, costOfThisIterOnly, loadedFor, avgK"
            };
            for (int iteration = startIteration; iteration < endIteration; iteration++)
            {
                {
                    foreach (CurrentLoadTree clt in calcLoadTrees.allClustersPerIteration[iteration].Values)
                    {
                        if (clt.wTCost >= Config.MIN_TREE_COST && clt.linksCount > Config.MIN_TREE_BRANCHES_COUNT)
                        {
                            currentTrees.Add(clt.PrintMe());
                        }
                    }
                }
            }
            File.WriteAllLines(pathH, currentTrees);
        }
        static void PrintTrunkOverTime(List<CityGraphLink> allLinks, CalcLoadTrees calcLoadTrees)
        {
            string fileName = "_TrunkOverTime.csv";
            string path = Config.OUTPUT_DIRECTORY_PATH + "/" + fileName;
            List<string> trunkOverTimeRes = new List<string>
            {
                "trunkId, totalCost, maxCost, totalIterations, maxIter, totalBranches, maxBranches, sumOfTempCosts, maximalAvgK, " +
                "count, maxCostIterationsCount, maxCostBranchesCount, maxBranchesIterationsCount"
            };
            foreach (TrunkOverTime TT in calcLoadTrees.allTrunksOverTime.Values)
            {
                trunkOverTimeRes.Add(TT.PrintMe());
            }
            File.WriteAllLines(path, trunkOverTimeRes);
        }
        static void MapTrunksOTPerDay(List<int> allTrunksOverTimeKeys, Dictionary<int, TrunkOverTime>[] allTrunksOverTimePerDay)
        {
            string fileNameA  = "_TrunksOTpDay_totalCost.csv";
            string fileNameAt = "_TrunksOTpDay_maxCost.csv";
            string fileNameB  = "_TrunksOTpDay_totalIterations.csv";
            string fileNameBt = "_TrunksOTpDay_maxIterations.csv";
            string fileNameC  = "_TrunksOTpDay_totalBranchs.csv";
            string fileNameCt = "_TrunksOTpDay_maxBranchs.csv";
            string fileNameD  = "_TrunksOTpDay_sumOfTempCosts.csv";
            string fileNameDt = "_TrunksOTpDay_maxCostsPerIteration.csv";
            
            string pathA  = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameA;
            string pathB  = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameB;
            string pathC  = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameC;
            string pathD  = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameD;
            string pathAt = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameAt;
            string pathBt = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameBt;
            string pathCt = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameCt;
            string pathDt = Config.OUTPUT_DIRECTORY_PATH + "/" + fileNameDt;

            string title = string.Empty;
            for (int dayInx = 0; dayInx < Config.NUMBER_OF_DAYS; dayInx++)
            {
                title += string.Format("day{0},", dayInx + 1);
            }

            List<string> treeOTTotalCostPerDay = new List<string>() { title };
            List<string> treeOTMaxCostPerDay = new List<string>() { title };
            List<string> treeOTTotalIterationsPerDay = new List<string>() { title };
            List<string> treeOTMaxIterationsPerDay = new List<string>() { title };
            List<string> treeOTTotalBranchsPerDay = new List<string>() { title };
            List<string> treeOTMaxBranchsPerDay = new List<string>() { title };
            List<string> treeOTSumOfTempCostsPerDay = new List<string>() { title };
            List<string> treeOTMaxCostsPerIterationsPerDay = new List<string>() { title };

            foreach (int trunk in allTrunksOverTimeKeys)
            {
                string trunkStringA  = string.Empty;
                string trunkStringAt = string.Empty;
                string trunkStringB  = string.Empty;
                string trunkStringBt = string.Empty;
                string trunkStringC  = string.Empty;
                string trunkStringCt = string.Empty;
                string trunkStringD  = string.Empty;
                string trunkStringDt = string.Empty;
                for (int dayInx = 0; dayInx < Config.NUMBER_OF_DAYS; dayInx++)
                {
                    if (allTrunksOverTimePerDay[dayInx].ContainsKey(trunk))
                    {
                        trunkStringA += (allTrunksOverTimePerDay[dayInx][trunk].totalCost) + ",";
                        trunkStringAt += (allTrunksOverTimePerDay[dayInx][trunk].maxCost) + ",";
                        trunkStringB  += (allTrunksOverTimePerDay[dayInx][trunk].totalIterations) + ",";
                        trunkStringBt += (allTrunksOverTimePerDay[dayInx][trunk].maxIterations) + ",";
                        trunkStringC  += (allTrunksOverTimePerDay[dayInx][trunk].totalBranchs) + ",";
                        trunkStringCt += (allTrunksOverTimePerDay[dayInx][trunk].maxBranches) + ",";
                        trunkStringD  += (allTrunksOverTimePerDay[dayInx][trunk].sumOfTempCosts) + ",";
                        trunkStringDt += (allTrunksOverTimePerDay[dayInx][trunk].maxTempCosts) + ",";
                    }
                    else
                    {
                        trunkStringA += "0,";
                        trunkStringAt += "0,";
                        trunkStringB  += "0,";
                        trunkStringBt += "0,";
                        trunkStringC  += "0,";
                        trunkStringCt += "0,";
                        trunkStringD  += "0,";
                        trunkStringDt += "0,";
                    }
                }
                treeOTTotalCostPerDay.Add(trunkStringA);
                treeOTMaxCostPerDay.Add(trunkStringAt);
                treeOTTotalIterationsPerDay.Add(trunkStringB);
                treeOTMaxIterationsPerDay.Add(trunkStringBt);
                treeOTTotalBranchsPerDay.Add(trunkStringC);
                treeOTMaxBranchsPerDay.Add(trunkStringCt);
                treeOTSumOfTempCostsPerDay.Add(trunkStringD);
                treeOTMaxCostsPerIterationsPerDay.Add(trunkStringDt);
            }
            File.WriteAllLines(pathA,  treeOTTotalCostPerDay);
            File.WriteAllLines(pathAt, treeOTMaxCostPerDay);
            File.WriteAllLines(pathB,  treeOTTotalIterationsPerDay);
            File.WriteAllLines(pathBt, treeOTMaxIterationsPerDay);
            File.WriteAllLines(pathC,  treeOTTotalBranchsPerDay);
            File.WriteAllLines(pathCt, treeOTMaxBranchsPerDay);
            File.WriteAllLines(pathD,  treeOTSumOfTempCostsPerDay);
            File.WriteAllLines(pathDt, treeOTMaxCostsPerIterationsPerDay);
        }
    }
}
