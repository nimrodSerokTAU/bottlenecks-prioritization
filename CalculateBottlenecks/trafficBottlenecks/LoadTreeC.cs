using System;
using System.Collections.Generic;

namespace trafficBottlenecks
{
    public class CurrentLoadTree
    {
        public Dictionary<int, LoadedTreeBranch> allMyBranches;//linkId, LoadedTreeBranch;  // measurement: count
        public List<int> allMyLeafs;//List of linkIds //TODO MAY: handle this
        public List<int> toDirectionNodes;
        public int baseLength;
        public List<int> baseMembers;
        public int trunkLoadedFor;
        public int numberOfNodes;
        public int trunk;
        public int wTCost;  // measurement
        public int splits; // measurement
        public int startLoadIteration;
        public string uniqueKey;
        public int iteration;
        public int dayInx;
        public int costOfThisIterOnly;  // measurement
        public double avgK;  // measurement
        public int publishedTrunk;
        public int linksCount;

        public CurrentLoadTree(int trunkLoadCount, int trunkId, int iteration)
        {
            toDirectionNodes = new List<int>();
            baseMembers = new List<int> { trunkId };
            trunk = trunkId;
            baseLength = 0;
            trunkLoadedFor = trunkLoadCount;
            wTCost = 0;
            allMyBranches = new Dictionary<int, LoadedTreeBranch>();
            allMyLeafs = new List<int>();
            startLoadIteration = iteration - trunkLoadCount + 1;
            uniqueKey = string.Format("{0}_{1}", startLoadIteration, trunkId);
            this.iteration = iteration;
            this.dayInx = Utils.GetTestDayInxFromIteration(iteration);
        }

        public void AddLinkToTree(LoadedTreeBranchToBeAdded branchToBeAdded, CityGraph cityGraph, Dictionary<int, CurrentLoadTree> allClusters,
                                    List<int> alreadyInUse, int myFather, int iteration)
        {
            int linkId = branchToBeAdded.linkId;
            if (allClusters.ContainsKey(linkId)) { //this is already a tree
                CurrentLoadTree addedTree = allClusters[linkId];
                foreach (int linkKey in addedTree.allMyBranches.Keys)
                {
                    allMyBranches.Add(linkKey, addedTree.allMyBranches[linkKey]);
                }
                allMyBranches[linkId].myFatherId = myFather;
                allMyLeafs.AddRange(addedTree.allMyLeafs);
                allClusters.Remove(linkId);
            }
            else {
                CityGraphLink addedLink = cityGraph.allLinks[linkId];
                toDirectionNodes.Add(addedLink.start_node);
                toDirectionNodes.Add(addedLink.end_node);
                int fatherLoadedFor = 0;
                if (myFather > -1)
                {
                    fatherLoadedFor = allMyBranches[myFather].loadedFor;
                }

                int addedLinkLoadedFor = branchToBeAdded.loadedFor;
                LoadedTreeBranch thisBranch = new LoadedTreeBranch(linkId, addedLinkLoadedFor, fatherLoadedFor,
                                                                    addedLink.measureType, myFather,
                                                                    branchToBeAdded.emptyGen, cityGraph.allLinks[trunk].lastLoadedCounter[iteration],
                                                                    addedLink.linkLength, this.baseMembers);
                allMyBranches.Add(linkId, thisBranch);
                if (baseMembers.Contains(linkId) && addedLink.measureType == (int)MeasureType.MEASURED)
                {
                    baseLength += addedLink.linkLength;
                }

                thisBranch.SearchPotentialSons(cityGraph.allLinks[linkId], cityGraph, alreadyInUse, iteration, this.toDirectionNodes, allClusters, this.allMyLeafs);

                while (thisBranch.linkIdsToBeAdded.Count > 0)
                {
                    LoadedTreeBranchToBeAdded nextBranchToBeAdded = thisBranch.linkIdsToBeAdded[0];
                    thisBranch.linkIdsToBeAdded.RemoveAt(0);
                    if (!allMyBranches.ContainsKey(nextBranchToBeAdded.linkId))
                    {
                        AddLinkToTree(nextBranchToBeAdded, cityGraph, allClusters, alreadyInUse, linkId, iteration);
                    }
                }
            }
        }

        internal void PurnLeafs(List<int> alreadyInUse)
        {
            List<int> leafsToBeValidate = new List<int>(allMyLeafs);
            List<int> validatedLeafs = new List<int>();
            while (leafsToBeValidate.Count > 0)
            {
                int leafId = leafsToBeValidate[0];
                leafsToBeValidate.Remove(leafId);
                bool needsToBeRemoved = false;
                int fatherLoadedFor = -1;
                if (this.allMyBranches[leafId].myFatherId >= 0)
                {
                    fatherLoadedFor = this.allMyBranches[leafId].fathersLoadedFor;
                }
                switch (this.allMyBranches[leafId].myMeasureType)
                {
                    case (int)MeasureType.UNKOWN:
                        needsToBeRemoved = true;
                        break;
                    case (int)MeasureType.MEASURED:
                        if (this.allMyBranches[leafId].loadedFor == 0)
                        {
                            needsToBeRemoved = true;
                        }
                        break;
                }
                int myFatherId = this.allMyBranches[leafId].myFatherId;
                if (needsToBeRemoved && myFatherId >= 0)
                {
                    this.allMyBranches[myFatherId].mySons.Remove(leafId);
                    alreadyInUse.Remove(leafId);
                    if (this.allMyBranches[myFatherId].mySons.Count == 0)
                    {
                        leafsToBeValidate.Add(myFatherId);
                    }
                    this.allMyBranches.Remove(leafId);
                }
                else
                {
                    validatedLeafs.Add(leafId);
                }
            }
            allMyLeafs = new List<int>(validatedLeafs);
        }

        internal void FillCurrentLoadTreeStatistics(CalcLoadTrees calcLoadTrees, double hour, int testDayInx, List<CityGraphLink> allLinks)
        {
            this.publishedTrunk = this.CalcTrunkKey(allLinks);
            Dictionary<int, int> allFathersCounts = new Dictionary<int, int>();
            foreach (LoadedTreeBranch branch in this.allMyBranches.Values)
            {
                allLinks[branch.linkId].inTreeIterations[dayInx]++;
                allLinks[branch.linkId].inTreeIterations[Config.NUMBER_OF_DAYS]++;
                if (!allFathersCounts.ContainsKey(branch.myFatherId))
                {
                    allFathersCounts.Add(branch.myFatherId, 0);
                }
                allFathersCounts[branch.myFatherId]++;
            }
            this.numberOfNodes = allFathersCounts.Count;
            this.avgK = (double)allMyBranches.Count / (double)this.numberOfNodes;
            if (this.wTCost >= Config.MIN_TREE_COST && this.linksCount > Config.MIN_TREE_BRANCHES_COUNT)
            {
                string loadTreeUniqueKey = string.Format("{0}_{1}", startLoadIteration, this.publishedTrunk);
                if (!calcLoadTrees.allTreesOverTime.ContainsKey(loadTreeUniqueKey))
                {
                    calcLoadTrees.allTreesOverTime[loadTreeUniqueKey] = new LoadTreeOverTime(this.publishedTrunk, this.startLoadIteration, -1);
                }
                calcLoadTrees.allTreesOverTime[loadTreeUniqueKey].AddLoadTree(this);

                if (!calcLoadTrees.allTreesOTPerDay[this.dayInx].ContainsKey(loadTreeUniqueKey))
                {
                    calcLoadTrees.allTreesOTPerDay[this.dayInx][loadTreeUniqueKey] = new LoadTreeOverTime(this.publishedTrunk, Math.Max(this.startLoadIteration, this.dayInx * 96), this.dayInx);
                }
                calcLoadTrees.allTreesOTPerDay[this.dayInx][loadTreeUniqueKey].AddLoadTree(this);

                if (this.splits > calcLoadTrees.allTreesOverTime[loadTreeUniqueKey].maxSplits)
                {
                    calcLoadTrees.allTreesOverTime[loadTreeUniqueKey].maxSplits = this.splits;
                }
                if (this.splits > calcLoadTrees.allTreesOTPerDay[this.dayInx][loadTreeUniqueKey].maxSplits)
                {
                    calcLoadTrees.allTreesOTPerDay[this.dayInx][loadTreeUniqueKey].maxSplits = this.splits;
                }
            }
        }

        private int CalcTrunkKey(List<CityGraphLink> allLinks)
        {
            List<int> potentialTrunks = new List<int> { this.trunk };
            int smallLinksCount = 0;
            while (potentialTrunks.Count > 0)
            {
                int currentTrunk = potentialTrunks[0];
                potentialTrunks.RemoveAt(0);
                smallLinksCount++;
                this.linksCount = this.allMyBranches.Values.Count - smallLinksCount + 1;
                return currentTrunk;
            }
            this.linksCount = 0;
            return this.trunk;
        }

        internal void CalcOneLoadTreeCost(CityGraph cityGraph, int iteration)
        {
            this.splits = 0;
            foreach (int linkId in this.allMyBranches.Keys)
            {
                this.allMyBranches[linkId].cost = cityGraph.allLinks[linkId].lastLoadedW[iteration];
                cityGraph.allLinks[linkId].totalCostInMinutes[dayInx] += cityGraph.allLinks[linkId].costPerIter[iteration];
                cityGraph.allLinks[linkId].totalCostInMinutes[Config.NUMBER_OF_DAYS] += cityGraph.allLinks[linkId].costPerIter[iteration];
                wTCost += cityGraph.allLinks[linkId].lastLoadedW[iteration];
                this.costOfThisIterOnly += cityGraph.allLinks[linkId].costPerIter[iteration];
                
                if (this.allMyBranches[linkId].mySons.Count > 1)
                {
                    this.splits += this.allMyBranches[linkId].mySons.Count - 1;
                }
            }
        }

        public void PrintLoadTreeWithBranches(CityGraph cityGraph, List<string> loadClustersOut)
        {
            foreach (LoadedTreeBranch thisBranch in this.allMyBranches.Values)
            {
                int linkId = thisBranch.linkId;
                loadClustersOut.Add(string.Format("{0},{1},{2},{3},{4},{5},{6},{7}", publishedTrunk, cityGraph.allLinks[linkId].locationKey, thisBranch.loadedFor,
                    cityGraph.allLinks[linkId].lanes, cityGraph.allLinks[linkId].lastLoadedW[iteration], wTCost, linkId, thisBranch.myMeasureType));
            }
        }

        internal string PrintMe()
        {
            return string.Format("{0},{1},{2},{3},{4},{5},{6},{7}", this.publishedTrunk, this.iteration, wTCost, allMyBranches.Count,
                                                                    splits, costOfThisIterOnly, iteration - startLoadIteration, avgK);
        }
    }
}

