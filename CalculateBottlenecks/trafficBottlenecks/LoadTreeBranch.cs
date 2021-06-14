using System;
using System.Collections.Generic;

namespace trafficBottlenecks
{
    public class LoadedTreeBranch
    {
        public int linkId;
        public List<int> mySons;
        public List<LoadedTreeBranchToBeAdded> linkIdsToBeAdded;
        public int myFatherId;
        public int fathersLoadedFor;
        public int loadedFor;
        public int emptyGeneration;
        public int myMeasureType;
        public int cost;
        public int linkLength;

        public LoadedTreeBranch(int id, int loadedFor, int fathersLoadedFor, int myMeasureType, int myFatherId,
                                                int emptyGen, int trunkLoadedFor, int linkLength, List<int> treeBase)
        {
            linkId = id;
            mySons = new List<int>();
            this.loadedFor = loadedFor;
            emptyGeneration = emptyGen;
            linkIdsToBeAdded = new List<LoadedTreeBranchToBeAdded>();
            this.myFatherId = myFatherId;
            this.fathersLoadedFor = fathersLoadedFor;
            this.myMeasureType = myMeasureType;
            if (myMeasureType == (int)MeasureType.UNKOWN)
            {
                if (myFatherId > -1)
                {
                    this.loadedFor = fathersLoadedFor;
                }
                else if (treeBase.Contains(myFatherId))
                {
                    treeBase.Add(linkId);
                }
            }
            else
            {
                if (this.loadedFor + Config.TETA >= trunkLoadedFor && this.loadedFor <= trunkLoadedFor)
                {
                    treeBase.Add(linkId);
                }
            }
            this.linkLength = linkLength;
            cost = 0;
        }

        public void SearchPotentialSons(CityGraphLink myLink, CityGraph cityGraph, List<int> alreadyInUse, int iteration, List<int> treeNodes, Dictionary<int, CurrentLoadTree> allClusters, List<int> treeLeafsList)
        {
            bool foundASon = false;
            foreach (int potentialSonId in cityGraph.allNodes[myLink.start_node].toMe)
            {
                if (!alreadyInUse.Contains(potentialSonId))
                {
                    if (emptyGeneration < Config.EMPTY_LOAD_ITERATION_GAP_FOR_CAUSALITY)
                    {
                        CityGraphLink potentialSonLink = cityGraph.allLinks[potentialSonId];
                        if (!treeNodes.Contains(potentialSonLink.start_node) || !treeNodes.Contains(potentialSonLink.end_node))
                        {
                            LoadedTreeBranchToBeAdded potentialSonBranchToAdd = IsOtherBrachLoadCanBeConnected(potentialSonLink, iteration);
                            if (potentialSonBranchToAdd != null)
                            {
                                mySons.Add(potentialSonId);
                                linkIdsToBeAdded.Add(potentialSonBranchToAdd);
                                foundASon = true;
                                if (!alreadyInUse.Contains(potentialSonId))
                                {
                                    alreadyInUse.Add(potentialSonId);
                                }
                            }
                        }
                    }
                }
                else if (allClusters.ContainsKey(potentialSonId))
                {
                    CurrentLoadTree otherTree = allClusters[potentialSonId];
                    LoadedTreeBranchToBeAdded potentialSonBranchToAdd = IsOtherTreeCanBeConnected(otherTree);
                    if (potentialSonBranchToAdd != null)
                    { //connect entire cluster:
                        mySons.Add(potentialSonId);
                        linkIdsToBeAdded.Add(potentialSonBranchToAdd);
                        foundASon = true;
                    }
                }
            }
            if (!foundASon)
            {
                treeLeafsList.Add(linkId);
            }
        }

        public LoadedTreeBranchToBeAdded IsOtherBrachLoadCanBeConnected(CityGraphLink potentialSon, int iteration)
        {
            int potentialSonLoadedFor = potentialSon.lastLoadedCounter[iteration];
            int potentialSonLength = potentialSon.linkLength;

            switch (potentialSon.measureType)
            {
                case (int)MeasureType.MEASURED:
                    if (potentialSonLoadedFor <= loadedFor)
                    {
                        if (loadedFor > 0 && loadedFor <= potentialSonLoadedFor + Config.TETA)
                            return new LoadedTreeBranchToBeAdded(potentialSon.id, 0, potentialSonLoadedFor, false);
                    }
                    return null;
                case (int)MeasureType.UNKOWN:
                    if (potentialSonLoadedFor <= loadedFor)
                    {
                        if (emptyGeneration <= Config.EMPTY_LOAD_ITERATION_GAP_FOR_CAUSALITY)
                            return new LoadedTreeBranchToBeAdded(potentialSon.id, this.emptyGeneration + 1, loadedFor, false);
                    }
                    return null;
                default: return new LoadedTreeBranchToBeAdded(potentialSon.id, this.emptyGeneration + 1, potentialSonLoadedFor, true);
            }
        }

        public LoadedTreeBranchToBeAdded IsOtherTreeCanBeConnected(CurrentLoadTree otherTree)
        {
            if (otherTree.trunkLoadedFor <= loadedFor && loadedFor <= otherTree.trunkLoadedFor + Config.TETA)
            {
                return new LoadedTreeBranchToBeAdded(otherTree.trunk, 0, otherTree.trunkLoadedFor, false);
            }
            return null;
        }
    }

    public class LoadedTreeBranchToBeAdded
    {
        public int linkId;
        public int emptyGen;
        public int loadedFor;
        public bool lengthGrace;

        public LoadedTreeBranchToBeAdded(int id, int emptyGen, int loadedFor, bool lengthGrace)
        {
            linkId = id;
            this.emptyGen = emptyGen;
            this.loadedFor = loadedFor;
            this.lengthGrace = lengthGrace;
        }
    }
}
