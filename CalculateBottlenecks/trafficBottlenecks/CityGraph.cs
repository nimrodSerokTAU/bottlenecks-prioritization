using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace trafficBottlenecks
{
    public class CityGraph
    {
        public List<CityGraphLink> allLinks;
        public List<CityGraphNode> allNodes;
        public Dictionary<string, int> allLinksRTDLocations; //location key to link id
        public Dictionary<string, int> allNodesLocations;//location key to id
        

        // these are per one specific hour
        public Dictionary<int, int>[] linksOfLoadTreeDic; //<link id, number of loaded iteration until this hour>  //all links that should appear in load trees of a specific hour; <link id,number of loaded iteration until this hour> 
        public Dictionary<int, List<int>>[] linksOfLoadTree;// loadCounter until checked iteration, List<linkId> that has this load until now;


        public CityGraph()
        {
            allLinks = new List<CityGraphLink>();
            allNodes = new List<CityGraphNode>();
            allLinksRTDLocations = new Dictionary<string, int>(); // location key to link id
            allNodesLocations = new Dictionary<string, int>(); // location key to tag key

            linksOfLoadTreeDic = new Dictionary<int, int>[Config.NUMBER_OF_TOTAL_ITERATIONS];
            linksOfLoadTree = new Dictionary<int, List<int>>[Config.NUMBER_OF_TOTAL_ITERATIONS];

            for(int iteration = 0; iteration < Config.NUMBER_OF_TOTAL_ITERATIONS; iteration++)
            {
                linksOfLoadTreeDic[iteration] = new Dictionary<int, int>();
                linksOfLoadTree[iteration] = new Dictionary<int, List<int>>();
            }
        }

        public void CreateNewLink(string originNodeLocKey, string destNodeLocKey, int linkLength, int lanes)
        {
            if (!allNodesLocations.ContainsKey(originNodeLocKey))
            {
                CreateNewNode(originNodeLocKey);
            }
            if (!allNodesLocations.ContainsKey(destNodeLocKey))
            {
                CreateNewNode(destNodeLocKey);
            }
            int originNodeId = allNodesLocations[originNodeLocKey];
            int destNodeId = allNodesLocations[destNodeLocKey];
            int linkId = allLinks.Count;
            CityGraphLink newLink = new CityGraphLink(linkId, allNodes[originNodeId], allNodes[destNodeId], linkLength, lanes);
            allLinks.Add(newLink);
            allLinksRTDLocations.Add(newLink.locationKey, linkId);
            allNodes[originNodeId].AddThisLinkToMe(linkId, true);
            allNodes[destNodeId].AddThisLinkToMe(linkId, false);
        }

        public void CreateNewNode(string originNodeLocKey)
        {
            CityGraphNode newOrigNode = new CityGraphNode(allNodes.Count, originNodeLocKey);
            allNodes.Add(newOrigNode);
            allNodesLocations.Add(newOrigNode.locationKey, newOrigNode.id);
        }

        public void CalcAvailabilityPerLink()
        {
            foreach (CityGraphLink thisLink in this.allLinks)
            {
                if (thisLink.measureType == (int)MeasureType.MEASURED)
                {
                    thisLink.FillLinkAvailability(this.linksOfLoadTreeDic, this.linksOfLoadTree);
                }
            }
        }
    }
}
