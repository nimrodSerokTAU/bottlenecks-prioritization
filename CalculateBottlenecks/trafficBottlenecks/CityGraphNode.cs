using System.Collections.Generic;

namespace trafficBottlenecks
{
    public class CityGraphNode
    {
        public int id;
        public string osmTag;
        public List<int> fromMe;
        public List<int> toMe;
        public string locationKey;
        public List<int> missingLinks;

        public CityGraphNode(int id, string locationKey)
        {
            this.id = id;
            fromMe = new List<int>();
            toMe = new List<int>();
            this.locationKey = locationKey;
            missingLinks = new List<int>();
        }

        public string PrintCityGraphNode(Dictionary<string, int> allNodesLocations, int nodeId)
        {
            allNodesLocations.Add(locationKey, nodeId);
            string res = string.Format("{0},{1}", this.id, locationKey);
            return res;
        }

        internal void AddThisLinkToMe(int linkId, bool isFromMe)
        {
            if (isFromMe)
            {
                fromMe.Add(linkId);
            }
            else
            {
                toMe.Add(linkId);
            }
            missingLinks.Add(linkId);
        }

        internal void MarkMyLinkAsMeasured(int linkId)
        {
            this.missingLinks.Remove(linkId);
        }
    }
}