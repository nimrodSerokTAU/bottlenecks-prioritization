
    using System;
    using System.IO;

namespace trafficBottlenecks
{
    public static class ReadStatic
    {
        public static CityGraph ReadStaticMap(CityGraph cityGraph)
        {
            string FilePath = string.Format(Config.INPUT_DIRECTORY_PATH + "/streetsFile.csv");
            using (StreamReader sr = new StreamReader(FilePath))
            {
                string line = sr.ReadLine();
                line = sr.ReadLine();
                while (!string.IsNullOrEmpty(line))
                {
                    string[] elements = line.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    if (elements.Length == 6)
                    {
                        string origLon = elements[0];
                        string origLat = elements[1];
                        string distLon = elements[2];
                        string distLat = elements[3];
                        int lanes = int.Parse(elements[4]);
                        int linkLength = int.Parse(elements[5]);
                        string originNodeLocKey = Utils.CalcNodeLocationKey(origLon, origLat);
                        string destNodeLocKey = Utils.CalcNodeLocationKey(distLon, distLat);

                        cityGraph.CreateNewLink(originNodeLocKey, destNodeLocKey, linkLength, lanes);
                    }
                    line = sr.ReadLine();
                }
                sr.Close();
            }
            return cityGraph;
        }
    }
}
