using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

namespace trafficBottlenecks
{
    public static class ReadRealTraffic
    {
        public static CityGraph ReadFromDirectory(DateTime[] timestamps, List<int> lastDayIterations, CityGraph cityGraph)
        {
            string workingDirectoryPath = string.Format(Config.INPUT_DIRECTORY_PATH + "/RT");
            string[] directoryEntries = Directory.GetDirectories(workingDirectoryPath);
            var directory = new DirectoryInfo(workingDirectoryPath);               
            int i = 0;
            foreach (FileInfo file in directory.GetFiles().OrderBy(fi => fi.Name))
            {
                ReadOneResFile(timestamps, lastDayIterations, cityGraph, file, i);
                i++;
            }
            return cityGraph;
        }

        static void ReadOneResFile(DateTime[] timestamps, List<int> lastDayIterations, CityGraph cityGraph, FileInfo file, int iteration)
        {
            DateTime sampleTime = Utils.CalcDateTimeFromFileString(file.Name);
            timestamps[iteration] = sampleTime;
            if (Utils.IsSampleTimeNewDay(sampleTime))
            {
                lastDayIterations.Add(iteration - 1);
            }

            using (StreamReader sr = new StreamReader(file.FullName))
            {
                string line = sr.ReadLine();
                line = sr.ReadLine();
                int inx = 0;

                while (!string.IsNullOrEmpty(line))
                {
                    string[] elements = line.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

                    double lonOrigAPI = Convert.ToDouble(elements[0]);
                    double latOrigAPI = Convert.ToDouble(elements[1]);
                    double lonDestAPI = Convert.ToDouble(elements[2]);
                    double latDestAPI = Convert.ToDouble(elements[3]);

                    int time = Convert.ToInt32(elements[4]);
 
                    string rTLocationKey = Utils.CalcLinkLocationKey(lonOrigAPI, latOrigAPI, lonDestAPI, latDestAPI);
                    if (cityGraph.allLinksRTDLocations.ContainsKey(rTLocationKey))
                    {
                        int thisLinkId = cityGraph.allLinksRTDLocations[rTLocationKey];
                        cityGraph.allLinks[thisLinkId].AddnewSample(iteration, time, cityGraph);
                    }
                    line = sr.ReadLine();
                    inx++;
                }
                sr.Close();
            }
        }
    }
}

