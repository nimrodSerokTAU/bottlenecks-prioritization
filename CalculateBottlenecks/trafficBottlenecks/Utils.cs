using System;

namespace trafficBottlenecks
{
    public static class Utils
    {
        public const double VELOCITY_MULTIPLIER = 3.6; // 2.23694//mph; 3.6//km/h
        public const int MINUTES_IN_DAY = 1440;
        public static string GetLocationKey(double location)
        {
            return location.ToString("000.0000000");
        }

        public static string CalcLinkLocationKey(double originLocX, double originLocY, double destLocX, double destLocY)
        {
            string originLocationKey = Utils.GetLocationKey(originLocX) + "_" + Utils.GetLocationKey(originLocY);
            string destLocationKey = Utils.GetLocationKey(destLocX) + "_" + Utils.GetLocationKey(destLocY);
            return originLocationKey + "," + destLocationKey;
        }

        public static string CalcNodeLocationKey(string lon, string lat)
        {
            return (Utils.GetLocationKey(double.Parse(lon)) + "_" + Utils.GetLocationKey(double.Parse(lat)));
        }

        internal static bool IsSampleTimeNewDay(DateTime sampleTime)
        {
            // assumes an iteration is less than an hour;
            return (sampleTime.Hour == 0 && sampleTime.Minute < Utils.MINUTES_IN_DAY / Config.NUMBER_OF_DAILY_ITERATIONS);
        }

        public static DateTime CalcDateTimeFromFileString(string fromFile)
        {
            string[] elements = fromFile.Split("_".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            int dayInx = int.Parse(elements[0]);
            int hours = int.Parse(elements[1]);
            string[] minutesPart = elements[2].Split(".".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            int minutes = (int)(Math.Round((double)(int.Parse(minutesPart[0]) / 15))) * 15;
            DateTime t = new DateTime(Config.TEST_YEAR, Config.TEST_MONTH, Config.datesToTest[dayInx], hours, minutes, 0);
            return t;
        }

        internal static int GetTestDayInxFromIteration(int iteration)
        {
            return (int)Math.Floor(iteration / (double)Config.NUMBER_OF_DAILY_ITERATIONS);
        }

        internal static double GetHourFromIteration(DateTime[] timestamps, int iteration)
        {
            int hour = timestamps[iteration].Hour;
            int minutes = timestamps[iteration].Minute;
            return (double)(hour + (double)(minutes / 60));
        }

        public static int CalcDensityByVelocity(double v, double vf)
        {
            switch (Config.macroscopicModel)
            {
                case (int)MacroscopicModel.GREENSHIELD:
                    return (int)(Config.MAX_ROAD_DENSITY * (1 - (Math.Min(v / vf, 1))));
                case (int)MacroscopicModel.MAY:
                    return (int)(Config.MAX_ROAD_DENSITY * Math.Pow((1.0 - Math.Pow(v, (1.0 - Config.M)) / Math.Pow(vf, (1.0 - Config.M))), (1.0 / (Config.L - 1.0))));

            }
        }
    }
}
