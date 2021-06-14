
using System.Collections.Generic;

namespace trafficBottlenecks
{
    enum MacroscopicModel { GREENSHIELD = 0, MAY = 1 };
    public static class Config
    {

        public const int NUMBER_OF_TOTAL_ITERATIONS = 23;
        public const int NUMBER_OF_DAILY_ITERATIONS = 96;
        public const double AVAILABILITY_THRESHOLD = 0.5;
        public const int MAX_ROAD_DENSITY = 150;
        public const int MAX_LANES_IN_RUN = 7;
        public const int NUMBER_OF_DAYS = 1;
        public const int TEST_YEAR = 2021;
        public const int TEST_MONTH = 2;
        public const double AVAILABILITY_PERCENTILE = 0.95;

        internal static int[] datesToTest = new int[] { 7,8,9 }; // dates of the tested month
        
        internal static string INPUT_DIRECTORY_PATH = string.Format("../../../inputFiles");
        internal static string OUTPUT_DIRECTORY_PATH = string.Format("../../../outputFiles");


        public const int TETA = 2; // the number of iterations' gap between loaded street to a street that leads to it, in order to suggests load causality
        public const int EMPTY_LOAD_ITERATION_GAP_FOR_CAUSALITY = 2; // the maximal number of empty branches allowed until a measured branch 
        public const int MIN_TREE_COST = 60; // minutes                                               
        public const int MIN_TREE_BRANCHES_COUNT = 2;

        public const int macroscopicModel = (int)MacroscopicModel.MAY;
        public const bool useShuffle = false; // Null model

        public const double M = 0.8;
        public const double L = 2.8;
    }
}
