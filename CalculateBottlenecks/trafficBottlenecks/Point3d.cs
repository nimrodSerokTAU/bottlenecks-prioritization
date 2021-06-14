using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace trafficBottlenecks
{
    public class Point3d
    {
        public double X;
        public double Y;
        public double Z;

        public Point3d(double x, double y, double z)
        {
            X = x;
            Y = y;
            Z = z;
        }

        public Point3d(Point3d other)
        {
            X = other.X;
            Y = other.Y;
            Z = other.Z;
        }

        public double DistanceTo(Point3d other)
        {
            double deltaX = X - other.X;
            double deltaY = Y - other.Y;
            double res = Math.Sqrt(Math.Pow(deltaX, 2) + Math.Pow(deltaY, 2));
            return res;
        }

        public static Point3d CreateFromPolylineInx(string[] points, int inx)
        {
            string[] cords = points[inx].Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            return new Point3d(double.Parse(cords[1]), double.Parse(cords[0]), 0);
        }
    }
}
