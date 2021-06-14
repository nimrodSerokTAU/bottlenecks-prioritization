using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace trafficBottlenecks
{
    public class Measurement
    {
        public int availability;
        public int velocity;
        public int density;
        public int flow;
        public bool isLoaded;

        public Measurement(double v)
        {
            this.velocity = (int)(Math.Round(v));
        }

        public Measurement()
        {
            this.velocity = 0;
            this.density = 0;
            this.flow = 0;
            this.availability = -1;
            this.isLoaded = false;
        }

        
        public void ComputeMyAvilability(int vf)
        {
            this.density = Utils.CalcDensityByVelocity(this.velocity, vf);
            this.flow = this.density * this.velocity;
            this.availability = (int)Math.Min(((double)this.velocity / (double)vf * 100.0), 100);
        }
    }
}
