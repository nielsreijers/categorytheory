using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CategoryTheory {
    public class Chapter6 {
        public interface Shape {
            double area();
            double circ();
        }

        public class Circle : Shape {
            public double r;
            public double area() => Math.PI * this.r * this.r;
            public double circ() => 2 * Math.PI * this.r;
        }
        public class Rect : Shape {
            public double d, h;
            public double area() => this.d * this.h;
            public double circ() => 2 * (this.d + this.h);
        }

        public class Square : Shape {
            public double e;
            public double area() => this.e * this.e;
            public double circ() => 4 * this.e;
        }
    }
}
