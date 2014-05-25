//
//  Arbor - version 0.91
//  a graph vizualization toolkit
//
//  Copyright (c) 2011 Samizdat Drafting Co.
//  Physics code derived from springy.js, copyright (c) 2010 Dennis Hotson
// 

using System;

namespace ExtUtils.ArborEngine
{
	public class ArborPoint
	{ 
        private static readonly Random _random = new Random();

        public double x;
        public double y;

		public ArborPoint(double a, double b)
		{
			this.x = a;
			this.y = b;
		}

		public static ArborPoint rnd(double a = 5)
		{
			return new ArborPoint(2 * a * (_random.NextDouble() - 0.5), 2 * a * (_random.NextDouble() - 0.5));
		}

		public bool exploded()
		{
            return (double.IsNaN(this.x) || double.IsNaN(this.y));
        }

		public ArborPoint add(ArborPoint a)
		{
            return new ArborPoint(this.x + a.x, this.y + a.y);
        }

		public ArborPoint sub(ArborPoint a)
		{
            return new ArborPoint(this.x - a.x, this.y - a.y);
        }

		public ArborPoint mul(double a)
		{
            return new ArborPoint(this.x * a, this.y * a);
        }

		public ArborPoint div(double a)
		{
            return new ArborPoint(this.x / a, this.y / a);
        }

		public double magnitude()
		{
            return Math.Sqrt(this.x * this.x + this.y * this.y);
        }

		public ArborPoint normal()
		{
            return new ArborPoint(-this.y, this.x);
        }

		public ArborPoint normalize()
		{
            return this.div(this.magnitude());
        }
	}
}
