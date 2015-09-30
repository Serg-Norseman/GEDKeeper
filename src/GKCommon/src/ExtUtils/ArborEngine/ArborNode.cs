//
//  Arbor - version 0.91
//  a graph vizualization toolkit
//
//  Copyright (c) 2011 Samizdat Drafting Co.
//  Physics code derived from springy.js, copyright (c) 2010 Dennis Hotson
// 

using System;
using System.Drawing;

namespace ExtUtils.ArborEngine
{
	public class ArborNode
    { 
		public string Sign;
		public object Data;

        public bool Fixed;
		public double Mass;
        public ArborPoint Pt;

        public Color Color;
		public RectangleF Box;

    	public ArborPoint v;
    	public ArborPoint f;

		public ArborNode(string sign)
		{
			this.Sign = sign;

			this.Fixed = false;
			this.Mass = 1;
			this.Pt = new ArborPoint(double.NaN, double.NaN);

			this.Color = Color.Gray;

			this.v = new ArborPoint(0, 0);
    		this.f = new ArborPoint(0, 0);
		}

		public void applyForce(ArborPoint a)
    	{
    		this.f = this.f.add(a.div(this.Mass));
    	}
    }
}
