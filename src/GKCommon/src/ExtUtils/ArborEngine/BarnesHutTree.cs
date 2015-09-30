//
//  Arbor - version 0.91
//  a graph vizualization toolkit
//
//  Copyright (c) 2011 Samizdat Drafting Co.
//  Physics code derived from springy.js, copyright (c) 2010 Dennis Hotson
// 

using System;
using System.Collections.Generic;
using GKCommon;

namespace ExtUtils.ArborEngine
{
	public class Branch
	{
		public ArborPoint origin = null;
		public ArborPoint size = null;
		public object[] q = new object[4] { null, null, null, null };
		public double mass = 0.0;
		public ArborPoint pt = null;
	}

	public class BarnesHutTree
	{
        private static readonly Random _random = new Random();

        private const int q_ne = 0;
        private const int q_nw = 1;
        private const int q_se = 2;
        private const int q_sw = 3;
        private const int q_none = 4;

		private readonly Branch root;
		private readonly double d = 0.5;

		public BarnesHutTree(ArborPoint origin, ArborPoint h, double d)
		{
			this.d = d;
			this.root = new Branch();
			root.origin = origin;
			root.size = h.sub(origin);
		}

		private static int getQuad(ArborNode i, Branch f)
		{
			try
			{
				if (i.Pt.exploded()) {
					return q_none;
				}
				ArborPoint h = i.Pt.sub(f.origin);
				ArborPoint g = f.size.div(2);
				if (h.y < g.y) {
					if (h.x < g.x) {
						return q_nw;
					} else {
						return q_ne;
					}
				} else {
					if (h.x < g.x) {
						return q_sw;
					} else {
						return q_se;
					}
				}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("BarnesHutTree.getQuad(): " + ex.Message);
				return q_none;
			}
		}

		public void insert(ArborNode j)
		{
			try
			{
				Branch f = root;
				List<ArborNode> gst = new List<ArborNode>();
				gst.Add(j);
				while (gst.Count > 0) {
					ArborNode h = gst[0];
					gst.RemoveAt(0);

					double m = h.Mass;
					int p = getQuad(h, f);
					object fp = f.q[p];

					if (fp == null) {
						f.q[p] = h;

						f.mass += m;
						if (f.pt != null) {
							f.pt = f.pt.add(h.Pt.mul(m));
						} else {
							f.pt = h.Pt.mul(m);
						}
					} else {
						if (fp is Branch) {
							f.mass += (m);
							if (f.pt != null) {
								f.pt = f.pt.add(h.Pt.mul(m));
							} else {
								f.pt = h.Pt.mul(m);
							}

							f = fp as Branch;

							gst.Insert(0, h);
						} else {
							ArborPoint l = f.size.div(2);
							ArborPoint n = new ArborPoint(f.origin.x, f.origin.y);

							if (p == q_se || p == q_sw) {
								n.y += l.y;
							}
							if (p == q_ne || p == q_se) {
								n.x += l.x;
							}

							ArborNode o = fp as ArborNode;
							fp = new Branch();
							(fp as Branch).origin = n;
							(fp as Branch).size = l;
							f.q[p] = fp;

							f.mass = m;
							f.pt = h.Pt.mul(m);

							f = fp as Branch;

							if (o.Pt.x == h.Pt.x && o.Pt.y == h.Pt.y) {
								double k = l.x * 0.08;
								double i = l.y * 0.08;
								o.Pt.x = Math.Min(n.x + l.x, Math.Max(n.x, o.Pt.x - k / 2 + _random.NextDouble() * k));
								o.Pt.y = Math.Min(n.y + l.y, Math.Max(n.y, o.Pt.y - i / 2 + _random.NextDouble() * i));
							}

							gst.Add(o);
							gst.Insert(0, h);
						}
					}
				}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("BarnesHutTree.insert(): " + ex.Message);
			}
		}

		public void applyForces(ArborNode m, double g)
		{
			try
			{
				List<object> f = new List<object>();
				f.Add(root);
				while (f.Count > 0) {
					object node = f[0];
					f.RemoveAt(0); //shift();

					if (node == null || node == m) continue;

					ArborPoint k;
					double l;
					ArborPoint i;
					if (node is ArborNode) {
						ArborNode p_node = (node as ArborNode);
						k = m.Pt.sub(p_node.Pt);
						l = Math.Max(1, k.magnitude());
						i = ((k.magnitude() > 0) ? k : ArborPoint.rnd(1)).normalize();
						m.applyForce(i.mul(g * p_node.Mass).div(l * l));
					} else {
						Branch b_node = (node as Branch);
						double j = m.Pt.sub(b_node.pt.div(b_node.mass)).magnitude();
						double h = Math.Sqrt(b_node.size.x * b_node.size.y);
						if (h / j > d) {
							f.Add(b_node.q[q_ne]);
							f.Add(b_node.q[q_nw]);
							f.Add(b_node.q[q_se]);
							f.Add(b_node.q[q_sw]);
						} else {
							k = m.Pt.sub(b_node.pt.div(b_node.mass));
							l = Math.Max(1, k.magnitude());
							i = ((k.magnitude() > 0) ? k : ArborPoint.rnd(1)).normalize();
							m.applyForce(i.mul(g * (b_node.mass)).div(l * l));
						}
					}
				}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("BarnesHutTree.applyForces(): " + ex.Message);
			}
		}
	}

}
