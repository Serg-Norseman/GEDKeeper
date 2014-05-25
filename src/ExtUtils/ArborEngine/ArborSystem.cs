//
//  Arbor - version 0.91
//  a graph vizualization toolkit
//
//  Copyright (c) 2011 Samizdat Drafting Co.
//  Physics code derived from springy.js, copyright (c) 2010 Dennis Hotson
// 

using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;

namespace ExtUtils.ArborEngine
{
	//public class Particle {}
	//public class Spring {}


	public class PSBounds
	{
		public ArborPoint topleft = null;
		public ArborPoint bottomright = null;
		
		public PSBounds(ArborPoint topleft, ArborPoint bottomright)
		{
			this.topleft = topleft;
			this.bottomright = bottomright;
		}
	}

    public class ArborSystem : BaseObject
	{
        private static readonly Random _random = new Random();

		private Size usz;
		private double mag = 0.04;
		private int[] margins = new int[4] {20, 20, 20, 20};
		private PSBounds n_bnd = null;
		private PSBounds o_bnd = null;

		private IArborRenderer c_renderer;
		private System.Timers.Timer itv = null;
		private DateTime tm = DateTime.FromBinary(0);
		private bool busy = false;

		private ArborPoint gdt_topleft = new ArborPoint(-1, -1);
		private ArborPoint gdt_bottomright = new ArborPoint(1, 1);
		private double theta = 0.4;

		public double energy_sum = 0;
		public double energy_max = 0;
		public double energy_mean = 0;
		public double energy_threshold = 0;

		public double param_repulsion = 1000; // отражение, отвращение
		public double param_stiffness = 600; // церемонность :)
		public double param_friction = 0.5; // трение
		public double param_dt = 0.01; // 0.02;
		public bool param_gravity = false;
		public double param_precision = 0.6;
		public double param_timeout = 1000 / 50;

		private Hashtable fNames = new Hashtable();
		private List<ArborNode> fNodes = new List<ArborNode>();
		private List<ArborEdge> fEdges = new List<ArborEdge>();

		private EventHandler fOnStart;
        private EventHandler fOnStop;

        #region Properties

        public List<ArborNode> Nodes
        {
        	get { return this.fNodes; }
        }

        public List<ArborEdge> Edges
        {
        	get { return this.fEdges; }
        }

        public event EventHandler OnStart
		{
			add {
				this.fOnStart = value;
			}
			remove {
				if (this.fOnStart == value) {
					this.fOnStart = null;
				}
			}
		}

        public event EventHandler OnStop
		{
			add {
				this.fOnStop = value;
			}
			remove {
				if (this.fOnStop == value) {
					this.fOnStop = null;
				}
			}
		}

        #endregion

        // repulsion - отталкивание, stiffness - тугоподвижность, friction - сила трения
		public ArborSystem(double repulsion, double stiffness, double friction, IArborRenderer renderer)
		{
			this.param_repulsion = repulsion;
			this.param_stiffness = stiffness;
			this.param_friction = friction;
			this.c_renderer = renderer;
		}

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.stop();
            }
            base.Dispose(disposing);
        }

        private void physicsUpdate(object sender, System.Timers.ElapsedEventArgs e)
		{
			if (this.busy) return;
			this.busy = true;
			try
			{
				this.tick();
				this.updateBounds();

				if (c_renderer != null) {
					c_renderer.redraw();
				}

				if (energy_threshold <= /*0.05*/ 0.7) {
					if (tm == DateTime.FromBinary(0)) {
						tm = DateTime.Now;
					}
					TimeSpan ts = DateTime.Now - tm;
					if (ts.TotalMilliseconds > 1000) {
						this.stop();
					}
				} else {
					tm = DateTime.FromBinary(0);
				}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("physicsUpdate(): " + ex.Message);
			}
			this.busy = false;
		}

		public void start()
		{
			if (fOnStart != null) fOnStart(this, new EventArgs());
			
			if (itv != null) {
				return;
			}
			tm = DateTime.FromBinary(0);

            itv = new System.Timers.Timer();
            itv.AutoReset = true;
            itv.Interval = param_timeout;
            itv.Elapsed += new System.Timers.ElapsedEventHandler(this.physicsUpdate);
            itv.Start();
		}

		public void stop()
		{
			if (itv != null) {
                itv.Stop();
                itv.Dispose();
				itv = null;
			}

			if (fOnStop != null) fOnStop(this, new EventArgs());
		}

		public ArborNode addNode(string sign, ArborPoint pt)
		{
			ArborNode node = this.getNode(sign);

			if (node != null) {
				return node;
			} else {
				node = new ArborNode(sign);
				node.Pt = pt;
				
				fNames.Add(sign, node);
				fNodes.Add(node);

				return node;
			}
		}

		public ArborNode addNode(string sign)
		{
			double xx = gdt_topleft.x + (gdt_bottomright.x - gdt_topleft.x) * _random.NextDouble();
			double yy = gdt_topleft.y + (gdt_bottomright.y - gdt_topleft.y) * _random.NextDouble();
			ArborPoint pt = new ArborPoint(xx, yy);

			return this.addNode(sign, pt);
		}

		public ArborNode getNode(string sign)
		{
			return (ArborNode)fNames[sign];
		}

		public ArborEdge addEdge(string srcSign, string tgtSign, int len = 1)
		{
			ArborNode src = this.getNode(srcSign);
			src = (src != null) ? src : this.addNode(srcSign);
			
			ArborNode tgt = this.getNode(tgtSign);
			tgt = (tgt != null) ? tgt : this.addNode(tgtSign);

			ArborEdge x = null;
			if (src != null && tgt != null) {
				foreach (ArborEdge edge in fEdges) {
					if (edge.Source == src && edge.Target == tgt) {
						x = edge;
						break;
					}
				}
			}

			if (x == null) {
				x = new ArborEdge(src, tgt, len, param_stiffness);
				fEdges.Add(x);
			}

			return x;
		}

		public void setScreenSize(int width, int height)
		{
			usz.Width = width;
			usz.Height = height;
			this.updateBounds();
		}

		public ArborPoint toScreen(ArborPoint pt)
		{
			if (n_bnd == null) return null;

			ArborPoint v = n_bnd.bottomright.sub(n_bnd.topleft);
			double sx = margins[3] + pt.sub(n_bnd.topleft).div(v.x).x * (usz.Width - (margins[1] + margins[3]));
			double sy = margins[0] + pt.sub(n_bnd.topleft).div(v.y).y * (usz.Height - (margins[0] + margins[2]));
			return new ArborPoint(sx, sy);
		}

		/*private ArborPoint fromScreen(int sx, int sy)
		{
			if (n_bnd == null) return null;

			ArborPoint x = n_bnd.bottomright.sub(n_bnd.topleft);
			double w = (sx - margins[3]) / (usz.Width - (margins[1] + margins[3])) * x.x + n_bnd.topleft.x;
			double v = (sy - margins[0]) / (usz.Height - (margins[0] + margins[2])) * x.y + n_bnd.topleft.y;
			return new ArborPoint(w, v);
		}*/

		private void updateBounds()
		{
			try
			{
                if (usz == null) return;

				o_bnd = this.getActualBounds();

                ArborPoint obr = new ArborPoint(o_bnd.bottomright.x, o_bnd.bottomright.y);
                ArborPoint otl = new ArborPoint(o_bnd.topleft.x, o_bnd.topleft.y);
                ArborPoint sz = obr.sub(otl);
                ArborPoint cent = otl.add(sz.div(2));

                double x = 4.0;
                ArborPoint D = new ArborPoint(Math.Max(sz.x, x), Math.Max(sz.y, x));
                o_bnd.topleft = cent.sub(D.div(2));
                o_bnd.bottomright = cent.add(D.div(2));

                if (n_bnd == null) {
                    n_bnd = o_bnd;
                    return;
                }

                ArborPoint _nb_BR = n_bnd.bottomright.add(o_bnd.bottomright.sub(n_bnd.bottomright).mul(mag));
                ArborPoint _nb_TL = n_bnd.topleft.add(o_bnd.topleft.sub(n_bnd.topleft).mul(mag));

                ArborPoint A = new ArborPoint(n_bnd.topleft.sub(_nb_TL).magnitude(), n_bnd.bottomright.sub(_nb_BR).magnitude());

                if (A.x * usz.Width > 1 || A.y * usz.Height > 1) {
                	n_bnd = new PSBounds(_nb_TL, _nb_BR);
                    return;
                } else {
                    return;
                }
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("_updateBounds(): " + ex.Message);
			}
		}

		private PSBounds getActualBounds()
		{
			ArborPoint tl = null;
			ArborPoint br = null;

			foreach (ArborNode node in this.fNodes)
			{
				ArborPoint pt = node.Pt;
				if (pt.exploded()) continue;

				if (br == null) {
					br = new ArborPoint(pt.x, pt.y);
					tl = new ArborPoint(pt.x, pt.y);
					continue;
				}

				if (pt.x < tl.x) tl.x = pt.x;
				if (pt.y < tl.y) tl.y = pt.y;
				if (pt.x > br.x) br.x = pt.x;
				if (pt.y > br.y) br.y = pt.y;
			}

			if (br != null && tl != null) {
				tl.x -= 1.2;
				tl.y -= 1.2;
				br.x += 1.2;
				br.y += 1.2;
				return new PSBounds(tl, br);
			} else {
				return new PSBounds(new ArborPoint(-1, -1), new ArborPoint(1, 1));
			}
		}

		private void tick()
		{
			try
			{
				// tend particles
				foreach (ArborNode p in fNodes) {
					p.v.x = 0;
					p.v.y = 0;
				}

				this.eulerIntegrator();

				// update geometry
				//for (int i = 0; i <= fNodes.Count - 1; i++) {}
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("tick(): " + ex.Message);
			}
		}

		private void eulerIntegrator()
		{
			if (param_repulsion > 0) {
				if (theta > 0) {
					this.applyBarnesHutRepulsion();
				} else {
					this.applyBruteForceRepulsion();
				}
			}

			if (param_stiffness > 0) {
				this.applySprings();
			}

			this.applyCenterDrift();

			if (param_gravity) {
				this.applyCenterGravity();
			}

			this.updateVelocity(param_dt);
			this.updatePosition(param_dt);
		}

		private void applyBruteForceRepulsion()
		{
			foreach (ArborNode p in fNodes) {
				foreach (ArborNode r in fNodes) {
					if (p != r) {
						ArborPoint u = p.Pt.sub(r.Pt);
						double v = Math.Max(1, u.magnitude());
						ArborPoint t = ((u.magnitude() > 0) ? u : ArborPoint.rnd(1)).normalize();
						p.applyForce(t.mul(param_repulsion * r.Mass * 0.5).div(v * v * 0.5));
						r.applyForce(t.mul(param_repulsion * p.Mass * 0.5).div(v * v * -0.5));
					}
				}
			}
		}

		private void applyBarnesHutRepulsion()
		{
			BarnesHutTree bht = new BarnesHutTree(gdt_topleft, gdt_bottomright, theta);

			foreach (ArborNode r in fNodes) {
				bht.insert(r);
			}

			foreach (ArborNode r in fNodes) {
				bht.applyForces(r, param_repulsion);
			}
		}

		private void applySprings()
		{
			foreach (ArborEdge spr in fEdges) {
				ArborPoint s = spr.Target.Pt.sub(spr.Source.Pt);

				double q = spr.Length - s.magnitude();
				ArborPoint r = ((s.magnitude() > 0) ? s : ArborPoint.rnd(1)).normalize();

				spr.Source.applyForce(r.mul(spr.Stiffness * q * -0.5));
				spr.Target.applyForce(r.mul(spr.Stiffness * q * 0.5));
			}
		}

		private void applyCenterDrift()
		{
			double q = 0.0;
			ArborPoint r = new ArborPoint(0, 0);
			foreach (ArborNode s in fNodes) {
				r.add(s.Pt);
				q++;
			}

			if (q == 0) return;

			ArborPoint p = r.div(-q);
			foreach (ArborNode s in fNodes) {
				s.applyForce(p);
			}
		}

		private void applyCenterGravity()
		{
			foreach(ArborNode p in fNodes) {
				ArborPoint q = p.Pt.mul(-1);
				p.applyForce(q.mul(param_repulsion / 100));
			}
		}

		private void updateVelocity(double p)
		{
			foreach (ArborNode q in fNodes) {
				if (q.Fixed) {
					q.v = new ArborPoint(0, 0);
					q.f = new ArborPoint(0, 0);
					continue;
				}
				q.v = q.v.add(q.f.mul(p)).mul(1 - param_friction);
				q.f.x = q.f.y = 0;
				double r = q.v.magnitude();
				if (r > 1000) {
					q.v = q.v.div(r * r);
				}
			}
		}

		private void updatePosition(double q)
		{
			double r = 0;
			double p = 0;
			double u = 0;
			ArborPoint br = null;
			ArborPoint tl = null;

			foreach (ArborNode v in fNodes)
			{
				v.Pt = v.Pt.add(v.v.mul(q));
				double x = v.v.magnitude();
				double z = x * x;
				r += z;
				p = Math.Max(z, p);
				u++;

				if (br == null) {
					br = new ArborPoint(v.Pt.x, v.Pt.y);
					tl = new ArborPoint(v.Pt.x, v.Pt.y);
					continue;
				}

				ArborPoint y = v.Pt;
				if (y.exploded()) continue;

				if (y.x > br.x) br.x = y.x;
				if (y.y > br.y) br.y = y.y;
				if (y.x < tl.x) tl.x = y.x;
				if (y.y < tl.y) tl.y = y.y;
			}

			energy_sum = r;
			energy_max = p;
			energy_mean = r / u;
			energy_threshold = (energy_mean) /* + energy_max) / 2*/;

			gdt_topleft = (tl != null) ? tl : new ArborPoint(-1, -1);
			gdt_bottomright = (br != null) ? br : new ArborPoint(1, 1);
		}
    }

}
