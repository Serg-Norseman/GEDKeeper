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
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Windows.Forms;

namespace Ext.ArborEngine
{

	public class ArbPoint
	{ 
        private static Random _random = new Random();

        public double x;
        public double y;

		public ArbPoint(double a, double b)
		{
			this.x = a;
			this.y = b;
		}

		public static ArbPoint rnd(double a = 5)
		{
			return new ArbPoint(2 * a * (_random.NextDouble() - 0.5), 2 * a * (_random.NextDouble() - 0.5));
		}

		public bool exploded()
		{
            return (double.IsNaN(this.x) || double.IsNaN(this.y));
        }

		public ArbPoint add(ArbPoint a)
		{
            return new ArbPoint(this.x + a.x, this.y + a.y);
        }

		public ArbPoint sub(ArbPoint a)
		{
            return new ArbPoint(this.x - a.x, this.y - a.y);
        }

		public ArbPoint mul(double a)
		{
            return new ArbPoint(this.x * a, this.y * a);
        }

		public ArbPoint div(double a)
		{
            return new ArbPoint(this.x / a, this.y / a);
        }

		public double magnitude()
		{
            return Math.Sqrt(this.x * this.x + this.y * this.y);
        }

		public ArbPoint normal()
		{
            return new ArbPoint(-this.y, this.x);
        }

		public ArbPoint normalize()
		{
            return this.div(this.magnitude());
        }
	}



	public class Node
    { 
    	private static int nextNodeId = 1;

        public int id;
		public string sign;
        public double mass;
        public bool _fixed;
        public ArbPoint pt;
		public Color color;
		public object data;

		public Node(string sign)
		{
			this.id = nextNodeId++;
			this.sign = sign;
			this.mass = 1;
			this._fixed = false;
			this.pt = new ArbPoint(double.NaN, double.NaN);
			this.color = Color.Gray;
			this.data = null;
		}

    }



	public class Edge
    {
    	private static int nextEdgeId = -1;

    	public int id;
    	public Node source;
    	public Node target;
    	public double length;
    	public bool directed;
    	public object data;

    	public Edge(Node b, Node c, double len = 1)
    	{
    		this.id = nextEdgeId--;
    		this.source = b;
    		this.target = c;
    		this.length = len;
    		this.directed = true;
    	}
    }



	public class Particle
    { 
    	public int id;
    	public ArbPoint p;
    	public double m;
    	public ArbPoint v;
    	public ArbPoint f;
    	public bool _fixed;

    	public Particle(int id, ArbPoint a, double b, bool f) {
    		this.id = id;
    		this.p = a;
    		this.m = b;
    		this.v = new ArbPoint(0, 0);
    		this.f = new ArbPoint(0, 0);
    		this._fixed = f;
    	}

    	public void applyForce(ArbPoint a) {
    		this.f = this.f.add(a.div(this.m));
    	}
    }



	public class Spring
    {
    	public Particle point1;
    	public Particle point2;
    	public double length;
    	public double k;

    	public Spring(Particle c, Particle b, double d, double a)
    	{
    		this.point1 = c;
    		this.point2 = b;
    		this.length = d;
    		this.k = a;
    	}
    }



	public class Branch
	{
		public ArbPoint origin = null;
		public ArbPoint size = null;
		public object[] q = new object[4] { null, null, null, null };
		public double mass = 0.0;
		public ArbPoint pt = null;
	}



	public class BarnesHutTree
	{
        private static Random _random = new Random();

        private static int q_ne = 0;
		private static int q_nw = 1;
		private static int q_se = 2;
		private static int q_sw = 3;
		private static int q_none = 4;

		private Branch root = null;
		private double d = 0.5;

		public BarnesHutTree(ArbPoint g, ArbPoint h, double f)
		{
			this.d = f;
			this.root = new Branch();
			root.origin = g;
			root.size = h.sub(g);
		}

		private int getQuad(Particle i, Branch f)
		{
			try
			{
				if (i.p.exploded()) {
					return q_none;
				}
				ArbPoint h = i.p.sub(f.origin);
				ArbPoint g = f.size.div(2);
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
				Trace.WriteLine("BarnesHutTree.getQuad(): " + ex.Message);
				return q_none;
			}
		}

		public void insert(Particle j)
		{
			try
			{
				Branch f = root;
				List<object> gst = new List<object>();
				gst.Add(j);
				while (gst.Count > 0) {
					Particle h = gst[0] as Particle;
					gst.RemoveAt(0);

					double m = h.m;
					int p = this.getQuad(h, f);
					object fp = f.q[p];

					if (fp == null) {
						f.q[p] = h;

						f.mass += m;
						if (f.pt != null) {
							f.pt = f.pt.add(h.p.mul(m));
						} else {
							f.pt = h.p.mul(m);
						}
					} else {
						if (fp is Branch) {
							f.mass += (m);
							if (f.pt != null) {
								f.pt = f.pt.add(h.p.mul(m));
							} else {
								f.pt = h.p.mul(m);
							}

							f = fp as Branch;

							gst.Insert(0, h);
						} else {
							ArbPoint l = f.size.div(2);
							ArbPoint n = new ArbPoint(f.origin.x, f.origin.y);

							if (p == q_se || p == q_sw) {
								n.y += l.y;
							}
							if (p == q_ne || p == q_se) {
								n.x += l.x;
							}

							Particle o = fp as Particle;
							fp = new Branch();
							(fp as Branch).origin = n;
							(fp as Branch).size = l;
							f.q[p] = fp;

							f.mass = m;
							f.pt = h.p.mul(m);

							f = fp as Branch;

							if (o.p.x == h.p.x && o.p.y == h.p.y) {
								double k = l.x * 0.08;
								double i = l.y * 0.08;
								o.p.x = Math.Min(n.x + l.x, Math.Max(n.x, o.p.x - k / 2 + _random.NextDouble() * k));
								o.p.y = Math.Min(n.y + l.y, Math.Max(n.y, o.p.y - i / 2 + _random.NextDouble() * i));
							}
							gst.Add(o);
							gst.Insert(0, h);
						}
					}
				}
			}
			catch (Exception ex)
			{
				Trace.WriteLine("BarnesHutTree.insert(): " + ex.Message);
			}
		}

		public void applyForces(Particle m, double g)
		{
			try
			{
				List<object> f = new List<object>();
				f.Add(root);
				while (f.Count > 0) {
					object node = f[0];
					f.RemoveAt(0); //shift();

					if (node == null || node == m) continue;

					ArbPoint k;
					double l;
					ArbPoint i;
					if (node is Particle) {
						Particle p_node = (node as Particle);
						k = m.p.sub(p_node.p);
						l = Math.Max(1, k.magnitude());
						i = ((k.magnitude() > 0) ? k : ArbPoint.rnd(1)).normalize();
						m.applyForce(i.mul(g * p_node.m).div(l * l));
					} else {
						Branch b_node = (node as Branch);
						double j = m.p.sub(b_node.pt.div(b_node.mass)).magnitude();
						double h = Math.Sqrt(b_node.size.x * b_node.size.y);
						if (h / j > d) {
							f.Add(b_node.q[q_ne]);
							f.Add(b_node.q[q_nw]);
							f.Add(b_node.q[q_se]);
							f.Add(b_node.q[q_sw]);
						} else {
							k = m.p.sub(b_node.pt.div(b_node.mass));
							l = Math.Max(1, k.magnitude());
							i = ((k.magnitude() > 0) ? k : ArbPoint.rnd(1)).normalize();
							m.applyForce(i.mul(g * (b_node.mass)).div(l * l));
						}
					}
				}
			}
			catch (Exception ex)
			{
				Trace.WriteLine("BarnesHutTree.applyForces(): " + ex.Message);
			}
		}
	}



	public interface IArborRenderer
	{
		void redraw();
	}



	public class PSBounds
	{
		public ArbPoint topleft = null;
		public ArbPoint bottomright = null;
		
		public PSBounds(ArbPoint topleft, ArbPoint bottomright)
		{
			this.topleft = topleft;
			this.bottomright = bottomright;
		}
	}


	public delegate void ArborEventHandler(object sender, EventArgs eArgs);

	public class ArborSystem
	{
        private static Random _random = new Random();

		private Size usz;
		private double mag = 0.04;
		private int[] margins = new int[4] {20, 20, 20, 20};
		private PSBounds n_bnd = null;
		private PSBounds o_bnd = null;

		private IArborRenderer c_renderer;
		private Hashtable c_names = new Hashtable();
		private System.Timers.Timer itv = null;
		private DateTime tm = DateTime.FromBinary(0);

		private List<Particle> c_particles = new List<Particle>();
		private List<Spring> c_springs = new List<Spring>();
		private ArbPoint gdt_topleft = new ArbPoint(-1, -1);
		private ArbPoint gdt_bottomright = new ArbPoint(1, 1);
		private double theta = 0.4;

		public double energy_sum = 0;
		public double energy_max = 0;
		public double energy_mean = 0;
		public double energy_threshold = 0;

		private bool busy = false;

		public double param_repulsion = 1000; // отражение, отвращение
		public double param_stiffness = 600; // церемонность :)
		public double param_friction = 0.5; // трение
		public double param_dt = 0.01; // 0.02;
		public bool param_gravity = false;
		public double param_precision = 0.6;
		public double param_timeout = 1000 / 50;

		public List<Node> c_nodes = new List<Node>();
		public List<Edge> c_edges = new List<Edge>();

		private ArborEventHandler FOnStart;
		private ArborEventHandler FOnStop;
		
		public event ArborEventHandler OnStart
		{
			add {
				this.FOnStart = value;
			}
			remove {
				if (this.FOnStart == value) {
					this.FOnStart = null;
				}
			}
		}

		public event ArborEventHandler OnStop
		{
			add {
				this.FOnStop = value;
			}
			remove {
				if (this.FOnStop == value) {
					this.FOnStop = null;
				}
			}
		}

		public ArborSystem(double repulsion, double stiffness, double friction, IArborRenderer renderer)
		{
			this.param_repulsion = repulsion;
			this.param_stiffness = stiffness;
			this.param_friction = friction;
			this.c_renderer = renderer;
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
				Trace.WriteLine("physicsUpdate(): " + ex.Message);
			}
			this.busy = false;
		}

		private System.Timers.Timer setInterval()
		{
			System.Timers.Timer timer = new System.Timers.Timer();
			timer.AutoReset = true;
			timer.Interval = param_timeout;
			timer.Elapsed += new System.Timers.ElapsedEventHandler(this.physicsUpdate);
			timer.Start();
			return timer;
		}

		private void clearInterval(System.Timers.Timer timer)
		{
			if (timer != null) timer.Stop();
		}

		public void start()
		{
			if (FOnStart != null) FOnStart(this, new EventArgs());

			if (itv != null) {
				return;
			}
			tm = DateTime.FromBinary(0);
			itv = setInterval();
		}

		public void stop()
		{
			if (itv != null) {
				clearInterval(itv);
				itv = null;
			}

			if (FOnStop != null) FOnStop(this, new EventArgs());
		}

		public Node addNode(string w)
		{
			Node node = this.getNode(w);

			if (node != null) {
				return node;
			} else {
				node = new Node(w);
				c_names.Add(w, node);
				c_nodes.Add(node);

				this.addParticle(node.id, node.mass, double.NaN, double.NaN, node._fixed);
				return node;
			}
		}

		public Node getNode(string v)
		{
			return (Node)c_names[v];
		}

		public Edge addEdge(string z, string A, int len = 1)
		{
			Node src = this.getNode(z);
			src = (src != null) ? src : this.addNode(z);
			
			Node tgt = this.getNode(A);
			tgt = (tgt != null) ? tgt : this.addNode(A);

			Edge x = null;
			if (src != null && tgt != null) {
				foreach (Edge edge in c_edges) {
					if (edge.source == src && edge.target == tgt) {
						x = edge;
						break;
					}
				}
			}

			if (x == null) {
				x = new Edge(src, tgt, len);
				c_edges.Add(x);
				this.addSpring(x.id, src.id, tgt.id, x.length);
			}
			return x;
		}

		public void setScreenSize(int width, int height)
		{
			usz.Width = width;
			usz.Height = height;
			this.updateBounds();
		}

		public ArbPoint toScreen(ArbPoint pt)
		{
			if (n_bnd == null) return null;

			ArbPoint v = n_bnd.bottomright.sub(n_bnd.topleft);
			double sx = margins[3] + pt.sub(n_bnd.topleft).div(v.x).x * (usz.Width - (margins[1] + margins[3]));
			double sy = margins[0] + pt.sub(n_bnd.topleft).div(v.y).y * (usz.Height - (margins[0] + margins[2]));
			return new ArbPoint(sx, sy);
		}

		public ArbPoint fromScreen(int sx, int sy)
		{
			if (n_bnd == null) return null;

			ArbPoint x = n_bnd.bottomright.sub(n_bnd.topleft);
			double w = (sx - margins[3]) / (usz.Width - (margins[1] + margins[3])) * x.x + n_bnd.topleft.x;
			double v = (sy - margins[0]) / (usz.Height - (margins[0] + margins[2])) * x.y + n_bnd.topleft.y;
			return new ArbPoint(w, v);
		}

		private void updateBounds()
		{
			try
			{
                if (usz == null) return;

				o_bnd = this.getBounds();

                ArbPoint z = new ArbPoint(o_bnd.bottomright.x, o_bnd.bottomright.y);
                ArbPoint y = new ArbPoint(o_bnd.topleft.x, o_bnd.topleft.y);
                ArbPoint B = z.sub(y);
                ArbPoint v = y.add(B.div(2));
                double x = 4.0;

                ArbPoint D = new ArbPoint(Math.Max(B.x, x), Math.Max(B.y, x));
                o_bnd.topleft = v.sub(D.div(2));
                o_bnd.bottomright = v.add(D.div(2));

                if (n_bnd == null) {
                    n_bnd = o_bnd;
                    return;
                }

                ArbPoint _nb_BR = n_bnd.bottomright.add(o_bnd.bottomright.sub(n_bnd.bottomright).mul(mag));
                ArbPoint _nb_TL = n_bnd.topleft.add(o_bnd.topleft.sub(n_bnd.topleft).mul(mag));

                ArbPoint A = new ArbPoint(n_bnd.topleft.sub(_nb_TL).magnitude(), n_bnd.bottomright.sub(_nb_BR).magnitude());

                if (A.x * usz.Width > 1 || A.y * usz.Height > 1) {
                	n_bnd = new PSBounds(_nb_TL, _nb_BR);
                    return;
                } else {
                    return;
                }
			}
			catch (Exception ex)
			{
				Trace.WriteLine("_updateBounds(): " + ex.Message);
			}
		}

		private PSBounds getBounds()
		{
			ArbPoint tl = null;
			ArbPoint br = null;

			foreach (Node node in c_nodes)
			{
				ArbPoint pt = node.pt;
				if (pt.exploded()) continue;

				if (br == null) {
					br = new ArbPoint(pt.x, pt.y);
					tl = new ArbPoint(pt.x, pt.y);
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
				return new PSBounds(new ArbPoint(-1, -1), new ArbPoint(1, 1));
			}
		}

		private void addParticle(int id, double m, double x, double y, bool f)
		{
			double xx = gdt_topleft.x + (gdt_bottomright.x - gdt_topleft.x) * _random.NextDouble();
			double yy = gdt_topleft.y + (gdt_bottomright.y - gdt_topleft.y) * _random.NextDouble();
			ArbPoint r = new ArbPoint(!double.IsNaN(x) ? x : xx, !double.IsNaN(y) ? y : yy);
			c_particles.Add(new Particle(id, r, m, f));
		}

		private void dropParticle(Particle s)
		{
			c_particles.Remove(s);
		}

		private Particle getParticle(int id)
		{
			foreach (Particle x in c_particles) {
				if (x.id == id) {
					return x;
				}
			}
			return null;
		}

		private void addSpring(int id, int fm_id, int to_id, double len)
		{
			Particle r = this.getParticle(fm_id);
			Particle q = this.getParticle(to_id);
			if (r != null && q != null) {
				c_springs.Add(new Spring(r, q, len, param_stiffness));
			}
		}

		private void dropSpring(Spring s)
		{
			c_springs.Remove(s);
		}

		private void tick()
		{
			try
			{
				// tend particles
				foreach (Particle p in c_particles) {
					p.v.x = 0;
					p.v.y = 0;
				}

				this.eulerIntegrator();

				// update geometry
				for (int i = 0; i <= c_particles.Count - 1; i++) {
					Particle q = c_particles[i];
					c_nodes[i].pt.x = q.p.x;
					c_nodes[i].pt.y = q.p.y;
				}
			}
			catch (Exception ex)
			{
				Trace.WriteLine("tick(): " + ex.Message);
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
			foreach (Particle p in c_particles) {
				foreach (Particle r in c_particles) {
					if (p != r) {
						ArbPoint u = p.p.sub(r.p);
						double v = Math.Max(1, u.magnitude());
						ArbPoint t = ((u.magnitude() > 0) ? u : ArbPoint.rnd(1)).normalize();
						p.applyForce(t.mul(param_repulsion * r.m * 0.5).div(v * v * 0.5));
						r.applyForce(t.mul(param_repulsion * p.m * 0.5).div(v * v * -0.5));
					}
				}
			}
		}

		private void applyBarnesHutRepulsion()
		{
			BarnesHutTree bht = new BarnesHutTree(gdt_topleft, gdt_bottomright, theta);

			foreach (Particle r in c_particles) {
				bht.insert(r);
			}

			foreach (Particle r in c_particles) {
				bht.applyForces(r, param_repulsion);
			}
		}

		private void applySprings()
		{
			foreach (Spring p in c_springs) {
				ArbPoint s = p.point2.p.sub(p.point1.p);
				double q = p.length - s.magnitude();
				ArbPoint r = ((s.magnitude() > 0) ? s : ArbPoint.rnd(1)).normalize();
				p.point1.applyForce(r.mul(p.k * q * -0.5));
				p.point2.applyForce(r.mul(p.k * q * 0.5));
			}
		}

		private void applyCenterDrift()
		{
			double q = 0.0;
			ArbPoint r = new ArbPoint(0, 0);
			foreach (Particle s in c_particles) {
				r.add(s.p);
				q++;
			}

			if (q == 0) return;

			ArbPoint p = r.div(-q);
			foreach(Particle s in c_particles) {
				s.applyForce(p);
			}
		}

		private void applyCenterGravity()
		{
			foreach(Particle p in c_particles) {
				ArbPoint q = p.p.mul(-1);
				p.applyForce(q.mul(param_repulsion / 100));
			}
		}

		private void updateVelocity(double p)
		{
			foreach (Particle q in c_particles) {
				if (q._fixed) {
					q.v = new ArbPoint(0, 0);
					q.f = new ArbPoint(0, 0);
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
			ArbPoint t = null;
			ArbPoint s = null;

			foreach (Particle v in c_particles) {
				v.p = v.p.add(v.v.mul(q));
				double x = v.v.magnitude();
				double z = x * x;
				r += z;
				p = Math.Max(z, p);
				u++;

				if (t == null) {
					t = new ArbPoint(v.p.x, v.p.y);
					s = new ArbPoint(v.p.x, v.p.y);
					continue;
				}

				ArbPoint y = v.p;
				if (y.exploded()) continue;

				if (y.x > t.x) t.x = y.x;
				if (y.y > t.y) t.y = y.y;
				if (y.x < s.x) s.x = y.x;
				if (y.y < s.y) s.y = y.y;
			}

			energy_sum = r;
			energy_max = p;
			energy_mean = r / u;
			energy_threshold = (energy_mean) /* + energy_max) / 2*/;

			gdt_topleft = (s != null) ? s : new ArbPoint(-1, -1);
			gdt_bottomright = (t != null) ? t : new ArbPoint(1, 1);
		}

	}



	public sealed class ArborViewer : Panel, IDisposable, IArborRenderer
	{
		//public Bitmap FBuffer;
		private Font FDrawFont;
		private ArborSystem FSys;

		public ArborSystem Sys
		{
			get { return this.FSys; }
		}

		public ArborViewer()
		{
			base.BorderStyle = BorderStyle.Fixed3D;
			base.DoubleBuffered = true;
			base.TabStop = true;
			base.BackColor = Color.White;

			FSys = new ArborSystem(10000, 1000, 0.1, this);
			FSys.setScreenSize(this.Width, this.Height);

			this.FDrawFont = new Font("Calibri", 9);
			this.Resize += new EventHandler(this.av_Resize);
		}

		private void av_Resize(object sender, EventArgs e)
		{
			FSys.setScreenSize(this.Width, this.Height);
			(this as IArborRenderer).redraw();
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
                FDrawFont.Dispose();
			}
			base.Dispose(Disposing);
		}

		protected override void OnPaint(PaintEventArgs pe)
		{
			Graphics bufx = pe.Graphics;

			try
			{
				bufx.SmoothingMode = SmoothingMode.AntiAlias;
				bufx.Clear(Color.White);

				RectangleF[] nodeBoxes = new RectangleF[FSys.c_nodes.Count];
				for (int i = 0; i <= FSys.c_nodes.Count - 1; i++)
				{
					Node node = FSys.c_nodes[i];
					SizeF tsz = bufx.MeasureString(node.sign, FDrawFont);
					float w = tsz.Width + 10;
					ArbPoint pt = FSys.toScreen(node.pt);
					pt.x = Math.Floor(pt.x);
					pt.y = Math.Floor(pt.y);

					bufx.FillRectangle(new SolidBrush(node.color), (float)pt.x - w / 2, (float)pt.y - 10, w, 20);
					nodeBoxes[i] = new RectangleF((float)pt.x - w / 2, (float)pt.y - 11, w, 22);

					bufx.DrawString(node.sign, FDrawFont, new SolidBrush(Color.White), (float)pt.x - tsz.Width / 2, (float)pt.y - tsz.Height / 2);
				}

				for (int i = 0; i <= FSys.c_edges.Count - 1; i++)
				{
					Edge edge = FSys.c_edges[i];

					ArbPoint pt1 = FSys.toScreen(edge.source.pt);
					ArbPoint pt2 = FSys.toScreen(edge.target.pt);

					int n1 = FSys.c_nodes.IndexOf(edge.source);
					int n2 = FSys.c_nodes.IndexOf(edge.target);

					ArbPoint tail = intersect_line_box(pt1, pt2, nodeBoxes[n1]);
					ArbPoint head = (tail == null) ? null : intersect_line_box(tail, pt2, nodeBoxes[n2]);

					if (head != null && tail != null) {
						using (Pen p = new Pen(Color.Gray, 1))
						{
							p.StartCap = LineCap.NoAnchor;
							p.EndCap = LineCap.ArrowAnchor;
							bufx.DrawLine(p, (int)tail.x, (int)tail.y, (int)head.x, (int)head.y);
						}
					}
				}

				// this is debug, don't delete
				//string energy = "max=" + FSys.energy_max + ", mean=" + FSys.energy_mean + ", thres=" + FSys.energy_threshold;
				//bufx.DrawString(energy, FDrawFont, new SolidBrush(Color.Black), 10, 10);
			} catch (Exception ex) {
				Trace.WriteLine("redraw(): " + ex.Message);
			}
		}

		void IArborRenderer.redraw()
		{
			this.Invalidate();
		}

		public ArbPoint intersect_line_line(ArbPoint p1, ArbPoint p2, ArbPoint p3, ArbPoint p4)
		{
			double denom = ((p4.y - p3.y) * (p2.x - p1.x) - (p4.x - p3.x) * (p2.y - p1.y));
			if (denom == 0) return null; // lines are parallel
			double ua = ((p4.x - p3.x) * (p1.y - p3.y) - (p4.y - p3.y) * (p1.x - p3.x)) / denom;
			double ub = ((p2.x - p1.x) * (p1.y - p3.y) - (p2.y - p1.y) * (p1.x - p3.x)) / denom;

			if (ua < 0 || ua > 1 || ub < 0 || ub > 1) return null;
			return new ArbPoint(p1.x + ua * (p2.x - p1.x), p1.y + ua * (p2.y - p1.y));
		}

		public ArbPoint intersect_line_box(ArbPoint p1, ArbPoint p2, RectangleF boxTuple)
		{
			double bx = boxTuple.X;
			double by = boxTuple.Y;
			double bw = boxTuple.Width;
			double bh = boxTuple.Height;

			ArbPoint tl = new ArbPoint(bx, by);
			ArbPoint tr = new ArbPoint(bx + bw, by);
			ArbPoint bl = new ArbPoint(bx, by + bh);
			ArbPoint br = new ArbPoint(bx + bw, by + bh);

			ArbPoint pt;

			pt = intersect_line_line(p1, p2, tl, tr);
			if (pt != null) return pt;

			pt = intersect_line_line(p1, p2, tr, br);
			if (pt != null) return pt;

			pt = intersect_line_line(p1, p2, br, bl);
			if (pt != null) return pt;

			pt = intersect_line_line(p1, p2, bl, tl);
			if (pt != null) return pt;

			return null;
		}

		public void start()
		{
			FSys.start();
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);
			if (!this.Focused) base.Focus();
		}

		public RectangleF getNodeRect(Graphics bufx, Node node)
		{
			SizeF tsz = bufx.MeasureString(node.sign, FDrawFont);
			float w = tsz.Width + 10;
			ArbPoint pt = FSys.toScreen(node.pt);
			pt.x = Math.Floor(pt.x);
			pt.y = Math.Floor(pt.y);

			return new RectangleF((float)pt.x - w / 2, (float)pt.y - 11, w, 22);
		}

		public void doSample()
		{
			FSys.addEdge("1", "4", 2);
			FSys.addEdge("1", "12");
			FSys.addEdge("4", "21");
			FSys.addEdge("4", "23");
			FSys.addEdge("7", "34");
			FSys.addEdge("7", "13");
			FSys.addEdge("7", "44");
			FSys.addEdge("12", "25");
			FSys.addEdge("12", "24");
			FSys.addEdge("23", "50");
			FSys.addEdge("23", "53");
			FSys.addEdge("24", "6");
			FSys.addEdge("24", "42");
			FSys.addEdge("25", "94");
			FSys.addEdge("25", "66");
			FSys.addEdge("32", "47");
			FSys.addEdge("32", "84");
			FSys.addEdge("42", "32");
			FSys.addEdge("42", "7");
			FSys.addEdge("50", "72");
			FSys.addEdge("50", "65");
			FSys.addEdge("53", "67");
			FSys.addEdge("53", "68");
			FSys.addEdge("66", "79");
			FSys.addEdge("66", "80");
			FSys.addEdge("67", "88");
			FSys.addEdge("67", "83");
			FSys.addEdge("68", "77");
			FSys.addEdge("68", "91");
			FSys.addEdge("80", "99");
			FSys.addEdge("80", "97");
			FSys.addEdge("88", "110");
			FSys.addEdge("88", "104");
			FSys.addEdge("91", "106");
			FSys.addEdge("91", "100");
		}
	}

}
