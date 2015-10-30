using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Security.Permissions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;

namespace GKUI.Charts
{
	public class AncestorsCircleOptions
	{
		private AncestorsCircle fOwner;

		public Color[] BrushColor = new Color[11];

		public AncestorsCircleOptions(AncestorsCircle owner)
		{
			this.fOwner = owner;

			this.BrushColor[ 0] = Color.Coral;
			this.BrushColor[ 1] = Color.CadetBlue;
			this.BrushColor[ 2] = Color.DarkGray;
			this.BrushColor[ 3] = Color.Khaki;
			this.BrushColor[ 4] = Color./*CadetBlue;*/LawnGreen;
			this.BrushColor[ 5] = Color./*DarkGray;*/Khaki;
			this.BrushColor[ 6] = Color./*Khaki;*/HotPink;
			this.BrushColor[ 7] = Color./*CadetBlue;*/Ivory;

			this.BrushColor[ 8] = Color.Black; // text
			this.BrushColor[ 9] = Color.Moccasin; // background
			this.BrushColor[10] = Color.Black; // lines
		}

		public void Apply()
		{
			this.fOwner.Changed();
		}
	}


    public delegate void ARootChangedEventHandler(object sender, GEDCOMIndividualRecord person);

	public class AncestorsCircle : CustomChart
	{
		private class PersonSegment
		{
			public readonly int Gen;
			public readonly GraphicsPath Path;

			public GEDCOMIndividualRecord IRec;
			public int rad;
			public float v;

			public PersonSegment(int generation)
			{
				this.Gen = generation;
				this.Path = new GraphicsPath();
				this.IRec = null;
			}
		}

		private static readonly object EventRootChanged;

		private const float PI = 3.1415926535897931f;
		private const int CenterRad = 90;

		private System.ComponentModel.IContainer components = null;
		private int fCenterX;
		private int fCenterY;
		private SolidBrush[] fCircleBrushes = new SolidBrush[11];
		private SolidBrush[] fDarkBrushes = new SolidBrush[11];
		private Font fFont;
		private int fGenWidth;
		private string fHint;
		private int fIndividualsCount;
		private int fOffsetX = 0;
		private int fOffsetY = 0;
		private AncestorsCircleOptions fOptions;
		private GEDCOMIndividualRecord fRootPerson;
		private List<PersonSegment> fSegments;
		private PersonSegment fSelected;
		private ToolTip fToolTip;
		private GEDCOMTree fTree;


		public AncestorsCircleOptions Options
		{
			get { return this.fOptions; }
		}

		public int IndividualsCount
		{
			get { return this.fIndividualsCount; }
		}

		public GEDCOMIndividualRecord RootPerson
		{
			get {
				return this.fRootPerson;
			}
			set {
				this.fRootPerson = value;

				this.NavAdd(value);
				this.Changed();

				this.DoRootChanged(value);
			}
		}

		public int GenWidth
		{
			get {
				return this.fGenWidth;
			}
			set {
				if (value < 20 || value > 100) return;
				
				this.fGenWidth = value;
				this.Changed();
			}
		}
		
		public event ARootChangedEventHandler RootChanged
		{
			add { base.Events.AddHandler(AncestorsCircle.EventRootChanged, value); }
			remove { base.Events.RemoveHandler(AncestorsCircle.EventRootChanged, value); }
		}

        static AncestorsCircle()
        {
            AncestorsCircle.EventRootChanged = new object();
        }

		public AncestorsCircle(GEDCOMTree tree)
		{
			this.components = new System.ComponentModel.Container();

			this.fTree = tree;
			this.fOptions = new AncestorsCircleOptions(this);

			this.DoubleBuffered = true;
			this.BackColor = this.fOptions.BrushColor[9];
			this.fFont = new Font("Arial", 10f);
			this.fSegments = new List<PersonSegment>();
			this.fSelected = null;
			this.fGenWidth = 60;

			this.fToolTip = new System.Windows.Forms.ToolTip(components);
			this.fToolTip.AutoPopDelay = 5000;
			this.fToolTip.InitialDelay = 250;
			this.fToolTip.ReshowDelay = 50;
			this.fToolTip.ShowAlways = true;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

        private void DoRootChanged(GEDCOMIndividualRecord person)
        {
            ARootChangedEventHandler eventHandler = (ARootChangedEventHandler)base.Events[AncestorsCircle.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }

        protected override void SetNavObject(object obj)
        {
        	this.RootPerson = obj as GEDCOMIndividualRecord;
        }
        
        #region Content
        
		private void CreateBrushes()
		{
			for (int i = 0; i < this.fOptions.BrushColor.Length; i++)
			{
				Color col = this.fOptions.BrushColor[i];

				this.fCircleBrushes[i] = new SolidBrush(col);
				this.fDarkBrushes[i] = new SolidBrush(ColorUtils.darker(col, 0.2f));
			}
		}

		public void Changed()
		{
			this.CreateBrushes();

			this.BuildPathTree();

			this.Invalidate();
		}

		private void BuildPathTree()
		{
			this.fSegments.Clear();

			int ctX = base.Width / 2 + this.fOffsetX;
			int ctY = base.Height / 2 + this.fOffsetY;
			this.fCenterX = ctX;
			this.fCenterY = ctY;

			int inRad = CenterRad - 50;

			PersonSegment segment = new PersonSegment(0);
			GraphicsPath path = segment.Path;
			path.StartFigure();
			path.AddEllipse(ctX - inRad, ctY - inRad, inRad * 2, inRad * 2);
			path.CloseFigure();
			this.fSegments.Add(segment);

			int maxSteps = 1;
			for (int gen = 1; gen <= 8; gen++) {
				int extRad = inRad + this.fGenWidth;
				maxSteps *= 2;

				int ir2 = inRad * 2;
				int er2 = extRad * 2;
				float stepAngle = (360.0f / maxSteps);

				for (int stp = 0; stp < maxSteps; stp++)
				{
					float ang1 = (stp * stepAngle) - 90.0f;
					float angval1 = ang1 * PI / 180.0f;
					int px1 = ctX + (int)(inRad * Math.Cos(angval1));
					int py1 = ctY + (int)(inRad * Math.Sin(angval1));
					int px2 = ctX + (int)(extRad * Math.Cos(angval1));
					int py2 = ctY + (int)(extRad * Math.Sin(angval1));

					float ang2 = ang1 + stepAngle;
					float angval2 = ang2 * PI / 180.0f;
					int nx1 = ctX + (int)(inRad * Math.Cos(angval2));
					int ny1 = ctY + (int)(inRad * Math.Sin(angval2));
					int nx2 = ctX + (int)(extRad * Math.Cos(angval2));
					int ny2 = ctY + (int)(extRad * Math.Sin(angval2));

					segment = new PersonSegment(gen);
					path = segment.Path;
					path.StartFigure();
					path.AddLine(px2, py2, px1, py1);
					path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, stepAngle);
					path.AddLine(nx1, ny1, nx2, ny2);
					path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -stepAngle);
					path.CloseFigure();
					this.fSegments.Add(segment);
				}

				inRad = extRad;
			}

			// traverse tree
			this.fIndividualsCount = 0;
			if (this.fRootPerson != null) {
				this.fIndividualsCount++;
				this.SetSegmentParams(0, this.fRootPerson, 0, 0);

				GEDCOMIndividualRecord father, mother;
				this.fRootPerson.GetParents(out father, out mother);

				if (mother != null) {
					this.TraverseAncestors(mother, 90f, 1, CenterRad, 91, 1);
				}

				if (father != null) {
					this.TraverseAncestors(father, 270.0f, 1, CenterRad, 91, 1);
				}
			}
		}

		private void TraverseAncestors(GEDCOMIndividualRecord iRec, float v, int gen, int rad, int ro, int prevSteps)
		{
			try
			{
				this.fIndividualsCount++;
				
				int genSize = (int)Math.Pow(2.0, gen);
				float ang = (float)(360.0 / genSize);

				int idx = prevSteps + (int)(v / ang);
				this.SetSegmentParams(idx, iRec, rad, v);

				if (gen < 8) {
					GEDCOMIndividualRecord father, mother;
					iRec.GetParents(out father, out mother);

					int ps = prevSteps + genSize;

					if (father != null) {
						v -= (float)(Math.Abs(ang - ro) / 2f);
						this.TraverseAncestors(father, v, gen + 1, rad + this.fGenWidth, ro / 2, ps);
					}

					if (mother != null) {
						v += (float)(ang / 2f);
						this.TraverseAncestors(mother, v, gen + 1, rad + this.fGenWidth, ro / 2, ps);
					}
				}
			}
			catch
			{
			}
		}

		private void SetSegmentParams(int index, GEDCOMIndividualRecord rec, int rad, float v)
		{
			try
			{
				PersonSegment segment = this.fSegments[index];

				segment.IRec = rec;
				segment.rad = rad;
				segment.v = v;
			}
			catch
			{
			}
		}

		private PersonSegment FindSegment(int mX, int mY)
		{
			PersonSegment result = null;

			int num = this.fSegments.Count;
			for (int i = 0; i < num; i++) {
				PersonSegment segment = this.fSegments[i];

				if (segment.Path.IsVisible(mX, mY)) {
					result = segment;
					break;
				}
			}

			return result;
		}

        #endregion

        #region Drawing

		private void InternalDraw(Graphics gfx)
		{
			gfx.SmoothingMode = SmoothingMode.AntiAlias;

			Pen pen = new Pen(this.Options.BrushColor[10]);

			int num = this.fSegments.Count;
			for (int i = 0; i < num; i++) {
				PersonSegment segment = this.fSegments[i];

				int brIndex = (segment.Gen == 0) ? 9 : segment.Gen - 1;

				SolidBrush brush;
				if (this.fSelected == segment) {
					brush = this.fDarkBrushes[brIndex];
				} else {
					brush = this.fCircleBrushes[brIndex];
				}

				GraphicsPath path = segment.Path;
				gfx.FillPath(brush, path);
				gfx.DrawPath(pen, path);
			}

			this.DrawAncestorName(gfx, 0, 0, 0, this.fRootPerson);

			for (int i = 0; i < num; i++) {
				PersonSegment segment = this.fSegments[i];
				this.DrawAncestorName(gfx, segment.rad, segment.Gen, segment.v, segment.IRec);
			}
		}

		private void DrawAncestorName(Graphics gfx, int rad, int gen, float v, GEDCOMIndividualRecord iRec)
		{
			string s2, s1;
			if (iRec == null) {
				if (gen == 0) {
					s1 = "Choose";
					s2 = "subject";
				} else {
					return;
				}
			} else {
				string dummy;
				iRec.GetNameParts(out s2, out s1, out dummy);
			}

			rad -= 20;
			switch (gen)
			{
				case 0:
					{
						gfx.ResetTransform();
						gfx.TranslateTransform(this.fCenterX, this.fCenterY);

						SizeF sizeF = gfx.MeasureString(s1, this.fFont);
						gfx.DrawString(s1, this.fFont, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
						sizeF = gfx.MeasureString(s2, this.fFont);
						gfx.DrawString(s2, this.fFont, this.fCircleBrushes[8], -sizeF.Width / 2f, 0f);
						break;
					}
				case 1:
					{
						float dx = (float)Math.Sin(PI * v / 180.0) * rad;
						float dy = (float)Math.Cos(PI * v / 180.0) * rad;
						gfx.ResetTransform();
						gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
						gfx.RotateTransform(v);

						SizeF sizeF = gfx.MeasureString(s1, this.fFont);
						gfx.DrawString(s1, this.fFont, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
						sizeF = gfx.MeasureString(s2, this.fFont);
						gfx.DrawString(s2, this.fFont, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
						break;
					}
				case 2:
				case 3:
				case 4:
				case 5:
					{
						float dx = (float)Math.Sin(PI * v / 180.0) * rad;
						float dy = (float)Math.Cos(PI * v / 180.0) * rad;
						gfx.ResetTransform();
						gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
						gfx.RotateTransform(v);

						SizeF sizeF2 = gfx.MeasureString(s1, this.fFont);
						gfx.DrawString(s1, this.fFont, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
						sizeF2 = gfx.MeasureString(s2, this.fFont);
						dx = (float)Math.Sin(PI * v / 180.0) * (rad - sizeF2.Height);
						dy = (float)Math.Cos(PI * v / 180.0) * (rad - sizeF2.Height);
						gfx.ResetTransform();
						gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
						gfx.RotateTransform(v);
						gfx.DrawString(s2, this.fFont, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
						break;
					}
				case 6:
				case 7:
				case 8:
					{
						float dx = (float)Math.Sin(PI * (v + 0.6) / 180.0) * rad;
						float dy = (float)Math.Cos(PI * (v + 0.6) / 180.0) * rad;
						gfx.ResetTransform();
						gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
						gfx.RotateTransform(v + 0.6f - 90.0f);

						SizeF sizeF4 = gfx.MeasureString(s1, this.fFont);
						gfx.DrawString(s1, this.fFont, this.fCircleBrushes[8], -sizeF4.Width / 2f, -sizeF4.Height / 2f);
						break;
					}
			}
		}

		#endregion

		#region Protected methods

		protected override void OnKeyDown(KeyEventArgs e)
		{
			base.OnKeyDown(e);

			e.Handled = false;
			switch (e.KeyCode) {
				case Keys.Add:
					this.GenWidth += 10;
					break;

				case Keys.Subtract:
					this.GenWidth -= 10;
					break;

				case Keys.Left:
					{
						if (this.fRootPerson != null) {
							GEDCOMIndividualRecord father, mother;
							this.fRootPerson.GetParents(out father, out mother);

							if (father != null) {
								this.RootPerson = father;
							}
						}
						break;
					}

				case Keys.Right:
					{
						if (this.fRootPerson != null) {
							GEDCOMIndividualRecord father, mother;
							this.fRootPerson.GetParents(out father, out mother);

							if (mother != null) {
								this.RootPerson = mother;
							}
						}
						break;
					}

				case Keys.Back:
					this.NavPrev();
					return;

				default:
					e.Handled = true;
					break;
			}

			if (!e.Handled) this.Changed();
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.InternalDraw(e.Graphics);
		}

		protected override void OnResize(EventArgs e)
		{
			base.OnResize(e);
			this.Changed();
		}

		protected override void OnMouseUp(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			PersonSegment selected = this.FindSegment(e.X, e.Y);

			if (selected != null && selected.IRec != null) {
				this.RootPerson = selected.IRec;
			}
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			PersonSegment selected = this.FindSegment(e.X, e.Y);

			string hint = "";
			if (!object.Equals(this.fSelected, selected)) {
				this.fSelected = selected;
				
				if (selected != null && selected.IRec != null) {
					string name = selected.IRec.GetNameString(true, false);
					hint = selected.Gen.ToString() + ", " + name;
				}

				this.Invalidate();
			}

			if (!object.Equals(this.fHint, hint)) {
				this.fHint = hint;

				if (!string.IsNullOrEmpty(hint)) {
					fToolTip.Show(hint, this, e.X, e.Y, 3000);
				} else {
					//fToolTip.Hide(this);
				}
			}
		}

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == NativeMethods.WM_GETDLGCODE)
			{
				m.Result = (IntPtr)(m.Result.ToInt32() | 
				                    NativeMethods.DLGC_WANTARROWS | NativeMethods.DLGC_WANTTAB | 
				                    NativeMethods.DLGC_WANTCHARS | NativeMethods.DLGC_WANTALLKEYS);
			}
		}

		#endregion
	}
}
