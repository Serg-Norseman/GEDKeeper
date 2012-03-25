using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Runtime.InteropServices;

using Ext.Utils;
using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Charts
{
	public class TreeChartPerson : IDisposable
	{
		public enum TPersonFlag : byte
		{
			pfDivorced, pfIsDead, pfSelected, pfDescByFather, pfDescByMother, pfIsDup
		}

		public enum TPersonKind : byte
		{
			pkDefault, pkSpouse
		}

		private string FBirthDate;
		private string FBirthYear;
		private TCustomChartBox FChart;
		private string FDeathDate;
		private string FDeathYear;
		private string FFamily;
		private int FHeight;
		private string FKinship;
		private string FName;
		private string FPatronymic;
		private int FPtX;
		private int FPtY;
		private TGEDCOMIndividualRecord FRec;
		private TGEDCOMSex FSex;
		private EnumSet FSigns;
		private int FWidth;
		private bool Disposed_;
		private TreeChartPerson FBaseSpouse;
		private TPersonList FChilds;
		private TreeChartPerson FFather;
		private int FGeneration;
		private TPersonKind FKind;
		private TreeChartPerson FMother;
		private TGraph.TGraphNode FNode;
		private TPersonList FSpouses;

		private Bitmap FPortrait;
		private int FPortraitWidth;

		public EnumSet FFlags;
		public string FPathDebug;
		public TreeChartPerson Parent;

		public TreeChartPerson BaseSpouse
		{
			get { return this.FBaseSpouse; }
			set { this.FBaseSpouse = value; }
		}

		/*
		public TPerson Childs
		{
			get { return this.GetChild(Index); }
		}*/

		public int ChildsCount
		{
			get { return this.GetChildsCount(); }
		}

		public TreeChartPerson Father
		{
			get { return this.FFather; }
			set { this.FFather = value; }
		}

		public int Generation
		{
			get { return this.FGeneration; }
			set { this.FGeneration = value; }
		}

		public TPersonKind Kind
		{
			get { return this.FKind; }
			set { this.FKind = value; }
		}

		public TreeChartPerson Mother
		{
			get { return this.FMother; }
			set { this.FMother = value; }
		}

		public TGraph.TGraphNode Node
		{
			get { return this.FNode; }
			set { this.FNode = value; }
		}
		
		/*
		public TPerson Spouses
		{
			get { return this.GetSpouse(Index); }
		}*/

		public int SpousesCount
		{
			get { return this.GetSpousesCount(); }
		}

		public TreeChartPerson GetChild(int Index)
		{
			TreeChartPerson Result = ((this.FChilds == null) ? null : this.FChilds[Index]);
			return Result;
		}

		public int GetChildsCount()
		{
			int Result = ((this.FChilds == null) ? 0 : this.FChilds.Count);
			return Result;
		}

		public TreeChartPerson GetSpouse(int Index)
		{
			TreeChartPerson Result = ((this.FSpouses == null) ? null : this.FSpouses[Index]);
			return Result;
		}

		public int GetSpousesCount()
		{
			int Result = ((this.FSpouses == null) ? 0 : this.FSpouses.Count);
			return Result;
		}

		public string BirthDate
		{
			get { return this.FBirthDate; }
		}

		public string DeathDate
		{
			get { return this.FDeathDate; }
		}

		public bool Divorced
		{
			get {
				return this.FFlags.InSet(TPersonFlag.pfDivorced);
			}
			set {
				if (value) {
					this.FFlags.Include(TPersonFlag.pfDivorced);
				} else {
					this.FFlags.Exclude(TPersonFlag.pfDivorced);
				}
			}
		}

		public bool IsDup
		{
			get { 
				return this.FFlags.InSet(TPersonFlag.pfIsDup);
			}
			set { 
				if (value) {
					this.FFlags.Include(TPersonFlag.pfIsDup);
				} else {
					this.FFlags.Exclude(TPersonFlag.pfIsDup);
				}
			}
		}

		public string Family
		{
			get { return this.FFamily; }
		}

		public int Height
		{
			get { return this.FHeight; }
		}

		public bool IsDead
		{
			get
			{
				return this.FFlags.InSet(TPersonFlag.pfIsDead);
			}
			set
			{
				if (value) {
					this.FFlags.Include(TPersonFlag.pfIsDead);
				} else {
					this.FFlags.Exclude(TPersonFlag.pfIsDead);
				}
			}
		}

		public string Kinship
		{
			get { return this.FKinship; }
			set { this.SetKinship(value); }
		}

		public string Name
		{
			get { return this.FName; }
		}

		public string Patronymic
		{
			get { return this.FPatronymic; }
		}

		public Point Pt
		{
			get { return this.GetPt(); }
			set { this.SetPt(value); }
		}

		public int PtX
		{
			get { return this.FPtX; }
			set { this.FPtX = value; }
		}

		public int PtY
		{
			get { return this.FPtY; }
			set { this.FPtY = value; }
		}

		public TGEDCOMIndividualRecord Rec
		{
			get { return this.FRec; }
		}

		public TRect Rect
		{
			get { return this.GetRect(); }
		}

		public bool Selected
		{
			get { return this.GetSelected(); }
			set { this.SetSelected(value); }
		}

		public TGEDCOMSex Sex
		{
			get { return this.FSex; }
			set { this.FSex = value; }
		}

		public EnumSet Signs
		{
			get { return this.FSigns; }
		}

		public int Width
		{
			get { return this.FWidth; }
		}

		protected int TextWidth(Graphics g, string st)
		{
			return g.MeasureString(st, this.FChart.DrawFont).ToSize().Width;
		}

		private GraphicsPath CreateRoundedRectangle(int x, int y, int width, int height, int radius)
	    {
			int xw = x + width;
			int yh = y + height;
			int xwr = xw - radius;
			int yhr = yh - radius;
			int xr = x + radius;
			int yr = y + radius;
			int r2 = radius * 2;
			int xwr2 = xw - r2;
			int yhr2 = yh - r2;

			GraphicsPath p = new GraphicsPath();
			p.StartFigure();

			//Top Left Corner
			p.AddArc(x, y, r2, r2, 180, 90);

			//Top Edge
			p.AddLine(xr, y, xwr, y);

			//Top Right Corner
			p.AddArc(xwr2, y, r2, r2, 270, 90);

			//Right Edge
			p.AddLine(xw, yr, xw, yhr);

			//Bottom Right Corner
			p.AddArc(xwr2, yhr2, r2, r2, 0, 90);

			//Bottom Edge
			p.AddLine(xwr, yh, xr, yh);

			//Bottom Left Corner
			p.AddArc(x, yhr2, r2, r2, 90, 90);

			//Left Edge
			p.AddLine(x, yhr, x, yr);

			p.CloseFigure();
			return p;
		}

		private void DrawBorder(Graphics aCanvas, Pen xpen, TRect rt, bool dead)
		{
			Rectangle rect = rt.ToRectangle();
			Color b_color;
			switch (this.FSex) {
				case TGEDCOMSex.svMale:
				{
					if (!dead) {
						if (this.IsDup) {
							b_color = Color.FromArgb(192, 192, 192);
						} else {
							if (this.Divorced) {
								b_color = this.FChart.Options.UnHusbandColor;
							} else {
								b_color = this.FChart.Options.MaleColor;
							}
						}
					} else {
						b_color = Color.Black;
					}
					aCanvas.FillRectangle(new SolidBrush(b_color), rect.Left, rect.Top, rect.Width, rect.Height);
					aCanvas.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
					break;
				}

				case TGEDCOMSex.svFemale:
				{
					if (!dead) {
						if (this.IsDup) {
							b_color = Color.FromArgb(192, 192, 192);
						} else {
							if (this.Divorced) {
								b_color = this.FChart.Options.UnWifeColor;
							} else {
								b_color = this.FChart.Options.FemaleColor;
							}
						}
					} else {
						b_color = Color.Black;
					}
					GraphicsPath path = CreateRoundedRectangle(rect.Left, rect.Top, rect.Width, rect.Height, 5);
					aCanvas.FillPath(new SolidBrush(b_color), path);
					aCanvas.DrawPath(xpen, path);
					break;
				}

				default: 
				{
					if (!dead) {
						b_color = this.FChart.Options.UnkSexColor;
					} else {
						b_color = Color.Black;
					}
					aCanvas.FillRectangle(new SolidBrush(b_color), rect.Left, rect.Top, rect.Width, rect.Height);
					aCanvas.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
					break;
				}
			}
		
		}

		private void xOffsetRect(ref TRect Rect, int DX, int DY)
		{
			Rect.Left += DX;
			Rect.Right -= DX;
			Rect.Top += DY;
			Rect.Bottom -= DY;
		}

		private void TextOut(Graphics aCanvas, TRect rt, string s, int h, ref int line)
		{
			int stw = aCanvas.MeasureString(s, this.FChart.DrawFont).ToSize().Width;
			int rx = rt.Left + ((rt.Right - rt.Left + 1) - stw) / 2;
			int ry = rt.Top + (10 + (h * line));
			aCanvas.DrawString(s, this.FChart.DrawFont, new SolidBrush(Color.Black), (float)rx, (float)ry);
			line++;
		}

		private Rectangle GetDestRect(TRect rt, Bitmap Portrait)
		{
			int w = Portrait.Width;
			int h = Portrait.Height;
			int cw = rt.Right - rt.Left + 1;
			int ch = rt.Bottom - rt.Top + 1;
			double xyaspect = ((double)w / (double)h);
			if (w > h) {
				w = cw;
				h = (int)SysUtils.Trunc((double)cw / xyaspect);
				if (h > ch)
				{
					h = ch;
					w = (int)SysUtils.Trunc((double)ch * xyaspect);
				}
			} else {
				h = ch;
				w = (int)SysUtils.Trunc((double)ch * xyaspect);
				if (w > cw)
				{
					w = cw;
					h = (int)SysUtils.Trunc((double)cw / xyaspect);
				}
			}

			Rectangle Result = new Rectangle(rt.Left, rt.Top, w, h);
			Result.Offset((cw - w) / 2, (ch - h) / 2);
			return Result;
		}

		private Point GetPt()
		{
			return new Point(this.FPtX, this.FPtY);
		}

		private TRect GetRect()
		{
			TRect Result;
			Result.Left = this.FPtX - this.FWidth / 2;
			Result.Right = Result.Left + this.FWidth - 1;
			Result.Top = this.FPtY;
			Result.Bottom = Result.Top + this.FHeight - 1;
			return Result;
		}

		private bool GetSelected()
		{
			return this.FFlags.InSet(TPersonFlag.pfSelected);
		}

		private void SetKinship([In] string Value)
		{
			this.FKinship = Value;
			this.CalcBounds();
		}

		private void SetPt([In] Point Value)
		{
			this.FPtX = Value.X;
			this.FPtY = Value.Y;
		}

		private void SetSelected([In] bool Value)
		{
			if (Value) {
				this.FFlags.Include(TPersonFlag.pfSelected);
			} else {
				this.FFlags.Exclude(TPersonFlag.pfSelected);
			}
		}

		protected string GetFullName()
		{
			return this.FName + " " + this.FPatronymic;
		}

		protected string GetLifeYears()
		{
			string Result;

			if (this.FBirthYear == "") {
				Result = "?";
			} else {
				Result = this.FBirthYear;
			}

			if (this.IsDead)
			{
				if (this.FDeathYear == "") {
					Result += " - ?";
				} else {
					Result = Result + " - " + this.FDeathYear;
				}
			}
			Result = "[ " + Result + " ]";
			return Result;
		}

		public TreeChartPerson(TCustomChartBox aChart)
		{
			this.FChart = aChart;

			this.FPortrait = null;
			this.FSpouses = null;
			this.FChilds = null;
		}

		public virtual void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FPortrait != null) this.FPortrait.Dispose();
				if (this.FChilds != null) this.FChilds.Dispose();
				if (this.FSpouses != null) this.FSpouses.Dispose();
				this.Disposed_ = true;
			}
		}

		public void AddChild(TreeChartPerson aChild)
		{
			if (aChild != null) {
				if (this.FChilds == null) this.FChilds = new TPersonList(false);

				this.FChilds.Add(aChild);
			}
		}

		public void AddSpouse(TreeChartPerson aSpouse)
		{
			if (aSpouse != null) {
				if (this.FSpouses == null) this.FSpouses = new TPersonList(false);

				this.FSpouses.Add(aSpouse);
			}
		}

		public void BuildBy(TGEDCOMIndividualRecord iRec, ref bool hasMediaFail)
		{
			this.FRec = iRec;
			if (iRec != null)
			{
				TTreeChartBox ancChart = (this.FChart as TTreeChartBox);
				if (ancChart.FPreparedIndividuals.IndexOf(iRec.XRef) < 0) {
					ancChart.FPreparedIndividuals.Add(iRec.XRef);
				}

				string fam, nam, pat;
				iRec.aux_GetNameParts(out fam, out nam, out pat);
				this.FFamily = fam;
				this.FName = nam;
				this.FPatronymic = pat;
				this.FBirthDate = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
				this.FDeathDate = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
				this.IsDead = !iRec.IsLive();
				this.FSex = iRec.Sex;
				this.FSigns = ancChart.GetPersonSign(iRec);
				this.FBirthYear = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfYYYY, false);
				this.FDeathYear = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfYYYY, false);

				if (this.FChart.Options.PortraitsVisible)
				{
					try
					{
						this.FPortrait = this.FChart.Engine.GetPrimaryBitmap(iRec, -1, -1, true);
					}
					catch (MediaFileNotFoundException)
					{
						if (!hasMediaFail) {
							TGenEngine.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
							hasMediaFail = true;
						}
					}
				}
			}
			else
			{
				this.FFamily = "";
				this.FName = "< ? >";
				this.FPatronymic = "";
				this.FBirthDate = "";
				this.FDeathDate = "";
				this.IsDead = false;
				this.FSex = TGEDCOMSex.svNone;
				this.FSigns = new EnumSet();
			}

			this.CalcBounds();
		}

		private void CalcBounds()
		{
			Graphics g = this.FChart.CreateGraphics();
			try
			{
				int lines = 2;
				int maxwid = this.TextWidth(g, this.FFamily);

				if (!this.FChart.Options.DiffLines) {
					int wt = this.TextWidth(g, this.GetFullName());
					if (maxwid < wt) maxwid = wt;
				} else {
					int wt = this.TextWidth(g, this.FName);
					if (maxwid < wt) maxwid = wt;

					wt = this.TextWidth(g, this.FPatronymic);
					if (maxwid < wt) maxwid = wt;

					lines++;
				}

				if (!this.FChart.Options.OnlyYears)
				{
					if (this.FChart.Options.BirthDateVisible) {
						int wt = this.TextWidth(g, this.FBirthDate);
						if (maxwid < wt) maxwid = wt;

						lines++;
					}

					if (this.FChart.Options.DeathDateVisible) {
						int wt = this.TextWidth(g, this.FDeathDate);
						if (maxwid < wt) maxwid = wt;

						lines++;
					}
				} else {
					int wt = this.TextWidth(g, this.GetLifeYears());
					if (maxwid < wt) maxwid = wt;

					lines++;
				}

				if (this.FChart.Options.Kinship) {
					int wt = this.TextWidth(g, this.FKinship);
					if (maxwid < wt) maxwid = wt;

					lines++;
				}

				if ((this.FChart as TTreeChartBox).PathDebug) {
					int wt = this.TextWidth(g, this.FPathDebug);
					if (maxwid < wt) maxwid = wt;

					lines++;
				}

				this.FWidth = maxwid + 20;
				this.FHeight = g.MeasureString("A", this.FChart.DrawFont).ToSize().Height * lines + 20;
				if (this.FChart.Options.PortraitsVisible && this.FPortrait != null)
				{
					Rectangle prt = this.GetDestRect(TRect.Create(0, 0, this.FHeight - 1, this.FHeight - 1), this.FPortrait);
					this.FPortraitWidth = prt.Right - prt.Left + 1;
					this.FWidth += this.FPortraitWidth;
				}
			}
			finally
			{
				g.Dispose();
			}
		}

		public virtual void Draw(Graphics aCanvas, int SPX, int SPY)
		{
			TRect rt = this.GetRect();
			rt = rt.GetOffset(SPX, SPY);
			int h = aCanvas.MeasureString("A", this.FChart.DrawFont).ToSize().Height;
			bool has_port = this.FChart.Options.PortraitsVisible && this.FPortrait != null;
			Pen xpen = new Pen(Color.Black, 1f);

			if (this.IsDead)
			{
				TRect dt = rt;
				dt = dt.GetOffset(-2, -2);
				this.DrawBorder(aCanvas, xpen, dt, true);
			}

			if (this.Selected)
			{
				int pen_width = 2;
				Color pen_color;
				switch (this.FSex) {
					case TGEDCOMSex.svMale:
						pen_color = Color.Blue;
						break;
					case TGEDCOMSex.svFemale:
						pen_color = Color.Red;
						break;
					default:
						pen_color = Color.Black;
						break;
				}
				xpen = new Pen(pen_color, ((float)((double)pen_width)));
			}

			this.DrawBorder(aCanvas, xpen, rt, false);
			xpen = new Pen(Color.Black, 1f);

			if (has_port)
			{
				TRect port_rt = rt;
				port_rt.Right = port_rt.Left + this.FPortraitWidth;
				this.xOffsetRect(ref port_rt, 3, 3);
				aCanvas.DrawImage(this.FPortrait, this.GetDestRect(port_rt, this.FPortrait));
				rt.Left += this.FPortraitWidth;
			}

			int line = 0;
			this.TextOut(aCanvas, rt, this.FFamily, h, ref line);

			if (!this.FChart.Options.DiffLines) {
				this.TextOut(aCanvas, rt, this.GetFullName(), h, ref line);
			} else {
				this.TextOut(aCanvas, rt, this.FName, h, ref line);
				this.TextOut(aCanvas, rt, this.FPatronymic, h, ref line);
			}

			if (!this.FChart.Options.OnlyYears) {
				if (this.FChart.Options.BirthDateVisible) {
					this.TextOut(aCanvas, rt, this.FBirthDate, h, ref line);
				}

				if (this.FChart.Options.DeathDateVisible) {
					this.TextOut(aCanvas, rt, this.FDeathDate, h, ref line);
				}
			} else {
				this.TextOut(aCanvas, rt, this.GetLifeYears(), h, ref line);
			}

			if (this.FChart.Options.Kinship)
			{
				this.TextOut(aCanvas, rt, this.FKinship, h, ref line);
			}

			if (this.FChart.Options.SignsVisible && !this.FSigns.IsEmpty())
			{
				int i = 0;
				for (TGenEngine.TChartPersonSign cps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross;
				     cps <= TGenEngine.TChartPersonSign.urUSSR_RearVeteran; cps++)
				{
					if (this.FSigns.InSet(cps))
					{
						Bitmap pic = (this.FChart as TTreeChartBox).SignsPic[(int)cps - 1];
						aCanvas.DrawImage(pic, rt.Right, rt.Top - 21 + i * pic.Height);
						i++;
					}
				}
			}

			if ((this.FChart as TTreeChartBox).PathDebug)
			{
				this.TextOut(aCanvas, rt, this.FPathDebug, h, ref line);
			}
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}



	public class TPersonList : TList
	{
		public new TreeChartPerson this[int Index]
		{
			get	{ return base.Get(Index) as TreeChartPerson; }
			set	{ base.Put(Index, value); }
		}

		public TPersonList(bool AOwnsObjects) : base(AOwnsObjects)
		{
		}
	}
}
