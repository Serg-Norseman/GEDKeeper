using GedCom551;
using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Charts
{
	public class TPerson : IDisposable
	{
		public enum TPersonFlag : byte
		{
			pfDivorced, pfIsDead, pfSelected, pfDescByFather, pfDescByMother
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
		private Bitmap FPortrait;
		private int FPortraitWidth;
		private int FPtX;
		private int FPtY;
		private TGEDCOMIndividualRecord FRec;
		private TGEDCOMObject.TGEDCOMSex FSex;
		private TGenEngine.TChartPersonSigns FSigns;
		private int FWidth;
		private bool Disposed_;
		private TPerson FBaseSpouse;
		private TPersonList FChilds;
		private TPerson FFather;
		private int FGeneration;
		private TPerson.TPersonKind FKind;
		private TPerson FMother;
		private TGraph.TGraphNode FNode;
		private TPersonList FSpouses;

		internal TEnumSet FFlags;
		internal string FPathDebug;
		internal TPerson Parent;

		public TPerson BaseSpouse
		{
			get { return this.FBaseSpouse; }
			set { this.FBaseSpouse = value; }
		}

		/*[Browsable(false)]
		public TPerson Childs
		{
			get { return this.GetChild(Index); }
		}*/

		public int ChildsCount
		{
			get { return this.GetChildsCount(); }
		}

		public TPerson Father
		{
			get { return this.FFather; }
			set { this.FFather = value; }
		}

		public int Generation
		{
			get { return this.FGeneration; }
			set { this.FGeneration = value; }
		}

		public TPerson.TPersonKind Kind
		{
			get { return this.FKind; }
			set { this.FKind = value; }
		}

		public TPerson Mother
		{
			get { return this.FMother; }
			set { this.FMother = value; }
		}

		public TGraph.TGraphNode Node
		{
			get { return this.FNode; }
			set { this.FNode = value; }
		}
		
		/*[Browsable(false)]
		public TPerson Spouses
		{
			get { return this.GetSpouse(Index); }
		}*/

		public int SpousesCount
		{
			get { return this.GetSpousesCount(); }
		}

		internal TPerson GetChild(int Index)
		{
			TPerson Result = ((this.FChilds == null) ? null : this.FChilds[Index]);
			return Result;
		}

		internal int GetChildsCount()
		{
			int Result = ((this.FChilds == null) ? 0 : this.FChilds.Count);
			return Result;
		}

		internal TPerson GetSpouse(int Index)
		{
			TPerson Result = ((this.FSpouses == null) ? null : this.FSpouses[Index]);
			return Result;
		}

		internal int GetSpousesCount()
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
			get { return this.GetDivorced(); }
			set { this.SetDivorced(value); }
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
			get { return this.GetIsDead(); }
			set { this.SetIsDead(value); }
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

		public TPoint Pt
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

		public TGEDCOMObject.TGEDCOMSex Sex
		{
			get { return this.FSex; }
			set { this.FSex = value; }
		}

		public TGenEngine.TChartPersonSigns Signs
		{
			get { return this.FSigns; }
		}

		public int Width
		{
			get { return this.FWidth; }
		}

		internal int TextWidth(Graphics g, string st)
		{
			return g.MeasureString(st, this.FChart.FDrawFont).ToSize().Width;
		}

		internal void DrawBorder(Graphics aCanvas, Pen xpen, TRect rt, bool dead)
		{
			Rectangle rect = rt.ToRectangle();
			TGEDCOMObject.TGEDCOMSex fSex = this.FSex;
			Color b_color;
			if (fSex != TGEDCOMObject.TGEDCOMSex.svNone)
			{
				if (fSex == TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (!dead) {
						if (this.Divorced) {
							b_color = this.FChart.Options.UnHusbandColor;
						} else {
							b_color = this.FChart.Options.MaleColor;
						}
					} else {
						b_color = Color.Black;
					}
					aCanvas.FillRectangle(new SolidBrush(b_color), rect);
					aCanvas.DrawRectangle(xpen, rect);
					return;
				}
				if (fSex == TGEDCOMObject.TGEDCOMSex.svFemale)
				{
					if (!dead) {
						if (this.Divorced) {
							b_color = this.FChart.Options.UnWifeColor;
						} else {
							b_color = this.FChart.Options.FemaleColor;
						}
					} else {
						b_color = Color.Black;
					}
					
					GraphicsPath path = TGKSys.CreateRoundedRectangle(rect.Left, rect.Top, rect.Width, rect.Height, 5);
					aCanvas.FillPath(new SolidBrush(b_color), path);
					aCanvas.DrawPath(xpen, path);
					
					//aCanvas.FillRectangle(new SolidBrush(b_color), rect);
					//aCanvas.DrawRectangle(xpen, rect);
					return;
				}
				if (fSex != TGEDCOMObject.TGEDCOMSex.svUndetermined)
				{
					return;
				}
			}
			if (!dead)
			{
				b_color = this.FChart.Options.UnkSexColor;
				goto IL_E7;
			}
			b_color = Color.Black;
			goto IL_E7;
			return;
			IL_E7:
			aCanvas.FillRectangle(new SolidBrush(b_color), rect);
			aCanvas.DrawRectangle(xpen, rect);
		}

		internal void xOffsetRect(ref TRect Rect, int DX, int DY)
		{
			Rect.Left += DX;
			Rect.Right -= DX;
			Rect.Top += DY;
			Rect.Bottom -= DY;
		}

		internal void TextOut(Graphics aCanvas, TRect rt, string s, int h, ref int line)
		{
			int rw = rt.Right - rt.Left + 1;
			int stw = aCanvas.MeasureString(s, this.FChart.FDrawFont).ToSize().Width;
			int rx = rt.Left + (rw - stw) / 2;
			int ry = rt.Top + 10 + h * line;
			aCanvas.DrawString(s, this.FChart.FDrawFont, new SolidBrush(Color.Black), ((float)((double)rx)), ((float)((double)ry)));
			line++;
		}

		internal void CalcBounds()
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

				if ((this.FChart as TAncestryChartBox).PathDebug) {
					int wt = this.TextWidth(g, this.FPathDebug);
					if (maxwid < wt) maxwid = wt;

					lines++;
				}

				this.FWidth = maxwid + 20;
				this.FHeight = g.MeasureString("A", this.FChart.FDrawFont).ToSize().Height * lines + 20;
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

		internal Rectangle GetDestRect(TRect rt, Bitmap Portrait)
		{
			int w = Portrait.Width;
			int h = Portrait.Height;
			int cw = rt.Right - rt.Left + 1;
			int ch = rt.Bottom - rt.Top + 1;
			double xyaspect = ((double)w / (double)h);
			if (w > h) {
				w = cw;
				h = (int)BDSSystem.Trunc((double)cw / xyaspect);
				if (h > ch)
				{
					h = ch;
					w = (int)BDSSystem.Trunc((double)ch * xyaspect);
				}
			} else {
				h = ch;
				w = (int)BDSSystem.Trunc((double)ch * xyaspect);
				if (w > cw)
				{
					w = cw;
					h = (int)BDSSystem.Trunc((double)cw / xyaspect);
				}
			}

			Rectangle Result = new Rectangle(rt.Left, rt.Top, w, h);
			Result.Offset((cw - w) / 2, (ch - h) / 2);
			return Result;
		}

		internal bool GetDivorced()
		{
			return this.FFlags.InSet(TPerson.TPersonFlag.pfDivorced);
		}

		internal bool GetIsDead()
		{
			return this.FFlags.InSet(TPerson.TPersonFlag.pfIsDead);
		}

		internal TPoint GetPt()
		{
			TPoint Result;
			Result.X = this.FPtX;
			Result.Y = this.FPtY;
			return Result;
		}

		internal TRect GetRect()
		{
			TRect Result;
			Result.Left = this.FPtX - this.FWidth / 2;
			Result.Right = Result.Left + this.FWidth - 1;
			Result.Top = this.FPtY;
			Result.Bottom = Result.Top + this.FHeight - 1;
			return Result;
		}

		internal bool GetSelected()
		{
			return this.FFlags.InSet(TPerson.TPersonFlag.pfSelected);
		}

		internal void SetDivorced([In] bool Value)
		{
			if (Value) {
				this.FFlags.Include(TPerson.TPersonFlag.pfDivorced);
			} else {
				this.FFlags.Exclude(TPerson.TPersonFlag.pfDivorced);
			}
		}

		internal void SetIsDead([In] bool Value)
		{
			if (Value) {
				this.FFlags.Include(TPerson.TPersonFlag.pfIsDead);
			} else {
				this.FFlags.Exclude(TPerson.TPersonFlag.pfIsDead);
			}
		}

		internal void SetKinship([In] string Value)
		{
			this.FKinship = Value;
			this.CalcBounds();
		}

		internal void SetPt([In] TPoint Value)
		{
			this.FPtX = Value.X;
			this.FPtY = Value.Y;
		}

		internal void SetSelected([In] bool Value)
		{
			if (Value) {
				this.FFlags.Include(TPerson.TPersonFlag.pfSelected);
			} else {
				this.FFlags.Exclude(TPerson.TPersonFlag.pfSelected);
			}
		}

		protected internal string GetFullName()
		{
			return this.FName + " " + this.FPatronymic;
		}

		protected internal string GetLifeYears()
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

		public TPerson(TCustomChartBox aChart)
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
				if (this.FPortrait != null) TObjectHelper.Free(this.FPortrait);
				if (this.FChilds != null) this.FChilds.Free();
				if (this.FSpouses != null) this.FSpouses.Free();
				this.Disposed_ = true;
			}
		}

		public void AddChild(TPerson aChild)
		{
			if (aChild != null) {
				if (this.FChilds == null) this.FChilds = new TPersonList(false);

				this.FChilds.Add(aChild);
			}
		}

		public void AddSpouse(TPerson aSpouse)
		{
			if (aSpouse != null) {
				if (this.FSpouses == null) this.FSpouses = new TPersonList(false);

				this.FSpouses.Add(aSpouse);
			}
		}

		public void BuildBy(TGEDCOMIndividualRecord iRec)
		{
			this.FRec = iRec;
			if (iRec != null)
			{
				string fam = "";
				string nam = "";
				string pat = "";
				TGenEngine.GetNameParts(iRec, ref fam, ref nam, ref pat);
				this.FFamily = fam;
				this.FName = nam;
				this.FPatronymic = pat;
				this.FBirthDate = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
				this.FDeathDate = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
				this.IsDead = !iRec.IsLive();
				this.FSex = iRec.Sex;
				this.FSigns = (this.FChart as TAncestryChartBox).GetPersonSign(iRec);
				this.FBirthYear = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfYYYY, false);
				this.FDeathYear = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfYYYY, false);
				if (this.FChart.Options.PortraitsVisible)
				{
					this.FPortrait = this.FChart.FEngine.GetPrimaryBitmap(iRec);
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
				this.FSex = TGEDCOMObject.TGEDCOMSex.svNone;
				this.FSigns = (TGenEngine.TChartPersonSigns)0;
			}

			this.CalcBounds();
		}

		public virtual void Draw(Graphics aCanvas, int SPX, int SPY)
		{
			TRect rt = this.GetRect();
			rt = rt.GetOffset(SPX, SPY);
			int h = aCanvas.MeasureString("A", this.FChart.FDrawFont).ToSize().Height;
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
				TGEDCOMObject.TGEDCOMSex fSex = this.FSex;
				Color pen_color = Color.Black;
				if (fSex != TGEDCOMObject.TGEDCOMSex.svNone)
				{
					if (fSex == TGEDCOMObject.TGEDCOMSex.svMale)
					{
						pen_color = Color.Blue;
						goto IL_D1;
					}
					if (fSex == TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						pen_color = Color.Red;
						goto IL_D1;
					}
					if (fSex != TGEDCOMObject.TGEDCOMSex.svUndetermined)
					{
						goto IL_D1;
					}
				}
				pen_color = Color.Black;
				IL_D1:
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

			if (this.FChart.Options.SignsVisible && this.FSigns != (TGenEngine.TChartPersonSigns)0)
			{
				int i = 0;
				TGenEngine.TChartPersonSign cps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross;
				do
				{
					if (cps < (TGenEngine.TChartPersonSign)8 && ((int)this.FSigns & 1 << (int)cps) != 0)
					{
						Bitmap pic = (this.FChart as TAncestryChartBox).SignsPic[(int)cps - 1];
						aCanvas.DrawImage(pic, rt.Right, rt.Top - 21 + i * pic.Height);
						i++;
					}
					cps++;
				}
				while (cps != (TGenEngine.TChartPersonSign)5);
			}

			if ((this.FChart as TAncestryChartBox).PathDebug)
			{
				this.TextOut(aCanvas, rt, this.FPathDebug, h, ref line);
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}

	///

	public class TPersonList : TObjectList
	{
		public new TPerson this[int Index]
		{
			get	{ return base.Get(Index) as TPerson; }
			set	{ base.Put(Index, value); }
		}

		public TPersonList()
		{
		}

		public TPersonList(bool AOwnsObjects) : base(AOwnsObjects)
		{
		}
	}
}
