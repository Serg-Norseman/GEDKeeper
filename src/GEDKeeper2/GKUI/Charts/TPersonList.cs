using System;
using System.Drawing;

using ExtUtils;
using ExtUtils.Graph;
using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Charts
{
    public class TreeChartPerson : BaseObject
	{
		public enum TPersonFlag : byte
		{
			pfDivorced, pfIsDead, pfSelected, pfDescByFather, pfDescByMother, pfIsDup
		}

		public enum TPersonKind : byte
		{
			pkDefault, pkSpouse
		}

		internal TTreeChartBox fChart;

		private string FBirthDate;
		private string FBirthYear;
		private string FDeathDate;
		private string FDeathYear;
		private string FFamily;
		private int FHeight;
		private string FKinship;
		private string FName;
		private string FPatronymic;
		private string FNick;
		private int FPtX;
		private int FPtY;
		private TGEDCOMIndividualRecord FRec;
		private TGEDCOMSex FSex;
		private EnumSet<TChartPersonSign> fSigns;
		private int FWidth;
		private TreeChartPerson FBaseSpouse;
		private TGEDCOMFamilyRecord FBaseFamily;
		private TPersonList FChilds;
		private TreeChartPerson FFather;
		private int FGeneration;
		private TPersonKind FKind;
		private TreeChartPerson FMother;
		private IVertex FNode;
		private TPersonList FSpouses;
		private EnumSet<TPersonFlag> fFlags;

		private Bitmap fPortrait;
		private int fPortraitWidth;

        public EnumSet<TPersonFlag> Flags 
        {
        	get { return this.fFlags; }
        }
        
        public TreeChartPerson Parent { get; set; }
        public string PathDebug { get; set; }

	    public TreeChartPerson BaseSpouse
		{
			get { return this.FBaseSpouse; }
			set { this.FBaseSpouse = value; }
		}

		public TGEDCOMFamilyRecord BaseFamily
		{
			get { return this.FBaseFamily; }
			set { this.FBaseFamily = value; }
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

		public IVertex Node
		{
			get { return this.FNode; }
			set { this.FNode = value; }
		}

		public Bitmap Portrait
		{
			get { return this.fPortrait; }
		}

		public int PortraitWidth
		{
			get { return this.fPortraitWidth; }
		}
		
		public TreeChartPerson GetChild(int index)
		{
			TreeChartPerson result = ((this.FChilds == null) ? null : this.FChilds[index]);
			return result;
		}

		public int GetChildsCount()
		{
			int result = ((this.FChilds == null) ? 0 : this.FChilds.Count);
			return result;
		}

		public TreeChartPerson GetSpouse(int index)
		{
			TreeChartPerson result = ((this.FSpouses == null) ? null : this.FSpouses[index]);
			return result;
		}

		public int GetSpousesCount()
		{
			int result = ((this.FSpouses == null) ? 0 : this.FSpouses.Count);
			return result;
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
				return this.fFlags.Contains(TPersonFlag.pfDivorced);
			}
			set {
				if (value) {
					this.fFlags.Include(TPersonFlag.pfDivorced);
				} else {
					this.fFlags.Exclude(TPersonFlag.pfDivorced);
				}
			}
		}

		public bool IsDup
		{
			get { 
				return this.fFlags.Contains(TPersonFlag.pfIsDup);
			}
			set { 
				if (value) {
					this.fFlags.Include(TPersonFlag.pfIsDup);
				} else {
					this.fFlags.Exclude(TPersonFlag.pfIsDup);
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
				return this.fFlags.Contains(TPersonFlag.pfIsDead);
			}
			set
			{
				if (value) {
					this.fFlags.Include(TPersonFlag.pfIsDead);
				} else {
					this.fFlags.Exclude(TPersonFlag.pfIsDead);
				}
			}
		}

		public string Kinship
		{
			get { return this.FKinship; }
			set { this.FKinship = value; }
		}

		public string Name
		{
			get {
				string st = this.FName;
				if (this.fChart.Options.NickVisible && !string.IsNullOrEmpty(this.FNick)) st = st + " \"" + this.FNick + "\"";
				return st;
			}
		}

		public string Patronymic
		{
			get { return this.FPatronymic; }
		}

		public string Nick
		{
			get { return this.FNick; }
		}

		public Point Pt
		{
			get {
				return new Point(this.FPtX, this.FPtY);
			}
			set {
				this.FPtX = value.X;
				this.FPtY = value.Y;
			}
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

		public ExtRect Rect
		{
			get { 
				ExtRect result;
				result.Left = this.FPtX - this.FWidth / 2;
				result.Right = result.Left + this.FWidth - 1;
				result.Top = this.FPtY;
				result.Bottom = result.Top + this.FHeight - 1;
				return result;
			}
		}

		public bool Selected
		{
			get {
				return this.fFlags.Contains(TPersonFlag.pfSelected);
			}
			set {
				if (value) {
					this.fFlags.Include(TPersonFlag.pfSelected);
				} else {
					this.fFlags.Exclude(TPersonFlag.pfSelected);
				}
			}
		}

		public TGEDCOMSex Sex
		{
			get { return this.FSex; }
			set { this.FSex = value; }
		}

		public EnumSet<TChartPersonSign> Signs
		{
			get { return this.fSigns; }
		}

		public int Width
		{
			get { return this.FWidth; }
		}

        public TreeChartPerson(TTreeChartBox chart)
        {
            this.fChart = chart;

            this.fFlags = EnumSet<TPersonFlag>.Create();
            this.fPortrait = null;
            this.FSpouses = null;
            this.FChilds = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (this.fPortrait != null) this.fPortrait.Dispose();
                if (this.FChilds != null) this.FChilds.Dispose();
                if (this.FSpouses != null) this.FSpouses.Dispose();
            }
            base.Dispose(disposing);
        }

        protected int TextWidth(Graphics g, string st)
		{
			return g.MeasureString(st, this.fChart.DrawFont).ToSize().Width;
		}

		public Rectangle GetDestRect(ExtRect rt, Bitmap portrait)
		{
			int w = portrait.Width;
			int h = portrait.Height;
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

			Rectangle result = new Rectangle(rt.Left, rt.Top, w, h);
			result.Offset((cw - w) / 2, (ch - h) / 2);
			return result;
		}

		public string GetFullName()
		{
			return this.Name + " " + this.FPatronymic; // attention: Name - combined property
		}

		public string GetLifeYears()
		{
			string result;

			if (this.FBirthYear == "") {
				result = "?";
			} else {
				result = this.FBirthYear;
			}

			if (this.IsDead)
			{
				if (this.FDeathYear == "") {
					result += " - ?";
				} else {
					result = result + " - " + this.FDeathYear;
				}
			}
			result = "[ " + result + " ]";
			return result;
		}

		public void AddChild(TreeChartPerson child)
		{
			if (child == null) return;

			if (this.FChilds == null) this.FChilds = new TPersonList(false);

			this.FChilds.Add(child);
		}

		public void AddSpouse(TreeChartPerson spouse)
		{
			if (spouse == null) return;

			if (this.FSpouses == null) this.FSpouses = new TPersonList(false);

			this.FSpouses.Add(spouse);
		}

		public void BuildBy(TGEDCOMIndividualRecord iRec, ref bool hasMediaFail)
		{
			try
			{
				this.FRec = iRec;

				if (iRec != null)
				{
					if (this.fChart.fPreparedIndividuals.IndexOf(iRec.XRef) < 0) {
						this.fChart.fPreparedIndividuals.Add(iRec.XRef);
					}

					string fam, nam, pat;
					iRec.aux_GetNameParts(out fam, out nam, out pat);
					this.FFamily = fam;
					this.FName = nam;
					this.FPatronymic = pat;
					this.FNick = iRec.aux_GetNickStr();
					this.FBirthDate = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, false);
					this.FDeathDate = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, false);
					this.IsDead = !iRec.IsLive();
					this.FSex = iRec.Sex;
					this.fSigns = this.fChart.GetPersonSign(iRec);
					this.FBirthYear = GKUtils.GetBirthDate(iRec, DateFormat.dfYYYY, false);
					this.FDeathYear = GKUtils.GetDeathDate(iRec, DateFormat.dfYYYY, false);

					if (this.fChart.Options.PortraitsVisible)
					{
						try
						{
							this.fPortrait = this.fChart.Base.GetPrimaryBitmap(iRec, -1, -1, true);
						}
						catch (MediaFileNotFoundException)
						{
							if (!hasMediaFail) {
								GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
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
					this.FNick = "";
					this.FBirthDate = "";
					this.FDeathDate = "";
					this.IsDead = false;
					this.FSex = TGEDCOMSex.svNone;
					this.fSigns = EnumSet<TChartPersonSign>.Create();
				}

				//this.CalcBounds();
			}
			catch (Exception ex)
			{
				this.fChart.Base.Host.LogWrite("TreeChartPerson.BuildBy(): " + ex.Message);
				throw ex;
			}
		}

		public void CalcBounds()
		{
			Graphics g = this.fChart.CreateGraphics();
			try
			{
				int lines = 0;
				int maxwid = 0;
				int wt;
				
				if (this.fChart.Options.FamilyVisible) {
					maxwid = this.TextWidth(g, this.FFamily);
					lines++;
				}

				if (!this.fChart.Options.DiffLines) {
					wt = this.TextWidth(g, this.GetFullName());
					if (maxwid < wt) maxwid = wt;
					lines++;
				} else {
					wt = this.TextWidth(g, this.Name); // attention: this combined property
					if (maxwid < wt) maxwid = wt;
					lines++;

					wt = this.TextWidth(g, this.FPatronymic);
					if (maxwid < wt) maxwid = wt;
					lines++;
				}

				if (!this.fChart.Options.OnlyYears) {
					if (this.fChart.Options.BirthDateVisible) {
						wt = this.TextWidth(g, this.FBirthDate);
						if (maxwid < wt) maxwid = wt;
						lines++;
					}

					if (this.fChart.Options.DeathDateVisible) {
						wt = this.TextWidth(g, this.FDeathDate);
						if (maxwid < wt) maxwid = wt;
						lines++;
					}
				} else {
					wt = this.TextWidth(g, this.GetLifeYears());
					if (maxwid < wt) maxwid = wt;
					lines++;
				}

				if (this.fChart.Options.Kinship) {
					wt = this.TextWidth(g, this.FKinship);
					if (maxwid < wt) maxwid = wt;
					lines++;
				}

				if (this.fChart.PathDebug) {
					wt = this.TextWidth(g, this.PathDebug);
					if (maxwid < wt) maxwid = wt;
					lines++;
				}

				this.FWidth = maxwid + 20;
				this.FHeight = g.MeasureString("A", this.fChart.DrawFont).ToSize().Height * lines + 20;
				if (this.fChart.Options.PortraitsVisible && this.fPortrait != null)
				{
					Rectangle prt = this.GetDestRect(ExtRect.Create(0, 0, this.FHeight - 1, this.FHeight - 1), this.fPortrait);
					this.fPortraitWidth = prt.Right - prt.Left + 1;
					this.FWidth += this.fPortraitWidth;
				}
			}
			finally
			{
				g.Dispose();
			}
		}

    }


	public class TPersonList : ExtList<TreeChartPerson>
	{
		public new TreeChartPerson this[int index]
		{
			get	{ return base[index] as TreeChartPerson; }
			set	{ base[index] = value; }
		}

		public TPersonList(bool ownsObjects) : base(ownsObjects)
		{
		}
	}


	public class TChartFilter : BaseObject, ICustomFilter
	{
		public enum TBranchCut
		{
			bcNone,
			bcYears,
			bcPersons
		}

		private TGroupMode Back_SourceMode;
		private string Back_SourceRef;
		private TChartFilter.TBranchCut Back_BranchCut;
		private int Back_BranchYear;
		private string Back_BranchPersons;

		public TGroupMode SourceMode;
		public string SourceRef;
		public TChartFilter.TBranchCut BranchCut;
		public int BranchYear;
		public string BranchPersons;

		public TChartFilter()
		{
			this.Reset();
		}

		public void Reset()
		{
			this.SourceMode = TGroupMode.gmAll;
			this.BranchCut = TChartFilter.TBranchCut.bcNone;
		}

		public void Backup()
		{
			this.Back_SourceMode = this.SourceMode;
			this.Back_SourceRef = this.SourceRef;
			this.Back_BranchCut = this.BranchCut;
			this.Back_BranchYear = this.BranchYear;
			this.Back_BranchPersons = this.BranchPersons;
		}

		public void Restore()
		{
			this.SourceMode = this.Back_SourceMode;
			this.SourceRef = this.Back_SourceRef;
			this.BranchCut = this.Back_BranchCut;
			this.BranchYear = this.Back_BranchYear;
			this.BranchPersons = this.Back_BranchPersons;
		}
	}
}
