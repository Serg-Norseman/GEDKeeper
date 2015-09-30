using System;
using System.Drawing;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCommon.Graph;
using GKCore;
using GKCore.Types;

namespace GKUI.Charts
{
	public enum PersonFlag
	{
		pfDivorced, pfIsDead, pfSelected, pfIsDup, 
		pfDescByFather, pfDescByMother,
		pfAncWalk, pfDescWalk,
		pfHasInvAnc, pfHasInvDesc
	}

	public enum PersonKind
	{
		pkDefault, pkSpouse
	}

	public class TreeChartPerson : BaseObject
    {
        protected TreeChartBox fChart;

        // base fields
        private EnumSet<PersonFlag> fFlags;
        private GEDCOMIndividualRecord fRec;
        private EnumSet<ChartPersonSign> fSigns;

        // strictly private
        private string fBirthDate;
        private string fBirthYear;
        private string fDeathDate;
        private string fDeathYear;
        private string fName;
        private string fNick;
        private string fPatronymic;
        private string fSurname;
        private GEDCOMSex fSex;
        private PersonList fChilds;
        private PersonList fSpouses;
        private Bitmap fPortrait;
        private int fPortraitWidth;

        // for rendering
        private int fHeight;
        private int fPtX;
        private int fPtY;
        private int fWidth;

        // without property controlling
        public GEDCOMFamilyRecord BaseFamily;
        public TreeChartPerson BaseSpouse;
        public TreeChartPerson Father;
        public TreeChartPerson Mother;
        public TreeChartPerson Parent;
        public bool CanExpand;
        public int Generation;
        public PersonKind Kind;
        public string Kinship;
        public string[] Lines;
        public IVertex Node;
        public string PathDebug;

        public float CertaintyAssessment;

        public EnumSet<PersonFlag> Flags
        {
            get { return this.fFlags; }
        }

        public Bitmap Portrait
        {
            get { return this.fPortrait; }
        }

        public int PortraitWidth
        {
            get { return this.fPortraitWidth; }
        }

        public bool Divorced
        {
            get {
                return this.fFlags.Contains(PersonFlag.pfDivorced);
            }
            set {
                if (value) {
                    this.fFlags.Include(PersonFlag.pfDivorced);
                } else {
                    this.fFlags.Exclude(PersonFlag.pfDivorced);
                }
            }
        }

        public bool IsDup
        {
            get {
                return this.fFlags.Contains(PersonFlag.pfIsDup);
            }
            set {
                if (value) {
                    this.fFlags.Include(PersonFlag.pfIsDup);
                } else {
                    this.fFlags.Exclude(PersonFlag.pfIsDup);
                }
            }
        }

        public int Height
        {
            get { return this.fHeight; }
        }

        public bool IsDead
        {
            get {
                return this.fFlags.Contains(PersonFlag.pfIsDead);
            }
            set {
                if (value) {
                    this.fFlags.Include(PersonFlag.pfIsDead);
                } else {
                    this.fFlags.Exclude(PersonFlag.pfIsDead);
                }
            }
        }

        public int PtX
        {
            get { return this.fPtX; }
            set { this.fPtX = value; }
        }

        public int PtY
        {
            get { return this.fPtY; }
            set { this.fPtY = value; }
        }

        public GEDCOMIndividualRecord Rec
        {
            get { return this.fRec; }
        }

        public ExtRect Rect
        {
            get {
                ExtRect result;
                result.Left = this.fPtX - this.fWidth / 2;
                result.Right = result.Left + this.fWidth - 1;
                result.Top = this.fPtY;
                result.Bottom = result.Top + this.fHeight - 1;
                return result;
            }
        }

        public bool Selected
        {
            get {
                return this.fFlags.Contains(PersonFlag.pfSelected);
            }
            set {
                if (value) {
                    this.fFlags.Include(PersonFlag.pfSelected);
                } else {
                    this.fFlags.Exclude(PersonFlag.pfSelected);
                }
            }
        }

        public GEDCOMSex Sex
        {
            get { return this.fSex; }
            set { this.fSex = value; }
        }

        public EnumSet<ChartPersonSign> Signs
        {
            get { return this.fSigns; }
        }

        public int Width
        {
            get { return this.fWidth; }
        }

        public TreeChartPerson(TreeChartBox chart)
        {
            this.fChart = chart;

            this.fFlags = EnumSet<PersonFlag>.Create();
            this.fPortrait = null;
            this.fSpouses = null;
            this.fChilds = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (this.fPortrait != null) this.fPortrait.Dispose();
                if (this.fChilds != null) this.fChilds.Dispose();
                if (this.fSpouses != null) this.fSpouses.Dispose();
            }
            base.Dispose(disposing);
        }

        protected int TextWidth(Graphics gfx, string st)
        {
            return gfx.MeasureString(st, this.fChart.DrawFont).ToSize().Width;
        }

        public Rectangle GetDestRect(ExtRect rt, Bitmap portrait)
        {
            int w = portrait.Width;
            int h = portrait.Height;
            int cw = rt.GetWidth();
            int ch = rt.GetHeight();

            float xyaspect = (w / (float)h);
            if (w > h) {
                w = cw;
                h = (int)(cw / xyaspect);
                if (h > ch) {
                    h = ch;
                    w = (int)(ch * xyaspect);
                }
            } else {
                h = ch;
                w = (int)(ch * xyaspect);
                if (w > cw) {
                    w = cw;
                    h = (int)(cw / xyaspect);
                }
            }

            Rectangle result = new Rectangle(rt.Left, rt.Top, w, h);
            result.Offset((cw - w) / 2, (ch - h) / 2);
            return result;
        }

        private string GetLifeYears()
        {
            string result;

            if (this.fBirthYear == "") {
                result = "?";
            } else {
                result = this.fBirthYear;
            }

            if (this.IsDead) {
                if (this.fDeathYear == "") {
                    result += " - ?";
                } else {
                    result = result + " - " + this.fDeathYear;
                }
            }
            result = "[ " + result + " ]";
            return result;
        }

        public TreeChartPerson GetChild(int index)
        {
            TreeChartPerson result = ((this.fChilds == null) ? null : this.fChilds[index]);
            return result;
        }

        public int GetChildsCount()
        {
            int result = ((this.fChilds == null) ? 0 : this.fChilds.Count);
            return result;
        }

        public TreeChartPerson GetSpouse(int index)
        {
            TreeChartPerson result = ((this.fSpouses == null) ? null : this.fSpouses[index]);
            return result;
        }

        public int GetSpousesCount()
        {
            int result = ((this.fSpouses == null) ? 0 : this.fSpouses.Count);
            return result;
        }

        public void AddChild(TreeChartPerson child)
        {
            if (child == null) return;

            if (this.fChilds == null) this.fChilds = new PersonList(false);

            this.fChilds.Add(child);
        }

        public void AddSpouse(TreeChartPerson spouse)
        {
            if (spouse == null) return;

            if (this.fSpouses == null) this.fSpouses = new PersonList(false);

            this.fSpouses.Add(spouse);
        }

		private EnumSet<ChartPersonSign> GetPersonSigns()
		{
			EnumSet<ChartPersonSign> result = EnumSet<ChartPersonSign>.Create();
			
			int num = this.fRec.UserReferences.Count;
			for (int i = 0; i < num; i++)
			{
				string rs = this.fRec.UserReferences[i].StringValue;
				for (ChartPersonSign cps = ChartPersonSign.urRI_StGeorgeCross; cps <= ChartPersonSign.urLast; cps++)
				{
					if (rs == GKData.UserRefs[(int)cps]) result.Include(cps);
				}
			}
			
			return result;
		}

        public void BuildBy(GEDCOMIndividualRecord iRec, ref bool hasMediaFail)
        {
            try
            {
                this.fRec = iRec;

                if (iRec != null) {
                    if (this.fChart.fPreparedIndividuals.IndexOf(iRec.XRef) < 0) {
                        this.fChart.fPreparedIndividuals.Add(iRec.XRef);
                    }

                    string fam, nam, pat;
                    iRec.GetNameParts(out fam, out nam, out pat);
                    this.fSurname = fam;
                    this.fName = nam;
                    this.fPatronymic = pat;
                    this.fNick = iRec.GetNickString();
                    this.fBirthDate = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, false);
                    this.fDeathDate = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, false);
                    this.IsDead = !iRec.IsLive();
                    this.fSex = iRec.Sex;
                    this.fSigns = this.GetPersonSigns();
                    this.fBirthYear = GKUtils.GetBirthDate(iRec, DateFormat.dfYYYY, false);
                    this.fDeathYear = GKUtils.GetDeathDate(iRec, DateFormat.dfYYYY, false);

                    if (this.fChart.Options.PortraitsVisible) {
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

                    this.CertaintyAssessment = iRec.GetCertaintyAssessment();
                } else {
                    this.fSurname = "";
                    this.fName = "< ? >";
                    this.fPatronymic = "";
                    this.fNick = "";
                    this.fBirthDate = "";
                    this.fDeathDate = "";
                    this.IsDead = false;
                    this.fSex = GEDCOMSex.svNone;
                    this.fSigns = EnumSet<ChartPersonSign>.Create();

                    this.CertaintyAssessment = 0.0f;
                }
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.BuildBy(): " + ex.Message);
                throw;
            }
        }

        private string GetName()
        {
        	string st = this.fName;
        	if (this.fChart.Options.NickVisible && !string.IsNullOrEmpty(this.fNick)) st = st + " \"" + this.fNick + "\"";
        	return st;
        }

        private void InitInfo(int lines)
        {
        	this.Lines = new string[lines];

            try
            {
                int idx = 0;

                if (this.fChart.Options.FamilyVisible) {
                	this.Lines[idx] = this.fSurname;
                    idx++;
                }

                if (!this.fChart.Options.DiffLines) {
                	this.Lines[idx] = this.GetName() + " " + this.fPatronymic; // attention: "Name" is combined property
                    idx++;
                } else {
                	this.Lines[idx] = this.GetName();
                    idx++;

                    this.Lines[idx] = this.fPatronymic;
                    idx++;
                }

                if (!this.fChart.Options.OnlyYears) {
                    if (this.fChart.Options.BirthDateVisible) {
                        this.Lines[idx] = this.fBirthDate;
                        idx++;
                    }

                    if (this.fChart.Options.DeathDateVisible) {
                        this.Lines[idx] = this.fDeathDate;
                        idx++;
                    }
                } else {
                    this.Lines[idx] = this.GetLifeYears();
                    idx++;
                }

                if (this.fChart.Options.Kinship) {
                    this.Lines[idx] = this.Kinship;
                    idx++;
                }

                if (this.fChart.PathDebug) {
                    this.Lines[idx] = this.PathDebug;
                    idx++;
                }
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.InitInfo(): " + ex.Message);
            }
        }
        
        public void CalcBounds(int lines, Graphics gfx)
        {
            try
            {
            	this.InitInfo(lines);
            	
                int maxwid = 0;
                for (int k = 0; k < lines; k++) {
                	int wt = this.TextWidth(gfx, this.Lines[k]);
                    if (maxwid < wt) maxwid = wt;
                }

                this.fWidth = maxwid + 20;
                this.fHeight = gfx.MeasureString("A", this.fChart.DrawFont).ToSize().Height * lines + 20;

                if (this.fChart.Options.PortraitsVisible && this.fPortrait != null) {
                    Rectangle prt = this.GetDestRect(ExtRect.Create(0, 0, this.fHeight - 1, this.fHeight - 1), this.fPortrait);
                    this.fPortraitWidth = prt.Right - prt.Left + 1;
                    this.fWidth += this.fPortraitWidth;
                }
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.CalcBounds(): " + ex.Message);
            }
        }

		public void DefineExpands()
		{
			if (this.fFlags.Contains(PersonFlag.pfAncWalk) && this.fFlags.Contains(PersonFlag.pfDescWalk)
			    && this.fFlags.Contains(PersonFlag.pfHasInvDesc))
			{
				// it's hack
				this.fFlags.Exclude(PersonFlag.pfHasInvDesc);
			}
			
			if (this.fFlags.Contains(PersonFlag.pfHasInvAnc)) {
				this.CanExpand = true;
			}
			
			if (this.fFlags.Contains(PersonFlag.pfHasInvDesc)) {
				this.CanExpand = true;
			}
		}

    }
}