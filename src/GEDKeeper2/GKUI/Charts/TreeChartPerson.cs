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
    public class TreeChartPerson : BaseObject
    {
        public enum PersonFlag : byte
        {
            pfDivorced, pfIsDead, pfSelected, pfDescByFather, pfDescByMother, pfIsDup
        }

        public enum PersonKind : byte
        {
            pkDefault, pkSpouse
        }

        protected TreeChartBox fChart;

        private string fBirthDate;
        private string fBirthYear;
        private string fDeathDate;
        private string fDeathYear;
        private string fFamily;
        private int fHeight;
        private string fName;
        private string fPatronymic;
        private string fNick;
        private int fPtX;
        private int fPtY;
        private GEDCOMIndividualRecord fRec;
        private GEDCOMSex fSex;
        private EnumSet<ChartPersonSign> fSigns;
        private int fWidth;
        private PersonList fChilds;
        private PersonList fSpouses;
        private EnumSet<PersonFlag> fFlags;

        private Bitmap fPortrait;
        private int fPortraitWidth;

        public EnumSet<PersonFlag> Flags
        {
            get { return this.fFlags; }
        }

        public TreeChartPerson Parent;
        public string PathDebug;
        public TreeChartPerson BaseSpouse;
        public GEDCOMFamilyRecord BaseFamily;
        public TreeChartPerson Father;
        public int Generation;
        public PersonKind Kind;
        public TreeChartPerson Mother;
        public IVertex Node;

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

        public string BirthDate
        {
            get { return this.fBirthDate; }
        }

        public string DeathDate
        {
            get { return this.fDeathDate; }
        }

        public bool Divorced
        {
            get
            {
                return this.fFlags.Contains(PersonFlag.pfDivorced);
            }
            set
            {
                if (value)
                {
                    this.fFlags.Include(PersonFlag.pfDivorced);
                }
                else
                {
                    this.fFlags.Exclude(PersonFlag.pfDivorced);
                }
            }
        }

        public bool IsDup
        {
            get
            {
                return this.fFlags.Contains(PersonFlag.pfIsDup);
            }
            set
            {
                if (value)
                {
                    this.fFlags.Include(PersonFlag.pfIsDup);
                }
                else
                {
                    this.fFlags.Exclude(PersonFlag.pfIsDup);
                }
            }
        }

        public string Family
        {
            get { return this.fFamily; }
        }

        public int Height
        {
            get { return this.fHeight; }
        }

        public bool IsDead
        {
            get
            {
                return this.fFlags.Contains(PersonFlag.pfIsDead);
            }
            set
            {
                if (value)
                {
                    this.fFlags.Include(PersonFlag.pfIsDead);
                }
                else
                {
                    this.fFlags.Exclude(PersonFlag.pfIsDead);
                }
            }
        }

        public string Kinship;

        public string Name
        {
            get
            {
                string st = this.fName;
                if (this.fChart.Options.NickVisible && !string.IsNullOrEmpty(this.fNick)) st = st + " \"" + this.fNick + "\"";
                return st;
            }
        }

        public string Patronymic
        {
            get { return this.fPatronymic; }
        }

        public string Nick
        {
            get { return this.fNick; }
        }

        public Point Pt
        {
            get
            {
                return new Point(this.fPtX, this.fPtY);
            }
            set
            {
                this.fPtX = value.X;
                this.fPtY = value.Y;
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
            get
            {
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
            get
            {
                return this.fFlags.Contains(PersonFlag.pfSelected);
            }
            set
            {
                if (value)
                {
                    this.fFlags.Include(PersonFlag.pfSelected);
                }
                else
                {
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
            if (disposing)
            {
                if (this.fPortrait != null) this.fPortrait.Dispose();
                if (this.fChilds != null) this.fChilds.Dispose();
                if (this.fSpouses != null) this.fSpouses.Dispose();
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
            double xyaspect = (w / (double)h);
            if (w > h)
            {
                w = cw;
                h = (int)SysUtils.Trunc(cw / xyaspect);
                if (h > ch)
                {
                    h = ch;
                    w = (int)SysUtils.Trunc(ch * xyaspect);
                }
            }
            else
            {
                h = ch;
                w = (int)SysUtils.Trunc(ch * xyaspect);
                if (w > cw)
                {
                    w = cw;
                    h = (int)SysUtils.Trunc(cw / xyaspect);
                }
            }

            Rectangle result = new Rectangle(rt.Left, rt.Top, w, h);
            result.Offset((cw - w) / 2, (ch - h) / 2);
            return result;
        }

        public string GetFullName()
        {
            return this.Name + " " + this.fPatronymic; // attention: Name - combined property
        }

        public string GetLifeYears()
        {
            string result;

            if (this.fBirthYear == "")
            {
                result = "?";
            }
            else
            {
                result = this.fBirthYear;
            }

            if (this.IsDead)
            {
                if (this.fDeathYear == "")
                {
                    result += " - ?";
                }
                else
                {
                    result = result + " - " + this.fDeathYear;
                }
            }
            result = "[ " + result + " ]";
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

        public void BuildBy(GEDCOMIndividualRecord iRec, ref bool hasMediaFail)
        {
            try
            {
                this.fRec = iRec;

                if (iRec != null)
                {
                    if (this.fChart.fPreparedIndividuals.IndexOf(iRec.XRef) < 0)
                    {
                        this.fChart.fPreparedIndividuals.Add(iRec.XRef);
                    }

                    string fam, nam, pat;
                    iRec.aux_GetNameParts(out fam, out nam, out pat);
                    this.fFamily = fam;
                    this.fName = nam;
                    this.fPatronymic = pat;
                    this.fNick = iRec.aux_GetNickStr();
                    this.fBirthDate = GKUtils.GetBirthDate(iRec, DateFormat.dfDD_MM_YYYY, false);
                    this.fDeathDate = GKUtils.GetDeathDate(iRec, DateFormat.dfDD_MM_YYYY, false);
                    this.IsDead = !iRec.IsLive();
                    this.fSex = iRec.Sex;
                    this.fSigns = this.fChart.GetPersonSign(iRec);
                    this.fBirthYear = GKUtils.GetBirthDate(iRec, DateFormat.dfYYYY, false);
                    this.fDeathYear = GKUtils.GetDeathDate(iRec, DateFormat.dfYYYY, false);

                    if (this.fChart.Options.PortraitsVisible)
                    {
                        try
                        {
                            this.fPortrait = this.fChart.Base.GetPrimaryBitmap(iRec, -1, -1, true);
                        }
                        catch (MediaFileNotFoundException)
                        {
                            if (!hasMediaFail)
                            {
                                GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                                hasMediaFail = true;
                            }
                        }
                    }
                }
                else
                {
                    this.fFamily = "";
                    this.fName = "< ? >";
                    this.fPatronymic = "";
                    this.fNick = "";
                    this.fBirthDate = "";
                    this.fDeathDate = "";
                    this.IsDead = false;
                    this.fSex = GEDCOMSex.svNone;
                    this.fSigns = EnumSet<ChartPersonSign>.Create();
                }

                //this.CalcBounds();
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.BuildBy(): " + ex.Message);
                throw;
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

                if (this.fChart.Options.FamilyVisible)
                {
                    maxwid = this.TextWidth(g, this.fFamily);
                    lines++;
                }

                if (!this.fChart.Options.DiffLines)
                {
                    wt = this.TextWidth(g, this.GetFullName());
                    if (maxwid < wt) maxwid = wt;
                    lines++;
                }
                else
                {
                    wt = this.TextWidth(g, this.Name); // attention: this combined property
                    if (maxwid < wt) maxwid = wt;
                    lines++;

                    wt = this.TextWidth(g, this.fPatronymic);
                    if (maxwid < wt) maxwid = wt;
                    lines++;
                }

                if (!this.fChart.Options.OnlyYears)
                {
                    if (this.fChart.Options.BirthDateVisible)
                    {
                        wt = this.TextWidth(g, this.fBirthDate);
                        if (maxwid < wt) maxwid = wt;
                        lines++;
                    }

                    if (this.fChart.Options.DeathDateVisible)
                    {
                        wt = this.TextWidth(g, this.fDeathDate);
                        if (maxwid < wt) maxwid = wt;
                        lines++;
                    }
                }
                else
                {
                    wt = this.TextWidth(g, this.GetLifeYears());
                    if (maxwid < wt) maxwid = wt;
                    lines++;
                }

                if (this.fChart.Options.Kinship)
                {
                    wt = this.TextWidth(g, this.Kinship);
                    if (maxwid < wt) maxwid = wt;
                    lines++;
                }

                if (this.fChart.PathDebug)
                {
                    wt = this.TextWidth(g, this.PathDebug);
                    if (maxwid < wt) maxwid = wt;
                    lines++;
                }

                this.fWidth = maxwid + 20;
                this.fHeight = g.MeasureString("A", this.fChart.DrawFont).ToSize().Height * lines + 20;
                if (this.fChart.Options.PortraitsVisible && this.fPortrait != null)
                {
                    Rectangle prt = this.GetDestRect(ExtRect.Create(0, 0, this.fHeight - 1, this.fHeight - 1), this.fPortrait);
                    this.fPortraitWidth = prt.Right - prt.Left + 1;
                    this.fWidth += this.fPortraitWidth;
                }
            }
            finally
            {
                g.Dispose();
            }
        }

    }
}