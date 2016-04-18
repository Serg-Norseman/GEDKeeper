/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
using GKCore;
using GKCore.Types;

namespace GKUI.Charts
{
	public enum PersonFlag
	{
		pfDivorced, pfIsDead, pfSelected, pfIsDup, 
		pfDescByFather, pfDescByMother, // descending flags
		pfAncWalk, pfDescWalk, // walk flags
		pfHasInvAnc, pfHasInvDesc // invisible flags
	}

	/// <summary>
	/// 
	/// </summary>
	public class TreeChartPerson : BaseObject
    {
        protected TreeChartBox fChart;

        // strictly private
        private string fBirthDate;
        private string fDeathDate;
        private EnumSet<PersonFlag> fFlags;
        private string fName;
        private string fNick;
        private string fPatronymic;
        private string fSurname;
        private GEDCOMIndividualRecord fRec;
        private EnumSet<SpecialUserRef> fSigns;
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
        public float CertaintyAssessment;
        public TreeChartPerson Father;
        public TreeChartPerson Mother;
        public TreeChartPerson Parent;
        public bool CanExpand;
        public int Generation;
        public bool OutsideKin;
        public string Kinship;
        public string[] Lines;
        public IVertex Node;
        public string PathDebug;
        public ExtRect PortraitArea;


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

        public EnumSet<SpecialUserRef> Signs
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

        public bool HasFlag(PersonFlag flag)
        {
        	return this.fFlags.Contains(flag);
        }

        public void SetFlag(PersonFlag flag)
        {
        	this.fFlags.Include(flag);
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
                    this.fSex = iRec.Sex;

                    GEDCOMCustomEvent birthEvent, deathEvent;
                    iRec.GetLifeDates(out birthEvent, out deathEvent);
                    DateFormat dateFormat = (this.fChart.Options.OnlyYears) ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;

                    this.IsDead = (deathEvent != null);
                    this.fBirthDate = GKUtils.GEDCOMEventToDateStr(birthEvent, dateFormat, false);
                    this.fDeathDate = GKUtils.GEDCOMEventToDateStr(deathEvent, dateFormat, false);

                    if (this.fChart.Options.SignsVisible) {
                    	EnumSet<SpecialUserRef> signs = EnumSet<SpecialUserRef>.Create();
                    	
                    	int num = this.fRec.UserReferences.Count;
                    	for (int i = 0; i < num; i++)
                    	{
                    		string rs = this.fRec.UserReferences[i].StringValue;
                    		for (SpecialUserRef cps = SpecialUserRef.urRI_StGeorgeCross; cps <= SpecialUserRef.urLast; cps++)
                    		{
                    			if (rs == GKData.SpecialUserRefs[(int)cps]) signs.Include(cps);
                    		}
                    	}
                    	
                    	this.fSigns = signs;
                    } else {
                    	this.fSigns = EnumSet<SpecialUserRef>.Create();
                    }

                    if (this.fChart.Options.PortraitsVisible) {
                        try
                        {
                            this.fPortrait = this.fChart.Base.Context.GetPrimaryBitmap(iRec, -1, -1, true);
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
                    this.fSigns = EnumSet<SpecialUserRef>.Create();

                    this.CertaintyAssessment = 0.0f;
                }
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.BuildBy(): " + ex.Message);
                throw;
            }
        }

        private void InitInfo(int lines)
        {
        	this.Lines = new string[lines];

            try
            {
            	// prepare
            	string nameLine = this.fName;
            	if (this.fChart.Options.NickVisible && !string.IsNullOrEmpty(this.fNick)) {
            		nameLine += " \"" + this.fNick + "\"";
            	}

            	// create lines
                int idx = 0;

                if (this.fChart.Options.FamilyVisible) {
                	this.Lines[idx] = this.fSurname;
                    idx++;
                }

                if (!this.fChart.Options.DiffLines) {
                	this.Lines[idx] = nameLine + " " + this.fPatronymic; // attention: "Name" is combined property
                    idx++;
                } else {
                	this.Lines[idx] = nameLine;
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
                	string lifeYears = "[ ";
                	lifeYears += (this.fBirthDate == "") ? "?" : this.fBirthDate;
                	if (this.IsDead) {
                		lifeYears += (this.fDeathDate == "") ? " - ?" : " - " + this.fDeathDate;
                	}
                	lifeYears += " ]";

                	this.Lines[idx] = lifeYears;
                    idx++;
                }

                if (this.fChart.Options.Kinship) {
                    this.Lines[idx] = this.Kinship;
                    idx++;
                }

                if (this.fChart.PathDebug) {
                    this.Lines[idx] = this.PathDebug;
                    //idx++;
                }
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.InitInfo(): " + ex.Message);
            }
        }

		private void DefineExpands()
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

        private int TextWidth(Graphics gfx, string st)
        {
            return gfx.MeasureString(st, this.fChart.DrawFont).ToSize().Width;
        }

        public void CalcBounds(int lines, Graphics gfx)
        {
            try
            {
            	this.InitInfo(lines);
            	this.DefineExpands();
            	
                int maxwid = 0;
                for (int k = 0; k < lines; k++) {
                	int wt = this.TextWidth(gfx, this.Lines[k]);
                    if (maxwid < wt) maxwid = wt;
                }

                this.fWidth = maxwid + 20;
                this.fHeight = gfx.MeasureString("A", this.fChart.DrawFont).ToSize().Height * lines + 20;

                if (this.fPortrait != null) {
                	ExtRect portRt = ExtRect.Create(0, 0, this.fHeight - 1, this.fHeight - 1);
                	portRt.Inflate(3, 3);

                    int rtW = portRt.GetWidth();
                    int rtH = portRt.GetHeight();
                    int imgW = fPortrait.Width;
                    int imgH = fPortrait.Height;
                    float ratio = GfxHelper.ZoomToFit(imgW, imgH, rtW, rtH);
                    imgW = (int)Math.Round(imgW * ratio);
                    imgH = (int)Math.Round(imgH * ratio);

                    this.PortraitArea = ExtRect.CreateBounds(portRt.Left, portRt.Top, imgW, imgH);
                    this.fPortraitWidth = imgW;

                    this.fWidth += imgW;
                }
            }
            catch (Exception ex)
            {
                this.fChart.Base.Host.LogWrite("TreeChartPerson.CalcBounds(): " + ex.Message);
            }
        }

    }
}