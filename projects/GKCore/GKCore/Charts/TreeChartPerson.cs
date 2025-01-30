/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using BSLib;
using BSLib.DataViz.SmartGraph;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design.Graphics;
using GKCore.Import;
using GKCore.Options;
using GKCore.Types;
using BSDColors = GKCore.Design.BSDConsts.Colors;

namespace GKCore.Charts
{
    /// <summary>
    ///
    /// </summary>
    public class PersonModifyEventArgs : EventArgs
    {
        public TreeChartPerson Person { get; set; }

        public PersonModifyEventArgs(TreeChartPerson person)
        {
            Person = person;
        }
    }

    public enum PersonFlag
    {
        pfDivorced, pfIsDead, pfSelected, pfIsDup, pfSpouse,
        pfDescByFather, pfDescByMother, // descending flags
        pfAncWalk, pfDescWalk, // walk flags
        pfHasInvAnc, pfHasInvDesc, // invisible flags
        pfSpecialMark, // debug flag for special goals
        pfOutsideKin, pfCanExpand, pfAdopted,
        pfRootSpouse, pfBookmark, pfCommonLaw
    }

    /// <summary>
    ///
    /// </summary>
    public class TreeChartPerson
    {
        private readonly TreeChartModel fModel;

        private string fAge;
        private string fBirthDate;
        private string fBirthPlace;
        private string fDeathDate;
        private string fDeathPlace;
        private EnumSet<PersonFlag> fFlags;
        private string fName;
        private string fNick;
        private string fPatronymic;
        private string fSurname;
        private GDMIndividualRecord fRec;
        private EnumSet<SpecialUserRef> fSigns;
        private GDMSex fSex;
        private List<TreeChartPerson> fChilds;
        private List<TreeChartPerson> fSpouses;
        private IImage fPortrait;
        private int fPortraitWidth;

        // for rendering
        private int fHeight;
        private int fPtX;
        private int fPtY;
        private int fWidth;

        // without property controlling
        public GDMFamilyRecord BaseFamily;
        public TreeChartPerson BaseSpouse;
        public float CertaintyAssessment;
        public TreeChartPerson Father;
        public TreeChartPerson Mother;
        public string FatherAge;
        public string MotherAge;
        public int Generation;
        public string Kinship;
        public string[] Lines;
        public Vertex Node;
        public ExtRect PortraitArea;
        public string MarriageDate;
        public bool IsCollapsed;
        public bool IsVisible;
        public IColor UserColor;
        public int NameLines;
        public string Note;
        public GDMSourceCitation[] Sources;


        public int Height
        {
            get { return fHeight; }
        }

        public bool IsDup
        {
            get {
                return fFlags.Contains(PersonFlag.pfIsDup);
            }
            set {
                if (value) {
                    fFlags.Include(PersonFlag.pfIsDup);
                } else {
                    fFlags.Exclude(PersonFlag.pfIsDup);
                }
            }
        }

        public IImage Portrait
        {
            get { return fPortrait; }
        }

        public int PortraitWidth
        {
            get { return fPortraitWidth; }
        }

        public int PtX
        {
            get { return fPtX; }
            set { fPtX = value; }
        }

        public int PtY
        {
            get { return fPtY; }
            set { fPtY = value; }
        }

        public GDMIndividualRecord Rec
        {
            get { return fRec; }
        }

        public ExtRect Rect
        {
            get {
                ExtRect result;
                result.Left = fPtX - fWidth / 2;
                result.Right = result.Left + fWidth - 1;
                result.Top = fPtY;
                result.Bottom = result.Top + fHeight - 1;
                return result;
            }
        }

        public bool Selected
        {
            get {
                return fFlags.Contains(PersonFlag.pfSelected);
            }
            set {
                if (value) {
                    fFlags.Include(PersonFlag.pfSelected);
                } else {
                    fFlags.Exclude(PersonFlag.pfSelected);
                }
            }
        }

        public GDMSex Sex
        {
            get { return fSex; }
            set { fSex = value; }
        }

        public EnumSet<SpecialUserRef> Signs
        {
            get { return fSigns; }
        }

        public int Width
        {
            get { return fWidth; }
            set { fWidth = value; }
        }


        public TreeChartPerson(TreeChartModel model)
        {
            fModel = model;
            fFlags = EnumSet<PersonFlag>.Create();
            fPortrait = null;
            fSpouses = null;
            fChilds = null;
            UserColor = null;
        }

        public bool HasFlag(PersonFlag flag)
        {
            return fFlags.Contains(flag);
        }

        public void SetFlag(PersonFlag flag)
        {
            fFlags.Include(flag);
        }

        public void SetFlag(PersonFlag flag, bool value)
        {
            if (value) {
                fFlags.Include(flag);
            } else {
                fFlags.Exclude(flag);
            }
        }

        public TreeChartPerson GetChild(int index)
        {
            TreeChartPerson result = ((fChilds == null) ? null : fChilds[index]);
            return result;
        }

        public int GetChildsCount()
        {
            int result = ((fChilds == null) ? 0 : fChilds.Count);
            return result;
        }

        public TreeChartPerson GetSpouse(int index)
        {
            TreeChartPerson result = ((fSpouses == null) ? null : fSpouses[index]);
            return result;
        }

        public int GetSpousesCount()
        {
            int result = ((fSpouses == null) ? 0 : fSpouses.Count);
            return result;
        }

        public int IndexOfSpouse(TreeChartPerson spouse)
        {
            return (fSpouses == null) ? -1 : fSpouses.IndexOf(spouse);
        }

        public void AddChild(TreeChartPerson child)
        {
            if (child == null) return;

            if (fChilds == null) fChilds = new List<TreeChartPerson>();

            fChilds.Add(child);
        }

        public void AddSpouse(TreeChartPerson spouse)
        {
            if (spouse == null) return;

            if (fSpouses == null) fSpouses = new List<TreeChartPerson>();

            fSpouses.Add(spouse);
        }

        public void BuildBy(GDMIndividualRecord iRec)
        {
            try {
                fRec = iRec;

                fSigns = EnumSet<SpecialUserRef>.Create();
                Note = string.Empty;

                if (iRec != null) {
                    if (!fModel.PreparedIndividuals.Contains(iRec.XRef)) {
                        fModel.PreparedIndividuals.Add(iRec.XRef);
                    }

                    var baseContext = fModel.Base.Context;
                    var parts = GKUtils.GetNameParts(baseContext.Tree, iRec, true, baseContext.DefaultLanguage);
                    fSurname = parts.Surname;
                    fName = parts.Name;
                    fPatronymic = parts.Patronymic;
                    fNick = GKUtils.GetNickString(iRec);
                    fSex = iRec.Sex;

                    TreeChartOptions options = fModel.Options;

                    var lifeDates = iRec.GetLifeEvents(true);
                    GDMCustomEvent birthEvent = lifeDates.BirthEvent;
                    GDMCustomEvent deathEvent = lifeDates.DeathEvent;
                    string birthSign = ImportUtils.STD_BIRTH_SIGN;
                    string deathSign = ImportUtils.STD_DEATH_SIGN;
                    if (options.UseAdditionalDates) {
                        if (birthEvent == null) {
                            birthEvent = lifeDates.BaptismEvent;
                            if (birthEvent != null) birthSign = ImportUtils.STD_BAPTISM_SIGN;
                        }
                        if (deathEvent == null) {
                            deathEvent = lifeDates.BurialEvent;
                            if (deathEvent != null) deathSign = ImportUtils.STD_BURIED_SIGN;
                        }
                    }

                    DateFormat dateFormat = (options.OnlyYears) ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;
                    bool shortenDateRanges = options.ShortenDateRanges && options.OnlyYears;

                    SetFlag(PersonFlag.pfIsDead, (deathEvent != null));
                    GlobalOptions glob = GlobalOptions.Instance;
                    fBirthDate = GKUtils.GEDCOMEventToDateStr(birthEvent, dateFormat, glob.ShowDatesSign, shortenDateRanges);
                    fDeathDate = GKUtils.GEDCOMEventToDateStr(deathEvent, dateFormat, glob.ShowDatesSign, shortenDateRanges);

                    if (options.ShowPlaces) {
                        fBirthPlace = GKUtils.GetPlaceStr(birthEvent, false, options.OnlyLocality);
                        if (!string.IsNullOrEmpty(fBirthPlace) && !options.SeparateDatesAndPlacesLines) {
                            if (!string.IsNullOrEmpty(fBirthDate)) {
                                fBirthDate += ", ";
                            }
                            fBirthDate += fBirthPlace;
                        }

                        fDeathPlace = GKUtils.GetPlaceStr(deathEvent, false, options.OnlyLocality);
                        if (!string.IsNullOrEmpty(fDeathPlace) && !options.SeparateDatesAndPlacesLines) {
                            if (!string.IsNullOrEmpty(fDeathDate)) {
                                fDeathDate += ", ";
                            }
                            fDeathDate += fDeathPlace;
                        }
                    }

                    if (options.AgeVisible) {
                        int age = GKUtils.GetAgeLD(lifeDates, -1);
                        fAge = (age == -1) ? "" : age.ToString();
                    } else {
                        fAge = "";
                    }

                    if (!string.IsNullOrEmpty(fBirthDate) && options.DateDesignations) {
                        fBirthDate = birthSign + " " + fBirthDate;
                    }
                    if (!string.IsNullOrEmpty(fDeathDate) && options.DateDesignations) {
                        fDeathDate = deathSign + " " + fDeathDate;
                    }

                    if ((options.SignsVisible || options.URNotesVisible) && fRec.HasUserReferences) {
                        int num = fRec.UserReferences.Count;
                        for (int i = 0; i < num; i++) {
                            var uRef = fRec.UserReferences[i];
                            string refVal = uRef.StringValue;
                            string refType = uRef.ReferenceType;

                            if (options.SignsVisible) {
                                for (var cps = SpecialUserRef.urRI_StGeorgeCross; cps <= SpecialUserRef.urLast; cps++) {
                                    string sur = LangMan.LS(GKData.SpecialUserRefs[(int)cps].Title);
                                    if (refVal == sur) {
                                        fSigns.Include(cps);
                                    }
                                }
                            }

                            if (options.URNotesVisible) {
                                if (refType == LangMan.LS(GKData.URTreeNoteType)) {
                                    Note = refVal;
                                }
                            }
                        }
                    }

                    if (options.PortraitsVisible) {
                        try {
                            fPortrait = PortraitsCache.Instance.GetImage(baseContext, iRec);

                            if (fPortrait == null && options.DefaultPortraits) {
                                string resImage = GKData.SexData[(int)fSex].DefPortraitImage;
                                fPortrait = fModel.Renderer.LoadResourceImage(resImage, false);
                            }
                        } catch (MediaFileNotFoundException) {
                            if (!fModel.HasMediaFail) {
                                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                                fModel.HasMediaFail = true;
                            }
                        }
                    }

                    SetFlag(PersonFlag.pfBookmark, iRec.Bookmark);

                    CertaintyAssessment = GKUtils.GetCertaintyAssessment(iRec);

                    if (options.TrackMatchedSources && fRec.HasSourceCitations) {
                        int num = fRec.SourceCitations.Count;
                        Sources = new GDMSourceCitation[num];
                        for (int i = 0; i < num; i++) {
                            var srcCit = fRec.SourceCitations[i];
                            //string srcRef = srcCit.XRef /*+ ":" + srcCit.Page*/;
                            Sources[i] = srcCit;
                        }
                    } else {
                        Sources = new GDMSourceCitation[0];
                    }
                } else {
                    fSurname = "";
                    fName = "< ? >";
                    fPatronymic = "";
                    fNick = "";
                    fBirthDate = "";
                    fBirthPlace = "";
                    fDeathDate = "";
                    fDeathPlace = "";
                    fAge = "";
                    SetFlag(PersonFlag.pfIsDead, false);
                    fSex = GDMSex.svUnknown;

                    CertaintyAssessment = 0.0f;
                    Sources = new GDMSourceCitation[0];
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartPerson.BuildBy()", ex);
                throw;
            }
        }

        public void SetParents()
        {
            if (!fModel.Options.ParentAges || fRec == null) return;

            FatherAge = string.Empty;
            MotherAge = string.Empty;

            GDMCustomEvent evtChild = fRec.FindEvent(GEDCOMTagName.BIRT);
            if (evtChild == null || !evtChild.Date.GetUDN().HasKnownYear()) return;

            if (Father != null && Father.Rec != null) {
                var evtFth = Father.Rec.FindEvent(GEDCOMTagName.BIRT);
                var diff = GKUtils.GetEventsYearsDiff(evtFth, evtChild, false);
                FatherAge = (diff != -1) ? diff.ToString() : string.Empty;
            }

            if (Mother != null && Mother.Rec != null) {
                var evtMth = Mother.Rec.FindEvent(GEDCOMTagName.BIRT);
                var diff = GKUtils.GetEventsYearsDiff(evtMth, evtChild, false);
                MotherAge = (diff != -1) ? diff.ToString() : string.Empty;
            }
        }

        internal static int InitInfoSize(TreeChartOptions options)
        {
            int lines = 0;

            if (options.FullNameOnOneLine) {
                lines += 3;
            } else {
                if (options.FamilyVisible) {
                    lines++;
                }

                if (!options.DiffLines) {
                    lines++;
                } else {
                    lines++;
                    lines++;
                }
            }

            if (options.OnlyYears && !options.ShowPlaces) {
                lines++;
            } else {
                if (options.BirthDateVisible) {
                    lines++;
                    if (options.SeparateDatesAndPlacesLines) {
                        lines++;
                    }
                }
                if (options.DeathDateVisible) {
                    lines++;
                    if (options.SeparateDatesAndPlacesLines) {
                        lines++;
                    }
                }
            }

            if (options.Kinship) {
                lines++;
            }

            if (options.URNotesVisible) {
                lines++;
            }

            return lines;
        }

        private void InitInfo(int lines)
        {
            Lines = new string[lines];

            try {
                TreeChartOptions options = fModel.Options;

                // prepare
                string nameLine = fName;
                if (options.NickVisible && !string.IsNullOrEmpty(fNick)) {
                    nameLine += " \"" + fNick + "\"";
                }

                NameLines = 0;

                // create lines
                int idx = 0;

                if (options.FullNameOnOneLine) {
                    if (options.SurnameFirstInOrder) {
                        NameLines++;
                        idx++;
                        Lines[idx] = fSurname + " " + nameLine;
                        NameLines += 2;
                        idx += 2;
                    } else {
                        NameLines++;
                        idx++;
                        Lines[idx] = nameLine + " " + fSurname;
                        NameLines += 2;
                        idx += 2;
                    }
                } else {
                    if (options.SurnameFirstInOrder) {
                        if (options.FamilyVisible) {
                            Lines[idx] = fSurname;
                            NameLines++;
                            idx++;
                        }

                        if (!options.DiffLines) {
                            Lines[idx] = nameLine + " " + fPatronymic; // attention: "Name" is combined property
                            NameLines++;
                            idx++;
                        } else {
                            Lines[idx] = nameLine;
                            NameLines++;
                            idx++;

                            Lines[idx] = fPatronymic;
                            NameLines++;
                            idx++;
                        }
                    } else {
                        if (!options.DiffLines) {
                            Lines[idx] = nameLine + " " + fPatronymic; // attention: "Name" is combined property
                            NameLines++;
                            idx++;
                        } else {
                            Lines[idx] = nameLine;
                            NameLines++;
                            idx++;

                            Lines[idx] = fPatronymic;
                            NameLines++;
                            idx++;
                        }

                        if (options.FamilyVisible) {
                            Lines[idx] = fSurname;
                            NameLines++;
                            idx++;
                        }
                    }
                }

                if (options.OnlyYears && !options.ShowPlaces) {
                    string lifeYears = string.Empty;
                    if (options.DateDesignations) lifeYears = "[ ";

                    lifeYears += (fBirthDate == "") ? "?" : fBirthDate;
                    if (HasFlag(PersonFlag.pfIsDead)) {
                        lifeYears += (fDeathDate == "") ? " - ?" : " - " + fDeathDate;
                    }
                    if (!string.IsNullOrEmpty(fAge)) {
                        lifeYears += string.Concat(" (", fAge, ")");
                    }

                    if (options.DateDesignations) lifeYears += " ]";

                    Lines[idx] = lifeYears;
                    idx++;
                } else {
                    if (options.BirthDateVisible) {
                        Lines[idx] = fBirthDate;
                        idx++;

                        if (options.SeparateDatesAndPlacesLines) {
                            Lines[idx] = fBirthPlace;
                            idx++;
                        }
                    }

                    if (options.DeathDateVisible) {
                        Lines[idx] = fDeathDate;
                        idx++;

                        if (options.SeparateDatesAndPlacesLines) {
                            Lines[idx] = fDeathPlace;
                            idx++;
                        }
                    }
                }

                if (options.Kinship) {
                    Lines[idx] = Kinship;
                    idx++;
                }

                if (options.URNotesVisible) {
                    Lines[idx] = Note;
                    idx++;
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartPerson.InitInfo()", ex);
            }
        }

        private void DefineExpands()
        {
            if (fFlags.Contains(PersonFlag.pfAncWalk) && fFlags.Contains(PersonFlag.pfDescWalk)
                && fFlags.Contains(PersonFlag.pfHasInvDesc))
            {
                // it's hack
                fFlags.Exclude(PersonFlag.pfHasInvDesc);
            }

            SetFlag(PersonFlag.pfCanExpand, fFlags.Contains(PersonFlag.pfHasInvAnc) || fFlags.Contains(PersonFlag.pfHasInvDesc));
        }

        public void CalcBounds(int lines, ChartRenderer renderer)
        {
            try {
                TreeChartOptions options = fModel.Options;

                InitInfo(lines);
                DefineExpands();

                int bh = renderer.GetTextHeight(fModel.BoldFont);
                int th = renderer.GetTextHeight(fModel.DrawFont);

                int maxwid = 0;
                int height = 0;
                for (int k = 0; k < lines; k++) {
                    IFont font;
                    if (options.BoldNames && k < NameLines) {
                        height += bh;
                        font = fModel.BoldFont;
                    } else {
                        height += th;
                        font = fModel.DrawFont;
                    }

                    int wt = renderer.GetTextWidth(Lines[k], font);
                    if (!options.URNotesVisible || Lines[k] != Note) {
                        if (maxwid < wt) maxwid = wt;
                    }
                }

                int pad2side = fModel.NodePadding * 2;
                fWidth = pad2side + maxwid;
                fHeight = pad2side + height;

                if (fPortrait != null) {
                    ExtRect portRt = ExtRect.Create(0, 0, fHeight - 1, fHeight - 1);
                    portRt.Inflate(-3, -3);

                    int rtW = portRt.GetWidth();
                    int rtH = portRt.GetHeight();
                    int imgW = fPortrait.Width;
                    int imgH = fPortrait.Height;
                    float ratio = GfxHelper.ZoomToFit(imgW, imgH, rtW, rtH);
                    imgW = (int)Math.Round(imgW * ratio);
                    imgH = (int)Math.Round(imgH * ratio);

                    PortraitArea = ExtRect.CreateBounds(portRt.Left, portRt.Top, imgW, imgH);
                    fPortraitWidth = imgW;

                    fWidth += imgW;
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartPerson.CalcBounds()", ex);
            }
        }

        public IColor GetSelectedColor()
        {
            int result;

            switch (fSex) {
                case GDMSex.svMale:
                    result = BSDColors.Blue;
                    break;

                case GDMSex.svFemale:
                    result = BSDColors.Red;
                    break;

                default:
                    result = BSDColors.Black;
                    break;
            }

            return ChartRenderer.GetColor(result);
        }

        public IColor GetFillColor(bool dead)
        {
            if (fFlags.Contains(PersonFlag.pfSpecialMark)) {
                return ChartRenderer.GetColor(BSDColors.Khaki);
            }

            if (dead) {
                return ChartRenderer.GetColor(BSDColors.Black);
            }

            if (IsDup) {
                return ChartRenderer.GetColor(BSDColors.Silver);
            }

            if (UserColor != null) {
                return UserColor;
            }

            bool divorced = HasFlag(PersonFlag.pfDivorced);

            IColor result;
            TreeChartOptions options = fModel.Options;
            switch (fSex) {
                case GDMSex.svMale:
                    result = divorced ? options.UnHusbandColor : options.MaleColor;
                    break;

                case GDMSex.svFemale:
                    result = divorced ? options.UnWifeColor : options.FemaleColor;
                    break;

                default:
                    result = options.UnkSexColor;
                    break;
            }
            return result;
        }

        public bool IntersectSources(TreeChartPerson other)
        {
            //var results = this.Sources.Intersect(other.Sources, StringComparer.Ordinal);
            //return results.Any();

            for (int i = 0; i < Sources.Length; i++) {
                var srcCit1 = Sources[i];

                for (int k = 0; k < other.Sources.Length; k++) {
                    var srcCit2 = other.Sources[k];

                    if (string.Equals(srcCit1.XRef, srcCit2.XRef)) {
                        /*int diff = SysUtils.StrDifference(srcCit1.Page, srcCit2.Page);
                        // One family can be listed on 1-2 pages of a census or other source,
                        // therefore the difference in page numbers should not exceed 1 character.
                        // But what to do with cases like "p. 235" and "p. 235bck" ("л. 235" and "л. 235об")?
                        if (diff == 0 || diff == 1) {
                            return true;
                        }*/

                        return true;
                    }
                }
            }

            return false;
        }
    }
}
