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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Controls;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public enum PersonColumnType
    {
        pctPatriarch,
        pctName,
        pctNick,
        pctSex,
        pctBirthDate,
        pctDeathDate,
        pctBirthPlace,
        pctDeathPlace,
        pctResidence,
        pctAge,
        pctLifeExpectancy,
        pctDaysForBirth,
        pctGroups,
        pctReligion,
        pctNationality,
        pctEducation,
        pctOccupation,
        pctCaste,
        pctMili,
        pctMiliInd,
        pctMiliDis,
        pctMiliRank,
        pctChangeDate,
        pctBookmark,
        pctTitle
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class IndividualListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            this.AddStatic(LSID.LSID_Patriarch, DataType.dtString, 25, true);
            this.AddStatic(LSID.LSID_FullName, DataType.dtString, 300, true);
            this.AddStatic(LSID.LSID_Nickname, DataType.dtString, 75, false);
            this.AddStatic(LSID.LSID_Sex, DataType.dtString, 45, true);
            this.AddStatic(LSID.LSID_BirthDate, DataType.dtGEDCOMDate, 100, true);
            this.AddStatic(LSID.LSID_DeathDate, DataType.dtGEDCOMDate, 100, true);
            this.AddStatic(LSID.LSID_BirthPlace, DataType.dtString, 100, true);
            this.AddStatic(LSID.LSID_DeathPlace, DataType.dtString, 100, true);
            this.AddStatic(LSID.LSID_Residence, DataType.dtString, 100, true);
            this.AddStatic(LSID.LSID_Age, DataType.dtInteger, 100, false);
            this.AddStatic(LSID.LSID_LifeExpectancy, DataType.dtInteger, 100, false);
            this.AddStatic(LSID.LSID_DaysForBirth, DataType.dtString, 100, false);
            this.AddStatic(LSID.LSID_RPGroups, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Religion, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Nationality, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Education, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Occupation, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Caste, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Mili, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_MiliInd, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_MiliDis, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_MiliRank, DataType.dtString, 200, false);
            this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
            this.AddStatic(LSID.LSID_Bookmark, DataType.dtString, 25, true);
            this.AddStatic(LSID.LSID_NobilityTitle, DataType.dtString, 200, false);
        }

        public IndividualListColumns()
        {
            InitData(typeof(PersonColumnType));
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class IndividualListFilter : ListFilter, IIndividualListFilter
    {
        public string AliveBeforeDate;
        public FilterGroupMode FilterGroupMode;
        public string GroupRef;
        public string Name;
        public bool PatriarchOnly;
        public string Residence;
        public GEDCOMSex Sex;
        public FilterGroupMode SourceMode;
        public string SourceRef;
        public string EventVal;

        public FilterLifeMode FilterLifeMode { get; set; }

        public IndividualListFilter()
        {
            this.Clear();
        }

        public override void Clear()
        {
            base.Clear();

            this.FilterGroupMode = FilterGroupMode.All;
            this.GroupRef = "";
            if (this.FilterLifeMode != FilterLifeMode.lmTimeLocked)
            {
                this.FilterLifeMode = FilterLifeMode.lmAll;
            }
            this.Name = "*";
            this.AliveBeforeDate = "";
            this.PatriarchOnly = false;
            this.Residence = "*";
            this.Sex = GEDCOMSex.svNone;
            this.SourceMode = FilterGroupMode.All;
            this.SourceRef = "";
            this.EventVal = "*";
        }
    }

    public sealed class IndividualListMan : ListManager
    {
        private GEDCOMIndividualRecord fRec;
        private GEDCOMGroupRecord filter_grp;
        private AbsDate filter_abd;
        private GEDCOMSourceRecord filter_source;

        protected override void CreateFilter()
        {
            this.fFilter = new IndividualListFilter();
        }

        private string GetGroups()
        {
            string result = "";

            int count = this.fRec.Groups.Count;
            for (int idx = 0; idx < count; idx++)
            {
                GEDCOMGroupRecord grp = this.fRec.Groups[idx].Value as GEDCOMGroupRecord;
                if (grp != null)
                {
                    result += grp.GroupName;
                    if (idx < count - 1) result += "; ";
                }
            }

            return result;
        }

        private bool HasPlace()
        {
            IndividualListFilter iFilter = fFilter as IndividualListFilter;
            if (iFilter == null) return false;

            bool res = false;

            bool addr = GlobalOptions.Instance.PlacesWithAddress;
            int num = this.fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                string place = GKUtils.GetPlaceStr(this.fRec.Events[i], addr);
                res = IsMatchesMask(place, iFilter.Residence);
                if (res) break;
            }

            return res;
        }

        private bool HasEventVal()
        {
            IndividualListFilter iFilter = fFilter as IndividualListFilter;
            if (iFilter == null) return false;

            bool result = false;

            int num = this.fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                result = IsMatchesMask(this.fRec.Events[i].StringValue, iFilter.EventVal);
                if (result) break;
            }

            return result;
        }

        private bool CheckSpecificFilter()
        {
            bool result = false;

            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            if ((iFilter.Sex == GEDCOMSex.svNone || this.fRec.Sex == iFilter.Sex)
                && (iFilter.Name == "*" || IsMatchesMask(buf_fullname, iFilter.Name))
                && (iFilter.Residence == "*" || this.HasPlace())
                && (iFilter.EventVal == "*" || this.HasEventVal())
                && (!iFilter.PatriarchOnly || this.fRec.Patriarch))
            {
                bool isLive = (buf_dd == null);

                switch (iFilter.FilterLifeMode) {
                    case FilterLifeMode.lmOnlyAlive:
                        if (!isLive) return false;
                        break;

                    case FilterLifeMode.lmOnlyDead:
                        if (isLive) return false;
                        break;

                    case FilterLifeMode.lmAliveBefore:
                        AbsDate bdt = GEDCOMUtils.GetAbstractDate(buf_bd);
                        AbsDate ddt = GEDCOMUtils.GetAbstractDate(buf_dd);
                        if ((bdt > this.filter_abd) || (ddt < this.filter_abd)) return false;
                        break;

                    case FilterLifeMode.lmTimeLocked:
                        break;
                }

                switch (iFilter.FilterGroupMode) {
                    case FilterGroupMode.All:
                        break;
                    case FilterGroupMode.None:
                        if (this.fRec.Groups.Count != 0) return false;
                        break;
                    case FilterGroupMode.Any:
                        if (this.fRec.Groups.Count == 0) return false;
                        break;
                    case FilterGroupMode.Selected:
                        if (this.fRec.IndexOfGroup(this.filter_grp) < 0) return false;
                        break;
                }

                switch (iFilter.SourceMode) {
                    case FilterGroupMode.All:
                        break;
                    case FilterGroupMode.None:
                        if (this.fRec.SourceCitations.Count != 0) return false;
                        break;
                    case FilterGroupMode.Any:
                        if (this.fRec.SourceCitations.Count == 0) return false;
                        break;
                    case FilterGroupMode.Selected:
                        if (this.fRec.IndexOfSource(this.filter_source) < 0) return false;
                        break;
                }

                result = true;
            }

            return result;
        }

        public override bool CheckFilter(ShieldState shieldState)
        {
            bool res = (GKUtils.IsRecordAccess(this.fRec.Restriction, shieldState)
                        && (this.QuickFilter == "*" || IsMatchesMask(buf_fullname, this.QuickFilter)));

            res = res && base.CheckCommonFilter() && this.CheckSpecificFilter();

            if (this.fExternalFilter != null) {
                res = res && this.fExternalFilter(this.fRec);
            }

            return res;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            PersonColumnType pct = (PersonColumnType)colType;

            object result = null;

            switch (pct) {
                case PersonColumnType.pctPatriarch:
                    result = ((this.fRec.Patriarch) ? "*" : " ");
                    break;

                case PersonColumnType.pctName:
                    {
                        if (colSubtype == -1) {
                            result = this.fRec.GetNameString(true, false);
                        } else {
                            NameFormat defNameFormat = GlobalOptions.Instance.DefNameFormat;
                            string f, i, p;

                            switch (defNameFormat) {
                                case NameFormat.nfFNP:
                                    result = this.fRec.GetNameString(true, false);
                                    break;

                                case NameFormat.nfF_NP:
                                    this.fRec.GetNameParts(out f, out i, out p);
                                    switch (colSubtype) {
                                        case 0:
                                            result = f;
                                            break;
                                        case 1:
                                            result = i + " " + p;
                                            break;
                                    }
                                    break;

                                case NameFormat.nfF_N_P:
                                    this.fRec.GetNameParts(out f, out i, out p);
                                    switch (colSubtype) {
                                        case 0:
                                            result = f;
                                            break;
                                        case 1:
                                            result = i;
                                            break;
                                        case 2:
                                            result = p;
                                            break;
                                    }
                                    break;
                            }
                        }
                        
                        break;
                    }

                case PersonColumnType.pctNick:
                    result = this.fRec.GetNickString();
                    break;

                case PersonColumnType.pctSex:
                    result = new string(GKUtils.SexStr(this.fRec.Sex)[0], 1);
                    break;

                case PersonColumnType.pctBirthDate:
                    result = GetDateValue(buf_bd, isVisible);
                    break;

                case PersonColumnType.pctBirthPlace:
                    result = GKUtils.GetPlaceStr(buf_bd, false);
                    break;

                case PersonColumnType.pctDeathDate:
                    result = GetDateValue(buf_dd, isVisible);
                    break;

                case PersonColumnType.pctDeathPlace:
                    result = GKUtils.GetPlaceStr(buf_dd, false);
                    break;

                case PersonColumnType.pctResidence:
                    result = buf_residence;
                    break;

                case PersonColumnType.pctAge:
                    result = (isVisible) ? (object)GKUtils.GetAgeStr(this.fRec, -1) : GKUtils.GetAge(this.fRec, -1);
                    break;

                case PersonColumnType.pctLifeExpectancy:
                    result = (isVisible) ? (object)GKUtils.GetLifeExpectancyStr(this.fRec) : GKUtils.GetLifeExpectancy(this.fRec);
                    break;

                case PersonColumnType.pctDaysForBirth:
                    result = GKUtils.GetDaysForBirth(this.fRec);
                    break;

                case PersonColumnType.pctGroups:
                    result = this.GetGroups();
                    break;

                case PersonColumnType.pctReligion:
                    result = buf_religion;
                    break;

                case PersonColumnType.pctNationality:
                    result = buf_nationality;
                    break;

                case PersonColumnType.pctEducation:
                    result = buf_education;
                    break;

                case PersonColumnType.pctOccupation:
                    result = buf_occupation;
                    break;

                case PersonColumnType.pctCaste:
                    result = buf_caste;
                    break;

                case PersonColumnType.pctMili:
                    result = buf_mili;
                    break;

                case PersonColumnType.pctMiliInd:
                    result = buf_mili_ind;
                    break;

                case PersonColumnType.pctMiliDis:
                    result = buf_mili_dis;
                    break;

                case PersonColumnType.pctMiliRank:
                    result = buf_mili_rank;
                    break;

                case PersonColumnType.pctChangeDate:
                    result = this.fRec.ChangeDate.ChangeDateTime;
                    break;

                case PersonColumnType.pctBookmark:
                    result = ((this.fRec.Bookmark) ? "*" : " ");
                    break;

                case PersonColumnType.pctTitle:
                    result = buf_title;
                    break;
            }
            return result;
        }

        public override void InitFilter()
        {
            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            this.filter_abd = GEDCOMUtils.GetAbstractDate(iFilter.AliveBeforeDate);

            if (iFilter.GroupRef == "") {
                this.filter_grp = null;
            } else {
                this.filter_grp = this.fTree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
            }

            if (iFilter.SourceRef == "") {
                this.filter_source = null;
            } else {
                this.filter_source = this.fTree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
            }
        }

        private GEDCOMCustomEvent buf_bd;
        private GEDCOMCustomEvent buf_dd;

        private string buf_fullname;
        private string buf_residence;
        private string buf_religion;
        private string buf_nationality;
        private string buf_education;
        private string buf_occupation;
        private string buf_caste;
        private string buf_mili;
        private string buf_mili_ind;
        private string buf_mili_dis;
        private string buf_mili_rank;
        private string buf_title;

        public override void Fetch(GEDCOMRecord aRec)
        {
            this.fRec = (aRec as GEDCOMIndividualRecord);
            if (this.fRec == null) return;

            buf_fullname = this.fRec.GetNameString(true, false);
            buf_bd = null;
            buf_dd = null;
            buf_residence = "";
            buf_religion = "";
            buf_nationality = "";
            buf_education = "";
            buf_occupation = "";
            buf_caste = "";
            buf_mili = "";
            buf_mili_ind = "";
            buf_mili_dis = "";
            buf_mili_rank = "";
            buf_title = "";

            GlobalOptions gOptions = GlobalOptions.Instance;

            int num = this.fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMCustomEvent ev = this.fRec.Events[i];

                if (ev.Name == "BIRT" && buf_bd == null)
                {
                    buf_bd = ev;
                }
                else if (ev.Name == "DEAT" && buf_dd == null)
                {
                    buf_dd = ev;
                }
                else if (ev.Name == "RESI" && buf_residence == "")
                {
                    buf_residence = GKUtils.GetPlaceStr(ev, gOptions.PlacesWithAddress);
                }
                else if (ev.Name == "RELI" && buf_religion == "")
                {
                    buf_religion = ev.StringValue;
                }
                else if (ev.Name == "NATI" && buf_nationality == "")
                {
                    buf_nationality = ev.StringValue;
                }
                else if (ev.Name == "EDUC" && buf_education == "")
                {
                    buf_education = ev.StringValue;
                }
                else if (ev.Name == "OCCU" && buf_occupation == "")
                {
                    buf_occupation = ev.StringValue;
                }
                else if (ev.Name == "CAST" && buf_caste == "")
                {
                    buf_caste = ev.StringValue;
                }
                else if (ev.Name == "_MILI" && buf_mili == "")
                {
                    buf_mili = ev.StringValue;
                }
                else if (ev.Name == "_MILI_IND" && buf_mili_ind == "")
                {
                    buf_mili_ind = ev.StringValue;
                }
                else if (ev.Name == "_MILI_DIS" && buf_mili_dis == "")
                {
                    buf_mili_dis = ev.StringValue;
                }
                else if (ev.Name == "_MILI_RANK" && buf_mili_rank == "")
                {
                    buf_mili_rank = ev.StringValue;
                }
                else if (ev.Name == "TITL" && buf_title == "")
                {
                    buf_title = ev.StringValue;
                }
            }
        }

        public override void UpdateItem(GKListItem item, bool isMain)
        {
            base.UpdateItem(item, isMain);

            GlobalOptions gOptions = GlobalOptions.Instance;

            if ((fRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnparentedPersons))
            {
                item.BackColor = HighlightUnparentedColor;
            }
            else if ((fRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnmarriedPersons))
            {
                item.BackColor = HighlightUnmarriedColor;
            }
        }

        #if GK_LINUX
        private static readonly Color HighlightUnparentedColor = Color.FromArgb(unchecked((int)0xFFFFCACA));
        private static readonly Color HighlightUnmarriedColor = Color.FromArgb(unchecked((int)0xFFFFFFA1));
        #else
        private static readonly Color HighlightUnparentedColor = Color.FromArgb(0xFFCACA);
        private static readonly Color HighlightUnmarriedColor = Color.FromArgb(0xFFFFA1);
        #endif

        public override void UpdateColumns(GKListView listView, bool isMain)
        {
            IndividualListColumns columns = GlobalOptions.Instance.IndividualListColumns;
            NameFormat defNameFormat = GlobalOptions.Instance.DefNameFormat;

            this.ColumnsMap_Clear();
            this.AddListColumn(listView, "№", 50, false, 0, 0);

            int num = columns.Count;
            for (int i = 0; i < num; i++)
            {
                if (columns[i].ColActive) {
                    byte bColType = columns[i].ColType;
                    PersonColumnType colType = (PersonColumnType)bColType;
                    int colWidth = columns[i].ColWidth;

                    if (colType == PersonColumnType.pctName) {
                        const bool asz = false;

                        switch (defNameFormat) {
                            case NameFormat.nfF_N_P:
                                this.AddListColumn(listView, LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
                                this.AddListColumn(listView, LangMan.LS(LSID.LSID_Name), 100, asz, bColType, 1);
                                this.AddListColumn(listView, LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 2);
                                break;

                            case NameFormat.nfF_NP:
                                this.AddListColumn(listView, LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
                                this.AddListColumn(listView, LangMan.LS(LSID.LSID_Name) + "," + LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 1);
                                break;

                            case NameFormat.nfFNP:
                                this.AddListColumn(listView, LangMan.LS(LSID.LSID_FullName), colWidth, asz, bColType, 0);
                                break;
                        }
                    } else {
                        string colName = LangMan.LS(columns.ColumnStatics[bColType].ColName);
                        this.AddListColumn(listView, colName, colWidth, false, bColType, 0);
                    }
                }
            }
        }

        public override Type GetColumnsEnum()
        {
            return typeof(PersonColumnType);
        }

        protected override ListColumns GetDefaultListColumns()
        {
            return GlobalOptions.Instance.IndividualListColumns;
        }

        public IndividualListMan(GEDCOMTree tree) : base(tree)
        {
        }
    }
}
