/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Text;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Search;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class IndividualListFilter : ListFilter, IIndividualListFilter
    {
        public string AliveBeforeDate;
        public FilterGroupMode FilterGroupMode;
        public string GroupRef;
        public string Name;
        public bool PatriarchOnly;
        public string Residence;
        public GDMSex Sex;
        public FilterGroupMode SourceMode;
        public string SourceRef;
        public string EventVal;

        public FilterLifeMode FilterLifeMode { get; set; }

        public IndividualListFilter()
        {
            Clear();
        }

        public override void Clear()
        {
            base.Clear();

            FilterGroupMode = FilterGroupMode.All;
            GroupRef = "";
            if (FilterLifeMode != FilterLifeMode.lmTimeLocked) {
                FilterLifeMode = FilterLifeMode.lmAll;
            }
            Name = "*";
            AliveBeforeDate = "";
            PatriarchOnly = false;
            Residence = "*";
            Sex = GDMSex.svUnknown;
            SourceMode = FilterGroupMode.All;
            SourceRef = "";
            EventVal = "*";
        }

        public override string ToString(IListSource listSource)
        {
            if (listSource == null)
                return string.Empty;

            var sb = new StringBuilder();

            if (Sex != GDMSex.svUnknown)
                AddCSChunk(sb, string.Format("{0} = `{1}`", LangMan.LS(LSID.LSID_Sex), GKUtils.SexStr(Sex)));

            if (Name != "*")
                AddCSChunk(sb, string.Format("{0} = `{1}`", LangMan.LS(LSID.LSID_NameMask), Name));

            if (Residence != "*")
                AddCSChunk(sb, string.Format("{0} = `{1}`", LangMan.LS(LSID.LSID_PlaceMask), Residence));

            if (EventVal != "*")
                AddCSChunk(sb, string.Format("{0} = `{1}`", LangMan.LS(LSID.LSID_EventMask), EventVal));

            if (PatriarchOnly)
                AddCSChunk(sb, string.Format("{0}", LangMan.LS(LSID.LSID_OnlyPatriarchs)));

            if (FilterGroupMode != FilterGroupMode.All)
                AddCSChunk(sb, string.Format("{0} = `{1}`", LangMan.LS(LSID.LSID_RPGroups), GroupRef));

            if (SourceMode != FilterGroupMode.All)
                AddCSChunk(sb, string.Format("{0} = `{1}`", LangMan.LS(LSID.LSID_RPSources), SourceRef));

            AddCSChunk(sb, base.ToString(listSource));

            return sb.ToString();
        }

        private static void AddCSChunk(StringBuilder sb, string chunk)
        {
            if (string.IsNullOrEmpty(chunk))
                return;

            if (sb.Length != 0)
                sb.Append(", ");

            sb.Append(chunk);
        }

        public override void Assign(IListFilter other)
        {
            var otherFilter = other as IndividualListFilter;
            if (otherFilter == null)
                throw new ArgumentNullException("other");

            base.Assign(otherFilter);

            AliveBeforeDate = otherFilter.AliveBeforeDate;
            FilterGroupMode = otherFilter.FilterGroupMode;
            GroupRef = otherFilter.GroupRef;
            Name = otherFilter.Name;
            PatriarchOnly = otherFilter.PatriarchOnly;
            Residence = otherFilter.Residence;
            Sex = otherFilter.Sex;
            SourceMode = otherFilter.SourceMode;
            SourceRef = otherFilter.SourceRef;
            EventVal = otherFilter.EventVal;
        }

        public override void Deserialize(string value)
        {
            var instance = JsonHelper.Deserialize<IndividualListFilter>(value);
            Assign(instance);
        }
    }


    public sealed class IndividualListModel : RecordsListModel<GDMIndividualRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctPatriarch,
            ctName,
            ctNick,
            ctSex,
            ctBirthDate,
            ctDeathDate,
            ctBirthPlace,
            ctDeathPlace,
            ctResidence,
            ctAge,
            ctLifeExpectancy,
            ctDaysForBirth,
            ctGroups,
            ctReligion,
            ctNationality,
            ctEducation,
            ctOccupation,
            ctCaste,
            ctMili,
            ctMiliInd,
            ctMiliDis,
            ctMiliRank,
            ctChangeDate,
            ctBookmark,
            ctTitle
        }


        private GDMGroupRecord filter_grp;
        private UDN filter_abd;
        private GDMSourceRecord filter_source;


        public IndividualListModel(IBaseContext baseContext) :
            base(baseContext, CreateIndividualListColumns(), GDMRecordType.rtIndividual)
        {
        }

        public static ListColumns<GDMIndividualRecord> CreateIndividualListColumns()
        {
            var result = new ListColumns<GDMIndividualRecord>();

            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.LSID_Patriarch, DataType.dtString, 25, true);
            result.AddColumn(LSID.LSID_FullName, DataType.dtString, 300, true);
            result.AddColumn(LSID.LSID_Nickname, DataType.dtString, 75, false);
            result.AddColumn(LSID.LSID_Sex, DataType.dtString, 45, true);
            result.AddColumn(LSID.LSID_BirthDate, DataType.dtGEDCOMDate, 100, true);
            result.AddColumn(LSID.LSID_DeathDate, DataType.dtGEDCOMDate, 100, true);
            result.AddColumn(LSID.LSID_BirthPlace, DataType.dtString, 100, true);
            result.AddColumn(LSID.LSID_DeathPlace, DataType.dtString, 100, true);
            result.AddColumn(LSID.LSID_Residence, DataType.dtString, 100, true);
            result.AddColumn(LSID.LSID_Age, DataType.dtInteger, 100, false);
            result.AddColumn(LSID.LSID_LifeExpectancy, DataType.dtInteger, 100, false);
            result.AddColumn(LSID.LSID_DaysForBirth, DataType.dtInteger, 100, false);
            result.AddColumn(LSID.LSID_RPGroups, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Religion, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Nationality, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Education, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Occupation, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Caste, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Mili, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_MiliInd, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_MiliDis, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_MiliRank, DataType.dtString, 200, false);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
            result.AddColumn(LSID.LSID_Bookmark, DataType.dtString, 25, true);
            result.AddColumn(LSID.LSID_NobilityTitle, DataType.dtString, 200, false);

            result.ResetDefaults();
            return result;
        }

        public override IList<ISearchResult> FindAll(string searchPattern)
        {
            var result = new List<ISearchResult>();
            var allNames = GlobalOptions.Instance.SearchAndFilterByAllNames;
            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = ContentList.Count;
            for (int i = 0; i < num; i++) {
                var iRec = (GDMIndividualRecord)ContentList[i].Record;
                if (GKUtils.IsMatchesNames(iRec, regex, allNames)) {
                    result.Add(new SearchResult(iRec));
                }
            }

            return result;
        }

        protected override void CreateFilter()
        {
            fFilter = new IndividualListFilter();
        }

        private bool IsMatchesPlace(string fltResidence)
        {
            if (fltResidence == "*") return true;

            bool result = false;

            if (fFetchedRec.HasEvents) {
                bool includeAddr = GlobalOptions.Instance.PlacesWithAddress;
                var events = fFetchedRec.Events;
                for (int i = 0, num = events.Count; i < num; i++) {
                    string place = GKUtils.GetPlaceStr(events[i], includeAddr);
                    result = IsMatchesMask(place, fltResidence);
                    if (result) break;
                }
            }

            return result;
        }

        private bool IsMatchesEventVal(string fltEventVal)
        {
            if (fltEventVal == "*") return true;

            bool result = false;

            if (fFetchedRec.HasEvents) {
                var events = fFetchedRec.Events;
                for (int i = 0, num = events.Count; i < num; i++) {
                    result = IsMatchesMask(events[i].StringValue, fltEventVal);
                    if (result) break;
                }
            }

            return result;
        }

        private bool IsMatchesNames(string fltName)
        {
            var allNames = GlobalOptions.Instance.SearchAndFilterByAllNames;

            if (!allNames) {
                return IsMatchesMask(buf_fullname, fltName);
            } else {
                for (int k = 0; k < fFetchedRec.PersonalNames.Count; k++) {
                    var persName = fFetchedRec.PersonalNames[k];

                    string recName = GKUtils.GetNameString(fFetchedRec, persName, true, false);
                    if (IsMatchesMask(recName, fltName)) {
                        return true;
                    }
                }
                return false;
            }
        }

        private bool CheckSpecificFilter()
        {
            bool result = false;

            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            if ((iFilter.Sex == GDMSex.svUnknown || fFetchedRec.Sex == iFilter.Sex)
                && (IsMatchesNames(iFilter.Name))
                && (IsMatchesPlace(iFilter.Residence))
                && (IsMatchesEventVal(iFilter.EventVal))
                && (!iFilter.PatriarchOnly || fFetchedRec.Patriarch))
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
                        UDN bdt = (buf_bd == null) ? UDN.CreateUnknown() : buf_bd.Date.GetUDN();
                        UDN ddt = (buf_dd == null) ? UDN.CreateUnknown() : buf_dd.Date.GetUDN();
                        if ((bdt.CompareTo(filter_abd) > 0) || (ddt.CompareTo(filter_abd) < 0)) return false;
                        break;

                    case FilterLifeMode.lmTimeLocked:
                        break;
                }

                switch (iFilter.FilterGroupMode) {
                    case FilterGroupMode.All:
                        break;

                    case FilterGroupMode.None:
                        if (fFetchedRec.HasGroups) return false;
                        break;

                    case FilterGroupMode.Any:
                        if (!fFetchedRec.HasGroups) return false;
                        break;

                    case FilterGroupMode.Selected:
                        if (fFetchedRec.IndexOfGroup(filter_grp) < 0) return false;
                        break;
                }

                switch (iFilter.SourceMode) {
                    case FilterGroupMode.All:
                        break;

                    case FilterGroupMode.None:
                        if (fFetchedRec.HasSourceCitations) return false;
                        break;

                    case FilterGroupMode.Any:
                        if (!fFetchedRec.HasSourceCitations) return false;
                        break;

                    case FilterGroupMode.Selected:
                        if (fFetchedRec.IndexOfSource(filter_source) < 0) return false;
                        break;
                }

                result = true;
            }

            return result;
        }

        public override bool CheckFilter()
        {
            bool res = (fBaseContext.IsRecordAccess(fFetchedRec.Restriction) && IsMatchesMask(buf_fullname, QuickFilter));

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec) && CheckSpecificFilter();

            return res;
        }

        private object GetNameValueEx(int colSubtype)
        {
            object result = null;

            if (colSubtype == -1) {
                result = GKUtils.GetNameString(fFetchedRec, true, false);
            } else {
                NameFormat defNameFormat = (SimpleList) ? NameFormat.nfFNP : GlobalOptions.Instance.DefNameFormat;
                NamePartsRet parts;
                GDMLanguageID defLang = fBaseContext.DefaultLanguage;

                switch (defNameFormat) {
                    case NameFormat.nfFNP:
                        result = GKUtils.GetNameString(fFetchedRec, GlobalOptions.Instance.SurnameFirstInOrder, false, defLang);
                        break;

                    case NameFormat.nfF_NP:
                        parts = GKUtils.GetNameParts(fBaseContext.Tree, fFetchedRec, true, defLang);
                        switch (colSubtype) {
                            case 0:
                                result = parts.Surname;
                                break;
                            case 1:
                                result = parts.Name + " " + parts.Patronymic;
                                break;
                        }
                        break;

                    case NameFormat.nfF_N_P:
                        parts = GKUtils.GetNameParts(fBaseContext.Tree, fFetchedRec, true, defLang);
                        switch (colSubtype) {
                            case 0:
                                result = parts.Surname;
                                break;
                            case 1:
                                result = parts.Name;
                                break;
                            case 2:
                                result = parts.Patronymic;
                                break;
                        }
                        break;
                }
            }

            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;

            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctPatriarch:
                    result = ((fFetchedRec.Patriarch) ? "*" : " ");
                    break;

                case ColumnType.ctName:
                    result = GetNameValueEx(colSubtype);
                    break;

                case ColumnType.ctNick:
                    result = GKUtils.GetNickString(fFetchedRec);
                    break;

                case ColumnType.ctSex:
                    result = GKUtils.SexChar(fFetchedRec.Sex);
                    break;

                case ColumnType.ctBirthDate:
                    result = GetDateValue(buf_bd, isVisible);
                    break;

                case ColumnType.ctBirthPlace:
                    result = GKUtils.GetPlaceStr(buf_bd, false);
                    break;

                case ColumnType.ctDeathDate:
                    result = GetDateValue(buf_dd, isVisible);
                    break;

                case ColumnType.ctDeathPlace:
                    result = GKUtils.GetPlaceStr(buf_dd, false);
                    break;

                case ColumnType.ctResidence:
                    result = buf_residence;
                    break;

                case ColumnType.ctAge:
                    result = (isVisible) ? (object)GKUtils.GetAgeStr(fFetchedRec, -1) : GKUtils.GetAge(fFetchedRec, -1);
                    break;

                case ColumnType.ctLifeExpectancy:
                    result = (isVisible) ? (object)GKUtils.GetLifeExpectancyStr(fFetchedRec) : GKUtils.GetLifeExpectancy(fFetchedRec);
                    break;

                case ColumnType.ctDaysForBirth:
                    int days = GKUtils.GetDaysForBirth(fFetchedRec);
                    result = (days >= 0) ? (object)days : null;
                    break;

                case ColumnType.ctGroups:
                    result = GKUtils.GetGroupsStr(fBaseContext.Tree, fFetchedRec);
                    break;

                case ColumnType.ctReligion:
                    result = buf_religion;
                    break;

                case ColumnType.ctNationality:
                    result = buf_nationality;
                    break;

                case ColumnType.ctEducation:
                    result = buf_education;
                    break;

                case ColumnType.ctOccupation:
                    result = buf_occupation;
                    break;

                case ColumnType.ctCaste:
                    result = buf_caste;
                    break;

                case ColumnType.ctMili:
                    result = buf_mili;
                    break;

                case ColumnType.ctMiliInd:
                    result = buf_mili_ind;
                    break;

                case ColumnType.ctMiliDis:
                    result = buf_mili_dis;
                    break;

                case ColumnType.ctMiliRank:
                    result = buf_mili_rank;
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;

                case ColumnType.ctBookmark:
                    result = ((fFetchedRec.Bookmark) ? "*" : " ");
                    break;

                case ColumnType.ctTitle:
                    result = buf_title;
                    break;
            }

            return result;
        }

        public override void PrepareFilter()
        {
            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            filter_abd = GDMDate.GetUDNByFormattedStr(iFilter.AliveBeforeDate, GDMCalendar.dcGregorian);

            if (iFilter.GroupRef == "") {
                filter_grp = null;
            } else {
                filter_grp = fBaseContext.Tree.XRefIndex_Find(iFilter.GroupRef) as GDMGroupRecord;
            }

            if (iFilter.SourceRef == "") {
                filter_source = null;
            } else {
                filter_source = fBaseContext.Tree.XRefIndex_Find(iFilter.SourceRef) as GDMSourceRecord;
            }
        }

        private GDMCustomEvent buf_bd;
        private GDMCustomEvent buf_dd;

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

        public override void Fetch(GDMIndividualRecord aRec)
        {
            base.Fetch(aRec);

            buf_fullname = GKUtils.GetNameString(fFetchedRec, true, false);
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

            int num = fFetchedRec.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent ev = fFetchedRec.Events[i];
                GEDCOMTagType evtType = ev.GetTagType();

                if (evtType == GEDCOMTagType.BIRT && buf_bd == null) {
                    buf_bd = ev;
                } else if (evtType == GEDCOMTagType.DEAT && buf_dd == null) {
                    buf_dd = ev;
                } else if (evtType == GEDCOMTagType.RESI && buf_residence == "") {
                    buf_residence = GKUtils.GetPlaceStr(ev, gOptions.PlacesWithAddress);
                } else if (evtType == GEDCOMTagType.RELI && buf_religion == "") {
                    buf_religion = ev.StringValue;
                } else if (evtType == GEDCOMTagType.NATI && buf_nationality == "") {
                    buf_nationality = ev.StringValue;
                } else if (evtType == GEDCOMTagType.EDUC && buf_education == "") {
                    buf_education = ev.StringValue;
                } else if (evtType == GEDCOMTagType.OCCU && buf_occupation == "") {
                    buf_occupation = ev.StringValue;
                } else if (evtType == GEDCOMTagType.CAST && buf_caste == "") {
                    buf_caste = ev.StringValue;
                } else if (evtType == GEDCOMTagType._MILI && buf_mili == "") {
                    buf_mili = ev.StringValue;
                } else if (evtType == GEDCOMTagType._MILI_IND && buf_mili_ind == "") {
                    buf_mili_ind = ev.StringValue;
                } else if (evtType == GEDCOMTagType._MILI_DIS && buf_mili_dis == "") {
                    buf_mili_dis = ev.StringValue;
                } else if (evtType == GEDCOMTagType._MILI_RANK && buf_mili_rank == "") {
                    buf_mili_rank = ev.StringValue;
                } else if (evtType == GEDCOMTagType.TITL && buf_title == "") {
                    buf_title = ev.StringValue;
                }
            }
        }

        public override IColor GetBackgroundColor(int itemIndex, object rowData)
        {
            var indiRec = (GDMIndividualRecord)rowData;

            GlobalOptions gOptions = GlobalOptions.Instance;
            if (gOptions.ListHighlightUnparentedPersons && (indiRec.ChildToFamilyLinks.Count == 0)) {
                return ChartRenderer.GetColor(GKData.HighlightUnparentedColor);
            } else if (gOptions.ListHighlightUnmarriedPersons && (indiRec.SpouseToFamilyLinks.Count == 0)) {
                return ChartRenderer.GetColor(GKData.HighlightUnmarriedColor);
            } else {
                return base.GetBackgroundColor(itemIndex, rowData);
            }
        }

        protected override void UpdateColumnsMap()
        {
            fColumnsMap.Clear();

            NameFormat defNameFormat = (SimpleList) ? NameFormat.nfFNP : GlobalOptions.Instance.DefNameFormat;
            IListColumns columns = this.ListColumns;

            int num = columns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn columnProps = columns.OrderedColumns[i];
                if (!columnProps.CurActive) continue;

                const bool asz = false;
                byte bColType = columnProps.Id;
                if (bColType == (byte)ColumnType.ctName && defNameFormat != NameFormat.nfFNP) {
                    switch (defNameFormat) {
                        case NameFormat.nfF_N_P:
                            AddColumn(LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
                            AddColumn(LangMan.LS(LSID.LSID_Name), 100, asz, bColType, 1);
                            AddColumn(LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 2);
                            break;

                        case NameFormat.nfF_NP:
                            AddColumn(LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
                            AddColumn(LangMan.LS(LSID.LSID_Name) + "," + LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 1);
                            break;
                    }
                } else {
                    AddColumn(LangMan.LS(columnProps.ColName), columnProps.CurWidth, false, bColType, 0);
                }
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class IndiGroupsListModel : SheetModel<GDMPointer>
    {
        private GDMGroupRecord fGroupRec;

        public IndiGroupsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Group, 350, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMPointer aRec)
        {
            base.Fetch(aRec);
            fGroupRec = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fGroupRec.GroupName;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (iRec == null) return;

            try {
                UpdateStructList(iRec.Groups);
            } catch (Exception ex) {
                Logger.WriteError("IndiGroupsListModel.UpdateContent()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMGroupRecord groupRec = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    groupRec = fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtGroup, null) as GDMGroupRecord;
                    if (groupRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, groupRec, iRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachGroupQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberDetach, groupRec, iRec);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class IndiNamesListModel : SheetModel<GDMPersonalName>
    {
        public IndiNamesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveDown, RecordAction.raMoveUp);

            fListColumns.AddColumn(LSID.LSID_NumberSym, 25, false);
            fListColumns.AddColumn(LSID.LSID_Name, 350, false);
            fListColumns.AddColumn(LSID.LSID_Type, 100, false);
            fListColumns.AddColumn(LSID.LSID_Language, 150, false);
            fListColumns.ResetDefaults();
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = GKUtils.GetNameString((GDMIndividualRecord)fDataOwner, fFetchedRec, true, false);
                    break;
                case 2:
                    result = LangMan.LS(GKData.NameTypes[(int)fFetchedRec.NameType]);
                    break;
                case 3:
                    result = GEDCOMUtils.GetLanguageStr(fFetchedRec.Language);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (iRec == null) return;

            try {
                UpdateStructList(iRec.PersonalNames);
            } catch (Exception ex) {
                Logger.WriteError("IndiNamesListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMPersonalName persName = eArgs.ItemData as GDMPersonalName;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (var dlg = AppHost.ResolveDialog<IPersonalNameEditDlg>(fBaseWin)) {
                        bool exists = (persName != null);
                        if (!exists) {
                            persName = new GDMPersonalName();
                        }

                        dlg.IndividualRecord = iRec;
                        dlg.PersonalName = persName;
                        result = AppHost.Instance.ShowModalX(dlg, fOwner, false);

                        if (!exists) {
                            if (result) {
                                result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualNameAdd, iRec, persName);
                            } else {
                                persName.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (iRec.PersonalNames.Count > 1) {
                        result = (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveNameQuery)));
                        if (result) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualNameRemove, iRec, persName);
                        }
                    } else {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_RemoveNameFailed));
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    int idx = iRec.PersonalNames.IndexOf(persName);
                    switch (eArgs.Action) {
                        case RecordAction.raMoveUp:
                            iRec.PersonalNames.Exchange(idx - 1, idx);
                            break;

                        case RecordAction.raMoveDown:
                            iRec.PersonalNames.Exchange(idx, idx + 1);
                            break;
                    }
                    result = true;
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class IndiParentsListModel : SheetModel<GDMChildToFamilyLink>
    {
        private GDMFamilyRecord fFamRec;

        public IndiParentsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveDown, RecordAction.raMoveUp);

            fListColumns.AddColumn(LSID.LSID_NumberSym, 25, false);
            fListColumns.AddColumn(LSID.LSID_Name, 350, false);
            fListColumns.AddColumn(LSID.LSID_Type, 100, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMChildToFamilyLink aRec)
        {
            base.Fetch(aRec);
            fFamRec = fBaseContext.Tree.GetPtrValue(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = GKUtils.GetFamilyString(fBaseContext.Tree, fFamRec);
                    break;
                case 2:
                    result = LangMan.LS(GKData.ParentTypes[(int)fFetchedRec.PedigreeLinkageType]);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (iRec == null) return;

            try {
                UpdateStructList(iRec.ChildToFamilyLinks);
            } catch (Exception ex) {
                Logger.WriteError("IndiParentsListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMChildToFamilyLink cfLink = eArgs.ItemData as GDMChildToFamilyLink;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GDMFamilyRecord family = fBaseWin.Context.SelectFamily(fOwner, iRec);
                    if (family != null && family.IndexOfChild(iRec) < 0) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, iRec, family);
                    }
                    break;

                case RecordAction.raEdit:
                    if (cfLink != null) {
                        using (var dlg = AppHost.ResolveDialog<IParentsEditDlg>(fBaseWin)) {
                            dlg.IndividualRecord = iRec;
                            dlg.ChildLink = cfLink;
                            result = AppHost.Instance.ShowModalX(dlg, fOwner, false);
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachParentsQuery))) {
                        var famRec = fBaseContext.Tree.GetPtrValue(cfLink);
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, iRec, famRec);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    int idx = iRec.ChildToFamilyLinks.IndexOf(cfLink);
                    switch (eArgs.Action) {
                        case RecordAction.raMoveUp:
                            iRec.ChildToFamilyLinks.Exchange(idx - 1, idx);
                            break;

                        case RecordAction.raMoveDown:
                            iRec.ChildToFamilyLinks.Exchange(idx, idx + 1);
                            break;
                    }
                    result = true;
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class IndiSpousesListModel : SheetModel<GDMSpouseToFamilyLink>
    {
        private GDMFamilyRecord fFamilyRec;
        private string fRelName;

        public IndiSpousesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump, RecordAction.raMoveUp, RecordAction.raMoveDown);

            fListColumns.AddColumn(LSID.LSID_NumberSym, 25, false);
            fListColumns.AddColumn(LSID.LSID_Spouse, 300, false);
            fListColumns.AddColumn(LSID.LSID_MarriageDate, 100, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMSpouseToFamilyLink aRec)
        {
            base.Fetch(aRec);

            var iRec = (GDMIndividualRecord)fDataOwner;

            fFamilyRec = fBaseContext.Tree.GetPtrValue(fFetchedRec);
            if (fFamilyRec == null) {
                fRelName = string.Empty;
                return;
            }

            GDMIndividualRecord relPerson;

            if (iRec.Sex == GDMSex.svMale) {
                relPerson = fBaseContext.Tree.GetPtrValue(fFamilyRec.Wife);
                fRelName = LangMan.LS(LSID.LSID_UnkFemale);
            } else {
                relPerson = fBaseContext.Tree.GetPtrValue(fFamilyRec.Husband);
                fRelName = LangMan.LS(LSID.LSID_UnkMale);
            }

            if (relPerson != null) {
                fRelName = GKUtils.GetNameString(relPerson, true, false);
            }
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = fRelName;
                    break;
                case 2:
                    result = new GDMDateItem(GKUtils.GetMarriageDate(fFamilyRec));
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (iRec == null) return;

            try {
                UpdateStructList(iRec.SpouseToFamilyLinks);
            } catch (Exception ex) {
                Logger.WriteError("IndiSpousesListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            var family = fBaseContext.Tree.GetPtrValue<GDMFamilyRecord>(eArgs.ItemData as GDMSpouseToFamilyLink);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    result = (BaseController.ModifyFamily(fOwner, fBaseWin, ref family, TargetMode.tmSpouse, iRec));
                    if (result) {
                        eArgs.ItemData = family;
                    }
                    break;

                case RecordAction.raEdit:
                    result = (BaseController.ModifyFamily(fOwner, fBaseWin, ref family, TargetMode.tmNone, null));
                    break;

                case RecordAction.raDelete:
                    if (family != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachSpouseQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, iRec);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = iRec.IndexOfSpouse(family);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                iRec.ExchangeSpouses(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                iRec.ExchangeSpouses(idx, idx + 1);
                                break;
                        }

                        result = true;
                        break;
                    }
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class URefsListModel : SheetModel<GDMUserReference>
    {
        public URefsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete);

            fListColumns.AddColumn(LSID.LSID_Reference, 300, false);
            fListColumns.AddColumn(LSID.LSID_Type, 200, false);
            fListColumns.ResetDefaults();
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fFetchedRec.StringValue;
                    break;
                case 1:
                    result = fFetchedRec.ReferenceType;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (iRec == null) return;

            try {
                UpdateStructList(iRec.UserReferences);
            } catch (Exception ex) {
                Logger.WriteError("URefsListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMUserReference userRef = eArgs.ItemData as GDMUserReference;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (var dlg = AppHost.ResolveDialog<IUserRefEditDlg>(fBaseWin)) {
                        bool exists = (userRef != null);
                        if (!exists) {
                            userRef = new GDMUserReference();
                        }

                        dlg.UserReference = userRef;
                        result = AppHost.Instance.ShowModalX(dlg, fOwner, false);

                        if (!exists) {
                            if (result) {
                                result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualURefAdd, iRec, userRef);
                            } else {
                                userRef.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    {
                        string confirmation = !string.IsNullOrEmpty(userRef.StringValue) ? userRef.StringValue : userRef.ReferenceType;
                        if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveUserRefQuery, confirmation))) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualURefRemove, iRec, userRef);
                            fBaseWin.Context.Modified = true;
                        }
                        break;
                    }
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
