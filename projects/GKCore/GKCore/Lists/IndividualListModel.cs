/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Filters;
using GKCore.Locales;
using GKCore.Names;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Search;

namespace GKCore.Lists
{
    public enum FilterGroupMode
    {
        All,
        None,
        Any,
        Selected
    }


    public enum FilterLifeMode
    {
        lmAll,
        lmOnlyAlive,
        lmOnlyDead,
        lmAliveBefore,
        lmTimeLocked
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class IndividualListFilter : ListFilter
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
        public int TimeLineYear;

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
            TimeLineYear = -1;
        }

        public override void Assign(ListFilter other)
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
            TimeLineYear = otherFilter.TimeLineYear;
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
        private GlobalOptions fOptions;


        public IndividualListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtIndividual)
        {
            fOptions = GlobalOptions.Instance;
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtIndividual);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Patriarch, DataType.dtString, 25, true);
            result.AddColumn(LSID.FullName, DataType.dtString, 300, true);
            result.AddColumn(LSID.Nickname, DataType.dtString, 75, false);
            result.AddColumn(LSID.Sex, DataType.dtString, 45, true);
            result.AddColumn(LSID.BirthDate, DataType.dtGEDCOMDate, 100, true);
            result.AddColumn(LSID.DeathDate, DataType.dtGEDCOMDate, 100, true);
            result.AddColumn(LSID.BirthPlace, DataType.dtString, 100, true);
            result.AddColumn(LSID.DeathPlace, DataType.dtString, 100, true);
            result.AddColumn(LSID.Residence, DataType.dtString, 100, true);
            result.AddColumn(LSID.Age, DataType.dtInteger, 100, false);
            result.AddColumn(LSID.LifeExpectancy, DataType.dtInteger, 100, false);
            result.AddColumn(LSID.DaysForBirth, DataType.dtInteger, 100, false);
            result.AddColumn(LSID.RPGroups, DataType.dtString, 200, false);
            result.AddColumn(LSID.Religion, DataType.dtString, 200, false);
            result.AddColumn(LSID.Nationality, DataType.dtString, 200, false);
            result.AddColumn(LSID.Education, DataType.dtString, 200, false);
            result.AddColumn(LSID.Occupation, DataType.dtString, 200, false);
            result.AddColumn(LSID.Caste, DataType.dtString, 200, false);
            result.AddColumn(LSID.Mili, DataType.dtString, 200, false);
            result.AddColumn(LSID.MiliInd, DataType.dtString, 200, false);
            result.AddColumn(LSID.MiliDis, DataType.dtString, 200, false);
            result.AddColumn(LSID.MiliRank, DataType.dtString, 200, false);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            result.AddColumn(LSID.Bookmark, DataType.dtString, 25, true);
            result.AddColumn(LSID.NobilityTitle, DataType.dtString, 200, false);
            return result;
        }

        private bool IsMatchesNames(GDMIndividualRecord iRec, string searchPattern, bool allNames)
        {
            if (!allNames) {
                string recName = GKUtils.GetNameString(iRec, true, false);
                if (IsMatchesMask(recName, searchPattern)) {
                    return true;
                }
            } else {
                for (int k = 0; k < iRec.PersonalNames.Count; k++) {
                    var persName = iRec.PersonalNames[k];

                    string recName = GKUtils.GetNameString(iRec, persName, true, false);
                    if (IsMatchesMask(recName, searchPattern)) {
                        return true;
                    }
                }
            }
            return false;
        }

        public override IList<ISearchResult> FindAll(string searchPattern)
        {
            var result = new List<ISearchResult>();

            if (string.IsNullOrEmpty(searchPattern))
                return result;

            searchPattern = GKUtils.PrepareQSF(searchPattern);

            var allNames = fOptions.SearchAndFilterByAllNames;

            for (int i = 0, num = ContentList.Count; i < num; i++) {
                var iRec = (GDMIndividualRecord)ContentList[i].Record;
                if (IsMatchesNames(iRec, searchPattern, allNames)) {
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
            bool result = false;

            if (fFetchedRec.HasEvents) {
                bool includeAddr = fOptions.PlacesWithAddress;
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
            var allNames = fOptions.SearchAndFilterByAllNames;

            if (!allNames) {
                return IsMatchesMask(fQuickFilterBuffer, fltName);
            } else {
                var persNames = fFetchedRec.PersonalNames;
                for (int k = 0, num = persNames.Count; k < num; k++) {
                    string recName = GKUtils.GetNameString(fFetchedRec, persNames[k], true, false);
                    if (IsMatchesMask(recName, fltName)) {
                        return true;
                    }
                }
                return false;
            }
        }

        public override bool CheckFilter()
        {
            bool res = base.CheckFilter();
            if (!res) return false;

            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            if (iFilter.Sex != GDMSex.svUnknown && fFetchedRec.Sex != iFilter.Sex) return false;
            if (iFilter.Name != "*" && !IsMatchesNames(iFilter.Name)) return false;
            if (iFilter.Residence != "*" && !IsMatchesPlace(iFilter.Residence)) return false;
            if (iFilter.EventVal != "*" && !IsMatchesEventVal(iFilter.EventVal)) return false;
            if (iFilter.PatriarchOnly && !fFetchedRec.Patriarch) return false;

            switch (iFilter.FilterLifeMode) {
                case FilterLifeMode.lmOnlyAlive:
                    if (buf_dd != null) return false;
                    break;

                case FilterLifeMode.lmOnlyDead:
                    if (buf_dd == null) return false;
                    break;

                case FilterLifeMode.lmAliveBefore:
                    UDN bdt = (buf_bd == null) ? UDN.Unknown : buf_bd.Date.GetUDN();
                    UDN ddt = (buf_dd == null) ? UDN.Unknown : buf_dd.Date.GetUDN();
                    if ((bdt.CompareTo(filter_abd) > 0) || (ddt.CompareTo(filter_abd) < 0)) return false;
                    break;

                case FilterLifeMode.lmAll:
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

            return true;
        }

        public override void PrepareFilter()
        {
            base.PrepareFilter();

            IndividualListFilter iFilter = (IndividualListFilter)fFilter;
            filter_abd = GDMDate.GetUDNByFormattedStr(iFilter.AliveBeforeDate, GDMCalendar.dcGregorian);
            filter_grp = (iFilter.GroupRef == "") ? null : fBaseContext.Tree.FindXRef<GDMGroupRecord>(iFilter.GroupRef);
            filter_source = (iFilter.SourceRef == "") ? null : fBaseContext.Tree.FindXRef<GDMSourceRecord>(iFilter.SourceRef);
        }

        private object GetNameValueEx(int colSubtype)
        {
            object result = null;

            if (colSubtype == -1) {
                result = GKUtils.GetNameString(fFetchedRec, false);
            } else {
                NameFormat defNameFormat = (SimpleList) ? NameFormat.nfFNP : fOptions.DefNameFormat;
                NamePartsRet parts;
                GDMLanguageID defLang = fBaseContext.DefaultLanguage;

                switch (defNameFormat) {
                    case NameFormat.nfFNP:
                        result = GKUtils.GetNameString(fFetchedRec, fOptions.SurnameFirstInOrder, false, defLang);
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
                    result = fFetchedRec.Patriarch ? "*" : " ";
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

                case ColumnType.ctAge: {
                        int tlYear = ((IndividualListFilter)fFilter).TimeLineYear;
                        int val = GKUtils.GetAge(fFetchedRec, tlYear);
                        result = (val == -1) ? null : (object)val;
                    }
                    break;

                case ColumnType.ctLifeExpectancy: {
                        int val = GKUtils.GetLifeExpectancy(fFetchedRec);
                        result = (val == -1) ? null : (object)val;
                    }
                    break;

                case ColumnType.ctDaysForBirth:
                    int days = GKUtils.GetDaysForBirth(fFetchedRec, true, out _, out _);
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
                    result = fFetchedRec.Bookmark ? "*" : " ";
                    break;

                case ColumnType.ctTitle:
                    result = buf_title;
                    break;
            }

            return result;
        }

        private GDMCustomEvent buf_bd;
        private GDMCustomEvent buf_dd;
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
            fQuickFilterBuffer = GKUtils.GetNameString(fFetchedRec, true, false);

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
            if (!fFetchedRec.HasEvents) return;

            int num = fFetchedRec.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent ev = fFetchedRec.Events[i];
                GEDCOMTagType evtType = ev.GetTagType();

                if (evtType == GEDCOMTagType.BIRT && buf_bd == null) {
                    buf_bd = ev;
                } else if (evtType == GEDCOMTagType.DEAT && buf_dd == null) {
                    buf_dd = ev;
                } else if (evtType == GEDCOMTagType.RESI && buf_residence == "") {
                    buf_residence = GKUtils.GetPlaceStr(ev, fOptions.PlacesWithAddress);
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

            if (fOptions.ListHighlightUnparentedPersons && (indiRec.ChildToFamilyLinks.Count == 0)) {
                return ChartRenderer.GetColor(GKData.HighlightUnparentedColor);
            } else if (fOptions.ListHighlightUnmarriedPersons && (indiRec.SpouseToFamilyLinks.Count == 0)) {
                return ChartRenderer.GetColor(GKData.HighlightUnmarriedColor);
            } else {
                return base.GetBackgroundColor(itemIndex, rowData);
            }
        }

        protected override void UpdateColumnsMap()
        {
            fColumnsMap.Clear();

            NameFormat defNameFormat = (SimpleList) ? NameFormat.nfFNP : fOptions.DefNameFormat;

            int num = fListColumns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn columnProps = fListColumns.OrderedColumns[i];
                if (!columnProps.CurActive) continue;

                const bool asz = false;
                byte bColType = columnProps.Id;
                if (bColType == (byte)ColumnType.ctName && defNameFormat != NameFormat.nfFNP) {
                    switch (defNameFormat) {
                        case NameFormat.nfF_N_P:
                            AddColumn(LangMan.LS(LSID.Surname), 150, asz, bColType, 0);
                            AddColumn(LangMan.LS(LSID.GivenName), 100, asz, bColType, 1);
                            AddColumn(LangMan.LS(LSID.Patronymic), 150, asz, bColType, 2);
                            break;

                        case NameFormat.nfF_NP:
                            AddColumn(LangMan.LS(LSID.Surname), 150, asz, bColType, 0);
                            AddColumn(LangMan.LS(LSID.GivenName) + "," + LangMan.LS(LSID.Patronymic), 150, asz, bColType, 1);
                            break;
                    }
                } else {
                    AddColumn(columnProps.ColName, columnProps.CurWidth, false, bColType, 0, columnProps.DataType);
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

        public IndiGroupsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(RecordAction.raAdd, RecordAction.raDelete, RecordAction.raJump);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stIndividualGroups);
            result.AddColumn(LSID.Group, 350, false);
            return result;
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
            if (iRec != null)
                UpdateStructList(iRec.Groups);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMGroupRecord groupRec = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    groupRec = await BaseController.SelectRecord(fOwner, fBaseWin, GDMRecordType.rtGroup, null) as GDMGroupRecord;
                    if (groupRec != null) {
                        if (groupRec.IndexOfMember(iRec) >= 0) {
                            AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                            return;
                        }

                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, groupRec, iRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachGroupQuery))) {
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
        public IndiNamesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveDown, RecordAction.raMoveUp);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stIndividualNames);
            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.GeneralName, 350, false);
            result.AddColumn(LSID.Type, 100, false);
            result.AddColumn(LSID.Language, 150, false);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = GKUtils.GetNameString((GDMIndividualRecord)fDataOwner, fFetchedRec, GlobalOptions.Instance.SurnameFirstInOrder, false);
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
            if (iRec != null)
                UpdateStructList(iRec.PersonalNames);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMPersonalName persName = eArgs.ItemData as GDMPersonalName;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        bool exists = (persName != null);
                        if (!exists) {
                            persName = new GDMPersonalName();
                        }

                        using (var dlg = AppHost.ResolveDialog<IPersonalNameEditDlg>(fBaseWin)) {
                            dlg.IndividualRecord = iRec;
                            dlg.PersonalName = persName;
                            result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, false);
                        }

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
                        result = (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveNameQuery)));
                        if (result) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualNameRemove, iRec, persName);
                        }
                    } else {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.RemoveNameFailed));
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    result = Exchange(iRec.PersonalNames, persName, eArgs.Action);
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

        public IndiParentsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveDown, RecordAction.raMoveUp, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stIndividualParents);
            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.GeneralName, 350, false);
            result.AddColumn(LSID.Type, 100, false);
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            var parents = itemData as GDMChildToFamilyLink;
            return (parents == null) ? null : fBaseContext.Tree.GetPtrValue(parents);
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
            if (iRec != null)
                UpdateStructList(iRec.ChildToFamilyLinks);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMChildToFamilyLink cfLink = eArgs.ItemData as GDMChildToFamilyLink;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GDMFamilyRecord family = await BaseController.SelectFamily(fBaseWin, fOwner, iRec);
                    if (family != null) {
                        if (family.HasMember(iRec)) {
                            AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                            return;
                        }

                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, iRec, family);
                    }
                    break;

                case RecordAction.raEdit:
                    if (cfLink != null) {
                        using (var dlg = AppHost.ResolveDialog<IParentsEditDlg>(fBaseWin)) {
                            dlg.IndividualRecord = iRec;
                            dlg.ChildLink = cfLink;
                            result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, false);
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachParentsQuery))) {
                        var famRec = fBaseContext.Tree.GetPtrValue(cfLink);
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, iRec, famRec);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    result = Exchange(iRec.ChildToFamilyLinks, cfLink, eArgs.Action);
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

        public IndiSpousesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump, RecordAction.raMoveUp, RecordAction.raMoveDown, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stIndividualSpouses);
            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.Spouse, 300, false);
            result.AddColumn(LSID.MarriageDate, 100, false);
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            var famRec = itemData as GDMSpouseToFamilyLink;
            return (famRec == null) ? null : fBaseContext.Tree.GetPtrValue(famRec);
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
                fRelName = LangMan.LS(LSID.UnkFemale);
            } else {
                relPerson = fBaseContext.Tree.GetPtrValue(fFamilyRec.Husband);
                fRelName = LangMan.LS(LSID.UnkMale);
            }

            if (relPerson != null) {
                fRelName = GKUtils.GetNameString(relPerson, false);
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
            if (iRec != null)
                UpdateStructList(iRec.SpouseToFamilyLinks);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || iRec == null) return;

            var family = fBaseContext.Tree.GetPtrValue<GDMFamilyRecord>(eArgs.ItemData as GDMSpouseToFamilyLink);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd: {
                        var famRes = await BaseController.ModifyFamily(fOwner, fBaseWin, family, TargetMode.tmSpouse, iRec);
                        result = famRes.Result;
                        if (result) {
                            eArgs.ItemData = famRes.Record;
                        }
                    }
                    break;

                case RecordAction.raEdit: {
                        var famRes = await BaseController.ModifyFamily(fOwner, fBaseWin, family, TargetMode.tmNone, null);
                        result = famRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (family != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachSpouseQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, iRec);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown: {
                        int idx = iRec.IndexOfSpouse(family);

                        switch (eArgs.Action) {
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
}
