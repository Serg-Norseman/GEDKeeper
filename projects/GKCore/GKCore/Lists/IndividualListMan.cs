/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Calendar;
using BSLib.Design.MVP.Controls;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Operations;
using GKCore.Options;
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
            if (FilterLifeMode != FilterLifeMode.lmTimeLocked)
            {
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
    }


    public sealed class IndividualListMan : ListManager<GDMIndividualRecord>
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


        private GDMIndividualRecord fRec;
        private GDMGroupRecord filter_grp;
        private UDN filter_abd;
        private GDMSourceRecord filter_source;


        public IndividualListMan(IBaseContext baseContext) :
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

        protected override void CreateFilter()
        {
            fFilter = new IndividualListFilter();
        }

        private bool IsMatchesPlace(string fltResidence)
        {
            if (fltResidence == "*") return true;

            bool result = false;

            if (fRec.HasEvents) {
                bool includeAddr = GlobalOptions.Instance.PlacesWithAddress;
                var events = fRec.Events;
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

            if (fRec.HasEvents) {
                var events = fRec.Events;
                for (int i = 0, num = events.Count; i < num; i++) {
                    result = IsMatchesMask(events[i].StringValue, fltEventVal);
                    if (result) break;
                }
            }

            return result;
        }

        private bool CheckSpecificFilter()
        {
            bool result = false;

            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            if ((iFilter.Sex == GDMSex.svUnknown || fRec.Sex == iFilter.Sex)
                && (IsMatchesMask(buf_fullname, iFilter.Name))
                && (IsMatchesPlace(iFilter.Residence))
                && (IsMatchesEventVal(iFilter.EventVal))
                && (!iFilter.PatriarchOnly || fRec.Patriarch))
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
                        UDN bdt = (buf_bd == null) ? UDN.CreateEmpty() : buf_bd.Date.GetUDN();
                        UDN ddt = (buf_dd == null) ? UDN.CreateEmpty() : buf_dd.Date.GetUDN();
                        if ((bdt.CompareTo(filter_abd) > 0) || (ddt.CompareTo(filter_abd) < 0)) return false;
                        break;

                    case FilterLifeMode.lmTimeLocked:
                        break;
                }

                switch (iFilter.FilterGroupMode) {
                    case FilterGroupMode.All:
                        break;

                    case FilterGroupMode.None:
                        if (fRec.HasGroups) return false;
                        break;

                    case FilterGroupMode.Any:
                        if (!fRec.HasGroups) return false;
                        break;

                    case FilterGroupMode.Selected:
                        if (fRec.IndexOfGroup(filter_grp) < 0) return false;
                        break;
                }

                switch (iFilter.SourceMode) {
                    case FilterGroupMode.All:
                        break;

                    case FilterGroupMode.None:
                        if (fRec.HasSourceCitations) return false;
                        break;

                    case FilterGroupMode.Any:
                        if (!fRec.HasSourceCitations) return false;
                        break;

                    case FilterGroupMode.Selected:
                        if (fRec.IndexOfSource(filter_source) < 0) return false;
                        break;
                }

                result = true;
            }

            return result;
        }

        public override bool CheckFilter()
        {
            bool res = (fBaseContext.IsRecordAccess(fRec.Restriction) && IsMatchesMask(buf_fullname, QuickFilter));

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec) && CheckSpecificFilter();

            return res;
        }

        private object GetNameValueEx(int colSubtype)
        {
            object result = null;

            if (colSubtype == -1) {
                result = GKUtils.GetNameString(fRec, true, false);
            } else {
                NameFormat defNameFormat = GlobalOptions.Instance.DefNameFormat;
                NamePartsRet parts;
                GDMLanguageID defLang = fBaseContext.DefaultLanguage;

                switch (defNameFormat) {
                    case NameFormat.nfFNP:
                        result = GKUtils.GetNameString(fRec, GlobalOptions.Instance.SurnameFirstInOrder, false, defLang);
                        break;

                    case NameFormat.nfF_NP:
                        parts = GKUtils.GetNameParts(fBaseContext.Tree, fRec, true, defLang);
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
                        parts = GKUtils.GetNameParts(fBaseContext.Tree, fRec, true, defLang);
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
                    result = fRec.GetId();
                    break;

                case ColumnType.ctPatriarch:
                    result = ((fRec.Patriarch) ? "*" : " ");
                    break;

                case ColumnType.ctName:
                    result = GetNameValueEx(colSubtype);
                    break;

                case ColumnType.ctNick:
                    result = GKUtils.GetNickString(fRec);
                    break;

                case ColumnType.ctSex:
                    result = GKUtils.SexChar(fRec.Sex);
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
                    result = (isVisible) ? (object)GKUtils.GetAgeStr(fRec, -1) : GKUtils.GetAge(fRec, -1);
                    break;

                case ColumnType.ctLifeExpectancy:
                    result = (isVisible) ? (object)GKUtils.GetLifeExpectancyStr(fRec) : GKUtils.GetLifeExpectancy(fRec);
                    break;

                case ColumnType.ctDaysForBirth:
                    int days = GKUtils.GetDaysForBirth(fRec);
                    result = (days >= 0) ? (object)days : null;
                    break;

                case ColumnType.ctGroups:
                    result = GKUtils.GetGroupsStr(fBaseContext.Tree, fRec);
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
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;

                case ColumnType.ctBookmark:
                    result = ((fRec.Bookmark) ? "*" : " ");
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

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (GDMIndividualRecord)aRec;

            buf_fullname = GKUtils.GetNameString(fRec, true, false);
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

            int num = fRec.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent ev = fRec.Events[i];
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

        public override IListItem CreateListItem(int itemIndex, object rowData, CreateListItemHandler handler)
        {
            var item = base.CreateListItem(itemIndex, rowData, handler);

            GlobalOptions gOptions = GlobalOptions.Instance;
            if ((fRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnparentedPersons)) {
                item.SetBackColor(ChartRenderer.GetColor(GKData.HighlightUnparentedColor));
            } else if ((fRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnmarriedPersons)) {
                item.SetBackColor(ChartRenderer.GetColor(GKData.HighlightUnmarriedColor));
            }

            return item;
        }

        public override void UpdateColumns(IListViewEx listView)
        {
            ColumnsMap_Clear();

            NameFormat defNameFormat = GlobalOptions.Instance.DefNameFormat;
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
                            AddColumn(listView, LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
                            AddColumn(listView, LangMan.LS(LSID.LSID_Name), 100, asz, bColType, 1);
                            AddColumn(listView, LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 2);
                            break;

                        case NameFormat.nfF_NP:
                            AddColumn(listView, LangMan.LS(LSID.LSID_Surname), 150, asz, bColType, 0);
                            AddColumn(listView, LangMan.LS(LSID.LSID_Name) + "," + LangMan.LS(LSID.LSID_Patronymic), 150, asz, bColType, 1);
                            break;
                    }
                } else {
                    AddColumn(listView, LangMan.LS(columnProps.ColName), columnProps.CurWidth, false, bColType, 0);
                }
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupsSublistModel : SheetModel<GDMPointer>
    {
        public GroupsSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Group, 350, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                if (iRec.HasGroups) {
                    foreach (GDMPointer ptr in iRec.Groups) {
                        var grp = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(ptr);
                        if (grp != null) {
                            fSheetList.ListView.AddItem(grp, new object[] { grp.GroupName });
                        }
                    }
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("GroupsSublistModel.UpdateContent()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GDMGroupRecord groupRec = eArgs.ItemData as GDMGroupRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    groupRec = fBaseWin.Context.SelectRecord(GDMRecordType.rtGroup, null) as GDMGroupRecord;
                    result = (groupRec != null);
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, groupRec, iRec);
                    }
                    break;

                case RecordAction.raDelete:
                    result = (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachGroupQuery)));
                    if (result) {
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
    public sealed class NamesSublistModel : SheetModel<GDMPersonalName>
    {
        public NamesSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveDown, RecordAction.raMoveUp);

            fListColumns.AddColumn(LSID.LSID_Name, 350, false);
            fListColumns.AddColumn(LSID.LSID_Type, 100, false);
            fListColumns.AddColumn(LSID.LSID_Language, 150, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                foreach (GDMPersonalName pn in iRec.PersonalNames) {
                    string lang = GEDCOMUtils.GetLanguageStr(pn.Language);
                    fSheetList.ListView.AddItem(pn, new object[] { GKUtils.GetNameString(iRec, pn, true, false),
                                                          LangMan.LS(GKData.NameTypes[(int)pn.NameType]), lang });
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("NamesSublistModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

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
                        result = AppHost.Instance.ShowModalX(dlg, false);

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
    public sealed class ParentsSublistModel : SheetModel<GDMChildToFamilyLink>
    {
        public ParentsSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveDown, RecordAction.raMoveUp);

            fListColumns.AddColumn(LSID.LSID_Name, 350, false);
            fListColumns.AddColumn(LSID.LSID_Type, 100, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                foreach (GDMChildToFamilyLink cfLink in iRec.ChildToFamilyLinks) {
                    GDMFamilyRecord famRec = fBaseContext.Tree.GetPtrValue(cfLink);

                    fSheetList.ListView.AddItem(cfLink, new object[] {
                        GKUtils.GetFamilyString(fBaseContext.Tree, famRec),
                        LangMan.LS(GKData.ParentTypes[(int)cfLink.PedigreeLinkageType])
                    });
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("ParentsSublistModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GDMChildToFamilyLink cfLink = eArgs.ItemData as GDMChildToFamilyLink;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GDMFamilyRecord family = fBaseWin.Context.SelectFamily(iRec);
                    if (family != null && family.IndexOfChild(iRec) < 0) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, iRec, family);
                    }
                    break;

                case RecordAction.raEdit:
                    if (cfLink != null) {
                        using (var dlg = AppHost.ResolveDialog<IParentsEditDlg>(fBaseWin)) {
                            dlg.IndividualRecord = iRec;
                            dlg.ChildLink = cfLink;
                            result = AppHost.Instance.ShowModalX(dlg, false);
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
    public sealed class SpousesSublistModel : SheetModel<GDMSpouseToFamilyLink>
    {
        public SpousesSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump, RecordAction.raMoveUp, RecordAction.raMoveDown);

            fListColumns.AddColumn(LSID.LSID_NumberSym, 25, false);
            fListColumns.AddColumn(LSID.LSID_Spouse, 300, false);
            fListColumns.AddColumn(LSID.LSID_MarriageDate, 100, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                int idx = 0;
                foreach (GDMSpouseToFamilyLink spLink in iRec.SpouseToFamilyLinks) {
                    idx += 1;

                    GDMFamilyRecord family = fBaseContext.Tree.GetPtrValue(spLink);
                    if (family == null) continue;

                    GDMIndividualRecord relPerson;
                    string relName;

                    if (iRec.Sex == GDMSex.svMale) {
                        relPerson = fBaseContext.Tree.GetPtrValue(family.Wife);
                        relName = LangMan.LS(LSID.LSID_UnkFemale);
                    } else {
                        relPerson = fBaseContext.Tree.GetPtrValue(family.Husband);
                        relName = LangMan.LS(LSID.LSID_UnkMale);
                    }

                    if (relPerson != null) {
                        relName = GKUtils.GetNameString(relPerson, true, false);
                    }

                    fSheetList.ListView.AddItem(family, new object[] { idx,
                                           relName, new GDMDateItem(GKUtils.GetMarriageDate(family)) } );
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("SpousesSublistModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GDMFamilyRecord family = eArgs.ItemData as GDMFamilyRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    result = (BaseController.ModifyFamily(fBaseWin, ref family, TargetMode.tmSpouse, iRec));
                    if (result) {
                        eArgs.ItemData = family;
                    }
                    break;

                case RecordAction.raEdit:
                    result = (BaseController.ModifyFamily(fBaseWin, ref family, TargetMode.tmNone, null));
                    break;

                case RecordAction.raDelete:
                    if (family != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachSpouseQuery)))
                    {
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
    public sealed class URefsSublistModel : SheetModel<GDMUserReference>
    {
        public URefsSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete);

            fListColumns.AddColumn(LSID.LSID_Reference, 300, false);
            fListColumns.AddColumn(LSID.LSID_Type, 200, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                if (iRec.HasUserReferences) {
                    foreach (GDMUserReference uref in iRec.UserReferences) {
                        fSheetList.ListView.AddItem(uref, new object[] { uref.StringValue, uref.ReferenceType });
                    }
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("URefsSublistModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

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
                        result = AppHost.Instance.ShowModalX(dlg, false);

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
                        string confirmation =
                            !string.IsNullOrEmpty(userRef.StringValue) ? userRef.StringValue : userRef.ReferenceType;
                        confirmation = string.Format(
                            LangMan.LS(LSID.LSID_RemoveUserRefQuery), confirmation);
                        if (AppHost.StdDialogs.ShowQuestionYN(confirmation)) {
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
