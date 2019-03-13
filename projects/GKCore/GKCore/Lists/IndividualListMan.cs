/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Text;

using BSLib;
using BSLib.Calendar;
using GKCommon.GEDCOM;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
    public enum PersonColumnType
    {
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
        public GEDCOMSex Sex;
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
            Sex = GEDCOMSex.svNone;
            SourceMode = FilterGroupMode.All;
            SourceRef = "";
            EventVal = "*";
        }
    }

    public sealed class IndividualListMan : ListManager
    {
        private GEDCOMIndividualRecord fRec;
        private GEDCOMGroupRecord filter_grp;
        private UDN filter_abd;
        private GEDCOMSourceRecord filter_source;

        public IndividualListMan(IBaseContext baseContext) :
            base(baseContext, CreateIndividualListColumns(), GEDCOMRecordType.rtIndividual)
        {
        }

        public static ListColumns CreateIndividualListColumns()
        {
            var result = new ListColumns();

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

        private string GetGroups()
        {
            StringBuilder result = new StringBuilder();

            int count = fRec.Groups.Count;
            for (int idx = 0; idx < count; idx++) {
                GEDCOMGroupRecord grp = fRec.Groups[idx].Value as GEDCOMGroupRecord;
                if (grp != null) {
                    if (idx > 0) result.Append("; ");

                    result.Append(grp.GroupName);
                }
            }

            return result.ToString();
        }

        private bool HasPlace()
        {
            bool result = false;

            string fltResidence = ((IndividualListFilter)fFilter).Residence;
            bool hasAddr = GlobalOptions.Instance.PlacesWithAddress;

            int num = fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                string place = GKUtils.GetPlaceStr(fRec.Events[i], hasAddr);
                result = IsMatchesMask(place, fltResidence);
                if (result) break;
            }

            return result;
        }

        private bool HasEventVal()
        {
            bool result = false;

            string fltEventVal = ((IndividualListFilter)fFilter).EventVal;

            int num = fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                result = IsMatchesMask(fRec.Events[i].StringValue, fltEventVal);
                if (result) break;
            }

            return result;
        }

        private bool CheckSpecificFilter()
        {
            bool result = false;

            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            if ((iFilter.Sex == GEDCOMSex.svNone || fRec.Sex == iFilter.Sex)
                && (iFilter.Name == "*" || IsMatchesMask(buf_fullname, iFilter.Name))
                && (iFilter.Residence == "*" || HasPlace())
                && (iFilter.EventVal == "*" || HasEventVal())
                && (!iFilter.PatriarchOnly || fRec.Patriarch))
            {
                bool isLive = (buf_dd == null);

                switch (iFilter.FilterLifeMode)
                {
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

                switch (iFilter.FilterGroupMode)
                {
                    case FilterGroupMode.All:
                        break;

                    case FilterGroupMode.None:
                        if (fRec.Groups.Count != 0) return false;
                        break;

                    case FilterGroupMode.Any:
                        if (fRec.Groups.Count == 0) return false;
                        break;

                    case FilterGroupMode.Selected:
                        if (fRec.IndexOfGroup(filter_grp) < 0) return false;
                        break;
                }

                switch (iFilter.SourceMode)
                {
                    case FilterGroupMode.All:
                        break;

                    case FilterGroupMode.None:
                        if (fRec.SourceCitations.Count != 0) return false;
                        break;

                    case FilterGroupMode.Any:
                        if (fRec.SourceCitations.Count == 0) return false;
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
            bool res = (fBaseContext.IsRecordAccess(fRec.Restriction)
                        && (QuickFilter == "*" || IsMatchesMask(buf_fullname, QuickFilter)));

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
                GKUtils.NamePartsRet parts;

                switch (defNameFormat) {
                    case NameFormat.nfFNP:
                        result = GKUtils.GetNameString(fRec, true, false);
                        break;

                    case NameFormat.nfF_NP:
                        parts = GKUtils.GetNameParts(fRec);
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
                        parts = GKUtils.GetNameParts(fRec);
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

            switch ((PersonColumnType)colType)
            {
                case PersonColumnType.ctPatriarch:
                    result = ((fRec.Patriarch) ? "*" : " ");
                    break;

                case PersonColumnType.ctName:
                    result = GetNameValueEx(colSubtype);
                    break;

                case PersonColumnType.ctNick:
                    result = GKUtils.GetNickString(fRec);
                    break;

                case PersonColumnType.ctSex:
                    result = GKUtils.SexChar(fRec.Sex);
                    break;

                case PersonColumnType.ctBirthDate:
                    result = GetDateValue(buf_bd, isVisible);
                    break;

                case PersonColumnType.ctBirthPlace:
                    result = GKUtils.GetPlaceStr(buf_bd, false);
                    break;

                case PersonColumnType.ctDeathDate:
                    result = GetDateValue(buf_dd, isVisible);
                    break;

                case PersonColumnType.ctDeathPlace:
                    result = GKUtils.GetPlaceStr(buf_dd, false);
                    break;

                case PersonColumnType.ctResidence:
                    result = buf_residence;
                    break;

                case PersonColumnType.ctAge:
                    result = (isVisible) ? (object)GKUtils.GetAgeStr(fRec, -1) : GKUtils.GetAge(fRec, -1);
                    break;

                case PersonColumnType.ctLifeExpectancy:
                    result = (isVisible) ? (object)GKUtils.GetLifeExpectancyStr(fRec) : GKUtils.GetLifeExpectancy(fRec);
                    break;

                case PersonColumnType.ctDaysForBirth:
                    int days = GKUtils.GetDaysForBirth(fRec);
                    result = (days >= 0) ? (object)days : null;
                    break;

                case PersonColumnType.ctGroups:
                    result = GetGroups();
                    break;

                case PersonColumnType.ctReligion:
                    result = buf_religion;
                    break;

                case PersonColumnType.ctNationality:
                    result = buf_nationality;
                    break;

                case PersonColumnType.ctEducation:
                    result = buf_education;
                    break;

                case PersonColumnType.ctOccupation:
                    result = buf_occupation;
                    break;

                case PersonColumnType.ctCaste:
                    result = buf_caste;
                    break;

                case PersonColumnType.ctMili:
                    result = buf_mili;
                    break;

                case PersonColumnType.ctMiliInd:
                    result = buf_mili_ind;
                    break;

                case PersonColumnType.ctMiliDis:
                    result = buf_mili_dis;
                    break;

                case PersonColumnType.ctMiliRank:
                    result = buf_mili_rank;
                    break;

                case PersonColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;

                case PersonColumnType.ctBookmark:
                    result = ((fRec.Bookmark) ? "*" : " ");
                    break;

                case PersonColumnType.ctTitle:
                    result = buf_title;
                    break;
            }

            return result;
        }

        public override void PrepareFilter()
        {
            IndividualListFilter iFilter = (IndividualListFilter)fFilter;

            filter_abd = GEDCOMDate.GetUDNByFormattedStr(iFilter.AliveBeforeDate, GEDCOMCalendar.dcGregorian);

            if (iFilter.GroupRef == "") {
                filter_grp = null;
            } else {
                filter_grp = fBaseContext.Tree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
            }

            if (iFilter.SourceRef == "") {
                filter_source = null;
            } else {
                filter_source = fBaseContext.Tree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
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
            fRec = (aRec as GEDCOMIndividualRecord);
            if (fRec == null) return;

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
            for (int i = 0; i < num; i++)
            {
                GEDCOMCustomEvent ev = fRec.Events[i];

                if (ev.Name == GEDCOMTagType.BIRT && buf_bd == null)
                {
                    buf_bd = ev;
                }
                else if (ev.Name == GEDCOMTagType.DEAT && buf_dd == null)
                {
                    buf_dd = ev;
                }
                else if (ev.Name == GEDCOMTagType.RESI && buf_residence == "")
                {
                    buf_residence = GKUtils.GetPlaceStr(ev, gOptions.PlacesWithAddress);
                }
                else if (ev.Name == GEDCOMTagType.RELI && buf_religion == "")
                {
                    buf_religion = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType.NATI && buf_nationality == "")
                {
                    buf_nationality = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType.EDUC && buf_education == "")
                {
                    buf_education = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType.OCCU && buf_occupation == "")
                {
                    buf_occupation = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType.CAST && buf_caste == "")
                {
                    buf_caste = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType._MILI && buf_mili == "")
                {
                    buf_mili = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType._MILI_IND && buf_mili_ind == "")
                {
                    buf_mili_ind = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType._MILI_DIS && buf_mili_dis == "")
                {
                    buf_mili_dis = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType._MILI_RANK && buf_mili_rank == "")
                {
                    buf_mili_rank = ev.StringValue;
                }
                else if (ev.Name == GEDCOMTagType.TITL && buf_title == "")
                {
                    buf_title = ev.StringValue;
                }
            }
        }

        public override void UpdateItem(int itemIndex, IListItem item, object rowData)
        {
            base.UpdateItem(itemIndex, item, rowData);

            GlobalOptions gOptions = GlobalOptions.Instance;

            if ((fRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnparentedPersons))
            {
                item.SetBackColor(ChartRenderer.GetColor(GKData.HighlightUnparentedColor));
            }
            else if ((fRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnmarriedPersons))
            {
                item.SetBackColor(ChartRenderer.GetColor(GKData.HighlightUnmarriedColor));
            }
        }

        public override void UpdateItemProps(IListItem item, object rowData)
        {
            GlobalOptions gOptions = GlobalOptions.Instance;

            if ((fRec.ChildToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnparentedPersons))
            {
                item.SetBackColor(ChartRenderer.GetColor(GKData.HighlightUnparentedColor));
            }
            else if ((fRec.SpouseToFamilyLinks.Count == 0) && (gOptions.ListHighlightUnmarriedPersons))
            {
                item.SetBackColor(ChartRenderer.GetColor(GKData.HighlightUnmarriedColor));
            }
        }

        public override void UpdateColumns(IListView listView)
        {
            ColumnsMap_Clear();
            AddColumn(listView, "№", 50, false, 0, 0);

            NameFormat defNameFormat = GlobalOptions.Instance.DefNameFormat;
            ListColumns columns = (ListColumns)this.ListColumns;

            int num = columns.Count;
            for (int i = 0; i < num; i++)
            {
                ListColumn columnProps = columns.OrderedColumns[i];
                if (!columnProps.CurActive) continue;

                const bool asz = false;
                byte bColType = columnProps.Id;
                if (bColType == (byte)PersonColumnType.ctName && defNameFormat != NameFormat.nfFNP) {
                    switch (defNameFormat)
                    {
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
    public sealed class GroupsSublistModel : ListModel
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
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GEDCOMPointer ptr in iRec.Groups) {
                    GEDCOMGroupRecord grp = ptr.Value as GEDCOMGroupRecord;
                    if (grp != null) {
                        fSheetList.AddItem(grp, new object[] { grp.GroupName });
                    }
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GroupsSublistModel.UpdateContent(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GEDCOMGroupRecord groupRec = eArgs.ItemData as GEDCOMGroupRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    groupRec = fBaseWin.Context.SelectRecord(GEDCOMRecordType.rtGroup, null) as GEDCOMGroupRecord;
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
    public sealed class NamesSublistModel : ListModel
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
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GEDCOMPersonalName pn in iRec.PersonalNames) {
                    string lang = GEDCOMUtils.GetLanguageStr(pn.Language.Value);
                    fSheetList.AddItem(pn, new object[] { GKUtils.GetNameString(iRec, pn, true, false),
                                                          LangMan.LS(GKData.NameTypes[(int)pn.NameType]), lang });
                }

                fSheetList.EndUpdate();
            } catch (Exception ex) {
                Logger.LogWrite("NamesSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GEDCOMPersonalName persName = eArgs.ItemData as GEDCOMPersonalName;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (var dlg = AppHost.Container.Resolve<IPersonalNameEditDlg>(fBaseWin)) {
                        bool exists = (persName != null);
                        if (!exists) {
                            persName = new GEDCOMPersonalName(fBaseWin.Context.Tree, iRec);
                        }

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
    public sealed class ParentsSublistModel : ListModel
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
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GEDCOMChildToFamilyLink cfLink in iRec.ChildToFamilyLinks) {
                    GEDCOMFamilyRecord famRec = cfLink.Family;

                    fSheetList.AddItem(cfLink, new object[] { GKUtils.GetFamilyString(famRec),
                                                          LangMan.LS(GKData.ParentTypes[(int)cfLink.PedigreeLinkageType]) });
                }

                fSheetList.EndUpdate();
            } catch (Exception ex) {
                Logger.LogWrite("ParentsSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GEDCOMChildToFamilyLink cfLink = eArgs.ItemData as GEDCOMChildToFamilyLink;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GEDCOMFamilyRecord family = fBaseWin.Context.SelectFamily(iRec);
                    if (family != null && family.IndexOfChild(iRec) < 0) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, iRec, family);
                    }
                    break;

                case RecordAction.raEdit:
                    if (cfLink != null) {
                        using (var dlg = AppHost.Container.Resolve<IParentsEditDlg>(fBaseWin)) {
                            dlg.Person = iRec;
                            dlg.Link = cfLink;
                            result = AppHost.Instance.ShowModalX(dlg, false);
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachParentsQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, iRec, cfLink.Family);
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
    public sealed class SpousesSublistModel : ListModel
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
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                int idx = 0;
                foreach (GEDCOMSpouseToFamilyLink spLink in iRec.SpouseToFamilyLinks) {
                    idx += 1;

                    GEDCOMFamilyRecord family = spLink.Family;
                    if (family == null) continue;

                    GEDCOMIndividualRecord relPerson;
                    string relName;

                    if (iRec.Sex == GEDCOMSex.svMale) {
                        relPerson = family.GetWife();
                        relName = LangMan.LS(LSID.LSID_UnkFemale);
                    } else {
                        relPerson = family.GetHusband();
                        relName = LangMan.LS(LSID.LSID_UnkMale);
                    }

                    if (relPerson != null) {
                        relName = GKUtils.GetNameString(relPerson, true, false);
                    }

                    fSheetList.AddItem(family, new object[] { idx,
                                           relName, new GEDCOMDateItem(GKUtils.GetMarriageDate(family)) } );
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SpousesSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GEDCOMFamilyRecord family = eArgs.ItemData as GEDCOMFamilyRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    result = (BaseController.ModifyFamily(fBaseWin, ref family, TargetMode.tmFamilySpouse, iRec));
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
    public sealed class URefsSublistModel : ListModel
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
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fSheetList == null || iRec == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GEDCOMUserReference uref in iRec.UserReferences) {
                    fSheetList.AddItem(uref, new object[] { uref.StringValue, uref.ReferenceType });
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("URefsSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GEDCOMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || iRec == null) return;

            GEDCOMUserReference userRef = eArgs.ItemData as GEDCOMUserReference;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (var dlg = AppHost.Container.Resolve<IUserRefEditDlg>(fBaseWin)) {
                        bool exists = (userRef != null);
                        if (!exists) {
                            userRef = new GEDCOMUserReference(fBaseWin.Context.Tree, iRec);
                        }

                        dlg.UserRef = userRef;
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
