/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public enum SourceColumnType
    {
        ctShortName,
        ctAuthor,
        ctTitle,
        ctChangeDate
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceListMan : ListManager
    {
        private GDMSourceRecord fRec;


        public SourceListMan(IBaseContext baseContext) :
            base(baseContext, CreateSourceListColumns(), GDMRecordType.rtSource)
        {
        }

        public static ListColumns CreateSourceListColumns()
        {
            var result = new ListColumns();

            result.AddColumn(LSID.LSID_ShortTitle, DataType.dtString, 120, true, true);
            result.AddColumn(LSID.LSID_Author, DataType.dtString, 200, true);
            result.AddColumn(LSID.LSID_Title, DataType.dtString, 200, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(fRec.ShortTitle, QuickFilter));

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (aRec as GDMSourceRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((SourceColumnType)colType)
            {
                case SourceColumnType.ctShortName:
                    result = fRec.ShortTitle.Trim();
                    break;

                case SourceColumnType.ctAuthor:
                    result = fRec.Originator.Lines.Text.Trim();
                    break;

                case SourceColumnType.ctTitle:
                    result = fRec.Title.Lines.Text.Trim();
                    break;

                case SourceColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceRepositoriesSublistModel : ListModel
    {
        public SourceRepositoriesSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Repository, 300, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var source = fDataOwner as GDMSourceRecord;
            if (fSheetList == null || source == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GDMRepositoryCitation repCit in source.RepositoryCitations) {
                    GDMRepositoryRecord rep = repCit.Value as GDMRepositoryRecord;
                    if (rep == null) continue;

                    fSheetList.AddItem(repCit, new object[] { rep.RepositoryName });
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex){
                Logger.LogException(ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var source = fDataOwner as GDMSourceRecord;
            if (fBaseWin == null || fSheetList == null || source == null) return;

            GDMRepositoryCitation cit = eArgs.ItemData as GDMRepositoryCitation;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GDMRepositoryRecord rep = fBaseWin.Context.SelectRecord(GDMRecordType.rtRepository, null) as GDMRepositoryRecord;
                    if (rep != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, source, rep);
                    }
                    break;

                case RecordAction.raDelete:
                    if (cit != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachRepositoryQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationRemove, source, cit.Value as GDMRepositoryRecord);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
