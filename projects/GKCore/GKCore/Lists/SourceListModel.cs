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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceListModel : RecordsListModel<GDMSourceRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctShortTitle,
            ctDate,
            ctAuthor,
            ctTitle,
            //ctPublication,
            ctRepositories,
            ctChangeDate
        }


        public SourceListModel(IBaseContext baseContext) :
            base(baseContext, CreateSourceListColumns(), GDMRecordType.rtSource)
        {
        }

        public static ListColumns<GDMSourceRecord> CreateSourceListColumns()
        {
            var result = new ListColumns<GDMSourceRecord>();

            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.ShortTitle, DataType.dtString, 120, true, true);
            result.AddColumn(LSID.Date, 80, false);
            result.AddColumn(LSID.Author, DataType.dtString, 200, true);
            result.AddColumn(LSID.Title, DataType.dtString, 200, true);
            //result.AddColumn(LSID.Publication, DataType.dtString, 200, true);
            result.AddColumn(LSID.RPRepositories, DataType.dtString, 200, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = CheckQuickFilter(fFetchedRec.ShortTitle);

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec);

            return res;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctShortTitle:
                    result = fFetchedRec.ShortTitle.Trim();
                    break;

                case ColumnType.ctDate:
                    result = new GDMDateItem(fFetchedRec.Date.Value);
                    break;

                case ColumnType.ctAuthor:
                    result = fFetchedRec.Originator.Lines.Text.Trim();
                    break;

                case ColumnType.ctTitle:
                    result = fFetchedRec.Title.Lines.Text.Trim();
                    break;

                //case ColumnType.ctPublication:
                //    result = fFetchedRec.Publication.Lines.Text.Trim();
                //    break;

                case ColumnType.ctRepositories:
                    result = GKUtils.GetSourceRepositories(fBaseContext.Tree, fFetchedRec);
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceRepositoriesListModel : SheetModel<GDMRepositoryCitation>
    {
        private GDMRepositoryRecord fRepoRec;

        public SourceRepositoriesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raDelete, RecordAction.raJump,
                RecordAction.raCopy, RecordAction.raPaste);

            fListColumns.AddColumn(LSID.Repository, 300, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMRepositoryCitation aRec)
        {
            base.Fetch(aRec);
            fRepoRec = fBaseContext.Tree.GetPtrValue<GDMRepositoryRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fRepoRec.RepositoryName;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var source = fDataOwner as GDMSourceRecord;
            if (source == null) return;

            try {
                UpdateStructList(source.RepositoryCitations);
            } catch (Exception ex) {
                Logger.WriteError("SourceRepositoriesListModel.UpdateContents()", ex);
            }
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var source = fDataOwner as GDMSourceRecord;
            if (fBaseWin == null || source == null) return;

            var repoCit = eArgs.ItemData as GDMRepositoryCitation;
            GDMRepositoryRecord repoRec = null;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    repoRec = await fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtRepository, null) as GDMRepositoryRecord;
                    if (repoRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, source, repoRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (repoCit != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachRepositoryQuery))) {
                        repoRec = fBaseContext.Tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationRemove, source, repoRec);
                    }
                    break;

                case RecordAction.raCopy:
                    AppHost.Instance.SetClipboardObj<GDMRepositoryCitation>(repoCit);
                    break;

                case RecordAction.raCut:
                    break;

                case RecordAction.raPaste:
                    repoCit = AppHost.Instance.GetClipboardObj<GDMRepositoryCitation>();
                    if (repoCit != null) {
                        repoRec = fBaseContext.Tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, source, repoRec);
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = source.FindRepository(repoRec);
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
