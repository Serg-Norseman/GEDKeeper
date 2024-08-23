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

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RepositoryCitationsListModel : SheetModel<GDMRepositoryCitation>
    {
        private GDMRepositoryRecord fRepoRec;

        public RepositoryCitationsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump, RecordAction.raCopy, RecordAction.raPaste, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stSourceRepositories);

            result.AddColumn(LSID.Repository, 280, false);
            result.AddColumn(LSID.CallNumbers, 220, false);

            result.ResetDefaults();
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            var repoCit = itemData as GDMRepositoryCitation;
            return (repoCit == null) ? null : fBaseContext.Tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
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

                case 1:
                    result = GKUtils.GetCallNumbersStr(fFetchedRec);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var source = fDataOwner as GDMSourceRecord;
            if (source != null)
                UpdateStructList(source.RepositoryCitations);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var source = fDataOwner as GDMSourceRecord;
            if (fBaseWin == null || source == null) return;

            var repoCit = eArgs.ItemData as GDMRepositoryCitation;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        var repoRecRes = await BaseController.ModifyRepositoryCitation(fOwner, fBaseWin, fUndoman, source, repoCit);
                        repoCit = repoRecRes.Record;
                        result = repoRecRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (repoCit != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachRepositoryQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationRemove, source, repoCit);
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
                        var repoRec = fBaseContext.Tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                        repoCit = new GDMRepositoryCitation();
                        repoCit.XRef = repoRec.XRef;
                        result = fUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, source, repoCit);
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = repoCit;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
