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
    public sealed class SourceCitationsListModel : SheetModel<GDMSourceCitation>
    {
        private GDMSourceRecord fSourceRec;

        public SourceCitationsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown,
                RecordAction.raCopy, RecordAction.raPaste, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stSourceCitations);

            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.Title, 260, false);
            result.AddColumn(LSID.Page, 90, false);
            result.AddColumn(LSID.Certainty, 220, false);
            result.AddColumn(LSID.Author, 70, false);

            result.ResetDefaults();
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            var srcCit = itemData as GDMSourceCitation;
            return (srcCit == null) ? null : fBaseContext.Tree.GetPtrValue<GDMSourceRecord>(srcCit);
        }

        public override void Fetch(GDMSourceCitation aRec)
        {
            base.Fetch(aRec);
            fSourceRec = fBaseContext.Tree.GetPtrValue<GDMSourceRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = fSourceRec.ShortTitle;
                    break;
                case 2:
                    result = fFetchedRec.Page;
                    break;
                case 3:
                    result = LangMan.LS(GKData.CertaintyAssessments[fFetchedRec.GetValidCertaintyAssessment()]);
                    break;
                case 4:
                    result = GKUtils.MergeStrings(fSourceRec.Originator.Lines);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as IGDMStructWithSourceCitations;
            if (dataOwner != null)
                UpdateStructList(dataOwner.SourceCitations);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGDMStructWithSourceCitations;
            if (fBaseWin == null || dataOwner == null) return;

            var srcCit = eArgs.ItemData as GDMSourceCitation;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        var srcCitRes = await BaseController.ModifySourceCitation(fOwner, fBaseWin, fUndoman, dataOwner, srcCit);
                        srcCit = srcCitRes.Record;
                        result = srcCitRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachSourceQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordSourceCitRemove, fDataOwner, srcCit);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    result = dataOwner.SourceCitations.Exchange(srcCit, eArgs.Action);
                    break;

                case RecordAction.raCopy:
                    AppHost.Instance.SetClipboardObj<GDMSourceCitation>(srcCit);
                    break;

                case RecordAction.raCut:
                    break;

                case RecordAction.raPaste:
                    srcCit = AppHost.Instance.GetClipboardObj<GDMSourceCitation>();
                    if (srcCit != null) {
                        srcCit = srcCit.Clone();
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordSourceCitAdd, fDataOwner, srcCit);
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = srcCit;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
