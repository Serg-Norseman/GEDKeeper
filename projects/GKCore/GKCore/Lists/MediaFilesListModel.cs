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

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class MediaFilesListModel : SheetModel<GDMFileReferenceWithTitle>
    {
        public MediaFilesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stMediaFiles);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Title, DataType.dtString, 150, true);
            result.AddColumn(LSID.Type, DataType.dtString, 85, true);
            result.AddColumn(LSID.File, DataType.dtString, 300, true);
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            return null;
        }

        public override void Fetch(GDMFileReferenceWithTitle aRec)
        {
            base.Fetch(aRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = fFetchedRec.Title;
                    break;
                case 2:
                    result = LangMan.LS(GKData.MediaTypes[(int)fFetchedRec.MediaType]);
                    break;
                case 3:
                    result = fFetchedRec.StringValue;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            if (fDataOwner is GDMMultimediaRecord dataOwner)
                UpdateStructList(dataOwner.FileReferences);
        }

        private void UpdateButtons()
        {
            var actions = AllowedActions;

            if (fStructList.Count <= 1) {
                actions.Exclude(RecordAction.raDelete);
            } else {
                actions.Include(RecordAction.raDelete);
            }

            AllowedActions = actions;
        }

        public override void OnItemSelected(int itemIndex, object rowData)
        {
            UpdateButtons();
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var mmRec = fDataOwner as GDMMultimediaRecord;
            if (fBaseWin == null || mmRec == null) return;

            var fileRef = eArgs.ItemData as GDMFileReferenceWithTitle;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        bool exists = (fileRef != null);
                        if (!exists) {
                            fileRef = new GDMFileReferenceWithTitle();
                        }

                        using (var dlg = AppHost.ResolveDialog<IMediaFileEditDlg>(fBaseWin)) {
                            dlg.FileRef = fileRef;
                            result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, true);
                        }

                        if (!result) {
                            if (!exists) {
                                fileRef.Dispose();
                            }
                        } else {
                            if (!exists) {
                                result = fUndoman.DoOrdinaryOperation(OperationType.otMediaFileAdd, mmRec, fileRef);
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachMultimediaQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otMediaFileRemove, mmRec, fileRef);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    result = Exchange(mmRec.FileReferences, fileRef, eArgs.Action);
                    break;
            }

            if (result) {
                UpdateButtons();

                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = fileRef;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
