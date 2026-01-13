/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
