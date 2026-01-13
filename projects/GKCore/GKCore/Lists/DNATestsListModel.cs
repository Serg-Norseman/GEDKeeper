/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
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
    public sealed class DNATestsListModel : SheetModel<GDMDNATest>
    {
        private readonly GlobalOptions fOptions;

        public DNATestsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete);

            fOptions = GlobalOptions.Instance;
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stIndividualDNATests);
            result.AddColumn(LSID.Date, 90, false);
            result.AddColumn(LSID.DNATestName, 90, false);
            result.AddColumn(LSID.DNALaboratory, 200, false);
            result.AddColumn(LSID.DNAFileFormat, 90, false);
            //result.AddColumn(LSID.FileReference, 90, false);
            result.AddColumn(LSID.MDNAHaplogroup, 90, false);
            result.AddColumn(LSID.YDNAHaplogroup, 90, false);
            result.AddColumn(LSID.RPNotes, 32, false);
            result.AddColumn(LSID.RPMultimedia, 32, false);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = new GDMDateItem(fFetchedRec.Date);
                    break;
                case 1:
                    result = fFetchedRec.TestName;
                    break;
                case 2:
                    result = fFetchedRec.Agency;
                    break;
                case 3:
                    result = fFetchedRec.FileFormat.ToString();
                    break;
                case 4:
                    result = fFetchedRec.MHaplogroup;
                    break;
                case 5:
                    result = fFetchedRec.YHaplogroup;
                    break;
                case 6:
                    result = fFetchedRec.HasNotes ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.Notes.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
                case 7:
                    result = fFetchedRec.HasMultimediaLinks ? (fOptions.ShowNumberOfSubstructures ? fFetchedRec.MultimediaLinks.Count.ToString() : GKData.CHECK_MARK) : string.Empty;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as IGDMStructWithDNA;
            if (dataOwner != null)
                UpdateStructList(dataOwner.DNATests);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var record = fDataOwner as IGDMStructWithDNA;
            if (fBaseWin == null || record == null) return;

            GDMDNATest dnaTest = eArgs.ItemData as GDMDNATest;

            bool result = false;

            try {
                switch (eArgs.Action) {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit: {
                            bool exists = (dnaTest != null);
                            if (!exists) {
                                dnaTest = new GDMDNATest();
                            }

                            using (var dlg = AppHost.ResolveDialog<IDNATestEditDlg>(fBaseWin)) {
                                dlg.DNATest = dnaTest;
                                result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, true);
                            }

                            if (!result) {
                                if (!exists) {
                                    dnaTest.Dispose();
                                }
                            } else {
                                if (!exists) {
                                    result = fUndoman.DoOrdinaryOperation(OperationType.otDNATestAdd, record, dnaTest);
                                }
                            }
                        }
                        break;

                    case RecordAction.raDelete:
                        if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveDNATestQuery))) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otDNATestRemove, record, dnaTest);
                            dnaTest = null;
                        }
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("DNATestsListModel.Modify()", ex);
                result = false;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = dnaTest;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
