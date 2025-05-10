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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

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

            result.ResetDefaults();
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
