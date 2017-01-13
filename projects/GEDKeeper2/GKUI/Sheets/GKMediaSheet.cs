/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public sealed class GKMediaSheet : GKCustomSheet
    {
        public GKMediaSheet(IBaseEditor baseEditor, Control owner, ChangeTracker undoman) : base(baseEditor, owner, undoman)
        {
            this.Columns_BeginUpdate();
            this.AddColumn(LangMan.LS(LSID.LSID_RPMultimedia), 300, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Type), 300, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                       SheetButton.lbMoveUp, SheetButton.lbMoveDown);
            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
            if (this.DataList == null) return;
            
            try
            {
                this.ClearItems();

                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    GEDCOMMultimediaLink mmLink = this.DataList.Current as GEDCOMMultimediaLink;
                    if (mmLink == null) continue;

                    GEDCOMMultimediaRecord mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                    if (mmRec == null) continue;

                    if (mmRec.FileReferences.Count == 0) continue;

                    GEDCOMFileReferenceWithTitle fileRef = mmRec.FileReferences[0];
                    GKListItem item = this.AddItem(fileRef.Title, mmLink);
                    item.AddSubItem(LangMan.LS(GKData.MediaTypes[(int) fileRef.MediaType]));
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKMediaSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            if (this.DataList == null) return;

            IBaseWindow baseWin = this.Editor.Base;
            if (baseWin == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            if (_struct == null) return;

            GEDCOMMultimediaLink mmLink = eArgs.ItemData as GEDCOMMultimediaLink;
            
            bool result = false;

            GEDCOMMultimediaRecord mmRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    mmRec = baseWin.SelectRecord(GEDCOMRecordType.rtMultimedia, new object[0]) as GEDCOMMultimediaRecord;
                    if (mmRec != null) {
                        //result = (_struct.AddMultimedia(mmRec) != null);
                        result = this.fUndoman.DoOrdinaryOperation(OperationType.otRecordMediaAdd, (GEDCOMObject)_struct, mmRec);
                    }
                    break;

                case RecordAction.raEdit:
                    if (mmLink != null)
                    {
                        mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                        result = baseWin.ModifyMedia(ref mmRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMultimediaQuery)) != DialogResult.No)
                    {
                        //_struct.MultimediaLinks.Delete(mmLink);
                        //result = true;
                        result = this.fUndoman.DoOrdinaryOperation(OperationType.otRecordMediaRemove, (GEDCOMObject)_struct, mmLink);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = _struct.MultimediaLinks.IndexOf(mmLink);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                _struct.MultimediaLinks.Exchange(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                _struct.MultimediaLinks.Exchange(idx, idx + 1);
                                break;
                        }

                        result = true;
                    }
                    break;
            }

            if (result) {
                baseWin.Modified = true;
                this.UpdateSheet();
            }
        }
    }
}
