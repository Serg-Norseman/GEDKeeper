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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class MediaLinksListModel : ListModel
    {
        public MediaLinksListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown);
        }

        public override void InitView()
        {
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_RPMultimedia), 300, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Type), 300, false);
        }

        public override void UpdateContent()
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fSheetList == null || dataOwner == null) return;

            try
            {
                fSheetList.ClearItems();

                foreach (GEDCOMMultimediaLink mmLink in dataOwner.MultimediaLinks)
                {
                    GEDCOMMultimediaRecord mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                    if (mmRec == null) continue;

                    if (mmRec.FileReferences.Count == 0) continue;

                    GEDCOMFileReferenceWithTitle fileRef = mmRec.FileReferences[0];
                    IListItem item = fSheetList.AddItem(fileRef.Title, mmLink);
                    item.AddSubItem(LangMan.LS(GKData.MediaTypes[(int) fileRef.MediaType]));
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKMediaSheet.UpdateSheet(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GEDCOMMultimediaLink mmLink = eArgs.ItemData as GEDCOMMultimediaLink;

            bool result = false;

            GEDCOMMultimediaRecord mmRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    mmRec = fBaseWin.Context.SelectRecord(GEDCOMRecordType.rtMultimedia, new object[0]) as GEDCOMMultimediaRecord;
                    if (mmRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordMediaAdd, (GEDCOMObject)dataOwner, mmRec);
                    }
                    break;

                case RecordAction.raEdit:
                    if (mmLink != null)
                    {
                        mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                        result = BaseController.ModifyMedia(fBaseWin, ref mmRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachMultimediaQuery)) != false)
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordMediaRemove, (GEDCOMObject)dataOwner, mmLink);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = dataOwner.MultimediaLinks.IndexOf(mmLink);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                dataOwner.MultimediaLinks.Exchange(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                dataOwner.MultimediaLinks.Exchange(idx, idx + 1);
                                break;
                        }

                        result = true;
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                fSheetList.UpdateSheet();
            }
        }
    }
}
