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
    public sealed class MediaLinksListModel : SheetModel<GDMMultimediaLink>
    {
        private GDMFileReferenceWithTitle fFileRef;

        public MediaLinksListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stMediaLinks);

            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.RPMultimedia, 300, false);
            result.AddColumn(LSID.Type, 300, false);

            result.ResetDefaults();
            return result;
        }

        public override void Fetch(GDMMultimediaLink aRec)
        {
            base.Fetch(aRec);

            GDMMultimediaRecord mmRec = fBaseContext.Tree.GetPtrValue<GDMMultimediaRecord>(fFetchedRec);
            fFileRef = (mmRec == null || mmRec.FileReferences.Count == 0) ? null : mmRec.FileReferences[0];
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = fFileRef.Title;
                    break;
                case 2:
                    result = LangMan.LS(GKData.MediaTypes[(int)fFileRef.MediaType]);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as IGDMStructWithMultimediaLinks;
            if (dataOwner != null)
                UpdateStructList(dataOwner.MultimediaLinks);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGDMStructWithMultimediaLinks;
            if (fBaseWin == null || dataOwner == null) return;

            var mmLink = eArgs.ItemData as GDMMultimediaLink;

            bool result = false;

            GDMMultimediaRecord mmRec;
            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    mmRec = await fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtMultimedia, new object[0]) as GDMMultimediaRecord;
                    if (mmRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordMediaAdd, (GDMObject)dataOwner, mmRec);
                        mmLink = dataOwner.FindMultimediaLink(mmRec);

                        if (result && mmLink != null && (dataOwner is GDMIndividualRecord) && GKUtils.MayContainPortrait(mmRec)) {
                            await BaseController.SelectPortraitRegion(fOwner, fBaseWin, mmLink);
                        }
                    }
                    break;

                case RecordAction.raEdit:
                    if (mmLink != null) {
                        mmRec = fBaseContext.Tree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                        var mmRes = await BaseController.ModifyMedia(fOwner, fBaseWin, mmRec);
                        result = mmRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachMultimediaQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordMediaRemove, (GDMObject)dataOwner, mmLink);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    result = dataOwner.MultimediaLinks.Exchange(mmLink, eArgs.Action);
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = mmLink;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
