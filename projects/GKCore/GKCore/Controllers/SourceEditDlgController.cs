/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceEditDlgController : DialogController<ISourceEditDlg>
    {
        private GEDCOMSourceRecord fSourceRecord;

        public GEDCOMSourceRecord SourceRecord
        {
            get { return fSourceRecord; }
            set {
                if (fSourceRecord != value) {
                    fSourceRecord = value;
                    UpdateView();
                }
            }
        }


        public SourceEditDlgController(ISourceEditDlg view) : base(view)
        {
            fView.ShortTitle.Activate();
        }

        public override bool Accept()
        {
            try {
                fSourceRecord.FiledByEntry = fView.ShortTitle.Text;
                fSourceRecord.Originator.Clear();
                fSourceRecord.SetOriginatorArray(fView.Author.Lines);
                fSourceRecord.Title.Clear();
                fSourceRecord.SetTitleArray(fView.Title.Lines);
                fSourceRecord.Publication.Clear();
                fSourceRecord.SetPublicationArray(fView.Publication.Lines);
                fSourceRecord.Text.Clear();
                fSourceRecord.SetTextArray(fView.Text.Lines);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fSourceRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("SourceEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.ShortTitle.Text = fSourceRecord.FiledByEntry;
            fView.Author.Text = fSourceRecord.Originator.Text.Trim();
            fView.Title.Text = fSourceRecord.Title.Text.Trim();
            fView.Publication.Text = fSourceRecord.Publication.Text.Trim();
            fView.Text.Text = fSourceRecord.Text.Text.Trim();

            fView.RepositoriesList.ListModel.DataOwner = fSourceRecord;
            fView.NotesList.ListModel.DataOwner = fSourceRecord;
            fView.MediaList.ListModel.DataOwner = fSourceRecord;
        }

        public void JumpToRecord(GEDCOMRecord record)
        {
            if (record != null && Accept()) {
                fBase.SelectRecordByXRef(record.XRef);
                fView.Close();
            }
        }
    }
}
