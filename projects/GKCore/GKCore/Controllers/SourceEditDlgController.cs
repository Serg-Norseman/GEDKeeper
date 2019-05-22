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
    public sealed class SourceEditDlgController : EditorController<GDMSourceRecord, ISourceEditDlg>
    {
        public SourceEditDlgController(ISourceEditDlg view) : base(view)
        {
            fView.ShortTitle.Activate();
        }

        public override bool Accept()
        {
            try {
                fModel.ShortTitle = fView.ShortTitle.Text;
                fModel.Originator.Clear();
                fModel.SetOriginatorArray(fView.Author.Lines);
                fModel.Title.Clear();
                fModel.SetTitleArray(fView.Title.Lines);
                fModel.Publication.Clear();
                fModel.SetPublicationArray(fView.Publication.Lines);
                fModel.Text.Clear();
                fModel.SetTextArray(fView.Text.Lines);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fModel, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("SourceEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.ShortTitle.Text = fModel.ShortTitle;
            fView.Author.Text = fModel.Originator.Text.Trim();
            fView.Title.Text = fModel.Title.Text.Trim();
            fView.Publication.Text = fModel.Publication.Text.Trim();
            fView.Text.Text = fModel.Text.Text.Trim();

            fView.RepositoriesList.ListModel.DataOwner = fModel;
            fView.NotesList.ListModel.DataOwner = fModel;
            fView.MediaList.ListModel.DataOwner = fModel;
        }

        public void JumpToRecord(GDMRecord record)
        {
            if (record != null && Accept()) {
                fBase.SelectRecordByXRef(record.XRef);
                fView.Close();
            }
        }
    }
}
