/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceEditDlgController : DialogController<ISourceEditDlg>
    {
        private GDMSourceRecord fSourceRecord;

        public GDMSourceRecord SourceRecord
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

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.RepositoriesList.ListModel = new SourceRepositoriesListModel(fView, baseWin, fLocalUndoman);
            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override bool Accept()
        {
            try {
                fSourceRecord.ShortTitle = fView.ShortTitle.Text;
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
                Logger.WriteError("SourceEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.ShortTitle.Text = fSourceRecord.ShortTitle;
            fView.Author.Text = fSourceRecord.Originator.Lines.Text.Trim();
            fView.Title.Text = fSourceRecord.Title.Lines.Text.Trim();
            fView.Publication.Text = fSourceRecord.Publication.Lines.Text.Trim();
            fView.Text.Text = fSourceRecord.Text.Lines.Text.Trim();

            fView.RepositoriesList.ListModel.DataOwner = fSourceRecord;
            fView.NotesList.ListModel.DataOwner = fSourceRecord;
            fView.MediaList.ListModel.DataOwner = fSourceRecord;
        }

        public override void SetLocale()
        {
            ((IView)fView).Title = LangMan.LS(LSID.LSID_Source);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ILabel>("lblShortTitle").Text = LangMan.LS(LSID.LSID_ShortTitle);
            GetControl<ILabel>("lblAuthor").Text = LangMan.LS(LSID.LSID_Author);
            GetControl<ILabel>("lblTitle").Text = LangMan.LS(LSID.LSID_Title);
            GetControl<ILabel>("lblPublication").Text = LangMan.LS(LSID.LSID_Publication);
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.LSID_Common);
            GetControl<ITabPage>("pageText").Text = LangMan.LS(LSID.LSID_Text);
            GetControl<ITabPage>("pageRepositories").Text = LangMan.LS(LSID.LSID_RPRepositories);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);
        }
    }
}
