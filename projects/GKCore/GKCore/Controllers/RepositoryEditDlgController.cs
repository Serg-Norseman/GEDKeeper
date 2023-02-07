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
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RepositoryEditDlgController : DialogController<IRepositoryEditDlg>
    {
        private GDMRepositoryRecord fRepositoryRecord;

        public GDMRepositoryRecord RepositoryRecord
        {
            get { return fRepositoryRecord; }
            set {
                if (fRepositoryRecord != value) {
                    fRepositoryRecord = value;
                    UpdateView();
                }
            }
        }


        public RepositoryEditDlgController(IRepositoryEditDlg view) : base(view)
        {
            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
        }

        public override bool Accept()
        {
            try {
                fRepositoryRecord.RepositoryName = fView.Name.Text;

                fBase.NotifyRecord(fRepositoryRecord, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("RepositoryEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Name.Text = fRepositoryRecord.RepositoryName;

            fView.NotesList.ListModel.DataOwner = fRepositoryRecord;
        }

        public void ModifyAddress()
        {
            BaseController.ModifyAddress(fBase, fRepositoryRecord.Address);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_Repository);
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.LSID_Title);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<IButton>("btnAddress").Text = LangMan.LS(LSID.LSID_Address) + @"...";
        }
    }
}
