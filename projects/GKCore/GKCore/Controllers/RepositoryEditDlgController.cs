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
using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

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

            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.UserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.NotesList.ListModel.SaveSettings();
            fView.UserRefList.ListModel.SaveSettings();
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
            fView.UserRefList.ListModel.DataOwner = fRepositoryRecord;
        }

        public async void ModifyAddress()
        {
            await BaseController.ModifyAddress(fView, fBase, fRepositoryRecord.Address);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Repository);
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.Title);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageUserRefs").Text = LangMan.LS(LSID.UserRefs);
            GetControl<IButton>("btnAddress").Text = LangMan.LS(LSID.Address) + @"...";
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            fView.NotesList.ApplyTheme();
            fView.UserRefList.ApplyTheme();
        }
    }
}
