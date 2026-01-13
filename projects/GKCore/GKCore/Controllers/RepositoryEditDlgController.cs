/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
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

                if (!Validate(fRepositoryRecord)) return false;

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
            fView.SetTitle(LangMan.LS(LSID.Repository));
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
        }
    }
}
