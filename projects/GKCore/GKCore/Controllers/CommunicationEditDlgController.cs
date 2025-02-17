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
    public sealed class CommunicationEditDlgController : DialogController<ICommunicationEditDlg>
    {
        private GDMCommunicationRecord fCommunicationRecord;
        private GDMIndividualRecord fTempInd;

        public GDMCommunicationRecord CommunicationRecord
        {
            get { return fCommunicationRecord; }
            set {
                if (fCommunicationRecord != value) {
                    fCommunicationRecord = value;
                    UpdateView();
                }
            }
        }


        public CommunicationEditDlgController(ICommunicationEditDlg view) : base(view)
        {
            fTempInd = null;

            for (GDMCommunicationType ct = GDMCommunicationType.ctCall; ct <= GDMCommunicationType.ctLast; ct++) {
                fView.CorrType.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            fView.Dir.AddRange(new object[] {
                LangMan.LS(LSID.CD_1),
                LangMan.LS(LSID.CD_2)
            });

            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(fView,baseWin,  fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.NotesList.ListModel.SaveSettings();
            fView.MediaList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                fCommunicationRecord.CommName = fView.Name.Text;
                fCommunicationRecord.CommunicationType = (GDMCommunicationType)fView.CorrType.SelectedIndex;
                fCommunicationRecord.Date.Assign(GDMDate.CreateByFormattedStr(fView.Date.NormalizeDate, true));
                fCommunicationRecord.SetCorresponder((GDMCommunicationDir)fView.Dir.SelectedIndex, fTempInd);

                fBase.NotifyRecord(fCommunicationRecord, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("CommunicationEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.NotesList.ListModel.DataOwner = fCommunicationRecord;
                fView.MediaList.ListModel.DataOwner = fCommunicationRecord;

                if (fCommunicationRecord == null) {
                    fView.Name.Text = "";
                    fView.CorrType.SelectedIndex = -1;
                    fView.Date.Text = "";
                    fView.Dir.SelectedIndex = 0;
                    fView.Corresponder.Text = "";
                } else {
                    fView.Name.Text = fCommunicationRecord.CommName;
                    fView.CorrType.SelectedIndex = (int)fCommunicationRecord.CommunicationType;
                    fView.Date.NormalizeDate = fCommunicationRecord.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                    fTempInd = fBase.Context.Tree.GetPtrValue(fCommunicationRecord.Corresponder);

                    if (fTempInd != null) {
                        fView.Dir.SelectedIndex = (int)fCommunicationRecord.CommDirection;
                        fView.Corresponder.Text = GKUtils.GetNameString(fTempInd, false);
                    } else {
                        fView.Dir.SelectedIndex = 0;
                        fView.Corresponder.Text = "";
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("CommunicationEditDlgController.SetCommunication()", ex);
            }
        }

        public async void SetPerson()
        {
            fTempInd = await fBase.Context.SelectPerson(fView, null, TargetMode.tmNone, GDMSex.svUnknown);
            fView.Corresponder.Text = ((fTempInd == null) ? "" : GKUtils.GetNameString(fTempInd, false));
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinCommunicationEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);
            GetControl<ILabel>("lblTheme").Text = LangMan.LS(LSID.Theme);
            GetControl<ILabel>("lblCorresponder").Text = LangMan.LS(LSID.Corresponder);
            GetControl<ILabel>("lblType").Text = LangMan.LS(LSID.Type);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.Date);

            SetToolTip("btnPersonAdd", LangMan.LS(LSID.PersonAttachTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnPersonAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);

            fView.NotesList.ApplyTheme();
            fView.MediaList.ApplyTheme();
        }
    }
}
