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
    public sealed class CommunicationEditDlgController : DialogController<ICommunicationEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IGroupBox _GroupBox1;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblTheme;
        private ITextBox _txtName;
        private ILabel _lblDate;
        private IDateBox _txtDate;
        private ILabel _lblType;
        private IComboBox _cmbCorrType;
        private IComboBox _txtDir;
        private ILabel _lblCorresponder;
        private ITextBox _txtCorresponder;
        private IButton _btnPersonAdd;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbCorrType.ReadOnly = true;
            for (GDMCommunicationType ct = GDMCommunicationType.ctCall; ct <= GDMCommunicationType.ctLast; ct++) {
                _cmbCorrType.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            _txtDir.ReadOnly = true;
            _txtDir.AddRange(new object[] {
                LangMan.LS(LSID.CD_1),
                LangMan.LS(LSID.CD_2)
            });

            _txtName.Activate();

            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                fCommunicationRecord.CommName = _txtName.Text;
                fCommunicationRecord.CommunicationType = (GDMCommunicationType)_cmbCorrType.SelectedIndex;
                fCommunicationRecord.Date.Assign(GDMDate.CreateByFormattedStr(_txtDate.NormalizeDate, true));
                fCommunicationRecord.SetCorresponder((GDMCommunicationDir)_txtDir.SelectedIndex, fTempInd);

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
                _fNotesList.ListModel.DataOwner = fCommunicationRecord;
                _fMediaList.ListModel.DataOwner = fCommunicationRecord;

                if (fCommunicationRecord == null) {
                    _txtName.Text = "";
                    _cmbCorrType.SelectedIndex = -1;
                    _txtDate.Text = "";
                    _txtDir.SelectedIndex = 0;
                    _txtCorresponder.Text = "";
                } else {
                    _txtName.Text = fCommunicationRecord.CommName;
                    _cmbCorrType.SelectedIndex = (int)fCommunicationRecord.CommunicationType;
                    _txtDate.NormalizeDate = fCommunicationRecord.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                    fTempInd = fBase.Context.Tree.GetPtrValue(fCommunicationRecord.Corresponder);

                    if (fTempInd != null) {
                        _txtDir.SelectedIndex = (int)fCommunicationRecord.CommDirection;
                        _txtCorresponder.Text = GKUtils.GetNameString(fTempInd, false);
                    } else {
                        _txtDir.SelectedIndex = 0;
                        _txtCorresponder.Text = "";
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("CommunicationEditDlgController.SetCommunication()", ex);
            }
        }

        public async void SetPerson()
        {
            fTempInd = await BaseController.SelectPerson(fView, fBase, null, TargetMode.tmNone, GDMSex.svUnknown);
            _txtCorresponder.Text = ((fTempInd == null) ? "" : GKUtils.GetNameString(fTempInd, false));
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinCommunicationEdit));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
            _lblTheme.Text = LangMan.LS(LSID.Theme);
            _lblCorresponder.Text = LangMan.LS(LSID.Corresponder);
            _lblType.Text = LangMan.LS(LSID.Type);
            _lblDate.Text = LangMan.LS(LSID.Date);

            SetToolTip("btnPersonAdd", LangMan.LS(LSID.PersonAttachTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnPersonAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
        }
    }
}
