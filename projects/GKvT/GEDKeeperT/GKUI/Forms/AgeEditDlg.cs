/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using Terminal.Gui.TextValidateProviders;

namespace GKUI.Forms
{
    public sealed partial class AgeEditDlg : CommonDialog<IAgeEditDlg, AgeEditDlgController>, IAgeEditDlg
    {
        public GDMCustomEvent Event
        {
            get { return fController.Event; }
            set { fController.Event = value; }
        }

        #region View Interface

        IComboBox IAgeEditDlg.RelativeCombo1
        {
            get { return GetControlHandler<IComboBox>(cmbRel1); }
        }

        ITextBox IAgeEditDlg.ValueText1
        {
            get { return GetControlHandler<ITextBox>(txtVal1); }
        }

        IComboBox IAgeEditDlg.RelativeCombo2
        {
            get { return GetControlHandler<IComboBox>(cmbRel2); }
        }

        ITextBox IAgeEditDlg.ValueText2
        {
            get { return GetControlHandler<ITextBox>(txtVal2); }
        }

        #endregion

        public AgeEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            txtVal1.Provider = new NetMaskedTextProvider(LangMan.LS(LSID.AgeInputMask));
            txtVal2.Provider = new NetMaskedTextProvider(LangMan.LS(LSID.AgeInputMask));

            fController = new AgeEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
