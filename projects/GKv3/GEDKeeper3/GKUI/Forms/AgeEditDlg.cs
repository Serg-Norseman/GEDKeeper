/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Globalization;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class AgeEditDlg : CommonDialog<IAgeEditDlg, AgeEditDlgController>, IAgeEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private GroupBox grpBox;
        private ComboBox cmbRel1;
        private ComboBox cmbRel2;
        private Label lblAge1;
        private MaskedTextBox txtVal1;
        private Label lblAge2;
        private MaskedTextBox txtVal2;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            txtVal1.Provider = new FixedMaskedTextProvider(LangMan.LS(LSID.AgeInputMask), CultureInfo.InvariantCulture);
            txtVal2.Provider = new FixedMaskedTextProvider(LangMan.LS(LSID.AgeInputMask), CultureInfo.InvariantCulture);

            fController = new AgeEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
