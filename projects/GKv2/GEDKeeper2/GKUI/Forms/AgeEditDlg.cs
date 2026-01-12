/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Globalization;
using System.Windows.Forms;
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

            txtVal1.Culture = CultureInfo.InvariantCulture;
            txtVal1.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            txtVal1.Mask = LangMan.LS(LSID.AgeInputMask);

            txtVal2.Culture = CultureInfo.InvariantCulture;
            txtVal2.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            txtVal2.Mask = LangMan.LS(LSID.AgeInputMask);

            fController = new AgeEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
