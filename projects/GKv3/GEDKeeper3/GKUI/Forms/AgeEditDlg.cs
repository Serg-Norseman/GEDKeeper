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
using GKCore.Design.Views;

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

        public AgeEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            var mask = AgeEditDlgController.GetFixedMask();
            txtVal1.Provider = new FixedMaskedTextProvider(mask, CultureInfo.InvariantCulture);
            txtVal2.Provider = new FixedMaskedTextProvider(mask, CultureInfo.InvariantCulture);

            fController = new AgeEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
