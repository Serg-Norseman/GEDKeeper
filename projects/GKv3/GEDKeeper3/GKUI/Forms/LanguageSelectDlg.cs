/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class LanguageSelectDlg : CommonDialog<ILanguageSelectDlg, LanguageSelectDlgController>, ILanguageSelectDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GKListView lstLanguages;
        private Button btnCancel;
        private Button btnAccept;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public int SelectedLanguage
        {
            get { return fController.SelectedLanguage; }
            set { fController.SelectedLanguage = value; }
        }

        #region View Interface

        IListView ILanguageSelectDlg.LanguagesList
        {
            get { return lstLanguages; }
        }

        #endregion

        public LanguageSelectDlg()
        {
            XamlReader.Load(this);

            fController = new LanguageSelectDlgController(this);
        }
    }
}
