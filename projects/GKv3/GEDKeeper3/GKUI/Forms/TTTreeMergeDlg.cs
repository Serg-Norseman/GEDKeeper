﻿/*
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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class TTTreeMergeDlg : CommonDialog<ITreeMergeDlg, TreeMergeController>, ITreeMergeDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pageTreeMerge;
        private Label lblMasterBase;
        private TextBox edMasterBase;
        private Label lblOtherBase;
        private TextBox edUpdateBase;
        private Button btnTreeMerge;
        private TextArea mSyncRes;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region View Interface

        ITextBox ITreeMergeDlg.UpdateBase
        {
            get { return GetControlHandler<ITextBox>(edUpdateBase); }
        }

        ITextBox ITreeMergeDlg.SyncLog
        {
            get { return GetControlHandler<ITextBox>(mSyncRes); }
        }

        #endregion

        public TTTreeMergeDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new TreeMergeController(this);
            fController.Init(baseWin);
        }

        private void btnTreeMerge_Click(object sender, EventArgs e)
        {
            fController.Merge();
        }
    }
}
