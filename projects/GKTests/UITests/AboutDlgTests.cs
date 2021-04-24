/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

#if !__MonoCS__

using System;
using System.Windows.Forms;
using GKTests;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class AboutDlgTests : CustomWindowTest
    {
        private AboutDlg fDialog;

        public override void Setup()
        {
            WFAppHost.ConfigureBootstrap(false);

            fDialog = new AboutDlg();
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Close()
        {
            //var lblMail = new LabelTester("lblMail", fDialog);
            //lblMail.FireEvent("Click", new EventArgs());

            ClickButton("btnClose", fDialog);
        }

        #region Handlers for external tests

        public static void AboutDlg_Handler(string name, IntPtr ptr, Form form)
        {
            ClickButton("btnClose", form);
        }

        #endregion
    }
}

#endif
