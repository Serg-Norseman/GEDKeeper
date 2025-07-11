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

#if !DIS_NUF

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class OrganizerWinTests : CustomWindowTest
    {
        private GDMAddress fAddress;
        private IBaseWindow fBase;
        private OrganizerWin fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fAddress = new GDMAddress();

            fAddress.AddWebPage("test");
            fAddress.AddPhoneNumber("test");
            fAddress.AddEmailAddress("test");
            fAddress.AddFaxNumber("test");

            fDialog = new OrganizerWin(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fAddress.Dispose();
        }

        [Test]
        public void Test_Common()
        {
        }

        #region Handlers for external tests

        public static void OrganizerWin_Handler(string name, IntPtr ptr, Form form)
        {
            KeyDownForm(form.Name, Keys.Escape);
            form.Dispose();
        }

        #endregion
    }
}

#endif
